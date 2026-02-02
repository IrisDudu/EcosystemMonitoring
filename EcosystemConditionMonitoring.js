/********************
 * Basic settings
 ********************/
var startYear = 2002, endYear = 2024;
var years = ee.List.sequence(startYear, endYear);
var scale = 1000;

/********************
 * Inputs
 ********************/
function annualLAI(y){
  y = ee.Number(y);
  return ee.ImageCollection('MODIS/061/MOD15A2H')
    .filterDate(y.format('%d-01-01'), y.add(1).format('%d-01-01'))
    .select('Lai_500m').map(function(i){return i.multiply(0.1).toFloat();})
    .mean().rename('LAI').set('year', y);
}
function annualNDVI(y){
  y = ee.Number(y);
  return ee.ImageCollection('MODIS/061/MOD13A2')
    .filterDate(y.format('%d-01-01'), y.add(1).format('%d-01-01'))
    .select('NDVI').map(function(i){return i.multiply(0.0001).toFloat();})
    .mean().rename('NDVI').set('year', y);
}
function annualGPP(y){
  y = ee.Number(y);
  return ee.ImageCollection('MODIS/061/MOD17A3HGF').select('Gpp')
    .filterDate(y.format('%d-01-01'), y.add(1).format('%d-01-01'))
    .map(function(i){return i.multiply(0.0001).toFloat();})
    .sum().rename('GPP').set('year', y); // 也可以改 mean()
}

var LAI_all  = ee.ImageCollection(ee.List.sequence(startYear, endYear).map(annualLAI));
var NDVI_all = ee.ImageCollection(ee.List.sequence(startYear, endYear).map(annualNDVI));
var GPP_all  = ee.ImageCollection(ee.List.sequence(startYear, endYear).map(annualGPP));

var LAI_min = LAI_all.min(),  LAI_max = LAI_all.max();
var NDVI_min = NDVI_all.min(), NDVI_max = NDVI_all.max();
var GPP_min = GPP_all.min(),  GPP_max = GPP_all.max();

function minMaxNorm(img, minImg, maxImg) {
  var rng = maxImg.subtract(minImg);
  rng = rng.where(rng.eq(0), 1e-6);
  return img.subtract(minImg).divide(rng).clamp(0, 1)
            .rename(img.bandNames());
}

/********************
 * 5-year window
 ********************/
function fiveYearWindow(y){
  y = ee.Number(y);
  var y0 = y.subtract(2).max(startYear);
  var y1 = y.add(2).min(endYear);
  var span = y1.subtract(y0).add(1);
  var deficit = ee.Number(5).subtract(span);
  var l = deficit.divide(2).floor();
  var r = deficit.subtract(l);
  y0 = y0.subtract(l).max(startYear);
  y1 = y1.add(r).min(endYear);
  y0 = y1.subtract(4).max(startYear);
  y1 = y0.add(4).min(endYear);
  return ee.List.sequence(y0, y1);
}

/********************
* EFI / ESI / ETT
********************/
var results = years.map(function(y){
  y = ee.Number(y);
  var winYears = fiveYearWindow(y);

  var laiWin  = ee.ImageCollection(winYears.map(annualLAI));
  var ndviWin = ee.ImageCollection(winYears.map(annualNDVI));
  var gppWin  = ee.ImageCollection(winYears.map(annualGPP));
  var lc   = annual_lc.filter(ee.Filter.eq('year', ee.String(y.toInt()))).first();

  var laiMeanN  = minMaxNorm(laiWin.mean(),  LAI_min,  LAI_max);
  var ndviMeanN = minMaxNorm(ndviWin.mean(), NDVI_min, NDVI_max);
  var gppMeanN  = minMaxNorm(gppWin.mean(),  GPP_min,  GPP_max);

  var EFI = ee.ImageCollection([laiMeanN.rename('c'),
                                ndviMeanN.rename('c'),
                                gppMeanN.rename('c')]).mean()
            .rename('EFI').set('year', y);

  var EFI_series = ee.ImageCollection(winYears.map(function(yy){
    yy = ee.Number(yy);
    var l = minMaxNorm(annualLAI(yy),  LAI_min,  LAI_max);
    var n = minMaxNorm(annualNDVI(yy), NDVI_min, NDVI_max);
    var g = minMaxNorm(annualGPP(yy),  GPP_min,  GPP_max);
    return ee.ImageCollection([l.rename('c'), n.rename('c'), g.rename('c')])
            .mean().rename('EFI_one').set('year', yy);
  }));
  var stack = EFI_series.toBands();
  var mean  = stack.reduce(ee.Reducer.mean());
  var std   = stack.reduce(ee.Reducer.stdDev());
  var ESI   = (std.pow(2)).divide(mean).rename('ESI').set('year', y);

  function lc2w(img){
    var scorelayer = ee.Image(2).addBands(ee.Image(1)).addBands(ee.Image(1)).addBands(ee.Image(1)).addBands(ee.Image(1))
                      .addBands(ee.Image(1)).addBands(ee.Image(1)).addBands(ee.Image(3)).addBands(ee.Image(2)).addBands(ee.Image(1));
    var w = img.multiply(scorelayer).reduce(ee.Reducer.sum());
    return w.rename('W');
  }
  var Wmean = lc2w(lc);
  var ETI   = Wmean.divide(3).rename('ETI').set('year', y);

  return EFI.addBands(ESI).addBands(ETI).set('year', y);
});

var outIC = ee.ImageCollection(results);
var outEQI=outIC.map(function(img){
  var time=img.get('system:time_start')
  var year=img.get('year')
  var EQI=((ee.Image(1).subtract(img.select('ESI')).multiply(ee.Image(0.43)))).add(img.select('EFI').multiply(ee.Image(0.37))).add((ee.Image(1).subtract(img.select('ETI')).multiply(ee.Image(0.2))))
  return img.addBands(EQI.rename('EQI')).set('system:time_start',time).set('year',year)
})

/********************
* EQI trend analysis
********************/
var getSlope=function(col){
  var afterFilter = ee.Filter.lessThan({
    leftField: 'system:time_start',
    rightField: 'system:time_start'
  });
  
  var joined = ee.ImageCollection(ee.Join.saveAll('after').apply({
    primary: col,
    secondary: col,
    condition: afterFilter
  }));
  
  var sign = function(i, j) { 
    var temp=ee.Image(j).subtract(i)
    temp=temp.where(temp.eq(0),0).where(temp.gt(0),1).where(temp.lt(0),-1)
    return temp;
  };
  
  var kendall = ee.ImageCollection(joined.map(function(current) {
    var afterCollection = ee.ImageCollection.fromImages(current.get('after'));
    return afterCollection.map(function(image) {
      return ee.Image(sign(current, image)).unmask(0);
    });
  }).flatten()).reduce('sum', 2);
  
  ////////////////////////varience/////////////////////////////
  var groups = col.map(function(i) {
    var matches = col.map(function(j) {
      return i.eq(j); // i and j are images.
    }).sum();
    return i.multiply(matches.gt(1));
  });
  
  var group = function(array) {
    var length = array.arrayLength(0);
    var indices = ee.Image([1])
        .arrayRepeat(0, length)
        .arrayAccum(0, ee.Reducer.sum())
        .toArray(1);
    var sorted = array.arraySort();
    var left = sorted.arraySlice(0, 1);
    var right = sorted.arraySlice(0, 0, -1);
    var mask = left.neq(right)
        .arrayCat(ee.Image(ee.Array([[1]])), 0);
    var runIndices = indices.arrayMask(mask);
    var groupSizes = runIndices.arraySlice(0, 1)
        .subtract(runIndices.arraySlice(0, 0, -1));
    return groupSizes;
  };
  
  var factors = function(image) {
    return image.expression('b() * (b() - 1) * (b() * 2 + 5)');
  };
  
  var groupSizes = group(groups.toArray());
  var groupFactors = factors(groupSizes);
  var groupFactorSum = groupFactors.arrayReduce('sum', [0])
        .arrayGet([0, 0]);
  
  var count = joined.count();
  
  var kendallVariance = factors(count)
      .subtract(groupFactorSum)
      .divide(18)
      .float();
  
  //////////////////////significance/////////////////////////
  var zero = kendall.multiply(kendall.eq(0));
  var pos = kendall.multiply(kendall.gt(0)).subtract(1);
  var neg = kendall.multiply(kendall.lt(0)).add(1);
  
  var z = zero
      .add(pos.divide(kendallVariance.sqrt()))
      .add(neg.divide(kendallVariance.sqrt()));
  
  function eeCdf(z) {
    return ee.Image(0.5)
        .multiply(ee.Image(1).add(ee.Image(z).divide(ee.Image(2).sqrt()).erf()));
  }
  
  function invCdf(p) {
    return ee.Image(2).sqrt()
        .multiply(ee.Image(p).multiply(2).subtract(1).erfInv());
  }
  
  var p = ee.Image(1).subtract(eeCdf(z.abs()));
  
  //////////////////////Sen's slope (median of pairwise slopes)//////////////////////
  var pairSlope = function(i, j) {
    i = ee.Image(i);
    j = ee.Image(j);
  
    var dt = ee.Number(j.get('system:time_start'))
        .subtract(ee.Number(i.get('system:time_start')));
    var dy = j.subtract(i);
  
    var slope = dy.divide(ee.Image.constant(dt))
        .multiply(1000 * 60 * 60 * 24 * 365);
  
    return slope.rename('sen_slope');
  };
  
  var senSlopes = ee.ImageCollection(joined.map(function(current) {
    var afterCollection = ee.ImageCollection.fromImages(current.get('after'));
    return afterCollection.map(function(image) {
      return pairSlope(current, image);
    });
  }).flatten());
  
  var senSlope = senSlopes.reduce(ee.Reducer.median()).rename('sen_slope');
  return senSlope.updateMask(p.lte(0.005))
}
var getNorm=function(img){
  var p = img.reduceRegion({
    reducer: ee.Reducer.percentile([2, 98]),
    geometry: global_geometry,
    scale: 10000,
    bestEffort: true,
    maxPixels: 1e13
  });
  print(p.select(['sen_slope_p2']).get('sen_slope_p2'))
  var mins = ee.Image.constant(p.select(['sen_slope_p2']).get('sen_slope_p2'));
  var maxs = ee.Image.constant(p.select(['sen_slope_p98']).get('sen_slope_p98'));
  
  var img11 = img.subtract(mins).divide(maxs.subtract(mins)).multiply(2).subtract(1);
  return img11
}

var EQI_COL=outEQI.select('EQI')
var EQI_slope=getSlope(EQI_COL)


