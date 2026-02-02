var demoYear=2024

/********************
 * Dictionaries
 ********************/
var landformNames = ee.Dictionary({
  0: 'tablelands', 1: 'plains', 2: 'mountains', 3: 'hills'
});
var climateNames = ee.Dictionary({
  0: 'warm_temperate_moist',
  1: 'warm_temperate_dry',
  2: 'warm_temperate_desert',
  3: 'tropical_moist',
  4: 'tropical_dry',
  5: 'tropical_desert',
  6: 'subtropical_moist',
  7: 'subtropical_dry',
  8: 'subtropical_desert',
  9: 'polar_moist',
  10: 'polar_dry',
  11: 'polar_desert',
  12: 'cool_temperate_moist',
  13: 'cool_temperate_dry',
  14: 'cool_temperate_desert',
  15: 'boreal_moist',
  16: 'boreal_dry',
  17: 'boreal_desert'
});
function oneHot(image, dict) {
  var comboImage=ee.ImageCollection(dict.keys().map(function(k){
    var mask = image.eq(ee.Number.parse(k)).rename(ee.List([dict.get(k)]));
    return mask;
  })).toBands()
  var oldNames = comboImage.bandNames()
  var newNames = oldNames.map(function(name){
    return ee.String(name).split('_').slice(1).join('_');
  });
  return comboImage.rename(newNames);
}

function comboBandName(lcBand, landformBand, climateBand) {
  return ee.List([ee.String(lcBand).cat('_')
          .cat(landformBand).cat('_')
          .cat(climateBand)]);
}

/********************
 * Ecosystem type mapping
 ********************/
function buildEcosystemCombos(year) {
  var lc = loadLCByYear(year);
  var landformOH = oneHot(landforms, landformNames);
  var climateOH  = oneHot(tempmoist, climateNames);

  var combos = ee.List([]);
  
  lc.bandNames().evaluate(function(lcNames){
    print('Landcover bands for year', year, lcNames);
  });

  var comboList = lc.bandNames().map(function(lcBand){
    lcBand = ee.String(lcBand);
    var lcImg = lc.select(lcBand).selfMask();

    return landformOH.bandNames().map(function(landBand){
      landBand = ee.String(landBand);
      var landMask = landformOH.select(landBand);
      
      return climateOH.bandNames().map(function(cliBand){
        cliBand = ee.String(cliBand);
        var cliMask = climateOH.select(cliBand);

        var name = comboBandName(lcBand, landBand, cliBand);
        var combo = lcImg.updateMask(landMask).updateMask(cliMask);
        var exp=ee.Image(0).where(combo.gt(0),combo).rename(name).updateMask(mask);
        return exp;
      });
    });
  }).flatten().flatten();

  var combined = ee.ImageCollection(comboList).toBands().set('year', year);
  var oldNames = combined.bandNames()
  var newNames = oldNames.map(function(name){
    return ee.String(name).split('_').slice(1).join('_');
  });
  return combined.rename(newNames).set('system:time_start',ee.Date.fromYMD(year,1,1).millis());
}

var combo = buildEcosystemCombos(demoYear);
var targetBand = 'water_hills_warm_temperate_moist';

/******************************
* EFG intergration
******************************/
var comboImage = combo;
var efgNames = [
  'T1_1','T1_2','T1_3','T1_4',
  'T2_1','T2_2','T2_3','T2_4','T2_5','T2_6',
  'T3_1','T3_2','T3_3','T3_4',
  'T4_1','T4_2','T4_3','T4_4','T4_5',
  'T5_1','T5_2','T5_3','T5_4','T5_5',
  'T6_2','T6_4','T6_5',
  'T7_2','T7_5',
  'TF1_1','TF1_2','TF1_3','TF1_4','TF1_5','TF1_6','TF1_7',
  'MT1_1','MT1_2','MT1_3','MT1_4',
  'MT2_1',
  'MT3_1',
  'MFT1_1','MFT1_2','MFT1_3'
];
for(var n=0;n<efgNames.length;n++){
  var img=ee.Image(0).rename(efgNames[n])
  if(n==0){
    var efgStack=img
  }
  else{
    efgStack=efgStack.addBands(img)
  }
}

var allBands = comboImage.bandNames();
var remainImage = comboImage.gte(0).toByte()
function bandsContaining(keyword) {
  keyword = ee.String(keyword);
  return allBands.filter(ee.Filter.stringContains('item', keyword));
}
var names=['tundra','water','plantation','ImperviousSurface','crop']
for(var n=0;n<names.length;n++){
  var bands=bandsContaining(names[n])
  var selectedBands=remainImage.select(bands).gt(0).not()
  remainImage=remainImage.addBands(selectedBands, null, true);
}
for(var e=1;e<=10;e++){
  var MAPPING_ASSET = 'projects/ee-duzhenrong03-04/assets/Ecosystem/ecosystem_to_efg_top50_ranked_by_pixels';
  var mappingFC = ee.FeatureCollection(MAPPING_ASSET).select(['ecosystem','efg'+e.toString()]);
  // load EFG boundary
  for(var n=0;n<efgNames.length;n++){
    var efg=ee.Image("users/duzhenrong/EFG/"+efgNames[n]).rename(efgNames[n])
    if(n==0){
      var efgImgs=efg
    }
    else{
      efgImgs=efgImgs.addBands(efg)
    }
  }
  var majorUnion = efgImgs.select([
                            'T1_1','T1_2','T1_3','T1_4',
                            'T2_1','T2_2','T2_3','T2_4','T2_5','T2_6',
                            'T3_1','T3_2','T3_3','T3_4',
                            'T4_1','T4_2','T4_3','T4_4','T4_5',
                            'T5_1','T5_2','T5_3','T5_4','T5_5',
                            'T6_2','T6_4','T6_5',
                            'T7_2','T7_5']).eq(1).reduce(ee.Reducer.max());
  function effectiveMask(efgCode) {
    var efg = efgImgs.select(efgCode);
    var majorMask = efg.eq(1);
    var minorMask = efg.eq(2)
    return ee.Image(0).where(majorMask.or(minorMask),1);
  }
  
  var allBands = comboImage.bandNames();
  function bandsContaining(keyword) {
    keyword = ee.String(keyword);
    return allBands.filter(ee.Filter.stringContains('item', keyword));
  }
  var tundraBands    = bandsContaining('tundra');
  var waterBands     = bandsContaining('water');
  var plantationBands = bandsContaining('plantation')
  var urbanBands = bandsContaining('ImperviousSurface')
  var cropBands = bandsContaining('crop')
  function excludeBadBands(bandList) {
    return ee.List(bandList).iterate(function(b, acc) {
      b   = ee.String(b);
      acc = ee.List(acc);
      var hasTundra = b.match('tundra').length().gt(0);
      var hasWater  = b.match('water').length().gt(0);
      var hasPlantation  = b.match('plantation').length().gt(0);
      var hasUrban  = b.match('ImperviousSurface').length().gt(0);
      var hasCrop  = b.match('crop').length().gt(0);
      var isBad = hasTundra.or(hasWater).or(hasPlantation).or(hasUrban).or(hasCrop);
      return ee.Algorithms.If(isBad, acc, acc.add(b));
    }, ee.List([]));
  }
  
  // band → efg
  function bandsForEFG(efgCode) {
    var base = mappingFC
                .filter(ee.Filter.eq('efg'+e.toString(), efgCode))
                .aggregate_array('ecosystem');
    return ee.List(excludeBadBands(base));
  }
  
  // get area percentage of EFGs
  function sumBands(image, bandList,mask) {
    bandList = ee.List(bandList);
    var empty = ee.Image(0).rename('tmp').reproject(image.projection());
    return ee.Algorithms.If(
      bandList.size().gt(0),
      image.select(bandList).multiply(remainImage.select(bandList)).reduce(ee.Reducer.sum()).multiply(mask),
      empty
    );
  }
  function getBands(image, bandList){
    bandList = ee.List(bandList);
    var empty = ee.Image(0).rename('tmp').reproject(image.projection());
    return ee.Algorithms.If(
      bandList.size().gt(0),
      image.select(bandList),
      empty
    );
  }
  
  var efgSumImages = efgNames.map(function(code){
    code = ee.String(code);
    var mask = effectiveMask(code);
    var sumImg = sumBands(comboImage, bandsForEFG(code),mask)
    var mask1=ee.Image(getBands(remainImage, bandsForEFG(code)))
    var used=ee.Image(getBands(comboImage, bandsForEFG(code))).multiply(mask)
    mask1=mask1.and( used.lte(0) );
    remainImage=remainImage.addBands(mask1,null,true)
    sumImg = ee.Image(sumImg).rename(code);
    return sumImg;
  });

  efgStack = ee.ImageCollection(efgSumImages).toBands();
  
  var oldNames = efgStack.bandNames();
  var newNames = oldNames.map(function(n){
    var parts = ee.String(n).split('_');
    var last2 = ee.List(parts).slice(ee.Number(parts.length()).subtract(2));
    return ee.Algorithms.If(
      ee.List(parts).size().gt(2),
      ee.String(ee.List(last2).join('_')),
      ee.String(n)
    );
  });
  efgStack = efgStack.rename(newNames);
  
  var temp = efgStack
  if(e==1){
    var output = temp
  }
  else{
    col=col.map(function(img){
      var now=temp.select([img.get('EFG')])
      return img.where(sum.lt(1).and(now.neq(img)),now.add(img))
    })
    output=col.toBands().rename(EFGbands)
  }
  
  var EFGbands = output.bandNames();
  var list = EFGbands.map(function(bandName) {
    return output.select([bandName]).rename('pct').set('EFG',bandName);
  });
  var col = ee.ImageCollection.fromImages(list);
  var sum=col.sum();
}
output = output
  .addBands(lc.select('water').rename('Freshwater'),  null, true)
  .addBands(lc.select('plantation').rename('T7_3'),   null, true)
  .addBands(lc.select('tundra').rename('T6_3'),       null, true)
  .addBands(lc.select('ImperviousSurface').rename('T7_4'), null, true)
  .addBands(lc.select('crop').rename('T7_1'),         null, true)
  .addBands(lc.select('SnowIce').rename('T6_1'),      null, true);
output=output.select(['T1_1','T1_2','T1_3','T1_4',
                      'T2_1','T2_2','T2_3','T2_4','T2_5','T2_6',
                      'T3_1','T3_2','T3_3','T3_4',
                      'T4_1','T4_2','T4_3','T4_4','T4_5',
                      'T5_1','T5_2','T5_3','T5_4','T5_5',
                      'T6_1','T6_2','T6_3','T6_4','T6_5',
                      'T7_1','T7_2','T7_3','T7_4','T7_5',
                      'TF1_1','TF1_2','TF1_3','TF1_4','TF1_5','TF1_6','TF1_7'])
            .addBands(output.select(['MT1_1','MT1_2','MT1_3','MT1_4','MT2_1','MT3_1']).reduce(ee.Reducer.sum()).rename('MT1'))
            .addBands(output.select(['MFT1_1','MFT1_2','MFT1_3']).reduce(ee.Reducer.sum()).rename('MFT1'))
            .addBands(output.select('Freshwater'))
var total = output.reduce(ee.Reducer.sum());
var efgPct = output.divide(total);
var efgMap=efgPct.toArray().arrayArgmax().arrayGet([0]).add(1);

/********************
 * Map visualization
 ********************/
var sld_intervals =
  '<RasterSymbolizer>' +
  ' <ColorMap  type="intervals" extended="false" >' +
      '<ColorMapEntry color="#228B22" quantity="1" label="T1.1 Tropical-subtropical lowland rainforests"/>' + 
      '<ColorMapEntry color="#8FBC8F" quantity="2" label="T1.2 Tropical-subtropical dry forests and thickets"/>' +
      '<ColorMapEntry color="#2E8B57" quantity="3" label="T1.3 Tropical-subtropical montane rainforests"/>' +
      '<ColorMapEntry color="#3CB371" quantity="4" label="T1.4 Tropical heath forests"/>' +
      
      '<ColorMapEntry color="#556B2F" quantity="5" label="T2.1 Boreal and temperate montane forests and woodlands"/>' +
      '<ColorMapEntry color="#6B8E23" quantity="6" label="T2.2 Deciduous temperate forests"/>' +
      '<ColorMapEntry color="#8F9779" quantity="7" label="T2.3 Oceanic cool temperate rainforests"/>' +
      '<ColorMapEntry color="#A9A9A9" quantity="8" label="T2.4 Warm temperate laurophyll forests"/>' +
      '<ColorMapEntry color="#BDB76B" quantity="9" label="T2.5 Temperate pyric humid forests"/>' +
      '<ColorMapEntry color="#A0522D" quantity="10" label="T2.6 Temperate pyric sclerophyll forests and woodlands"/>' +
      
      '<ColorMapEntry color="#BC8F8F" quantity="11" label="T3.1 Seasonally dry tropical shrublands"/>' +
      '<ColorMapEntry color="#DAA520" quantity="12" label="T3.2 Seasonally dry temperate heaths and shrublands"/>' +
      '<ColorMapEntry color="#DEB887" quantity="13" label="T3.3 Cool temperate heathlands"/>' +
      '<ColorMapEntry color="#8B4513" quantity="14" label="T3.4 Rocky pavements, screes and lava flows"/>' +
      
      '<ColorMapEntry color="#CD853F" quantity="15" label="T4.1 Trophic savannas"/>' +
      '<ColorMapEntry color="#FFA07A" quantity="16" label="T4.2 Pyric tussock savannas"/>' +
      '<ColorMapEntry color="#FFD700" quantity="17" label="T4.3 Hummock savannas"/>' +
      '<ColorMapEntry color="#FFE4B5" quantity="18" label="T4.4 Temperate woodlands"/>' +
      '<ColorMapEntry color="#EEE8AA" quantity="19" label="T4.5 Temperate tussock grasslands"/>' +
      
      '<ColorMapEntry color="#C1A07E" quantity="20" label="T5.1 Semi-desert steppes"/>' +
      '<ColorMapEntry color="#D2B48C" quantity="21" label="T5.2 Thorny deserts and semi-deserts"/>' +
      '<ColorMapEntry color="#E3C5A3" quantity="22" label="T5.3 Sclerophyll hot deserts and semi-deserts"/>' +
      '<ColorMapEntry color="#F4D6B5" quantity="23" label="T5.4 Cool deserts and semi-deserts"/>' +
      '<ColorMapEntry color="#E4C9AD" quantity="24" label="T5.5 Hyper-arid deserts"/>' +
      
      '<ColorMapEntry color="#FFFFFF" quantity="25" label="T6.1 Ice sheets, glaciers and perennial snowfields"/>' +
      '<ColorMapEntry color="#D9EAD3" quantity="26" label="T6.2 Polar-alpine rocky outcrops"/>' +
      '<ColorMapEntry color="#A9CCE3" quantity="27" label="T6.3 Polar tundra and deserts"/>' +
      '<ColorMapEntry color="#C6D9F0" quantity="28" label="T6.4 Temperate alpine grasslands and shrublands"/>' +
      '<ColorMapEntry color="#98CF8F" quantity="29" label="T6.5 Tropical alpine grasslands and shrublands"/>' +
      
      '<ColorMapEntry color="#D7A6D6" quantity="30" label="T7.1 Croplands"/>' +
      '<ColorMapEntry color="#E4B7E3" quantity="31" label="T7.2 Intensive livestock pastures"/>' +
      '<ColorMapEntry color="#DA70D6" quantity="32" label="T7.3 Plantations"/>' +
      '<ColorMapEntry color="#8B008B" quantity="33" label="T7.4 Cities, villages and infrastructure"/>' +
      '<ColorMapEntry color="#D8BFD8" quantity="34" label="T7.5 Derived semi-natural pastures and oldfields"/>' +
      
      '<ColorMapEntry color="#76C2A1" quantity="35" label="TF1.1 Tropical flooded forests and peat forests"/>' +
      '<ColorMapEntry color="#8CCCCC" quantity="36" label="TF1.2 Subtropical/temperate forested wetlands"/>' +
      '<ColorMapEntry color="#A2DED2" quantity="37" label="TF1.3 Permanent marshes"/>' +
      '<ColorMapEntry color="#B8E8E4" quantity="38" label="TF1.4 Seasonal floodplain marshes"/>' +
      '<ColorMapEntry color="#CEF2F6" quantity="39" label="TF1.5 Episodic arid floodplains"/>' +
      '<ColorMapEntry color="#D4F6EC" quantity="40" label="TF1.6 Boreal, temperate and montane peat bogs"/>' +
      '<ColorMapEntry color="#E0F7F2" quantity="41" label="TF1.7 Boreal and temperate fens"/>' +
      
      '<ColorMapEntry color="#C6E0B4" quantity="42" label="MT Marine-Terrestrial"/>' +
      
      '<ColorMapEntry color="#FFD966" quantity="43" label="MFT Marine-Freshwater-Terrestrial"/>' +
      
      '<ColorMapEntry color="#4682B4" quantity="44" label="Freshwater"/>' +
      
    '</ColorMap>' +
  '</RasterSymbolizer>';
Map.addLayer(efgMap.sldStyle(sld_intervals),{},'EFG '+demoYear.toString())

