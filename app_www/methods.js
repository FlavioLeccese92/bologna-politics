// https://github.com/rstudio/leaflet/issues/496

window.LeafletWidget.methods.setStyle = function(category, layerId, style){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){ // in case a single layerid is given
    layerId = [layerId];
  }
  // console.log(map)

  //convert columnstore to row store
  style = HTMLWidgets.dataframeToD3(style);
  //console.log(style);

  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    // console.log(layer);
    if (layer){ // or should this raise an error?
      layer.setStyle(style[i]);
    }
  });
};

window.LeafletWidget.methods.setRadius = function(layerId, radius){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){ // in case a single layerid is given
    layerId = [layerId];
    radius = [radius];
  }

  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer("marker", d);
    if (layer){ // or should this raise an error?
      layer.setRadius(radius[i]);
    }
  });
};

window.LeafletWidget.methods.setLabel = function(category, layerId, label){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){ // in case a single layerid is given
    layerId = [layerId];
  }

  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){ // or should this raise an error?
      layer.unbindTooltip();
      // the object subsetting to get the integer array and casting to string is what I added
      layer.bindTooltip(label.label[i].toString());
    }
  });
};
