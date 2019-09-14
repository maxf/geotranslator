let map, view;

require([
  'esri/Map',
  'esri/views/MapView'
], function(Map, MapView) {

  map = new Map({
    basemap: 'topo-vector'
  });

  view = new MapView({
    container: 'viewDiv',
    map: map,
    center: [-2,55], // longitude, latitude
    zoom: 5
  });
});
