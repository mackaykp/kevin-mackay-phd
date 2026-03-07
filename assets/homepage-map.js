/**
 * Homepage scroll-driven map narrative
 * Pattern adapted from leaflet-storymap (https://github.com/slead/leaflet-storymap):
 * scroll position determines active "chapter" (step); we update blur and map layers.
 * Uses Leaflet (https://mourner.github.io/Leaflet/) for the map.
 */
(function () {
  'use strict';

  function run() {
    var mapEl = document.getElementById('homepage-map') || document.querySelector('.homepage-map');
    var blurEl = document.getElementById('map-blur-overlay');
    var steps = document.querySelectorAll('.map-step');
    if (!mapEl || !blurEl || !steps.length) return;
    if (typeof L === 'undefined') return;

    var base = document.querySelector('base');
    var baseHref = (base && base.getAttribute('href')) || '';
    var dataRoot = baseHref + 'projects/nyc-airbnb-map/data/processed/';

    var blurByStep = [12, 8, 4, 2, 0];
    var map = null;
    var layerPoints = null;
    var layerAreasChoro = null;
    var layerAreasLisa = null;
    var legend = null;
    var currentStep = -1;
    var scrollTicking = false;

    function setBlur(step) {
      var px = blurByStep[step] || 0;
      blurEl.style.backdropFilter = px ? 'blur(' + px + 'px)' : 'none';
      blurEl.style.webkitBackdropFilter = blurEl.style.backdropFilter;
    }

    function setLayers(step) {
      if (!map) return;
      if (layerPoints && map.hasLayer(layerPoints)) map.removeLayer(layerPoints);
      if (layerAreasChoro && map.hasLayer(layerAreasChoro)) map.removeLayer(layerAreasChoro);
      if (layerAreasLisa && map.hasLayer(layerAreasLisa)) map.removeLayer(layerAreasLisa);
      if (legend) {
        try { map.removeControl(legend); } catch (e) {}
      }
      if (step >= 1 && layerPoints) map.addLayer(layerPoints);
      if (step >= 2 && layerAreasChoro) map.addLayer(layerAreasChoro);
      if (step >= 3 && layerAreasLisa) map.addLayer(layerAreasLisa);
      if (step >= 4 && legend) map.addControl(legend);
    }

    function updateStep(step) {
      if (step === currentStep) return;
      currentStep = step;
      setBlur(step);
      setLayers(step);
    }

    /**
     * Find which step contains the viewport center (leaflet-storymap style:
     * compare scroll position to each chapter's area).
     */
    function getActiveStep() {
      var viewHeight = window.innerHeight;
      var viewCenter = viewHeight / 2;
      for (var i = 0; i < steps.length; i++) {
        var rect = steps[i].getBoundingClientRect();
        if (rect.top <= viewCenter && rect.bottom >= viewCenter)
          return i;
      }
      var wrap = document.querySelector('.map-scroll-wrap');
      if (!wrap) return 0;
      var wrapRect = wrap.getBoundingClientRect();
      if (wrapRect.bottom < 0) return 0;
      if (wrapRect.top > viewHeight) return 0;
      return 0;
    }

    function onScroll() {
      if (scrollTicking) return;
      scrollTicking = true;
      requestAnimationFrame(function () {
        var active = getActiveStep();
        updateStep(active);
        scrollTicking = false;
      });
    }

    function initMap() {
      map = L.map(mapEl, {
        center: [40.72, -73.98],
        zoom: 11,
        zoomControl: true,
        scrollWheelZoom: true
      });

      L.tileLayer('https://{s}.basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}{r}.png', {
        attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> &copy; <a href="https://carto.com/attributions">CARTO</a>',
        subdomains: 'abcd',
        maxZoom: 20
      }).addTo(map);

      Promise.all([
        fetch(dataRoot + 'airbnb_points.geojson').then(function (r) { return r.json(); }),
        fetch(dataRoot + 'airbnb_areas.geojson').then(function (r) { return r.json(); })
      ]).then(function (results) {
        var pointsGeo = results[0];
        var areasGeo = results[1];

        var medPrices = areasGeo.features.map(function (f) { return f.properties.med_price; }).filter(Boolean);
        var minP = Math.min.apply(null, medPrices);
        var maxP = Math.max.apply(null, medPrices);

        function choroColor(p) {
          if (p == null) return '#cccccc';
          var t = (p - minP) / (maxP - minP || 1);
          return 'rgb(' + Math.round(1 + (191 - 1) * t) + ',' + Math.round(38 + (83 - 38) * t) + ',' + Math.round(38 + (44 - 38) * t) + ')';
        }

        var lisaColors = { 'H-H': '#EB0000', 'H-L': '#ffb1b1', 'L-H': '#b0caff', 'L-L': '#0055ff' };
        function lisaColor(cat) { return lisaColors[cat] || '#cccccc'; }

        layerPoints = L.geoJSON(pointsGeo, {
          pointToLayer: function (feature, latlng) {
            return L.circleMarker(latlng, {
              radius: 2,
              fillColor: '#012626',
              color: 'transparent',
              fillOpacity: 0.4,
              weight: 0
            });
          }
        });

        layerAreasChoro = L.geoJSON(areasGeo, {
          style: function (feature) {
            return {
              fillColor: choroColor(feature.properties.med_price),
              fillOpacity: 0.65,
              color: '#fff',
              weight: 0.6
            };
          }
        });

        layerAreasLisa = L.geoJSON(areasGeo, {
          style: function (feature) {
            return {
              fillColor: lisaColor(feature.properties.lisa_category),
              fillOpacity: 0.6,
              color: '#fff',
              weight: 0.5
            };
          }
        });

        legend = L.control({ position: 'bottomleft' });
        legend.onAdd = function () {
          var div = L.DomUtil.create('div', 'homepage-map-legend');
          div.innerHTML = '<strong>LISA</strong><br>H-H High-High<br>H-L High-Low<br>L-H Low-High<br>L-L Low-Low';
          div.style.cssText = 'padding:8px 10px;background:rgba(243,234,231,0.95);border-radius:6px;font-size:12px;color:#012626;box-shadow:0 2px 8px rgba(0,0,0,0.1);';
          return div;
        };

        setBlur(0);
        setLayers(0);

        var container = document.getElementById('map-container') || mapEl.closest('.map-container');
        if (container) {
          var io = new IntersectionObserver(function (entries) {
            entries.forEach(function (entry) {
              if (entry.isIntersecting && map) {
                map.invalidateSize();
              }
            });
          }, { threshold: 0, rootMargin: '0px' });
          io.observe(container);
        }

        setTimeout(function () {
          if (map) map.invalidateSize();
        }, 300);
      }).catch(function () {
        setBlur(0);
      });
    }

    initMap();

    window.addEventListener('scroll', onScroll, { passive: true });
    window.addEventListener('resize', function () {
      onScroll();
      if (map) map.invalidateSize();
    }, { passive: true });

    onScroll();
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', run);
  } else {
    run();
  }
})();
