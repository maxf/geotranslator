// version 0.10.0

const urlsToCache = [
  '/',
  '/elm.js',
  '/logo-1668x1572.png',
  '/logo-512x512.png',
  '/logo-192x192.png'
];

const cacheName = 'cache';


self.addEventListener('install', function(evt) {
  console.log('install');
  evt.waitUntil(
    caches.open(cacheName).then(cache => {
      console.log('[ServiceWorker] Pre-caching offline page');
      self.skipWaiting();
      console.log('adding all');
      return cache.addAll(urlsToCache);
    })
  );
});

self.addEventListener('activate', function(evt) {
  console.log('activate');
  evt.waitUntil(
    caches.keys().then(keyList => {
      return Promise.all(keyList.map(key => {
        if (key !== cacheName) {
          console.log('[ServiceWorker] Removing old cache', key);
          return caches.delete(key);
        }
      }));
      self.clients.claim();
    })
  );
});

self.addEventListener('fetch', function(event) {
  console.log('fetch');
  event.respondWith(
    // Try the cache
    caches.match(event.request).then(function(response) {
      // Fall back to network
      return response || fetch(event.request);
    }).catch(function() {
      // If both fail, show a generic fallback:
      return caches.match('/');
      // However, in reality you'd have many different
      // fallbacks, depending on URL & headers.
      // Eg, a fallback silhouette image for avatars.
    })
  );
});
