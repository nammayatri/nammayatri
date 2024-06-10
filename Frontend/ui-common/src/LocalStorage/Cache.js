const JBridge = window.JBridge;
export const getFromCache = function (key, nothing, just) {
  window.cacheMap = window.cacheMap || {}
  if (typeof window.cacheMap[key] !== "undefined") {
    return just(window.cacheMap[key]);
  } else {
    return nothing;
  }
}

export const setInCache = function (key, value) {
  window.cacheMap = window.cacheMap || {}
  window.cacheMap[key] = value;
  return value;
}
export const clearCache = function (key) {
  window.cacheMap = window.cacheMap || {}
  if (key != "") {
    delete window.cacheMap[key];
  } else {
    window.cacheMap = {};
  }
}

export const setKeyInSharedPref = function (key, value) {
  return JBridge.setInSharedPrefs(key, value);
};