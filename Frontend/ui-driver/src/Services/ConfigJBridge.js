const JBridge = window.JBridge;
export const getKeyInSharedPrefKeysConfig = function (key) {
  if (JBridge.getFromSharedPrefs) {
    return JBridge.getFromSharedPrefs(key);
  }
  return JBridge.getKeyInNativeSharedPrefKeys(key);
};

export const getKeyInSharedPrefKeysConfigEff = function (key) {
  if (JBridge.getFromSharedPrefs) {
    return JBridge.getFromSharedPrefs(key);
  }
  return JBridge.getKeyInNativeSharedPrefKeys(key);
};

export const getValueToLocalNativeStoreConfig = function (key) {
  if (JBridge.getFromSharedPrefs) {
    return JBridge.getFromSharedPrefs(key);
  }
  return JBridge.getKeyInNativeSharedPrefKeys(key);
}

export const setKeyInSharedPrefKeysImpl = function (key) {
  return function (value) {
    window.JBridge.setKeysInSharedPrefs(key, value);
  };
};
