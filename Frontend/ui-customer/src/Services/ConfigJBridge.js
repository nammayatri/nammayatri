const JBridge = window.JBridge;
export const getKeyInSharedPrefKeysConfig = function (key) {
  return JBridge.getKeysInSharedPref(key);
};

export const getKeyInSharedPrefKeysConfigEff = function (key) {
  return JBridge.getKeysInSharedPref(key);
};

export const getValueToLocalNativeStoreConfig = function (key) {
  return JBridge.getFromSharedPrefs(key);
}

export const setKeyInSharedPrefKeysImpl = function (key) {
  return function (value) {
    window.JBridge.setKeysInSharedPrefs(key, value);
  };
};
