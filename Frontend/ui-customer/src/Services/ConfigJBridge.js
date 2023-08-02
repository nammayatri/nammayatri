export const getKeyInSharedPrefKeysConfig = function (key) {
  return JBridge.getKeysInSharedPref(key);
};

export const getKeyInSharedPrefKeysConfigEff = function (key) {
  return JBridge.getKeysInSharedPref(key);
};

export const getValueToLocalNativeStoreConfig = function (key) {
  return JBridge.getFromSharedPrefs(key);
}