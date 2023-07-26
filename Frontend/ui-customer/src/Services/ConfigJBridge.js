export const getKeyInSharedPrefKeysConfig = function (key) {
  if (JBridge.getKeysInSharedPref) {
    return JBridge.getKeysInSharedPref(key);
  }
  if (window.__OS == "IOS") {
    return JBridge.getKeysInSharedPrefs(key);
  }
  return JBridge.getKeyInNativeSharedPrefKeys(key);
};

export const getKeyInSharedPrefKeysConfigEff = function (key) {
  if (JBridge.getKeysInSharedPref) {
    return JBridge.getKeysInSharedPref(key);
  }
  if (window.__OS == "IOS") {
    return JBridge.getKeysInSharedPrefs(key);
  }
  return JBridge.getKeyInNativeSharedPrefKeys(key);
};

export const getValueToLocalNativeStoreConfig = function (key) {
  if (JBridge.getKeysInSharedPref) {
    return JBridge.getKeysInSharedPref(key);
  }
  if (window.__OS == "IOS") {
    return JBridge.getKeysInSharedPrefs(key);
  }
  return JBridge.getKeyInNativeSharedPrefKeys(key);
}