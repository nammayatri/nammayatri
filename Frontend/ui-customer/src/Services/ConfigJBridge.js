exports.getKeyInSharedPrefKeysConfig = function (key) {
  if (JBridge.getKeysInSharedPref) {
    return JBridge.getKeysInSharedPref(key);
  }
  if (window.__OS == "IOS") {
    return JBridge.getKeysInSharedPrefs(key);
  }
  return JBridge.getKeyInNativeSharedPrefKeys(key);
};

exports.getKeyInSharedPrefKeysConfigEff = function (key) {
  if (JBridge.getKeysInSharedPref) {
    return JBridge.getKeysInSharedPref(key);
  }
  if (window.__OS == "IOS") {
    return JBridge.getKeysInSharedPrefs(key);
  }
  return JBridge.getKeyInNativeSharedPrefKeys(key);
};

exports.getValueToLocalNativeStoreConfig = function (key) {
  if (JBridge.getKeysInSharedPref) {
    return JBridge.getKeysInSharedPref(key);
  }
  if (window.__OS == "IOS") {
    return JBridge.getKeysInSharedPrefs(key);
  }
  return JBridge.getKeyInNativeSharedPrefKeys(key);
}