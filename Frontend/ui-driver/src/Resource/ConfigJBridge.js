exports.getKeyInSharedPrefKeysConfig = function (key) {
  if (JBridge.getKeysInSharedPref) {
    return JBridge.getKeysInSharedPref(key);
  }
  return JBridge.getKeyInNativeSharedPrefKeys(key);
};

exports.getKeyInSharedPrefKeysConfigEff = function (key) {
if (JBridge.getKeysInSharedPref) {
  return JBridge.getKeysInSharedPref(key);
}
return JBridge.getKeyInNativeSharedPrefKeys(key);
};

exports.getValueToLocalNativeStoreConfig = function (key) {
if (JBridge.getKeysInSharedPref) {
  return JBridge.getKeysInSharedPref(key);
}
return JBridge.getKeyInNativeSharedPrefKeys(key);
}
