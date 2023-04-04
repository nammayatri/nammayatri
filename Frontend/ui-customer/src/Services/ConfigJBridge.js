const callbackMapper = require('presto-ui').callbackMapper;
exports.getKeyInSharedPrefKeysConfig = function (key) {
    return JBridge.getKeysInSharedPrefs(key);
  };

exports.getKeyInSharedPrefKeysConfigEff = function (key) {
    return JBridge.getKeysInSharedPrefs(key);
  };

exports.getValueToLocalNativeStoreConfig = function (key) {
  if (window.__OS == "IOS") {
    return JBridge.getKeysInSharedPrefs(key);
  }
  return JBridge.getKeyInNativeSharedPrefKeys(key);
}
