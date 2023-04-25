const callbackMapper = require('presto-ui').callbackMapper;
exports.getKeyInSharedPrefKeysConfig = function (key) {
  return JBridge.getKeysInSharedPref(key);
};

exports.getValueToLocalNativeStoreConfig = function (key) {
return JBridge.getKeysInSharedPref(key);
}
