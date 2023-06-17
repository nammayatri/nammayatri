export const getKeyInSharedPrefKeysConfig = function (key) {
    return window.JBridge.getKeysInSharedPrefs(key);
  };

export const getValueToLocalNativeStoreConfig = function (key) {
  return window.JBridge.getKeyInNativeSharedPrefKeys(key);
}