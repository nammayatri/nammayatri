export const getKeyInSharedPrefKeysConfig = function (key) {
    return window.JBridge.getKeysInSharedPrefs(key);
  };

export const getValueToLocalNativeStoreConfig = function (key) {
  return window.JBridge.getKeyInNativeSharedPrefKeys(key);
}

export const setKeyInSharedPrefKeysImpl = function (key) {
  return function (value) {
    window.JBridge.setKeysInSharedPrefs(key, value);
  };
};