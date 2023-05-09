import { callbackMapper } from "presto-ui";

export const getKeyInSharedPrefKeysConfig = function (key) {
  return window.JBridge.getKeysInSharedPrefs(key);
};

export const getKeyInSharedPrefKeysConfigEff = function (key) {
  return window.JBridge.getKeysInSharedPrefs(key);
};

export const getValueToLocalNativeStoreConfig = function (key) {
  if (window.__OS == "IOS") {
    return window.JBridge.getKeysInSharedPrefs(key);
  }
  return window.JBridge.getKeyInNativeSharedPrefKeys(key);
}
