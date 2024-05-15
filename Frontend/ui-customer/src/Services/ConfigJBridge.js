const JBridge = window.JBridge;
export const getKeyInSharedPrefKeysConfig = function (key) {
  if (key == "LOCAL_STAGE") {
    if (window.LOCAL_STAGE != undefined)
      return window.LOCAL_STAGE;
    const stage = JBridge.getKeysInSharedPref(key);
    window.LOCAL_STAGE = stage;
    return stage;
  }
  return JBridge.getKeysInSharedPref(key);
};

export const getKeyInSharedPrefKeysConfigEff = function (key) {
  if (key == "LOCAL_STAGE") {
    if (window.LOCAL_STAGE != undefined)
      return window.LOCAL_STAGE;
    const stage = JBridge.getKeysInSharedPref(key);
    window.LOCAL_STAGE = stage;
    return stage;
  }
  return JBridge.getKeysInSharedPref(key);
};

export const getValueToLocalNativeStoreConfig = function (key) {
  if (key == "LOCAL_STAGE") {
    if (window.LOCAL_STAGE != undefined)
      return window.LOCAL_STAGE;
    const stage = JBridge.getFromSharedPrefs(key);
    window.LOCAL_STAGE = stage;
    return stage;
  }
  return JBridge.getFromSharedPrefs(key);
}

export const setKeyInSharedPrefKeysImpl = function (key) {
  return function (value) {
    if (key == "LOCAL_STAGE")
      window.LOCAL_STAGE = value;
    window.JBridge.setInSharedPrefs(key, value);
  };
};
