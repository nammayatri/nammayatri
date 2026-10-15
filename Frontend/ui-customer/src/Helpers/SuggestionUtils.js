
export const setSuggestionsMapInJson = function (map) {
  const jsonString = JSON.stringify(map);
  const unstring = JSON.parse(jsonString);
  window.JBridge.setKeysInSharedPrefs("SUGGESTIONS_MAP", jsonString);
  return Array.from(unstring);
}
  
export const getSuggestedDestinationsJsonFromLocal = function (key){
    
  const stringifiedMap=window.JBridge.getKeysInSharedPref ? window.JBridge.getKeysInSharedPref(key) : window.JBridge.getKeysInSharedPrefs(key); //  window.JBridge.getKeysInSharedPrefs(key);
  if (stringifiedMap != "__failed" && stringifiedMap != "(null)") {
    const unstring = JSON.parse(stringifiedMap);
    return Array.from(unstring);
  } 
  console.log("err", stringifiedMap)
  return stringifiedMap;
}