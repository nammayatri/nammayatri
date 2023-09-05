export const toggleLoaderIOS = function(flag){
  console.log("inside toggle loader")
  return JBridge.toggleLoader(flag);
}

export const loaderTextIOS = function(mainTxt, subTxt){
  console.log("inside loader Text IOS")
  return JBridge.loaderText(mainTxt,subTxt);
}

export const getFromWindow = function (key) {
  if (typeof window[key] !== "undefined") {
    return window[key];
  }
}

export const createTable = function(dbName, tableName, columns){
  if(window.JBridge.createTable) {
    return window.JBridge.createTable(dbName, tableName, JSON.stringify({"columns": columns}));
  }
  else {
   return
  }
}

export const deleteTable = function(dbName, tableName){
  if(window.JBridge.deleteTable)
    return window.JBridge.deleteTable(dbName, tableName);
  else return false
}

export const deleteDb = function(dbName){
  if (window.JBridge.deleteDB)
    return window.JBridge.deleteDB(dbName);
  else return
}

export const addToSqlite = function(dbName, tableName, record){
  if (window.JBridge.addToSqlite){
    return window.JBridge.addToSqlite(dbName, tableName, JSON.stringify(record));
  }
  else return
}

export const readFromSqlite = function(dbName, tableName, selection, selectionArgs, just, nothing){
  if (window.JBridge.readFromSqlite){
      var jsonstr = window.JBridge.readFromSqlite(dbName, tableName, selection, JSON.stringify({"selectionArgs": selectionArgs}));
      try{
        var record = JSON.parse(jsonstr);
        return just(record);
      } catch(e){
      console.log("Error reading from sqlite", e);
        return nothing;
      }
  }
  else {
      return nothing
  }
}

export const deleteFromSqlite = function(dbName, tableName, selection, selectionArgs){
  if (window.JBridge.deleteFromSqlite) 
    return window.JBridge.deleteFromSqlite(dbName, tableName, selection, JSON.stringify({"selectionArgs": selectionArgs})) == "1" ? true : false;
  else return false
}

export const updateInSqlite = function(dbName, tableName, selection, selectionArgs, record){
  if (window.JBridge.updateInSqlite)
    return window.JBridge.updateInSqlite(dbName, tableName, selection, JSON.stringify({"selectionArgs": selectionArgs}), JSON.stringify(record));
  else return -1
}
