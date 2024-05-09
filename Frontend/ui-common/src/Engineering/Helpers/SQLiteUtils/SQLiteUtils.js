import { callbackMapper } from "presto-ui";

const JBridge = window.JBridge;

export const createTable = function(dbName, tableName, columns){
  if(window.JBridge.createTable) {
    console.log("createTable found");
    console.log("dbName", dbName);
    console.log("tableName", tableName);
    console.log("columns", JSON.stringify({"columns": columns}));
    return window.JBridge.createTable(dbName, tableName, JSON.stringify({"columns": columns}));
  }
  else {
    console.log("createTable not found");
     
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
}
 
export const addToSqlite = function(dbName, tableName, record){
  if (window.JBridge.addToSqlite){
    console.log("zxc addToSqlite found");
    console.log("zxc  dbName", dbName);
    console.log("zxc tableName", tableName);
    console.log("zxc record", JSON.stringify(record));
    return window.JBridge.addToSqlite(dbName, tableName, JSON.stringify(record));
  }
}
 
export const readFromSqlite = function(dbName, tableName, selection, selectionArgs, just, nothing){
  if (window.JBridge.readFromSqlite){
    const jsonstr = window.JBridge.readFromSqlite(dbName, tableName, selection, JSON.stringify({"selectionArgs": selectionArgs}));
    console.log("readFromSqlite zxc -> ", jsonstr);
    try{

      const record = JSON.parse(jsonstr);
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

export const executeQuery = function(dbName, query, just, nothing){
  if (window.JBridge.executeQuery){
    const jsonstr = window.JBridge.executeQuery(dbName, query);
    try{
      console.log("executeQuery zxc -> ", jsonstr);
      const record = JSON.parse(jsonstr);
      console.log("executeQuery zxc record -> ", record);
      return record.length > 0 ? just(record) : nothing;
    } catch(e){
      console.log("Error executing query", e);
      return nothing;
    }
  }
  else {
    return nothing
  }
}


// @JavascriptInterface
//     public JSONArray executeQuery(String dbName, String query){
//         try(DatabaseHelper dbHelper = new DatabaseHelper(bridgeComponents.getContext(), dbName)){
//             SQLiteDatabase db = dbHelper.getWritableDatabase();
//             return dbHelper.executeQuery(db, query);
//         } catch (Exception e){
//             Log.i("SQLiteLog", "Error executing query: " + e);
//             return new JSONArray();
//         }
//     } 