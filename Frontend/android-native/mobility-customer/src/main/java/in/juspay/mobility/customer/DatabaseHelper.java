package in.juspay.mobility.customer;

import android.content.Context;
import android.content.ContentValues;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.util.Log;


import android.database.Cursor;
import org.json.JSONObject;
import org.json.JSONArray;


public class DatabaseHelper extends SQLiteOpenHelper {
    private SQLiteDatabase db;
    private String dbName;

    public DatabaseHelper(Context context, String _dbName) {
      super(context, _dbName, null,1);
      this.dbName = _dbName;
    }

    public void deleteDb(Context context) {
        try {
            context.deleteDatabase(this.dbName);
        } catch (Exception e){
            Log.i("SQLiteLog", "Exception while deleting db " + e);
        }
    }

    public boolean isTableExists(String tableName) {
        SQLiteDatabase db = this.getWritableDatabase();
        Cursor cursor = db.rawQuery("SELECT name FROM sqlite_master WHERE type='table' AND name=?", new String[]{tableName});
        boolean exists = cursor.getCount() > 0;
        cursor.close();
        return exists;
    }

    public void createTable(SQLiteDatabase db, String tableName, JSONArray columns){
      try{
        if(!isTableExists(tableName)){
            StringBuilder createString = new StringBuilder("create table " + tableName + "(" + columns.getJSONObject(0).getString("key") + " " + columns.getJSONObject(0).getString("type"));
            for(int i=1;i<columns.length() ;i++){
                createString.append(",").append(columns.getJSONObject(i).getString("key")).append(" ").append(columns.getJSONObject(i).getString("type"));
            }
            createString.append(")");
            Log.i("SQLiteLog", "createtable String : " + createString);
            db.execSQL(createString.toString());
        }
        } catch(Exception e){
            Log.i("SQLiteLog", "Error creating table: " + e);
        }
    }

    @Override
    public void onCreate(SQLiteDatabase db) { }

    @Override
    public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) { }

    public int createRecord(SQLiteDatabase db, String tableName, JSONObject record){
      try{
        ContentValues contentValues = new ContentValues();
        String[] columns = db.query(tableName, null, null, null, null, null, null).getColumnNames();
        for(String col : columns) {
          if (!col.equals("id")) contentValues.put(col, record.getString(col));
        }
        int key = (int) db.insert(tableName, null, contentValues);
        db.close();
        return key;
      } catch(Exception e){
        Log.i("SQLiteLog", "Error writing to " + tableName + ":" + e);
        return -1;
      }
    }

    public JSONObject readRecord(SQLiteDatabase db, String tableName, String selection, String[] selectionArgs){
      try{
        JSONObject data = new JSONObject();
        String[] columns = db.query(tableName, null, null, null, null, null, null).getColumnNames();
        Cursor cursor;
        cursor = db.query(tableName, columns, selection, selectionArgs, null, null, null); 
        if (cursor.moveToFirst()) {
          for(String col : columns) data.put(col, cursor.getString(cursor.getColumnIndex(col)));
        }
        cursor.close();
        db.close();
        return data;
      } catch(Exception e){
        Log.i("SQLiteLog", "Error reading from " + tableName + ": " + e);
        return new JSONObject();
      }
    }

    public int updateRecord(SQLiteDatabase db, String tableName, JSONObject record, String selection, String[] selectionArgs){
      try{
        ContentValues newRec = new ContentValues();
        String[] columns = db.query(tableName, null, null, null, null, null, null).getColumnNames();
        for (String col: columns) {
            if(!col.equals("id"))
              newRec.put(col, record.getString(col));
        }
        int updatedRecId = db.update(tableName, newRec, selection, selectionArgs);
        db.close();
        return updatedRecId;
      } catch(Exception e){
        Log.i("SQLiteLog", "Error updating in " + tableName + ":" + e);
        return -1;
      }
    }

    public Boolean deleteRecord(SQLiteDatabase db, String tableName, String selection, String[] selectionArgs){
        try{
          db.delete(tableName, selection, selectionArgs);
          db.close();
          return true;
        } catch(Exception e){
          Log.i("SQLiteLog", "Error deleting record from " + tableName + ":" + e);
          return false;
        }
    }
}
