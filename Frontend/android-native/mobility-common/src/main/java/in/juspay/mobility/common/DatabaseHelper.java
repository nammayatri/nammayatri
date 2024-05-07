package in.juspay.mobility.common;

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

    public void deleteDb(Context context) throws Exception {
        try {
            context.deleteDatabase(this.dbName);
        } catch (Exception e){
            Log.e("SQLiteLog", "Exception while deleting db ", e);
            throw e;
        }
    }

    public boolean isTableExists(String tableName) throws Exception {
        SQLiteDatabase db = this.getWritableDatabase();
        Cursor cursor = null;
        try {
            cursor = db.rawQuery("SELECT name FROM sqlite_master WHERE type='table' AND name=?", new String[]{tableName});
            return cursor.getCount() > 0;
        } catch (Exception e) {
            Log.i("SQLiteLog", "Error checking if table exists: " + e);
            throw e;
        } finally {
            if (cursor != null) {
                cursor.close();
            }
        }
    }

    public void createTable(SQLiteDatabase db, String tableName, JSONArray columns) throws Exception {
        try {
            if (!isTableExists(tableName)) {
                StringBuilder createString = new StringBuilder(String.format("CREATE TABLE %s (", tableName));
                for (int i = 0; i < columns.length(); i++) {
                    JSONObject column = columns.getJSONObject(i);
                    if (i > 0) {
                        createString.append(", ");
                    }
                    createString.append(String.format("%s %s", column.getString("key"), column.getString("type")));
                }
                createString.append(")");
                Log.i("SQLiteLog", "createTable String : " + createString);
                db.execSQL(createString.toString());
            }
        } catch (Exception e) {
            Log.i("SQLiteLog", "Error creating table: " + e);
            throw e;
        }
    }

    @Override
    public void onCreate(SQLiteDatabase db) { }

    @Override
    public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) { }

    public int createRecord(SQLiteDatabase db, String tableName, JSONObject record) throws Exception {
        try {
            db.beginTransaction();
            ContentValues contentValues = new ContentValues();
            String[] columns = db.query(tableName, null, null, null, null, null, null).getColumnNames();
            for (String col : columns) {
                if (!col.equals("id")) {
                    contentValues.put(col, record.optString(col, ""));
                }
            }
            int rowId = (int) db.insert(tableName, null, contentValues);
            db.setTransactionSuccessful();
            return rowId;
        } catch (Exception e) {
            Log.i("SQLiteLog", "Error writing to " + tableName + ":" + e);
            throw e;
        } finally {
            db.endTransaction();
        }
    }

    public JSONObject readRecord(SQLiteDatabase db, String tableName, String selection, String[] selectionArgs) throws Exception {
        JSONObject data = new JSONObject();
        String[] columns = db.query(tableName, null, null, null, null, null, null).getColumnNames();
        Cursor cursor = db.query(tableName, columns, selection, selectionArgs, null, null, null);
        if (cursor.moveToFirst()) {
            for(String col : columns) {
                int index = cursor.getColumnIndex(col);
                if (index != -1) {
                    data.put(col, cursor.getString(index));
                }
            }
        }
        cursor.close();
        System.out.println("zxc-> "+ data);
        return data;
    }

    public enum QueryType {
    SELECT,
    INSERT,
    UPDATE,
    DELETE,
    CREATE_TABLE,
    UNKNOWN
}
public QueryType getQueryType(String query) {
    String trimmedUpperQuery = query.trim().toUpperCase();
    if (trimmedUpperQuery.startsWith("SELECT")) {
        return QueryType.SELECT;
    } else if (trimmedUpperQuery.startsWith("INSERT")) {
        return QueryType.INSERT;
    } else if (trimmedUpperQuery.startsWith("UPDATE")) {
        return QueryType.UPDATE;
    } else if (trimmedUpperQuery.startsWith("DELETE")) {
        return QueryType.DELETE;
    } else if (trimmedUpperQuery.startsWith("CREATE TABLE")) {
        return QueryType.CREATE_TABLE;
    } else {
        return QueryType.UNKNOWN;
    }
}

public JSONArray executeQuery(SQLiteDatabase db, String query) {
    JSONArray data = new JSONArray();
    try {
        QueryType queryType = getQueryType(query);
        switch (queryType) {
            case SELECT:
                Cursor cursor = db.rawQuery(query, null);
                String[] columns = cursor.getColumnNames();
                while (cursor.moveToNext()) {
                    JSONObject row = new JSONObject();
                    for(String col : columns) {
                        int index = cursor.getColumnIndex(col);
                        if (index != -1) {
                            row.put(col, cursor.getString(index));
                        }
                    }
                    data.put(row);
                }
                cursor.close();
                break;
            case INSERT:
                long insertResult = db.compileStatement(query).executeUpdateDelete();
                JSONObject insertObj = new JSONObject();
                insertObj.put("insertedRowId", insertResult);
                data.put(insertObj);
                break;
            case UPDATE:
                long updateResult = db.compileStatement(query).executeUpdateDelete();
                JSONObject updateObj = new JSONObject();
                updateObj.put("updatedRows", updateResult);
                data.put(updateObj);
                break;
            case DELETE:
                long deleteResult = db.compileStatement(query).executeUpdateDelete();
                JSONObject deleteObj = new JSONObject();
                deleteObj.put("deletedRows", deleteResult);
                data.put(deleteObj);
                break;
            case CREATE_TABLE:
                db.execSQL(query);
                JSONObject statusObj = new JSONObject();
                statusObj.put("status", "Table created successfully");
                data.put(statusObj);
                break;
            default:
                Log.i("SQLiteLog", "Unknown query type: " + query);
                break;
        }
    } catch (Exception e) {
        Log.i("SQLiteLog", "Error executing query: " + e);
    }
    return data;
}

    public int updateRecord(SQLiteDatabase db, String tableName, JSONObject record, String selection, String[] selectionArgs) throws Exception {
        ContentValues newRec = new ContentValues();
        String[] columns = db.query(tableName, null, null, null, null, null, null).getColumnNames();
        for (String col: columns) {
            if(!col.equals("id"))
                newRec.put(col, record.optString(col, ""));
        }
        return db.update(tableName, newRec, selection, selectionArgs);
    }

    public Boolean deleteRecord(SQLiteDatabase db, String tableName, String selection, String[] selectionArgs){
        try{
            int rowsDeleted = db.delete(tableName, selection, selectionArgs);
            db.close();
            return rowsDeleted > 0;
        } catch(Exception e){
            Log.i("SQLiteLog", "Error deleting record from " + tableName + ":" + e);
            return false;
        }
    }
}