package in.juspay.mobility.customer;

import android.content.Context;
import android.content.ContentValues;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.util.Log;

import android.database.Cursor;
import org.json.JSONObject;
import java.util.ArrayList;
import java.util.List;

public class DatabaseHelper extends SQLiteOpenHelper {

    public static final String DATABASE_NAME = "DriverInfoDb";
    public static final String DRIVERINFO_TABLE_NAME = "DriverInfo";
    private static SQLiteDatabase db;


    public DatabaseHelper(Context context) {
      super(context,DATABASE_NAME,null,1);
    }


    public void deleteAllRows(SQLiteDatabase db){
        db.delete(DRIVERINFO_TABLE_NAME, null, null);
    }

    public List<List<String>> getAllRows(SQLiteDatabase db){
        List<List<String>> dataList = new ArrayList<>();
        String[] projection = { "otp", "driverName", "vehicleDetails", "registrationNumber", "rating", "source", "destination", "price", "distance", "estimatedDistance", "driverNumber", "merchantExoPhone", "vehicleVariant"};
        Cursor cursor = db.query(DRIVERINFO_TABLE_NAME, projection, null, null, null, null, null);
        if (cursor.moveToFirst()) {
            do {
                List<String> data = new ArrayList();
                data.add(cursor.getString(cursor.getColumnIndex("otp")));
                data.add(cursor.getString(cursor.getColumnIndex("driverName")));
                data.add(cursor.getString(cursor.getColumnIndex("vehicleDetails")));
                data.add(cursor.getString(cursor.getColumnIndex("registrationNumber")));
                data.add(cursor.getString(cursor.getColumnIndex("rating")));
                data.add(cursor.getString(cursor.getColumnIndex("source")));
                data.add(cursor.getString(cursor.getColumnIndex("destination")));
                data.add(cursor.getString(cursor.getColumnIndex("price")));
                data.add(cursor.getString(cursor.getColumnIndex("distance")));
                data.add(cursor.getString(cursor.getColumnIndex("estimatedDistance")));
                data.add(cursor.getString(cursor.getColumnIndex("driverNumber")));
                data.add(cursor.getString(cursor.getColumnIndex("merchantExoPhone")));
                data.add(cursor.getString(cursor.getColumnIndex("vehicleVariant")));
                dataList.add(data);
            } while (cursor.moveToNext());
        }
        cursor.close();
        db.close();
        return dataList;
    }

    @Override
    public void onCreate(SQLiteDatabase db) {
        db.execSQL(
            "create table " + DRIVERINFO_TABLE_NAME + "(id integer primary key, otp string, driverName string, vehicleDetails string, registrationNumber string, rating string, source string, destination string, price string, distance string, estimatedDistance string, driverNumber string, merchantExoPhone string, vehicleVariant string)"
        );
    }

    @Override
    public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        db.execSQL("DROP TABLE IF EXISTS " + DRIVERINFO_TABLE_NAME);
        onCreate(db);
    }
}
