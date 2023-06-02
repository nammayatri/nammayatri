package in.juspay.mobility.utils.database;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;

import androidx.annotation.Nullable;

import java.util.ArrayList;
import java.util.List;

public class AtlasDatabaseHelper extends SQLiteOpenHelper {

    private Context context;
    public static final String DATABASE_NAME = "atlasDatabase";
    public static final int DATABASE_VERSION = 1;
    private static final String RECENT_SEARCHES_TABLE = "RecentSearchedLocation";
    private static final String RIDE_HISTORY_TABLE = "RideHistoryTable";


    public AtlasDatabaseHelper(@Nullable Context context) {
        super(context, DATABASE_NAME, null, DATABASE_VERSION);
        this.context = context;
    }

    @Override
    public void onCreate(SQLiteDatabase sqLiteDatabase) {
        // Recent search table create query
        String CREATE_RECENTSEARCH_TABLE = "CREATE TABLE " + RECENT_SEARCHES_TABLE + "("
                + "id" + " INTEGER PRIMARY KEY AUTOINCREMENT, "+ "place_id" + " TEXT NOT NULL UNIQUE,"  + "place_title" + " TEXT," + "place_sub_tittle" + " TEXT,"
                + "description" + " TEXT" + ")";

        String createRideHistoryQuery = "CREATE TABLE " + RIDE_HISTORY_TABLE + "("
                + RideHistoryCols.id + " TEXT PRIMARY KEY, "
                + RideHistoryCols.status + " TEXT,"
                + RideHistoryCols.time + " TEXT,"
                + RideHistoryCols.date + " TEXT,"
                + RideHistoryCols.shortRideId + " TEXT,"
                + RideHistoryCols.vehicleNumber + " TEXT,"
                + RideHistoryCols.driverName + " TEXT,"
                + RideHistoryCols.driverSelectedFare + " INTEGER,"
                + RideHistoryCols.rideDistance + " TEXT,"
                + RideHistoryCols.updatedAt + " TEXT,"
                + RideHistoryCols.source + " TEXT,"
                + RideHistoryCols.totalAmount + " INTEGER,"
                + RideHistoryCols.destination + " TEXT"
                + ")";
        sqLiteDatabase.execSQL(CREATE_RECENTSEARCH_TABLE);
        sqLiteDatabase.execSQL(createRideHistoryQuery);
//        sqLiteDatabase.close();
    }

    @Override
    public void onUpgrade(SQLiteDatabase sqLiteDatabase, int i, int i1) {
        // Drop older table if existed
        sqLiteDatabase.execSQL("DROP TABLE IF EXISTS " + RECENT_SEARCHES_TABLE);
        sqLiteDatabase.execSQL("DROP TABLE IF EXISTS " + RIDE_HISTORY_TABLE);
        // Create tables again
        onCreate(sqLiteDatabase);
    }



    public void addRideHistoryToDB(RideHistoryModel rideHistoryModel){
        SQLiteDatabase db = this.getWritableDatabase();
        db.insert(RIDE_HISTORY_TABLE, null, getContentValues(rideHistoryModel));
    }

    public List<RideHistoryModel> getRideHistoryFromDB(){
        List<RideHistoryModel> rideHistoryModelList = new ArrayList<>();
        String selectQuery = "SELECT * FROM " + RIDE_HISTORY_TABLE;
        SQLiteDatabase db = this.getReadableDatabase();
        Cursor cursor = db.rawQuery(selectQuery, null);
        while (cursor.moveToNext()){
            RideHistoryModel rideHistoryModel = new RideHistoryModel();
            rideHistoryModel.setId(cursor.getString(0));
            rideHistoryModel.setStatus(cursor.getString(1));
            rideHistoryModel.setTime(cursor.getString(2));
            rideHistoryModel.setDate(cursor.getString(3));
            rideHistoryModel.setShortRideId(cursor.getString(4));
            rideHistoryModel.setVehicleNumber(cursor.getString(5));
            rideHistoryModel.setDriverName(cursor.getString(6));
            rideHistoryModel.setDriverSelectedFare(cursor.getInt(7));
            rideHistoryModel.setRideDistance(cursor.getString(8));
            rideHistoryModel.setUpdatedAt(cursor.getString(9));
            rideHistoryModel.setSource(cursor.getString(10));
            rideHistoryModel.setTotalAmount(cursor.getInt(11));
            rideHistoryModel.setDestination(cursor.getString(12));
            rideHistoryModelList.add(rideHistoryModel);
        }
        return rideHistoryModelList;
    }

    public void updateRideHistoryModel(RideHistoryModel rideHistoryModel){
        SQLiteDatabase db = this.getWritableDatabase();
        db.update(RIDE_HISTORY_TABLE, getContentValues(rideHistoryModel), RideHistoryCols.id + " = " + rideHistoryModel.getId(), null);
    }

    public void deleteRideHistoryModel(String id){
        SQLiteDatabase db = this.getWritableDatabase();
        db.delete(RIDE_HISTORY_TABLE, RideHistoryCols.id + " = ? ",new String[]{id});
    }

    private ContentValues getContentValues(RideHistoryModel rideHistoryModel){
        ContentValues contentValues = new ContentValues();
        contentValues.put( RideHistoryCols.id, rideHistoryModel.getId());
        contentValues.put( RideHistoryCols.status, rideHistoryModel.getStatus());
        contentValues.put( RideHistoryCols.time, rideHistoryModel.getTime());
        contentValues.put( RideHistoryCols.date, rideHistoryModel.getDate());
        contentValues.put( RideHistoryCols.totalAmount, rideHistoryModel.getTotalAmount());
        contentValues.put( RideHistoryCols.rideDistance, rideHistoryModel.getRideDistance());
        contentValues.put( RideHistoryCols.shortRideId, rideHistoryModel.getShortRideId());
        contentValues.put( RideHistoryCols.vehicleNumber, rideHistoryModel.getVehicleNumber());
        contentValues.put( RideHistoryCols.driverName, rideHistoryModel.getDriverName());
        contentValues.put( RideHistoryCols.driverSelectedFare, rideHistoryModel.getDriverSelectedFare());
        contentValues.put( RideHistoryCols.updatedAt, rideHistoryModel.getUpdatedAt());
        contentValues.put( RideHistoryCols.source, rideHistoryModel.getSource());
        contentValues.put( RideHistoryCols.destination, rideHistoryModel.getDestination());
        return contentValues;
    }

    private class RideHistoryCols {
        private static final String
                id = "id",
                status = "status",
                date = "date",
                time = "time",
                shortRideId = "shortRideId",
                vehicleNumber = "vehicleNumber",
                driverName = "driverName",
                driverSelectedFare = "driverSelectedFare",
                rideDistance = "rideDistance",
                updatedAt = "updatedAt",
                source = "source",
                totalAmount = "totalAmount",
                destination = "destination";
    }
}