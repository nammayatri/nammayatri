package in.juspay.mobility.utils;

import android.Manifest;
import android.app.Application;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.location.Location;
import android.location.LocationListener;
import android.location.LocationManager;
import android.os.Bundle;
import android.os.Handler;
import android.os.IBinder;
import android.os.Looper;
import android.util.Log;

import androidx.annotation.NonNull;
import androidx.core.app.ActivityCompat;

import com.google.android.gms.location.FusedLocationProviderClient;
import com.google.android.gms.location.LocationServices;
import com.google.android.gms.tasks.OnFailureListener;
import com.google.android.gms.tasks.OnSuccessListener;
import com.google.android.gms.tasks.Task;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.TimeZone;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.net.ssl.HttpsURLConnection;

import in.juspay.mobility.CommonJsInterface;
import in.juspay.mobility.R;

public class LocationUpdateService extends Service {
    private static final String LOG_TAG = "LocationServices";
    final int notificationServiceId = 15082022; // ARDU pilot launch date : DDMMYYYY
    Timer timer = new Timer();
    double gpsLatitude;
    double gpsLongitude;
    float gpsAccuracy;
    JSONArray payload = new JSONArray();
    private static ArrayList<LocationUpdateService.UpdateTimeCallback> timeUpdateCallback = new ArrayList<>();
    public interface UpdateTimeCallback{
        public void timeUpdateFlow(String time);
    }
    public  static void registerCallback(LocationUpdateService.UpdateTimeCallback timeUpdateCallback){
        LocationUpdateService.timeUpdateCallback.add(timeUpdateCallback);
    }
    public static void deRegisterCallback(LocationUpdateService.UpdateTimeCallback timeUpdateCallback){
        LocationUpdateService.timeUpdateCallback.remove(timeUpdateCallback);
    }



    @Override
    public void onCreate() {
        super.onCreate();
        System.out.println("onCreate location service");
        try {
            this.startForeground(notificationServiceId, NotificationUtils.createNotification(this, "Updating", "Your location is being updated", new JSONObject()));
        } catch (JSONException e) {
            e.printStackTrace();
        }
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        if (intent != null) {
            try {
                String action = intent.getStringExtra("action");
                if (action != null) {
                    switch (action) {
                        case "start":
                            try {
                                getLocationFromGPS();
                                startTimerTask();
                            } catch (Exception e) {
                                e.printStackTrace();
                            }
                            break;
                        default:
                            if (timer != null) {
                                timer.cancel();
                            }
                            stopForeground(true);
                            stopSelf();
                    }
                } else {
                    if (timer != null) {
                        timer.cancel();
                    }
                    stopForeground(true);
                    stopSelf();
                }
            } catch (Exception e){
                Log.e(LOG_TAG, e.toString());
            }
        }
        return START_STICKY;
    }

    public void startTimerTask() {
        try {
            if (timer != null) {
                System.out.println("startTimer inside if" + timer);
                timer.cancel();
            } else {
                timer = new Timer();
            }
        }
        catch (Exception e){
            System.out.println("startTimer task exception" + e);
        }
        timer = new Timer();
        TimerTask timerTask = new TimerTask() {
            @Override
            public void run() {
                System.out.println("timerTask inside run");
                fetchCurrentLocation();
            }
        };
        timer.scheduleAtFixedRate(timerTask, 0, 20000);
    }

    private void getLocationFromGPS() {
        if (ActivityCompat.checkSelfPermission(this, Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED && ActivityCompat.checkSelfPermission(this, Manifest.permission.ACCESS_COARSE_LOCATION) != PackageManager.PERMISSION_GRANTED) {
            // TODO: Consider calling
            //    ActivityCompat#requestPermissions
            // here to request the missing permissions, and then overriding
            //   public void onRequestPermissionsResult(int requestCode, String[] permissions,
            //                                          int[] grantResults)
            // to handle the case where the user grants the permission. See the documentation
            // for ActivityCompat#requestPermissions for more details.
            return;
        }
        LocationManager locationManager = (LocationManager) this.getSystemService(Context.LOCATION_SERVICE);
        boolean gpsEnabled = locationManager.isProviderEnabled(LocationManager.GPS_PROVIDER); // Asking for GPS specifically so that the first location is dependant on phone's GPS.
        String provider = LocationManager.GPS_PROVIDER;
        LocationListener listener = new LocationListener() {
            @Override
            public void onLocationChanged(@NonNull Location location) {
                gpsLatitude = location.getLatitude();
                gpsLongitude = location.getLongitude();
                gpsAccuracy = location.getAccuracy();
            }
            @Override
            public void onStatusChanged(String provider, int status, Bundle extras) {

            }

            @Override
            public void onProviderEnabled(@NonNull String provider) {

            }

            @Override
            public void onProviderDisabled(@NonNull String provider) {

            }
        };
        locationManager.requestLocationUpdates(provider, 20000, 10, listener);
        System.out.println("requestLocationUpdates" + gpsLatitude + gpsLongitude + gpsAccuracy);

    }

    private void fetchCurrentLocation() {
        FusedLocationProviderClient client = LocationServices.getFusedLocationProviderClient(this);
        if (ActivityCompat.checkSelfPermission(this, Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED && ActivityCompat.checkSelfPermission(this, Manifest.permission.ACCESS_COARSE_LOCATION) != PackageManager.PERMISSION_GRANTED) {
            // TODO: Consider calling
            //    ActivityCompat#requestPermissions
            // here to request the missing permissions, and then overriding
            //   public void onRequestPermissionsResult(int requestCode, String[] permissions,
            //                                          int[] grantResults)
            // to handle the case where the user grants the permission. See the documentation
            // for ActivityCompat#requestPermissions for more details.
            return;
        }
        Task<Location> currentLocation = client.getLastLocation();
        System.out.println("fetchCurrentLocation after task" + currentLocation);
        currentLocation.addOnSuccessListener(new OnSuccessListener<Location>() {
            @Override
            public void onSuccess(Location location) {
                if (currentLocation != null && location!=null){
                    double latitude = location.getLatitude();
                    double longitude = location.getLongitude();
                    float accuracy = location.getAccuracy();
                    System.out.println("onSuccess before callDriverCurrentLocationAPI" + latitude + " " + longitude + " " + accuracy );
                    callDriverCurrentLocationAPI(latitude, longitude, accuracy);
                }
            }
        }).addOnFailureListener(new OnFailureListener() {
            @Override
            public void onFailure(@NonNull Exception e) {
                getLocationFromGPS();
                System.out.println("onFailure before callDriverCurrentLocationAPI" + gpsLatitude + " " + gpsLongitude + " " + gpsAccuracy );
                callDriverCurrentLocationAPI(gpsLatitude, gpsLongitude, gpsAccuracy);
            }
        });

    }

    private void callDriverCurrentLocationAPI(double latitude, double longitude, float accuracy) {
    ExecutorService executor = Executors.newSingleThreadExecutor();
    Handler handler = new Handler(Looper.getMainLooper());
    executor.execute(() -> {
        StringBuilder result = new StringBuilder();
        System.out.println("callDriverCurrentLocationAPI");
        SharedPreferences sharedPref = getApplicationContext().getSharedPreferences(
                        getApplicationContext().getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        String token = sharedPref.getString("REGISTERATION_TOKEN", "null");
        String baseUrl = sharedPref.getString("BASE_URL", "null");
        try {
            String orderUrl = baseUrl + "/driver/location";
            final SimpleDateFormat f = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");
            System.out.println(" in SimpleDateFormat : " + f);
            f.setTimeZone(TimeZone.getTimeZone("UTC"));
            String getCurrTime = f.format(new Date());
            Log.d(LOG_TAG, "orderUrl " + orderUrl);
            
            HttpURLConnection connection = (HttpURLConnection) (new URL(orderUrl).openConnection());
            if (connection instanceof HttpsURLConnection)
               ((HttpsURLConnection) connection).setSSLSocketFactory(new TLSSocketFactory());
            connection.setRequestMethod("POST");
            connection.setRequestProperty("Content-Type", "application/json");
            connection.setRequestProperty("token", token);
            connection.setDoOutput(true);

            JSONObject point = new JSONObject();
            JSONObject locationData = new JSONObject();
            point.put("lat", latitude);
            point.put("lon", longitude );
            locationData.put("pt",point);
            locationData.put("ts",getCurrTime);
            locationData.put("acc",accuracy);
            payload.put(locationData);
            Log.d(LOG_TAG, "in payload "+ payload.toString());
            OutputStream stream = connection.getOutputStream();
            stream.write(payload.toString().getBytes());
            connection.connect();
            payload = new JSONArray();
            int respCode = connection.getResponseCode();
            InputStreamReader respReader;
            Log.d(LOG_TAG, "respCode "+ respCode);

            if ((respCode < 200 || respCode >= 300) && respCode != 302) {
                respReader = new InputStreamReader(connection.getErrorStream());
                Log.d(LOG_TAG, "in error "+ respReader);
            } else {
                respReader = new InputStreamReader(connection.getInputStream());
                Log.d(LOG_TAG, "in 200 "+ respReader);
                for(int i =0;i<timeUpdateCallback.size();i++){
                    timeUpdateCallback.get(i).timeUpdateFlow(getCurrTime);
                }
            }

            BufferedReader in = new BufferedReader(respReader);
            String inputLine;
            while ((inputLine = in.readLine()) != null) {
                result.append(inputLine);
            }
            Log.d(LOG_TAG, "in result "+ result.toString());
        }
        catch (Exception ignored) {
            Log.d(LOG_TAG, "Catch in callDriverCurrentLocationAPI : "+ignored);
        }

        handler.post(() ->{
            try {
                JSONObject resp = new JSONObject(String.valueOf(result));
                if (resp.get("errorCode").equals("INVALID_TOKEN")) {
                    System.out.println("Inisde Invalid token " + resp.get("errorCode") );
                    SharedPreferences sharedPrefs = this.getSharedPreferences(
                    this.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
                    String RegToken = sharedPrefs.getString("REGISTERATION_TOKEN", "__failed");
                    SharedPreferences.Editor editor = sharedPref.edit();
                    editor.putString("REGISTERATION_TOKEN", "__failed");
                    editor.apply();
                    timer.cancel();
                    //cancel alarmManager
                    LocationUpdateAlarm locationUpdateAlarm = new LocationUpdateAlarm();
                    locationUpdateAlarm.stopAlarm(this);
                    onDestroy();
                    stopForeground(true);
                    stopSelf();
                    executor.shutdown();
                }
            } catch (JSONException e) {
                e.printStackTrace();
            }
        });
        });
    }

    @Override
    public IBinder onBind(Intent intent) {
        // TODO: Return the communication channel to the service.
        throw new UnsupportedOperationException("Not yet implemented");
    }
}