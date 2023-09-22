/*
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.app;

import static android.Manifest.permission.ACCESS_FINE_LOCATION;

import static in.juspay.mobility.app.Utils.compareTimeWithInRange;

import android.Manifest;
import android.annotation.SuppressLint;
import android.app.AlarmManager;
import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.graphics.BitmapFactory;
import android.location.Location;
import android.location.LocationManager;
import android.os.BatteryManager;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.IBinder;
import android.os.Looper;
import android.service.notification.StatusBarNotification;
import android.util.Log;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.app.ActivityCompat;
import androidx.core.app.NotificationCompat;
import androidx.core.app.NotificationManagerCompat;
import androidx.core.location.LocationManagerCompat;
import androidx.work.WorkManager;

import com.google.android.gms.location.FusedLocationProviderClient;
import com.google.android.gms.location.LocationCallback;
import com.google.android.gms.location.LocationRequest;
import com.google.android.gms.location.LocationResult;
import com.google.android.gms.location.LocationServices;
import com.google.android.gms.location.Priority;
import com.google.android.gms.maps.model.LatLng;
import com.google.android.gms.tasks.CancellationTokenSource;
import com.google.firebase.analytics.FirebaseAnalytics;
import com.google.maps.android.PolyUtil;
import com.google.maps.android.SphericalUtil;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.chrono.ChronoLocalDate;
import java.time.temporal.ChronoUnit;
import java.util.Calendar;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.TimeZone;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.net.ssl.HttpsURLConnection;

import in.juspay.hypersdk.data.KeyValueStore;

public class LocationUpdateService extends Service {
    private static final String LOG_TAG = "LocationServices";
    private final String LOCATION_UPDATES = "LOCATION_UPDATES";
    private static final String LOCATION_PAYLOAD = "LOCATION_PAYLOAD";
    private static final String LAST_LOCATION_TIME = "LAST_LOCATION_TIME";
    final int notificationServiceId = 15082022; // ARDU pilot launch date : DDMMYYYY
    final int alertNotificationId = 07102022;
    FusedLocationProviderClient fusedLocationProviderClient;
    LocationCallback locationCallback;
    double lastLatitudeValue;
    double lastLongitudeValue;
    double prevLat;
    double prevLon;
    boolean updated;
    private static Boolean isLocationUpdating = false;
    private Timer timer;
    private String gpsMethodSwitch;
    private Context context;
    private int delayForG = 500000, delayForT = 20000;
    private int delayForGNew = 500000, delayForTNew = 20000;
    private int maximumLimit = 60;
    private int pointsToRemove = 1;
    private int maximumLimitNotOnRide = 3;
    private float minDispDistanceNew = 25.0f, minDispDistance = 25.0f;
    private TimerTask timerTask;
    private boolean alertSent = false;
    private boolean deviationAlert = false;
    private String rideWaypoints = null;
    CancellationTokenSource cancellationTokenSource;
    static JSONArray metaDataForLocation;
    static JSONObject deviceManufacturer;
    static JSONObject deviceModel;
    static JSONObject batteryPercentage;
    static JSONObject isOnCharge;
    static JSONObject triggerFunction;
    static JSONObject androidVersion;

    enum LocationSource {
        CurrentLocation,
        LastLocation
    }

    enum TriggerFunction {
        TimerTask,
        GoogleCallback
    }


    private static final ArrayList<UpdateTimeCallback> updateTimeCallbacks = new ArrayList<>();

    public interface UpdateTimeCallback {
        void triggerUpdateTimeCallBack(String time, String lat, String lng);
    }

    public static void registerCallback(UpdateTimeCallback timeUpdateCallback) {
        LocationUpdateService.updateTimeCallbacks.add(timeUpdateCallback);
    }

    public static void deRegisterCallback(UpdateTimeCallback timeUpdateCallback) {
        LocationUpdateService.updateTimeCallbacks.add(timeUpdateCallback);
    }

    @Override
    public void onCreate() {
        super.onCreate();
        updateConfigVariables();
        initialiseJSONObjects();
        context = getApplicationContext();
        isLocationUpdating = false;
        this.startForeground(notificationServiceId, createNotification());
        fusedLocationProviderClient = LocationServices.getFusedLocationProviderClient(this);
        timer = new Timer();
        resetTimer(delayForGNew, minDispDistanceNew, delayForTNew);
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        /* Start the service if the driver is active*/
        startForeground(notificationServiceId, createNotification());
        initialiseJSONObjects();
        updateDeviceDetails();
        updateConfigVariables();
        logEventForHealthCheck(intent);
        if (delayForGNew != delayForG || minDispDistanceNew != minDispDistance || delayForTNew != delayForT) {
            resetTimer(delayForGNew, minDispDistanceNew, delayForTNew);
        } else if (!isLocationUpdating) {
            startLocationUpdates(delayForGNew, minDispDistanceNew, delayForTNew);
        }
        return START_STICKY;
    }

    private void updateDeviceDetails() {
        try {
            updateJSONObject(deviceManufacturer, "deviceManufacturer", Build.MANUFACTURER);
            updateJSONObject(androidVersion, "androidVersion", Build.VERSION.SDK_INT);
            updateJSONObject(deviceModel, "deviceModel", Build.MODEL);
            metaDataForLocation.put(deviceManufacturer);
            metaDataForLocation.put(deviceModel);
            metaDataForLocation.put(androidVersion);
        } catch (JSONException e) {
            e.printStackTrace();
            Log.d("LOG_TAG", "Unable to put data in metaData " + e);
        }
    }

    private void initialiseJSONObjects() {
        metaDataForLocation = new JSONArray();
        deviceManufacturer = new JSONObject();
        deviceModel = new JSONObject();
        batteryPercentage = new JSONObject();
        isOnCharge = new JSONObject();
        triggerFunction = new JSONObject();
        androidVersion = new JSONObject();
    }

    private void updateJSONObject(JSONObject obj, String key, String value) throws JSONException {
        obj.put("key", key);
        obj.put("value", value);
    }

    private void updateJSONObject(JSONObject obj, String key, int value) throws JSONException {
        obj.put("key", key);
        obj.put("value", value);
    }

    private void updateJSONObject(JSONObject obj, String key, boolean value) throws JSONException {
        obj.put("key", key);
        obj.put("value", value);
    }

    private void updateConfigVariables() {
        try {
            SharedPreferences sharedPrefs = getApplicationContext().getSharedPreferences(this.getString(R.string.preference_file_key), Context.MODE_PRIVATE);

            String MAX_LIMIT_TO_STORE_LOCATION_PT = "MAX_LIMIT_TO_STORE_LOCATION_PT";
            maximumLimit = Integer.parseInt(sharedPrefs.getString(MAX_LIMIT_TO_STORE_LOCATION_PT, "60"));
            String MAX_LIMIT_TO_STORE_LOCATION_PT_NOT = "MAX_LIMIT_TO_STORE_LOCATION_PT_NOT";
            maximumLimitNotOnRide = Integer.parseInt(sharedPrefs.getString(MAX_LIMIT_TO_STORE_LOCATION_PT_NOT, "3"));
            // UPDATE FOR GOOGLE CALLBACK FREQUENCY
            String locationGFrequency = sharedPrefs.getString("RIDE_G_FREQUENCY", null);
            delayForGNew = locationGFrequency != null ? Integer.parseInt(locationGFrequency) : 50000;

            // UPDATE FOR TIMER TASK FREQUENCY
            String locationTFrequency = sharedPrefs.getString("RIDE_T_FREQUENCY", null);
            delayForTNew = locationTFrequency != null ? Integer.parseInt(locationTFrequency) : 20000;

            //UPDATE FOR GOOGLE MIN DISPLACEMENT VALUE
            String minimumDisplacement = sharedPrefs.getString("DRIVER_MIN_DISPLACEMENT", null);
            minDispDistanceNew = minimumDisplacement != null ? Float.parseFloat(minimumDisplacement) : 30.0f;
            String gpsMethod = sharedPrefs.getString("GPS_METHOD", null);
            gpsMethodSwitch = gpsMethod != null ? gpsMethod : "CURRENT";

            cancellationTokenSource = new CancellationTokenSource();
        } catch (Exception exception) {
            Log.e(LOG_TAG, "Exception updateConfigVariables " + exception);
        }
    }

    private void resetTimer(int delayInMilliS, float minDisplacement, int delayForTinMillis) {
        cancelTimer();
        if (fusedLocationProviderClient != null && locationCallback != null) {
            fusedLocationProviderClient.removeLocationUpdates(locationCallback);
        }
        startLocationUpdates(delayInMilliS, minDisplacement, delayForTinMillis);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        isLocationUpdating = false;
        cancelTimer();
        if (fusedLocationProviderClient != null && locationCallback != null) {
            fusedLocationProviderClient.removeLocationUpdates(locationCallback);
        }
        stopForeground(true);
        stopSelf();
    }

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

// MAIN FUNCTIONS

    @SuppressLint("MissingPermission")
    private void startLocationUpdates(int delayInMilliS, float minDisplacement, int delayInTMillis) {
        if (!isLocationUpdating) {
            delayForG = delayInMilliS;
            minDispDistance = minDisplacement;
            delayForT = delayInTMillis;
            isLocationUpdating = true;
            checkLocation(); // Checking the location permissions and status(on or off)

            // Starting fusedLocationProviderClient callback
            LocationCallback lc = getLocationCallback();
            LocationRequest lr = createLocationRequest(delayForGNew, minDispDistanceNew);
            fusedLocationProviderClient.requestLocationUpdates(lr, lc, Looper.getMainLooper());

            TimerTask tt = createTimer();
            if (timer == null) timer = new Timer();
            timer.scheduleAtFixedRate(tt, 0, delayForT);
        }
    }

    /* To update driver status if location is disabled. To prevent false location updates*/
    private void updateDriverStatus(Boolean status) {
        ExecutorService executor = Executors.newSingleThreadExecutor();
        executor.execute(() ->
        {
            StringBuilder result = new StringBuilder();
            System.out.println("updateDriverStatus");
            SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
            String token = sharedPref.getString("REGISTERATION_TOKEN", "null");
            String bundle_version = sharedPref.getString("BUNDLE_VERSION", "null");
            String baseUrl = sharedPref.getString("BASE_URL", "null");
            String deviceDetails = sharedPref.getString("DEVICE_DETAILS", "null");
            String version = sharedPref.getString("VERSION_NAME", "null");
            try {
                //endPoint for driver status
                String orderUrl = baseUrl + "/driver/setActivity?active=" + status;
                Log.d(LOG_TAG, "orderUrl " + orderUrl);
                //Http connection to make API call
                HttpURLConnection connection = (HttpURLConnection) (new URL(orderUrl).openConnection());
                if (connection instanceof HttpsURLConnection)
                    ((HttpsURLConnection) connection).setSSLSocketFactory(new TLSSocketFactory());
                connection.setRequestMethod("POST");
                connection.setRequestProperty("Content-Type", "application/json");
                connection.setRequestProperty("x-client-version", version);
                connection.setRequestProperty("token", token);
                connection.setRequestProperty("x-bundle-version", bundle_version);
                connection.setRequestProperty("x-device", deviceDetails);
                connection.setDoOutput(true);
                connection.connect();

                // validating the response code
                int respCode = connection.getResponseCode();
                InputStreamReader respReader;
                Log.d(LOG_TAG, "respCode " + respCode);

                if ((respCode < 200 || respCode >= 300) && respCode != 302) {
                    respReader = new InputStreamReader(connection.getErrorStream());
                    Log.d(LOG_TAG, "in error " + respReader);
                } else {
                    respReader = new InputStreamReader(connection.getInputStream());
                    Log.d(LOG_TAG, "in 200 " + respReader);
                }

                BufferedReader in = new BufferedReader(respReader);
                String inputLine;
                while ((inputLine = in.readLine()) != null) {
                    result.append(inputLine);
                }
                updateStorage("LOCATION_STATUS", "PAUSE");
                updateStorage("DRIVER_STATUS", "__failed");
                Log.d(LOG_TAG, "in result " + result);
                showAlertNotification(); // To notify the driver that he is offline.
                cancelTimer();
                WorkManager mWorkManager = WorkManager.getInstance(context);
                mWorkManager.cancelAllWorkByTag(context.getString(R.string.location_update));
                onDestroy();
                executor.shutdown();
            } catch (Exception error) {
                Log.d(LOG_TAG, "Catch in updateDriverStatus : " + error);
            }
        });
    }

    @Nullable
    private String getValueFromStorage(String k) {
        return KeyValueStore.read(getApplicationContext(),getApplicationContext().getString(R.string.preference_file_key),k,null);
    }

    private JSONObject getLatLng(Double lat, Double lng) throws JSONException {
        JSONObject latLng = new JSONObject();
        latLng.put("lat", lat);
        latLng.put("lon", lng);
        return latLng;
    }

    private void getRoute(Double startLat, Double startLng, Double endLat, Double endLng) {
        StringBuilder result = new StringBuilder();

        String regToken = getValueFromStorage("REGISTERATION_TOKEN");
        String baseUrl = getValueFromStorage("BASE_URL");
        String version = getValueFromStorage("VERSION_NAME");

        if (regToken == null || baseUrl == null || version == null) {
            return;
        }
        try {
            String rideId = getValueFromStorage("RIDE_ID");
            if (rideId == null) {
                return;
            }
            String url = baseUrl + "/" + rideId + "/route";
            HttpURLConnection connection = (HttpURLConnection) (new URL(url).openConnection());
            connection.setRequestMethod("POST");
            connection.setRequestProperty("Content-Type", "application/json");
            connection.setRequestProperty("token", regToken);
            connection.setRequestProperty("x-client-version", version);

            connection.connect();
            int respCode = connection.getResponseCode();
            InputStreamReader respReader;
            if (respCode == 200) {
                respReader = new InputStreamReader(connection.getInputStream());
                BufferedReader in = new BufferedReader(respReader);
                String inputLine;
                while ((inputLine = in.readLine()) != null) {
                    result.append(inputLine);
                }
                JSONObject res = new JSONObject(String.valueOf(result));
                JSONArray pathLatLng = (JSONArray) res.get("points");
                rideWaypoints = String.valueOf(pathLatLng);
            } else if (respCode == 404) {
                String routeFallbackUrl = baseUrl + "/trip/route";
                HttpURLConnection fallbackConnection = (HttpURLConnection) (new URL(routeFallbackUrl).openConnection());
                fallbackConnection.setRequestMethod("POST");
                fallbackConnection.setRequestProperty("Content-Type", "application/json");
                fallbackConnection.setRequestProperty("token", regToken);
                fallbackConnection.setRequestProperty("x-client-version", version);

                JSONObject payload = new JSONObject();

                JSONObject startLatLng = getLatLng(startLat, startLng);
                JSONObject endLatLng = getLatLng(endLat, endLng);

                JSONArray waypoints = new JSONArray();
                waypoints.put(startLatLng);
                waypoints.put(endLatLng);

                payload.put("waypoints", waypoints);
                payload.put("mode", "CAR");
                payload.put("calcPoints", true);


                OutputStream fallbackStream = fallbackConnection.getOutputStream();
                fallbackStream.write(payload.toString().getBytes());
                fallbackConnection.connect();
                int respC = fallbackConnection.getResponseCode();

                InputStreamReader fallbackRespReader;
                if (respC == 200) {
                    fallbackRespReader = new InputStreamReader(fallbackConnection.getInputStream());
                    BufferedReader in = new BufferedReader(fallbackRespReader);
                    String inputLine;
                    while ((inputLine = in.readLine()) != null) {
                        result.append(inputLine);
                    }
                    JSONObject res = (JSONObject) new JSONArray(String.valueOf(result)).get(0);
                    JSONArray pathLatLng = (JSONArray) res.get("points");
                    rideWaypoints = String.valueOf(pathLatLng);
                } else {
                    Bundle params = new Bundle();
                    FirebaseAnalytics.getInstance(context).logEvent("LS_ERROR_GETTING_ROUTE", params);
                }
            } else {
                Bundle params = new Bundle();
                FirebaseAnalytics.getInstance(context).logEvent("LS_ERROR_GETTING_ROUTE", params);
            }
        } catch (Exception e) {
            Log.e("get route api exception", e.toString());
        }
    }

    private void callDeviationAlertAPI(String reason){
        try{
            String baseUrl = getValueFromStorage("BASE_URL");
            String rideID = getValueFromStorage("RIDE_ID");
            String version = getValueFromStorage("VERSION_NAME");
            String token = getValueFromStorage("REGISTERATION_TOKEN");
            String bundle_version = getValueFromStorage("BUNDLE_VERSION");
            String deviceDetails = getValueFromStorage("DEVICE_DETAILS");
            String orderURL = baseUrl + "/deviationAlert";
            JSONObject post_data = new JSONObject();
            post_data.put("rideId", rideID);
            post_data.put("reason", reason);
            HttpURLConnection connection = (HttpURLConnection) (new URL(orderURL).openConnection());
            if (connection instanceof HttpsURLConnection)
                ((HttpsURLConnection) connection).setSSLSocketFactory(new TLSSocketFactory());
            connection.setRequestMethod("POST");
            connection.setRequestProperty("Content-Type", "application/json");
            connection.setRequestProperty("token", token);
            connection.setRequestProperty("x-bundle-version", bundle_version);
            connection.setRequestProperty("x-device", deviceDetails);
            connection.setRequestProperty("x-client-version", version);
            connection.setDoOutput(true);
            OutputStream stream = connection.getOutputStream();
            stream.write(post_data.toString().getBytes());
            connection.connect();
            int response = connection.getResponseCode();
            if (response < 200 || response >= 300 && response != 302){
                InputStreamReader respReader = new InputStreamReader(connection.getInputStream());
                System.out.println("LOCATION_UPDATE: ERROR API respReader :- " + respReader);
                Log.d(LOG_TAG, "in error " + respReader);
            }
        }
        catch (Exception e){
            Log.e("callDeviationAlertAPI Error", e.toString());
        }
    }

    private void checkIfNotMoving(double lat, double lon, float accuracy){
        try {
            SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
            String jsonValue = sharedPref.getString("DRIVER_LATLON_TIME", "__failed");
            LocalDateTime now = null;
            JSONObject obj;
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O)
                now = LocalDateTime.now();
            if (jsonValue.equals("__failed")) {
                obj = new JSONObject();
                obj.put("lat", lat);
                obj.put("lon", lon);
                obj.put("timeStamp", now.toString());
                sharedPref.edit().putString("DRIVER_LATLON_TIME", obj.toString()).apply();
            } else {
                obj = new JSONObject(jsonValue);
                LatLng oldLatLan = new LatLng(obj.getDouble("lat"), obj.getDouble("lon"));
                LatLng newLatLan = new LatLng(lat, lon);
                Double dist = SphericalUtil.computeDistanceBetween(oldLatLan, newLatLan);
                if (dist < accuracy) {
                    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                        SimpleDateFormat parser = new SimpleDateFormat("HH:mm");
                        Date startTime  = parser.parse("22:00");
                        Date endTime    = parser.parse("06:00");
                        Date date   = new Date();
                        Date currTime = parser.parse(parser.format(date)) ;
                        LocalDateTime prevTime = LocalDateTime.parse(obj.getString("timeStamp"));
                        String alertVal = sharedPref.getString("SAFETY_ALERT", "false");
                        if (ChronoUnit.SECONDS.between(prevTime, now) / 60 >= 5 && alertVal.equals("false") && compareTimeWithInRange(startTime,endTime, currTime)) {
                            sharedPref.edit().putString("SAFETY_ALERT","true").apply();
                            callDeviationAlertAPI("NotMoving");
                        }
                    }
                } else {
                    obj = new JSONObject();
                    obj.put("lat", lat);
                    obj.put("lon", lon);
                    obj.put("timeStamp", now.toString());
                    sharedPref.edit().putString("DRIVER_LATLON_TIME", obj.toString()).apply();
                }
            }
        }
        catch (Exception e) {
            Log.e("Driver Not Moving Exception", e.toString());
        }
    }

    private void updateDeviation(double latitude, double longitude, String waypoints) {
        try {
            List<LatLng> latLngPoints = new ArrayList<>();
            JSONArray waypointArray = new JSONArray(waypoints);

            for (int i = 0; i < waypointArray.length(); i++) {
                JSONObject pt = waypointArray.getJSONObject(i);
                latLngPoints.add(new LatLng((Double) pt.get("lat"), (Double) pt.get("lon")));
            }

            String toleranceEarth = getValueFromStorage("TOLERANCE_EARTH");
            double toleranceEarthVal = toleranceEarth == null ? 30.0 : Double.parseDouble(toleranceEarth);

            Integer devCount;
            try {
                devCount = Integer.parseInt(getValueFromStorage("RIDE_WAYPOINT_DEVIATION_COUNT"));
            } catch (NumberFormatException e) {
                devCount = null;
            }
            if (devCount == null) {
                return;
            }
            SimpleDateFormat parser = new SimpleDateFormat("HH:mm");
            Date startTime  = parser.parse("22:00");
            Date endTime    = parser.parse("06:00");
            Date date   = new Date();
            Date currTime = parser.parse(parser.format(date)) ;
            SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
            String alertVal = sharedPref.getString("SAFETY_ALERT","false");
            if (PolyUtil.locationIndexOnEdgeOrPath(new LatLng(latitude, longitude), latLngPoints, false, true, 1000.0) == -1 && alertVal.equals("false") && compareTimeWithInRange(startTime, endTime, currTime)){
                sharedPref.edit().putString("SAFETY_ALERT","true").apply();
                callDeviationAlertAPI("Deviation");
            }
            if (devCount < 3) {
                int resultIndex = PolyUtil.locationIndexOnEdgeOrPath(new LatLng(latitude, longitude), latLngPoints, PolyUtil.isClosedPolygon(latLngPoints), true, toleranceEarthVal);
                if (resultIndex == -1) {
                    devCount = devCount + 1;
                    updateStorage("RIDE_WAYPOINT_DEVIATION_COUNT", String.valueOf(devCount));
                    Integer storage_dev_count = Integer.parseInt(getValueFromStorage("RIDE_WAYPOINT_DEVIATION_COUNT"));
                    Log.d("ride deviation count", String.valueOf(storage_dev_count));
                } else {
                    updateStorage("RIDE_WAYPOINT_DEVIATION_COUNT", String.valueOf(0));
                    Log.d("in path", String.valueOf(resultIndex));
                }
            }

        } catch (Exception e) {
            Log.e("deviation update exception", e.toString());
        }
    }

    /*Location update API call*/
    private void callDriverCurrentLocationAPI(double latitude, double longitude, float accuracy, String locTime, String log, String locationSource, String triggerFunctionValue) {
        try {
            String localStage = getValueFromStorage("LOCAL_STAGE");

            if (localStage.equals("RideStarted") && accuracy<=50.0) {
                if (rideWaypoints == null) {
                    updateStorage("RIDE_WAYPOINT_DEVIATION_COUNT", String.valueOf(0));
                    ExecutorService executor1 = Executors.newSingleThreadExecutor();
                    executor1.execute(() -> {
                        try {
                            Handler routeHandler = new Handler(Looper.getMainLooper());
                            String rideStartLat = getValueFromStorage("RIDE_START_LAT");
                            String rideStartLon = getValueFromStorage("RIDE_START_LON");
                            String rideEndLat = getValueFromStorage("RIDE_END_LAT");
                            String rideEndLon = getValueFromStorage("RIDE_END_LON");

                            if (rideStartLat == null || rideStartLon == null || rideEndLat == null || rideEndLon == null) {
                                return;
                            }

                            getRoute(Double.parseDouble(rideStartLat), Double.parseDouble(rideStartLon), Double.parseDouble(rideEndLat), Double.parseDouble(rideEndLon));
                            routeHandler.post(() -> {
                                updateDeviation(latitude, longitude, rideWaypoints);
                                checkIfNotMoving(latitude, longitude, accuracy);
                            });
                        } catch (Exception e) {
                            Log.e("get route exception", e.toString());
                        }
                    });
                } else {
                    updateDeviation(latitude, longitude, rideWaypoints);
                    checkIfNotMoving(latitude, longitude, accuracy);
                }
            } else if (localStage.equals("RideCompleted")) {
                rideWaypoints = null;
            }
        } catch (Exception e) {
            Log.e("callDriverCurrentLocationAPI", e.toString());
        }

        ExecutorService executor = Executors.newSingleThreadExecutor();
        Handler handler = new Handler(Looper.getMainLooper());
        executor.execute(() ->
        {
            StringBuilder result = new StringBuilder();
            SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);

            try {
                BatteryManager bm = (BatteryManager) context.getSystemService(BATTERY_SERVICE);
                int batteryPercentageValue = bm.getIntProperty(BatteryManager.BATTERY_PROPERTY_CAPACITY);
                boolean isCharging = bm.isCharging();
                updateJSONObject(batteryPercentage, "batteryPercentage", batteryPercentageValue);
                updateJSONObject(isOnCharge, "isOnCharge", isCharging);
                updateJSONObject(triggerFunction, "triggerFunction", triggerFunctionValue);
            } catch (JSONException e) {
                e.printStackTrace();
                Log.d("LOG_TAG", "Json exception while putting data in metaData" + e);
            }

            String token = sharedPref.getString("REGISTERATION_TOKEN", "null");
            String isValidTime = sharedPref.getString("IS_VALID_TIME", "true");
            String makeNullAPICall = sharedPref.getString("MAKE_NULL_API_CALL", "NO");
            String demoModePassword = sharedPref.getString("DEMO_MODE_PASSWORD", "null");
            String isDemoModeEnabled = sharedPref.getString("IS_DEMOMODE_ENABLED", "null");
            String bundle_version = sharedPref.getString("BUNDLE_VERSION", "null");
            String version = sharedPref.getString("VERSION_NAME", "null");
            String baseUrl = sharedPref.getString("BASE_URL", "null");
            String deviceDetails = sharedPref.getString("DEVICE_DETAILS", "null");
            String bufferedLocationObjects = sharedPref.getString(LOCATION_PAYLOAD, null);
            JSONArray locationPayload;
            if (bufferedLocationObjects != null) {
                try {
                    locationPayload = new JSONArray(bufferedLocationObjects);
                    String rideStatus = getValueFromStorage("IS_RIDE_ACTIVE");

                    maximumLimit = rideStatus!=null && rideStatus.equals("false") ? maximumLimitNotOnRide : maximumLimit;

                    while (locationPayload.length() >= maximumLimit) {
                        int index = (pointsToRemove++) % (locationPayload.length() - 1);
                        if (index != 0) locationPayload.remove(index);
                        if (pointsToRemove > 1000000) pointsToRemove = 1;
                    }
                } catch (JSONException e) {
                    e.printStackTrace();
                    Log.d("LOG_TAG", "Unable to parse buffered Locations from sharedPref" + e);
                    locationPayload = new JSONArray();
                }
            } else {
                locationPayload = new JSONArray();
            }
            JSONArray metaData;
            metaData = metaDataForLocation;
            metaData.put(batteryPercentage);
            metaData.put(isOnCharge);
            metaData.put(triggerFunction);
            try {
                // endPoint for location update
                if (!token.equals("__failed") && isValidTime.equals("true")) {
                    String orderUrl = baseUrl + "/driver/location";
                    System.out.println("LOCATION_UPDATE: Log by " + log + "baseUrl - " + orderUrl);

                    final SimpleDateFormat f = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'", new Locale("en", "US"));
                    f.setTimeZone(TimeZone.getTimeZone("UTC"));
                    String getCurrTime = f.format(new Date());


                    // Http connection for making API call
                    HttpURLConnection connection = (HttpURLConnection) (new URL(orderUrl).openConnection());
                    if (connection instanceof HttpsURLConnection)
                        ((HttpsURLConnection) connection).setSSLSocketFactory(new TLSSocketFactory());
                    connection.setRequestMethod("POST");
                    connection.setRequestProperty("Content-Type", "application/json");
                    connection.setRequestProperty("x-client-version", version);
                    connection.setRequestProperty("source", log);
                    connection.setRequestProperty("token", token);
                    connection.setRequestProperty("x-bundle-version", bundle_version);
                    connection.setRequestProperty("x-device", deviceDetails);
                    String merchantId = getValueFromStorage("MERCHANT_ID");
                    String vehicleVariant = getValueFromStorage("VEHICLE_VARIANT");
                    String driverMode = getValueFromStorage("DRIVER_STATUS_N");
                    if (merchantId != null) connection.setRequestProperty("mId", merchantId);
                    if (vehicleVariant != null) connection.setRequestProperty("vt", vehicleVariant);
                    if (driverMode != null) connection.setRequestProperty("dm", driverMode.toUpperCase());
                    connection.setDoOutput(true);

                    // Request Body for API call
                    JSONObject point = new JSONObject();
                    JSONObject locationData = new JSONObject();
                    if (isDemoModeEnabled.equals("true")) {
                        switch (demoModePassword) {
                            case "7891234":
                                point.put("lat", 13.311895563147432);
                                point.put("lon", 76.93981481869986);
                                break;
                            case "8917234":
                                point.put("lat", 13.260559676317829);
                                point.put("lon", 76.4785809882692);
                                break;
                            case "9178234":
                                point.put("lat", 13.160550263780683);
                                point.put("lon", 76.66727044721313);
                                break;
                            case "1789234":
                                point.put("lat", 12.522069908884921);
                                point.put("lon", 76.89518072273476);
                                break;
                            default:
                                point.put("lat", latitude);
                                point.put("lon", longitude);
                                updateStorage(LAST_LOCATION_TIME, locTime);
                                break;
                        }
                    } else {
                        if (latitude == 0.0 || longitude == 0.0) {
                            if (makeNullAPICall.equals("NO")) return;
                            point.put("lat", null);
                            point.put("lon", null);
                        } else {
                            point.put("lat", latitude);
                            point.put("lon", longitude);
                            updateStorage(LAST_LOCATION_TIME, locTime);
                        }

                    }

                    locationData.put("pt", point);
                    locationData.put("ts", locTime);
                    locationData.put("acc", accuracy);
                    locationData.put("source", locationSource);
                    if (metaData.length() != 0) locationData.put("metaData", metaData);
                    if (!locationData.has("pt")) return;
                    locationPayload.put(locationData);
                    updateStorage(LOCATION_PAYLOAD, locationPayload.toString());
                    Log.d("LOG_TAG", "Location payload for API call" + locationPayload);
                    System.out.println("LOCATION_UPDATE: PAYLOAD CREATED :- " + locationPayload);
                    Log.d(LOG_TAG, "condition 1  ::: " + (token.equals("null")));
                    Log.d(LOG_TAG, "condition 2  ::: " + !(token.equals("null")));
                    Log.d(LOG_TAG, "condition 3  ::: " + sharedPref.getString("REGISTERATION_TOKEN", "null"));
                    OutputStream stream = connection.getOutputStream();
                    stream.write(locationPayload.toString().getBytes());
                    connection.connect();

                    int respCode = connection.getResponseCode();
                    InputStreamReader respReader;
                    Log.d(LOG_TAG, "respCode " + respCode);

                    if ((respCode < 200 || respCode >= 300) && respCode != 302) {
                        respReader = new InputStreamReader(connection.getErrorStream());
                        System.out.println("LOCATION_UPDATE: ERROR API respReader :- " + respReader);
                        Log.d(LOG_TAG, "in error " + respReader);
                    } else {
                        respReader = new InputStreamReader(connection.getInputStream());
                        Log.d(LOG_TAG, "in 200 " + respReader);
                        System.out.println("LOCATION_UPDATE: SUCCESS API respReader :- " + respReader);
//                        payload = new JSONArray();
                        updateStorage(LOCATION_PAYLOAD, new JSONArray().toString());
                        for (int i = 0; i < updateTimeCallbacks.size(); i++) {
                            updateTimeCallbacks.get(i).triggerUpdateTimeCallBack(getCurrTime, String.valueOf(latitude), String.valueOf(longitude));
                        }
                    }
                    BufferedReader in = new BufferedReader(respReader);
                    String inputLine;
                    while ((inputLine = in.readLine()) != null) {
                        result.append(inputLine);
                    }
                    System.out.println("LOCATION_UPDATE: API result :- " + result);
                    Log.d(LOG_TAG, "in result OVERALL " + result);
                }
            } catch (Exception e) {
                Log.d(LOG_TAG, "Catch in callDriverCurrentLocationAPI : " + e);
            }

            handler.post(() -> {
                try {
                    JSONObject resp = new JSONObject(String.valueOf(result));
                    if (resp.has("errorCode") && resp.get("errorCode").equals("INVALID_TOKEN")) {
                        System.out.println("Inisde Invalid token " + resp.get("errorCode"));
                        updateStorage("REGISTERATION_TOKEN", "__failed");
                        cancelTimer();
                        onDestroy();
                        executor.shutdown();
                    }
                } catch (JSONException e) {
                    e.printStackTrace();
                }
            });
        });
    }

    // HELPER FUNCTIONS
    /* creates location request */
    private LocationRequest createLocationRequest(int intervalForLocationUpdate, float minDispDistance) {
        return new LocationRequest.Builder(Priority.PRIORITY_HIGH_ACCURACY)
                .setIntervalMillis(intervalForLocationUpdate)
                .setMinUpdateDistanceMeters(minDispDistance)
                .build();
    }

    /*Creating channel for sticky notification*/
    private void createNotificationChannel() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            String LOCATION_DESCRIPTION = "LOCATION_IS_UPDATING";
            NotificationChannel channel = new NotificationChannel(LOCATION_UPDATES, LOCATION_SERVICE, NotificationManager.IMPORTANCE_MIN);
            channel.setDescription(LOCATION_DESCRIPTION);
            NotificationManager notificationManager = getSystemService(NotificationManager.class);
            notificationManager.createNotificationChannel(channel);
        }
    }

    /* returns notification for foreground services */
    private Notification createNotification() {
        createNotificationChannel();
        Intent notificationIntent = getPackageManager().getLaunchIntentForPackage(context.getPackageName());
        PendingIntent pendingIntent = PendingIntent.getActivity(this, 10, notificationIntent, PendingIntent.FLAG_IMMUTABLE);
        NotificationCompat.Builder notification =
                new NotificationCompat.Builder(this, LOCATION_UPDATES)
                        .setContentTitle("Updating")
                        .setContentText(getString(R.string.your_location_is_being_updated))
                        .setSmallIcon(Utils.getResIdentifier(context,"ic_launcher", "drawable"))
                        .setPriority(NotificationCompat.PRIORITY_MIN)
                        .setOngoing(true)
                        .setContentIntent(pendingIntent);
        return notification.build();
    }

    /* Creating alert notification to notify that he is offline */
    private void showAlertNotification() {
        System.out.println("Notification");
        SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        String token = sharedPref.getString("REGISTERATION_TOKEN", "null");
        if (token.equals("null") || token.equals("__failed")) return;
        Intent notificationIntent = getPackageManager().getLaunchIntentForPackage(context.getPackageName());
        PendingIntent pendingIntent = PendingIntent.getActivity(this, alertNotificationId, notificationIntent, PendingIntent.FLAG_IMMUTABLE);
        NotificationCompat.Builder mBuilder = new NotificationCompat.Builder(context, "General");
        mBuilder.setLargeIcon(BitmapFactory.decodeResource(getResources(), Utils.getResIdentifier(context,"ic_launcher", "drawable")));
        mBuilder.setContentTitle(getString(R.string.we_made_you_offline))
                .setSmallIcon(Utils.getResIdentifier(context,"ic_launcher", "drawable"))
                .setContentText(getString(R.string.location_is_turned_off_permission_is_disabled))
                .setAutoCancel(true)
                .setPriority(NotificationCompat.PRIORITY_MAX);
        mBuilder.setContentIntent(pendingIntent);
        NotificationManagerCompat notificationManager = NotificationManagerCompat.from(context);
        if (ActivityCompat.checkSelfPermission(this, Manifest.permission.POST_NOTIFICATIONS) != PackageManager.PERMISSION_GRANTED) {
            Log.e(LOG_TAG, "no notification permission");
            // TODO: Consider calling
            //    ActivityCompat#requestPermissions
            // here to request the missing permissions, and then overriding
            //   public void onRequestPermissionsResult(int requestCode, String[] permissions,
            //                                          int[] grantResults)
            // to handle the case where the user grants the permission. See the documentation
            // for ActivityCompat#requestPermissions for more details.
            return;
        }
        notificationManager.notify(alertNotificationId, mBuilder.build());

        startGPSListeningService();
    }

    private void startGPSListeningService() {
        try {
            Intent gpsListeningService = new Intent(this, GpsListeningService.class);
            SharedPreferences sharedPrefs = getApplicationContext().getSharedPreferences(this.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
            gpsListeningService.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK | Intent.FLAG_ACTIVITY_NEW_TASK);
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S && sharedPrefs.getString("ACTIVITY_STATUS", "null").equals("onPause")) {
                AlarmManager manager = (AlarmManager) context.getSystemService(Context.ALARM_SERVICE);
                Intent alarmIntent = new Intent(context, GPSBroadcastReceiver.class);
                PendingIntent pendingIntent = PendingIntent.getBroadcast(context, 0, alarmIntent, PendingIntent.FLAG_IMMUTABLE);
                manager.setExact(AlarmManager.RTC_WAKEUP, System.currentTimeMillis(), pendingIntent);
            } else if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                this.getApplicationContext().startForegroundService(gpsListeningService);
            } else {
                this.startService(gpsListeningService);
            }
        } catch (Exception e) {
            FirebaseAnalytics.getInstance(this).logEvent("Exception_in_startGPSListeningService", null);
            Log.e(LOG_TAG, "Error in startGPSListeningService : " + e);
        }
    }

    /* creates the Location callback  */
    private LocationCallback getLocationCallback() {
        System.out.println("LOCATION_UPDATE: Created Location CallBack");
        locationCallback = new LocationCallback() {
            @Override
            public void onLocationResult(@NonNull LocationResult locationResult) {
                super.onLocationResult(locationResult);
                Location lastLocation = locationResult.getLastLocation();
                if (lastLocation != null) {
                    updated = true;
                    Log.e("startLocationUpdates", lastLocation.getLatitude() + "/" + lastLocation.getLongitude());
                    double lat = lastLocation.getLatitude();
                    double lng = lastLocation.getLongitude();
                    float acc = lastLocation.getAccuracy();
                    lastLatitudeValue = lat;
                    lastLongitudeValue = lng;
                    long locTimeMilliSeconds = lastLocation.getTime();
                    SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'", new Locale("en", "US"));
                    sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
                    Date locTime = new Date(locTimeMilliSeconds);
                    String thisLocationTimeStamp = sdf.format(locTime);
                    SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
                    String isValidTime = sharedPref.getString("IS_VALID_TIME", "true");
                    if (isValidTime.equals("true")) {
                        updateStorage("LAST_KNOWN_LAT", String.valueOf(lastLatitudeValue));
                        updateStorage("LAST_KNOWN_LON", String.valueOf(lastLongitudeValue));
                        callDriverCurrentLocationAPI(lat, lng, acc, thisLocationTimeStamp, "fused_location_callback", LocationSource.LastLocation.toString(), TriggerFunction.GoogleCallback.toString());
                    }
                    prevLat = lastLatitudeValue;
                    prevLon = lastLongitudeValue;
                }
            }
        };
        return locationCallback;
    }

    /* check all the cases of location permission */
    private void checkLocation() {
        if (ActivityCompat.checkSelfPermission(context, ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED && ActivityCompat.checkSelfPermission(context, Manifest.permission.ACCESS_COARSE_LOCATION) != PackageManager.PERMISSION_GRANTED) {
            showAlertNotification();
            onDestroy();
        } else if (!isLocationEnabled()) {
            updateDriverStatus(false);
        } else {
            NotificationManager notificationManager = (NotificationManager) context.getSystemService(Context.NOTIFICATION_SERVICE);
            StatusBarNotification[] currentNotifications = notificationManager.getActiveNotifications();
            for (StatusBarNotification currentNotification : currentNotifications) {
                if (currentNotification.getId() == alertNotificationId) {
                    notificationManager.cancel(alertNotificationId);
                }
            }
        }
    }

    /*  returns true if location enabled
        return false or if location disabled    */
    private boolean isLocationEnabled() {
        LocationManager locationManager = (LocationManager) context.getSystemService(Context.LOCATION_SERVICE);
        return locationManager != null && LocationManagerCompat.isLocationEnabled(locationManager);
    }

    /* create timer task */
    @SuppressLint("MissingPermission")
    private TimerTask createTimer() {
        System.out.println("LOCATION_UPDATE: Created Timer");
        timer = new Timer();
        /* triggering the location update explicitly if we are not getting any updates for 5sec */
        timerTask = new TimerTask() {
            @Override
            public void run() {
                //                    if (updated == true && lastUpdatedTime != null && checkIfUpdateRequired(lastUpdatedTime)) { NEED TO HANDLE THIS MORE CONFIDENTLY
//                        if (timer != null)
//                            timer.cancel();
//                        updated = false;
//                    } else
                if (timerTask != null) {
                    System.out.println("LOCATION_UPDATE: Created Timer where not null");
                    System.out.println("Timer now is : " + timer);
                    timer = new Timer();
                    checkLocation();
                    if (gpsMethodSwitch.equals("CURRENT")) {
                        System.out.println("LOCATION_UPDATE: CURRENT LOCATION FETCHED BY GPS");
                        fusedLocationProviderClient.getCurrentLocation(Priority.PRIORITY_HIGH_ACCURACY, cancellationTokenSource.getToken())
                                .addOnSuccessListener(location -> {
                                    SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'", new Locale("en", "US"));
                                    sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
                                    if (location != null) {
                                        long locTimeMilliSeconds = location.getTime();
                                        Date locTime = new Date(locTimeMilliSeconds);
                                        String thisLocationTimeStamp = sdf.format(locTime);
                                        SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
                                        String isValidTime = sharedPref.getString("IS_VALID_TIME", "true");
                                        if (isValidTime.equals("true")) {
                                            updateStorage("LAST_KNOWN_LAT", String.valueOf(lastLatitudeValue));
                                            updateStorage("LAST_KNOWN_LON", String.valueOf(lastLongitudeValue));
                                            callDriverCurrentLocationAPI(location.getLatitude(), location.getLongitude(), location.getAccuracy(), thisLocationTimeStamp, "timer_task", LocationSource.CurrentLocation.toString(), TriggerFunction.TimerTask.toString());
                                        }
                                    } else {
                                        System.out.println("LOCATION_UPDATE: CURRENT LOCATION IS NULL");
                                        callDriverCurrentLocationAPI(0.0, 0.0, 0, sdf.format(new Date()), "timer_task_null_location", LocationSource.CurrentLocation.toString(), TriggerFunction.TimerTask.toString());
                                    }
                                })
                                .addOnFailureListener(Throwable::printStackTrace);
                    } else {
                        System.out.println("LOCATION_UPDATE: LAST KNOWN LOCATION FETCHED BY GPS");
                        fusedLocationProviderClient.getLastLocation()
                                .addOnSuccessListener(location -> {
                                    SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
                                    String isValidTime = sharedPref.getString("IS_VALID_TIME", "true");
                                    if (location != null && isValidTime.equals("true")) {
                                        updateStorage("LAST_KNOWN_LAT", String.valueOf(lastLatitudeValue));
                                        updateStorage("LAST_KNOWN_LON", String.valueOf(lastLongitudeValue));
                                        long locTimeMilliSeconds = location.getTime();
                                        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'", new Locale("en", "US"));
                                        sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
                                        Date locTime = new Date(locTimeMilliSeconds);
                                        String thisLocationTimeStamp = sdf.format(locTime);
                                        callDriverCurrentLocationAPI(location.getLatitude(), location.getLongitude(), location.getAccuracy(), thisLocationTimeStamp, "COMING FROM TIMER", LocationSource.LastLocation.toString(), TriggerFunction.TimerTask.toString());
                                    }
                                })
                                .addOnFailureListener(Throwable::printStackTrace);
                    }

                    System.out.println("Inside else of handler");
                }
            }
        };
        return timerTask;
    }

    // to cancel timer
    private void cancelTimer() {
        try {
            System.out.println("LOCATION_UPDATE: CANCEL TIMER CALLED INSIDE");
            if (timer != null) {
                timer.cancel();
                timer.purge();
            }
            if (timerTask != null)
                timerTask.cancel();
            timer = null;
            timerTask = null;
            isLocationUpdating = false;
        } catch (Exception e) {
            FirebaseAnalytics.getInstance(this).logEvent("Exception_in_cancelTimer", null);
            Log.e(LOG_TAG, "Error in cancelTimer " + e);
        }
    }

    private void logEventForHealthCheck(Intent intent) {
        if (intent != null) {
            String serviceStartingSource = intent.getStringExtra("StartingSource");
            if (serviceStartingSource != null) {
                if (serviceStartingSource.equals("TRIGGER_SERVICE")) {
                    FirebaseAnalytics.getInstance(this).logEvent("service_triggered_by_health_check", new Bundle());
                }
            }
        }
    }

    private void updateStorage(String key, String value) {
        SharedPreferences sharedPref = context.getSharedPreferences(
                context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        SharedPreferences.Editor editor = sharedPref.edit();
        editor.putString(key, value);
        editor.apply();
    }

}