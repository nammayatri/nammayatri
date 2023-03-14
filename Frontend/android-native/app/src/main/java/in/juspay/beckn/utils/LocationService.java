package in.juspay.beckn.utils;

import android.Manifest;
import android.annotation.SuppressLint;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.location.Location;
import android.location.LocationListener;
import android.location.LocationManager;
import android.os.AsyncTask;
import android.os.Bundle;
import android.os.Handler;
import android.os.IBinder;
import android.os.Looper;
import android.util.Log;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.app.ActivityCompat;

import com.google.android.gms.location.FusedLocationProviderClient;
import com.google.android.gms.location.LocationAvailability;
import com.google.android.gms.location.LocationCallback;
import com.google.android.gms.location.LocationRequest;
import com.google.android.gms.location.LocationResult;
import com.google.android.gms.location.LocationServices;
import com.google.android.gms.tasks.OnFailureListener;
import com.google.android.gms.tasks.OnSuccessListener;
import com.google.android.gms.tasks.Task;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.net.HttpURLConnection;
import java.net.InetAddress;
import java.net.Socket;
import java.net.URL;
import java.net.URLEncoder;
import java.net.UnknownHostException;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.security.cert.X509Certificate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Map;
import java.util.TimeZone;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;

import in.juspay.beckn.BuildConfig;
import in.juspay.beckn.R;
import in.juspay.beckn.BootUpReceiver;
import in.juspay.beckn.R;
import in.juspay.hypersdk.core.JuspayServices;

public class LocationService extends Service {

    final static int serviceId = 137628;
    private JuspayServices juspayServices;
    private static final String LOG_TAG = "LocationService";
    FusedLocationProviderClient client;
    private LocationListener listener;
    private boolean serviceRunning = false;
    private JuspayServices juspayServicesGlobal;
    double lastLat;
    double lastLng ;
    float accuracy;
    private Timer timer;
    private String role;
    private String registration_token = null;
    JSONArray payload = new JSONArray();
    public interface UpdateTimeCallback{
        public void timeUpdateFlow(String time);
    }
    private static ArrayList<UpdateTimeCallback> timeUpdateCallback = new ArrayList<>();
    public  static void registerCallback(UpdateTimeCallback timeUpdateCallback)
    {
        LocationService.timeUpdateCallback.add(timeUpdateCallback);
    }
    public static void deRegisterCallback(UpdateTimeCallback timeUpdateCallback)
    {
        LocationService.timeUpdateCallback.remove(timeUpdateCallback);
    }

    String versionName = BuildConfig.VERSION_NAME;

    @Override
    public void onCreate() {
        super.onCreate();
        try{
            startForeground(serviceId, NotificationUtils.createNotification(getApplicationContext(), "Updating","location", new JSONObject("{entitiy_ids: Nothing, entity_type: Nothing, notification_type: Nothing}")));
        }
        catch(Exception e) {
            e.printStackTrace();
        }
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        boolean triggeredUpdate;
        try {
            triggeredUpdate = intent.getBooleanExtra("triggered_update",false);
        }catch (Exception e){
            triggeredUpdate = false;
            e.printStackTrace();
        } 
        serviceRunning = true;
        if (intent != null) {
            try {
                String action = intent.getStringExtra("action");
                if (action != null) {
                    switch (action) {
                        case "start":
                            try {
                                role = intent.getStringExtra("role");
                                String baseUrl = intent.getStringExtra("baseUrl");
                                String shared_prefs_key = intent.getStringExtra("shared_prefs_key");
                                String registration_token = intent.getStringExtra("registration_token");
                                String caseId = intent.getStringExtra("caseId");
                                int millis = intent.getIntExtra("interval", 10000);
                                //startForeground(serviceId, NotificationUtils.createNotification(getApplicationContext(), title, content, new JSONObject(data)));
                                startTimerTask(baseUrl, caseId, shared_prefs_key, millis, registration_token, versionName);
                                startLocationManager();
                            } catch (Exception e) {
                                e.printStackTrace();
                            }
                            break;
                        default:
                            if (timer != null) {
                                timer.cancel();
                            }
                            serviceRunning = false;
                            stopForeground(true);
                            stopSelf();

                    }
                } else {
                    if (timer != null) {
                        timer.cancel();
                    }
                    serviceRunning = false;
                    stopForeground(true);
                    stopSelf();


                }
            }catch (Exception e){
                Log.e(LOG_TAG, e.toString());
            }
        }
        return START_STICKY;
    }

    public void startLocationManager(){
        if (ActivityCompat.checkSelfPermission(getBaseContext()
                , Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED
                && ActivityCompat.checkSelfPermission(getBaseContext()
                , Manifest.permission.ACCESS_COARSE_LOCATION) != PackageManager.PERMISSION_GRANTED) {
            return;
        }

        LocationManager locationManager =
                (LocationManager) this.getSystemService(Context.LOCATION_SERVICE);
        final boolean gpsEnabled = locationManager.isProviderEnabled(LocationManager.GPS_PROVIDER);
        String provider;
        if(gpsEnabled){
            provider = LocationManager.GPS_PROVIDER;
        } else{
            provider = LocationManager.NETWORK_PROVIDER;
        }

        listener = new LocationListener() {

            @Override
            public void onLocationChanged(Location location) {
                // A new location update is received.  Do something useful with it.  In this case,
                // we're sending the update to a handler which then updates the UI with the new
                // location.
                lastLat = location.getLatitude();
                lastLng = location.getLongitude();
                accuracy = location.getAccuracy();
                Log.d(LOG_TAG, "onLocationChanged" + lastLat + lastLng + accuracy);
            }

            @Override
            public void onStatusChanged(String provider, int status, Bundle extras) {

            }

            @Override
            public void onProviderEnabled(String provider) {

            }

            @Override
            public void onProviderDisabled(String provider) {

            }
        };

        locationManager.requestLocationUpdates(provider,
                10000,          // 10-second interval.
                10,             // 10 meters.
                listener);
    }

    public void startTimerTask(String baseUrl, String caseId, String shared_prefs_key, int time, String registration_token, String versionName){
        if(timer != null){
            timer.cancel();
        }
        timer = new Timer();
        TimerTask timerTask = new TimerTask() {
            @Override
            public void run() {
                getCurrentLocation(baseUrl, caseId, shared_prefs_key, registration_token,versionName);
            }
        };
        timer.scheduleAtFixedRate(timerTask, 0, time);

    }

    public void getCurrentLocation(String baseUrl, String caseId,String shared_prefs_key, String registration_token, String versionName) {
        if (ActivityCompat.checkSelfPermission(getBaseContext()
                , Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED
                && ActivityCompat.checkSelfPermission(getBaseContext()
                , Manifest.permission.ACCESS_COARSE_LOCATION) != PackageManager.PERMISSION_GRANTED) {
            return;
        }
        if(client == null){
            client =LocationServices.getFusedLocationProviderClient(getApplicationContext());
        }
        Task location = client.getLastLocation();
        location.addOnSuccessListener(new OnSuccessListener<Location>() {
            @Override
            public void onSuccess(Location location) {
                    if (location != null) {
                        double lat = location.getLatitude();
                        double lng = location.getLongitude();
                        float acc = location.getAccuracy();
                        callLatLongApi(lat, lng,acc, baseUrl , caseId, shared_prefs_key,registration_token,versionName);
                        Log.d(LOG_TAG, "getCurrentPosition" + lat + lng + acc);
                    } else {
                        // startLocationUpdates();
                        // callLatLongApi(lastLat, lastLng, baseUrl , caseId, shared_prefs_key,registration_token);
                        Log.d(LOG_TAG, "getCurrentPosition Fallback point1" + lastLat + lastLng + accuracy);
                    }
            }
        }).addOnFailureListener( new OnFailureListener() {
            @Override
            public void onFailure(@NonNull Exception e) {
                callLatLongApi(lastLat, lastLng, accuracy, baseUrl , caseId, shared_prefs_key,registration_token,versionName);
                Log.d(LOG_TAG, "getCurrentPosition  Fallback point2" + lastLat + lastLng +accuracy);
            }
        });

    }

    @SuppressLint("StaticFieldLeak")
    public void callLatLongApi (double lat, double lng, float acc, final String baseurl, final String caseId, final String shared_prefs_key, String registration_token , final String versionName){
    ExecutorService executor = Executors.newSingleThreadExecutor();
    Handler handler = new Handler(Looper.getMainLooper());
    executor.execute(() -> {
        StringBuilder result = new StringBuilder();
        System.out.println("CallLatLongAPI");
        SharedPreferences sharedPref = getApplicationContext().getSharedPreferences(
                        getApplicationContext().getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        String adId = sharedPref.getString("AD_ID", "null");
        try {
            String orderUrl = baseurl + "/driver/location"; //+ caseId;
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
            connection.setRequestProperty("x-client-version", versionName);
            connection.setRequestProperty("x-client-adId", adId);
            connection.setRequestProperty("token", registration_token);

            connection.setDoOutput(true);

            JSONObject pt = new JSONObject();
            JSONObject waypointss = new JSONObject();
            pt.put("lat", lat);
            pt.put("lon", lng );
            waypointss.put("pt",pt);
            waypointss.put("ts",getCurrTime);
            waypointss.put("acc",acc);
            payload.put(waypointss);
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
                f.setTimeZone(TimeZone.getTimeZone("UTC"));
                getCurrTime = f.format(new Date());
                for(int i =0;i<timeUpdateCallback.size();i++)
                {
                    timeUpdateCallback.get(i).timeUpdateFlow(getCurrTime);
                }
                respReader = new InputStreamReader(connection.getInputStream());
                Log.d(LOG_TAG, "in 200 "+ respReader);
            }

            BufferedReader in = new BufferedReader(respReader);
            String inputLine;

            while ((inputLine = in.readLine()) != null) {
                result.append(inputLine);
            }
            Log.d(LOG_TAG, "in result "+ result.toString());
        }
        catch (java.net.UnknownHostException e) {
            try {
                String isOnRide = sharedPref.getString("IS_ONRIDE", "null");
                System.out.println("NO internet -> LocationService");
                if (isOnRide.equals("true")){
                    final SimpleDateFormat f = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");
                    System.out.println(" in SimpleDateFormat : " + f);
                    Log.d(LOG_TAG, "Catch in locationupdateapi without internet inside if : "+e);
                    f.setTimeZone(TimeZone.getTimeZone("UTC"));
                    String getCurrTime = f.format(new Date());
                    JSONObject pt = new JSONObject();
                    JSONObject waypointss = new JSONObject();
                    pt.put("lat", lat);
                    pt.put("lon", lng );
                    waypointss.put("pt",pt);
                    waypointss.put("ts",getCurrTime);
                    waypointss.put("acc",acc);
                    payload.put(waypointss);
                }
                else payload = new JSONArray();
                Log.d(LOG_TAG, "Catch in locationupdateapi without internet outside if : "+e);
            }
            catch(Exception ee){
                ee.printStackTrace();
            }
            Log.d(LOG_TAG, "Catch in locationupdateapi without internet : "+e);
//            return result.toString();
        }
        catch (Exception ignored) {
            Log.d(LOG_TAG, "Catch in locationupdateapi : "+ignored);
        }

        handler.post(() ->{
            try {
                JSONObject resp = new JSONObject(String.valueOf(result));
                System.out.println("String s " + resp);
                System.out.println("errorCodeeeeeee " +resp.get("errorCode") );
                System.out.println("ROLE OF THE LOCATIONSERVICE " + role);
                if((resp.get("errorCode").equals("INVALID_TOKEN"))|| !role.equals("DRIVER")) {
                    System.out.println("Inisde Invalid token " + resp.get("errorCode") );
                    serviceRunning = false;
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

    public static String generateQueryString(@Nullable Map<String, String> queryString)
            throws UnsupportedEncodingException {
        StringBuilder sb = new StringBuilder();
        if (queryString != null) {
            for (Map.Entry<String, String> e : queryString.entrySet()) {
                if (sb.length() > 0) {
                    sb.append('&');
                }
                sb.append(URLEncoder.encode(e.getKey(), "UTF-8")).append('=')
                        .append(URLEncoder.encode(e.getValue(), "UTF-8"));
            }
        }

        return sb.toString();
    }

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    protected void onPostExecute(String result) {
        System.out.println(" in onPostExecute : " + result);
    }

    @Override
    public void onTaskRemoved(Intent rootIntent){
        Intent broadcastIntent = new Intent();
        System.out.println("Inside onTaskRemoved");
        broadcastIntent.setAction("restartservice");
        broadcastIntent.setClass(this, BootUpReceiver.class);
        SharedPreferences sharedPref = this.getSharedPreferences(
                this.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        String location_status = sharedPref.getString("LOCATION_STATUS", "PAUSE");
       if(location_status.equals("START") && !(ActivityCompat.checkSelfPermission(getApplicationContext(), Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED && ActivityCompat.checkSelfPermission(getApplicationContext(), Manifest.permission.ACCESS_COARSE_LOCATION) != PackageManager.PERMISSION_GRANTED)){
           this.sendBroadcast(broadcastIntent);
       }
        // this.sendBroadcast(broadcastIntent);
        super.onTaskRemoved(rootIntent);
    }


}
