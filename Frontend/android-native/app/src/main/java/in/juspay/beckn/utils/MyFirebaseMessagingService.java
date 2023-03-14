package in.juspay.mobility.utils;

import static in.juspay.hypersdk.utils.GPayUtils.LOG_TAG;

import android.Manifest;
import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.location.Location;
import android.os.Build;
import android.os.Handler;
import android.os.Looper;
import android.provider.Settings;
import android.util.Log;
import androidx.annotation.NonNull;
import androidx.annotation.RequiresApi;
import androidx.core.app.ActivityCompat;
import com.google.android.gms.location.FusedLocationProviderClient;
import com.google.android.gms.location.LocationAvailability;
import com.google.android.gms.location.LocationCallback;
import com.google.android.gms.location.LocationRequest;
import com.google.android.gms.location.LocationResult;
import com.google.android.gms.location.LocationServices;
import com.google.firebase.messaging.FirebaseMessagingService;
import com.google.firebase.messaging.RemoteMessage;
import org.json.JSONObject;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.net.ssl.HttpsURLConnection;

import in.juspay.mobility.R;

public class MyFirebaseMessagingService extends FirebaseMessagingService {

    private static final String TAG = "MyFirebaseMessagingServ";
    private FusedLocationProviderClient client;
    private String lastLatitudeValue;
    private String lastLongitudeValue;
    String baseUrl = new String();

    @Override
    public void onNewToken(@NonNull String newToken){
        super.onNewToken(newToken);
        Log.e("newToken", newToken);
        String deviceToken = newToken;
        SharedPreferences sharedPref = this.getSharedPreferences(
                this.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        SharedPreferences.Editor editor = sharedPref.edit();
        editor.putString("FCM_TOKEN", newToken);
        editor.apply();
        String regToken = sharedPref.getString("REGISTERATION_TOKEN", "null");
//        String baseUrl = null;
        if (!regToken.equals("null") && !regToken.equals("__failed"))
        {
            updateFCMToken(deviceToken);
        }
        Log.e("newToken", newToken);
    }

    @RequiresApi(api = Build.VERSION_CODES.M)
    @Override
    public void onMessageReceived(@NonNull RemoteMessage remoteMessage){

        super.onMessageReceived(remoteMessage);
        Log.e("onMessageReceived", remoteMessage.getData().toString());
        System.out.println("onMessageReceived remoteMessage->"+remoteMessage.toString());/**/
        System.out.println("onMessageReceived->"+remoteMessage.getData().toString());/**/
        JSONObject payload = new JSONObject();
        JSONObject notification_payload;
        JSONObject entity_payload;
        try {
            payload.put("notification_type", remoteMessage.getData().get("notification_type"));
            System.out.println("onMessageReceived->"+remoteMessage.getData().get("notification_type"));/**/
            payload.put("entity_ids", remoteMessage.getData().get("entity_ids"));
            System.out.println("onMessageReceived->"+remoteMessage.getData().get("entity_ids"));/**/
            payload.put("entity_type", remoteMessage.getData().get("entity_type"));
            System.out.println("onMessageReceived->"+remoteMessage.getData().get("entity_type"));/**/
            payload.put("show_notification", remoteMessage.getData().get("show_notification"));
            System.out.println("onMessageReceived->"+remoteMessage.getData().get("show_notification"));/**/

            String title;
            String body;
            String icon;
            String imageUrl;
            notification_payload = null;
            entity_payload = null;
            if (remoteMessage.getData().containsKey("notification_json") ) {
                notification_payload = new JSONObject(remoteMessage.getData().get("notification_json"));
            }
            if (remoteMessage.getData().containsKey("entity_data") && remoteMessage.getData().get("notification_type").equals("NEW_RIDE_AVAILABLE") ) {
                entity_payload = new JSONObject(remoteMessage.getData().get("entity_data"));
            }

        RemoteMessage.Notification notification = remoteMessage.getNotification();
        if (notification != null )
        {
            title = notification.getTitle();
            body = notification.getBody();
            icon = notification.getIcon();
            imageUrl = remoteMessage.getData().get("image-url");
        }
        else
        {
            title = notification_payload.get("title").toString();
            body = notification_payload.get("body").toString();
            icon = notification_payload.get("icon").toString();
            if (notification_payload.has("imageUrl"))
            {
                imageUrl = notification_payload.get("imageUrl").toString();
            }
            else
            {
                imageUrl = null;
            }
        }

        if(notification_payload != null || notification != null) {
            System.out.println("--------=======================------");
            System.out.println(notification_payload);
            System.out.println("Firebase");
            System.out.println(title);
            System.out.println(body);
            System.out.println(icon);
            System.out.println(payload);
            System.out.println("--------=======================------");
            SharedPreferences sharedPref = this.getSharedPreferences(this.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
            if (payload.get("notification_type").equals("TRIGGER_SERVICE") && getString(R.string.service).equals("nammayatripartner")) {
                createLocationService();
            }
            else if(payload.get("notification_type").equals("NEW_RIDE_AVAILABLE")){
                sharedPref.edit().putString("RIDE_STATUS", "NEW_RIDE_AVAILABLE").apply();
                NotificationUtils.showAllocationNotification(this, title, body, payload,imageUrl, entity_payload);
            }
            else if(payload.get("show_notification").equals("true")){
                if (payload.get("notification_type").equals("DRIVER_ASSIGNMENT")){
                    sharedPref.edit().putString(getResources().getString(R.string.IS_RIDE_ACTIVE), "true").apply();
                    sharedPref.edit().putString("RIDE_STATUS", "DRIVER_ASSIGNMENT").apply();
                    if (getResources().getString(R.string.service).equals("nammayatripartner") && Settings.canDrawOverlays(getApplicationContext())  && !sharedPref.getString(getResources().getString(R.string.REGISTERATION_TOKEN), "null").equals("null") && (sharedPref.getString(getResources().getString(R.string.ACTIVITY_STATUS), "null").equals("onPause") || sharedPref.getString(getResources().getString(R.string.ACTIVITY_STATUS), "null").equals("onDestroy")) ) {
                        Intent widgetService = new Intent(getApplicationContext(), WidgetService.class);
                        widgetService.putExtra(getResources().getString(R.string.WIDGET_MESSAGE),"New Ride Assigned");
                        try{
                            startService(widgetService);
                        }catch (Exception e) {
                            e.printStackTrace();
                        }
                    }
                }else if (payload.get("notification_type").equals("CANCELLED_PRODUCT")){
                    sharedPref.edit().putString(getResources().getString(R.string.IS_RIDE_ACTIVE), "false").apply();
                    if (getResources().getString(R.string.service).equals("nammayatripartner")) {
                        stopService(new Intent(getApplicationContext(), WidgetService.class));
                    }
                }
                NotificationUtils.showNotification(this, title, body, payload,imageUrl);
            }
            else
                //Handle silent Notifications if required.

                return;
        }
        }catch (Exception e){
            return;
        }
    }

    @SuppressLint("StaticFieldLeak")
    private void updateFCMToken( final String deviceToken){
        SharedPreferences sharedPref = this.getSharedPreferences(
                this.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        String token = sharedPref.getString("REGISTERATION_TOKEN", "null");
        String baseUrl = sharedPref.getString("BASE_URL", "null");
        ExecutorService executor = Executors.newSingleThreadExecutor();
        Handler handler = new Handler(Looper.getMainLooper());
        executor.execute(() -> {
                StringBuilder result = new StringBuilder();
                try {
                    String orderUrl ;
                    if (getString(R.string.service).equals("nammayatripartner")) {
                        orderUrl = baseUrl + "/driver/profile";
                    }
                    else {
                        orderUrl = baseUrl + "/profile";
                        }
                    System.out.print("in updateFCMToken");
                    HttpURLConnection connection = (HttpURLConnection) (new URL(orderUrl).openConnection());
                    if (connection instanceof HttpsURLConnection)
                        ((HttpsURLConnection) connection).setSSLSocketFactory(new TLSSocketFactory());
                    connection.setRequestMethod("POST");
                    connection.setRequestProperty("Content-Type", "application/json");
                    connection.setRequestProperty("token", token);
                    connection.setDoOutput(true);

                    JSONObject payload = new JSONObject();
                    payload.put("deviceToken", deviceToken);

                    OutputStream stream = connection.getOutputStream();
                    stream.write(payload.toString().getBytes());
                    connection.connect();
                    int respCode = connection.getResponseCode();
                    InputStreamReader respReader;

                    if ((respCode < 200 || respCode >= 300) && respCode != 302) {
                        respReader = new InputStreamReader(connection.getErrorStream());
                        System.out.print("in error : " + respReader);
                    } else {
                        respReader = new InputStreamReader(connection.getInputStream());
                        System.out.print("in 200 : " + respReader);
                    }

                    BufferedReader in = new BufferedReader(respReader);
                    String inputLine;

                    while ((inputLine = in.readLine()) != null) {
                        result.append(inputLine);
                    }
                    System.out.print("in result : " + result.toString());

                } catch (Exception ignored) {
                    System.out.println("Catch in updateFCMToken : " +ignored);
                }
                handler.post(()->{
                    onDestroy();
                    stopForeground(true);
                    stopSelf();
                    executor.shutdown();
                });
            });
    }

    public void createLocationService() {
        startLocationUpdates();
        Context context = getApplicationContext();
        Intent locationService = new Intent(context, LocationUpdateService.class);
        locationService.putExtra("action", "start");
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            context.startForegroundService(locationService);
        } else {
//            context.startService(locationService);
        }
    }

    public void startLocationUpdates() {
        client = LocationServices.getFusedLocationProviderClient(getApplicationContext());
        LocationRequest lReq = LocationRequest.create();
        lReq.setPriority(LocationRequest.PRIORITY_HIGH_ACCURACY);
        lReq.setInterval(10000);
        lReq.setFastestInterval(2500);
        JSONObject logPayload = new JSONObject();
        System.out.println("Client 124124 " + client);
        System.out.println("Stone Cold");
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
        client.requestLocationUpdates(lReq,
                new LocationCallback() {
                    @SuppressLint("MissingPermission")
                    @Override
                    public void onLocationResult(LocationResult locationResult) {
                        super.onLocationResult(locationResult);
                        System.out.println("locationResult " + locationResult);
                        Location lastLocation = locationResult.getLastLocation();
                        System.out.println("lastLocation " + lastLocation);
                        if (lastLocation != null) {
                            Log.e("startLocationUpdates", lastLocation.getLatitude() + "/" + lastLocation.getLongitude());
                            String lat = Double.toString(lastLocation.getLatitude());
                            String lng = Double.toString(lastLocation.getLongitude());

                            lastLatitudeValue = lat;
                            lastLongitudeValue = lng;
                        } else {
                            Log.e(LOG_TAG, "Couldn't get location from updates");
                        }
                    }

                    @Override
                    public void onLocationAvailability(LocationAvailability locationAvailability) {
                        super.onLocationAvailability(locationAvailability);
                    }
                },
                Looper.getMainLooper()
        );
    }

}
