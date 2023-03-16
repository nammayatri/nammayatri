/* 
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.utils;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.provider.Settings;
import android.util.Log;
import androidx.annotation.NonNull;
import androidx.annotation.RequiresApi;

import com.google.firebase.analytics.FirebaseAnalytics;
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
import in.juspay.mobility.MainActivity;
import in.juspay.mobility.R;
import com.google.firebase.analytics.FirebaseAnalytics;

public class MyFirebaseMessagingService extends FirebaseMessagingService {
    private FirebaseAnalytics mFirebaseAnalytics;

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
        if (!regToken.equals("null") && !regToken.equals("__failed")) {
            updateFCMToken(deviceToken);
        }
        Log.e("newToken", newToken);
    }

    @RequiresApi(api = Build.VERSION_CODES.M)
    @Override
    public void onMessageReceived(@NonNull RemoteMessage remoteMessage){
        firebaseLogEventWithParams("notification_received","type",remoteMessage.getData().get("notification_type"));

        super.onMessageReceived(remoteMessage);
        Log.e("onMessageReceived", remoteMessage.getData().toString());
        JSONObject payload = new JSONObject();
        JSONObject notification_payload = null;
        JSONObject entity_payload = null;
        try {
            payload.put("notification_type", remoteMessage.getData().get("notification_type"));
            payload.put("entity_ids", remoteMessage.getData().get("entity_ids"));
            payload.put("entity_type", remoteMessage.getData().get("entity_type"));
            payload.put("show_notification", remoteMessage.getData().get("show_notification"));

            String title;
            String body;
            String icon;
            String imageUrl;

            if (remoteMessage.getData().containsKey("notification_json")) {
                notification_payload = new JSONObject(remoteMessage.getData().get("notification_json"));
            }
            if (remoteMessage.getData().containsKey("entity_data") && remoteMessage.getData().get("notification_type").equals("NEW_RIDE_AVAILABLE") ) {
                entity_payload = new JSONObject(remoteMessage.getData().get("entity_data"));
            }
            
            RemoteMessage.Notification notification = remoteMessage.getNotification();
            if (notification != null) {
                title = notification.getTitle();
                body = notification.getBody();
                icon = notification.getIcon();
                imageUrl = remoteMessage.getData().get("image-url");
            } else {
                title = notification_payload.get("title").toString();
                body = notification_payload.get("body").toString();
                icon = notification_payload.get("icon").toString();
                if (notification_payload.has("imageUrl")) {
                    imageUrl = notification_payload.get("imageUrl").toString();
                } else {
                    imageUrl = null;
                }
            }

            if (notification_payload != null || notification != null) {
                SharedPreferences sharedPref = this.getSharedPreferences(this.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
                String notificationType = (String) payload.get("notification_type");

                switch (notificationType) {
                    case NotificationTypes.TRIGGER_SERVICE :
                        if (getString(R.string.service).equals(getString(R.string.nammayatripartner))) {
                            FirebaseAnalytics.getInstance(this).logEvent("notification_trigger_service", new Bundle());
                            restartLocationService();
                        }
                        break;

                    case NotificationTypes.NEW_RIDE_AVAILABLE :
                        sharedPref.edit().putString(getString(R.string.RIDE_STATUS), getString(R.string.NEW_RIDE_AVAILABLE)).apply();
                        NotificationUtils.showAllocationNotification(this, title, body, payload, imageUrl, entity_payload);
                        break;

                    case NotificationTypes.CLEARED_FARE :
                        sharedPref.edit().putString(getString(R.string.CLEAR_FARE), String.valueOf(payload.get(getString(R.string.entity_ids)))).apply();
                        NotificationUtils.showAllocationNotification(this, title, body, payload, imageUrl, entity_payload);
                        break;

                    case NotificationTypes.CANCELLED_PRODUCT :
                        NotificationUtils.showNotification(this, title, body, payload, imageUrl);
                        sharedPref.edit().putString(getResources().getString(R.string.IS_RIDE_ACTIVE), "false").apply();
                        startWidgetService(getString(R.string.ride_cancelled));
                        break;

                    case NotificationTypes.DRIVER_QUOTE_INCOMING :
                        if (sharedPref.getString(getResources().getString(R.string.ACTIVITY_STATUS), "null").equals("onPause")) {
                            NotificationUtils.showNotification(this, title, body, payload, imageUrl);
                        }
                        break;

                    case NotificationTypes.TRIP_FINISHED :
                        NotificationUtils.showNotification(this, title, body, payload, imageUrl);
                        if (getResources().getString(R.string.service).equals("nammayatri")) {
                            sharedPref.edit().putInt("RIDE_COUNT", sharedPref.getInt("RIDE_COUNT", 0) + 1).apply();
                            sharedPref.edit().putString("COMPLETED_RIDE_COUNT", String.valueOf(sharedPref.getInt("RIDE_COUNT", 0))).apply();
                        }
                        break;

                    case NotificationTypes.DRIVER_ASSIGNMENT :
                        NotificationUtils.showNotification(this, title, body, payload, imageUrl);
                        sharedPref.edit().putString(getResources().getString(R.string.IS_RIDE_ACTIVE), "true").apply();
                        sharedPref.edit().putString(getString(R.string.RIDE_STATUS), getString(R.string.DRIVER_ASSIGNMENT)).apply();
                        startMainActivity();
                        break;

                    case NotificationTypes.BUNDLE_UPDATE :
                        try{
                            if (MainActivity.getInstance() != null) {
                                MainActivity.getInstance().showAlertForUpdate();
                            }
                            else {
                                firebaseLogEventWithParams("unable_to_update_bundle","reason","Main Activity instance is null");
                            }
                        } catch (Exception e){
                            e.printStackTrace();
                            firebaseLogEventWithParams("exception_in_bundle_update _fcm","exception",e.toString());
                        }
                        break;

                    case NotificationTypes.CANCELLED_SEARCH_REQUEST :
                        sharedPref.edit().putString(getString(R.string.CANCELLED_SEARCH_REQUEST), String.valueOf(payload.get(getString(R.string.entity_ids)))).apply();
                        NotificationUtils.showAllocationNotification(this, title, body, payload, imageUrl, entity_payload);
                        break;

                    case NotificationTypes.NEW_MESSAGE :
                        sharedPref.edit().putString("ALERT_RECEIVED", "true").apply();
                        NotificationUtils.showNotification(this, title, body, payload, imageUrl);
                        break;

                    case NotificationTypes.REGISTRATION_APPROVED :
                        sharedPref.edit().putString(getString(R.string.REGISTRATION_APPROVED), "true").apply();
                        break;

                    case NotificationTypes.REFERRAL_ACTIVATED :
                        sharedPref.edit().putString("REFERRAL_ACTIVATED", "true").apply();
                        break;
                        
                    default:
                        if (payload.get("show_notification").equals("true")) {
                            NotificationUtils.showNotification(this, title, body, payload, imageUrl);
                        } else {
                            // Silent notification
                        }
                        break;
                }
            }
        } catch (Exception e) {
            firebaseLogEventWithParams("exception_in_notification","remoteMessage",remoteMessage.getData().toString());
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
    public void restartLocationService() 
    {
        Context context = getApplicationContext();
        Intent locationService = new Intent(context, LocationUpdateService.class);
        locationService.putExtra("StartingSource","TRIGGER_SERVICE");
        locationService.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK | Intent.FLAG_ACTIVITY_NEW_TASK);
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            context.startForegroundService(locationService);
        }else{
            context.startService(locationService);
        }
    }

    private void startWidgetService(String widgetMessage){
        SharedPreferences sharedPref = this.getSharedPreferences(this.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        Intent widgetService = new Intent(getApplicationContext(), WidgetService.class);
        if (getResources().getString(R.string.service).equals(getString(R.string.nammayatripartner)) && Settings.canDrawOverlays(getApplicationContext())  && !sharedPref.getString(getResources().getString(R.string.REGISTERATION_TOKEN), "null").equals("null") && (sharedPref.getString(getResources().getString(R.string.ACTIVITY_STATUS), "null").equals("onPause") || sharedPref.getString(getResources().getString(R.string.ACTIVITY_STATUS), "null").equals("onDestroy"))) {
            widgetService.putExtra(getResources().getString(R.string.WIDGET_MESSAGE),widgetMessage);
            try{
                startService(widgetService);
            }catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    private void startMainActivity() {
        SharedPreferences sharedPref = this.getSharedPreferences(this.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        if (getResources().getString(R.string.service).equals(getString(R.string.nammayatripartner)) && !sharedPref.getString(getResources().getString(R.string.REGISTERATION_TOKEN), "null").equals("null") && (sharedPref.getString(getResources().getString(R.string.ACTIVITY_STATUS), "null").equals("onPause") || sharedPref.getString(getResources().getString(R.string.ACTIVITY_STATUS), "null").equals("onDestroy"))) {
            Intent intent = new Intent(getApplicationContext(), MainActivity.class);
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_REORDER_TO_FRONT);
            try {
                getApplicationContext().startActivity(intent);
            }catch (Exception e){
                firebaseLogEventWithParams("exception" , "startMainActivity", e.toString());
            }
        }
    }

    public void firebaseLogEventWithParams(String event,String paramKey,String paramValue) {
        Bundle params = new Bundle();
        params.putString(paramKey,paramValue);
        mFirebaseAnalytics = FirebaseAnalytics.getInstance(this);
        mFirebaseAnalytics.logEvent(event, params);
    }

    private class NotificationTypes {
        private static final String TRIGGER_SERVICE = "TRIGGER_SERVICE";
        private static final String NEW_RIDE_AVAILABLE = "NEW_RIDE_AVAILABLE";
        private static final String CLEARED_FARE = "CLEARED_FARE";
        private static final String CANCELLED_PRODUCT = "CANCELLED_PRODUCT";
        private static final String DRIVER_QUOTE_INCOMING = "DRIVER_QUOTE_INCOMING";
        private static final String TRIP_FINISHED = "TRIP_FINISHED";
        private static final String DRIVER_ASSIGNMENT = "DRIVER_ASSIGNMENT";
        private static final String BUNDLE_UPDATE = "BUNDLE_UPDATE";
        private static final String CANCELLED_SEARCH_REQUEST = "CANCELLED_SEARCH_REQUEST";
        private static final String NEW_MESSAGE = "NEW_MESSAGE";
        private static final String REGISTRATION_APPROVED = "REGISTRATION_APPROVED";
        private static final String REFERRAL_ACTIVATED = "REFERRAL_ACTIVATED";
    }
}