/*
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.app;

import android.Manifest;
import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.graphics.BitmapFactory;
import android.graphics.Color;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.provider.Settings;
import android.util.Log;
import android.view.View;
import android.widget.Toast;

import androidx.core.app.ActivityCompat;
import androidx.core.app.NotificationCompat;
import androidx.core.app.NotificationManagerCompat;

import com.google.firebase.analytics.FirebaseAnalytics;

import org.json.JSONException;
import org.json.JSONObject;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.SocketTimeoutException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.net.ssl.HttpsURLConnection;


public class RideRequestUtils {
    private final static int rideReqNotificationId = 5032023;
    private final static String RIDE_REQUEST_CHANNEL = "in.juspay.mobility.riderequest";
    private final static int rideReqNotificationReqCode = 6032023;
    private static final String LOG_TAG = "RideRequestUtils";

    public static Boolean driverRespondApi(String searchRequestId, double offeredPrice, boolean isAccept, Context context, int slotNumber) {
        Handler mainLooper = new Handler(Looper.getMainLooper());
        StringBuilder result = new StringBuilder();
        SharedPreferences sharedPref = context.getApplicationContext().getSharedPreferences(context.getApplicationContext().getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        String bundle_version = sharedPref.getString("BUNDLE_VERSION", "null");
        String version = sharedPref.getString("VERSION_NAME", "null");
        String deviceDetails = sharedPref.getString("DEVICE_DETAILS", "null");
        try {
            String orderUrl = sharedPref.getString("BASE_URL", "null") + "/driver/searchRequest/quote/respond";
            HttpURLConnection connection = (HttpURLConnection) (new URL(orderUrl).openConnection());
            if (connection instanceof HttpsURLConnection)
                ((HttpsURLConnection) connection).setSSLSocketFactory(new TLSSocketFactory());
            connection.setRequestMethod("POST");
            connection.setRequestProperty("Content-Type", "application/json");
            connection.setRequestProperty("x-client-version", version);
            connection.setRequestProperty("token", sharedPref.getString(context.getResources().getString(R.string.REGISTERATION_TOKEN), "null"));
            connection.setRequestProperty("x-bundle-version", bundle_version);
            connection.setRequestProperty("x-device", deviceDetails);
            connection.setDoOutput(true);
            connection.setConnectTimeout(20000);
            connection.setReadTimeout(20000);
            JSONObject payload = new JSONObject();
            if (!isAccept || offeredPrice == 0) {
                payload.put(context.getResources().getString(R.string.OFFERED_FARE), null);
            } else {
                payload.put(context.getResources().getString(R.string.OFFERED_FARE), (offeredPrice));
            }
            payload.put(context.getResources().getString(R.string.SEARCH_REQUEST_ID), searchRequestId);
            if (isAccept) payload.put("response", "Accept");
            else payload.put("response", "Reject");
            payload.put("slotNumber", slotNumber);
            OutputStream stream = connection.getOutputStream();
            stream.write(payload.toString().getBytes());
            connection.connect();
            int respCode = connection.getResponseCode();
            InputStreamReader respReader;
            if ((respCode < 200 || respCode >= 300) && respCode != 302) {
                respReader = new InputStreamReader(connection.getErrorStream());
                BufferedReader in = new BufferedReader(respReader);
                String inputLine;
                while ((inputLine = in.readLine()) != null) {
                    result.append(inputLine);
                }
                JSONObject errorPayload = new JSONObject(result.toString());
                if (errorPayload.has(context.getResources().getString(R.string.ERROR_MESSAGE))) {
                    mainLooper.post(() -> {
                        try {
                            Toast.makeText(context.getApplicationContext(), errorPayload.getString(context.getResources().getString(R.string.ERROR_MESSAGE)), Toast.LENGTH_SHORT).show();
                        } catch (JSONException e) {
                            e.printStackTrace();
                        }
                    });
                }
            } else {
                //API Success
                return true;
            }
            return false;
        } catch (SocketTimeoutException e) {
            mainLooper.post(() -> Toast.makeText(context.getApplicationContext(), "Request Timeout", Toast.LENGTH_SHORT).show());
            return false;
        } catch (Exception e) {
            return false;
        }
    }

    public static void updateViewFromMlTranslation(SheetAdapter.SheetViewHolder holder, SheetModel model, SharedPreferences sharedPref, Context context){
        String lang = sharedPref.getString( "LANGUAGE_KEY", "ENGLISH");
        TranslatorMLKit translate = new TranslatorMLKit("en", lang, context);
        translate.translateStringInTextView(removeCommas(model.getSourceArea()), holder.sourceArea);
        translate.translateStringInTextView(model.getSourceAddress(),  holder.sourceAddress);
        translate.translateStringInTextView(removeCommas(model.getDestinationArea()), holder.destinationArea);
        translate.translateStringInTextView(model.getDestinationAddress(),  holder.destinationAddress);
    }

    public static String removeCommas(String input) {
        String str = input;
        input = input.trim();
        input = input.replaceAll(",+\\s*$", "");
        if (str.trim().endsWith(",")) {
            input += " ,";
        }
        return input;
    }


    public static int calculateExpireTimer(String expireTimeTemp, String currTimeTemp) {
        if (expireTimeTemp == null || currTimeTemp == null) return 0;
        String[] arrOfA = expireTimeTemp.split("T");
        String[] arrOfB = currTimeTemp.split("T");
        if (!arrOfA[0].equals(arrOfB[0])) return -1;

        String[] timeTempExpire = arrOfA[1].split(":");
        String[] timeTempCurrent = arrOfB[1].split(":");
        timeTempExpire[2] = timeTempExpire[2].substring(0, 2);
        timeTempCurrent[2] = timeTempCurrent[2].substring(0, 2);
        int currTime = 0, expireTime = 0, calculate = 3600;
        for (int i = 0; i < timeTempCurrent.length; i++) {
            currTime += (Integer.parseInt(timeTempCurrent[i]) * calculate);
            expireTime += (Integer.parseInt(timeTempExpire[i]) * calculate);
            calculate = calculate / 60;
        }
        return Math.max((expireTime - currTime), 0);
    }
    public static int timeDifferenceInMinutes(Long expireTimeTemp, Long currTimeTemp){
        return (int) (((expireTimeTemp-currTimeTemp)/1000)/60);
    }

    public static void createRideRequestNotification(Context context) {
        long[] vibrationPattern = {1000, 1000, 1000, 800, 800, 800, 800, 800, 800, 800, 800, 800};
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            NotificationChannel channel = new NotificationChannel(RIDE_REQUEST_CHANNEL, "RideRequestChannel", NotificationManager.IMPORTANCE_HIGH);
            channel.enableVibration(true);
            channel.setVibrationPattern(vibrationPattern);
            channel.setLockscreenVisibility(Notification.VISIBILITY_PUBLIC);
            channel.setImportance(NotificationManager.IMPORTANCE_HIGH);
            NotificationManager notificationManager = context.getSystemService(NotificationManager.class);
            notificationManager.createNotificationChannel(channel);
        }
        Intent notificationIntent;
        if (RideRequestActivity.getInstance() == null) {
            notificationIntent = context.getPackageManager().getLaunchIntentForPackage(context.getPackageName());
        } else {
            notificationIntent = new Intent(context, RideRequestActivity.class);
        }
        notificationIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_REORDER_TO_FRONT);
        PendingIntent pendingIntent = PendingIntent.getActivity(context, rideReqNotificationReqCode, notificationIntent, PendingIntent.FLAG_IMMUTABLE);
        NotificationCompat.Builder mBuilder = new NotificationCompat.Builder(context, RIDE_REQUEST_CHANNEL);
        mBuilder.setLargeIcon(BitmapFactory.decodeResource(context.getResources(), R.mipmap.ic_launcher));
        mBuilder.setContentTitle(context.getString(R.string.new_ride_req))
                .setContentText(context.getString(R.string.new_ride_available_for_offering))
                .setSmallIcon((R.mipmap.ic_launcher))
                .setAutoCancel(true)
                .setVibrate(vibrationPattern)
                .setSound(null)
                .setPriority(NotificationCompat.PRIORITY_HIGH);
        mBuilder.setContentIntent(pendingIntent);
        NotificationManagerCompat managerCompat = NotificationManagerCompat.from(context.getApplicationContext());
        if (ActivityCompat.checkSelfPermission(context, Manifest.permission.POST_NOTIFICATIONS) != PackageManager.PERMISSION_GRANTED) {
            Log.e(LOG_TAG, "no notification permission");
        }
        managerCompat.notify(rideReqNotificationId, mBuilder.build());
    }


    public static void cancelRideReqNotification(Context context) {
        String ns = Context.NOTIFICATION_SERVICE;
        NotificationManager notificationManager = (NotificationManager) context.getSystemService(ns);
        notificationManager.cancel(rideReqNotificationId);
    }

    public static void firebaseLogEventWithParams(String event, String paramKey, String paramValue, Context context) {
        Bundle params = new Bundle();
        params.putString(paramKey, paramValue);
        FirebaseAnalytics mFirebaseAnalytics = FirebaseAnalytics.getInstance(context);
        mFirebaseAnalytics.logEvent(event, params);
    }

    public static void restartLocationService(Context context) {
        Intent locationService = new Intent(context, LocationUpdateService.class);
        locationService.putExtra("StartingSource", "TRIGGER_SERVICE");
        locationService.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK | Intent.FLAG_ACTIVITY_NEW_TASK);
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            context.startForegroundService(locationService);
        } else {
            context.startService(locationService);
        }
        Intent restartIntent = context.getPackageManager().getLaunchIntentForPackage(context.getPackageName());
        restartIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK | Intent.FLAG_ACTIVITY_NEW_TASK);

        if(Settings.canDrawOverlays(context)){
            try{
                Handler handler = new Handler();
                handler.postDelayed(() -> {
                    context.startActivity(restartIntent);
                    Utils.minimizeApp(context);
                }, 5000);
            } catch (Exception e) {
                Log.e("BootUpReceiver", "Unable to Start Widget Service");
            }
        }
    }

    public static void callAPIViaFCM(String orderUrl, JSONObject requestBody, String method, Context context) {
        SharedPreferences sharedPref = context.getSharedPreferences(
                context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        String token = sharedPref.getString("REGISTERATION_TOKEN", "null");
        String deviceDetails = sharedPref.getString("DEVICE_DETAILS", "null");
        ExecutorService executor = Executors.newSingleThreadExecutor();
        Handler handler = new Handler(Looper.getMainLooper());
        executor.execute(() -> {
            StringBuilder result = new StringBuilder();
            try {
                System.out.println("in callAPIViaFCM");
                HttpURLConnection connection = (HttpURLConnection) (new URL(orderUrl).openConnection());
                if (connection instanceof HttpsURLConnection)
                    ((HttpsURLConnection) connection).setSSLSocketFactory(new TLSSocketFactory());
                connection.setRequestMethod(method);
                connection.setRequestProperty("Content-Type", "application/json");
                connection.setRequestProperty("token", token);
                connection.setRequestProperty("x-device", deviceDetails);
                connection.setDoOutput(true);

                OutputStream stream = connection.getOutputStream();
                if (requestBody != null) {
                    stream.write(requestBody.toString().getBytes());
                }
                connection.connect();
                int respCode = connection.getResponseCode();
                InputStreamReader respReader;

                if ((respCode < 200 || respCode >= 300) && respCode != 302) {
                    respReader = new InputStreamReader(connection.getErrorStream());
                    firebaseLogEventWithParams("ny_fcm_error_calling_api", "status_code", String.valueOf(respCode), context);
                    System.out.println("in error : " + respReader);
                } else {
                    respReader = new InputStreamReader(connection.getInputStream());
                    firebaseLogEventWithParams("ny_fcm_success_calling_api", "status_code", String.valueOf(respCode), context);
                    System.out.println("in 200 : " + respReader);
                }

                BufferedReader in = new BufferedReader(respReader);
                String inputLine;

                while ((inputLine = in.readLine()) != null) {
                    result.append(inputLine);
                }
                Log.i(LOG_TAG, "in result : " + result);

            } catch (Exception e) {
                Log.i(LOG_TAG, "Catch in callAPIViaFCM : " + e);
            }
            handler.post(executor::shutdown);
        });
    }

    public static void openApplication(Context context) {
        Intent intent = context.getPackageManager().getLaunchIntentForPackage(context.getPackageName());
        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_REORDER_TO_FRONT);
        try {
            context.startActivity(intent);
        } catch (Exception e) {
            Log.e(LOG_TAG, "Exception in openApplication");
        }
    }


    public static  JSONObject getZoneConfig(String tag, Context context){
        try {
            String key;
            String[] arrOfStr = tag.split("_");
            String pickup = arrOfStr[0];
            String drop = arrOfStr[1];
            String priority = arrOfStr[2];
            if (priority.equals("PriorityPickup")) {
                key = pickup + "_Pickup";
            } else if (priority.equals("PriorityDrop")){
                key = drop + "_Drop";
            } else {
                return new JSONObject();
            }
            InputStream is = context.getAssets().open("juspay/zone_config.json");
            int size = is.available();
            byte[] buffer = new byte[size];
            is.read(buffer);
            is.close();
            String json = new String(buffer, StandardCharsets.UTF_8);
            return new JSONObject(json).getJSONObject(key);
        } catch (Exception ex) {
            ex.printStackTrace();
            return new JSONObject();
        }
    }

    public static void setSpecialZoneAttrs(SheetAdapter.SheetViewHolder holder, String specialLocationTag, Context context) {
        try{
            JSONObject zoneConfig = getZoneConfig(specialLocationTag,context);
            holder.assetZonePickup.setImageURI(Uri.parse("android.resource://"+ context.getPackageName() +"/drawable/"+ zoneConfig.get("imageUrl")));
            holder.assetZoneDrop.setImageURI(Uri.parse("android.resource://"+ context.getPackageName() +"/drawable/"+ zoneConfig.get("imageUrl")));
            holder.assetZonePickup.setVisibility(zoneConfig.getInt("assetZonePickupVisibility"));
            holder.assetZoneDrop.setVisibility(zoneConfig.getInt("assetZoneDropVisibility"));
        }catch (Exception e){
            e.printStackTrace();
        }
    }

    public static void updateDriverStatus(Boolean status, String mode, Context context, Boolean startWidget) {
        ExecutorService executor = Executors.newSingleThreadExecutor();
        executor.execute(() ->
        {
            StringBuilder result = new StringBuilder();
            SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
            String token = sharedPref.getString("REGISTERATION_TOKEN", "null");
            String bundle_version = sharedPref.getString("BUNDLE_VERSION","null");
            String baseUrl = sharedPref.getString("BASE_URL", "null");
            String deviceDetails = sharedPref.getString("DEVICE_DETAILS", "null");
            String versionName = sharedPref.getString("VERSION_NAME", "null");
            try
            {
                //endPoint for driver status
                String orderUrl = baseUrl + "/driver/setActivity?active=" + status + "&mode=\"" + mode + "\"";
                Log.d(LOG_TAG, "orderUrl " + orderUrl);
                //Http connection to make API call
                HttpURLConnection connection = (HttpURLConnection) (new URL(orderUrl).openConnection());
                if (connection instanceof HttpsURLConnection)
                    ((HttpsURLConnection) connection).setSSLSocketFactory(new TLSSocketFactory());
                connection.setRequestMethod("POST");
                connection.setRequestProperty("Content-Type", "application/json");
                connection.setRequestProperty("x-client-version", versionName);
                connection.setRequestProperty("token", token);
                connection.setRequestProperty("x-bundle-version", bundle_version);
                connection.setRequestProperty("x-device", deviceDetails);
                connection.setDoOutput(true);
                connection.connect();

                // validating the response code
                int respCode = connection.getResponseCode();
                InputStreamReader respReader;
                Log.d(LOG_TAG, "respCode "+ respCode);

                if ((respCode < 200 || respCode >= 300) && respCode != 302) {
                    respReader = new InputStreamReader(connection.getErrorStream());
                    Log.d(LOG_TAG, "in error "+ respReader);
                } else {
                    if (startWidget && Settings.canDrawOverlays(context)  && !sharedPref.getString(context.getResources().getString(R.string.REGISTERATION_TOKEN), "null").equals("null") && (sharedPref.getString(context.getResources().getString(R.string.ACTIVITY_STATUS), "null").equals("onPause") || sharedPref.getString(context.getResources().getString(R.string.ACTIVITY_STATUS), "null").equals("onDestroy"))) {
                            Intent widgetService = new Intent(context, WidgetService.class);
                            context.startService(widgetService);
                    }
                    respReader = new InputStreamReader(connection.getInputStream());
                    Log.d(LOG_TAG, "in 200 "+ respReader);
                }

                BufferedReader in = new BufferedReader(respReader);
                String inputLine;
                while ((inputLine = in.readLine()) != null) {
                    result.append(inputLine);
                }
                sharedPref.edit().putString("DRIVER_STATUS","__failed").apply();
                Log.d(LOG_TAG, "in result "+ result);
            }
            catch (Exception error)
            {
                Log.d(LOG_TAG, "Catch in updateDriverStatus : "+error);
            }
        });
    }
}
