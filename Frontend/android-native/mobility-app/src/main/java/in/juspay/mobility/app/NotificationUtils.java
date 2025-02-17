/*
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.app;

import static android.content.Context.ACTIVITY_SERVICE;
import static android.content.Context.BIND_AUTO_CREATE;

import android.Manifest;
import android.annotation.SuppressLint;
import android.app.ActivityManager;
import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Typeface;
import android.media.AudioAttributes;
import android.media.AudioManager;
import android.media.MediaPlayer;
import android.media.RingtoneManager;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.IBinder;
import android.os.Looper;
import android.provider.Settings;
import android.text.SpannableString;
import android.text.Spanned;
import android.text.style.StyleSpan;
import android.util.Log;

import androidx.core.app.ActivityCompat;
import androidx.core.app.NotificationCompat;
import androidx.core.app.NotificationManagerCompat;

import com.google.firebase.analytics.FirebaseAnalytics;
import com.google.firebase.crashlytics.FirebaseCrashlytics;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Random;
import java.util.Set;
import java.util.TimeZone;

import javax.net.ssl.HttpsURLConnection;

import in.juspay.mobility.app.RemoteConfigs.MobilityRemoteConfigs;
import in.juspay.mobility.app.callbacks.CallBack;
import in.juspay.mobility.common.services.TLSSocketFactory;


public class NotificationUtils {

    public static class RequestSource {
        public static final String FCM = "FCM";
        public static final String GRPC = "GRPC";
    }

    private static final String LOG_TAG = "LocationServices";
    private static final String TAG = "NotificationUtils";
    public static final String GENERAL_NOTIFICATION = "GENERAL_NOTIFICATION_NEW";
    public static final String DRIVER_HAS_REACHED = "DRIVER_HAS_REACHED_NEW";
    public static final String ALLOCATION_TYPE = "NEW_RIDE_AVAILABLE";
    public static final String CANCELLED_PRODUCT = "CANCELLED_PRODUCT_NEW";
    public static final String DRIVER_ASSIGNMENT = "DRIVER_ASSIGNMENT_NEW";
    public static final String REALLOCATE_PRODUCT = "REALLOCATE_PRODUCT_NEW";
    public static final String RIDE_STARTED = "RIDE_STARTED_NEW";
    public static final String NO_VARIANT = "NO_VARIANT";
    public static final String DRIVER_QUOTE_INCOMING = "DRIVER_QUOTE_INCOMING_NEW";
    public static final String SOS_TRIGGERED = "SOS_TRIGGERED_NEW";
    public static final String SOS_RESOLVED = "SOS_RESOLVED_NEW";
    public static final String NOSOUND_NOTIFICATION = "NOSOUND_NOTIFICATION";
    public static final String RENTAL = "Rental";
    public static final String INTERCITY = "InterCity";
    public static final String DELIVERY = "Delivery";
    public static Uri soundUri = null;
    public static OverlaySheetService.OverlayBinder binder;
    public static ArrayList<Bundle> listData = new ArrayList<>();
    @SuppressLint("MissingPermission")
    private static FirebaseAnalytics mFirebaseAnalytics;
    static Random rand = new Random();
    public static int notificationId = rand.nextInt(1000000);
    public static MediaPlayer mediaPlayer;
    public static Bundle lastRideReq = new Bundle();
    private static final ArrayList<CallBack> callBack = new ArrayList<>();
    private static final Map<String, Long> processedNotificationIds = new HashMap<>();

    private static MobilityRemoteConfigs remoteConfigs = new MobilityRemoteConfigs(false, true);

    public static void registerCallback(CallBack notificationCallback) {
        callBack.add(notificationCallback);
    }

    public static void deRegisterCallback(CallBack notificationCallback) {
        callBack.remove(notificationCallback);
    }
    public static int chatNotificationId = 18012023;

    public static String[] parseRideStartDateTime(String rideDateTimeString){
        String rideStartTime="",rideStartDate="";
        try {
            final SimpleDateFormat dateTimeWithMillis = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'", new Locale("en", "US"));
            final SimpleDateFormat dateTimeWithoutMillis = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'", new Locale("en", "US"));
            dateTimeWithMillis.setTimeZone(TimeZone.getTimeZone("UTC"));
            dateTimeWithoutMillis.setTimeZone(TimeZone.getTimeZone("UTC"));
            Date rideDateTime;
            try {
                rideDateTime = dateTimeWithMillis.parse(rideDateTimeString);
            } catch (Exception e) {
                rideDateTime = dateTimeWithoutMillis.parse(rideDateTimeString);
            }
            rideDateTime.setTime(rideDateTime.getTime() + (330 * 60 * 1000));
            final SimpleDateFormat df1 = new SimpleDateFormat("yyyy-MM-dd");
            final SimpleDateFormat tf1 = new SimpleDateFormat("hh:mm a");
            df1.setTimeZone(TimeZone.getTimeZone("IST"));
            tf1.setTimeZone(TimeZone.getTimeZone("IST"));
            
            String date = df1.format(rideDateTime);
            String time = tf1.format(rideDateTime);
            
            rideStartTime = time;
            rideStartDate = date.equals(df1.format(new Date())) ? "Today" : date;
        }
        catch(Exception e) {
            rideStartDate = "Today";
            rideStartTime = "now";
            e.printStackTrace();
            
        }
        return new String[]{rideStartDate, rideStartTime};
    }

    public static void showAllocationNotification(Context context, JSONObject data, JSONObject entity_payload, String source) {
        try {

            System.out.println(" entity_payload- > " + entity_payload);
            SharedPreferences sharedPref = context.getSharedPreferences(
                    context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);

            boolean incVol = sharedPref.getString("AUTO_INCREASE_VOL", "true").equals("true");
            String notificationType = data.getString("notification_type");
            String notificationIdString = "null", expTime = "null";

            final SimpleDateFormat f = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'",new Locale("en","US"));
            f.setTimeZone(TimeZone.getTimeZone("IST"));
            String currTime = f.format(new Date());

            try{
                notificationIdString = data.getString("notification_id");
                expTime = entity_payload.getString("searchRequestValidTill");
            }catch(Exception e){
                Log.i("SHOW_ALLOCATION", "notification_id field not present in " + notificationType);
                Exception exception = new Exception("Error in notificationID not persent " + e);
                FirebaseCrashlytics.getInstance().recordException(exception);
            }

            if(!notificationIdString.equals("null")){
                synchronized (MyFirebaseMessagingService.notificationIdsReceived){
                    if(MyFirebaseMessagingService.notificationIdsReceived.containsKey(notificationIdString)){
                        Log.i("SHOW_ALLOCATION", "Already There - [" + source + "] - " + notificationIdString);
                        return;
                    }
                }
                Log.i("SHOW_ALLOCATION", "adding notification id for - [" + source + "] - " + notificationIdString);
                MyFirebaseMessagingService.notificationIdsReceived.put(notificationIdString, expTime.equals("null") ? currTime : expTime);
                // add logs to monitor success rate - which sources' notification was used for NEW_RIDE_AVAILABLE
            }

            Log.i("SHOW_ALLOCATION", "processing for notification type " + notificationType);

            if (ALLOCATION_TYPE.equals(notificationType) && MyFirebaseMessagingService.clearedRideRequest.containsKey(data.getString("entity_ids"))) {
                System.out.println("The remove notification cleared " + data.getString("entity_ids"));
                MyFirebaseMessagingService.clearedRideRequest.remove(data.getString("entity_ids"));
                return;
            }

            if (ALLOCATION_TYPE.equals(notificationType)) {
                System.out.println("In_if_in_notification before");
                Bundle params = new Bundle();
                mFirebaseAnalytics = FirebaseAnalytics.getInstance(context);
                mFirebaseAnalytics.logEvent("ride_request_received", params);
                RideRequestUtils.addRideReceivedEvent(entity_payload,null,null, "ride_request_received", context );
                //Recieved Notification && checking for permission if overlay permission is given, if not then it will redirect to give permission

                Intent svcT = new Intent(context, OverlaySheetService.class);
                svcT.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                System.out.println("Call Service before");
                String tripType = entity_payload.has("tripCategory") && !entity_payload.isNull("tripCategory") ? (entity_payload.getJSONObject("tripCategory")).getString("tag") : "";
                Bundle sheetData = new Bundle();
                String expiryTime = "";
                String searchRequestId = "";
                String rideDateTimeString  = entity_payload.has("startTime") && !entity_payload.isNull("startTime") ? entity_payload.getString("startTime") : "";
                String[] rideDateTimeValues = parseRideStartDateTime(rideDateTimeString);
                String rideStartDate;
                String rideStartTime;
                if(rideDateTimeValues.length > 1){
                 rideStartDate= rideDateTimeValues[0];
                 rideStartTime = rideDateTimeValues[1];
                }
                else
                {
                    rideStartDate = "Today";
                    rideStartTime = "now";
                }
                try {
                    JSONObject addressPickUp = new JSONObject(entity_payload.get("fromLocation").toString());
                    JSONObject addressDrop = new JSONObject(entity_payload.has("toLocation") && !entity_payload.isNull("toLocation") ? entity_payload.get("toLocation").toString() : "{}");
                    JSONObject driverDefaultStepFeeWithCurrency = new JSONObject(
                            entity_payload.has("driverDefaultStepFeeWithCurrencyV2") && !entity_payload.isNull("driverDefaultStepFeeWithCurrencyV2") ?
                            entity_payload.get("driverDefaultStepFeeWithCurrencyV2").toString() : (entity_payload.has("driverDefaultStepFeeWithCurrency") && !entity_payload.isNull("driverDefaultStepFeeWithCurrency") ? entity_payload.get("driverDefaultStepFeeWithCurrency").toString() : "{}"));
                    JSONObject driverStepFeeWithCurrency = new JSONObject(entity_payload.has("driverStepFeeWithCurrency") && !entity_payload.isNull("driverStepFeeWithCurrency") ? entity_payload.get("driverStepFeeWithCurrency").toString() : "{}");
                    int negotiationUnit = Integer.parseInt(sharedPref.getString( "NEGOTIATION_UNIT", "10"));
                    String[] specialZoneSplit = entity_payload.optString("specialLocationTag", "None").split("_");
                    boolean isPickupZone = entity_payload.optBoolean("pickupZone", false);
                    boolean isSpecialPickupZone = false;
                    if(specialZoneSplit.length > 0) {
                        isSpecialPickupZone = "PickupZone".equals(specialZoneSplit[specialZoneSplit.length - 1]) && isPickupZone;
                    }
                    sheetData.putString("searchRequestId", entity_payload.getString("searchRequestId"));
                    sheetData.putString("searchRequestValidTill", entity_payload.getString("searchRequestValidTill"));
                    sheetData.putInt("baseFare", entity_payload.getInt("baseFare"));
                    sheetData.putString("currency", sharedPref.getString("CURRENCY", "₹"));
                    sheetData.putInt("distanceToPickup", entity_payload.getInt("distanceToPickup"));
                    sheetData.putString("durationToPickup", entity_payload.getString("durationToPickup"));
                    sheetData.putInt("distanceTobeCovered", entity_payload.getInt("distance"));
                    sheetData.putInt("tollCharges", entity_payload.optInt("tollCharges", 0));
                    sheetData.putString("sourceArea", addressPickUp.getString("area"));
                    sheetData.putString("destinationArea", addressDrop.has("area") && !addressDrop.isNull("area") ? addressDrop.has("area") && !addressDrop.isNull("area") ? addressDrop.getString("area") : "" : "");
                    sheetData.putDouble("srcLat", addressPickUp.has("lat") && !addressPickUp.isNull("lat") ? addressPickUp.getDouble("lat") : 0.0);
                    sheetData.putDouble("srcLng", addressPickUp.has("lon") && !addressPickUp.isNull("lon") ? addressPickUp.getDouble("lon"): 0.0);
                    sheetData.putDouble("destLat", addressDrop.has("lat") && !addressDrop.isNull("lat") ? addressDrop.getDouble("lat"): 0.0);
                    sheetData.putDouble("destLng", addressDrop.has("lon") && !addressDrop.isNull("lon") ? addressDrop.getDouble("lon"): 0.0);
                    sheetData.putString("addressPickUp", addressPickUp.getString("full_address"));
                    sheetData.putString("addressDrop", addressDrop.has("full_address") && !addressDrop.isNull("full_address") ? addressDrop.getString("full_address") : "");
                    sheetData.putInt("driverMinExtraFee", entity_payload.has("driverMinExtraFee") ? entity_payload.optInt("driverMinExtraFee", 0) : 0);
                    sheetData.putInt("driverMaxExtraFee", entity_payload.has("driverMaxExtraFee") ? entity_payload.optInt("driverMaxExtraFee", 0) : 0);
                    sheetData.putString("specialLocationTag", entity_payload.has("specialLocationTag") && !entity_payload.isNull("specialLocationTag") ?entity_payload.getString("specialLocationTag"):null);//null "SureAirport - Pickup"
                    sheetData.putInt("rideRequestPopupDelayDuration", entity_payload.has("rideRequestPopupDelayDuration") ? entity_payload.getInt("rideRequestPopupDelayDuration") : 0);
                    sheetData.putInt("customerExtraFee", (entity_payload.has("customerExtraFee") && !entity_payload.isNull("customerExtraFee") ? entity_payload.getInt("customerExtraFee") : 0));
                    sheetData.putInt("keepHiddenForSeconds", (entity_payload.has("keepHiddenForSeconds") && !entity_payload.isNull("keepHiddenForSeconds") ? entity_payload.getInt("keepHiddenForSeconds") : 0));
                    sheetData.putBoolean("isTranslated", (!entity_payload.has("isTranslated") || entity_payload.isNull("isTranslated") || entity_payload.getBoolean("isTranslated")));
                    sheetData.putString("sourcePinCode", addressPickUp.has("areaCode") && !addressPickUp.isNull("areaCode") ? addressPickUp.getString("areaCode"): "");
                    sheetData.putString("destinationPinCode", addressDrop.has("areaCode") && !addressDrop.isNull("areaCode") ? addressDrop.getString("areaCode") : "");
                    sheetData.putString("requestedVehicleVariant", (entity_payload.has("requestedVehicleVariant") && !entity_payload.isNull("requestedVehicleVariant")) ? entity_payload.getString("requestedVehicleVariant") : NO_VARIANT);
                    sheetData.putBoolean("disabilityTag", (entity_payload.has("disabilityTag") && !entity_payload.isNull("disabilityTag")));
                    sheetData.putBoolean("gotoTag", entity_payload.has("goHomeRequestId") && !entity_payload.isNull("goHomeRequestId"));
                    sheetData.putInt("driverPickUpCharges", entity_payload.has("driverPickUpCharges") ? entity_payload.optInt("driverPickUpCharges", 0): 0);
                    sheetData.putBoolean("specialZonePickup", isSpecialPickupZone);
                    sheetData.putBoolean("downgradeEnabled", entity_payload.optBoolean("downgradeEnabled", true));
                    sheetData.putInt("airConditioned", entity_payload.has("airConditioned") && !entity_payload.isNull("airConditioned") ? (entity_payload.getBoolean("airConditioned") ? 1 : 0) : -1);
                    sheetData.putString("vehicleServiceTier", entity_payload.optString("vehicleServiceTier", null));
                    sheetData.putString("rideProductType", tripType);
                    sheetData.putInt("rideDuration",entity_payload.has("duration") && !entity_payload.isNull("duration") ? entity_payload.getInt("duration") : 0);
                    sheetData.putInt("rideDistance",entity_payload.has("distance") && !entity_payload.isNull("distance") ? entity_payload.getInt("distance") : 0);
                    sheetData.putString("rideStartTime", rideStartTime);
                    sheetData.putString("rideStartDate", rideStartDate);
                    sheetData.putString("notificationSource", source);
                    sheetData.putBoolean("isThirdPartyBooking", entity_payload.has("isValueAddNP") && !entity_payload.optBoolean("isValueAddNP", true));
                    sheetData.putInt("driverDefaultStepFee", driverDefaultStepFeeWithCurrency.optInt("amount", 0));
                    sheetData.putInt("driverStepFeeWithCurrency", driverStepFeeWithCurrency.optInt("amount", negotiationUnit));
                    sheetData.putDouble("parkingCharge", entity_payload.optDouble("parkingCharge", 0.0));
                    sheetData.putBoolean("isFavourite", entity_payload.has("isFavourite") && entity_payload.optBoolean("isFavourite", false));
                    sheetData.putInt("middleStopCount", entity_payload.optInt("middleStopCount", 0));
                    sheetData.putBoolean("roundTrip" , entity_payload.optBoolean("roundTrip", false));
                    expiryTime = entity_payload.getString("searchRequestValidTill");
                    searchRequestId = entity_payload.getString("searchRequestId");
                    System.out.println(entity_payload);
                } catch (Exception e) {
                    Exception exception = new Exception("Error in parse overlay data " + e);
                    FirebaseCrashlytics.getInstance().recordException(exception);
                    System.out.println("exception_parsing_overlay_data" + " <> " + searchRequestId + " <> " + sharedPref.getString("DRIVER_ID", "null"));
                    Bundle overlayExceptionParams = new Bundle();
                    overlayExceptionParams.putString("search_request_id", searchRequestId);
                    overlayExceptionParams.putString("driver_id", sharedPref.getString("DRIVER_ID", "null"));
                    mFirebaseAnalytics = FirebaseAnalytics.getInstance(context);
                    mFirebaseAnalytics.logEvent("exception_parsing_overlay_data", overlayExceptionParams);
                    Log.e(TAG, "Exception" + e);

                }
                boolean rideReqExpired = (RideRequestUtils.calculateExpireTimer(expiryTime, currTime))<=1;
                Log.e(TAG, "TimeDifference : " + (RideRequestUtils.calculateExpireTimer(expiryTime, currTime)));
                if (RideRequestUtils.calculateExpireTimer(expiryTime, currTime) > 2){
                    if (checkPermission(context)) {
                        //Starting OverlaySheetService
                        if (binder == null) {
                            context.startService(svcT);
                            listData.add(sheetData);
                        } else {
                            new Handler(Looper.getMainLooper()).post(() -> {
                                if (binder != null) {
                                    binder.getService().addToList(sheetData);
                                }
                            });
                        }
                        context.bindService(svcT, new ServiceConnection() {
                            @Override
                            public void onServiceConnected(ComponentName name, IBinder service) {
                                if (service instanceof OverlaySheetService.OverlayBinder) {
                                    new Handler(Looper.getMainLooper()).post(() -> {
                                        binder = (OverlaySheetService.OverlayBinder) service;
                                        ArrayList<Bundle> x = listData;
                                        listData = new ArrayList<>();
                                        for (Bundle item : x) {
                                            binder.getService().addToList(item);
                                        }
                                    });
                                }
                            }

                            @Override
                            public void onServiceDisconnected(ComponentName name) {
                                binder = null;
                            }
                        }, BIND_AUTO_CREATE);
                    } else {
                        if (overlayFeatureNotAvailable(context)) {
                            Handler handler = new Handler(Looper.getMainLooper());
                            handler.postDelayed(() -> {
                                try {
                                    String reqId = entity_payload.getString("searchRequestId");
                                    boolean isClearedReq = MyFirebaseMessagingService.clearedRideRequest.containsKey(reqId);
                                    if (isClearedReq){
                                        MyFirebaseMessagingService.clearedRideRequest.remove(reqId);
                                        return;
                                    }
                                    mFirebaseAnalytics.logEvent("low_ram_device", params);
                                    if (RideRequestActivity.getInstance() == null) {
                                        Intent intent = new Intent(context.getApplicationContext(), RideRequestActivity.class);
                                        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_REORDER_TO_FRONT);
                                        intent.putExtras(sheetData);
                                        context.getApplicationContext().startActivity(intent);
                                    } else {
                                        RideRequestActivity.getInstance().addToList(sheetData);
                                    }
                                    lastRideReq = new Bundle();
                                    lastRideReq.putAll(sheetData);
                                    lastRideReq.putBoolean("rideReqExpired", rideReqExpired);
                                    RideRequestUtils.createRideRequestNotification(context);
                                } catch (Exception e) {
                                    Exception exception = new Exception("Error in onCreate ride req activity " + e);
                                    FirebaseCrashlytics.getInstance().recordException(exception);
                                    params.putString("exception", e.toString());
                                    mFirebaseAnalytics.logEvent("exception_in_opening_ride_req_activity", params);
                                }
                            }, (sheetData.getInt("keepHiddenForSeconds", 0) * 1000L));
                        }

//                        Log.i("notificationCallback_size", Integer.toString(notificationCallback.size()));
                        System.out.println("no_overlay_permission" + " <> " + searchRequestId + " <> " + sharedPref.getString("DRIVER_ID", "null"));
                        Bundle overlayPermissionParams = new Bundle();
                        overlayPermissionParams.putString("search_request_id", searchRequestId);
                        overlayPermissionParams.putString("driver_id", sharedPref.getString("DRIVER_ID", "null"));
                        mFirebaseAnalytics = FirebaseAnalytics.getInstance(context);
                        mFirebaseAnalytics.logEvent("no_overlay_permission", overlayPermissionParams);
                    }
                } else {
                    Bundle overlayParams = new Bundle();
                    String expireTimer = String.valueOf(RideRequestUtils.calculateExpireTimer(expiryTime, currTime));
                    System.out.println("expired notification" + " <> " + currTime + " <> " + expiryTime + " <> " + expireTimer + " <> " + searchRequestId + " <> " + sharedPref.getString("DRIVER_ID","null"));
                    overlayParams.putString("current_time", currTime);
                    overlayParams.putString("expiry_time", expiryTime);
                    overlayParams.putString("time_difference", expireTimer);
                    overlayParams.putString("search_request_id", searchRequestId);
                    overlayParams.putString("driver_id", sharedPref.getString("DRIVER_ID", "null"));
                    mFirebaseAnalytics = FirebaseAnalytics.getInstance(context);
                    mFirebaseAnalytics.logEvent("overlay_popup_expired", overlayParams);
                    RideRequestUtils.addRideReceivedEvent(entity_payload, null,null,"overlay_popup_expired", context);
                }
                notificationId++;
                for(Iterator<Map.Entry<String, Long>> iterator = MyFirebaseMessagingService.clearedRideRequest.entrySet().iterator(); iterator.hasNext(); ) {
                    Map.Entry<String, Long> entry = iterator.next();
                      if(RideRequestUtils.timeDifferenceInMinutes(entry.getValue(), System.currentTimeMillis()) > 30) {
                          iterator.remove();
                      }
                  }
            }
            handleClearedReq(context, notificationType, data);
            removeExpiredNotificationIds();
        } catch (Exception e) {
            Log.i("SHOW_ALLOCATION", "error occurred" + e);
            e.printStackTrace();
            Exception exception = new Exception("Error in showAllocationNotification " + e);
            FirebaseCrashlytics.getInstance().recordException(exception);
            if (mFirebaseAnalytics!=null) mFirebaseAnalytics.logEvent("exception_in_showAllocationNotification", new Bundle());
        }
    }

    private static void removeExpiredNotificationIds(){
        final SimpleDateFormat f = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'",new Locale("en","US"));
        f.setTimeZone(TimeZone.getTimeZone("IST"));
        String currTime = f.format(new Date());

        Iterator<Map.Entry<String, String>> iterator = MyFirebaseMessagingService.notificationIdsReceived.entrySet().iterator();
        while(iterator.hasNext()){
            Map.Entry<String, String> entry = iterator.next();
            if(RideRequestUtils.calculateExpireTimer(entry.getValue(), currTime) < 1){
                Log.i("SHOW_ALLOCATION", "Removing the entry for notification Id : " + entry.getKey() + " - expiry time : " + entry.getValue() + " - curr time : " + currTime);
                iterator.remove();
            }
        }
    }

    private static void handleClearedReq (Context context, String notificationType, JSONObject data) throws JSONException {
        if (notificationType.equals(context.getString(R.string.CLEARED_FARE)) || notificationType.equals(context.getString(R.string.CANCELLED_SEARCH_REQUEST))) {
            if (binder != null) {
                if (!binder.getService().removeCardById(data.getString(context.getString(R.string.entity_ids))))
                {
                    MyFirebaseMessagingService.clearedRideRequest.put(data.getString("entity_ids"),System.currentTimeMillis());
                }
            }
            if (overlayFeatureNotAvailable(context)){
                RideRequestActivity requestActivity =  RideRequestActivity.getInstance();
                if (requestActivity != null){
                    boolean isCardRemoved = RideRequestActivity.getInstance().removeCardById(data.getString(context.getString(R.string.entity_ids)));
                    if (!isCardRemoved) {
                        MyFirebaseMessagingService.clearedRideRequest.put(data.getString("entity_ids"),System.currentTimeMillis());
                    }
                } else {
                    MyFirebaseMessagingService.clearedRideRequest.put(data.getString("entity_ids"),System.currentTimeMillis());
                }
            }
        }
    }

    public static void showNotification(Context context, String title, String msg, JSONObject data, String imageUrl) throws JSONException {
        Log.e(TAG, "SHOWNOTIFICATION MESSAGE");
        int smallIcon =  Utils.getResIdentifier(context, (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) ? "ic_launcher_small_icon" : "ic_launcher", "drawable") ;
        String show_notification = data.getString("show_notification");
        Bitmap bitmap = null;
        if (imageUrl != null) {
            bitmap = getBitmapfromUrl(imageUrl);
        }
        SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        boolean incVol = sharedPref.getString("AUTO_INCREASE_VOL", "true").equals("true");
        String disabilityName = sharedPref.getString("DISABILITY_NAME", "");
        final PackageManager pm = context.getPackageManager();
        Intent intent = pm.getLaunchIntentForPackage(context.getPackageName());
        System.out.println("Notificationn Utils Data" + data.toString());
        System.out.println("Notificationn111" + data.getString("notification_type"));
        System.out.println("Notificationn222" + (data.getString("entity_ids")));
        System.out.println("imageUrl" + imageUrl);
        System.out.println("smallIcon" + smallIcon);
        intent.putExtra("NOTIFICATION_DATA", data.toString());
        JSONObject jsonObject = new JSONObject();
        jsonObject.put("title",title)
                  .put("msg",msg);
        intent.putExtra("fullNotificationBody",jsonObject.toString());
        //            intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_CLEAR_TASK);
        intent.setFlags(Intent.FLAG_ACTIVITY_SINGLE_TOP);
        PendingIntent pendingIntent = PendingIntent.getActivity(context, notificationId, intent, PendingIntent.FLAG_IMMUTABLE);
        String notificationType = data.getString("notification_type");
        String channelId;
        String merchantType = context.getString(R.string.service);
        String key = merchantType.contains("provider") ? "DRIVER" : "USER";
        System.out.println("key" + key);
        if (MyFirebaseMessagingService.NotificationTypes.TRIP_STARTED.equals(notificationType)) {
            channelId = RIDE_STARTED;
        }
        else if (notificationType.equals(MyFirebaseMessagingService.NotificationTypes.CANCELLED_PRODUCT) ||
                notificationType.equals(MyFirebaseMessagingService.NotificationTypes.DRIVER_HAS_REACHED) ||
                notificationType.equals(MyFirebaseMessagingService.NotificationTypes.SOS_RESOLVED) ||
                notificationType.equals(MyFirebaseMessagingService.NotificationTypes.SOS_TRIGGERED) ||
                notificationType.equals(MyFirebaseMessagingService.NotificationTypes.DRIVER_QUOTE_INCOMING) ||
                notificationType.equals(MyFirebaseMessagingService.NotificationTypes.DRIVER_ASSIGNMENT) ||
                notificationType.equals(MyFirebaseMessagingService.NotificationTypes.REALLOCATE_PRODUCT)) {
            channelId = notificationType + "_NEW";
        } 
        else {
            channelId = GENERAL_NOTIFICATION;
        }

        if(!allowSoundForNotification(notificationType,data,key)){
            channelId = NOSOUND_NOTIFICATION;
        }
        NotificationCompat.Builder mBuilder = new NotificationCompat.Builder(context, channelId);
        if (imageUrl != null && bitmap != null) {
            mBuilder.setLargeIcon(bitmap)
                    .setSmallIcon(smallIcon)
                    .setContentTitle(title)
                    .setContentText(msg)
                    .setStyle(new NotificationCompat.BigTextStyle().bigText(title))
                    .setStyle(new NotificationCompat.BigTextStyle().bigText(msg))
                    .setAutoCancel(true)
                    .setContentIntent(pendingIntent)
                    .setChannelId(channelId)
                    .setStyle(
                            new NotificationCompat.BigPictureStyle()
                                    .bigPicture(bitmap)
                                    .bigLargeIcon(null));
        } else {
            mBuilder.setSmallIcon(smallIcon)
                    .setContentTitle(title)
                    .setContentText(msg)
                    .setStyle(new NotificationCompat.BigTextStyle().bigText(title))
                    .setStyle(new NotificationCompat.BigTextStyle().bigText(msg))
                    .setAutoCancel(true)
                    .setContentIntent(pendingIntent)
                    .setChannelId(channelId);
        }

       if (Build.VERSION.SDK_INT < Build.VERSION_CODES.O) {
            System.out.println("Default sound");
            Uri notificationSound;
            if (notificationType.equals(ALLOCATION_TYPE)) {
                notificationSound = Uri.parse("android.resource://" + context.getPackageName() + "/raw/allocation_request");
            } else if (notificationType.equals(MyFirebaseMessagingService.NotificationTypes.TRIP_STARTED)) {
                notificationSound = Uri.parse("android.resource://" + context.getPackageName() + "/raw/ride_started");
            } else if (notificationType.equals(MyFirebaseMessagingService.NotificationTypes.CANCELLED_PRODUCT) || notificationType.equals(MyFirebaseMessagingService.NotificationTypes.REALLOCATE_PRODUCT)){
                notificationSound = Uri.parse("android.resource://" + context.getPackageName() + "/raw/cancel_notification_sound");
            } else if (notificationType.equals(MyFirebaseMessagingService.NotificationTypes.SOS_TRIGGERED)){
                notificationSound = Uri.parse("android.resource://" + context.getPackageName() + "/raw/ny_ic_sos_danger");
            } else if (notificationType.equals(MyFirebaseMessagingService.NotificationTypes.SOS_RESOLVED)){
                notificationSound = Uri.parse("android.resource://" + context.getPackageName() + "/raw/ny_ic_sos_safe");
            } else {
                notificationSound = RingtoneManager.getDefaultUri(RingtoneManager.TYPE_NOTIFICATION);
            }
            boolean playSoundForNotification = allowSoundForNotification(notificationType,data,key);
            if(playSoundForNotification){
              mBuilder.setSound(notificationSound);
            } 
            System.out.println("Default sound" + notificationSound);
       }
        NotificationManagerCompat notificationManager = NotificationManagerCompat.from(context);
        System.out.println("In clean notification before notify");

        if (notificationType.equals(ALLOCATION_TYPE)) {
            System.out.println("In clean notification if");
        }


        if (MyFirebaseMessagingService.NotificationTypes.TRIP_STARTED.equals(notificationType)) {
            if (key.equals("USER")) {
                Utils.logEvent ("ny_user_ride_started", context);
                if (disabilityName.equals("BLIND_LOW_VISION")) {
                    startMediaPlayer(context, R.raw.ride_started_talkback, false);
                }
            }
            else
                Utils.logEvent("ride_started", context);
        }
        if (MyFirebaseMessagingService.NotificationTypes.TRIP_FINISHED.equals(notificationType)) {
            mFirebaseAnalytics = FirebaseAnalytics.getInstance(context);
            if (key.equals("USER")){
                if(disabilityName.equals("BLIND_LOW_VISION")) {
                    startMediaPlayer(context, R.raw.ride_completed_talkback, false);
                }
                Utils.logEvent("ny_user_ride_completed", context);
                String rideTaken = sharedPref.getString("HAS_TAKEN_FIRST_RIDE", "false");
                if(rideTaken.equals("false")){
                    Utils.logEvent("ny_user_first_ride_completed", context);
                }
            }
            else
                Utils.logEvent("ride_completed",context);
        }
        if (MyFirebaseMessagingService.NotificationTypes.CANCELLED_PRODUCT.equals(notificationType) ||
                MyFirebaseMessagingService.NotificationTypes.REALLOCATE_PRODUCT.equals(notificationType)) {
            mFirebaseAnalytics = FirebaseAnalytics.getInstance(context);
            if (key.equals("USER"))
            {if (MyFirebaseMessagingService.NotificationTypes.CANCELLED_PRODUCT.equals(notificationType)) {
                Utils.logEvent ("ny_user_ride_cancelled", context);
            }
            else
                Utils.logEvent("ny_user_ride_reallocation", context);}
            else
                Utils.logEvent("ride_cancelled", context);
            if (key.equals("DRIVER") && msg.contains("Customer had to cancel your ride")) {
                startMediaPlayer(context, R.raw.ride_cancelled_media, incVol);
            } else {
                int cancellationSound = R.raw.cancel_notification_sound;
                if (disabilityName.equals("BLIND_LOW_VISION"))
                {  if (msg.contains("The driver had cancelled the ride") || msg.contains("The driver had to cancel the ride") )
                    cancellationSound = R.raw.ride_cancelled_by_driver;
                    else
                        cancellationSound = R.raw.you_have_cancelled_the_ride;
                }
                if(allowSoundForNotification(notificationType,data,key)) {
                    startMediaPlayer(context, cancellationSound, false);
                }
            }
        }

        if(MyFirebaseMessagingService.NotificationTypes.DRIVER_HAS_REACHED.equals(notificationType) && key.equals("USER")){
            startMediaPlayer(context, R.raw.driver_arrived, false );
        }

        if (MyFirebaseMessagingService.NotificationTypes.DRIVER_ASSIGNMENT.equals(notificationType)) {
            mFirebaseAnalytics = FirebaseAnalytics.getInstance(context);
            int audio = R.raw.ride_assigned;
            if (key.equals("USER")) {
                Utils.logEvent ("ny_user_ride_assigned", context);
            }
            if (key.equals("DRIVER")) {
                Utils.logEvent("driver_assigned", context);
            }
            if(key.equals("USER") && disabilityName.equals("BLIND_LOW_VISION")) {
                audio = R.raw.ride_assigned_talkback;
            }
            if(allowSoundForNotification(notificationType,data,key)) {
                startMediaPlayer(context, audio, key.equals("DRIVER") && incVol );
            }
        }
        notificationId++;
        if(MyFirebaseMessagingService.NotificationTypes.NEW_STOP_ADDED.equals(notificationType) || MyFirebaseMessagingService.NotificationTypes.EDIT_STOP.equals(notificationType) ){
            for (int i = 0; i < callBack.size(); i++) {
                callBack.get(i).addStopCallBack(data.optString("stopStatus", ""));
            }
        }
        if (ActivityCompat.checkSelfPermission(context, Manifest.permission.POST_NOTIFICATIONS) != PackageManager.PERMISSION_GRANTED) {
            Log.e(LOG_TAG, "no notification permission");
        } else {
            notificationManager.notify(notificationId, mBuilder.build());
        }
    }

    public static Boolean allowSoundForNotification(String notificationType, JSONObject data,String key){
        if(data.has("rideTime")){
            try {
                long scheduled_ride_buffer_time = 30 * 60 * 1000;
                if(remoteConfigs.hasKey("scheduled_ride_buffer_time")){
                    scheduled_ride_buffer_time = remoteConfigs.getLong("scheduled_ride_buffer_time");
                }
                SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'", Locale.US);
                sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
                Date rideTime = sdf.parse(data.getString("rideTime"));
                Date currentTime = new Date();
                if (rideTime != null) {
                    long timeDifference = rideTime.getTime() - currentTime.getTime();
                    return (timeDifference < (long) scheduled_ride_buffer_time);
                }else{
                    return true;
                }
            } catch (Exception e) {
                Log.e(TAG, "Error parsing rideTime: " + e.getMessage());
            }
        }
        return true;
    }
    public static void triggerUICallbacks (String notificationType, String notificationData) {
        for (int i = 0; i < callBack.size(); i++) {
            callBack.get(i).customerCallBack(notificationType,notificationData);
            callBack.get(i).driverCallBack(notificationType, notificationData);
        }
    }

    private static Bitmap getBitmapfromUrl(String imageUrl) {
        try {
            URL url = new URL(imageUrl);
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            if (connection instanceof HttpsURLConnection)
                ((HttpsURLConnection) connection).setSSLSocketFactory(new TLSSocketFactory());
            connection.setDoInput(true);
            connection.connect();
            InputStream input = connection.getInputStream();
            return BitmapFactory.decodeStream(input);

        } catch (Exception e) {
            Log.e("awesome", "Error in getting notification image: " + e.getLocalizedMessage());
            return null;
        }
    }

    private static boolean checkPermission(Context context) {
        return Settings.canDrawOverlays(context.getApplicationContext());
    }

    public static void createNotificationChannel(Context context, String channel_Id) {
        try {
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                String description = "Important Notification";
                int importance = NotificationManager.IMPORTANCE_HIGH;
                if (channel_Id.equals("FLOATING_NOTIFICATION")) {
                    importance = NotificationManager.IMPORTANCE_DEFAULT;
                }
                NotificationChannel channel = new NotificationChannel(channel_Id, channel_Id, importance);
                channel.setDescription(description);
                switch (channel_Id) {
                    case RIDE_STARTED:
                        soundUri = Uri.parse("android.resource://" + context.getPackageName() + "/raw/ride_started");
                        channel.setName("Ride Started");
                        channel.setDescription("Used to notify when ride has started");
                        break;
                    case CANCELLED_PRODUCT:
                        soundUri = Uri.parse("android.resource://" + context.getPackageName() + "/raw/cancel_notification_sound");
                        channel.setName("Ride Cancelled");
                        channel.setDescription("Used to notify when ride is cancelled");
                        break;
                    case DRIVER_HAS_REACHED:
                        soundUri = Uri.parse("android.resource://" + context.getPackageName() + "/raw/driver_arrived");
                        channel.setName("Driver Arrived");
                        channel.setDescription("Used to notify when driver has arrived");
                        break;
                    case SOS_TRIGGERED:
                        soundUri = Uri.parse("android.resource://" + context.getPackageName() + "/raw/ny_ic_sos_danger");
                        channel.setName("SOS Triggered");
                        channel.setDescription("Used to alert when SOS is triggered by emergency contact");
                        break;
                    case SOS_RESOLVED:
                        soundUri = Uri.parse("android.resource://" + context.getPackageName() + "/raw/ny_ic_sos_safe");
                        channel.setName("SOS Resolved");
                        channel.setDescription("Used to alert when SOS is resolved by emergency contact");
                        break;
                    case DRIVER_QUOTE_INCOMING:
                        soundUri = RingtoneManager.getDefaultUri(RingtoneManager.TYPE_NOTIFICATION);
                        channel.setName("Driver Quote Incoming");
                        channel.setDescription("Driver quote related Notifications");
                        break;
                    case DRIVER_ASSIGNMENT:
                        soundUri = RingtoneManager.getDefaultUri(RingtoneManager.TYPE_NOTIFICATION);
                        channel.setName("Driver Assignment");
                        channel.setDescription("Driver Assignment related Notifications");
                        break;
                    case REALLOCATE_PRODUCT:
                        soundUri = RingtoneManager.getDefaultUri(RingtoneManager.TYPE_NOTIFICATION);
                        channel.setName("Ride Reallocation");
                        channel.setDescription("Ride Reallocation related Notifications");
                        break;
                    case NOSOUND_NOTIFICATION:
                        soundUri = null;
                        channel.setName("Other Silent notifications");
                        channel.setDescription("Other ride related silent Notifications");
                        channel.setSound(null,null);
                        break;
                    default:
                        soundUri = RingtoneManager.getDefaultUri(RingtoneManager.TYPE_NOTIFICATION);
                        channel.setName("Other ride related");
                        channel.setDescription("Other ride related Notifications");
                }


                AudioAttributes attributes = new AudioAttributes.Builder()
                        .setContentType(AudioAttributes.CONTENT_TYPE_SONIFICATION)
                        .setUsage(AudioAttributes.USAGE_NOTIFICATION)
                        .build();
                if(soundUri!=null){
                    channel.setSound(soundUri, attributes);
                }

                switch (channel_Id) {
                    case RIDE_STARTED:
                    case DRIVER_HAS_REACHED:
                    case CANCELLED_PRODUCT:
                    case DRIVER_QUOTE_INCOMING:
                    case DRIVER_ASSIGNMENT:
                    case REALLOCATE_PRODUCT:
                        channel.setGroup("2_ride_related");
                        break;
                    case SOS_TRIGGERED:
                    case SOS_RESOLVED:
                        channel.setGroup("1_safety");
                        break;
                }

                NotificationManager notificationManager = context.getSystemService(NotificationManager.class);
                if (notificationManager != null) {
                    notificationManager.createNotificationChannel(channel);
                }
            }
        } catch (Exception e) {
            Exception exception = new Exception("Error in Create Notification Channel " + e);
            FirebaseCrashlytics.getInstance().recordException(exception);
            e.printStackTrace();
        }
    }

    public static void startMediaPlayer(Context context, int mediaFile, boolean increaseVolume) {
        if (mediaPlayer != null) {
            mediaPlayer.stop();
            mediaPlayer = null;
        }
        AudioManager audio = (AudioManager) context.getSystemService(Context.AUDIO_SERVICE);
        if (increaseVolume)
            audio.setStreamVolume(AudioManager.STREAM_MUSIC, audio.getStreamMaxVolume(AudioManager.STREAM_MUSIC), 0); // need to verify this cc: @Vicky
        mediaPlayer = MediaPlayer.create(context, mediaFile);
        mediaPlayer.start();
    }

    public static void showRR(Context context, JSONObject entity_payload, JSONObject payload, String source){
        SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        boolean useSilentFCMForForwardBatch = fetchUseSilentFCMForForwardBatch(entity_payload);
        sharedPref.edit().putString(context.getString(R.string.RIDE_STATUS), context.getString(R.string.NEW_RIDE_AVAILABLE)).apply();
        boolean activityBasedChecks = Arrays.asList("onPause", "onDestroy").contains(sharedPref.getString("ACTIVITY_STATUS", "null"));
        boolean useWidgetService = (sharedPref.getString("DRIVER_STATUS_N", "null").equals("Silent") && activityBasedChecks) || useSilentFCMForForwardBatch;
        boolean widgetCheckForNonOverlay = useWidgetService && !NotificationUtils.overlayFeatureNotAvailable(context);
         boolean reqPresentCheckForWidget = binder == null && widgetCheckForNonOverlay;
         if (reqPresentCheckForWidget) {
             NotificationUtils.startWidgetService(context, null, payload, entity_payload, source);
        } else {
            NotificationUtils.showAllocationNotification(context, payload, entity_payload, source);
        }
    }

    public static void startWidgetService(Context context, String widgetMessage, JSONObject data, JSONObject payload, String source) {
        SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        Intent widgetService = new Intent(context, WidgetService.class);
        String key = context.getString(R.string.service);
        boolean useSilentFCMForForwardBatch = fetchUseSilentFCMForForwardBatch(payload);
        boolean activityStatusCheck = (sharedPref.getString(context.getResources().getString(R.string.ACTIVITY_STATUS), "null").equals("onPause") || sharedPref.getString(context.getResources().getString(R.string.ACTIVITY_STATUS), "null").equals("onDestroy"));
        String merchantType = key.contains("partner") || key.contains("driver") || key.contains("provider")? "DRIVER" : "USER";
        if (merchantType.equals("DRIVER") && Settings.canDrawOverlays(context) && !sharedPref.getString(context.getResources().getString(R.string.REGISTERATION_TOKEN), "null").equals("null") && !sharedPref.getString("ANOTHER_ACTIVITY_LAUNCHED", "false").equals("true") && (activityStatusCheck || useSilentFCMForForwardBatch)) {
            widgetService.putExtra(context.getResources().getString(R.string.WIDGET_MESSAGE), widgetMessage);
            widgetService.putExtra("payload", payload != null ? payload.toString() : null);
            widgetService.putExtra("data", data != null ? data.toString() : null);
            widgetService.putExtra("requestSource", source);
            widgetService.putExtra("isForwardRequest", useSilentFCMForForwardBatch);
            try {
                context.startService(widgetService);
            } catch (Exception e) {
                Exception exception = new Exception("Error in WidgetServiceStart " + e);
                FirebaseCrashlytics.getInstance().recordException(exception);
                e.printStackTrace();
            }
        }
    }
   
    public static boolean fetchUseSilentFCMForForwardBatch(JSONObject payload) {
        boolean isOnRide = payload.optBoolean("isOnRide", false);
        return payload.optBoolean("useSilentFCMForForwardBatch", false) && isOnRide;
    }

    public static void firebaseLogEventWithParams(Context context, String event, String paramKey, String paramValue) {
        Bundle params = new Bundle();
        params.putString(paramKey, paramValue);
        FirebaseAnalytics mFirebaseAnalytics = FirebaseAnalytics.getInstance(context);
        mFirebaseAnalytics.logEvent(event, params);
    }

    public static boolean overlayFeatureNotAvailable(Context context) {
        ActivityManager activityManager = (ActivityManager) context.getSystemService(ACTIVITY_SERVICE);
        boolean modelNotSupported = false;
        boolean isLowRamDevice = activityManager.isLowRamDevice();
        String buildModel = uniqueDeviceDetails();
        try {
            if (remoteConfigs.hasKey("overlay_not_supported_devices")){
                String devices = remoteConfigs.getString("overlay_not_supported_devices");
                JSONArray jsonArray = new JSONArray(devices);
                for ( int i = 0 ; i < jsonArray.length() ; i ++){
                    if (jsonArray.getString(i).equals(buildModel)){
                        modelNotSupported = true;
                        firebaseLogEventWithParams(context, "overlay_skipped","","");
                        break;
                    }
                }
            }
        }catch (Exception e){
            firebaseLogEventWithParams(context, "exception_in_overlayFeatureNotAvailable","","");
        }
        return isLowRamDevice || modelNotSupported;
    }

    public static void updateLocationUpdateDisAndFreq(String fcmType, SharedPreferences sharedPref) {
        if (remoteConfigs.hasKey("loc_pings_config")){
            String locationPingsConfig = remoteConfigs.getString("loc_pings_config");
            try{
                JSONObject locationPingsOb = new JSONObject(locationPingsConfig);
                boolean enabled = false;
                JSONArray enabledForFcmTypes = locationPingsOb.getJSONArray("enable_for_notification_type");
                for (int i = 0; i < enabledForFcmTypes.length(); i++) {
                    if (enabledForFcmTypes.getString(i).equals(fcmType)){
                        enabled = true;
                        break;
                    }
                }
                if (sharedPref == null || !enabled) return;
                String ride_g_freq_off_ride = locationPingsOb.getString("ride_g_freq_off_ride");
                String ride_g_freq_on_ride_pickup_stage = locationPingsOb.getString("ride_g_freq_on_ride_pickup_stage");
                String ride_g_freq_on_ride_drop_stage = locationPingsOb.getString("ride_g_freq_on_ride_drop_stage");
                String min_displacement_off_ride = locationPingsOb.getString("min_displacement_off_ride");
                String min_displacement_on_ride_pickup_stage = locationPingsOb.getString("min_displacement_on_ride_pickup_stage");
                String min_displacement_on_ride_drop_stage = locationPingsOb.getString("min_displacement_on_ride_drop_stage");
                switch (fcmType){
                    case MyFirebaseMessagingService.NotificationTypes.DRIVER_ASSIGNMENT:
                        sharedPref.edit().putString("RIDE_G_FREQUENCY", ride_g_freq_on_ride_pickup_stage).apply();
                        sharedPref.edit().putString("DRIVER_MIN_DISPLACEMENT", min_displacement_on_ride_pickup_stage).apply();
                        break;
                    case MyFirebaseMessagingService.NotificationTypes.TRIP_STARTED:
                        sharedPref.edit().putString("RIDE_G_FREQUENCY", ride_g_freq_on_ride_drop_stage).apply();
                        sharedPref.edit().putString("DRIVER_MIN_DISPLACEMENT", min_displacement_on_ride_drop_stage).apply();
                        break;
                    case MyFirebaseMessagingService.NotificationTypes.CANCELLED_PRODUCT:
                    case MyFirebaseMessagingService.NotificationTypes.TRIP_FINISHED:
                        sharedPref.edit().putString("RIDE_G_FREQUENCY", ride_g_freq_off_ride).apply();
                        sharedPref.edit().putString("DRIVER_MIN_DISPLACEMENT", min_displacement_off_ride).apply();
                        break;
                }
            }catch (Exception e){
                FirebaseCrashlytics.getInstance().recordException(e);
            }
        }
    }

    public static void createChatNotification(String sentBy, String message, Context context) {
        createChatNotificationChannel(context);
        Intent notificationIntent = context.getPackageManager().getLaunchIntentForPackage(context.getPackageName());
        JSONObject payload = new JSONObject();
        try {
            payload.put("notification_type", "CHAT_MESSAGE");
        } catch (JSONException e) {
            Log.e(LOG_TAG, "Error in adding data to jsonObject");
        }
        notificationIntent.putExtra("NOTIFICATION_DATA", payload.toString());
        notificationIntent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP | Intent.FLAG_ACTIVITY_SINGLE_TOP);
        PendingIntent pendingIntent = PendingIntent.getActivity(context, chatNotificationId, notificationIntent, PendingIntent.FLAG_IMMUTABLE);
        String _sentBy = "Message from " + sentBy;
        SpannableString titleBold = new SpannableString(_sentBy);
        titleBold.setSpan(new StyleSpan(Typeface.BOLD),0,_sentBy.length(), Spanned.SPAN_EXCLUSIVE_EXCLUSIVE);
        Notification notification =
                new NotificationCompat.Builder(context, "MessageUpdates")
                        .setContentTitle(titleBold)
                        .setAutoCancel(true)
                        .setContentText(message)
                        .setStyle(new NotificationCompat.BigTextStyle().bigText(titleBold))
                        .setStyle(new NotificationCompat.BigTextStyle().bigText(message))
                        .setSmallIcon(Utils.getResIdentifier(context, (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) ? "ic_launcher_small_icon" : "ny_ic_launcher", "drawable"))
                        .setDefaults(Notification.DEFAULT_ALL)
                        .setPriority(NotificationCompat.PRIORITY_HIGH)
                        .setContentIntent(pendingIntent)
                        .setLargeIcon(BitmapFactory.decodeResource(context.getResources(),Utils.getResIdentifier(context,"ny_ic_driver_profile", "drawable")))
                        .setSound(RingtoneManager.getDefaultUri(RingtoneManager.TYPE_NOTIFICATION))
                        .addAction(1, "Reply", pendingIntent)
                        .build();

        NotificationManagerCompat notificationManager = NotificationManagerCompat.from(context);
        if (ActivityCompat.checkSelfPermission(context, Manifest.permission.POST_NOTIFICATIONS) != PackageManager.PERMISSION_GRANTED) {
            Log.e(LOG_TAG, "no notification permission");
            return;
        }
        notificationManager.notify(chatNotificationId, notification);
    }

    private static void createChatNotificationChannel(Context context) {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            CharSequence name = "Chat Message";
            String description = "Chat Notification Channel";
            NotificationChannel channel = new NotificationChannel("MessageUpdates", name, NotificationManager.IMPORTANCE_HIGH);
            channel.setDescription(description);
            channel.setGroup("2_ride_related");
            NotificationManager notificationManager = context.getSystemService(NotificationManager.class);
            notificationManager.createNotificationChannel(channel);
        }
    }

    public static String getCategorizedVariant(String variant, Context context){
        SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        String driverVehicle = sharedPref.getString("VEHICLE_VARIANT","");
//        if (driverVehicle.equals(variant)) return NO_VARIANT; TODO :: if driver's variant is same then don't show
        String buildType = context.getResources().getString(R.string.service);
        switch (buildType){
            case  "yatrisathiprovider" : return NO_VARIANT;
            case "nammayatriprovider" : return NO_VARIANT;

            case "yatriprovider" :
                switch (variant){
                    case "SEDAN" : return  "Sedan";
                    case "HATCHBACK" : return "Hatchback";
                    case "TAXI_PLUS" : return "";
                    case "SUV" : return "Suv";
                    default : return NO_VARIANT;
                }
            default:return NO_VARIANT;
        }
    }

    public static String uniqueDeviceDetails() {
        String deviceDetails = "";
        String bDevice = Build.DEVICE;
        String bModel = Build.MODEL;
        String bManufacturer = Build.MANUFACTURER;
        String bBoard = Build.BOARD;
        String bHardware = Build.HARDWARE;
        bDevice = bDevice == null || bDevice.isEmpty() ? "null" : bDevice;
        bModel = bModel == null || bModel.isEmpty() ? "null" : bModel;
        bManufacturer = bManufacturer == null || bManufacturer.isEmpty() ? "null" : bManufacturer;
        bBoard = bBoard == null || bBoard.isEmpty() ? "null" : bBoard;
        bHardware = bHardware == null || bHardware.isEmpty() ? "null" : bHardware;
        deviceDetails = bManufacturer + "/" + bModel + "/" + bDevice + "/" + bBoard + "/" + bHardware;
        return deviceDetails;
    }

    public static void handleNotifications (String notificationType, JSONObject payload, String notificationId, Context context, SharedPreferences sharedPref,boolean triggerUICallback){
        try {
            if (notificationAlreadyProcessed(notificationId)) return;
            String title = payload.getString("title");
            String body = payload.getString("body");
            String imageUrl = payload.getString("imageUrl");
            String key = context.getString(R.string.service);
            String merchantType = key.contains("partner") || key.contains("driver") || key.contains("provider") ? "DRIVER" : "USER";
            if (triggerUICallback) NotificationUtils.triggerUICallbacks(notificationType, String.valueOf(new JSONObject().put("title", title).put("msg" , body)));
            switch (notificationType){
                case MyFirebaseMessagingService.NotificationTypes.DRIVER_ASSIGNMENT:
                    NotificationUtils.showNotification(context, title, body, payload, imageUrl);
                    sharedPref.edit().putString(context.getResources().getString(R.string.IS_RIDE_ACTIVE), "true").apply();
                    sharedPref.edit().putString(context.getString(R.string.RIDE_STATUS), context.getString(R.string.DRIVER_ASSIGNMENT)).apply();
                    startMainActivity(context);
                    if (merchantType.equals("DRIVER")){
                        NotificationUtils.updateLocationUpdateDisAndFreq(notificationType, sharedPref);
                    }
                    break;
                case MyFirebaseMessagingService.NotificationTypes.TRIP_STARTED:
                    if (payload.get("show_notification").equals("true")) {
                        NotificationUtils.showNotification(context, title, body, payload, imageUrl);
                    }
                    if (merchantType.equals("DRIVER")){
                        NotificationUtils.updateLocationUpdateDisAndFreq(notificationType, sharedPref);
                    }
                    break;
            }
        }catch (Exception e){
            FirebaseCrashlytics.getInstance().recordException(e);
        }
    }

    public static boolean notificationAlreadyProcessed (String notificationId){
        long currentTime = System.currentTimeMillis();
        synchronized (processedNotificationIds) {
            if (processedNotificationIds.containsKey(notificationId)) {
                return true;
            }
            //put new notificationId
            processedNotificationIds.put(notificationId, currentTime);
        }
        // Remove expired entries
        long notificationExpiryTime = remoteConfigs.hasKey("notificationExpiryTime") ? remoteConfigs.getLong("notificationExpiryTime") : 5 * 60 * 1000; // 5 minutes
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.N) {
            processedNotificationIds.entrySet().removeIf(entry -> currentTime - entry.getValue() >= notificationExpiryTime);
        } else {
            Iterator<Map.Entry<String, Long>> iterator = processedNotificationIds.entrySet().iterator();
            while (iterator.hasNext()) {
                Map.Entry<String, Long> entry = iterator.next();
                if (currentTime - entry.getValue() > notificationExpiryTime) {
                    iterator.remove();
                }
            }
        }
        return false;
    }

    public static void startMainActivity(Context context) {
        SharedPreferences sharedPref = context.getApplicationContext().getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        String key = context.getString(R.string.service);
        String merchantType = key.contains("partner") || key.contains("driver") || key.contains("provider")? "DRIVER" : "USER";
        if (merchantType.equals("DRIVER") && !sharedPref.getString(context.getResources().getString(R.string.REGISTERATION_TOKEN), "null").equals("null") && (sharedPref.getString(context.getResources().getString(R.string.ACTIVITY_STATUS), "null").equals("onPause") || sharedPref.getString(context.getResources().getString(R.string.ACTIVITY_STATUS), "null").equals("onDestroy"))) {
            Intent intent = context.getPackageManager().getLaunchIntentForPackage(context.getPackageName());
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_REORDER_TO_FRONT);
            try {
                context.getApplicationContext().startActivity(intent);
            } catch (Exception e) {
                Exception exception = new Exception("Error in startMainActivity " + e);
                FirebaseCrashlytics.getInstance().recordException(exception);
                NotificationUtils.firebaseLogEventWithParams(context, "exception_in_startMainActivity", "startMainActivity", String.valueOf(e));
            }
        }
    }
}