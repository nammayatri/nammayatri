/*
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.app;

import android.annotation.SuppressLint;
import android.app.AlarmManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.HandlerThread;
import android.os.Looper;
import android.util.Log;

import androidx.annotation.NonNull;

import com.clevertap.android.sdk.CleverTapAPI;
import com.clevertap.android.sdk.pushnotification.NotificationInfo;
import com.clevertap.android.sdk.pushnotification.fcm.CTFcmMessageHandler;
import com.google.android.gms.tasks.Task;
import com.google.firebase.analytics.FirebaseAnalytics;
import com.google.firebase.crashlytics.FirebaseCrashlytics;
import com.google.firebase.messaging.FirebaseMessaging;
import com.google.firebase.messaging.RemoteMessage;

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
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.net.ssl.HttpsURLConnection;

import in.juspay.mobility.app.callbacks.ShowNotificationCallBack;
import in.juspay.mobility.app.overlayMessage.MessagingView;
import in.juspay.mobility.app.overlayMessage.Service;
import in.juspay.mobility.common.services.TLSSocketFactory;

public  class MyFirebaseMessagingService {

    private static final ArrayList<BundleUpdateCallBack> bundleUpdate = new ArrayList<>();
    private static final ArrayList<ShowNotificationCallBack> showNotificationCallBacks = new ArrayList<>();
    public static final HashMap<String, Long> clearedRideRequest = new HashMap<>();

    public static final Hashtable<String, String> notificationIdsReceived = new Hashtable<>();
    private static final String LOG_TAG = "FirebaseMessagingService";


    public static void onNewToken(Context context, @NonNull String newToken) {
        Log.e("newToken", newToken);
        if(newToken.equals(" ") || newToken.equals("__failed") || newToken.equals("null") || newToken.equals("(null)") || newToken.isEmpty()){
            Task<String> tokenTask = FirebaseMessaging.getInstance().getToken();
            tokenTask.addOnCompleteListener(task -> {
                if (task.isSuccessful()) {
                    String token = task.getResult();
                    Log.d("TAG", "token: " + token);
                    handleFCMToken(context,token);
                }
            });
        }else handleFCMToken(context,newToken);
        Log.e("newToken", newToken);
    }

    private static void handleFCMToken (Context context, String newToken){
        SharedPreferences sharedPref = context.getSharedPreferences(
                context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        SharedPreferences.Editor editor = sharedPref.edit();
        editor.putString("FCM_TOKEN", newToken);
        editor.apply();
        CleverTapAPI cleverTapAPI = CleverTapAPI.getDefaultInstance(context);
        if (cleverTapAPI != null) {
            cleverTapAPI.pushFcmRegistrationId(newToken,true);
        }
        String regToken = sharedPref.getString("", "null");
        if (!regToken.equals("null") && !regToken.equals("__failed")) {
            updateFCMToken(context,newToken);
        }
    }

    public static void registerBundleUpdateCallback(BundleUpdateCallBack notificationCallback) {
        bundleUpdate.add(notificationCallback);
    }

    public static void deRegisterBundleUpdateCallback(BundleUpdateCallBack notificationCallback) {
        bundleUpdate.remove(notificationCallback);
    }

    public static void registerShowNotificationCallBack(ShowNotificationCallBack notificationCallback) {
        showNotificationCallBacks.add(notificationCallback);
    }

    public static void deRegisterShowNotificationCallBack(ShowNotificationCallBack notificationCallback) {
        showNotificationCallBacks.remove(notificationCallback);
    }

    public interface BundleUpdateCallBack {
        void callBundleUpdate();
    }

    public static  void onMessageReceived(Context context, @NonNull RemoteMessage remoteMessage) {
        Log.e("onMessageReceived FCM", remoteMessage.getData().toString());
        NotificationUtils.firebaseLogEventWithParams(context, "notification_received", "type", remoteMessage.getData().get("notification_type"));

        JSONObject payload = new JSONObject();
        JSONObject notification_payload = new JSONObject();
        JSONObject entity_payload = new JSONObject();
        try {
            if (!remoteMessage.getData().isEmpty()) {
                Bundle extras = new Bundle();
                for (Map.Entry<String, String> entry : remoteMessage.getData().entrySet()) {
                    extras.putString(entry.getKey(), entry.getValue());
                }

                NotificationInfo info = CleverTapAPI.getNotificationInfo(extras);
                if (info.fromCleverTap) {
                    new CTFcmMessageHandler().createNotification(context, remoteMessage);
                    return;
                }

                String title;
                String body;
                String imageUrl;

                if (remoteMessage.getData().containsKey("notification_json") && remoteMessage.getData().get("notification_json") != null) {
                    String notificationData = remoteMessage.getData().get("notification_json");
                    if (notificationData != null) {
                        notification_payload = new JSONObject(notificationData);
                    }
                }
                if (remoteMessage.getData().containsKey("entity_data")) {
                    String notificationType = remoteMessage.getData().get("notification_type");
                    if (notificationType != null && (notificationType.equals(NotificationTypes.NEW_RIDE_AVAILABLE) || notificationType.equals(NotificationTypes.TRIGGER_FCM) || notificationType.equals(NotificationTypes.EKD_LIVE_CALL_FEEDBACK))) {
                        String entityPayload = remoteMessage.getData().get("entity_data");
                        if (entityPayload != null) {
                            entity_payload = new JSONObject(entityPayload);
                            payload.put("entity_data", entity_payload);
                        }
                    }
                }

                RemoteMessage.Notification notification = remoteMessage.getNotification();
                if (notification != null) {
                    title = notification.getTitle();
                    body = notification.getBody();
                    imageUrl = remoteMessage.getData().get("image-url");
                } else {
                    title = notification_payload.optString("title");
                    body = notification_payload.optString("body");
                    if (notification_payload.has("imageUrl")) {
                        imageUrl = notification_payload.optString("imageUrl");
                    } else if (notification_payload.has("icon")) {
                        imageUrl = notification_payload.optString("icon");
                    } else {
                        imageUrl = null;
                    }
                }

                payload.put("notification_id", remoteMessage.getData().get("notification_id"));
                payload.put("notification_type", remoteMessage.getData().get("notification_type"));
                payload.put("entity_ids", remoteMessage.getData().get("entity_ids"));
                payload.put("entity_type", remoteMessage.getData().get("entity_type"));
                payload.put("show_notification", remoteMessage.getData().get("show_notification"));
                payload.put("title", title);
                payload.put("body", body);
                payload.put("imageUrl", imageUrl);

                SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
                String notificationType = (String) payload.get("notification_type");
                String notificationId = remoteMessage.getData().get("notification_id");
                stopChatService(context, notificationType, sharedPref);
                String key = context.getString(R.string.service);
                String merchantType = key.contains("partner") || key.contains("driver") || key.contains("provider") ? "DRIVER" : "USER";
                JSONObject notificationData = new JSONObject();
                String entityDataString = remoteMessage.getData().get("entity_data");
                if (entityDataString != null) {
                    try {
                        JSONObject entityData = new JSONObject(entityDataString);
                        if (entityData.has("rideTime")) {
                            notificationData.put("rideTime", entityData.getString("rideTime"));
                            payload.put("rideTime", entityData.getString("rideTime"));
                        }
                        if (entityData.has("bookingId")) {
                            notificationData.put("bookingId", entityData.getString("bookingId"));
                        }
                    } catch (JSONException e) {
                        Log.e("MyFirebaseMessagingService", "Error parsing entity_data: " + e.getMessage());
                        FirebaseCrashlytics.getInstance().recordException(e);
                    }
                }
                notificationData.put("title", title)
                        .put("msg", body);
                if (entityDataString != null) {
                    notificationData.put("entityData", entityDataString.replace("\"", "\\\""));
                }
                NotificationUtils.triggerUICallbacks(notificationType, notificationData.toString());
                switch (notificationType) {
                    case NotificationTypes.EDIT_STOP:
                    case NotificationTypes.NEW_STOP_ADDED:
                        if (remoteMessage.getData().containsKey("entity_data")) {
                            String entityPayload = remoteMessage.getData().get("entity_data");
                            if (entityPayload != null) {
                                JSONObject driverPayloadJsonObject = new JSONObject(entityPayload);
                                addUpdateStop(context,driverPayloadJsonObject);
                                if (driverPayloadJsonObject.has("isEdit") && !driverPayloadJsonObject.isNull("isEdit")) {
                                    String stopStatus = driverPayloadJsonObject.optBoolean("isEdit", false) ? "EDIT_STOP" : "ADD_STOP";
                                    payload.put("stopStatus", stopStatus);
                                    NotificationUtils.showNotification(context, title, body, payload, null);
                                }
                            }
                        }
                        break;
                    case NotificationTypes.DRIVER_NOTIFY:
                    case NotificationTypes.DRIVER_NOTIFY_LOCATION_UPDATE:
                        if (remoteMessage.getData().containsKey("driver_notification_payload")) {
                            String driverNotification = remoteMessage.getData().get("driver_notification_payload");
                            if (driverNotification != null) {
                                JSONObject driverNotificationJsonObject = new JSONObject(driverNotification);
                                if (notificationType.equals(NotificationTypes.DRIVER_NOTIFY_LOCATION_UPDATE)) {
                                    if (remoteMessage.getData().containsKey("entity_data")) {
                                        String entityPayload = remoteMessage.getData().get("entity_data");
                                        if (entityPayload != null) {
                                            JSONObject updateLocDetails = new JSONObject(entityPayload);
                                            driverNotificationJsonObject.put("updateLocDetails", updateLocDetails);
                                        }
                                    }
                                }
                                showOverlayMessage(context, driverNotificationJsonObject, NotificationTypes.DRIVER_NOTIFY_LOCATION_UPDATE);
                                if (driverNotificationJsonObject.has("showPushNotification") && !driverNotificationJsonObject.isNull("showPushNotification") && driverNotificationJsonObject.getBoolean("showPushNotification")) {
                                    NotificationUtils.showNotification(context, title, body, payload, null);
                                }
                            }
                        }
                        break;
                    case NotificationTypes.EDIT_LOCATION:
                        if (remoteMessage.getData().containsKey("driver_notification_payload")) {
                            String driverNotification = remoteMessage.getData().get("driver_notification_payload");
                            String jsonData = remoteMessage.getData().get("entity_data");
                            if (jsonData != null) {
                                JSONObject dataModel = new JSONObject(jsonData);
                                JSONObject driverNotificationModel = new JSONObject(driverNotification);
                                locationChanged(context, dataModel, driverNotificationModel);
                            }
                        }
                        break;
                    case NotificationTypes.CANCELLATION_RATE_NUDGE_DAILY :
                        cancellationNudgeOverlay(context, remoteMessage, NotificationTypes.CANCELLATION_RATE_NUDGE_DAILY);
                        break;
                    case NotificationTypes.CANCELLATION_RATE_NUDGE_WEEKLY :
                        cancellationNudgeOverlay(context, remoteMessage, NotificationTypes.CANCELLATION_RATE_NUDGE_WEEKLY);
                        break;
                    case NotificationTypes.DRIVER_STOP_DETECTED :
                        stopDetectedOverlay(context);
                    case NotificationTypes.TRIGGER_SERVICE:
                        if (merchantType.equals("DRIVER")) {
                            if (title != null && title.equals("You were inactive"))
                                FirebaseAnalytics.getInstance(context).logEvent("notification_trigger_service_inactive", new Bundle());
                            FirebaseAnalytics.getInstance(context).logEvent("notification_trigger_service", new Bundle());
                            RideRequestUtils.restartLocationService(context);
                        }
                        break;

                    case NotificationTypes.MARKETING_EVENTS:
                            if (title != null){
                                try {
                                   String destinationPayload = remoteMessage.getData().get("entity_data");
                                   if (destinationPayload != null){
                                    JSONArray array =  new JSONArray(destinationPayload);
                                    for(int i=0; i<array.length(); i++){
                                        Object item = array.get(i);
                                        if (item instanceof String) {
                                            String destination = (String) item;
                                            System.out.println("Event is :" + title + " , to be logged in : " + destination);
                                            if (destination.equalsIgnoreCase("FIREBASE")){
                                                FirebaseAnalytics.getInstance(context).logEvent(title, new Bundle());
                                            } else if (destination.equalsIgnoreCase("CLEVERTAP")){
                                                CleverTapAPI cleverTapAPI = CleverTapAPI.getDefaultInstance(context);
                                                if (cleverTapAPI != null) {
                                                    cleverTapAPI.pushEvent(title);
                                                }
                                            }
                                        }
                                    }
                                }
                            }catch(Exception e){Log.e(LOG_TAG, e.getMessage());}}
                        break;

                    case NotificationTypes.NEW_RIDE_AVAILABLE:
                        RideRequestUtils.addRideReceivedEvent(entity_payload, null, null, "ride_request_fcm_received", context);
                        if (sharedPref.getString("DISABLE_WIDGET", "null").equals("true") && sharedPref.getString("REMOVE_CONDITION", "false").equals("false")) {
                            if (sharedPref.getString("ACTIVITY_STATUS", "null").equals("onDestroy"))
                                NotificationUtils.showRR(context, entity_payload, payload, NotificationUtils.RequestSource.FCM);
                            else {
                                RideRequestUtils.addRideReceivedEvent(entity_payload, null, null, "ride_request_ignored", context);
                                NotificationUtils.firebaseLogEventWithParams(context, "ride_ignored", "payload", entity_payload.toString());
                            }
                        } else
                            NotificationUtils.showRR(context, entity_payload, payload, NotificationUtils.RequestSource.FCM);
                        break;

                    case NotificationTypes.CLEARED_FARE:
                        sharedPref.edit().putString(context.getString(R.string.CLEAR_FARE), String.valueOf(payload.get(context.getString(R.string.entity_ids)))).apply();
                        NotificationUtils.showAllocationNotification(context, payload, entity_payload, NotificationUtils.RequestSource.FCM);
                        NotificationUtils.startWidgetService(context, "CLEAR_FARE", payload, entity_payload, NotificationUtils.RequestSource.FCM);
                        break;

                    case NotificationTypes.CANCELLED_PRODUCT:
                        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.N) {
                            NotificationUtils.showNotification(context, title, body, payload, imageUrl);
                        }
                        sharedPref.edit().putString(context.getResources().getString(R.string.IS_RIDE_ACTIVE), "false").apply();
                        if (sharedPref.getString("MAPS_OPENED", "null").equals("true")) {
                            NotificationUtils.startMainActivity(context);
                        } else {
                            NotificationUtils.startWidgetService(context, context.getString(R.string.ride_cancelled), payload, entity_payload, NotificationUtils.RequestSource.FCM);
                        }
                        if (merchantType.equals("DRIVER")) {
                            NotificationUtils.updateLocationUpdateDisAndFreq(notificationType, sharedPref);
                        }
                        break;

                    case NotificationTypes.DRIVER_QUOTE_INCOMING:
                        if (sharedPref.getString(context.getResources().getString(R.string.ACTIVITY_STATUS), "null").equals("onPause")) {
                            NotificationUtils.showNotification(context, title, body, payload, imageUrl);
                        }
                        break;

                    case NotificationTypes.TRIP_FINISHED:
                        NotificationUtils.showNotification(context, title, body, payload, imageUrl);
                        if (merchantType.equals("USER")) {
                            String vehicleCategory = entity_payload.optString("vehicleCategory");
                            String appNamePrefix = getPackageNamePrefix(context.getPackageName());
                            String vehicleCategoryKey = getVehicleCategoryKey(vehicleCategory);
                            NotificationUtils.firebaseLogEventWithParams(context, appNamePrefix + vehicleCategoryKey + "ride_completed_", "vehicle_category", vehicleCategory);
                            sharedPref.edit().putInt("RIDE_COUNT", sharedPref.getInt("RIDE_COUNT", 0) + 1).apply();
                            sharedPref.edit().putString("COMPLETED_RIDE_COUNT", String.valueOf(sharedPref.getInt("RIDE_COUNT", 0))).apply();
                        } else {
                            NotificationUtils.updateLocationUpdateDisAndFreq(notificationType, sharedPref);
                        }
                        break;

                    case NotificationTypes.FIRST_RIDE_EVENT:
                        if (merchantType.equals("USER")) {
                            String vehicleCategory = entity_payload.optString("vehicleCategory");
                            String appNamePrefix = getPackageNamePrefix(context.getPackageName());
                            String vehicleCategoryKey = getVehicleCategoryKey(vehicleCategory);
                            NotificationUtils.firebaseLogEventWithParams(context, appNamePrefix + vehicleCategoryKey + "ride_completed_first", "vehicle_category", vehicleCategory);
                        }
                        break;
                    case NotificationTypes.DRIVER_ASSIGNMENT:
                    case NotificationTypes.TRIP_STARTED:
                        NotificationUtils.handleNotifications(notificationType, payload, notificationId, context, sharedPref, false);
                        break;

                    case NotificationTypes.TRIP_UPDATED:
                        for (ShowNotificationCallBack callbacks : showNotificationCallBacks) {
                            callbacks.hideInAppNotification("EditDest");
                        }
                        if (payload.get("show_notification").equals("true")) {
                            NotificationUtils.showNotification(context, title, body, payload, imageUrl);
                        }
                        break;

                    case NotificationTypes.BUNDLE_UPDATE:
                        try {
                            if (!bundleUpdate.isEmpty()) {
                                for (int i = 0; i < bundleUpdate.size(); i++) {
                                    bundleUpdate.get(i).callBundleUpdate();
                                }
                            } else {
                                NotificationUtils.firebaseLogEventWithParams(context, "unable_to_update_bundle", "reason", "Main Activity instance is null");
                            }
                        } catch (Exception e) {
                            Exception exception = new Exception("Error in BUNDLE_UPDATE " + e);
                            FirebaseCrashlytics.getInstance().recordException(exception);
                            e.printStackTrace();
                            NotificationUtils.firebaseLogEventWithParams(context, "exception_in_bundle_update _fcm", "exception", e.toString());
                        }
                        break;

                    case NotificationTypes.CANCELLED_SEARCH_REQUEST:
                        sharedPref.edit().putString(context.getString(R.string.CANCELLED_SEARCH_REQUEST), String.valueOf(payload.get(context.getString(R.string.entity_ids)))).apply();
                        NotificationUtils.showAllocationNotification(context, payload, entity_payload, NotificationUtils.RequestSource.FCM);
                        NotificationUtils.startWidgetService(context, "CLEAR_FARE", payload, entity_payload, NotificationUtils.RequestSource.FCM);
                        break;

                    case NotificationTypes.NEW_MESSAGE:
                        sharedPref.edit().putString("ALERT_RECEIVED", "true").apply();
                        NotificationUtils.showNotification(context, title, body, payload, imageUrl);
                        break;

                    case NotificationTypes.REGISTRATION_APPROVED:
                        sharedPref.edit().putString(context.getString(R.string.REGISTRATION_APPROVED), "true").apply();
                        NotificationUtils.showNotification(context, title, body, payload, imageUrl);
                        break;

                    case NotificationTypes.REFERRAL_ACTIVATED:
                        sharedPref.edit().putString("REFERRAL_ACTIVATED", "true").apply();
                        break;

                    case NotificationTypes.UPDATE_STORAGE:
                        if (notification_payload.has("storage_key") && notification_payload.has("storage_value")) {
                            String storage_key = notification_payload.get("storage_key").toString();
                            String storage_value = notification_payload.get("storage_value").toString();
                            if (storage_key.equals("update_driver_status") && merchantType.equals("DRIVER")) {
                                boolean status = storage_value.equals("SILENT") || storage_value.equals("ONLINE");
                                RideRequestUtils.updateDriverStatus(status, storage_value, context, true);
                            } else
                                sharedPref.edit().putString(storage_key, storage_value).apply();
                        }
                        NotificationUtils.showAllocationNotification(context, payload, entity_payload, NotificationUtils.RequestSource.FCM);
                        break;

                    case NotificationTypes.CALL_API:
                        try {
                            String endPoint = notification_payload.get("endpoint").toString();
                            String method = notification_payload.get("method").toString();
                            JSONObject reqBody = (JSONObject) notification_payload.get("reqBody");
                            RideRequestUtils.callAPIViaFCM(endPoint, reqBody, method, context);

                        } catch (Exception e) {
                            Exception exception = new Exception("Error in CALL_API " + e);
                            FirebaseCrashlytics.getInstance().recordException(exception);
                            Log.e(LOG_TAG, "Error in CALL_API " + e);
                        }
                    case NotificationTypes.CHAT_MESSAGE:
                        try {
                            String appState = "";
                            if (sharedPref != null)
                                appState = sharedPref.getString("ACTIVITY_STATUS", "null");
                            if ((appState.equals("onDestroy") || appState.equals("onPause"))) {
                                NotificationUtils.createChatNotification(title, body, context);
                            }
                        } catch (Exception e) {
                            Exception exception = new Exception("Error in ChatMessage " + e);
                            FirebaseCrashlytics.getInstance().recordException(exception);
                            Log.e("MyFirebaseMessagingService", "Error in CHAT_MESSAGE " + e);
                        }
                        break;
                    case NotificationTypes.REALLOCATE_PRODUCT:
                        try {
                            if (sharedPref.getString("REALLOCATE_PRODUCT_ENABLED", "false").equals("false"))
                                break;
                            SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'", new Locale("en", "US"));
                            dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
                            String getCurrTime = dateFormat.format(new Date());
                            sharedPref.edit().putString(context.getString(R.string.FINDING_QUOTES_START_TIME), getCurrTime).apply();
                            sharedPref.edit().putString(context.getString(R.string.LOCAL_STAGE), context.getString(R.string.ReAllocated)).apply();
                            NotificationUtils.showNotification(context, title, body, payload, imageUrl);
                            sharedPref.edit().putString(context.getResources().getString(R.string.IS_RIDE_ACTIVE), "false").apply();
                        } catch (Exception e) {
                            Exception exception = new Exception("Error in REALLOCATE_PRODUCT " + e);
                            FirebaseCrashlytics.getInstance().recordException(exception);
                            e.printStackTrace();
                        }
                        break;

                    case NotificationTypes.PAYMENT_OVERDUE:

                    case NotificationTypes.PAYMENT_PENDING:
                        showOverlayMessage(context, constructOverlayMessage(notification_payload), NotificationTypes.PAYMENT_PENDING);
                        break;

                    case NotificationTypes.JOIN_NAMMAYATRI:
                        JSONObject jsonObject = new JSONObject();
                        jsonObject.put("title", title);
                        jsonObject.put("description", body);
                        jsonObject.put("imageUrl", imageUrl);
                        jsonObject.put("buttonText", entity_payload.has("buttonText") && !entity_payload.isNull("buttonText") ? entity_payload.getString("buttonText") : "Okay");
                        jsonObject.put("heading", entity_payload.has("heading") && !entity_payload.isNull("heading") ? entity_payload.getString("heading") : "");
                        sharedPref.edit().putString("SHOW_JOIN_NAMMAYATRI", jsonObject.toString()).apply();
                        NotificationUtils.showNotification(context, title, body, payload, imageUrl);
                        break;
                    case NotificationTypes.UPDATE_BUNDLE:
                        Intent bundle = new Intent(context, RemoteAssetsDownloader.class);
                        bundle.putExtra("merchantType", merchantType);
                        context.startService(bundle);
                        break;
                    case NotificationTypes.FCM_UPDATE_BUNDLE:
                        if (remoteMessage.getData().containsKey("bundle_payload")) {
                            try {
                                Intent fcmBundle = new Intent(context, RemoteAssetsDownloader.class);
                                fcmBundle.putExtra("merchantType", merchantType);
                                fcmBundle.putExtra("bundleType", NotificationUtils.RequestSource.FCM);
                                fcmBundle.putExtra("payload", remoteMessage.getData().get("bundle_payload"));
                                context.startService(fcmBundle);
                            } catch (Exception e) {
                                Exception exception = new Exception("Error in FCM_UPDATE_BUNDLE " + e);
                                FirebaseCrashlytics.getInstance().recordException(exception);
                                startFCMBundleUpdateService(context, remoteMessage, merchantType);
                            }
                        }
                        break;
                    case NotificationTypes.SAFETY_ALERT:
                        showSafetyAlert(context, title, body, payload, imageUrl);
                        break;
                    case NotificationTypes.EKD_LIVE_CALL_FEEDBACK:
                        showOverlayMessage(context, payload, NotificationTypes.EKD_LIVE_CALL_FEEDBACK);
                    default:
                        if (payload.get("show_notification").equals("true")) {
                            NotificationUtils.showNotification(context, title, body, payload, imageUrl);
                        }  // Silent notification
                        break;
                }

            }
        } catch (Exception e) {
            Exception exception = new Exception("Error in exception_in_notification " + e);
            FirebaseCrashlytics.getInstance().recordException(exception);
            NotificationUtils.firebaseLogEventWithParams(context, "exception_in_notification", "remoteMessage", remoteMessage.getData().toString());
        }
    }

    private static String getVehicleCategoryKey(String vehicleCategory) {
        switch (vehicleCategory) {
            case "CAB":
                return "cab_";
            case "AUTO_RICKSHAW":
                return "auto_";
            case "METRO":
                return "metro_";
            case "MOTORCYCLE":
                return "bike_";
            case "AMBULANCE":
                return "ambulance_";
            default:
                return "not_found_";
        }
    }

    private static String getPackageNamePrefix(String appName) {
        if(appName.contains("nammayatri")){
            return "ny_";
        }else if (appName.contains("manayatri")) {
            return "my_";
        }else if (appName.contains("yatri")) {
            return "y_";
        }else {
            return "unknown_";
        }
    }

    public static void locationChanged(Context context,JSONObject dataModel, JSONObject driverNotification) {
        try {
            boolean hasAdvancedBooking = dataModel.getBoolean("hasAdvanceBooking");
            if (hasAdvancedBooking) return;
            JSONObject originData = dataModel.getJSONObject("origin");
            String lat = originData.getString("lat");
            String lon = originData.getString("lon");
            JSONObject locationChanged = new JSONObject();
            locationChanged.put("okButtonText", driverNotification.getString("okButtonText"));
            locationChanged.put("buttonOkVisibility",true);
            locationChanged.put("imageVisibility",true);
            locationChanged.put("imageUrl",driverNotification.getString("imageUrl"));
            locationChanged.put("buttonLayoutVisibility",true);
            locationChanged.put("titleVisibility",true);
            JSONArray arr = new JSONArray();
            arr.put("EDIT_LOCATION");
            locationChanged.put("actions",arr);
            locationChanged.put("title",driverNotification.getString("title"));
            locationChanged.put("lat", lat);
            locationChanged.put("lon", lon);
            showOverlayMessage(context,locationChanged, NotificationTypes.EDIT_LOCATION);
        } catch (Exception e) {
            e.printStackTrace();
            Log.e("MyFirebaseMessagingService", "Error in EDIT_LOCATION " + e);
        }
    }
    public static  void stopDetectedOverlay(Context context) {
        Intent intent = new Intent(context, MessageOverlayService.class);
        String date = String.valueOf(new Date());
        intent.putExtra("timestamp", date);
        intent.putExtra("isStopDetected", true);
        context.startService(intent);
    }
    public static void  cancellationNudgeOverlay(Context context, RemoteMessage remoteMessage, String overlayKey) {
        try {
            if (remoteMessage.getData().containsKey("driver_notification_payload")) {
                String driverNotification = remoteMessage.getData().get("driver_notification_payload");
                JSONObject driverNotificationModel = new JSONObject(driverNotification);
                if (driverNotificationModel != null) {
                    cancellationRateNudgeOverlay(context, driverNotificationModel, overlayKey);
                }
            }
        } catch (JSONException e) {
            Log.e("MyFirebaseMessagingService", "Error in cancellationNudgeOverlay -> " + overlayKey + " : " + e);
        }
    }

    public static void cancellationRateNudgeOverlay(Context context, JSONObject driverNotification, String overlayKey) {
        try {
            JSONObject cancellationNudge = new JSONObject();
            cancellationNudge.put("okButtonText", driverNotification.getString("okButtonText"));
            cancellationNudge.put("buttonOkVisibility",true);
            cancellationNudge.put("imageVisibility",true);
            cancellationNudge.put("imageUrl",driverNotification.getString("imageUrl"));
            cancellationNudge.put("buttonLayoutVisibility",true);
            cancellationNudge.put("titleVisibility",true);
            JSONArray arr = new JSONArray();
            arr.put(overlayKey);
            cancellationNudge.put("actions",arr);
            cancellationNudge.put("title",driverNotification.getString("title"));
            cancellationNudge.put("description", driverNotification.getString("description"));
            cancellationNudge.put("descriptionVisibility", driverNotification.getString("descriptionVisibility"));
            showOverlayMessage(context, cancellationNudge, overlayKey);
        } catch (Exception e) {
            e.printStackTrace();
            Log.e("MyFirebaseMessagingService", "Error in " + overlayKey + " : " + e);
        }
    }

    public static void startFCMBundleUpdateService(Context context, RemoteMessage remoteMessage, String merchantType) {
        try {
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
                AlarmManager manager = (AlarmManager) context.getSystemService(Context.ALARM_SERVICE);
                Intent alarmIntent = new Intent(context, FCMBundleUpdateBroadcastReceiver.class);
                alarmIntent.putExtra("payload",remoteMessage.getData().get("bundle_payload"));
                alarmIntent.putExtra("merchantType",merchantType);
                PendingIntent pendingIntent = PendingIntent.getBroadcast(context, 0, alarmIntent, PendingIntent.FLAG_IMMUTABLE);
                manager.setExact(AlarmManager.RTC_WAKEUP, System.currentTimeMillis(), pendingIntent);
            } else {
                RemoteAssetsDownloader remoteAssetsDownloader = new RemoteAssetsDownloader();
                String payload = remoteMessage.getData().get("bundle_payload");
                if(payload==null){
                    payload = "";
                }
                remoteAssetsDownloader.updateBundle(null, new JSONObject(payload), context);
            }
        } catch (Exception e) {
            Exception exception = new Exception("Error in startFCMBundleUpdateService " + e);
            FirebaseCrashlytics.getInstance().recordException(exception);
            Log.e("FCMBundleUpdateService", "Failed to start BundleUpdateService : " + e);
        }
    }

    private static void addUpdateStop(Context context,JSONObject driverPayloadJsonObject){
        try {
            String stopTitle = driverPayloadJsonObject.has("isEdit") && driverPayloadJsonObject.getBoolean("isEdit") ? context.getString(R.string.customer_edited) : context.getString(R.string.customer_added); // TODO:: Temporary added until context data comes from backend
            driverPayloadJsonObject.put("title", stopTitle);
            driverPayloadJsonObject.put("okButtonText", context.getString(R.string.navigate_to_location));
            driverPayloadJsonObject.put("cancelButtonText", context.getString(R.string.close));
            driverPayloadJsonObject.put("imageUrl", "https://firebasestorage.googleapis.com/v0/b/jp-beckn-dev.appspot.com/o/do_not%2Fadd_stop.png?alt=media&token=063c3661-3cfe-4950-a043-f7f49ed2c7fc");
            driverPayloadJsonObject.put("titleVisibility", true);
            driverPayloadJsonObject.put("buttonOkVisibility", true);
            driverPayloadJsonObject.put("buttonCancelVisibility", true);
            driverPayloadJsonObject.put("imageVisibility", true);
            driverPayloadJsonObject.put("buttonLayoutVisibility", true);
            driverPayloadJsonObject.put("actions", new JSONArray().put("NAVIGATE"));
            double editLat = Double.NaN;
            double editLon = Double.NaN;
            if (driverPayloadJsonObject.has("stop")) {
                JSONObject stopObject = driverPayloadJsonObject.getJSONObject("stop");
                if (stopObject.has("lat")) {
                    editLat = stopObject.getDouble("lat");
                }
                if (stopObject.has("lon")) {
                    editLon = stopObject.getDouble("lon");
                }
            }
            driverPayloadJsonObject.put("editlat", editLat);
            driverPayloadJsonObject.put("editlon", editLon);
            showOverlayMessage(context,driverPayloadJsonObject, NotificationTypes.NEW_STOP_ADDED);
        }catch (Exception e){
            Exception exception = new Exception("Error in addUpdateStop " + e);
            FirebaseCrashlytics.getInstance().recordException(exception);
            e.printStackTrace();
            NotificationUtils.firebaseLogEventWithParams(context, "exception_in_add_or_edit_stop_fcm", "exception", e.toString());
        }
    }
    private static void showOverlayMessage(Context context,JSONObject payload, String notificationType) {
        try {
            int delay = payload.optInt("delay", 0);
            HandlerThread handlerThread = new HandlerThread("OverlayHandlerThread");
            handlerThread.start();

            Handler handler = new Handler(handlerThread.getLooper());

            handler.postDelayed(() -> {
                try {
                    Intent showMessage = new Intent(context, Service.class);
                    showMessage.putExtra("payload", payload.toString());
                    showMessage.putExtra("notificationType", notificationType);
                    context.startService(showMessage);
                } catch (Exception e) {
                    Exception exception = new Exception("Error in showOverlayMessage " + e);
                    FirebaseCrashlytics.getInstance().recordException(exception);
                    Log.e(LOG_TAG, e.getMessage());
                }
            }, delay * 1000);

        } catch (Exception e) {
            Exception exception = new Exception("Error in showOverlayMessage " + e);
            FirebaseCrashlytics.getInstance().recordException(exception);
            e.printStackTrace();
        }
    }

    @SuppressLint("StaticFieldLeak")
    private static void updateFCMToken(Context context, final String deviceToken) {
        SharedPreferences sharedPref = context.getSharedPreferences(
                context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        String token = sharedPref.getString("REGISTERATION_TOKEN", "null");
        String baseUrl = sharedPref.getString("BASE_URL", "null");
        String deviceDetails = sharedPref.getString("DEVICE_DETAILS", "null");
        String key = context.getString(R.string.service);
        String merchantType = key.contains("partner") || key.contains("driver") || key.contains("provider") ? "DRIVER" : "USER";
        ExecutorService executor = Executors.newSingleThreadExecutor();
        Handler handler = new Handler(Looper.getMainLooper());
        executor.execute(() -> {
            StringBuilder result = new StringBuilder();
            try {
                String orderUrl;
                if (merchantType.equals("DRIVER")) {
                    orderUrl = baseUrl + "/driver/profile";
                } else {
                    orderUrl = baseUrl + "/profile";
                }
                System.out.print("in updateFCMToken");
                HttpURLConnection connection = (HttpURLConnection) (new URL(orderUrl).openConnection());
                if (connection instanceof HttpsURLConnection)
                    ((HttpsURLConnection) connection).setSSLSocketFactory(new TLSSocketFactory());
                connection.setRequestMethod("POST");
                connection.setRequestProperty("Content-Type", "application/json");
                connection.setRequestProperty("token", token);
                connection.setRequestProperty("x-device", deviceDetails);
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
                Log.i(LOG_TAG, "in result : " + result);

            } catch (Exception e) {
                Exception exception = new Exception("Error in updateFCMToken " + e);
                FirebaseCrashlytics.getInstance().recordException(exception);
                Log.e(LOG_TAG, "Catch in updateFCMToken : " + e);
            }
            handler.post(executor::shutdown);
        });
    }

    private static void stopChatService(Context context,String notificationType, SharedPreferences sharedPref) {
        if (notificationType.equals("TRIP_FINISHED") || notificationType.equals("CANCELLED_PRODUCT") || notificationType.equals("TRIP_STARTED") || notificationType.equals("REALLOCATE_PRODUCT")) {
            try {
                Intent chatListenerService = new Intent(context, ChatService.class);
                context.stopService(chatListenerService);
                SharedPreferences.Editor editor = sharedPref.edit();
                editor.putString("READ_MESSAGES", "0");
                editor.apply();
                Intent overlayService = new Intent(context, MessageOverlayService.class);
                context.stopService(overlayService);
            } catch (Exception e) {
                Exception exception = new Exception("Error in stopChatService " + e);
                FirebaseCrashlytics.getInstance().recordException(exception);
                Log.e("MyFirebaseMessagingService", "Error in stopChatService : " + e);
            }
        }
    }

    private static JSONObject constructOverlayMessage (JSONObject notification_payload) throws JSONException {
        JSONObject ob = new JSONObject();
        JSONArray jsonArray = new JSONArray();
        jsonArray.put("OPEN_APP");
        ob.put("title", notification_payload.get("title"))
                .put("description", notification_payload.get("body"))
                .put("cancelButtonText", "Not now")
                .put("okButtonText", "Go To Yatri Sathi")
                .put("imageUrl", "https://firebasestorage.googleapis.com/v0/b/my-re-cycler.appspot.com/o/ic_bill_generated(1).png?alt=media&token=9d9e7a0c-3805-4436-aa0f-95e2461cb787")
                .put("titleVisibility", true)
                .put("descriptionVisibility", true)
                .put("buttonOkVisibility", true)
                .put("buttonCancelVisibility", true)
                .put("imageVisibility", true)
                .putOpt("actions", jsonArray)
                .put("buttonLayoutVisibility", true);
        return ob;
    }

    private static void showSafetyAlert( Context context,String title, String body, JSONObject payload, String imageUrl){
        try {
            String notificationBody;
            String notificationTitle = context.getString(R.string.everything_okay);
            SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
            sharedPref.edit().putString("SAFETY_ALERT_TYPE", body).apply();
            CleverTapAPI cleverTapAPI = CleverTapAPI.getDefaultInstance(context);
            HashMap<String, Object> cleverTapParams = new HashMap<>();
            cleverTapParams.put("searchRequestId", body);
            if (cleverTapAPI != null) {
                cleverTapAPI.pushEvent("ny_user_night_safety_alert", cleverTapParams);
            }
            notificationBody = body.equals("Deviation") ? context.getString(R.string.safety_deviation_alert) : context.getString(R.string.we_noticed_your_driver_hasn_t_moved_for_a_while_are_you_feeling_safe_on_your_trip);
            NotificationUtils.showNotification(context, notificationTitle, notificationBody, payload, imageUrl);
        }
        catch (Exception e) {
            Exception exception = new Exception("Error in showSafetyAlert " + e);
            FirebaseCrashlytics.getInstance().recordException(exception);
            e.printStackTrace();
        }
    }
    public static class NotificationTypes {
        public static final String NEW_STOP_ADDED = "ADD_STOP";
        public static final String EDIT_STOP = "EDIT_STOP";
        public static final String TRIGGER_SERVICE = "TRIGGER_SERVICE";
        public static final String MARKETING_EVENTS = "MARKETING_EVENTS";
        public static final String NEW_RIDE_AVAILABLE = "NEW_RIDE_AVAILABLE";
        public static final String TRIGGER_FCM = "TRIGGER_FCM";
        public static final String CLEARED_FARE = "CLEARED_FARE";
        public static final String CANCELLED_PRODUCT = "CANCELLED_PRODUCT";
        public static final String DRIVER_QUOTE_INCOMING = "DRIVER_QUOTE_INCOMING";
        public static final String TRIP_FINISHED = "TRIP_FINISHED";

        public static final String FIRST_RIDE_EVENT = "FIRST_RIDE_EVENT";
        public static final String DRIVER_ASSIGNMENT = "DRIVER_ASSIGNMENT";
        public static final String TRIP_STARTED = "TRIP_STARTED";
        public static final String TRIP_UPDATED = "TRIP_UPDATED";
        public static final String BUNDLE_UPDATE = "BUNDLE_UPDATE";
        public static final String CANCELLED_SEARCH_REQUEST = "CANCELLED_SEARCH_REQUEST";
        public static final String NEW_MESSAGE = "NEW_MESSAGE";
        public static final String REGISTRATION_APPROVED = "REGISTRATION_APPROVED";
        public static final String REFERRAL_ACTIVATED = "REFERRAL_ACTIVATED";
        public static final String UPDATE_STORAGE = "UPDATE_STORAGE";
        public static final String CALL_API = "CALL_API";
        public static final String CHAT_MESSAGE = "CHAT_MESSAGE";
        private static final String EDIT_LOCATION = "EDIT_LOCATION";
        public static final String DRIVER_NOTIFY = "DRIVER_NOTIFY";
        public static final String DRIVER_NOTIFY_LOCATION_UPDATE = "DRIVER_NOTIFY_LOCATION_UPDATE";
        public static final String REALLOCATE_PRODUCT = "REALLOCATE_PRODUCT";
        public static final String PAYMENT_OVERDUE = "PAYMENT_OVERDUE";
        public static final String PAYMENT_PENDING = "PAYMENT_PENDING";
        public static final String JOIN_NAMMAYATRI = "JOIN_NAMMAYATRI";
        public static final String UPDATE_BUNDLE = "UPDATE_BUNDLE";
        public static final String FCM_UPDATE_BUNDLE = "FCM_UPDATE_BUNDLE";
        public static final String COINS_SUCCESS = "COINS_SUCCESS";
        public static final String SHARE_RIDE = "SHARE_RIDE";
        public static final String SOS_RESOLVED = "SOS_RESOLVED";
        public static final String SOS_TRIGGERED = "SOS_TRIGGERED";
        public static final String SOS_MOCK_DRILL = "SOS_MOCK_DRILL";
        public static final String FOLLOW_RIDE = "FOLLOW_RIDE";
        public static final String DRIVER_HAS_REACHED = "DRIVER_HAS_REACHED";
        public static final String SAFETY_ALERT = "SAFETY_ALERT";
        public static final String CANCELLATION_RATE_NUDGE_DAILY = "CANCELLATION_RATE_NUDGE_DAILY";
        public static final String CANCELLATION_RATE_NUDGE_WEEKLY = "CANCELLATION_RATE_NUDGE_WEEKLY";
        public static final String DRIVER_STOP_DETECTED = "DRIVER_STOP_DETECTED";
        public static final String EKD_LIVE_CALL_FEEDBACK = "EKD_LIVE_CALL_FEEDBACK";
    }
}