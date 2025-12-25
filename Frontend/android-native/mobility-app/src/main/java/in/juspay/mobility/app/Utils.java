package in.juspay.mobility.app;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.net.Uri;
import android.os.Bundle;
import android.util.Base64;
import android.util.Log;
import android.view.Gravity;

import androidx.annotation.Nullable;
import androidx.core.content.FileProvider;

import com.clevertap.android.sdk.CleverTapAPI;
import com.facebook.appevents.AppEventsLogger;
import com.google.firebase.analytics.FirebaseAnalytics;

import org.json.JSONException;
import org.json.JSONObject;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import in.juspay.mobility.app.RemoteConfigs.MobilityRemoteConfigs;
import in.juspay.mobility.app.callbacks.CallBack;

public class Utils {

    private static final String UTILS = "UTILS";
    public static final String DRIVER_STATUS = "DRIVER_STATUS_N";
    public static final String DRIVER_STATUS_OFFLINE = "Offline";


    public static void minimizeApp(Context context) {
        Intent startMain = new Intent(Intent.ACTION_MAIN);
        startMain.addCategory(Intent.CATEGORY_HOME);
        startMain.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        context.startActivity(startMain);
    }

    private static final Map<String, Integer> PRIORITY_MAP = Map.of(
            "PRIORITY_BALANCED_POWER_ACCURACY", 102,
            "PRIORITY_HIGH_ACCURACY", 100,
            "PRIORITY_LOW_POWER", 104,
            "PRIORITY_PASSIVE", 105
    );

    private static final int DEFAULT_PRIORITY = 102; // Default priority to PRIORITY_HIGH_ACCURACY if invalid

    public static int getPriority(String priority) {
        Integer value = PRIORITY_MAP.get(priority);
        return (value != null) ? value : DEFAULT_PRIORITY;
    }

    public static int getLocationPriority(String priority) {
        MobilityRemoteConfigs remoteConfigs = new MobilityRemoteConfigs(false, false);
        try {
            String priorityMap = remoteConfigs.getString("perf_config");
            JSONObject config = new JSONObject(priorityMap);
            int finalConfig = getPriority(config.optString(priority));
            Log.i("RemoteConfig", "Location Update Priority: " + config + " " + finalConfig);
            return finalConfig;

        } catch (Exception e) {
            Log.e("RemoteConfig", "Failed to parse JSON for location Update", e);
            return getPriority("");
        }
    }

    public static int getResIdentifier (Context context, String resName, String resType) {
        return context.getResources().getIdentifier(resName,resType,context.getPackageName());
    }

    public static void logEvent(String event, Context context) {
        CleverTapAPI clevertapDefaultInstance = CleverTapAPI.getDefaultInstance(context);
        Bundle params = new Bundle();
        FirebaseAnalytics mFirebaseAnalytics = FirebaseAnalytics.getInstance(context);
        mFirebaseAnalytics.logEvent(event, params);
        if (clevertapDefaultInstance != null){
            clevertapDefaultInstance.pushEvent(event);
        }
    }
    public static void logEventWithParams(String event, HashMap<String,String> params, Context context) {
        try {
            CleverTapAPI clevertapDefaultInstance = CleverTapAPI.getDefaultInstance(context);
            AppEventsLogger logger = AppEventsLogger.newLogger(context);
            FirebaseAnalytics mFirebaseAnalytics = FirebaseAnalytics.getInstance(context);
            Bundle bundleParams = new Bundle();
            for(Map.Entry<String, String> entry : params.entrySet()) {
                bundleParams.putString(entry.getKey(),entry.getValue());
                if (clevertapDefaultInstance != null) {
                    HashMap<String, Object> mapCustomEvent = new HashMap<>();
                    mapCustomEvent.put(entry.getKey(),entry.getValue());
                    clevertapDefaultInstance.pushEvent(event, mapCustomEvent);
                }
            }
            logger.logEvent(event, bundleParams);
            mFirebaseAnalytics.logEvent(event, bundleParams);
        } catch (Exception e) {
            Log.e(UTILS, "Error in logEventWithParams " + e);
        }
    }

    public static void setCleverTapUserProp(String key, String value, Context context) {
        try {
            CleverTapAPI clevertapDefaultInstance = CleverTapAPI.getDefaultInstance(context);
            HashMap<String, Object> profileUpdate = new HashMap<>();
            profileUpdate.put(key, value);
            if (clevertapDefaultInstance != null)
                clevertapDefaultInstance.pushProfile(profileUpdate);
        } catch (Exception e) {
            Log.e(UTILS, "Error sending user data: " + e);
        }
    }

    public static VariantType getVariantType(String variant) {
        if (variant.equals("Non AC Taxi")) {
            return VariantType.NON_AC;
        }
        return VariantType.AC;
    }

    public enum VariantType { AC, NON_AC }

    public static int getGravity(String gravity){
        switch (gravity){
            case "LEFT": return Gravity.LEFT;
            case "RIGHT" : return Gravity.RIGHT;
            case "TOP" :  return Gravity.TOP;
            case "BOTTOM" : return Gravity.BOTTOM;
            default: return Gravity.CENTER;}
    }

    public static JSONObject createNotificationPayload(String title, String message, String onTapAction, String action1Text, String action2Text, String action1Image, String action2Image, String channelId, int durationInMilliSeconds) throws JSONException {
        JSONObject notificationPayload = new JSONObject();
        notificationPayload
                .put("title", title)
                .put("message", message)
                .put("channelId", channelId)
                .put("action1Text", action1Text)
                .put("action2Text", action2Text)
                .put("action1Image", action1Image)
                .put("action2Image", action2Image)
                .put("onTapAction", onTapAction)
                .put("durationInMilliSeconds", durationInMilliSeconds);
        return  notificationPayload;
    }

    // method to convert dp to pixels
    public static int dpToPx(Context context, float dp) {
        return Math.round(dp * context.getResources().getDisplayMetrics().density);
    }

}
