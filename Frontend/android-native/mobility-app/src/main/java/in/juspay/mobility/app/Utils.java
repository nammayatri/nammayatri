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
import com.theartofdev.edmodo.cropper.CropImage;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import in.juspay.mobility.app.callbacks.CallBack;

public class Utils {

    private static final String UTILS = "UTILS";

    private static final ArrayList<CallBack> callBack = new ArrayList<>();

    public static void registerCallback(CallBack notificationCallback) {
        callBack.add(notificationCallback);
    }

    public static void deRegisterCallback(CallBack notificationCallback) {
        callBack.remove(notificationCallback);
    }

    public static void captureImage(@Nullable Intent data, Activity activity, Context context) {
        try {
            Uri imageUri;
            SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
            if (data == null || data.getData() == null) { //Camera
                File image = new File(context.getFilesDir(), "IMG_" + sharedPref.getString(context.getResources().getString(R.string.TIME_STAMP_FILE_UPLOAD), "null") + ".jpg");
                imageUri = FileProvider.getUriForFile(context, context.getPackageName() + ".provider", image);
            } else { // storage
                imageUri = data.getData();
            }
            startCropImageActivity(imageUri, activity);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void startCropImageActivity(Uri imageUri, Activity activity) {
        CropImage.activity(imageUri)
                .setAllowFlipping(false)
                .start(activity);
    }

    public static ImageProperties encodeImageToBase64(String path, Context context,Bitmap selectedImage) {
        FirebaseAnalytics mFirebaseAnalytics = FirebaseAnalytics.getInstance(context);
        try {
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            byte[] b;
            String encImage;
            selectedImage.compress(Bitmap.CompressFormat.JPEG, 100, baos);
            b = baos.toByteArray();
            encImage = Base64.encodeToString(b, Base64.NO_WRAP);

            Log.d(UTILS, "camera image size : " + (((encImage.length() / 4) * 3) / 1000));

            if (((encImage.length() / 4) * 3) / 1000 > 400) {
                int reduceQuality = 10;
                selectedImage.compress(Bitmap.CompressFormat.JPEG, 100 - reduceQuality, baos);
                b = baos.toByteArray();
                encImage = Base64.encodeToString(b, Base64.NO_WRAP);
                while (((encImage.length() / 4) * 3) / 1000 > 400) {
                    if (reduceQuality >= 90) {
                        break;
                    }
                    reduceQuality += 10;
                    baos.reset();
                    selectedImage.compress(Bitmap.CompressFormat.JPEG, 100 - reduceQuality, baos);
                    b = baos.toByteArray();
                    encImage = Base64.encodeToString(b, Base64.NO_WRAP);
                }
            }
            Log.d(UTILS, "encoded image size camera : " + (((encImage.length() / 4) * 3) / 1000));
            String timeStamp = new SimpleDateFormat("yyyyMMdd_HHmmss", Locale.getDefault()).format(new Date());
            return new ImageProperties(encImage, "IMG_" + timeStamp + ".jpg", path);
        } catch (Exception e) {
            e.printStackTrace();
            Bundle params = new Bundle();
            mFirebaseAnalytics.logEvent("exception_crop_image", params);
            return new ImageProperties(null, null, null);
        }
    }

    public static void minimizeApp(Context context) {
        Intent startMain = new Intent(Intent.ACTION_MAIN);
        startMain.addCategory(Intent.CATEGORY_HOME);
        startMain.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        context.startActivity(startMain);
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

}
