package in.juspay.mobility.common;

import static in.juspay.hyper.core.JuspayCoreLib.getApplicationContext;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.app.ActivityManager;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.content.res.Configuration;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Canvas;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.Parcel;
import android.os.Parcelable;
import android.provider.MediaStore;
import android.provider.Settings;
import android.util.Base64;
import android.util.Log;

import androidx.annotation.Nullable;
import androidx.core.app.ActivityCompat;
import androidx.core.content.ContextCompat;
import androidx.core.content.FileProvider;

import in.juspay.mobility.common.cropImage.CropImage;
import in.juspay.mobility.common.cropImage.CropImageActivity;
import in.juspay.mobility.common.cropImage.CropImageView;

import org.json.JSONObject;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;


public class Utils {

    private static final String UTILS = Utils.class.getSimpleName();

    private static final ArrayList<UICallBacks> callBack = new ArrayList<>();

    public static void registerCallback(UICallBacks notificationCallback) {
        callBack.add(notificationCallback);
    }

    public static void deRegisterCallback(UICallBacks notificationCallback) {
        callBack.remove(notificationCallback);
    }


    public static void updateLocaleResource(String languageKey, Context context) {
        Locale locale;
        switch (languageKey) {
            case "HI_IN":
                locale = new Locale("hi");
                break;
            case "KN_IN":
                locale = new Locale("kn");
                break;
            case "EN_US":
                locale = new Locale("en");
                break;
            case "TA_IN":
                locale = new Locale("ta");
                break;
            case "BN_IN":
                locale = new Locale("bn");
                break;
            case "ML_IN":
                locale = new Locale("ml");
                break;
            case "FR_FR":
                locale = new Locale("fr");
                break;
            case "TE_IN" :
                locale = new Locale("te");
                break;
            default:
                return;
        }
        Locale.setDefault(locale);
        Configuration configuration = context.getResources().getConfiguration();
        configuration.setLocale(locale);
        context.getResources().updateConfiguration(configuration, context.getResources().getDisplayMetrics());
    }

    public static String getUTCTimeStampFromMills(long time) {
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'", new Locale("en", "US"));
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
        Date locTime = new Date(time);
        return sdf.format(locTime);
    }

    public static CircleRippleEffectOptions getCircleOptionsFromJSON(JSONObject config, CircleRippleEffectOptions defOptions) {
        long delay = config.optLong("delay", defOptions.getDelay());
        long duration = config.optLong("duration", defOptions.getDuration());
        long pause = config.optLong("pause", defOptions.getPause());
        int repeatMode = config.optInt("repeatMode", defOptions.getRepeatMode());
        double maxRadius = config.optDouble("maxRadius", defOptions.getMaxRadius());
        double radius = config.optDouble("radius", defOptions.getRadius());
        double strokeWidth = config.optDouble("strokeWidth", defOptions.getStrokeWidth());
        double maxStrokeWidth = config.optDouble("maxStrokeWidth", defOptions.getMaxStrokeWidth());
        String fromStrokeColor = config.optString("fromStrokeColor", defOptions.getFromStrokeColor());
        String toStrokeColor = config.optString("toStrokeColor", fromStrokeColor);
        defOptions.radius((float) radius)
                .delay(delay)
                .duration(duration)
                .repeatMode(repeatMode)
                .pause(pause)
                .maxRadius((float) maxRadius)
                .strokeWidth((float) strokeWidth)
                .maxStrokeWidth((float) maxStrokeWidth)
                .fromStrokeColor(fromStrokeColor)
                .toStrokeColor(toStrokeColor);
        return defOptions;
    }


    public static Bitmap drawableToBitmap (Drawable drawable) {
        Bitmap bitmap = null;

        if (drawable instanceof BitmapDrawable) {
            BitmapDrawable bitmapDrawable = (BitmapDrawable) drawable;
            if(bitmapDrawable.getBitmap() != null) {
                return bitmapDrawable.getBitmap();
            }
        }

        if(drawable.getIntrinsicWidth() <= 0 || drawable.getIntrinsicHeight() <= 0) {
            bitmap = Bitmap.createBitmap(1, 1, Bitmap.Config.ARGB_8888); // Single color bitmap will be created of 1x1 pixel
        } else {
            bitmap = Bitmap.createBitmap(drawable.getIntrinsicWidth(), drawable.getIntrinsicHeight(), Bitmap.Config.ARGB_8888);
        }

        Canvas canvas = new Canvas(bitmap);
        drawable.setBounds(0, 0, canvas.getWidth(), canvas.getHeight());
        drawable.draw(canvas);
        return bitmap;
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
            startCropImageActivity(imageUri, activity, false);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void startCropImageActivity(Uri imageUri, Activity activity, boolean isCircularCrop) {
        if (isCircularCrop) {
            CropImage.activity(imageUri)
                    .setCropShape(CropImageView.CropShape.OVAL)
                    .setAspectRatio(1, 1)
                    .setAllowFlipping(false)
                    .setCropMenuCropButtonTitle("Done")
                    .start(activity);
        } else {
            CropImage.activity(imageUri)
                    .setAllowFlipping(false)
                    .setCropMenuCropButtonTitle("Done")
                    .start(activity);
        }
    }

    public static void encodeImageToBase64(@Nullable Intent data, Context context, @Nullable Uri imageData) {
        try {
            Uri fileUri;
            if(imageData == null) {
                CropImage.ActivityResult result = CropImage.getActivityResult(data);
                fileUri = result.getUri();
            }
            else {
                fileUri = imageData;
            }
            InputStream imageStream = context.getContentResolver().openInputStream(fileUri);
            Bitmap selectedImage = BitmapFactory.decodeStream(imageStream);
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
            {
                String timeStamp = new SimpleDateFormat("yyyyMMdd_HHmmss", Locale.getDefault()).format(new Date());
                for (int i = 0; i < callBack.size(); i++) {
                    if (fileUri != null) {
                        callBack.get(i).imageUploadCallBack(encImage, "IMG_" + timeStamp + ".jpg", fileUri.getPath());
                    }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    // Return: Device total RAM in GB
    public static int getDeviceRAM(){
        try{
            Context context = getApplicationContext();
            if(context == null)
                return 0;
            ActivityManager activityManager = (ActivityManager) context.getSystemService(Context.ACTIVITY_SERVICE);
            ActivityManager.MemoryInfo memInfo = new ActivityManager.MemoryInfo();
            activityManager.getMemoryInfo(memInfo);
            return (int)(1L + memInfo.totalMem / (1024L * 1024L * 1024L));
        }catch (Exception e){
            e.printStackTrace();
            return 0;
        }
    }

    public static void checkPermissionRationale(String permission, Context context, Activity activity) {
        if (context == null || activity == null) return;
        if (ContextCompat.checkSelfPermission(context, permission) == PackageManager.PERMISSION_DENIED) {
            if (ActivityCompat.shouldShowRequestPermissionRationale(activity, permission)){
                // We need to provide a rationale or explanation to the user before requesting the permission again
            } else {
                Intent appSettingsIntent = new Intent(Settings.ACTION_APPLICATION_DETAILS_SETTINGS);
                appSettingsIntent.setData(Uri.fromParts("package", context.getPackageName(), null));
                appSettingsIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                context.startActivity(appSettingsIntent);
            }
        }
    }
}
