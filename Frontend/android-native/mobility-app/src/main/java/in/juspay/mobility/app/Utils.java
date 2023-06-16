package in.juspay.mobility.app;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.res.Configuration;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.net.Uri;
import android.os.Bundle;
import android.util.Base64;
import android.util.Log;

import androidx.annotation.Nullable;
import androidx.core.content.FileProvider;

import com.google.firebase.analytics.FirebaseAnalytics;
import com.theartofdev.edmodo.cropper.CropImage;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Locale;

import in.juspay.mobility.app.callbacks.CallBack;

public class Utils {

    private static final String UTILS = "UTILS";
    Context context;
    private FirebaseAnalytics mFirebaseAnalytics;
    private SharedPreferences sharedPref;

    public Utils(Context context) {
        this.context = context;
        mFirebaseAnalytics = FirebaseAnalytics.getInstance(context);
        sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key
        ),Context.MODE_PRIVATE);
    }

    private static final ArrayList<CallBack> callBack = new ArrayList<>();
    public static void registerCallback(CallBack notificationCallback)
    {
        callBack.add(notificationCallback);
    }
    public static void deRegisterCallback(CallBack notificationCallback)
    {
        callBack.remove(notificationCallback);
    }
    public void updateLocaleResource(String languageKey) {
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
            case "BN_IN" :
                locale = new Locale("bn");
                break;
            case "ML_IN" : 
                locale = new Locale("ml");
                break;
            default:
                return;
        }
        Locale.setDefault(locale);
        Configuration configuration = context.getResources().getConfiguration();
        configuration.setLocale(locale);
        context.getResources().updateConfiguration(configuration, context.getResources().getDisplayMetrics());
    }

    public void captureImage(@Nullable Intent data, Activity activity) {
        try {
            Uri imageUri;
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

    public void startCropImageActivity(Uri imageUri, Activity activity) {
        CropImage.activity(imageUri)
                .setAllowFlipping(false)
                .start(activity);
    }

    public void encodeImageToBase64(@Nullable Intent data) {
        try {
            CropImage.ActivityResult result = CropImage.getActivityResult(data);
            Uri fileUri = result.getUri();
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
                for(int i = 0; i< callBack.size(); i++) {
                    callBack.get(i).imageUploadCallBack(encImage, "IMG_" + timeStamp +".jpg", result.getUri().getPath());
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            Bundle params = new Bundle();
            mFirebaseAnalytics.logEvent("exception_crop_image", params);
        }
    }
    public static void minimizeApp(Context context){
        Intent startMain = new Intent(Intent.ACTION_MAIN);
        startMain.addCategory(Intent.CATEGORY_HOME);
        startMain.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        context.startActivity(startMain);
    }
}
