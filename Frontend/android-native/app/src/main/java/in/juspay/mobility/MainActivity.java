/*
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility;

import static in.juspay.mobility.BuildConfig.MERCHANT_TYPE;
import static in.juspay.mobility.app.Utils.minimizeApp;

import android.Manifest;
import android.animation.Animator;
import android.annotation.SuppressLint;
import android.app.Activity;
import android.app.ActivityManager;
import android.app.AlertDialog;
import android.app.NotificationManager;
import android.content.Context;
import android.content.Intent;
import android.content.IntentSender;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.content.res.Resources;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.provider.Settings;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.InflateException;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.view.WindowManager;
import android.webkit.WebView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.cardview.widget.CardView;
import androidx.constraintlayout.widget.ConstraintLayout;
import androidx.core.app.ActivityCompat;
import androidx.work.WorkManager;

import com.airbnb.lottie.LottieAnimationView;
import com.clevertap.android.sdk.CleverTapAPI;
import com.clevertap.android.pushtemplates.PushTemplateNotificationHandler;
import com.clevertap.android.sdk.interfaces.NotificationHandler;
import com.google.android.gms.ads.identifier.AdvertisingIdClient;
import com.google.android.gms.common.ConnectionResult;
import com.google.android.gms.common.GoogleApiAvailability;
import com.google.android.gms.common.GooglePlayServicesNotAvailableException;
import com.google.android.gms.common.GooglePlayServicesRepairableException;
import com.google.android.gms.maps.MapsInitializer;
import com.google.android.gms.tasks.OnFailureListener;
import com.google.android.gms.tasks.Task;
import com.google.android.play.core.appupdate.AppUpdateInfo;
import com.google.android.play.core.appupdate.AppUpdateManager;
import com.google.android.play.core.appupdate.AppUpdateManagerFactory;
import com.google.android.play.core.install.model.AppUpdateType;
import com.google.android.play.core.install.model.UpdateAvailability;
import com.google.firebase.analytics.FirebaseAnalytics;
import com.google.firebase.dynamiclinks.FirebaseDynamicLinks;
import com.google.firebase.messaging.FirebaseMessaging;
import com.google.gson.Gson;

import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.Vector;
import java.util.concurrent.TimeUnit;

import in.juspay.hypersdk.core.PaymentConstants;
import in.juspay.hypersdk.data.JuspayResponseHandler;
import in.juspay.hypersdk.ui.HyperPaymentsCallbackAdapter;
import in.juspay.mobility.app.BootUpReceiver;
import in.juspay.mobility.app.ChatService;
import in.juspay.mobility.app.InAppNotification;
import in.juspay.mobility.app.LocationUpdateService;
import in.juspay.mobility.app.MyFirebaseMessagingService;
import in.juspay.mobility.app.NotificationUtils;
import in.juspay.mobility.app.RideRequestActivity;
import in.juspay.mobility.app.WidgetService;
import in.juspay.mobility.app.callbacks.ShowNotificationCallBack;
import in.juspay.mobility.common.MobilityCommonBridge;
import in.juspay.mobility.common.Utils;
import in.juspay.services.HyperServices;


public class MainActivity extends AppCompatActivity {

    private static final String LOG_TAG = "MAIN_ACTIVITY";

    private static int updateType;
    MyFirebaseMessagingService.BundleUpdateCallBack bundleUpdateCallBack;
    private HyperServices hyperServices;
    private Context context;
    private Activity activity;
    @Nullable
    private SharedPreferences sharedPref;
    @SuppressLint("StaticFieldLeak")
    private static InAppNotification inAppNotification;
    ShowNotificationCallBack inappCallBack;
    SharedPreferences.OnSharedPreferenceChangeListener mListener = new SharedPreferences.OnSharedPreferenceChangeListener() {
        @Override
        public void onSharedPreferenceChanged(SharedPreferences sharedPreferences, String key) {
            if (key != null && key.equals("LANGUAGE_KEY")) {
                Utils.updateLocaleResource(sharedPreferences.getString(key,"__failed"),context);
            }
            if (key != null && key.equals("REGISTERATION_TOKEN")) {
                String token = sharedPreferences.getString("REGISTERATION_TOKEN", "null");
                if (token.equals("__failed")) {
                    final PackageManager pm = getApplicationContext().getPackageManager();
                    final Intent intent = pm.getLaunchIntentForPackage(getApplicationContext().getPackageName());
                    try {
                        if (activity != null) {
                            activity.finishAffinity();// Finishes all activities.
                            activity.startActivity(intent);
                        } else {
                            sharedPreferences.edit().clear().apply();
                        }
                    } catch (NullPointerException e) {
                        e.printStackTrace();
                    }
                }
            }
            // Update Driver status in Local Storage
            if (key != null && key.equals("DRIVER_STATUS")) {
                String status = sharedPreferences.getString("DRIVER_STATUS", "null");
                WorkManager mWorkManager = WorkManager.getInstance(getApplicationContext());
                if (status.equals("null")) {
                    if (context != null) {
                        Intent locationUpdateIntent = new Intent(context, LocationUpdateService.class);
                        context.stopService(locationUpdateIntent);
                        mWorkManager.cancelAllWorkByTag(context.getString(in.juspay.mobility.app.R.string.location_update));
                    } else {
                        Context context = getApplicationContext();
                        Intent locationUpdateIntent = new Intent(context, LocationUpdateService.class);
                        context.stopService(locationUpdateIntent);
                        mWorkManager.cancelAllWorkByTag(context.getString(in.juspay.mobility.app.R.string.location_update));
                    }

                }
            }
            if (key != null && sharedPreferences.getString("DRIVER_STATUS", "null").equals("true") && (key.equals("RIDE_G_FREQUENCY") || key.equals("MAX_LIMIT_TO_STORE_LOCATION_PT") || key.equals("NO_OF_LOCATION_PT_TO_REMOVE") || key.equals("DRIVER_MIN_DISPLACEMENT") || key.equals("RIDE_T_FREQUENCY"))) {
                System.out.println("TRIGGERED UPDATE POLLING");
                Context context = getApplicationContext();
                Intent locationUpdateIntent = new Intent(context, LocationUpdateService.class);
                context.startService(locationUpdateIntent);
            }
        }
    };
    private Intent widgetService;
    private boolean isHideSplashEventCalled = false;
    private boolean isSystemAnimEnabled = true;
    private String GAID;

    @Override
    public void onBackPressed() {
        if (hyperServices != null && !hyperServices.onBackPressed()) {
            super.onBackPressed();
        }
    }

    public String getDeviceRAM() {
        String deviceRAM = "__failed";
        if (sharedPref != null) {
            deviceRAM = sharedPref.getString("DEVICE_RAM", "__failed");
        }
        if (!deviceRAM.equals("__failed"))
            return deviceRAM;
        long memory;
        try {
            ActivityManager activityManager = (ActivityManager) getApplicationContext().getSystemService(Context.ACTIVITY_SERVICE);
            ActivityManager.MemoryInfo memInfo = new ActivityManager.MemoryInfo();
            activityManager.getMemoryInfo(memInfo);
            memory = 1 + memInfo.totalMem / (1024 * 1024 * 1024);
            deviceRAM = memory == 0 ? "null" : memory + " GB";
            sharedPref.edit().putString("DEVICE_RAM", deviceRAM).apply();
        } catch (Exception e) {
            System.out.println("In getDeviceRAM error: ");
            e.printStackTrace();
        }
        return deviceRAM;
    }

    public String[] getScreenDimensions() {
        String[] res = new String[0];
        if (sharedPref != null) {
            res = new String[]{sharedPref.getString("DEVICE_RESOLUTION", "__failed"), sharedPref.getString("DEVICE_SIZE", "__failed")};
        }
        if (!res[0].equals("__failed") && !res[1].equals("__failed"))
            return res;
        int height;
        int width;
        float size;
        try {
            DisplayMetrics displayMetrics = new DisplayMetrics();
            getWindowManager().getDefaultDisplay().getRealMetrics(displayMetrics);
            height = displayMetrics.heightPixels;
            width = displayMetrics.widthPixels;
            float x = height / displayMetrics.ydpi;
            float y = width / displayMetrics.xdpi;
            size = (float) Math.sqrt(x * x + y * y);
            size = (float) Math.round(size * 100) / 100;
            res[0] = height != 0 && width != 0 ? height + "x" + width + "px" : "null";
            res[1] = size != 0 ? size + " Inches" : "null";
            sharedPref.edit().putString("DEVICE_RESOLUTION", res[0]).apply();
            sharedPref.edit().putString("DEVICE_SIZE", res[1]).apply();
        } catch (Exception e) {
            System.out.println("In getScreenDimensions error: ");
            e.printStackTrace();
        }
        return res;
    }

    public String getDeviceDetails() {
        String deviceDetails = "";
        try {
            String bVersion = Build.VERSION.RELEASE;
            String bModel = Build.MODEL;
            String bBrand = Build.BRAND;
            String[] dim = getScreenDimensions();
            String deviceRAM = getDeviceRAM();
            if (bModel == null || bModel.equals(""))
                bModel = "null";
            if (bBrand == null || bBrand.equals(""))
                bBrand = "null";
            bVersion = bVersion == null || bVersion.equals("") ? "null" : "Android v" + bVersion;
            deviceDetails = bBrand + "/" + bModel + "/" + bVersion + "/" + deviceRAM + "/" + dim[1] + "/" + dim[0];
        } catch (Exception e) {
            e.printStackTrace();
        }
        return deviceDetails;
    }

    private static HashMap<String, String> getQueryMap(String link) {
        String[] query_params_array = link.split("&");
        HashMap<String, String> query_params = new HashMap<>();
        for (String query_param : query_params_array) {
            String[] key_value = query_param.split("=");
            String key = key_value[0];
            String value = key_value[1];
            query_params.put(key, value);
        }
        return query_params;
    }

    @SuppressLint("SetJavaScriptEnabled")
    @Override
    protected void onCreate(Bundle savedInstanceState) {

        Vector<String> res = handleDeepLinkIfAvailable(getIntent());
        String viewParam = null, deepLinkJson =null;
        if (res!=null ){
            viewParam = res.get(0);
            deepLinkJson = res.get(1);
        }
        // https://nammayatri.in/partner?checking=this&vd=ewt34gwrg&fdg=srgrw34&er=e453tge3&pl=35t345gg // ---NOTE:// DeepLink example for debug
        
        super.onCreate(savedInstanceState);
        FirebaseAnalytics mFirebaseAnalytics = FirebaseAnalytics.getInstance(this);
        context = getApplicationContext();
        boolean isMigrated = migrateLocalStore(context);
        String clientId = context.getResources().getString(R.string.client_id);
        activity = this;
        try {
            setContentView(R.layout.activity_main);
        } catch (Exception e){
            Bundle bundle = new Bundle();
            bundle.putString("Exception",e.toString());
            mFirebaseAnalytics.logEvent("splash_screen_inflate_exception",bundle);
            setContentView(R.layout.activity_main_without_bg);
        }
//        String key = getResources().getString(R.string.service);
//        String androidId = Settings.Secure.getString(getContentResolver(),Settings.Secure.ANDROID_ID);

//        Bundle params = new Bundle(); TODO:: Do we want these anymore??
//        params.putString("id", androidId);
//        mFirebaseAnalytics.logEvent("device_id", params);

//        FirebaseDynamicLinks.getInstance() TODO:: This is deprecated and not suggested to use in projects. What was the intent of adding it??
//                .getDynamicLink(getIntent())
//                .addOnSuccessListener(this, pendingDynamicLinkData -> {
//                    // Get deep link from result (may be null if no link is found)
//                    if (pendingDynamicLinkData != null) {
//                        pendingDynamicLinkData.getLink();
//                    }
//                })
//                .addOnFailureListener(this, e -> Log.w(LOG_TAG, "getDynamicLink:onFailure", e));
        
        WebView.setWebContentsDebuggingEnabled(true);
        sharedPref = context.getSharedPreferences(this.getString(R.string.preference_file_key), Context.MODE_PRIVATE);

        if (MERCHANT_TYPE.equals("DRIVER")) {
            widgetService = new Intent(this, WidgetService.class);
            getWindow().addFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON);
            Utils.updateLocaleResource(sharedPref.getString(getResources().getString(R.string.LANGUAGE_KEY), "null"),context);
        } else {
            LottieAnimationView splashLottieView = findViewById(R.id.splash_lottie);
            try {
                if (Settings.Global.getFloat(getContentResolver(), Settings.Global.ANIMATOR_DURATION_SCALE) == 0f) {
                    isSystemAnimEnabled = false;
                } else {
                    splashLottieView.addAnimatorListener(new Animator.AnimatorListener() {
                        @Override
                        public void onAnimationEnd(Animator animation) {
                            if (isHideSplashEventCalled) hideSplash();
                                else splashLottieView.playAnimation();
                        }

                        @Override
                        public void onAnimationCancel(Animator animation) {}
                        @Override
                        public void onAnimationRepeat(Animator animation) {}
                        @Override
                        public void onAnimationStart(Animator animation) {}
                    });
                }
            } catch (Settings.SettingNotFoundException e) {
                isSystemAnimEnabled = false;
            }
        }

        updateConfigURL();
        initApp(viewParam, deepLinkJson);

        mFirebaseAnalytics.logEvent(isMigrated ?"migrate_local_store_success" : "migrate_local_store_failed",new Bundle());
        CleverTapAPI cleverTap = CleverTapAPI.getDefaultInstance(context);
        CleverTapAPI.createNotificationChannel(context,clientId,clientId,"notification",NotificationManager.IMPORTANCE_MAX,true);
        CleverTapAPI.setDebugLevel(CleverTapAPI.LogLevel.VERBOSE);
        cleverTap.enableDeviceNetworkInfoReporting(true);
        CleverTapAPI.setNotificationHandler((NotificationHandler)new PushTemplateNotificationHandler());


        sharedPref.edit().putString("DEVICE_DETAILS", getDeviceDetails()).apply();
        sharedPref.registerOnSharedPreferenceChangeListener(mListener);
        sharedPref.edit().putString(getResources().getString(in.juspay.mobility.app.R.string.ACTIVITY_STATUS), "onCreate").apply();

        try {
            MapsInitializer.initialize(getApplicationContext());
        } catch (Exception e) {
            e.printStackTrace();
        }
        registerCallBack();
        checkUpdate();
        in.juspay.mobility.app.Utils.startUpdate(context, activity);
        inAppNotification = new InAppNotification(this);
        initNotificationChannel();
        if (BuildConfig.DEBUG) {
            FirebaseMessaging.getInstance().subscribeToTopic("test");
        }
        Window window = this.getWindow();
        window.addFlags(WindowManager.LayoutParams.FLAG_DRAWS_SYSTEM_BAR_BACKGROUNDS);
        window.clearFlags(WindowManager.LayoutParams.FLAG_TRANSLUCENT_STATUS);
        window.setStatusBarColor(this.getResources().getColor(R.color.colorPrimaryDark, getTheme()));
        countAppUsageDays();
    }

    private void registerCallBack() {
        inappCallBack = this::showInAppNotification;
        ChatService.registerInAppCallback(inappCallBack);
        bundleUpdateCallBack = this::showAlertForUpdate;
        MyFirebaseMessagingService.registerBundleUpdateCallback(bundleUpdateCallBack);
        MyFirebaseMessagingService.registerShowNotificationCallBack(inappCallBack);
    }
    public void checkUpdate(){

        SharedPreferences sharedPref = this.getSharedPreferences(
                activity.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        int updateType = sharedPref.getString(getString(R.string.UPDATE_TYPE), "IMMEDIATE").equals("IMMEDIATE")
                ? AppUpdateType.IMMEDIATE : AppUpdateType.FLEXIBLE;
        AppUpdateManager appUpdateManager = AppUpdateManagerFactory.create(this);
        appUpdateManager.getAppUpdateInfo().addOnSuccessListener(appUpdateInfo -> {
            appUpdateInfo = appUpdateInfo;
            if (appUpdateInfo.updateAvailability() == UpdateAvailability.UPDATE_AVAILABLE
                    && appUpdateInfo.isUpdateTypeAllowed(updateType)) {
                in.juspay.mobility.app.Utils.isUpdateAvailable = true;
            }

        });
    }



    private void initNotificationChannel() {
        NotificationUtils.createNotificationChannel(this, NotificationUtils.CHANNEL_ID);
        NotificationUtils.createNotificationChannel(this, NotificationUtils.FLOATING_NOTIFICATION);
        NotificationUtils.createNotificationChannel(this, NotificationUtils.RINGING_CHANNEL_ID);
        NotificationUtils.createNotificationChannel(this, NotificationUtils.TRIP_CHANNEL_ID);
        NotificationUtils.createNotificationChannel(this, NotificationUtils.CANCELLED_PRODUCT);
        NotificationUtils.createNotificationChannel(this, NotificationUtils.DRIVER_HAS_REACHED);
    }

    public void updateConfigURL() {
        String key = MERCHANT_TYPE;
        String merchantId = key.equals("USER") ? in.juspay.mobility.BuildConfig.MERCHANT_ID_USER : in.juspay.mobility.BuildConfig.MERCHANT_ID_DRIVER;
        String baseUrl = key.equals("USER") ? in.juspay.mobility.BuildConfig.CONFIG_URL_USER : in.juspay.mobility.BuildConfig.CONFIG_URL_DRIVER;
        SharedPreferences sharedPreff = getApplicationContext().getSharedPreferences(
                activity.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        SharedPreferences.Editor editor = sharedPreff.edit();
        editor.putString("MERCHANT_ID", merchantId);
        editor.putString("BASE_URL", baseUrl);
        editor.apply();
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, @Nullable Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
        hyperServices.onActivityResult(requestCode, resultCode, data);
        if (requestCode == in.juspay.mobility.app.Utils.REQUEST_CODE_UPDATE_APP) {
                if (resultCode != RESULT_OK) {
                    Log.i(LOG_TAG,"Update flow failed! Result code: " + resultCode);
                    if(updateType == AppUpdateType.IMMEDIATE){
                        finishAndRemoveTask();
                    }
                }
            }
    }

    private void initApp(String viewParam, String deepLinkJSON) {

        hyperServices = new HyperServices(this, findViewById(R.id.cl_dui_container));
        final JSONObject json = new JSONObject();
        JSONObject payload = new JSONObject();

        try {

            json.put("requestId", UUID.randomUUID());
            json.put("service", getService());
            json.put("betaAssets", false);
            payload = getInnerPayload("initiate");
            if (viewParam != null) payload.put("viewParam", viewParam);
            if (deepLinkJSON != null) payload.put("deepLinkJSON", deepLinkJSON);
            json.put(PaymentConstants.PAYLOAD, payload);
        } catch (JSONException e) {
            e.printStackTrace();
        }
        hyperServices.initiate(json, new HyperPaymentsCallbackAdapter() {
            @Override
            public void onEvent(JSONObject jsonObject, JuspayResponseHandler juspayResponseHandler) {
                Log.d(LOG_TAG, "onEvent: " + jsonObject.toString());
                String event = jsonObject.optString("event");
                switch (event) {
                    case "initiate_result":
                        try {
                            JSONObject innerPayload = json.getJSONObject(PaymentConstants.PAYLOAD);
                            innerPayload.put("action", "process");
                            if (getIntent().hasExtra("NOTIFICATION_DATA") || (getIntent().hasExtra("notification_type") && getIntent().hasExtra("entity_ids") && getIntent().hasExtra("entity_type"))) {
                                innerPayload.put("notificationData", getNotificationDataFromIntent());
                            }
                            json.put(PaymentConstants.PAYLOAD, innerPayload);
                        } catch (JSONException e) {
                            Log.e(LOG_TAG, e.toString());
                        }
                        hyperServices.process(json);
                        break;
                    case "hide_loader":
                    case "hide_splash":
                        String key = getResources().getString(R.string.service);
                        if (key.equals("nammayatri") && isSystemAnimEnabled) {
                            isHideSplashEventCalled = true;
                        } else {
                            hideSplash();
                        }
                        break;
                    case "show_splash":
                        View v = findViewById(R.id.splash);
                        if (v != null) {
                            findViewById(R.id.splash).setVisibility(View.VISIBLE);
                        }
                        break;
                    case "reboot":
                        Log.i(LOG_TAG, "event reboot");
                        hyperServices.terminate();
                        hyperServices = null;
                        initApp(null,null);
                        break;
                    case "in_app_notification":
                        String title = jsonObject.optString("title");
                        String message = jsonObject.optString("message");
                        String channelId = jsonObject.optString("channelId");
                        String action1Text = jsonObject.optString("action1Text");
                        String action2Text = jsonObject.optString("action2Text");
                        String action1Image = jsonObject.optString("action1Image");
                        String action2Image = jsonObject.optString("action2Image");
                        String onTapAction = jsonObject.optString("onTapAction");
                        int durationInMilliSeconds = Integer.parseInt(jsonObject.optString("durationInMilliSeconds"));
                        showInAppNotification(title, message, onTapAction, action1Text, action2Text, action1Image, action2Image, channelId, durationInMilliSeconds, context);
                        break;
                    case "process_result":
                        try {
                            JSONObject innerPayload = jsonObject.getJSONObject(PaymentConstants.PAYLOAD);
                            if (innerPayload.getString("action").equals("terminate")) {
                                minimizeApp(context);
                            }
                        } catch (Exception ignored) {
                        } break;
                    case "log_stream":
                        JSONObject payload;
                        try {
                            payload = jsonObject.getJSONObject("payload");
                            HashMap<String, String> params = new HashMap<>();
                            switch (payload.optString("label")) {
                                case "current_screen":
                                    params.put("screen_name", payload.getJSONObject("value").getString("screen_name"));
                                    in.juspay.mobility.app.Utils.logEventWithParams("ny_driver_payment_current_screen", params ,context);
                                    break;
                                case "button_clicked":
                                    params.put("button_name",payload.getJSONObject("value").getString("button_name"));
                                    in.juspay.mobility.app.Utils.logEventWithParams("ny_driver_payment_button_clicked",params ,context);
                                    break;
                                case "upi_apps":
                                    params.put("app_name",payload.getJSONObject("value").getString("appName"));
                                    params.put("package_name",payload.getJSONObject("value").getString("packageName"));
                                    in.juspay.mobility.app.Utils.logEventWithParams("ny_driver_payment_upi_app_selected",params ,context);
                                    break;
                                default:
                            }
                        } catch (JSONException e) {
                            Log.e(LOG_TAG, "empty payload" + json);
                        }
                    default:
                        Log.e(LOG_TAG, "json_payload" + json);
                }
            }
        });
    }

    public void showAlertForUpdate() {
        System.out.println("inside showAlertForUpdate");
        AlertDialog.Builder builder = new AlertDialog.Builder(MainActivity.this);
        builder.setCancelable(false);
        ConstraintLayout constraintLayout = (ConstraintLayout) getLayoutInflater().inflate(in.juspay.mobility.app.R.layout.dynamic_update_loader, null);
        CardView cardView = constraintLayout.findViewById(in.juspay.mobility.app.R.id.apiLoaderOverlayCard);
        cardView.setCardElevation(0);
        cardView.setRadius(0);

        ViewGroup.LayoutParams layoutParams = new ConstraintLayout.LayoutParams(ConstraintLayout.LayoutParams.MATCH_PARENT, ConstraintLayout.LayoutParams.WRAP_CONTENT);
        constraintLayout.setLayoutParams(layoutParams);
        builder.setView(constraintLayout);
        builder.setPositiveButton(in.juspay.mobility.app.R.string.okay_got_it, (dialog, which) -> {
            dialog.cancel();
            hyperServices.terminate();
            hyperServices = null;
            initApp(null, null);
        });
        runOnUiThread(() -> {
            AlertDialog alertDialog = builder.create();
            alertDialog.show();
        });
    }


    protected Vector<String> handleDeepLinkIfAvailable(Intent appLinkIntent){
        if(appLinkIntent==null) return null;
        Vector<String> res = new Vector<>();
        Uri appLinkData = appLinkIntent.getData();
        String deepLinkJson = null, viewParam = null;
        if (appLinkData != null && appLinkData.getQuery() != null) {
            String query = appLinkData.getQuery();
            HashMap<String, String> query_params = getQueryMap(query);
            for (String key : query_params.keySet()) {
                if (key.equals("vp")){
                    viewParam = query_params.get(key);
                    break;
                }
            }
            Gson gson = new Gson();
            deepLinkJson = gson.toJson(query_params);
        } else return null;
        if(viewParam==null || deepLinkJson == null) return null;
        res.add(viewParam);
        res.add(deepLinkJson);
        return res;
    }

    @Override
    protected void onNewIntent(Intent intent) {
        Vector<String> res = handleDeepLinkIfAvailable(intent);
        String viewParam = null, deepLinkJson =null;
        if (res!=null ){
            viewParam = res.get(0);
            deepLinkJson = res.get(1);
        }
        JSONObject proccessPayloadDL = new JSONObject();
        try {
            JSONObject innerPayloadDL = getInnerPayload("process");
            if (viewParam != null && deepLinkJson != null) {
                innerPayloadDL.put("viewParamNewIntent", viewParam)
                        .put("deepLinkJSON", deepLinkJson);
                proccessPayloadDL.put("service", getService())
                        .put("merchantId", getResources().getString(R.string.merchant_id))
                        .put("requestId", UUID.randomUUID())
                        .put(PaymentConstants.PAYLOAD, innerPayloadDL);
                {hyperServices.process(proccessPayloadDL);}
            }
        }catch (Exception e){
            // Need to handle exception
        }
        if (intent != null && intent.hasExtra("NOTIFICATION_DATA")) {
            try {
                String data = intent.getExtras().getString("NOTIFICATION_DATA");
                JSONObject proccessPayload = new JSONObject().put("service", getService())
                        .put("requestId", UUID.randomUUID());
                JSONObject innerPayload = new JSONObject();
                JSONObject jsonData = new JSONObject(data);
                if (jsonData.has("notification_type") && jsonData.getString("notification_type").equals("CHAT_MESSAGE")) {
                    innerPayload = getInnerPayload("OpenChatScreen");
                    NotificationManager notificationManager = (NotificationManager) getSystemService(Context.NOTIFICATION_SERVICE);
                    notificationManager.cancel(NotificationUtils.chatNotificationId);
                    innerPayload.put("notification_type", "CHAT_MESSAGE");
                }
                if (jsonData.has("notification_type") && jsonData.has("entity_ids")) {
                    String id = jsonData.getString("entity_ids");
                    String type = jsonData.getString("notification_type");
                    if (type.equals("NEW_MESSAGE")) {
                        innerPayload = getInnerPayload("callDriverAlert");
                        innerPayload.put("id", id)
                                .put("popType", type);
                    }
                }
                proccessPayload.put(PaymentConstants.PAYLOAD, innerPayload);
                {hyperServices.process(proccessPayload);}
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        super.onNewIntent(intent);
    }

    @Override
    protected void onResume() {
        super.onResume();
        AppUpdateManager appUpdateManager = AppUpdateManagerFactory.create(this);
        if (sharedPref != null) {
            sharedPref.edit().putString(getResources().getString(in.juspay.mobility.app.R.string.ACTIVITY_STATUS), "onResume").apply();
            sharedPref.edit().putString("MAPS_OPENED", "null").apply();
        }
        if (appUpdateManager != null){
            appUpdateManager.getAppUpdateInfo().addOnSuccessListener(appUpdateInfo -> {
                if (appUpdateInfo.updateAvailability() == UpdateAvailability.DEVELOPER_TRIGGERED_UPDATE_IN_PROGRESS) {
                    // If an in-app update is already running, resume the update.
                    try {
                        appUpdateManager.startUpdateFlowForResult(
                                appUpdateInfo,
                                AppUpdateType.IMMEDIATE,
                                this,
                                in.juspay.mobility.app.Utils.REQUEST_CODE_UPDATE_APP
                        );
                    } catch (IntentSender.SendIntentException e) {
                        e.printStackTrace();
                    }
                }
            });
        }
        if (MERCHANT_TYPE.equals("DRIVER")) {
            if (NotificationUtils.overlayFeatureNotAvailable(this)) {
                checkRideRequest();
            }
            if (widgetService != null) {
                stopService(widgetService);
            }
        }
    }

    @Override
    protected void onPause() {
        super.onPause();
        if (sharedPref != null)
            sharedPref.edit().putString(getResources().getString(in.juspay.mobility.app.R.string.ACTIVITY_STATUS), "onPause").apply();
        if (BuildConfig.MERCHANT_TYPE.equals("DRIVER") &&
                widgetService != null && Settings.canDrawOverlays(this) &&
                !sharedPref.getString(getResources().getString(in.juspay.mobility.app.R.string.REGISTERATION_TOKEN), "null").equals("null") &&
                !sharedPref.getString("DISABLE_WIDGET", "true").equals("true")) {
            widgetService.putExtra("payload", "{}");
            widgetService.putExtra("data", "{}");
            startService(widgetService);
        }
    }

    @Override
    protected void onDestroy() {
        if (sharedPref != null) {
            sharedPref.edit().putString(getResources().getString(in.juspay.mobility.app.R.string.ACTIVITY_STATUS), "onDestroy").apply();
            String role = sharedPref.getString("ROLE_KEY", "null");
            String location_status = sharedPref.getString("LOCATION_STATUS", "PAUSE");
            System.out.println("Outside onDestroy Driver" + role);
            if (role.equals("DRIVER") && location_status.equals("START") && (ActivityCompat.checkSelfPermission(getApplicationContext(), Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED && ActivityCompat.checkSelfPermission(getApplicationContext(), Manifest.permission.ACCESS_COARSE_LOCATION) == PackageManager.PERMISSION_GRANTED)) {
                Intent broadcastIntent = new Intent();
                broadcastIntent.setAction("restartservice");
                broadcastIntent.setClass(this, BootUpReceiver.class);
                this.sendBroadcast(broadcastIntent);
            }
        }
        if (hyperServices != null) {
            hyperServices.terminate();
        }
        ChatService.deRegisterInAppCallback(inappCallBack);
        MyFirebaseMessagingService.deRegisterBundleUpdateCallback(bundleUpdateCallBack);
        MyFirebaseMessagingService.deRegisterShowNotificationCallBack(inappCallBack);
        inAppNotification = null;
        super.onDestroy();
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults);
        hyperServices.onRequestPermissionsResult(requestCode, permissions, grantResults);
    }

    public void hideSplash() {
        View v = findViewById(in.juspay.mobility.app.R.id.cl_dui_container);
        if (v != null) {
            findViewById(in.juspay.mobility.app.R.id.cl_dui_container).setVisibility(View.VISIBLE);
        }
        View splashView = findViewById(R.id.splash);
        if (splashView != null) {
            splashView.setVisibility(View.GONE);
        }
    }

    private void countAppUsageDays() {
        Date currentDate = new Date();
        SharedPreferences sharedPref = getApplicationContext().getSharedPreferences(this.getString(in.juspay.mobility.app.R.string.preference_file_key), Context.MODE_PRIVATE);
        long millis = sharedPref.getLong("PREVIOUS_USED_DATE", 0L);
        if (millis == 0L) {
            sharedPref.edit().putLong("PREVIOUS_USED_DATE", currentDate.getTime()).apply();
        }
        Date previousDate = new Date(sharedPref.getLong("PREVIOUS_USED_DATE", 0L));
        if (TimeUnit.MILLISECONDS.toDays(currentDate.getTime() - previousDate.getTime()) > 0) {
            // update days Count
            sharedPref.edit().putInt("DAYS_COUNT", sharedPref.getInt("DAYS_COUNT", 0) + 1).apply();
            sharedPref.edit().putString("USED_DAYS_COUNT", String.valueOf(sharedPref.getInt("DAYS_COUNT", 0))).apply();
            // update previousDate to currentDate
            sharedPref.edit().putLong("PREVIOUS_USED_DATE", currentDate.getTime()).apply();
        }
    }

    public void showInAppNotification(String title, String message, String onTapAction, String action1Text, String action2Text, String action1Image, String action2Image, String channelId, int durationInMilliSeconds, Context context) {
        try {
            Handler handler = new Handler(context.getMainLooper());
            handler.postDelayed(() -> {
                try {
                    if (inAppNotification != null){
                        inAppNotification.generateNotification(title, message, onTapAction, action1Text, action2Text, action1Image, action2Image, channelId, durationInMilliSeconds);
                    }
                } catch (JSONException e) {
                    Log.e(LOG_TAG, "Error in In App Notification Handler " + e);
                }
            }, 0);
        } catch (Exception e) {
            Log.e(LOG_TAG, "Error in In App Notification " + e);
        }
    }

    private void checkRideRequest() {
        try {
            boolean rideReqExpired = NotificationUtils.lastRideReq.getBoolean("rideReqExpired", true);
            if (rideReqExpired) return;
            Intent rideReqActivity = new Intent(this, RideRequestActivity.class);
            rideReqActivity.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_REORDER_TO_FRONT);
            rideReqActivity.putExtras(NotificationUtils.lastRideReq);
            startActivity(rideReqActivity);
        } catch (Exception e) {
            Log.e(LOG_TAG, "Exception in checkRideRequest");
        }
    }

    private JSONObject getNotificationDataFromIntent() throws JSONException {
        Bundle bundle = getIntent().getExtras();
        JSONObject data;
        //Handling local and foreground notifications
        if (getIntent().hasExtra("NOTIFICATION_DATA")) {
            data = new JSONObject(bundle.getString("NOTIFICATION_DATA"));
        }
        //Handling background notifications
        else if (getIntent().hasExtra("notification_type") && getIntent().hasExtra("entity_ids") && getIntent().hasExtra("entity_type")) {
            data = new JSONObject();
            data.put("notification_type", bundle.getString("notification_type"));
            data.put("entity_ids", bundle.getString("entity_ids"));
            data.put("entity_type", bundle.getString("entity_type"));
        } else {
            data = new JSONObject();
        }
        return data;
    }
    private class GetGAIDTask extends AsyncTask<String, Integer, String> {
        @Override
        protected String doInBackground(String... strings) {
            AdvertisingIdClient.Info adInfo;
            adInfo = null;
            try {
                if(GoogleApiAvailability.getInstance().isGooglePlayServicesAvailable(MainActivity.this.getApplicationContext()) != ConnectionResult.SUCCESS) {
                    return "google play service not available";
                }
                adInfo = AdvertisingIdClient.getAdvertisingIdInfo(MainActivity.this.getApplicationContext());
                if (adInfo.isLimitAdTrackingEnabled()) // check if user has opted out of tracking
                    return "did not found GAID... sorry";
            } catch (IOException | GooglePlayServicesRepairableException |
                     GooglePlayServicesNotAvailableException e) {
                e.printStackTrace();
            }
            return adInfo != null ? adInfo.getId() : "did not found GAID... sorry";
        }
        @Override
        protected void onPostExecute(String s) {
            GAID = s;
            System.out.println("GAID "+GAID);
            Bundle params = new Bundle();
            params.putString("id",GAID);
            FirebaseAnalytics.getInstance(context).logEvent("ad_id", params);
        }
    }

    public String getService() {
        if (MERCHANT_TYPE.equals("USER")) {
            return "in.yatri.consumer";
        } else {
            return "in.yatri.provider";
        }
    }

    private boolean migrateLocalStore(Context context) {
        SharedPreferences oldSharedPref = context.getSharedPreferences("namma_yatri_app_local_keys",MODE_PRIVATE);
        SharedPreferences currentSharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key),MODE_PRIVATE);
        Map<String,?> oldEntries = oldSharedPref.getAll();
        for (Map.Entry<String, ?> entry : oldEntries.entrySet()) {
            Object current = entry.getValue();
            if (current instanceof Integer) {
                currentSharedPref.edit().putInt(entry.getKey(),(int)current).apply();
            } else if (current instanceof String) {
                currentSharedPref.edit().putString(entry.getKey(),(String) current).apply();
            }else if (current instanceof Float) {
                currentSharedPref.edit().putFloat(entry.getKey(),(float) current).apply();
            }else if (current instanceof Long) {
                currentSharedPref.edit().putLong(entry.getKey(),(long) current).apply();
            }else if (current instanceof Boolean) {
                currentSharedPref.edit().putBoolean(entry.getKey(),(boolean) current).apply();
            }
        }
        oldSharedPref.edit().clear().apply();
        return true;
    }

    private JSONObject getInnerPayload(String action) throws JSONException{
        JSONObject payload = new JSONObject();
        payload.put("clientId", getResources().getString(R.string.client_id));
        payload.put("merchantId", getResources().getString(R.string.merchant_id));
        payload.put("action", action);
        payload.put("logLevel",1);
        payload.put(PaymentConstants.ENV, "prod");
        return payload;
    }
}