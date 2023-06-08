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
import android.app.AlertDialog;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentSender;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.media.Ringtone;
import android.media.RingtoneManager;
import android.net.Uri;
import android.os.Bundle;
import android.os.Handler;
import android.provider.Settings;
import android.system.Os;
import android.util.Base64;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.view.WindowManager;
import android.view.animation.AnimationUtils;
import android.webkit.WebView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.cardview.widget.CardView;
import androidx.constraintlayout.widget.ConstraintLayout;
import androidx.core.app.ActivityCompat;
import androidx.work.WorkManager;

import com.airbnb.lottie.LottieAnimationView;
import com.google.android.gms.maps.MapsInitializer;
import com.google.android.gms.tasks.Task;
import com.google.android.play.core.appupdate.AppUpdateInfo;
import com.google.android.play.core.appupdate.AppUpdateManager;
import com.google.android.play.core.appupdate.AppUpdateManagerFactory;
import com.google.android.play.core.install.model.AppUpdateType;
import com.google.android.play.core.install.model.UpdateAvailability;
import com.google.firebase.analytics.FirebaseAnalytics;
import com.google.firebase.dynamiclinks.FirebaseDynamicLinks;
import com.google.firebase.messaging.FirebaseMessaging;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.Date;
import java.util.Timer;
import java.util.TimerTask;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

import in.juspay.hypersdk.core.PaymentConstants;
import in.juspay.hypersdk.data.JuspayResponseHandler;
import in.juspay.hypersdk.ui.HyperPaymentsCallbackAdapter;
import in.juspay.mobility.app.BootUpReceiver;
import in.juspay.mobility.app.LocationUpdateService;
import in.juspay.mobility.app.MyFirebaseMessagingService;
import in.juspay.mobility.app.NotificationUtils;
import in.juspay.mobility.app.RideRequestActivity;
import in.juspay.mobility.app.Utils;
import in.juspay.mobility.app.WidgetService;
import in.juspay.services.HyperServices;


public class MainActivity extends AppCompatActivity {

    private static final String LOG_TAG = "MAIN_ACTIVITY";
    private static final int REQUEST_CODE_UPDATE_APP = 587;
    private static int updateType;
    MyFirebaseMessagingService.BundleUpdateCallBack bundleUpdateCallBack;
    private HyperServices hyperServices;
    private Context context;
    private Activity activity;
    @Nullable
    private SharedPreferences sharedPref;
    SharedPreferences.OnSharedPreferenceChangeListener mListener = new SharedPreferences.OnSharedPreferenceChangeListener() {
        @Override
        public void onSharedPreferenceChanged(SharedPreferences sharedPreferences, String key) {
            if (key != null && key.equals("REGISTERATION_TOKEN")) {
                String token = sharedPref.getString("REGISTERATION_TOKEN", "null");
                if (token.equals("__failed")) {
                    final PackageManager pm = getApplicationContext().getPackageManager();
                    final Intent intent = pm.getLaunchIntentForPackage(getApplicationContext().getPackageName());
                    try {
                        if (activity != null) {
                            activity.finishAffinity();// Finishes all activities.
                            activity.startActivity(intent);
                        } else {
                            sharedPref.edit().clear().apply();
                        }
                    } catch (NullPointerException e) {
                        e.printStackTrace();
                    }
                }
            }
            // Update Driver status in Local Storage
            if (key != null && key.equals("DRIVER_STATUS")) {
                String status = sharedPref.getString("DRIVER_STATUS", "null");
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
            if (key != null && sharedPref.getString("DRIVER_STATUS", "null").equals("true") && (key.equals("RIDE_G_FREQUENCY") || key.equals("MAX_LIMIT_TO_STORE_LOCATION_PT") || key.equals("NO_OF_LOCATION_PT_TO_REMOVE") || key.equals("DRIVER_MIN_DISPLACEMENT") || key.equals("RIDE_T_FREQUENCY"))) {
                System.out.println("TRIGGERED UPDATE POLLING");
                Context context = getApplicationContext();
                Intent locationUpdateIntent = new Intent(context, LocationUpdateService.class);
                context.startService(locationUpdateIntent);
            }
        }
    };
    private Intent widgetService;
    private AppUpdateManager appUpdateManager;
    private boolean isHideSplashEventCalled = false;
    private boolean isSystemAnimEnabled = true;

    @Override
    public void onBackPressed() {
        if (hyperServices != null && !hyperServices.onBackPressed()) {
            super.onBackPressed();
        }
    }

    public String getDeviceRAM()
    {
        String deviceRAM = sharedPref.getString("DEVICE_RAM", "__failed");
        if(deviceRAM != "__failed")
            return deviceRAM;
        long memory=0;
        try {
            ActivityManager activityManager = (ActivityManager) getApplicationContext().getSystemService(Context.ACTIVITY_SERVICE);
            ActivityManager.MemoryInfo memInfo = new ActivityManager.MemoryInfo();
            activityManager.getMemoryInfo(memInfo);
            memory = 1 + memInfo.totalMem / (1024 * 1024 * 1024);
            deviceRAM = memory == 0 ? "null" : memory+" GB" ;
            sharedPref.edit().putString("DEVICE_RAM", deviceRAM).apply();
        } catch(Exception e){
            System.out.println("In getDeviceRAM error: ");
            e.printStackTrace();
        }
        return deviceRAM;
    }
    public String[] getScreenDimensions()
    {
        String[] res= {sharedPref.getString("DEVICE_RESOLUTION", "__failed"),sharedPref.getString("DEVICE_SIZE", "__failed")};
        if(res[0] != "__failed" && res[1] != "__failed")
            return res;
        int height = 0;
        int width  = 0;
        float size = 0;
        try {
            DisplayMetrics displayMetrics = new DisplayMetrics();
            getWindowManager().getDefaultDisplay().getRealMetrics(displayMetrics);
            height = displayMetrics.heightPixels;
            width = displayMetrics.widthPixels;
            float x = height / displayMetrics.ydpi;
            float y = width / displayMetrics.xdpi;
            size = (float) Math.sqrt(x * x + y * y);
            size = Math.round(size * 100) / 100;
            res[0] = height != 0 && width != 0 ? height+"x"+width+"px" : "null" ;
            res[1] = size!=0 ? size + " Inches" : "null" ;
            sharedPref.edit().putString("DEVICE_RESOLUTION", res[0]).apply();
            sharedPref.edit().putString("DEVICE_SIZE", res[1]).apply();
        }catch(Exception e){
            System.out.println("In getScreenDimensions error: ");
            e.printStackTrace();
        }
        return res;
    }

    public String getDeviceDetails()
    {
        String deviceDetails = "";
        try {
            String bVersion = Build.VERSION.RELEASE;
            String bModel = Build.MODEL;
            String bBrand = Build.BRAND;
            String[] dim = getScreenDimensions();
            String deviceRAM = getDeviceRAM();
            if(bModel == null || bModel == "")
                bModel="null";
            if(bBrand == null || bBrand == "")
                bBrand="null";
            bVersion = bVersion == null || bVersion == "" ? "null" : "Android v"+bVersion ;
            deviceDetails = bBrand+"/" + bModel+"/" + bVersion+"/" + deviceRAM + "/" + dim[1]+"/" + dim[0];
        } catch (Exception e) {
            e.printStackTrace();
        }
        return deviceDetails;
    }

    @SuppressLint("SetJavaScriptEnabled")
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        FirebaseAnalytics mFirebaseAnalytics = FirebaseAnalytics.getInstance(this);
        context = getApplicationContext();
        try {
            MapsInitializer.initialize(getApplicationContext());
        } catch (Exception e) {
            e.printStackTrace();
        }
        activity = this;
        sharedPref = getSharedPreferences(this.getString(in.juspay.mobility.app.R.string.preference_file_key), Context.MODE_PRIVATE);
        sharedPref.edit().putString("DEVICE_DETAILS", getDeviceDetails()).apply();
        sharedPref.registerOnSharedPreferenceChangeListener(mListener);
        sharedPref.edit().putString(getResources().getString(in.juspay.mobility.app.R.string.ACTIVITY_STATUS), "onCreate").apply();
        @SuppressLint("HardwareIds") String androidId = Settings.Secure.getString(getContentResolver(), Settings.Secure.ANDROID_ID);
        Bundle params = new Bundle();
        params.putString("id", androidId);
        mFirebaseAnalytics.logEvent("device_id", params);
        widgetService = new Intent(this, WidgetService.class);
        FirebaseDynamicLinks.getInstance()
                .getDynamicLink(getIntent())
                .addOnSuccessListener(this, pendingDynamicLinkData -> {
                    // Get deep link from result (may be null if no link is found)
                    if (pendingDynamicLinkData != null) {
                        pendingDynamicLinkData.getLink();
                    }
                })
                .addOnFailureListener(this, e -> Log.w(LOG_TAG, "getDynamicLink:onFailure", e));


        WebView.setWebContentsDebuggingEnabled(true);
        setContentView(R.layout.activity_main);
        if (MERCHANT_TYPE.equals("DRIVER")) {
            getWindow().addFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON);
            new Utils(context).updateLocaleResource(sharedPref.getString(getResources().getString(in.juspay.mobility.app.R.string.LANGUAGE_KEY), "null"));
        } else {
            LottieAnimationView splashLottieView = findViewById(in.juspay.mobility.app.R.id.splash_lottie);
            try {
                if (Settings.Global.getFloat(getContentResolver(), Settings.Global.ANIMATOR_DURATION_SCALE) == 0f) {
                    isSystemAnimEnabled = false;
                } else {
                    splashLottieView.addAnimatorListener(new Animator.AnimatorListener() {
                        @Override
                        public void onAnimationStart(Animator animation) {

                        }

                        @Override
                        public void onAnimationEnd(Animator animation) {
                            if (isHideSplashEventCalled) {
                                hideSplash();
                            } else {
                                splashLottieView.playAnimation();
                            }
                        }

                        @Override
                        public void onAnimationCancel(Animator animation) {
                        }

                        @Override
                        public void onAnimationRepeat(Animator animation) {

                        }
                    });
                }
            } catch (Settings.SettingNotFoundException e) {
                isSystemAnimEnabled = false;
            }
        }
        bundleUpdateCallBack = this::showAlertForUpdate;
        MyFirebaseMessagingService.registerBundleUpdateCallback(bundleUpdateCallBack);
        appUpdateManager = AppUpdateManagerFactory.create(this);
        // Returns an intent object that you use to check for an update.
        Task<AppUpdateInfo> appUpdateInfoTask = appUpdateManager.getAppUpdateInfo();
        updateType = AppUpdateType.IMMEDIATE;
        appUpdateInfoTask.addOnSuccessListener(appUpdateInfo -> {
            if (appUpdateInfo.updateAvailability() == UpdateAvailability.UPDATE_AVAILABLE
                    && appUpdateInfo.isUpdateTypeAllowed(updateType)) {
                Log.d(LOG_TAG, "Inside update");
                try {
                    appUpdateManager.startUpdateFlowForResult(
                            // Pass the intent that is returned by 'getAppUpdateInfo()'.
                            appUpdateInfo,
                            // Or 'AppUpdateType.FLEXIBLE' for flexible updates.
                            updateType,
                            // The current activity making the update request.
                            this,
                            // Include a request code to later monitor this update request.
                            getResources().getInteger(REQUEST_CODE_UPDATE_APP)
                    );
                } catch (IntentSender.SendIntentException e) {
                    e.printStackTrace();
                }
                Log.d(LOG_TAG, "Update available");
            } else {
                Log.d(LOG_TAG, "No Update available");
            }
        });
        updateConfigURL();
        initApp();
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
        SharedPreferences sharedPreff = this.getSharedPreferences(
                activity.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        SharedPreferences.Editor editor = sharedPreff.edit();
        editor.putString("MERCHANT_ID", merchantId);
        editor.putString("BASE_URL", baseUrl);
        editor.apply();
    }

    public void triggerPopUPMain(String id, String type) {

        try {
            Log.i(LOG_TAG, "Triggering the process");
            JSONObject payload = new JSONObject()
                    .put("service", getService())
                    .put("requestId", UUID.randomUUID());
            JSONObject innerPayload = new JSONObject()
                    .put("action", "showPopup")
                    .put("id", id)
                    .put("popType", type);
            payload.put("payload", innerPayload);
            hyperServices.process(payload);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void initApp() {

        hyperServices = new HyperServices(this, findViewById(in.juspay.mobility.app.R.id.cl_dui_container));
        final JSONObject json = new JSONObject();
        JSONObject payload = new JSONObject();

        try {

            json.put("requestId", UUID.randomUUID());
            json.put("service", getService());
            json.put("betaAssets", false);
            payload.put("clientId", getResources().getString(R.string.client_id));
            payload.put("action", "initiate");
            payload.put("service", getService());
            payload.put(PaymentConstants.ENV, "master");
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
                        if (getIntent().hasExtra("NOTIFICATION_DATA") || (getIntent().hasExtra("notification_type") && getIntent().hasExtra("entity_ids") && getIntent().hasExtra("entity_type"))) {
                            try {
                                JSONObject innerPayload = json.getJSONObject(PaymentConstants.PAYLOAD);
                                innerPayload.put("action", "process");
                                json.put(PaymentConstants.PAYLOAD, innerPayload);
                            } catch (JSONException e) {
                                Log.e(LOG_TAG, e.toString());
                            }
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
                        View v = findViewById(in.juspay.mobility.app.R.id.splash);
                        if (v != null) {
                            findViewById(in.juspay.mobility.app.R.id.splash).setVisibility(View.VISIBLE);
                        }
                        break;
                    case "reboot":
                        Log.i(LOG_TAG, "event reboot");
                        hyperServices.terminate();
                        hyperServices = null;
                        initApp();
                        break;
                    case "in_app_notification":
                        String title = jsonObject.optString("title");
                        String message = jsonObject.optString("message");
                        String channelId = jsonObject.optString("channelId");
                        String action1Text = jsonObject.optString("action1Text") ;
                        String action2Text = jsonObject.optString("action2Text");
                        String action1Image = jsonObject.optString("action1Image") ;
                        String action2Image = jsonObject.optString("action2Image");
                        String onTapAction = jsonObject.optString("onTapAction");
                        int durationInMilliSeconds = Integer.parseInt(jsonObject.optString("durationInMilliSeconds"));
                        showInAppNotification(title, message, onTapAction, action1Text,action2Text , action1Image,action2Image , channelId , durationInMilliSeconds, context);
                        break;
                    case "process_result":
                        try {
                            JSONObject innerPayload = json.getJSONObject(PaymentConstants.PAYLOAD);
                            if (innerPayload.getString("action").equals("terminate")) {
                                minimizeApp(context);
                            }
                        } catch (Exception ignored) {
                        }
                    }
                    Log.e(LOG_TAG, "json_payload" + json);
                    hyperServices.process(json);
                } else if (event.equals("hide_loader") || event.equals("hide_splash")) {
                    String key = getResources().getString(R.string.service);
                    if (key.equals("nammayatri") && isSystemAnimEnabled) {
                        isHideSplashEventCalled = true;
                    } else {
                        hideSplash();
                    }
                } else if (event.equals("show_splash")) {
                    View v = findViewById(in.juspay.mobility.app.R.id.splash);
                    if (v != null) {
                        findViewById(in.juspay.mobility.app.R.id.splash).setVisibility(View.VISIBLE);
                    }
                } else if (event.equals("reboot")) {
                    Log.i(LOG_TAG, "event reboot");
                    hyperServices.terminate();
                    hyperServices = null;
                    initApp();
                } else if(jsonObject.optString("event").equals("in_app_notification")){
                    String title = jsonObject.optString("title");
                    String message = jsonObject.optString("message");
                    String channelId = jsonObject.optString("channelId");
                    String action1Text = jsonObject.optString("action1Text") ;
                    String action2Text = jsonObject.optString("action2Text");
                    String action1Image = jsonObject.optString("action1Image") ;
                    String action2Image = jsonObject.optString("action2Image");
                    String onTapAction = jsonObject.optString("onTapAction");
                    int durationInMilliSeconds = Integer.parseInt(jsonObject.optString("durationInMilliSeconds"));
                    showInAppNotification(title, message, onTapAction, action1Text,action2Text , action1Image,action2Image , channelId , durationInMilliSeconds, context);
                } else if (event.equals("in_app_notification")) {
                    showInAppNotifiation(jsonObject.optString("title"), jsonObject.optString("message"));
                } else if (event.equals("location_permission")) {
                    try {
                        JSONObject payload1 = json.getJSONObject(PaymentConstants.PAYLOAD);
                        payload1.put("action", "location_permission_result");
                        json.put(PaymentConstants.PAYLOAD, payload1);
                    } catch (Exception e) {
                        Log.e(LOG_TAG,"Exception in location_permission");
                    }
                    hyperServices.process(json);
                } else if (event.equals("process_result")) {
                    try {
                        JSONObject payload1 = json.getJSONObject(PaymentConstants.PAYLOAD);
                        if (payload1.getString("action").equals("terminate")) {
                            Intent startMain = new Intent(Intent.ACTION_MAIN);
                            startMain.addCategory(Intent.CATEGORY_HOME);
                            startMain.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                            context.startActivity(startMain);
                        }
                    } catch (Exception ignored) {
                    }
                }
            });
        }
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
            initApp();
        });
        runOnUiThread(() -> {
            AlertDialog alertDialog = builder.create();
            alertDialog.show();
        });
    }

    @Override
    protected void onNewIntent(Intent intent) {
        if (intent != null && intent.hasExtra("NOTIFICATION_DATA")) {
            String data = intent.getExtras().getString("NOTIFICATION_DATA");
            try {
                JSONObject jsonData = new JSONObject(data);
                if(jsonData.has("notification_type") && jsonData.getString("notification_type").equals("CHAT_MESSAGE")){
                    hyperServices.process(new JSONObject().put("service", "in.juspay." + getResources().getString(R.string.service)).put("requestId", UUID.randomUUID()).put("payload", new JSONObject().put("action", "OpenChatScreen").put("notification_type", "CHAT_MESSAGE")));
                }
                if (jsonData.has("notification_type") && jsonData.has("entity_ids")) {
                    String id = jsonData.getString("entity_ids");
                    String type = jsonData.getString("notification_type");
                    if (type.equals("NEW_MESSAGE")) {
                        hyperServices.process(new JSONObject().put("service", getService()).put("requestId", UUID.randomUUID()).put("payload", new JSONObject().put("action", "callDriverAlert").put("id", id).put("popType", type)));
                    }
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        super.onNewIntent(intent);
    }

    @Override
    protected void onResume() {
        super.onResume();
        if (sharedPref != null) {
            sharedPref.edit().putString(getResources().getString(in.juspay.mobility.app.R.string.ACTIVITY_STATUS), "onResume").apply();
            sharedPref.edit().putString("MAPS_OPENED", "null").apply();
        }
        appUpdateManager.getAppUpdateInfo().addOnSuccessListener(appUpdateInfo -> {
            if (appUpdateInfo.updateAvailability() == UpdateAvailability.DEVELOPER_TRIGGERED_UPDATE_IN_PROGRESS) {
                // If an in-app update is already running, resume the update.
                try {
                    appUpdateManager.startUpdateFlowForResult(
                            appUpdateInfo,
                            AppUpdateType.IMMEDIATE,
                            this,
                            getResources().getInteger(REQUEST_CODE_UPDATE_APP)
                    );
                } catch (IntentSender.SendIntentException e) {
                    e.printStackTrace();
                }
            }
        });
        if (getResources().getString(R.string.service).equals("DRIVER")) {
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
        if (getResources().getString(R.string.service).equals("nammayatripartner") && widgetService != null && Settings.canDrawOverlays(this) && !sharedPref.getString(getResources().getString(in.juspay.mobility.app.R.string.REGISTERATION_TOKEN), "null").equals("null")) {
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
        MyFirebaseMessagingService.deRegisterBundleUpdateCallback(bundleUpdateCallBack);
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
        View splashView = findViewById(in.juspay.mobility.app.R.id.splash);
        if (splashView != null) {
            splashView.setVisibility(View.GONE);
        }
    }

    private void countAppUsageDays() {
        Date currentDate = new Date();
        SharedPreferences sharedPref = this.getSharedPreferences(this.getString(in.juspay.mobility.app.R.string.preference_file_key), Context.MODE_PRIVATE);
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

public static void showInAppNotification(String title, String message, String onTapAction, String action1Text, String action2Text, String action1Image, String action2Image, String channelId, int durationInMilliSeconds, Context context) {
        // getting the main Layout as a Container to add the notification .
        ConstraintLayout mainLayout = findViewById(in.juspay.mobility.app.R.id.main_layout);

        // inflating the app_notification as view to append in main layout .
        @SuppressLint("InflateParams") View notification = getLayoutInflater().inflate(in.juspay.mobility.app.R.layout.app_notification, null);
        notification.setLayoutParams(new ConstraintLayout.LayoutParams(ConstraintLayout.LayoutParams.WRAP_CONTENT, ConstraintLayout.LayoutParams.WRAP_CONTENT));
        notification.bringToFront();


        // adding animation to the notification
        notification.startAnimation(AnimationUtils.loadAnimation(this, in.juspay.mobility.app.R.anim.top_to_bottom));

        // setting the title and description to the notification
        TextView Title = notification.findViewById(in.juspay.mobility.app.R.id.title);
        TextView Desc = notification.findViewById(in.juspay.mobility.app.R.id.desc);
        Title.setText(title);
        Desc.setText(desc);

        // adding the evenListener to the cross button
        notification.findViewById(in.juspay.mobility.app.R.id.cross).setOnClickListener(view -> {
            notification.startAnimation(AnimationUtils.loadAnimation(getApplicationContext(), in.juspay.mobility.app.R.anim.bottom_to_top));
            mainLayout.removeView(notification);
        });

        // ring the notification bell
        try {
            Handler handler = new Handler(context.getMainLooper());
            handler.postDelayed(() -> {
                try {
                    inAppNotification.generateNotification(title, message, onTapAction, action1Text, action2Text, action1Image, action2Image, channelId, durationInMilliSeconds);
                } catch (JSONException e) {
                    Log.e(TAG, "Error in In App Notification Handler " + e);
                }
            }, 0);
        } catch (Exception e) {
            Log.e(TAG, "Error in In App Notification " + e);
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

    static class NotificationListener extends BroadcastReceiver {
        @Override
        public void onReceive(Context context, Intent intent) {
            Log.e("In Main activity", context.toString());
        }
    }

    public String getService() {
        if (MERCHANT_TYPE.equals("USER")) {
            return "in.yatri.consumer";
        } else {
            return "in.yatri.provider";
        }
    }
}
