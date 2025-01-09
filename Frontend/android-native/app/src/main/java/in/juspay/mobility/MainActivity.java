/*
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility;

import static android.Manifest.permission.ACCESS_FINE_LOCATION;
import static in.juspay.mobility.BuildConfig.MERCHANT_TYPE;
import static in.juspay.mobility.Utils.getInnerPayload;
import static in.juspay.mobility.Utils.handleGlResp;
import static in.juspay.mobility.Utils.initCTSignedCall;
import static in.juspay.mobility.app.Utils.minimizeApp;
import static in.juspay.mobility.app.Utils.setCleverTapUserProp;
import static in.juspay.mobility.common.MobilityCommonBridge.isClassAvailable;

import android.Manifest;
import android.animation.ValueAnimator;
import android.annotation.SuppressLint;
import android.app.Activity;
import android.app.AlertDialog;
import android.app.NotificationChannelGroup;
import android.app.NotificationManager;
import android.content.Context;
import android.content.Intent;
import android.content.IntentSender;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.provider.Settings;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.view.WindowManager;
import android.webkit.WebView;

import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.cardview.widget.CardView;
import androidx.constraintlayout.widget.ConstraintLayout;
import androidx.core.app.ActivityCompat;
import androidx.fragment.app.FragmentActivity;
import androidx.work.WorkManager;

import com.airbnb.lottie.LottieAnimationView;
import com.clevertap.android.pushtemplates.PushTemplateNotificationHandler;
import com.clevertap.android.sdk.ActivityLifecycleCallback;
import com.clevertap.android.sdk.CleverTapAPI;
import com.clevertap.android.sdk.interfaces.NotificationHandler;
import com.clevertap.android.signedcall.fcm.SignedCallNotificationHandler;
import com.clevertap.android.signedcall.init.SignedCallAPI;
import com.clevertap.android.signedcall.interfaces.SCNetworkQualityHandler;
import com.facebook.soloader.SoLoader;
import com.google.android.gms.ads.identifier.AdvertisingIdClient;
import com.google.android.gms.common.ConnectionResult;
import com.google.android.gms.common.GoogleApiAvailability;
import com.google.android.gms.common.GooglePlayServicesNotAvailableException;
import com.google.android.gms.common.GooglePlayServicesRepairableException;
import com.google.android.gms.location.FusedLocationProviderClient;
import com.google.android.gms.location.LocationServices;
import com.google.android.gms.location.Priority;
import com.google.android.gms.maps.MapsInitializer;
import com.google.android.gms.tasks.CancellationTokenSource;
import com.google.android.play.core.appupdate.AppUpdateManager;
import com.google.android.play.core.install.model.AppUpdateType;
import com.google.android.play.core.install.model.UpdateAvailability;
import com.google.firebase.analytics.FirebaseAnalytics;
import com.google.firebase.messaging.FirebaseMessaging;
import com.google.firebase.perf.metrics.AddTrace;
import com.google.gson.Gson;

import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.Vector;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import in.juspay.hypersdk.core.PaymentConstants;
import in.juspay.hypersdk.data.JuspayResponseHandler;
import in.juspay.hypersdk.ui.HyperPaymentsCallbackAdapter;
import in.juspay.mobility.app.ChatService;
import in.juspay.mobility.app.InAppNotification;
import in.juspay.mobility.app.CleverTapSignedCall;
import in.juspay.mobility.app.LocationUpdateService;
import in.juspay.mobility.app.MissedCallActionsHandler;
import in.juspay.mobility.app.MobilityAppBridge;
import in.juspay.mobility.app.MyFirebaseMessagingService;
import com.google.firebase.crashlytics.FirebaseCrashlytics;
import in.juspay.mobility.app.NotificationUtils;
import in.juspay.mobility.app.OverlaySheetService;
import in.juspay.mobility.app.RemoteConfigs.MobilityRemoteConfigs;
import in.juspay.mobility.app.RideRequestActivity;
import in.juspay.mobility.app.TranslatorMLKit;
import in.juspay.mobility.app.WidgetService;
import in.juspay.mobility.app.callbacks.ShowNotificationCallBack;
import in.juspay.mobility.app.services.MobilityAppUpdate;
import in.juspay.mobility.common.utils.CipherUtil;
import in.juspay.mobility.common.utils.Utils;
import in.juspay.mobility.common.services.MobilityAPIResponse;
import in.juspay.mobility.common.services.MobilityCallAPI;
import in.juspay.services.HyperServices;

import java.util.Iterator;


import co.hyperverge.hyperkyc.HyperKyc;
import co.hyperverge.hyperkyc.data.models.HyperKycConfig;
import co.hyperverge.hyperkyc.data.models.result.HyperKycResult;


public class MainActivity extends AppCompatActivity {

    private static final String LOG_TAG = "MAIN_ACTIVITY";
    private static final int REQUEST_CODE_UPDATE_APP = 587;
    private static int updateType;
    MyFirebaseMessagingService.BundleUpdateCallBack bundleUpdateCallBack;

    private FirebaseAnalytics mFirebaseAnalytics = FirebaseAnalytics.getInstance(this);

    private Context context;
    private Activity activity;
    @Nullable
    private SharedPreferences sharedPref;
    ShowNotificationCallBack inappCallBack;
    private JSONObject currentLocationRes = new JSONObject();
    private JSONObject preInitFutureTaskResult = null;
    long onCreateTimeStamp = 0;
    private ActivityResultLauncher<Intent> startForResult;
    private static final MobilityRemoteConfigs remoteConfigs = new MobilityRemoteConfigs(false, true);
    ActivityResultLauncher<HyperKycConfig> launcher;
    private String registeredCallBackForHV;
    private ExecutorService currentLocExecuter;
    private final HyperPaymentsCallbackAdapter callbackAdapter = new HyperPaymentsCallbackAdapter() {
        @Override
        public void onEvent(JSONObject jsonObject, JuspayResponseHandler juspayResponseHandler) {
            JSONObject json = MobilityServiceHolder.getInitiatePayload();
            Log.d(LOG_TAG, "onEvent: " + jsonObject.toString());
            String event = jsonObject.optString("event");
            mFirebaseAnalytics.logEvent("ny_hyper_" + event,null);
            switch (event) {
                case "initiate_result":
                    Log.i("APP_PERF", "INITIATE_RESULT : " + System.currentTimeMillis());
                    try {
                        JSONObject innerPayload = json.getJSONObject(PaymentConstants.PAYLOAD);

                        String viewParam = null, deepLinkJSON = null;
                        if(preInitFutureTaskResult != null) {
                            Log.i("APP_PERF", "PRE_INIT : " + System.currentTimeMillis());
                            viewParam = preInitFutureTaskResult.optString("viewParam");
                            deepLinkJSON = preInitFutureTaskResult.optString("deepLinkJSON");
                        }
                        Log.i("APP_PERF", "INIT_FUTURE_TASK_RESULT : " + System.currentTimeMillis());

                        innerPayload.put("action", "process");
                        innerPayload.put("viewParam", viewParam);
                        innerPayload.put("view_param", viewParam);
                        innerPayload.put("deepLinkJSON", deepLinkJSON);
                        innerPayload.put("onCreateTimeStamp", onCreateTimeStamp);
                        innerPayload.put("initiateTimeStamp", MobilityServiceHolder.getInstance(context).getInitiateTime());
                        innerPayload.put("currentLocation", currentLocationRes);

                        if (getIntent() != null) {
                            setNotificationData(innerPayload, getIntent());
                            handleGeoSchemeData(innerPayload, getIntent());
                        }
                        json.put(PaymentConstants.PAYLOAD, innerPayload);
                        mFirebaseAnalytics.logEvent("ny_hyper_process", null);
                        Log.i("APP_PERF", "INIT_HYPER_SERVICE_INITIATE_RESULT : " + System.currentTimeMillis());
                        MobilityServiceHolder.getInstance(context).process((FragmentActivity) activity,findViewById(R.id.cl_dui_container),json);
                    } catch (JSONException e) {
                        throw new RuntimeException(e);
                    }
                    break;
                case "hide_loader":
                case "hide_splash":
                    hideSplash();
                    break;
                case "show_splash":
                    View v = findViewById(R.id.splash);
                    if (v != null) {
                        findViewById(R.id.splash).setVisibility(View.VISIBLE);
                    }
                    break;
                case "reboot":
                    Log.i(LOG_TAG, "event reboot");
                    mFirebaseAnalytics.logEvent("ny_hyper_terminate",null);
                    MobilityServiceHolder.getInstance(context).terminate();
                    initApp();
                    break;
                case "gl_sdk" :
                    in.juspay.mobility.Utils.onGullakEvent(jsonObject, MainActivity.this, sharedPref, startForResult );
                    break;
                case "in_app_notification":
                    showInAppNotificationApp(jsonObject, context);
                    break;
                case "process_result":
                    try {
                        JSONObject innerPayload = jsonObject.getJSONObject(PaymentConstants.PAYLOAD);
                        if (innerPayload.getString("action").equals("terminate")) {
                            minimizeApp(context);
                        }
                    } catch (Exception ignored) {
                    }
                    break;
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
                    break;
                case "launchHyperVerge":
                    try {
                        String cb = jsonObject.getString("callback");
                        registeredCallBackForHV = cb;
                        initHyperVergeSdk(jsonObject.getString("accessToken"), jsonObject.getString("workFlowId"), jsonObject.getString("transactionId"), jsonObject.getBoolean("useLocation"), jsonObject.getString("defLanguageCode"),jsonObject.getString("inputJson"));
                    }
                    catch (JSONException e) {
                        Log.e("Error Occurred while calling Hyperverge SDK ", e.toString());
                    }
                    break;

                default:
                    Log.e(LOG_TAG, "json_payload" + json);
            }
        }
    };
    private CleverTapSignedCall cleverTapSignedCall;


    SharedPreferences.OnSharedPreferenceChangeListener mListener = new SharedPreferences.OnSharedPreferenceChangeListener() {
        @Override
        public void onSharedPreferenceChanged(SharedPreferences sharedPreferences, String key) {
            if (key != null && key.equals("LANGUAGE_KEY")) {
                new TranslatorMLKit("en",sharedPreferences.getString(key, "null"),MainActivity.this);
                Utils.updateLocaleResource(sharedPreferences.getString(key,"__failed"),context);
            }
            if (key != null && key.equals("REGISTERATION_TOKEN")) {
                String token = sharedPreferences.getString(key, "null");
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
            if (key != null && sharedPreferences.getString("DRIVER_STATUS", "null").equals("true") && (key.equals("RIDE_G_FREQUENCY") || key.equals("MAX_LIMIT_TO_STORE_LOCATION_PT") || key.equals("NO_OF_LOCATION_PT_TO_REMOVE") || key.equals("DRIVER_MIN_DISPLACEMENT") || key.equals("RIDE_T_FREQUENCY") || key.equals("TRIP_STATUS"))) {
                System.out.println("TRIGGERED UPDATE POLLING");
                Context context = getApplicationContext();
                Intent locationUpdateIntent = new Intent(context, LocationUpdateService.class);
                if(key.equals("TRIP_STATUS")){
                    locationUpdateIntent.putExtra("TRIP_STATUS", sharedPreferences.getString(key,"null"));
                }
                context.startService(locationUpdateIntent);
            }
        }
    };
    private Intent widgetService;
    private AppUpdateManager appUpdateManager;

    @Override
    public void onBackPressed() {
        if (MobilityServiceHolder.getInstance(context).onBackPressed()) {
            super.onBackPressed();
        }
    }

    public String getAndUpdateRAMinSP() {
        String deviceRAM = "__failed";
        if (sharedPref != null) {
            deviceRAM = sharedPref.getString("DEVICE_RAM", "__failed");
        }
        if (!deviceRAM.equals("__failed"))
            return deviceRAM;
        long memory;
        try {
            memory = Utils.getDeviceRAM();
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
            String deviceRAM = getAndUpdateRAMinSP();
            if (bModel == null || bModel.isEmpty())
                bModel = "null";
            if (bBrand == null || bBrand.isEmpty())
                bBrand = "null";
            bVersion = bVersion == null || bVersion.isEmpty() ? "null" : "Android v" + bVersion;
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

    private String getDriverProfile() {
        String baseUrl = in.juspay.mobility.BuildConfig.CONFIG_URL_DRIVER;
        String driverProfileUrl = baseUrl + "/driver/profile";
        try {
            MobilityCallAPI mobilityApiHandler = MobilityCallAPI.getInstance(context);
            Map<String, String> baseHeaders = mobilityApiHandler.getBaseHeaders(context);
            MobilityAPIResponse apiResponse = mobilityApiHandler.callAPI(driverProfileUrl, baseHeaders, null, "GET", false);
            return apiResponse.getResponseBody();
        } catch (Exception error) {
            Log.d(LOG_TAG, "Catch in getDriverProfile : " + error);
        }
        return null;
    }

    protected JSONObject preInitFlow() {
        Vector<String> res = handleDeepLinkIfAvailable(getIntent());
        Vector<String> notificationDeepLinkVector = notificationTypeHasDL(getIntent());

        String viewParam = null, deepLinkJSON =null;
        if (res!=null ){
            viewParam = res.get(0);
            deepLinkJSON = res.get(1);
        }
        else if (notificationDeepLinkVector != null) {
            viewParam = notificationDeepLinkVector.get(0);
            deepLinkJSON = notificationDeepLinkVector.get(1);
        }

        if (MERCHANT_TYPE.equals("DRIVER")) {
            widgetService = new Intent(this, WidgetService.class);
            getWindow().addFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON);
            if (sharedPref != null) {
                Utils.updateLocaleResource(sharedPref.getString(getResources().getString(in.juspay.mobility.app.R.string.LANGUAGE_KEY), "null"),context);
            }
        }
        
        MobilityAppUpdate mobilityAppUpdate = new MobilityAppUpdate(this);
        mobilityAppUpdate.checkAndUpdateApp(remoteConfigs);

        updateConfigURL();
        mFirebaseAnalytics.setUserProperty("ct_objectId", Objects.requireNonNull(CleverTapAPI.getDefaultInstance(context)).getCleverTapID());

        JSONObject results = new JSONObject();
        try {
            if (viewParam != null) results.put("viewParam", viewParam);
            if (viewParam != null) results.put("view_param", viewParam);
            if (deepLinkJSON != null) results.put("deepLinkJSON", deepLinkJSON);
        } catch (JSONException e) {
            e.printStackTrace();
        }

        return results;
    }

    protected JSONObject getDriverInfoFlow() {
        String token = sharedPref.getString("REGISTERATION_TOKEN", "null");

        boolean shouldCallAPI = MERCHANT_TYPE.equals("DRIVER") && !token.equals("null") && !token.equals("__failed") && !token.equals("");

        JSONObject results = new JSONObject();
        try {
            if (shouldCallAPI) {
                String driverProfile = getDriverProfile();
                if (driverProfile != null) {
                    results.put("driverInfoResponse", new JSONObject(driverProfile));
                }
            }
        } catch (JSONException e) {
            e.printStackTrace();
        }
                return results;
    }

    protected void getCurrentLocationFlow(int locationPriority) {
        if (ActivityCompat.checkSelfPermission(this, ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED || ActivityCompat.checkSelfPermission(this, Manifest.permission.ACCESS_COARSE_LOCATION) != PackageManager.PERMISSION_GRANTED) return;
        FusedLocationProviderClient client = LocationServices.getFusedLocationProviderClient(this);
        CancellationTokenSource cancellationTokenSource = new CancellationTokenSource();
        client.getCurrentLocation(locationPriority, cancellationTokenSource.getToken())
                .addOnSuccessListener(location -> {
                    if (location != null) {
                        try {
                            currentLocationRes.put("lat",location.getLatitude());
                            currentLocationRes.put("lon",location.getLongitude());
                        } catch (JSONException e) {
                            currentLocationRes = null;
                        }
                    }});
    }

    @SuppressLint("SetJavaScriptEnabled")
    @Override
    @AddTrace(name = "onCreateTrace", enabled = true /* optional */)
    protected void onCreate(Bundle savedInstanceState) {
        Log.i("APP_PERF", "ON_CREATE_START : " + System.currentTimeMillis());
        onCreateTimeStamp = System.currentTimeMillis();
        currentLocExecuter = Executors.newSingleThreadExecutor();
        ActivityLifecycleCallback.register(this.getApplication());
        super.onCreate(savedInstanceState);
        context = getApplicationContext();
        sharedPref = context.getSharedPreferences(this.getString(in.juspay.mobility.app.R.string.preference_file_key), Context.MODE_PRIVATE);
        activity = this;
        initiateHvLauncher();
        if (isClassAvailable("com.facebook.soloader.SoLoader")) SoLoader.init(this, false);
        initiateRSIntegration();
        boolean isPerfEnabled = false, isPerfEnabledCustomer = false;
        String okHttpConfig = "{}";
        try{
            isPerfEnabled = remoteConfigs.getBoolean("perf_enabled");
            isPerfEnabledCustomer = remoteConfigs.getBoolean("perf_enabled_customer");
            okHttpConfig = remoteConfigs.getString("ok_http_config");
            Log.i("PERF", "Fetched from remote config - perf enabled : " + isPerfEnabled);

        }catch(Exception e){
            Log.i("PERF", "unable to fetch PERF remote config");
            Exception exception = new Exception("Error in parsing perf config " + e);
            FirebaseCrashlytics.getInstance().recordException(exception);            
        }
        if (!MobilityServiceHolder.getInstance(context).isInitialized()) {
            MobilityServiceHolder.getInstance(context).initiate(context);
        }
        MobilityServiceHolder.getInstance(context).setCallbackAdapter(callbackAdapter);

        if(isPerfEnabledCustomer){
            String appName = getApplicationContext().getResources().getString(R.string.app_type);
            if(appName.equals("driver")){
                String configPriority = remoteConfigs.getString("driver_location_priority");
                int priority = in.juspay.mobility.app.Utils.getPriority(configPriority);
                currentLocExecuter.execute(() -> getCurrentLocationFlow(priority));
            }else{
                currentLocExecuter.execute(() -> getCurrentLocationFlow(Priority.PRIORITY_HIGH_ACCURACY));
            }
        }

        preInitFutureTaskResult = preInitFlow();

        handleSplashScreen();

        WebView.setWebContentsDebuggingEnabled(true);

        boolean isMigrated = migrateLocalStore(context);
        String clientId = context.getResources().getString(R.string.client_id);

        mFirebaseAnalytics.logEvent(isMigrated ?"migrate_local_store_success" : "migrate_local_store_failed",new Bundle());
        CleverTapAPI cleverTap = CleverTapAPI.getDefaultInstance(context);
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            CleverTapAPI.createNotificationChannel(context,clientId,"Promotion","Notifications Related to promotion",NotificationManager.IMPORTANCE_MAX, "4_promotional",true);
            CleverTapAPI.createNotificationChannel(context,"nammayatriHindi","nammayatriHindi","notification",NotificationManager.IMPORTANCE_MAX,true,"clevertap_custom_notification.mp3");
        }else{
            CleverTapAPI.createNotificationChannel(context,clientId,"Promotion","Notifications Related to promotion",NotificationManager.IMPORTANCE_MAX,true);
        }
        CleverTapAPI.setDebugLevel(CleverTapAPI.LogLevel.VERBOSE);
        cleverTap.enableDeviceNetworkInfoReporting(true);
        CleverTapAPI.setNotificationHandler((NotificationHandler)new PushTemplateNotificationHandler());
        initCTSignedCall(context,activity,remoteConfigs);

        sharedPref.edit().putString("DEVICE_DETAILS", getDeviceDetails()).apply();
        sharedPref.edit().putString("UNIQUE_DD", NotificationUtils.uniqueDeviceDetails()).apply();
        sharedPref.registerOnSharedPreferenceChangeListener(mListener);
        sharedPref.edit().putString(getResources().getString(in.juspay.mobility.app.R.string.ACTIVITY_STATUS), "onCreate").apply();
        sharedPref.edit().putString("OK_HTTP_CONFIG", okHttpConfig).apply();


        try {
            MapsInitializer.initialize(getApplicationContext());
        } catch (Exception e) {
            e.printStackTrace();
        }
        registerCallBack();

        if (BuildConfig.DEBUG) {
            FirebaseMessaging.getInstance().subscribeToTopic("test");
        }
        Window window = this.getWindow();
        window.addFlags(WindowManager.LayoutParams.FLAG_DRAWS_SYSTEM_BAR_BACKGROUNDS);
        window.clearFlags(WindowManager.LayoutParams.FLAG_TRANSLUCENT_STATUS);
        window.setStatusBarColor(this.getResources().getColor(R.color.colorPrimaryDark, getTheme()));
        countAppUsageDays();
        startForResult = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), result -> handleGlResp(result, MainActivity.this));
    }

    private void initiateRSIntegration() {
        String algo = BuildConfig.RS_ALGO;
        String algoPadding = BuildConfig.RS_ALGO_PADDING;
        String instanceType = BuildConfig.RS_INSTANCE_TYPE;
        String encKey = BuildConfig.RS_ENC_KEY;

        CipherUtil cipherUtil = CipherUtil.getInstance();
        cipherUtil.setParams(algo, algoPadding, instanceType, encKey);
    }


    public void initiateHvLauncher() {
        if (isClassAvailable ("co.hyperverge.hyperkyc.HyperKyc") && isClassAvailable("co.hyperverge.hyperkyc.data.models.result.HyperKycResult") && isClassAvailable("com.google.gson.Gson")) {
            launcher = this.registerForActivityResult(new HyperKyc.Contract(), new ActivityResultCallback<HyperKycResult>() {
                @Override
                public void onActivityResult(HyperKycResult result) {
                    try {
                        Gson gson = new Gson();
                        String jsonStr = gson.toJson(result);


                        JSONObject processPL = new JSONObject();
                        JSONObject innerPayload = getInnerPayload(new JSONObject(),"process_hv_resp", MainActivity.this);
                        innerPayload.put("callback", registeredCallBackForHV)
                                .put("hv_response", jsonStr);
                        processPL.put(PaymentConstants.PAYLOAD, innerPayload)
                                .put("requestId", UUID.randomUUID())
                                .put("service", getService());
                        MobilityServiceHolder.getInstance(context).process(processPL);
                    } catch (Exception e) {
                        Log.e("HV error : ", "error_in_HyperKycResult");
                    }

                }
            });
        }
    }
    private void handleSplashScreen() {
        try {
            setContentView(R.layout.activity_main);
            boolean skipDefaultSplash = false;
            String city = "__failed";
            if (sharedPref != null) {
                city = sharedPref.getString("DRIVER_LOCATION", "__failed");
                if (city.equals("__failed")) {
                    city = sharedPref.getString("CUSTOMER_LOCATION", "__failed");
                }
            }
            String merchantId = context.getResources().getString(R.string.merchant_id);
            JSONObject clevertapConfig = new JSONObject(remoteConfigs.getString("enable_city_based_splash_scn"));
            boolean enableCityBasedSplash = clevertapConfig.getBoolean(merchantId);
            View splash = findViewById(R.id.splash);
            LottieAnimationView splashLottie = splash.findViewById(R.id.splash_lottie);
            if (splashLottie != null) {
                if ((!city.equals("__failed")) && enableCityBasedSplash ) {
                    skipDefaultSplash = setSplashAnimAndStart(splashLottie, city.toLowerCase());
                }
                if ((!skipDefaultSplash) && enableCityBasedSplash) {
                    if ((splashLottie.getTag() != null) && splashLottie.getTag().equals("autoStart")) {
                        splashLottie.setVisibility(View.VISIBLE);
                        splashLottie.setRepeatCount(ValueAnimator.INFINITE);
                        splashLottie.playAnimation();
                    }
                }
                splash.setVisibility(View.VISIBLE);
            }
        } catch (Exception e){
            Bundle bundle = new Bundle();
            bundle.putString("Exception",e.toString());
            FirebaseAnalytics mFirebaseAnalytics = FirebaseAnalytics.getInstance(this);
            mFirebaseAnalytics.logEvent("splash_screen_inflate_exception",bundle);
            setContentView(R.layout.activity_main_without_bg);
        }
    }

    private boolean setSplashAnimAndStart (LottieAnimationView view ,String city) {
        ResourceHandler resourceHandler = new ResourceHandler(this);
        @Nullable
        String animationFile = null;
        try {
            JSONObject cityConfig = getCityConfig(city);
            String file = cityConfig.optString("file_name","");
            if  (resourceHandler.isResourcePresent("raw",file) && !cityConfig.optBoolean("force_remote",false)) {
                animationFile = resourceHandler.getRawResource(file);
            } else {
                animationFile = cityConfig.optString("url");
            }
        } catch (Exception e) {
            Bundle bundle = new Bundle();
            bundle.putString("Exception",e.toString());
            FirebaseAnalytics mFirebaseAnalytics = FirebaseAnalytics.getInstance(this);
            mFirebaseAnalytics.logEvent("exception_while_reading_city_config",bundle);
        }
        resourceHandler.close();
        if (animationFile != null && !animationFile.isEmpty()) {
            if (animationFile.startsWith("http")) {
                view.setFailureListener(throwable -> mFirebaseAnalytics.logEvent("failure_in_set_animation_from_url",new Bundle()));
                view.setAnimationFromUrl(animationFile);
            } else {
                view.setAnimationFromJson(animationFile,null);
            }
            view.setVisibility(View.VISIBLE);
            if ((view.getTag() != null) && view.getTag().equals("autoStart")) view.setRepeatCount(ValueAnimator.INFINITE);
            view.playAnimation();
            return true;
        } else {
            return false;
        }
    }

// @NotUsed
//    @NonNull
//    private boolean getCityConfigForFeatureFlags(String key) {
//        MobilityRemoteConfigs remoteConfigs = new MobilityRemoteConfigs(false, false);
//        String city = sharedPref.getString("DRIVER_LOCATION", "__failed").toLowerCase();
//        String forward_dispatch_config = remoteConfigs.getString(key);
//        JSONObject config = new JSONObject();
//        JSONObject cityConfig = new JSONObject();
//        boolean isFeatureEnabled = false;
//        Log.d("Feature flags","remote config for feature :-" + key +  "->" + forward_dispatch_config);
//        try {
//            config = new JSONObject(forward_dispatch_config);
//            cityConfig = config.optJSONObject(city);
//            Log.d("feature bool", "getCityConfigForFeatureFlags: " + cityConfig);
//            if(cityConfig != null){isFeatureEnabled = cityConfig.optBoolean("is_" + key + "_Enabled", false);}
//        } catch (Exception e) {
//            Bundle bundle = new Bundle();
//            bundle.putString("Exception",e.toString());
//            FirebaseAnalytics mFirebaseAnalytics = FirebaseAnalytics.getInstance(this);
//            Log.d("feature bool deterimatal", "getCityConfigForFeatureFlags: " + cityConfig);
//            mFirebaseAnalytics.logEvent("exception_while_reading_splash_config",bundle);
//        }
//        return isFeatureEnabled ;
//    }

    @NonNull
    private JSONObject getCityConfig(String city) {
        MobilityRemoteConfigs remoteConfigs = new MobilityRemoteConfigs(false, false);
        String splashScreenConfig = remoteConfigs.getString("splash_screen_" + city);
        JSONObject config = new JSONObject();
        JSONObject cityConfig = new JSONObject();
        try {
            cityConfig = new JSONObject(splashScreenConfig);
            String merchant = MERCHANT_TYPE.toLowerCase();
            config = cityConfig.optJSONObject(merchant);
        } catch (Exception e) {
            Bundle bundle = new Bundle();
            bundle.putString("Exception",e.toString());
            FirebaseAnalytics mFirebaseAnalytics = FirebaseAnalytics.getInstance(this);
            mFirebaseAnalytics.logEvent("exception_while_reading_splash_config",bundle);
        }
        return config != null ? config : cityConfig ;
    }

    private void registerCallBack() {
        inappCallBack = new ShowNotificationCallBack() {
            @Override
            public void showInAppNotification(JSONObject jsonObject, Context context) {
                showInAppNotificationApp(jsonObject, context);
            }

            @Override
            public void hideInAppNotification(String channelId) {
                hideInAppNotificationApp(channelId);
            }
        };
        ChatService.registerInAppCallback(inappCallBack);
        bundleUpdateCallBack = this::showAlertForUpdate;
        MyFirebaseMessagingService.registerBundleUpdateCallback(bundleUpdateCallBack);
        MyFirebaseMessagingService.registerShowNotificationCallBack(inappCallBack);
    }


    public void updateConfigURL() {
        String key = MERCHANT_TYPE;
        String merchantId = key.equals("USER") ? in.juspay.mobility.BuildConfig.MERCHANT_ID_USER : in.juspay.mobility.BuildConfig.MERCHANT_ID_DRIVER;
        String baseUrl = key.equals("USER") ? in.juspay.mobility.BuildConfig.CONFIG_URL_USER : in.juspay.mobility.BuildConfig.CONFIG_URL_DRIVER;
        SharedPreferences sharedPreff = getApplicationContext().getSharedPreferences(
                activity.getString(in.juspay.mobility.app.R.string.preference_file_key), Context.MODE_PRIVATE);
        SharedPreferences.Editor editor = sharedPreff.edit();
        editor.putString("MERCHANT_ID", merchantId);
        editor.putString("BASE_URL", baseUrl);
        editor.apply();
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, @Nullable Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
        mFirebaseAnalytics.logEvent("ny_hyper_onActivityResult",null);
        MobilityServiceHolder.getInstance(context).onActivityResult(requestCode, resultCode, data);
        if (requestCode == REQUEST_CODE_UPDATE_APP) {
            if (resultCode != RESULT_OK) {
                Log.i(LOG_TAG,"Update flow failed! Result code: " + resultCode);
                if(updateType == AppUpdateType.IMMEDIATE){
                    finishAndRemoveTask();
                }
            }
        }
    }

    private void initApp() {
        MobilityServiceHolder.getInstance(context).setCallbackAdapter(callbackAdapter);
        MobilityServiceHolder.getInstance(context).initiate(context);
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
            mFirebaseAnalytics.logEvent("ny_hyper_terminate",null);
            MobilityServiceHolder.getInstance(context).terminate();
            initApp();
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
        String deepLinkJSON = null, viewParam = null;
        if (appLinkData != null && appLinkData.getQuery() != null) {
            String query = appLinkData.getQuery();
            HashMap<String, String> query_params = getQueryMap(query);
            for (String key : query_params.keySet()) {
                if (key.equals("vp")){
                    viewParam = query_params.get(key);
                    break;
                } else if (key.equals("referrer")) {
                    viewParam = query;
                    break;
                }
            }
            Gson gson = new Gson();
            deepLinkJSON = gson.toJson(query_params);
        } else return null;
        if(viewParam==null || deepLinkJSON == null) return null;
        res.add(viewParam);
        res.add(deepLinkJSON);
        return res;
    }

    private void processDeeplink(String viewParam, String deepLinkJSON){
        try {
            JSONObject processPayloadDL = new JSONObject();
            JSONObject innerPayloadDL = getInnerPayload(new JSONObject(),"process", MainActivity.this);
            if (viewParam != null && deepLinkJSON != null) {
                innerPayloadDL.put("view_param", viewParam)
                        .put("deepLinkJSON", deepLinkJSON)
                        .put("viewParamNewIntent", viewParam)
                        .put("onNewIntent", true);
                processPayloadDL.put("service", getService())
                        .put("merchantId", getResources().getString(R.string.merchant_id))
                        .put("requestId", UUID.randomUUID())
                        .put(PaymentConstants.PAYLOAD, innerPayloadDL);
                mFirebaseAnalytics.logEvent("ny_hyper_process",null);
                MobilityServiceHolder.getInstance(context).process(processPayloadDL);
            }
        }catch (Exception e){
            // Need to handle exception
        }
    }

    private Vector<String> notificationTypeHasDL(Intent intent) {
        try {
            if (intent != null && intent.hasExtra("NOTIFICATION_DATA")) {
                String data = intent.getExtras().getString("NOTIFICATION_DATA");
                JSONObject jsonData = new JSONObject(data);
                if (jsonData.has("notification_type")){
                    String type = jsonData.getString("notification_type");
                    if (type.equals("COINS_SUCCESS")){
                        return new Vector<>(Arrays.asList("coins", "{\"vp\":\"coins\"}"));
                    }
                }
            }
        }catch (Exception e){

        }
        return null;
    }

    private Vector<Double> handleGeoSchemeVector(Intent intent){
        Uri data = intent.getData();
        Vector<Double> geoData = new Vector<>();
        String[] parts;
        try{
            if (data != null && data.getScheme().equals("geo")){
                parts = data.getSchemeSpecificPart().split(",");
                if (parts.length != 3) return geoData;
                String latitude = parts[0];
                String longitude = parts[2];
                geoData.add(Double.parseDouble(latitude));
                geoData.add(Double.parseDouble(longitude));
            }
        }catch (Exception e){
            e.printStackTrace();
        }

        return geoData;

    }
    private void handleGeoSchemeData(JSONObject innerPayload , Intent intent){
        try {
            Uri data = intent.getData();
            if (data != null && intent.getScheme().equals("geo")) {
                Vector<Double> geoData = handleGeoSchemeVector(intent);
                JSONObject geoObj = new JSONObject();
                geoObj.put("lat", geoData.get(0));
                geoObj.put("lon", geoData.get(1));
                geoObj.put("name",null);
                innerPayload.put("destination", geoObj);
            }
        }catch(Exception e){
            e.printStackTrace();
        }
    }
    @Override
    protected void onNewIntent(Intent intent) {
        Vector<String> res = handleDeepLinkIfAvailable(intent);
        Vector<String> notificationDeepLinkVector = notificationTypeHasDL(intent);
        String viewParam = null, deepLinkJSON =null;
        if (res!=null ){
            viewParam = res.get(0);
            deepLinkJSON = res.get(1);
        } else if (notificationDeepLinkVector != null) {
            viewParam = notificationDeepLinkVector.get(0);
            deepLinkJSON = notificationDeepLinkVector.get(1);
        }
        processDeeplink(viewParam, deepLinkJSON);
        if (intent != null && intent.hasExtra("NOTIFICATION_DATA")) {
            try {
                JSONObject proccessPayload = new JSONObject().put("service", getService())
                        .put("requestId", UUID.randomUUID());
                JSONObject innerPayload = new JSONObject().put("onNewIntent", true);
                proccessPayload.put(PaymentConstants.PAYLOAD, innerPayload);
                setNotificationData(innerPayload, intent);
                mFirebaseAnalytics.logEvent("ny_hyper_process",null);
                MobilityServiceHolder.getInstance(context).process(proccessPayload);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        if(intent.getScheme()!=null && intent.getScheme().equals("geo")){
            try{
                JSONObject proccessPayload = new JSONObject().put("service", getService())
                        .put("requestId", UUID.randomUUID());
                JSONObject innerPayload = new JSONObject().put("onNewIntent", true);
                proccessPayload.put(PaymentConstants.PAYLOAD, innerPayload);
                handleGeoSchemeData(innerPayload, intent);
                mFirebaseAnalytics.logEvent("ny_hyper_process",null);
                MobilityServiceHolder.getInstance(context).process(proccessPayload);
            }
            catch (Exception e){
                e.printStackTrace();
            }
        }
        super.onNewIntent(intent);
    }

    public void setNotificationData (JSONObject innerPayload, Intent intent) {
        try {
            String data = intent.getExtras().getString("NOTIFICATION_DATA");
            String fullNotificationString = intent.getExtras().getString("fullNotificationBody");
            JSONObject jsonData = new JSONObject(data);
            if (fullNotificationString != null) {
                JSONObject fullNotification = new JSONObject(fullNotificationString);
                innerPayload.put("fullNotificationBody", fullNotification);
            }
            if (jsonData.has("notification_type") && jsonData.getString("notification_type").equals("CHAT_MESSAGE")) {
                getInnerPayload(innerPayload, "OpenChatScreen", MainActivity.this);
                NotificationManager notificationManager = (NotificationManager) getSystemService(Context.NOTIFICATION_SERVICE);
                notificationManager.cancel(NotificationUtils.chatNotificationId);
                innerPayload.put("notification_type", "CHAT_MESSAGE");
            }

            if(jsonData.has("entity_data")){
                JSONObject entityData = (JSONObject) jsonData.get("entity_data");
                if(entityData.has("channelId")){
                    innerPayload.put("chatMessageData", jsonData.get("entity_data"));
                }
            }

            if (jsonData.has("notification_type") && jsonData.has("entity_ids")) {
                String id = jsonData.getString("entity_ids");
                String type = jsonData.getString("notification_type");
                innerPayload.put("notification_type", type);
                if (type.equals("NEW_MESSAGE")) {
                    getInnerPayload(innerPayload, "callDriverAlert", MainActivity.this);
                    innerPayload.put("id", id)
                            .put("popType", type);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Override
    protected void onResume() {
        super.onResume();
        setCleverTapUserProp("Session Status" , "true" , context);
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
                                REQUEST_CODE_UPDATE_APP
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
        pauseYoutubePlayer();
        setCleverTapUserProp("Session Status" , "false" , context);
        if (sharedPref != null)
            sharedPref.edit().putString(getResources().getString(in.juspay.mobility.app.R.string.ACTIVITY_STATUS), "onPause").apply();
        if (BuildConfig.MERCHANT_TYPE.equals("DRIVER") &&
                widgetService != null && Settings.canDrawOverlays(this) &&
                !sharedPref.getString(getResources().getString(in.juspay.mobility.app.R.string.REGISTERATION_TOKEN), "null").equals("null") &&
                !sharedPref.getString("DISABLE_WIDGET", "true").equals("true") &&
                !sharedPref.getString("ANOTHER_ACTIVITY_LAUNCHED", "false").equals("true")) {
            widgetService.putExtra("payload", "{}");
            widgetService.putExtra("data", "{}");
            startService(widgetService);
        }
    }

    @Override
    protected void onDestroy() {
        if (currentLocExecuter != null) {
            currentLocExecuter.shutdown();
            try {
                if (!currentLocExecuter.awaitTermination(800, TimeUnit.MILLISECONDS)) {
                    currentLocExecuter.shutdownNow();
                }
            } catch (InterruptedException e) {
                currentLocExecuter.shutdownNow();
            }
        }
        if (sharedPref != null) {
            sharedPref.edit().putString(getResources().getString(in.juspay.mobility.app.R.string.ACTIVITY_STATUS), "onDestroy").apply();
        }
        pauseYoutubePlayer();
        mFirebaseAnalytics.logEvent("ny_hyper_terminate",null);
        MobilityServiceHolder.getInstance(context).terminate();
        ChatService.deRegisterInAppCallback(inappCallBack);
        MyFirebaseMessagingService.deRegisterBundleUpdateCallback(bundleUpdateCallBack);
        MyFirebaseMessagingService.deRegisterShowNotificationCallBack(inappCallBack);
        super.onDestroy();
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults);
        mFirebaseAnalytics.logEvent("ny_hyper_onRequestPermissionsResult",null);
        MobilityServiceHolder.getInstance(context).onRequestPermissionsResult(requestCode, permissions, grantResults);
    }

    private void pauseYoutubePlayer(){
        MobilityAppBridge.youtubeVideoStatus = "PAUSE";
        if (MobilityAppBridge.youtubePlayer != null ) {
            MobilityAppBridge.youtubePlayer.pause();
        } else if (MobilityAppBridge.youTubePlayerView != null ) {
            MobilityAppBridge.youTubePlayerView = null;
        }
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

    public void showInAppNotificationApp(JSONObject payload, Context context) {
        try {
            Handler handler = new Handler(context.getMainLooper());
            handler.postDelayed(() -> {
                try {
                    InAppNotification.getInstance(activity,activity.findViewById(R.id.cl_dui_container)).generateNotification(payload);
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

    public void hideInAppNotificationApp (String channelId) {
        InAppNotification.getInstance(activity, activity.findViewById(R.id.cl_dui_container)).hideInAppNotification(channelId);
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
            System.out.println("GAID "+ s);
            Bundle params = new Bundle();
            params.putString("id", s);
            FirebaseAnalytics.getInstance(context).logEvent("ad_id", params);
        }
    }

    public static String getService() {
        if (MERCHANT_TYPE.equals("USER")) {
            return "in.yatri.consumer";
        } else {
            return "in.yatri.provider";
        }
    }

    private boolean migrateLocalStore(Context context) {
        SharedPreferences oldSharedPref = context.getSharedPreferences("namma_yatri_app_local_keys",MODE_PRIVATE);
        SharedPreferences currentSharedPref = context.getSharedPreferences(context.getString(in.juspay.mobility.app.R.string.preference_file_key),MODE_PRIVATE);
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
    
    public void initHyperVergeSdk(String accessToken,  String workFlowId, String transactionId, boolean useLocation, String defLanguageCode, String inputsJson) {
        if (isClassAvailable ("co.hyperverge.hyperkyc.data.models.HyperKycConfig")) {
                HyperKycConfig config = new HyperKycConfig(accessToken, workFlowId, transactionId);
                config.setUseLocation(useLocation);
                config.setDefaultLangCode(defLanguageCode);
                if (inputsJson.length() > 0) {
                    Map<String, String> inpMap = new HashMap<>();
                    JSONObject jsonObject;
                    try {
                        jsonObject = new JSONObject(inputsJson);
                        for (Iterator<String> it = jsonObject.keys(); it.hasNext(); ) {
                            String key = it.next();
                            inpMap.put(key, jsonObject.getString(key));
                        }
                    }
                    catch (JSONException e) {
                        Log.e("Unable find Specified Key, So returning config without setting inputs.", inputsJson);
                        e.printStackTrace();
                        return;
                    }
                    if (inpMap.size() > 0)  config.setInputs(inpMap);
                    else Log.d("HyperKycConfig Inputs JSON: ", "Empty json passed as input so not initializing inputs in config");
                }
                else Log.d("HyperKycConfig Inputs JSON: ", "Not initializing inputs as inputs json passed is null");
                launcher.launch(config);
            } else {

        }
        }

}