/*
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility;

import static android.Manifest.permission.CAMERA;
import static android.Manifest.permission.READ_EXTERNAL_STORAGE;
import static android.Manifest.permission.WRITE_EXTERNAL_STORAGE;
import static in.juspay.mobility.R.integer.REQUEST_CODE_UPDATE_APP;

import android.Manifest;
import android.animation.Animator;
import android.annotation.SuppressLint;
import android.app.Activity;
import android.app.ActivityManager;
import android.app.AlertDialog;
import android.content.BroadcastReceiver;
import android.content.ContentResolver;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.IntentSender;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.database.Cursor;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.location.LocationManager;
import android.media.Ringtone;
import android.media.RingtoneManager;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Bundle;
import android.os.Environment;
import android.os.Handler;
import android.os.PowerManager;
import android.provider.ContactsContract;
import android.provider.MediaStore;
import android.provider.OpenableColumns;
import android.provider.Settings;
import android.util.Base64;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.view.WindowManager;
import android.view.animation.AnimationUtils;
import android.view.inputmethod.InputMethodManager;
import android.webkit.WebView;
import android.widget.TextView;
import android.widget.Toast;
import android.media.MediaPlayer;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.cardview.widget.CardView;
import androidx.constraintlayout.widget.ConstraintLayout;
import androidx.core.app.ActivityCompat;
import androidx.core.content.ContextCompat;
import androidx.core.content.FileProvider;
import androidx.core.content.IntentCompat;
import androidx.core.location.LocationManagerCompat;
import androidx.work.WorkManager;

import com.airbnb.lottie.LottieAnimationView;
import com.google.android.gms.ads.identifier.AdvertisingIdClient;
import com.google.android.gms.common.ConnectionResult;
import com.google.android.gms.common.GoogleApiAvailability;
import com.google.android.gms.common.GooglePlayServicesNotAvailableException;
import com.google.android.gms.common.GooglePlayServicesRepairableException;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.maps.MapsInitializer;
import com.google.android.gms.tasks.OnFailureListener;
import com.google.android.gms.tasks.OnSuccessListener;
import com.google.android.gms.tasks.Task;
import com.google.android.libraries.places.api.model.Place;
import com.google.android.libraries.places.widget.Autocomplete;
import com.google.android.libraries.places.widget.AutocompleteActivity;
import com.google.android.play.core.appupdate.AppUpdateInfo;
import com.google.android.play.core.appupdate.AppUpdateManager;
import com.google.android.play.core.appupdate.AppUpdateManagerFactory;
import com.google.android.play.core.install.model.AppUpdateType;
import com.google.android.play.core.install.model.UpdateAvailability;
import com.google.firebase.analytics.FirebaseAnalytics;
// import com.google.firebase.database.DataSnapshot;
// import com.google.firebase.database.DatabaseError;
// import com.google.firebase.database.DatabaseReference;
// import com.google.firebase.database.FirebaseDatabase;
// import com.google.firebase.database.ValueEventListener;
import com.google.firebase.dynamiclinks.FirebaseDynamicLinks;
import com.google.firebase.dynamiclinks.PendingDynamicLinkData;
import com.google.firebase.messaging.BuildConfig;
import com.google.firebase.messaging.FirebaseMessaging;
import com.theartofdev.edmodo.cropper.CropImage;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.Timer;
import java.util.TimerTask;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

import in.juspay.mobility.utils.ConnectionStateMonitor;
import in.juspay.mobility.utils.LocationUpdateService;
import in.juspay.mobility.utils.MediaPlayerView;
import in.juspay.mobility.utils.MyFirebaseMessagingService;
import in.juspay.mobility.utils.NetworkBroadcastReceiver;
import in.juspay.mobility.utils.NotificationUtils;
import in.juspay.mobility.utils.RideRequestActivity;
import in.juspay.mobility.utils.RideRequestUtils;
import in.juspay.mobility.utils.WidgetService;
import in.juspay.mobility.utils.mediaPlayer.DefaultMediaPlayerControl;
import in.juspay.hypersdk.core.JuspayServices;
import in.juspay.hypersdk.core.Labels;
import in.juspay.hypersdk.core.PaymentConstants;
import in.juspay.hypersdk.core.SdkTracker;
import in.juspay.hypersdk.data.JuspayResponseHandler;
import in.juspay.hypersdk.ui.HyperPaymentsCallbackAdapter;
import in.juspay.hypersdk.core.DuiCallback;
import in.juspay.services.HyperServices;

//import com.facebook.LoggingBehavior;
//import com.facebook.applinks.AppLinkData;


public class MainActivity extends AppCompatActivity {

    private static final String TAG = "MainActivity";
    private static final int IMAGE_PERMISSION_REQ_CODE = 4997;
    private static final int IMAGE_CAPTURE_REQ_CODE = 101;
    private static final int STORAGE_PERMISSION = 67;
    private HyperServices hyperServices;
    private ConnectionStateMonitor stateMonitor;
    private WebView webView;
    private Context context;
    public JuspayServices juspayServicesGlobal;
    private Activity activity;
    private NotificationUtils.NotificationCallback notificationCallback;
    private LocationUpdateService.UpdateTimeCallback timeUpdateCallback;
    private static FirebaseAnalytics mFirebaseAnalytics;
    private SharedPreferences sharedPref;
    private IntentFilter intentFilter;
    private Intent widgetService;
    String GAID;
    public String imageBase64 = "sin";
    private AppUpdateManager appUpdateManager;
    private static int updateType;
    private LocationManager locationManager;
    private MediaPlayer mediaPlayer;
    private static MainActivity instance;
    private NetworkBroadcastReceiver networkBroadcastReceiver;
    private boolean isHideSplashEventCalled = false;
    private boolean isSystemAnimEnabled = true;
    public static MainActivity getInstance() {
        return instance;
    }


    BroadcastReceiver gpsReceiver = new BroadcastReceiver(){
        @Override
        public void onReceive(Context context, Intent intent) {
            locationManager = (LocationManager) context.getSystemService(Context.LOCATION_SERVICE);
            Boolean isGpsEnabled = locationManager.isProviderEnabled(LocationManager.GPS_PROVIDER);
            String token = sharedPref.getString("REGISTERATION_TOKEN", "null");;
            if (!isGpsEnabled && !token.equals("null")){
                triggerPopUPMain("true","LOCATION_DISABLED");
            }else{
                callingDriverLocationPermission();
            }
        }
    };

    SharedPreferences.OnSharedPreferenceChangeListener mListener = new SharedPreferences.OnSharedPreferenceChangeListener() {
        @Override
        public void onSharedPreferenceChanged(SharedPreferences sharedPreferences, String key) {
            if (key != null && key.equals("REGISTERATION_TOKEN")){
                String token = sharedPref.getString("REGISTERATION_TOKEN", "null");
                if (token.equals("__failed")) {
                    final PackageManager pm = getApplicationContext().getPackageManager();
                    final Intent intent = pm.getLaunchIntentForPackage(getApplicationContext().getPackageName());
                    try {
                        if (activity != null) {
                            activity.finishAffinity();// Finishes all activities.
                            activity.startActivity(intent);
                        } else {
                            SharedPreferences.Editor editor = sharedPref.edit();
                            editor.clear();
                        }
                    }catch (NullPointerException e){
                        e.printStackTrace();
                    }
                }
            }
            // Update Driver status in Local Storage
            if (key != null && key.equals("DRIVER_STATUS"))
            {String status = sharedPref.getString("DRIVER_STATUS", "null");
                WorkManager mWorkManager = WorkManager.getInstance(getApplicationContext());
                if (status.equals("null"))
                {
                    if  (context != null)
                    {
                        Intent locationUpdateIntent = new Intent(context, LocationUpdateService.class);
                        context.stopService(locationUpdateIntent);
                        mWorkManager.cancelAllWorkByTag(context.getString(R.string.location_update));
                    }
                    else {
                        Context context = getApplicationContext();
                        Intent locationUpdateIntent = new Intent(context, LocationUpdateService.class);
                        context.stopService(locationUpdateIntent);
                        mWorkManager.cancelAllWorkByTag(context.getString(R.string.location_update));
                    }

                }
            }
            if (key != null && sharedPref.getString("DRIVER_STATUS", "null").equals("true") && (key.equals("RIDE_G_FREQUENCY") || key.equals("MAX_LIMIT_TO_STORE_LOCATION_PT") || key.equals("NO_OF_LOCATION_PT_TO_REMOVE") || key.equals("DRIVER_MIN_DISPLACEMENT") || key.equals("RIDE_T_FREQUENCY"))){
                System.out.println("TRIGGERED UPDATE POLLING");
                Context context = getApplicationContext();
                Intent locationUpdateIntent = new Intent(context, LocationUpdateService.class);
                context.startService(locationUpdateIntent);
            }
        }
    };

    class NotificationListener extends BroadcastReceiver {
        @Override
        public void onReceive(Context context, Intent intent) {
            Log.e("In Main activity", context.toString());
        }
    }

    @Override
    public void onBackPressed() {
        if (webView.getVisibility() == View.VISIBLE) {
            webView.setVisibility(View.GONE);
            return;
        }
        if (hyperServices != null && !hyperServices.onBackPressed()) {
            super.onBackPressed();
        }
    }

    @SuppressLint("SetJavaScriptEnabled")
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        // Obtain the FirebaseAnalytics instance.
        mFirebaseAnalytics = FirebaseAnalytics.getInstance(this);
        instance = this;
        try {
            MapsInitializer.initialize(getApplicationContext());
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        // TODO :- Discuss on the approach for handling realtime database
//        FirebaseDatabase database = FirebaseDatabase.getInstance();
//        DatabaseReference myRef = database.getReference("app");
//        // Read from the database
//        myRef.addValueEventListener(new ValueEventListener() {
//            @Override
//            public void onDataChange(DataSnapshot dataSnapshot) {
//                // This method is called once with the initial value and again
//                // whenever data at this location is updated.
//                String value = dataSnapshot.getValue(String.class);
//                Log.d(TAG, "Value is: " + value);
//            }
//
//            @Override
//            public void onCancelled(DatabaseError error) {
//                // Failed to read value
//                Log.w(TAG, "Failed to read value.", error.toException());
//            }
//        });
        this.activity = this;
        sharedPref = this.getSharedPreferences(this.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        sharedPref.registerOnSharedPreferenceChangeListener(mListener);
        intentFilter = new IntentFilter(LocationManager.PROVIDERS_CHANGED_ACTION);
        networkBroadcastReceiver = new NetworkBroadcastReceiver();
        registerReceiver(networkBroadcastReceiver,new IntentFilter(ConnectivityManager.CONNECTIVITY_ACTION));
        registerReceiver(gpsReceiver,intentFilter);
        String key = getResources().getString(R.string.service);
        String androidId = Settings.Secure.getString(getContentResolver(),Settings.Secure.ANDROID_ID);
        System.out.println("androidId => " + androidId );
        Bundle params = new Bundle();
        params.putString("id",androidId);
        mFirebaseAnalytics.logEvent("device_id", params);
        widgetService = new Intent(MainActivity.this, WidgetService.class);
        new GetGAIDTask().execute();
        // if(key.equals("becknuser")){
        //     //            FacebookSdk.sdkInitialize(getApplicationContext());
        //     //            AppEventsLogger.activateApp(getApplication());
        //                 FacebookSdk.setApplicationId("192510696087613");
        //                 FacebookSdk.setAutoInitEnabled(true);
        //                 FacebookSdk.fullyInitialize();
        //                 AppLinkData.fetchDeferredAppLinkData(this,new AppLinkData.CompletionHandler() {
        //                     @Override
        //                     public void onDeferredAppLinkDataFetched(AppLinkData appLinkData) {
        //                     }
        //                 }
        //                 );
        //             }
        FirebaseDynamicLinks.getInstance()
                .getDynamicLink(getIntent())
                .addOnSuccessListener(this, new OnSuccessListener<PendingDynamicLinkData>() {
                    @Override
                    public void onSuccess(PendingDynamicLinkData pendingDynamicLinkData) {
                        // Get deep link from result (may be null if no link is found)
                        Uri deepLink = null;
                        if (pendingDynamicLinkData != null) {
                            deepLink = pendingDynamicLinkData.getLink();
                        }
                    }
                })
                .addOnFailureListener(this, new OnFailureListener() {
                    @Override
                    public void onFailure(@NonNull Exception e) {
                        Log.w(TAG, "getDynamicLink:onFailure", e);
                    }
                });


        WebView.setWebContentsDebuggingEnabled(true);
        setContentView(R.layout.activity_main);
        Intent intent = getIntent();
        String action = intent.getAction();
        Uri data = intent.getData();
        System.out.println("HERE IN LOGGING Ad action =>" + action);
        System.out.println("HERE IN LOGGING Ad data =>" + data);
        webView = findViewById(R.id.webView);
        webView.getSettings().setJavaScriptEnabled(true);
        webView.getSettings().setLoadWithOverviewMode(true);
        webView.getSettings().setUseWideViewPort(true);
        if (key != null && key.equals("nammayatripartner")) {
            getWindow().addFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON);
            CommonJsInterface.updateLocaleResource(sharedPref.getString(getResources().getString(R.string.LANGUAGE_KEY), "null"));
        } else {
            LottieAnimationView splashLottieView = findViewById(R.id.splash_lottie);
            try {
                if (Settings.Global.getFloat(getContentResolver(), Settings.Global.ANIMATOR_DURATION_SCALE) == 0f){
                    isSystemAnimEnabled = false;
                }else {
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
//        mobileNetworkCheck();
        appUpdateManager = AppUpdateManagerFactory.create(this);
        // Returns an intent object that you use to check for an update.
        Task<AppUpdateInfo> appUpdateInfoTask = appUpdateManager.getAppUpdateInfo();
        updateType = AppUpdateType.IMMEDIATE;
        appUpdateInfoTask.addOnSuccessListener(appUpdateInfo -> {
            if (appUpdateInfo.updateAvailability() == UpdateAvailability.UPDATE_AVAILABLE
                    && appUpdateInfo.isUpdateTypeAllowed(updateType)) {
                Log.d(TAG, "Inside update");
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
                Log.d(TAG, "Update available");
            } else {
                Log.d(TAG, "No Update available");
            }
        });
        updateConfigURL();
        initApp();

       NotificationUtils.createNotificationChannel(this, NotificationUtils.CHANNEL_ID);
       NotificationUtils.createNotificationChannel(this, NotificationUtils.FLOATING_NOTIFICATION);
       NotificationUtils.createNotificationChannel(this, NotificationUtils.RINGING_CHANNEL_ID);
       NotificationUtils.createNotificationChannel(this, NotificationUtils.TRIP_CHANNEL_ID);
       NotificationUtils.createNotificationChannel(this, NotificationUtils.CANCELLED_PRODUCT);
       NotificationUtils.createNotificationChannel(this, NotificationUtils.DRIVER_HAS_REACHED);
       this.notificationCallback = new NotificationUtils.NotificationCallback() {
           @Override
           public void triggerPop(String id, String type) {
               MainActivity.this.triggerPopUP(id, type);
           }
           @Override
           public void triggerPopUp(String id, String type) {
               System.out.println("Calling function triggerPopUp from main to main");
               MainActivity.this.triggerPopUPMain(id, type);
           }
           @Override
           public void triggerAllocationPopUp(String id, String type, JSONObject entity_payload) {
//               Removed Purescript RideRequestPopUp Call
//               MainActivity.this.triggerAllocationPopUpMain(id, type, entity_payload);
           }
           @Override
           public void callFlowCustomer(String notificationType) {
               MainActivity.this.callingFlowCustomer(notificationType);
           }
       };
       NotificationUtils.registerCallback(this.notificationCallback);
       this.timeUpdateCallback = new LocationUpdateService.UpdateTimeCallback() {
           @Override
           public void timeUpdateFlow(String time, String lat, String lng) {
               MainActivity.this.timeUpdateDriverFlow(time, lat, lng);
           }
       };
       LocationUpdateService.registerCallback(this.timeUpdateCallback);
       if (BuildConfig.DEBUG) {
           FirebaseMessaging.getInstance().subscribeToTopic("test");
       }
       if (Build.VERSION.SDK_INT >= 21) {
            Window window = this.getWindow();
            window.addFlags(WindowManager.LayoutParams.FLAG_DRAWS_SYSTEM_BAR_BACKGROUNDS);
            window.clearFlags(WindowManager.LayoutParams.FLAG_TRANSLUCENT_STATUS);
            window.setStatusBarColor(this.getResources().getColor(R.color.colorPrimaryDark));
       }
        countAppUsageDays();
    }

    public void triggerPopUP(String id, String type) {
        try {
            if (juspayServicesGlobal.getDynamicUI() != null){
                CommonJsInterface.callingStoreCall(juspayServicesGlobal.getDuiCallback(), type);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void updateConfigURL() {
        String merchantId = getResources().getString(R.string.service).equals("nammayatri") ? in.juspay.mobility.BuildConfig.MERCHANT_ID_USER : in.juspay.mobility.BuildConfig.MERCHANT_ID_DRIVER;
        String baseUrl = getResources().getString(R.string.service).equals("nammayatri") ? in.juspay.mobility.BuildConfig.CONFIG_URL_USER : in.juspay.mobility.BuildConfig.CONFIG_URL_DRIVER;
        SharedPreferences sharedPreff = this.getSharedPreferences(
                activity.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        SharedPreferences.Editor editor = sharedPreff.edit();
        editor.putString("MERCHANT_ID", merchantId);
        editor.putString("BASE_URL", baseUrl);
        editor.apply();
    }

    public void triggerPopUPMain(String id, String type) {

        try {
            System.out.println("Calling function triggerPopUP1 from main to main try");
//            CommonJsInterface.callingStoreCall(juspayServicesGlobal.getDuiCallback()());
                hyperServices.process(new JSONObject().put("service", "in.juspay." + getResources().getString(R.string.service)).put("requestId", UUID.randomUUID()).put("payload", new JSONObject().put("action", "showPopup").put("id", id).put("popType", type)));
            System.out.println("Calling function triggerPopUP1 from main to main try after");
        } catch (Exception e) {
            System.out.println("Calling function triggerPopUP1 from main to main catch : " + e);
            e.printStackTrace();
        }
    }

    public void triggerAllocationPopUpMain(String id, String type, JSONObject entity_payload) {
        try {
            if (mediaPlayer == null) {
                System.out.println("playing from media player");
                mediaPlayer = MediaPlayer.create(getApplicationContext(), R.raw.allocation_request);
                mediaPlayer.setLooping(true);
                mediaPlayer.start();
            }
            System.out.println("Calling function triggerAllocationPopUpMain from main to main try");
            if (CommonJsInterface.storeCallBackPopUp != null && juspayServicesGlobal.getDynamicUI() != null)
            {
                CommonJsInterface.callingStoreCallBackPopUp(juspayServicesGlobal.getDuiCallback(), entity_payload);
            }else {
                hyperServices.process(new JSONObject().put("service", "in.juspay." + getResources().getString(R.string.service)).put("requestId", UUID.randomUUID()).put("payload", new JSONObject().put("action", "showPopup").put("id", id).put("popType", type).put("entityPayload", entity_payload)));
                System.out.println("Calling function triggerAllocationPopUpMain from main to main try after");
            }
        } catch (Exception e) {
            System.out.println("Calling function triggerAllocationPopUpMain from main to main catch : " + e);
            e.printStackTrace();
        }
    }

    public void stopAllocationNotification() {
        if (mediaPlayer != null) {
            mediaPlayer.stop();
            mediaPlayer = null;
        }
    }

    public void timeUpdateDriverFlow (String time, String lat, String lng){
        try {
            if (juspayServicesGlobal.getDynamicUI() != null){
                CommonJsInterface.callingStoreCallBackTime(juspayServicesGlobal.getDuiCallback(), time, lat, lng, context);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void callingFlowCustomer(String notificationType) {
        try {
            if (juspayServicesGlobal.getDynamicUI() != null){
                CommonJsInterface.callingStoreCallCustomer(juspayServicesGlobal.getDuiCallback(), notificationType);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void callingDriverLocationPermission() {
        boolean locationPermission = !(ActivityCompat.checkSelfPermission(this, Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED && ActivityCompat.checkSelfPermission(this, Manifest.permission.ACCESS_COARSE_LOCATION) != PackageManager.PERMISSION_GRANTED);
        LocationManager locationManager = (LocationManager) this.getSystemService(Context.LOCATION_SERVICE);
        boolean locationEnabled = locationManager != null && LocationManagerCompat.isLocationEnabled(locationManager);
        System.out.println("Inside callingDriverLocationPermission mainActivity locationPermission "+ locationPermission + " locationEnabled " + locationEnabled);
        String permissionEnabled = new String(String.valueOf(locationPermission && locationEnabled));
        try {
            if (juspayServicesGlobal.getDynamicUI() != null){
                CommonJsInterface.callingStoreCallBackDriverLocationPermission(juspayServicesGlobal.getDuiCallback(), permissionEnabled);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void callingOverlayPermission() {
        Handler handler = new Handler();
        handler.postDelayed(new Runnable() {
            @Override
            public void run() {
                if(Settings.canDrawOverlays(MainActivity.this)){
                    try {
                        if (juspayServicesGlobal.getDynamicUI() != null){
                            CommonJsInterface.callingStoreCallBackOverlayPermission(juspayServicesGlobal.getDuiCallback(), "true");
                        }
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }
            }
        }, 500);

    }

    public void callingCheckBatteryOptimization() {
        Handler handler = new Handler();
        handler.postDelayed(new Runnable() {
            @Override
            public void run() {
                PowerManager powerManager = (PowerManager) MainActivity.this.getSystemService(Context.POWER_SERVICE);
                if (powerManager.isIgnoringBatteryOptimizations(MainActivity.this.getPackageName())){
                    try {
                        if (juspayServicesGlobal.getDynamicUI() != null){
                            CommonJsInterface.callingStoreCallBackBatteryUsagePermission(juspayServicesGlobal.getDuiCallback(), "true");
                        }
                    }catch (Exception e){
                        e.printStackTrace();
                    }
                }
            }
        }, 900);
    }

    private void initApp() {

        hyperServices = new HyperServices(this, (ViewGroup) findViewById(R.id.cl_dui_container));
        hyperServices.attachDUIInterface(new JsInterface());
        final JSONObject json = new JSONObject();
        JSONObject payload = new JSONObject();

        try {
            String key = "in.juspay." + getResources().getString(R.string.service);
            json.put("requestId", "123");
            json.put("service", key);
            json.put("betaAssets", false);
            payload.put("clientId", getResources().getString(R.string.client_id));
            payload.put("action", "initiate");
            payload.put("service", key);
            payload.put(PaymentConstants.ENV, "prod");

            json.put(PaymentConstants.PAYLOAD, payload);
        } catch (JSONException e) {
            e.printStackTrace();
        }
//        try {
//            String key = "net.openkochi." + getResources().getString(R.string.service);
//            json.put("requestId", "123");
//            json.put("service", key);
//            json.put("betaAssets", false);
//            payload.put("clientId", "open-kochi");
//            payload.put("action", "initiate");
//            payload.put("service", key);
//            payload.put(PaymentConstants.ENV, "prod");
//
//            json.put(PaymentConstants.PAYLOAD, payload);
//        } catch (JSONException e) {
//            e.printStackTrace();
//        }

        hyperServices.initiate(json, new HyperPaymentsCallbackAdapter() {
            @Override
            public void onEvent(JSONObject jsonObject, JuspayResponseHandler juspayResponseHandler) {
                Log.d(TAG, "onEvent: " + jsonObject.toString());
                final JuspayServices juspayServices = hyperServices.getJuspayServices();
                juspayServicesGlobal = juspayServices;
                final DuiCallback dynamicUI = juspayServices.getDuiCallback();
                dynamicUI.addJsToWebView("document.title = 'Beckn App';");
                System.out.println("==-NOTIFICATION_DATA-->" + jsonObject.optString("event"));/**/

                if (jsonObject.optString("event").equals("initiate_result")) {
                    System.out.println();
                    System.out.println("Hello" + getIntent().toString());
                    System.out.println("==-NOTIFICATION_DATA-->" + getIntent().hasExtra("NOTIFICATION_DATA"));/**/
                    System.out.println("==--notificationType->" + (getIntent().hasExtra("notification_type")));/**/
                    System.out.println("==-entityIds-->" + getIntent().hasExtra("entity_ids"));/**/
                    System.out.println("==-entityType-->" + getIntent().hasExtra("entity_type"));/**/
                    //Getting Notification Data from Intent and attaching to process payload
                    if (getIntent().hasExtra("NOTIFICATION_DATA") || (getIntent().hasExtra("notification_type") && getIntent().hasExtra("entity_ids") && getIntent().hasExtra("entity_type"))) {
                        try {
                            System.out.println("It has entered if statment");
                            JSONObject payload1 = json.getJSONObject(PaymentConstants.PAYLOAD);
                            payload1.put("action", "process");
                            payload1.put("notificationData", getNotificationDataFromIntent());
                            json.put(PaymentConstants.PAYLOAD, payload1);
                            System.out.println("Payment Constants" + PaymentConstants.PAYLOAD);
                        } catch (JSONException e) {
                            Log.e("NOTIFICATIONDATA", e.toString());
                        }
                    }
                    Log.e("json_payload", json.toString());
                    System.out.println("Json Data" + json.toString());
                    hyperServices.process(json);
                } else if (jsonObject.optString("event").equals("hide_splash")) {
                    String key = getResources().getString(R.string.service);
                    if (key != null && key.equals("nammayatri") && isSystemAnimEnabled) {
                            isHideSplashEventCalled = true;
                    } else {
                        hideSplash();
                    }
                } else if (jsonObject.optString("event").equals("show_splash")) {
                    View v = findViewById(R.id.splash);
                    if (v != null) {
                        findViewById(R.id.splash).setVisibility(View.VISIBLE);
                    }
                } else if (jsonObject.optString("event").equals("reboot")){
                    // showAlertForUpdate();
                    System.out.println("event reboot");
                    hyperServices.terminate();
                    hyperServices = null;
                    initApp();
                } else if(jsonObject.optString("event").equals("in_app_notification")){
                    showInAppNotifiation(jsonObject.optString("title") , jsonObject.optString("message"));
                }
            }
        });
    }

    public static void restart(Context context){
    Intent mainIntent = IntentCompat.makeMainSelectorActivity(Intent.ACTION_MAIN, Intent.CATEGORY_LAUNCHER);
    mainIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
    context.getApplicationContext().startActivity(mainIntent);
    System.exit(0);
}

    public void showAlertForUpdate() {
        System.out.println("inside showAlertForUpdate");
        AlertDialog.Builder builder = new AlertDialog.Builder(MainActivity.this);
        builder.setCancelable(false);
        ConstraintLayout constraintLayout = (ConstraintLayout) getLayoutInflater().inflate(R.layout.dynamic_update_loader, null);
        CardView cardView = constraintLayout.findViewById(R.id.apiLoaderOverlayCard);
        cardView.setCardElevation(0);
        cardView.setRadius(0);

        ViewGroup.LayoutParams layoutParams= new ConstraintLayout.LayoutParams(ConstraintLayout.LayoutParams.MATCH_PARENT,ConstraintLayout.LayoutParams.WRAP_CONTENT);
        constraintLayout.setLayoutParams(layoutParams);
        builder.setView(constraintLayout);
        builder.setPositiveButton(R.string.okay_got_it, new DialogInterface.OnClickListener() {
            public void onClick(DialogInterface dialog, int which) {
                dialog.cancel();
                hyperServices.terminate();
                hyperServices = null;
                initApp();
            }
        });
        runOnUiThread(new Runnable()
        {
            @Override
            public void run()
            {
                AlertDialog alertDialog = builder.create();
                alertDialog.show();
            }
        });
//        builder.show();
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

    public native String getPayloadSignature(String payload);


    public NetworkInfo getInfo() {
        return ((ConnectivityManager) getApplicationContext().getSystemService(Context.CONNECTIVITY_SERVICE)).getActiveNetworkInfo();
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
            } catch (IOException e) {
                e.printStackTrace();
            } catch (GooglePlayServicesNotAvailableException e) {
                e.printStackTrace();
            } catch (GooglePlayServicesRepairableException e) {
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
            mFirebaseAnalytics.logEvent("ad_id", params);
        }
    }

    @Override
    protected void onNewIntent(Intent intent) {
        Intent pendingIntent = intent;
        if (pendingIntent != null && pendingIntent.hasExtra("NOTIFICATION_DATA")) {
            String data = pendingIntent.getExtras().getString("NOTIFICATION_DATA");
            try {
                JSONObject jsonData = new JSONObject(data);
                if (jsonData.has("notification_type") && jsonData.has("entity_ids")) {
                    String id = jsonData.getString("entity_ids");
                    String type = jsonData.getString("notification_type");
                    if (type.equals("NEW_MESSAGE")) {
                        hyperServices.process(new JSONObject().put("service", "in.juspay." + getResources().getString(R.string.service)).put("requestId", UUID.randomUUID()).put("payload", new JSONObject().put("action", "callDriverAlert").put("id", id).put("popType", type)));
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
        sharedPref.edit().putString(getResources().getString(R.string.ACTIVITY_STATUS),"onResume").apply();
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
        MainActivity.this.callingDriverLocationPermission();
        MainActivity.this.callingOverlayPermission();
        MainActivity.this.callingCheckBatteryOptimization();
        try {
            if ( juspayServicesGlobal != null && juspayServicesGlobal.getDynamicUI() != null){
                CommonJsInterface.callOnResumeUpdateCallback(juspayServicesGlobal.getDuiCallback());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        if (stateMonitor != null){
            final SdkTracker sdkTracker = this.hyperServices.getJuspayServices().getSdkTracker();
            new Thread(new Runnable() {
                @Override
                public void run() {
                    String key = "in.juspay." + getResources().getString(R.string.service);
                    sdkTracker.trackLifecycle(PaymentConstants.SubCategory.LifeCycle.ANDROID, PaymentConstants.LogLevel.INFO, Labels.Android.ON_RESUME, "class", key);
                }
            }).start();
            stateMonitor.enable(this);
        }
        if (getResources().getString(R.string.service).equals("nammayatripartner")){
            if (NotificationUtils.overlayFeatureNotAvailable(this)){
                checkRideRequest();
            }
            if (widgetService!=null){
                stopService(widgetService);
            }
        }
    }

    @Override
    protected void onPause() {
        super.onPause();
        sharedPref.edit().putString(getResources().getString(R.string.ACTIVITY_STATUS),"onPause").apply();
        DefaultMediaPlayerControl.mediaPlayer.pause();
        for (MediaPlayerView audioPlayer : CommonJsInterface.audioPlayers) {
            audioPlayer.onPause(audioPlayer.getPlayer());
        }
        if (stateMonitor != null){
            final SdkTracker sdkTracker = this.hyperServices.getJuspayServices().getSdkTracker();
            new Thread(new Runnable() {
                @Override
                public void run() {
                    String key = "in.juspay." + getResources().getString(R.string.service);
                    sdkTracker.trackLifecycle(PaymentConstants.SubCategory.LifeCycle.ANDROID, PaymentConstants.LogLevel.INFO, Labels.Android.ON_PAUSE, "class", key);
                }
            }).start();
            stateMonitor.disable(this);
        }
        if (getResources().getString(R.string.service).equals("nammayatripartner") && widgetService != null && Settings.canDrawOverlays(this)  && !sharedPref.getString(getResources().getString(R.string.REGISTERATION_TOKEN), "null").equals("null")) {
            startService(widgetService);
        }
    }

    @Override
    protected void onDestroy() {
        sharedPref.edit().putString(getResources().getString(R.string.ACTIVITY_STATUS),"onDestroy").apply();
        String role = sharedPref.getString("ROLE_KEY", "null");
        String location_status = sharedPref.getString("LOCATION_STATUS", "PAUSE");
        System.out.println("Outside onDestroy Driver" + role);
       if (role.equals("DRIVER") && location_status.equals("START") && !(ActivityCompat.checkSelfPermission(getApplicationContext(), Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED && ActivityCompat.checkSelfPermission(getApplicationContext(), Manifest.permission.ACCESS_COARSE_LOCATION) != PackageManager.PERMISSION_GRANTED)) {
           System.out.println("Inside onDestroy Driver" + role);
           Intent broadcastIntent = new Intent();
           broadcastIntent.setAction("restartservice");
           broadcastIntent.setClass(this, BootUpReceiver.class);
           this.sendBroadcast(broadcastIntent);
       }
        if (hyperServices != null) {
            JuspayServices juspayServices = this.hyperServices.getJuspayServices();
            if (juspayServices!= null){
                final SdkTracker sdkTracker = juspayServices.getSdkTracker();

            new Thread(new Runnable() {
                @Override
                public void run() {
                    String key = "in.juspay." + getResources().getString(R.string.service);
                    sdkTracker.trackLifecycle(PaymentConstants.SubCategory.LifeCycle.ANDROID, PaymentConstants.LogLevel.INFO, Labels.Android.ON_DESTROY, "class", key);
                }
            }).start();
            }
            hyperServices.terminate();
        }
        NotificationUtils.deRegisterCallback(this.notificationCallback);
        LocationUpdateService.deRegisterCallback(this.timeUpdateCallback);
        unregisterReceiver(gpsReceiver);
        unregisterReceiver(networkBroadcastReceiver);
        DefaultMediaPlayerControl.mediaPlayer.reset();
        super.onDestroy();
    }

    public HyperServices getHyperServices() {
        return hyperServices;
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, @Nullable Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
        switch (requestCode) {
            case IMAGE_CAPTURE_REQ_CODE:
                if (resultCode == RESULT_OK) {
                    captureImage (data);
                }
                break;
            case REQUEST_CODE_UPDATE_APP:
                if (resultCode != RESULT_OK) {
                    Log.i(TAG,"Update flow failed! Result code: " + resultCode);
                    if(updateType == AppUpdateType.IMMEDIATE){
                        finishAndRemoveTask();
                    }
                }
                break;
            case CropImage.CROP_IMAGE_ACTIVITY_REQUEST_CODE:
                if (resultCode == RESULT_OK) {
                    new Thread(new Runnable() {
                        @Override
                        public void run() {
                            encodeImageToBase64 (data);
                        }
                    }).start();
                } else if (resultCode == CropImage.CROP_IMAGE_ACTIVITY_RESULT_ERROR_CODE) {
                    CropImage.ActivityResult result = CropImage.getActivityResult(data);
                    Log.e(TAG,result.getError().toString());
                }
                break;
            default:return;
        }
    }

  @Override
  public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
      super.onRequestPermissionsResult(requestCode, permissions, grantResults);
      switch (requestCode) {
          case IMAGE_PERMISSION_REQ_CODE :
              if ((ActivityCompat.checkSelfPermission(this, WRITE_EXTERNAL_STORAGE) == PackageManager.PERMISSION_GRANTED) && (ActivityCompat.checkSelfPermission(this, CAMERA) == PackageManager.PERMISSION_GRANTED) && (ActivityCompat.checkSelfPermission(this, READ_EXTERNAL_STORAGE) == PackageManager.PERMISSION_GRANTED)){
                  Intent takePicture = new Intent(MediaStore.ACTION_IMAGE_CAPTURE);
                  String timeStamp = new SimpleDateFormat("yyyyMMdd_HHmmss", Locale.getDefault()).format(new Date());
                  sharedPref.edit().putString(getResources().getString(R.string.TIME_STAMP_FILE_UPLOAD), timeStamp).apply();
                  Uri photoFile = FileProvider.getUriForFile(this.getApplicationContext(),this.getResources().getString(R.string.fileProviderPath), new File(this.getApplicationContext().getFilesDir(), "IMG_" + timeStamp+".jpg"));
                  takePicture.putExtra(MediaStore.EXTRA_OUTPUT, photoFile);
                  Intent chooseFromFile = new Intent(Intent.ACTION_GET_CONTENT);
                  chooseFromFile.setType("image/*");
                  Intent chooser = Intent.createChooser(takePicture, getString(R.string.upload_image));
                  chooser.putExtra(Intent.EXTRA_INITIAL_INTENTS, new Intent[] { chooseFromFile });
                  this.startActivityForResult(chooser,IMAGE_CAPTURE_REQ_CODE);
              } else {
                  Toast.makeText(this, getString(R.string.please_allow_permission_to_capture_the_image), Toast.LENGTH_SHORT).show();
              }
              break;
          case CommonJsInterface.REQUEST_CALL :
              if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                  Intent intent = new Intent(Intent.ACTION_CALL,Uri.parse("tel:"+CommonJsInterface.phoneNumber));
                  this.startActivity(intent);
              }else{
                  enablePermissionFromSettings(Manifest.permission.CALL_PHONE, "Phone");
              }
              break;
          case CommonJsInterface.LOCATION_PERMISSION_REQ_CODE:
              if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                  System.out.println("Location Permission Granted");
              }else{
                  enablePermissionFromSettings(Manifest.permission.ACCESS_FINE_LOCATION, "Location");
              }
              break;
          case CommonJsInterface.STORAGE_PERMISSION:
              if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                  try {
                      CommonJsInterface.downloadPDF(CommonJsInterface.invoice , (Activity) this,this);
                  } catch (JSONException e) {
                      e.printStackTrace();
                  }
              }else {
                  Toast.makeText(this, "Permission Denied", Toast.LENGTH_SHORT).show();
              }
              break;
          case CommonJsInterface.REQUEST_CONTACTS:
              boolean flag = ContextCompat.checkSelfPermission(MainActivity.getInstance(), Manifest.permission.READ_CONTACTS) == PackageManager.PERMISSION_GRANTED;
              String contacts = null;
              try {
                  if (flag){
                      contacts = getPhoneContacts();
                  } else {
                      JSONArray flagArray = new JSONArray();
                      contacts = flagArray.toString();
                  }
                  if (juspayServicesGlobal.getDynamicUI() != null) {
                      CommonJsInterface.contactsStoreCall(juspayServicesGlobal.getDuiCallback(), contacts);
                  }
              } catch (JSONException e) {
                  e.printStackTrace();
              }
              break;
          default: return;
      }
  }

    public String getPhoneContacts() throws JSONException {
        ContentResolver contentResolver = getContentResolver();
        Uri uri = ContactsContract.CommonDataKinds.Phone.CONTENT_URI;
        Cursor cursor = contentResolver.query(uri,null,null,null,null);

        JSONArray contacts = new JSONArray();

        if(cursor.getCount()>0){
            while(cursor.moveToNext()){
                String contactNameStr = cursor.getString(cursor.getColumnIndexOrThrow(ContactsContract.CommonDataKinds.Phone.DISPLAY_NAME));
                String contactStr = cursor.getString(cursor.getColumnIndexOrThrow(ContactsContract.CommonDataKinds.Phone.NUMBER));
                String contactNumber = contactStr.replaceAll("[^0-9]", "");
                String contactName = contactNameStr.replaceAll("'","");
                JSONObject tempPoints = new JSONObject();
                tempPoints.put("name",contactName);
                tempPoints.put("number",contactNumber);
                contacts.put(tempPoints);
            }
        }

        JSONObject flagObject = new JSONObject();
        flagObject.put("name","beckn_contacts_flag");
        flagObject.put("number","true");
        contacts.put(flagObject);
        System.out.print("Contacts " + contacts);
        return contacts.toString();
    }

    public void firstTimeAskingPermission(Context context, String permission){
        SharedPreferences sharedPreference = context.getSharedPreferences(activity.getString(R.string.preference_file_key), MODE_PRIVATE);
        sharedPreference.edit().putString(permission, "false").apply();
    }

    public String isFirstTimeAskingPermission(Context context, String permission){
        return context.getSharedPreferences(activity.getString(R.string.preference_file_key), MODE_PRIVATE).getString(permission, "true");
    }

    public void enablePermissionFromSettings(@NonNull String permission, String permissionName){
//        if(!isFirstTimeAskingPermission(this, Manifest.permission.ACCESS_FINE_LOCATION))

        if (ActivityCompat.shouldShowRequestPermissionRationale(this, permission)){
            firstTimeAskingPermission(this, permission);
        }else{
            if(isFirstTimeAskingPermission(this, permission).equals("false")){
                try {
                    LayoutInflater inflater = (this).getLayoutInflater();
                    View permissionStepsView = ((LayoutInflater) this.getSystemService(Context.LAYOUT_INFLATER_SERVICE)).inflate(R.layout.permission_steps_layout, null);
                    TextView stepText = permissionStepsView.findViewById(R.id.step_text);

                    AlertDialog.Builder builder = new AlertDialog.Builder(this);
                    stepText.setText("3. Tap on "+permissionName);
                    builder.setTitle("Permission Required")
                            .setCancelable(true)
                            .setView(permissionStepsView)
                            .setPositiveButton("Go to settings", new DialogInterface.OnClickListener() {
                                public void onClick(DialogInterface dialog, int which) {
                                    Intent settingsIntent = new Intent(Settings.ACTION_APPLICATION_DETAILS_SETTINGS);
                                    Uri uri = Uri.fromParts("package", getPackageName(), null);
                                    settingsIntent.setData(uri);
                                    startActivity(settingsIntent);
                                }
                            });
                    AlertDialog alert = builder.create();
                    alert.show();
                } catch (Exception e) {
                    Log.d("error", e.toString());
                }
            }
        }
    }

  private String getImageName(Uri uri){
      Cursor returnCursor = getContentResolver().query(uri, null, null, null, null);
      int nameIndex = returnCursor.getColumnIndex(OpenableColumns.DISPLAY_NAME);
      returnCursor.moveToFirst();
      return returnCursor.getString(nameIndex);
  }

  private long getImageSizeKB(Uri uri){
      Cursor returnCursor = getContentResolver().query(uri, null, null, null, null);
      int sizeIndex = returnCursor.getColumnIndex(OpenableColumns.SIZE);
      returnCursor.moveToFirst();
      return returnCursor.getLong(sizeIndex)/1000;
  }

  public JuspayServices getJuspayServices(){
        return juspayServicesGlobal;
  }

  public void hideSplash (){
      View v = findViewById(R.id.cl_dui_container);
      if (v != null) {
          findViewById(R.id.cl_dui_container).setVisibility(View.VISIBLE);
      }
      View splashView = findViewById(R.id.splash);
      if (splashView != null) {
          splashView.setVisibility(View.GONE);
      }
  }

    private void countAppUsageDays() {
        Date currentDate = new Date();
        SharedPreferences sharedPref = this.getSharedPreferences(this.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        long millis = sharedPref.getLong("PREVIOUS_USED_DATE", 0L);
        if (millis == 0L){
            sharedPref.edit().putLong("PREVIOUS_USED_DATE", currentDate.getTime()).apply();
        }
        Date previousDate = new Date(sharedPref.getLong("PREVIOUS_USED_DATE", 0L));
        if (TimeUnit.MILLISECONDS.toDays(currentDate.getTime() - previousDate.getTime()) > 0){
            // update days Count
            sharedPref.edit().putInt("DAYS_COUNT", sharedPref.getInt("DAYS_COUNT", 0)+1).apply();
            sharedPref.edit().putString("USED_DAYS_COUNT", String.valueOf(sharedPref.getInt("DAYS_COUNT", 0))).apply();
            // update previousDate to currentDate
            sharedPref.edit().putLong("PREVIOUS_USED_DATE", currentDate.getTime()).apply();
        }
    }

    private void captureImage (@Nullable Intent data) {
        try {
            Uri imageUri;
            if (data == null || data.getData() == null) { //Camera
                File image = new File(this.getApplicationContext().getFilesDir(), "IMG_" + sharedPref.getString(getResources().getString(R.string.TIME_STAMP_FILE_UPLOAD), "null") + ".jpg");
                imageUri = FileProvider.getUriForFile(this.getApplicationContext(), this.getResources().getString(R.string.fileProviderPath), image);
            }
            else { // storage
                imageUri = data.getData();
            }
            startCropImageActivity(imageUri);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void startCropImageActivity(Uri imageUri){
        CropImage.activity(imageUri)
                .setAllowFlipping(false)
                .start(this);
    }

    private void encodeImageToBase64(@Nullable Intent data) {
        try {
            CropImage.ActivityResult result = CropImage.getActivityResult(data);
            Uri fileUri = result.getUri();
            InputStream imageStream = getContentResolver().openInputStream(fileUri);
            Bitmap selectedImage = BitmapFactory.decodeStream(imageStream);
            ByteArrayOutputStream baos = new ByteArrayOutputStream();

            byte[] b;
            String encImage;

            selectedImage.compress(Bitmap.CompressFormat.JPEG, 100, baos);
            b = baos.toByteArray();
            encImage = Base64.encodeToString(b, Base64.NO_WRAP);

            Log.d(TAG, "camera image size : " + String.valueOf((Math.ceil(encImage.length() / 4) * 3) / 1000));

            if ((Math.ceil(encImage.length() / 4) * 3) / 1000 > 400) {
                Integer reduceQuality = 10;
                selectedImage.compress(Bitmap.CompressFormat.JPEG, 100 - reduceQuality, baos);
                b = baos.toByteArray();
                encImage = Base64.encodeToString(b, Base64.NO_WRAP);
                while ((Math.ceil(encImage.length() / 4) * 3) / 1000 > 400) {
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

            Log.d(TAG, "encoded image size camera : " + String.valueOf((Math.ceil(encImage.length() / 4) * 3) / 1000));
            if (juspayServicesGlobal.getDynamicUI() != null) {
                String timeStamp = new SimpleDateFormat("yyyyMMdd_HHmmss", Locale.getDefault()).format(new Date());
                CommonJsInterface.callingStoreCallImageUpload(juspayServicesGlobal.getDuiCallback(), encImage, "IMG_" + timeStamp +".jpg");
            }
        }
        catch (Exception e){
            e.printStackTrace();
            Bundle params = new Bundle();
            mFirebaseAnalytics.logEvent("exception_crop_image", params);
        }
    }

    public  void showInAppNotifiation(String title , String desc){
        // getting the main Layout as a Container to add the notification .
        ConstraintLayout mainLayout = (ConstraintLayout) findViewById(R.id.main_layout);

        // inflating the app_notification as view to append in main layout .
        View notification= getLayoutInflater().inflate(R.layout.app_notification,null);
        notification.setLayoutParams(new ConstraintLayout.LayoutParams(ConstraintLayout.LayoutParams.WRAP_CONTENT,ConstraintLayout.LayoutParams.WRAP_CONTENT));
        notification.bringToFront();


        // adding animation to the notification
        notification.startAnimation(AnimationUtils.loadAnimation(this , R.anim.top_to_bottom));

        // setting the title and description to the notification
        TextView Title = notification.findViewById(R.id.title);
        TextView Desc = notification.findViewById(R.id.desc);
        Title.setText(title);
        Desc.setText(desc);

        // adding the evenListener to the cross button
        notification.findViewById(R.id.cross).setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                notification.startAnimation(AnimationUtils.loadAnimation(getApplicationContext() , R.anim.bottom_to_top));
                mainLayout.removeView(notification);
            }
        });

        // ring the notification bell
        try {
            Uri notify = RingtoneManager.getDefaultUri(RingtoneManager.TYPE_NOTIFICATION);
            Ringtone r = RingtoneManager.getRingtone(getApplicationContext(), notify);
            r.play();
        } catch (Exception e) {
            e.printStackTrace();
        }

        // adding the notification to the main layout
        mainLayout.addView(notification);


        // adding a timer task to automatically remove the notification after 5 second
        Timer timer;
        TimerTask timerTask;
        Handler timerHandler = new Handler();
        timer = new Timer();
        timerTask = new TimerTask() {
            public void run() {
                timerHandler.post(new Runnable() {
                    public void run(){
                        notification.startAnimation(AnimationUtils.loadAnimation(getApplicationContext() , R.anim.bottom_to_top));
                        mainLayout.removeView(notification);
                        if(timer != null){
                            timer.cancel();
                            timer.purge();
                        }
                    }
                });
            }
        };

        timer.schedule(timerTask, 5000);
    }

    private void checkRideRequest(){
        try {
            boolean rideReqExpired = NotificationUtils.lastRideReq.getBoolean("rideReqExpired", true);
            if (rideReqExpired) return;
            Intent rideReqActivity = new Intent(this, RideRequestActivity.class);
            rideReqActivity.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_REORDER_TO_FRONT);
            rideReqActivity.putExtras(NotificationUtils.lastRideReq);
            startActivity(rideReqActivity);
        }catch (Exception e){
            Log.e(TAG, "Exception in checkRideRequest");
        }
    }
}
