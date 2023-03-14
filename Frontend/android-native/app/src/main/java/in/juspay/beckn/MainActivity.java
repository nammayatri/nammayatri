package in.juspay.mobility;

import static android.Manifest.permission.CAMERA;
import static android.Manifest.permission.READ_EXTERNAL_STORAGE;
import static android.Manifest.permission.WRITE_EXTERNAL_STORAGE;
import android.Manifest;
import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.IntentSender;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.database.Cursor;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.location.LocationManager;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.PowerManager;
import android.provider.MediaStore;
import android.provider.OpenableColumns;
import android.provider.Settings;
import android.util.Base64;
import android.util.Log;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.view.WindowManager;
import android.view.inputmethod.InputMethodManager;
import android.webkit.WebView;
import android.widget.Toast;
import android.media.MediaPlayer;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.app.ActivityCompat;
import androidx.core.content.FileProvider;
import androidx.core.location.LocationManagerCompat;

import com.google.android.gms.ads.identifier.AdvertisingIdClient;
import com.google.android.gms.common.ConnectionResult;
import com.google.android.gms.common.GoogleApiAvailability;
import com.google.android.gms.common.GooglePlayServicesNotAvailableException;
import com.google.android.gms.common.GooglePlayServicesRepairableException;
import com.google.android.gms.common.api.Status;
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
import com.google.firebase.messaging.FirebaseMessaging;

import org.json.JSONException;
import org.json.JSONObject;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.UUID;

import in.juspay.mobility.utils.ConnectionStateMonitor;
import in.juspay.mobility.utils.LocationService;
import in.juspay.mobility.utils.LocationUpdateService;
import in.juspay.mobility.utils.NetworkBroadcastReceiver;
import in.juspay.mobility.utils.NotificationUtils;
import in.juspay.mobility.utils.WidgetService;
import in.juspay.hypersdk.BuildConfig;
import in.juspay.hypersdk.core.JuspayServices;
import in.juspay.hypersdk.core.Labels;
import in.juspay.hypersdk.core.PaymentConstants;
import in.juspay.hypersdk.core.SdkTracker;
import in.juspay.hypersdk.data.JuspayResponseHandler;
import in.juspay.hypersdk.ui.HyperPaymentsCallbackAdapter;
import in.juspay.mystique.DynamicUI;
import in.juspay.services.HyperServices;

//import com.facebook.LoggingBehavior;
//import com.facebook.applinks.AppLinkData;


public class MainActivity extends AppCompatActivity {

    private static final String TAG = "MainActivity";
    private static final int IMAGE_PERMISSION_REQ_CODE = 4997;
    private static final int IMAGE_CAPTURE_REQ_CODE = 101;
    private BecknServices hyperServices;
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
            System.out.println("sharedPreferences " + sharedPreferences + " key " + key);
            if (key.equals("REGISTERATION_TOKEN")){
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
        if (key.equals("nammayatripartner")) {
            getWindow().addFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON);
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
                            getResources().getInteger(R.integer.REQUEST_CODE_UPDATE_APP)
                    );
                } catch (IntentSender.SendIntentException e) {
                    e.printStackTrace();
                }
                Log.d(TAG, "Update available");
            } else {
                Log.d(TAG, "No Update available");
            }
        });
        initApp();

       NotificationUtils.createNotificationChannel(this, NotificationUtils.CHANNEL_ID);
       NotificationUtils.createNotificationChannel(this, NotificationUtils.FLOATING_NOTIFICATION);
       NotificationUtils.createNotificationChannel(this, NotificationUtils.RINGING_CHANNEL_ID);
       NotificationUtils.createNotificationChannel(this, NotificationUtils.TRIP_CHANNEL_ID);
       NotificationUtils.createNotificationChannel(this, NotificationUtils.CANCELLED_PRODUCT);
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
               System.out.println("Calling function triggerAllocationPopUp");
               MainActivity.this.triggerAllocationPopUpMain(id, type, entity_payload);
           }
           @Override
           public void callFlowCustomer(String notificationType) {
               MainActivity.this.callingFlowCustomer(notificationType);
           }
       };
       NotificationUtils.registerCallback(this.notificationCallback);
       this.timeUpdateCallback = new LocationUpdateService.UpdateTimeCallback() {
           @Override
           public void timeUpdateFlow(String time) {
               MainActivity.this.timeUpdateDriverFlow(time);
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
    }

    public void triggerPopUP(String id, String type) {

        try {
            CommonJsInterface.callingStoreCall(juspayServicesGlobal.getDynamicUI());
//            hyperServices.process(new JSONObject().put("service", "in.juspay." + getResources().getString(R.string.service)).put("requestId", UUID.randomUUID()).put("payload", new JSONObject().put("action", "showPopup").put("id", id).put("popType", type)));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void triggerPopUPMain(String id, String type) {

        try {
            System.out.println("Calling function triggerPopUP1 from main to main try");
//            CommonJsInterface.callingStoreCall(juspayServicesGlobal.getDynamicUI());
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
            if (CommonJsInterface.storeCallBackPopUp != null)
            {
                CommonJsInterface.callingStoreCallBackPopUp(juspayServicesGlobal.getDynamicUI(), entity_payload);
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

    public void timeUpdateDriverFlow (String time){
        try {
            CommonJsInterface.callingStoreCallBackTime(juspayServicesGlobal.getDynamicUI(), time);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void callingFlowCustomer(String notificationType) {

        try {
            CommonJsInterface.callingStoreCallCustomer(juspayServicesGlobal.getDynamicUI(), notificationType);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void callingDriverLocationPermission() {
        System.out.println("MainActivity callingDriverLocationPermission()");
        boolean locationPermission = !(ActivityCompat.checkSelfPermission(this, Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED && ActivityCompat.checkSelfPermission(this, Manifest.permission.ACCESS_COARSE_LOCATION) != PackageManager.PERMISSION_GRANTED);
        LocationManager locationManager = (LocationManager) this.getSystemService(Context.LOCATION_SERVICE);
        boolean locationEnabled = locationManager != null && LocationManagerCompat.isLocationEnabled(locationManager);
        System.out.println("Inside callingDriverLocationPermission mainActivity locationPermission "+ locationPermission + " locationEnabled " + locationEnabled);
        String permissionEnabled = new String(String.valueOf(locationPermission && locationEnabled));
        try {
            CommonJsInterface.callingStoreCallBackDriverLocationPermission(juspayServicesGlobal.getDynamicUI(),
                    permissionEnabled);
        } catch (Exception e) {
            System.out.println("Execption in callingDriverLocationPermission " + e);
            e.printStackTrace();
        }
    }

    public void callingOverlayPermission() {
        System.out.println("MainActivity callingOverlayPermission()");
        System.out.println("MainActivity callingOverlayPermission() " + Settings.canDrawOverlays(getApplicationContext()));
        String permissionEnabled = new String(String.valueOf(Settings.canDrawOverlays(getApplicationContext())));
        Handler handler = new Handler();
        handler.postDelayed(new Runnable() {
            @Override
            public void run() {
                if(!Settings.canDrawOverlays(MainActivity.this)){
                    try {
                        System.out.println("InisdeHandle runnable if");
                        CommonJsInterface.callingStoreCallBackOverlayPermission(juspayServicesGlobal.getDynamicUI(),
                                "false");
                    } catch (Exception e) {
                        System.out.println("Execption in callingDriverLocationPermission " + e);
                        e.printStackTrace();
                    }
                }
                else{
                    try {
                        System.out.println("InisdeHandle runnable if");
                        CommonJsInterface.callingStoreCallBackOverlayPermission(juspayServicesGlobal.getDynamicUI(),
                                "true");
                    } catch (Exception e) {
                        System.out.println("Execption in callingDriverLocationPermission " + e);
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
                if ((powerManager.isIgnoringBatteryOptimizations(MainActivity.this.getPackageName()))){
                    try {
                        CommonJsInterface.callingStoreCallBackBatteryUsagePermission(juspayServicesGlobal.getDynamicUI(), "true");
                    }catch (Exception e){
                        e.printStackTrace();
                    }
                }
            }
        }, 900);
    }

    private void initApp() {

        hyperServices = new BecknServices(this, (ViewGroup) findViewById(R.id.cl_dui_container));

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
                final DynamicUI dynamicUI = juspayServices.getDynamicUI();
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
                    View v = findViewById(R.id.splash);
                    if (v != null) {
                        findViewById(R.id.splash).setVisibility(View.GONE);
                    }
                } else if (jsonObject.optString("event").equals("show_splash")) {
                    View v = findViewById(R.id.splash);
                    if (v != null) {
                        findViewById(R.id.splash).setVisibility(View.VISIBLE);
                    }
                }
            }
        });
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
                            getResources().getInteger(R.integer.REQUEST_CODE_UPDATE_APP)
                    );
                } catch (IntentSender.SendIntentException e) {
                    e.printStackTrace();
                }
            }
        });
        System.out.println("Inside onResume mainActivity");
        MainActivity.this.callingDriverLocationPermission();
        MainActivity.this.callingOverlayPermission();
        MainActivity.this.callingCheckBatteryOptimization();
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
        sharedPref.registerOnSharedPreferenceChangeListener(mListener);
        if (widgetService != null && getResources().getString(R.string.service).equals("nammayatripartner")) {
            stopService(widgetService);
        }
    }

    @Override
    protected void onPause() {
        super.onPause();
        sharedPref.edit().putString(getResources().getString(R.string.ACTIVITY_STATUS),"onPause").apply();
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
        if (getResources().getString(R.string.service).equals("nammayatripartner") && widgetService != null && Settings.canDrawOverlays(this)  && !sharedPref.getString(getResources().getString(R.string.REGISTERATION_TOKEN), "null").equals("null") && sharedPref.getString(getResources().getString(R.string.IS_RIDE_ACTIVE), "null").equals("true")) {
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
            final SdkTracker sdkTracker = this.hyperServices.getJuspayServices().getSdkTracker();
            new Thread(new Runnable() {
                @Override
                public void run() {
                    String key = "in.juspay." + getResources().getString(R.string.service);
                    sdkTracker.trackLifecycle(PaymentConstants.SubCategory.LifeCycle.ANDROID, PaymentConstants.LogLevel.INFO, Labels.Android.ON_DESTROY, "class", key);
                }
            }).start();
            hyperServices.terminate();
        }
        NotificationUtils.deRegisterCallback(this.notificationCallback);
        LocationUpdateService.deRegisterCallback(this.timeUpdateCallback);
        unregisterReceiver(gpsReceiver);
        unregisterReceiver(networkBroadcastReceiver);
        super.onDestroy();
    }

    public HyperServices getHyperServices() {
        return hyperServices;
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, @Nullable Intent data) {
        System.out.println("onActivityResult MainActivity Request Code " + requestCode + " ResultCode " + resultCode + " IntentData " + data);
        if (requestCode == getResources().getInteger(R.integer.AUTOCOMPLETE_REQUEST_CODE)) {
            if (resultCode == RESULT_OK) {
                Place place = Autocomplete.getPlaceFromIntent(data);
                Intent payload = new Intent();
                payload.putExtra("id", place.getId());
                payload.putExtra("address", place.getAddress());
                payload.putExtra("name", place.getName());
                if(place.getLatLng() != null) {
                    payload.putExtra("lat", place.getLatLng().latitude);
                    payload.putExtra("lng", place.getLatLng().longitude);
                }
                hyperServices.onActivityResult(requestCode, resultCode, payload);
                Log.i(TAG, "Place: " + place.getName() + ", " + place.getId());
                // Hack for hiding Keyboard
                new Thread(){
                    @Override
                    public void run() {
                        super.run();
                        try {
                            Thread.sleep(50);
                            InputMethodManager imm = (InputMethodManager) MainActivity.this.getSystemService(Activity.INPUT_METHOD_SERVICE);
                            imm.hideSoftInputFromWindow(MainActivity.this.getWindow().getDecorView().getWindowToken(), 0);
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                    }
                }.start();
            } else if (resultCode == AutocompleteActivity.RESULT_ERROR) {
                Status status = Autocomplete.getStatusFromIntent(data);
                Log.i(TAG, status.getStatusMessage());
            }
        }
        else if (requestCode == IMAGE_CAPTURE_REQ_CODE){
            if (resultCode == RESULT_OK){
                imageBase64 = "";
                if (data==null || data.getData()==null)
                {  //Camera
                    try {
                        File image = new File(this.getApplicationContext().getFilesDir(), "IMG_"+sharedPref.getString(getResources().getString(R.string.TIME_STAMP_FILE_UPLOAD), "null")+".jpg");
                        Uri fileUri = FileProvider.getUriForFile(this.getApplicationContext(),this.getResources().getString(R.string.fileProviderPath), image);
                        InputStream imageStream = getContentResolver().openInputStream(fileUri);
                        Bitmap selectedImage = BitmapFactory.decodeStream(imageStream);
                        ByteArrayOutputStream baos = new ByteArrayOutputStream();
                        System.out.println("camera image size : " + String.valueOf(getImageSizeKB(fileUri)));
                        byte[] b;
                        String encImage;
                        if (getImageSizeKB(fileUri) > 2048){
                            Integer reduceQuality = 10;
                            selectedImage.compress(Bitmap.CompressFormat.JPEG,100-reduceQuality,baos);
                            b = baos.toByteArray();
                            encImage = Base64.encodeToString(b, Base64.NO_WRAP);
                            while ( (Math.ceil(encImage.length() / 4) * 3)/1000 > 2048 ){
                                if (reduceQuality >= 90) {
                                    break;
                                }
                                reduceQuality += 10;
                                baos.reset();
                                selectedImage.compress(Bitmap.CompressFormat.JPEG,100-reduceQuality,baos);
                                b = baos.toByteArray();
                                encImage = Base64.encodeToString(b, Base64.NO_WRAP);
                            }
                        }else{
                            selectedImage.compress(Bitmap.CompressFormat.JPEG,100,baos);
                            b = baos.toByteArray();
                            encImage = Base64.encodeToString(b, Base64.NO_WRAP);
                        }
                        System.out.println("encoded image size camera : " + String.valueOf((Math.ceil(encImage.length() / 4) * 3)/1000));
                        CommonJsInterface.callingStoreCallImageUpload(juspayServicesGlobal.getDynamicUI(), encImage, getImageName(fileUri));
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                } else {
                    //Storage
                    try {
                        Uri imageUri=data.getData();
                        final InputStream imageStream = getContentResolver().openInputStream(imageUri);
                        final Bitmap selectedImage = BitmapFactory.decodeStream(imageStream);
                        ByteArrayOutputStream baos = new ByteArrayOutputStream();
                        System.out.println("upload image size : " + String.valueOf(getImageSizeKB(imageUri)));
                        selectedImage.compress(Bitmap.CompressFormat.JPEG,100,baos);
                        byte[] b = baos.toByteArray();
                        String encImage = Base64.encodeToString(b, Base64.NO_WRAP);
                        if ( (Math.ceil(encImage.length() / 4) * 3)/1000 > 2048){
                            Integer reduceQuality = 10;
                            baos.reset();
                            selectedImage.compress(Bitmap.CompressFormat.JPEG,100-reduceQuality,baos);
                            b = baos.toByteArray();
                            encImage = Base64.encodeToString(b, Base64.NO_WRAP);
                            while ( (Math.ceil(encImage.length() / 4) * 3)/1000 > 2048 ){
                                if (reduceQuality >= 90) {
                                    break;
                                }
                                reduceQuality += 10;
                                baos.reset();
                                selectedImage.compress(Bitmap.CompressFormat.JPEG,100-reduceQuality,baos);
                                b = baos.toByteArray();
                                encImage = Base64.encodeToString(b, Base64.NO_WRAP);
                            }
                        }
                        System.out.println("encoded image size upload : " + String.valueOf((Math.ceil(encImage.length() / 4) * 3)/1000));
                        CommonJsInterface.callingStoreCallImageUpload(juspayServicesGlobal.getDynamicUI(), encImage, getImageName(imageUri));
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }
            }
        }
        else {
            super.onActivityResult(requestCode, resultCode, data);
        }
        if (requestCode == getResources().getInteger(R.integer.REQUEST_CODE_UPDATE_APP)) {
            if (resultCode != RESULT_OK) {
                Log.i(TAG,"Update flow failed! Result code: " + resultCode);
                if(updateType == AppUpdateType.IMMEDIATE){
                    finishAndRemoveTask();
                }
            }
        }
    }

  @Override
  public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
      super.onRequestPermissionsResult(requestCode, permissions, grantResults);
      if (requestCode == IMAGE_PERMISSION_REQ_CODE){
          if ((ActivityCompat.checkSelfPermission(this, WRITE_EXTERNAL_STORAGE) == PackageManager.PERMISSION_GRANTED) && (ActivityCompat.checkSelfPermission(this, CAMERA) == PackageManager.PERMISSION_GRANTED) && (ActivityCompat.checkSelfPermission(this, READ_EXTERNAL_STORAGE) == PackageManager.PERMISSION_GRANTED)){
              Intent takePicture = new Intent(MediaStore.ACTION_IMAGE_CAPTURE);
              String timeStamp = new SimpleDateFormat("yyyyMMdd_HHmmss", Locale.getDefault()).format(new Date());
              sharedPref.edit().putString(getResources().getString(R.string.TIME_STAMP_FILE_UPLOAD), timeStamp).apply();
              Uri photoFile = FileProvider.getUriForFile(this.getApplicationContext(),this.getResources().getString(R.string.fileProviderPath), new File(this.getApplicationContext().getFilesDir(), "IMG_" + timeStamp+".jpg"));
              takePicture.putExtra(MediaStore.EXTRA_OUTPUT, photoFile);
              Intent chooseFromFile = new Intent(Intent.ACTION_GET_CONTENT);
              chooseFromFile.setType("image/*");
              Intent chooser = Intent.createChooser(takePicture, "Upload Image");
              chooser.putExtra(Intent.EXTRA_INITIAL_INTENTS, new Intent[] { chooseFromFile });
              this.startActivityForResult(chooser,IMAGE_CAPTURE_REQ_CODE);
          } else {
              Toast.makeText(this, "Please allow permissions to capture the image", Toast.LENGTH_SHORT).show();
          }
      }
      if (requestCode == CommonJsInterface.REQUEST_CALL) {
            if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                Intent intent = new Intent(Intent.ACTION_CALL,Uri.parse("tel:"+CommonJsInterface.phoneNumber));
                this.startActivity(intent);
            }
        }
        if ((requestCode == 67)) {
            if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                try {
                    CommonJsInterface.downloadPDF(CommonJsInterface.invoice , (Activity) this,this);
                } catch (JSONException e) {
                    e.printStackTrace();
                }
            }else {
                Toast.makeText(this, "Permission Denied", Toast.LENGTH_SHORT).show();
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
  
}
