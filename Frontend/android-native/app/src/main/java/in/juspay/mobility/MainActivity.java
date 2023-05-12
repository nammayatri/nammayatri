/*
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility;

import android.Manifest;
import android.animation.Animator;
import android.annotation.SuppressLint;
import android.app.Activity;
import android.app.AlertDialog;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.IntentSender;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.location.LocationManager;
import android.media.Ringtone;
import android.media.RingtoneManager;
import android.net.ConnectivityManager;
import android.net.Network;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.provider.Settings;
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
import in.juspay.mobility.app.NetworkBroadcastReceiver;
import in.juspay.mobility.app.NotificationUtils;
import in.juspay.mobility.app.RideRequestActivity;
import in.juspay.mobility.app.Utils;
import in.juspay.mobility.app.WidgetService;
import in.juspay.services.HyperServices;


public class MainActivity extends AppCompatActivity {

    private static final String LOG_TAG = "MAIN_ACTIVITY";
    private static final int REQUEST_CODE_UPDATE_APP = 587;
    private static int updateType;
    @SuppressLint("StaticFieldLeak")
    NetworkBroadcastReceiver.ProcessCallBack processCallBack;
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
    BroadcastReceiver gpsReceiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            LocationManager locationManager = (LocationManager) context.getSystemService(Context.LOCATION_SERVICE);
            boolean isGpsEnabled = locationManager.isProviderEnabled(LocationManager.GPS_PROVIDER);
            String token = "null";
            if (sharedPref != null) {
                token = sharedPref.getString("REGISTERATION_TOKEN", "null");
            }
            if (!isGpsEnabled && !token.equals("null")) {
                triggerPopUPMain("true", "LOCATION_DISABLED");
            }
        }
    };
    private Intent widgetService;
    private AppUpdateManager appUpdateManager;
    private NetworkBroadcastReceiver networkBroadcastReceiver;
    private boolean isHideSplashEventCalled = false;
    private boolean isSystemAnimEnabled = true;

    @Override
    public void onBackPressed() {
        if (hyperServices != null && !hyperServices.onBackPressed()) {
            super.onBackPressed();
        }
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
        sharedPref.registerOnSharedPreferenceChangeListener(mListener);
        sharedPref.edit().putString(getResources().getString(in.juspay.mobility.app.R.string.ACTIVITY_STATUS), "onCreate").apply();
        IntentFilter intentFilter = new IntentFilter(LocationManager.PROVIDERS_CHANGED_ACTION);
        networkBroadcastReceiver = new NetworkBroadcastReceiver();
        registerReceiver(networkBroadcastReceiver, new IntentFilter(ConnectivityManager.CONNECTIVITY_ACTION));
        registerReceiver(gpsReceiver, intentFilter);
        String key = getResources().getString(R.string.service);
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
        if (key != null && key.equals("nammayatripartner")) {
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
        } else if (in.juspay.mobility.BuildConfig.MERCHANT_TYPE.equals("DRIVER")) {
            getWindow().addFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON);
            CommonJsInterface.updateLocaleResource(sharedPref.getString(getResources().getString(R.string.LANGUAGE_KEY), "null"));
        }
        processCallBack = this::triggerPopUPMain;
        NetworkBroadcastReceiver.registerProcessCallback(processCallBack);
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
        String key = in.juspay.mobility.BuildConfig.MERCHANT_TYPE;
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
            hyperServices.process(new JSONObject().put("service", "in.juspay.nammayatri").put("requestId", UUID.randomUUID()).put("payload", new JSONObject().put("action", "showPopup").put("id", id).put("popType", type)));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void initApp() {

        hyperServices = new HyperServices(this, findViewById(in.juspay.mobility.app.R.id.cl_dui_container));
        final JSONObject json = new JSONObject();
        JSONObject payload = new JSONObject();

        try {

            json.put("requestId", "123");
            json.put("service", getService());
            json.put("betaAssets", false);
            payload.put("clientId",getResources().getString(R.string.client_id));
            payload.put("action", "initiate");
            payload.put("service", getService());
            payload.put(PaymentConstants.ENV, "prod");

//            JSONObject signatureAuthData = new JSONObject();
//            signatureAuthData.put("signature", "Tz8ew9MYewcXBKIkjT7U+Tu3bPN06RZHBIKKbaJjMQ+e5uTaI4Hz0Ktu2KAXITR+7xBhBaLkMZ4Fb6HyaEOUjZES/qid/cVghyi1rJn3A0mI4VmMGt50IOep0b+5Ae2N1yCz58SwWvIRunv345amE0URHD6uca71rk2Rijva5XGjwgNrqOWXpzrHT0y0FRrvEr4u3du8QS2q0Wu4fZ2Ps9RSh04iPVNNuuMcUgkGSktxFP5vJLVYllYJDUzrOxi7nq3R11utNlSQMu18+ATSO5HyMTLxpndjhFlUJqn4QGxYovpp7amztJYjvoEnG9itPp2WdYamVeRAEcJnqRon2w==");
//            signatureAuthData.put("authData", "{\"mobileNumber\":\"9642429378\",\"mobileCountryCode\":\"+91\",\"merchantId\":\"NAMMA_YATRI\",\"timestamp\":\"2023-04-13T07:28:40+00:00\"}");
//            payload.put("signatureAuthData", signatureAuthData);

            json.put(PaymentConstants.PAYLOAD, payload);
        } catch (JSONException e) {
            e.printStackTrace();
        }
        hyperServices.initiate(json, new HyperPaymentsCallbackAdapter() {
            @Override
            public void onEvent(JSONObject jsonObject, JuspayResponseHandler juspayResponseHandler) {
                Log.d(LOG_TAG, "onEvent: " + jsonObject.toString());
                if (jsonObject.optString("event").equals("initiate_result")) {
                    //Getting Notification Data from Bundle and attaching to process payload
                    if (getIntent().hasExtra("NOTIFICATION_DATA") || (getIntent().hasExtra("notification_type") && getIntent().hasExtra("entity_ids") && getIntent().hasExtra("entity_type"))) {
                        try {
                            System.out.println("It has entered if statment");
                            JSONObject payload1 = json.getJSONObject(PaymentConstants.PAYLOAD);
                            payload1.put("action", "process");
                            payload1.put("notificationData", getNotificationDataFromIntent());
                            json.put(PaymentConstants.PAYLOAD, payload1);
                        } catch (JSONException e) {
                            Log.e("NOTIFICATIONDATA", e.toString());
                        }
                    }
                    Log.e(LOG_TAG, "json_payload" + json);
                    hyperServices.process(json);
                } else if (jsonObject.optString("event").equals("hide_splash")) {
                    String key = getResources().getString(R.string.service);
                    if (key.equals("nammayatri") && isSystemAnimEnabled) {
                        isHideSplashEventCalled = true;
                    } else {
                        hideSplash();
                    }
                } else if (jsonObject.optString("event").equals("show_splash")) {
                    View v = findViewById(in.juspay.mobility.app.R.id.splash);
                    if (v != null) {
                        findViewById(in.juspay.mobility.app.R.id.splash).setVisibility(View.VISIBLE);
                    }
                } else if (jsonObject.optString("event").equals("reboot")) {
                    Log.i(LOG_TAG, "event reboot");
                    hyperServices.terminate();
                    hyperServices = null;
                    initApp();
                } else if (jsonObject.optString("event").equals("in_app_notification")) {
                    showInAppNotifiation(jsonObject.optString("title"), jsonObject.optString("message"));
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
            initApp();
        });
        runOnUiThread(() -> {
            AlertDialog alertDialog = builder.create();
            alertDialog.show();
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

    @Override
    protected void onNewIntent(Intent intent) {
        if (intent != null && intent.hasExtra("NOTIFICATION_DATA")) {
            String data = intent.getExtras().getString("NOTIFICATION_DATA");
            try {
                JSONObject jsonData = new JSONObject(data);
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
        if (sharedPref != null)
            sharedPref.edit().putString(getResources().getString(in.juspay.mobility.app.R.string.ACTIVITY_STATUS), "onResume").apply();
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
        unregisterReceiver(gpsReceiver);
        unregisterReceiver(networkBroadcastReceiver);
        NetworkBroadcastReceiver.deRegisterProcessCallback(processCallBack);
        MyFirebaseMessagingService.deRegisterBundleUpdateCallback(bundleUpdateCallBack);
        super.onDestroy();
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, @Nullable Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
        hyperServices.onActivityResult(requestCode, resultCode, data);
        if (requestCode == REQUEST_CODE_UPDATE_APP) {
            if (resultCode != RESULT_OK) {
                Log.i(LOG_TAG, "Update flow failed! Result code: " + resultCode);
                if (updateType == AppUpdateType.IMMEDIATE) {
                    finishAndRemoveTask();
                }
            }
        }
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

    public void showInAppNotifiation(String title, String desc) {
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
                timerHandler.post(() -> {
                    notification.startAnimation(AnimationUtils.loadAnimation(getApplicationContext(), in.juspay.mobility.app.R.anim.bottom_to_top));
                    mainLayout.removeView(notification);
                    timer.cancel();
                    timer.purge();
                });
            }
        };
        timer.schedule(timerTask, 5000);
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
    public String getService () {
        StringBuilder key = new StringBuilder();
        if (in.juspay.mobility.BuildConfig.MERCHANT.equals("KL")) {
            key.append("net.openkochi.");
        } else {
            key.append("in.juspay.");
        }
        key.append(getResources().getString(R.string.service));
        return key.toString();
    }
}
