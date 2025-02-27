package in.juspay.mobility.app;

import static android.Manifest.permission.BLUETOOTH_CONNECT;
import static android.Manifest.permission.POST_NOTIFICATIONS;
import static android.Manifest.permission.RECORD_AUDIO;

import android.annotation.SuppressLint;
import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothManager;
import android.content.Context;
import android.app.Activity;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.net.ConnectivityManager;
import android.net.NetworkCapabilities;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.provider.Settings;
import android.util.Log;
import android.view.LayoutInflater;
import android.widget.Button;

import androidx.annotation.NonNull;
import androidx.constraintlayout.widget.ConstraintLayout;
import androidx.core.app.ActivityCompat;


import com.clevertap.android.sdk.CleverTapAPI;
import com.clevertap.android.signedcall.enums.VoIPCallStatus;
import com.clevertap.android.signedcall.exception.CallException;
import com.clevertap.android.signedcall.exception.InitException;
import com.clevertap.android.signedcall.init.SignedCallAPI;
import com.clevertap.android.signedcall.init.SignedCallInitConfiguration;
import com.clevertap.android.signedcall.interfaces.OutgoingCallResponse;
import com.clevertap.android.signedcall.interfaces.SCVoIPCallStatusListener;
import com.clevertap.android.signedcall.interfaces.SignedCallInitResponse;
import com.clevertap.android.signedcall.models.CallDetails;
import com.clevertap.android.signedcall.models.MissedCallAction;
import com.clevertap.android.signedcall.models.SCCallStatusDetails;
import com.clevertap.android.signedcall.models.SignedCallScreenBranding;
import com.google.android.material.bottomsheet.BottomSheetDialog;
import com.google.firebase.analytics.FirebaseAnalytics;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import in.juspay.hyper.core.BridgeComponents;
import in.juspay.mobility.app.RemoteConfigs.MobilityRemoteConfigs;

public class CleverTapSignedCall {

    private Context context ;
    private Activity activity ;
    public static String phone;
    public static String rideId;
    public static String merchantId;
    public static String callback;
    public static final int REQUEST_CALL = 8;
    public static final int REQUEST_MICROPHONE = 9;
    public static final int REQUEST_BLUETOOTH = 11;
    private static final int REQUEST_CODE_NOTIFICATION_PERMISSION = 10;
    private static String SC_API_KEY;
    private static String SC_ACCOUNT_ID;

    private BottomSheetDialog bottomSheetDialog;
    private static SharedPreferences sharedPrefs;
    private static BridgeComponents bridgeComponent;
    private MobilityRemoteConfigs remoteConfig;
    public static int useFallbackDialer;
    public static int callAttempts;
    private static boolean isDriver;
    private static int multipleCallAttempts;

    private static final String KEY_NETWORK_QUALITY_HIGH = "network_quality_high_threshold";
    private static final String KEY_NETWORK_QUALITY_MODERATE = "network_quality_moderate_threshold";
    private static final int DEFAULT_HIGH_THRESHOLD = 5000;
    private static final int DEFAULT_MODERATE_THRESHOLD = 1000;
    
    private void init(Context cxt, Activity act) {
        context = cxt;
        activity = act;
        sharedPrefs = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        remoteConfig = new MobilityRemoteConfigs(true, false);
        merchantId = context.getResources().getString(R.string.merchant_id);
    }

    public CleverTapSignedCall(Context cxt, Activity act){
        init(cxt,act);
    }

    public CleverTapSignedCall(Context cxt, Activity act, boolean isListenerOn, String ctApiKey, String ctAccountId){
        init(cxt,act);
        SC_API_KEY = ctApiKey;
        SC_ACCOUNT_ID = ctAccountId;
        if(isListenerOn)signedCallListener();
    }

    public void showDialer(String phoneNum) {
       Intent intent = new Intent(Intent.ACTION_DIAL);
       intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
       intent.setData(Uri.parse("tel:" + phoneNum));
       context.startActivity(intent);
    }

    protected boolean checkAudioPermission (){
        return context != null && ActivityCompat.checkSelfPermission(context, RECORD_AUDIO) == PackageManager.PERMISSION_GRANTED;
    }

    protected void checkAndAskBluetoothPermission() {
        if (Build.VERSION.SDK_INT > Build.VERSION_CODES.R) {
            BluetoothManager bluetoothManager = (BluetoothManager) context.getSystemService(Context.BLUETOOTH_SERVICE);
            if (bluetoothManager != null) {
                BluetoothAdapter bluetoothAdapter = bluetoothManager.getAdapter();
                if (bluetoothAdapter != null && bluetoothAdapter.isEnabled()) {
                    if (activity != null && context != null && ActivityCompat.checkSelfPermission(context, BLUETOOTH_CONNECT) != PackageManager.PERMISSION_GRANTED) {
                        ActivityCompat.requestPermissions(activity, new String[]{BLUETOOTH_CONNECT}, REQUEST_BLUETOOTH);
                    }
                }
            }
        }
    }



    public boolean isSignedCallInitialized(){
        return  SignedCallAPI.getInstance().isInitialized(context);
    }

    private String getNetworkType() {
        if(context != null){
            ConnectivityManager cm = (ConnectivityManager) context.getSystemService(Context.CONNECTIVITY_SERVICE);
            if (cm != null) {
                NetworkCapabilities capabilities = cm.getNetworkCapabilities(cm.getActiveNetwork());
                if (capabilities != null) {
                    if (capabilities.hasTransport(NetworkCapabilities.TRANSPORT_WIFI)) {
                        return "Wi-Fi";
                    } else if (capabilities.hasTransport(NetworkCapabilities.TRANSPORT_CELLULAR)) {
                        return "Cellular";
                    } else if (capabilities.hasTransport(NetworkCapabilities.TRANSPORT_ETHERNET)) {
                        return "Ethernet";
                    } else if (capabilities.hasTransport(NetworkCapabilities.TRANSPORT_BLUETOOTH)) {
                        return "Bluetooth";
                    }
                }
            }
        return "No Connection";
        }
        return "Unknown";
    }

    private String getNetworkQuality() {
        if(context != null){
            ConnectivityManager cm = (ConnectivityManager) context.getSystemService(Context.CONNECTIVITY_SERVICE);
            if (cm != null) {
                NetworkCapabilities capabilities = cm.getNetworkCapabilities(cm.getActiveNetwork());
                if (capabilities != null) {
                    if (capabilities.hasCapability(NetworkCapabilities.NET_CAPABILITY_INTERNET)) {
                        JSONObject voipCallConfig = null;
                        int highThreshold = DEFAULT_HIGH_THRESHOLD;
                        int moderateThreshold = DEFAULT_MODERATE_THRESHOLD;
                        int downSpeed = capabilities.getLinkDownstreamBandwidthKbps();
                        try {
                            voipCallConfig = new JSONObject(remoteConfig.getString("voip_call_config"));
                            highThreshold = voipCallConfig.optInt(KEY_NETWORK_QUALITY_HIGH, DEFAULT_HIGH_THRESHOLD);
                            moderateThreshold = voipCallConfig.optInt(KEY_NETWORK_QUALITY_MODERATE, DEFAULT_MODERATE_THRESHOLD);
                        } catch (JSONException e) {
                            Log.d("SC","Failed to fetch voip call config");
                        }
                        if (downSpeed > highThreshold) {
                            return "Good";
                        } else if (downSpeed > moderateThreshold) {
                            return "Moderate";
                        } else {
                            return "Poor";
                        }
                    }
                }
            }
        }
        return "Unknown";
    }

    public void voipDialer(String configJson, String phoneNum, String callback, BridgeComponents bridgeComponents) {
        this.bridgeComponent = bridgeComponents;
        this.phone = phoneNum;
        CleverTapSignedCall.callback = callback;
        useFallbackDialer = sharedPrefs.getInt("USE_FALLBACK_DIALER", 0);
        callAttempts = sharedPrefs.getInt("VOIP_CALL_ATTEMPTS", 0);
        
        JSONObject voipCallConfig = null;
        int fallBackThreshold = 1;
        int callAttemptsThreshold = 3;
        
        String receiverCuid, callContext, remoteContext, callerCuid;
        boolean isDriverBool, isMissed;
        
        try {
            JSONObject config = new JSONObject(configJson);
            rideId = config.optString("rideId", rideId);
            receiverCuid = config.optString("receiverCuid", "");
            callerCuid = config.optString("callerCuid", "");
            callContext = config.optString("callContext", "");
            remoteContext = config.optString("remoteContext", "");
            isDriverBool = config.optBoolean("isDriver", false);
            isMissed = config.optBoolean("isMissed", false);
        } catch (JSONException e) {
            Log.e("SignedCall", "Invalid JSON format in voipDialer", e);
            showDialer(phoneNum);
            return;
        }
        isDriver = isDriverBool;

        if (!isSignedCallInitialized()) {
            JSONObject initConfig = new JSONObject();
            try {
                initConfig.put("rideId", rideId);
                initConfig.put("cuid", callerCuid);
                initConfig.put("isDriver", isDriver);
            } catch (JSONException e) {
                Log.e("SignedCall", "Error creating JSON config for initSignedCall", e);
            }
            initSignedCall(initConfig.toString());
        }
    
        try {
            voipCallConfig = new JSONObject(remoteConfig.getString("voip_call_config"));
            fallBackThreshold = voipCallConfig.optInt("fallbackDialerThreshold", 1);
            callAttemptsThreshold = voipCallConfig.optInt("callAttemptsThreshold", 3);
        } catch (JSONException e) {
            Log.d("SC", "Failed to fetch voip call config");
        }
    
        if (useFallbackDialer > fallBackThreshold || callAttempts > callAttemptsThreshold) {
            showDialer(phoneNum);
            return;
        }
    
        JSONObject callOptions = new JSONObject();
        try {
            callOptions.put("remote_context", remoteContext);
        } catch (JSONException e) {
            Log.d("SignedCall", "JSON error when setting up VOIP call");
        }
    
        FirebaseAnalytics mFirebaseAnalytics = FirebaseAnalytics.getInstance(context);
        Bundle bundle = new Bundle();
        bundle.putString(isMissed ? "missed_voip_callback" : "ride_id", rideId);
        String exoPhone = phone;
        boolean IsDriver = isDriver;
        OutgoingCallResponse outgoingCallResponseListener = new OutgoingCallResponse() {
            @Override
            public void onSuccess() {
                mFirebaseAnalytics.logEvent("voip_call_success",bundle);
                Log.d("SignedCall", "Signed call initiation success");
            }
            @Override
            public void onFailure(CallException callException) {
                Log.d("SignedCall: ", "error code: " + callException.getErrorCode()
                    + "\n error message: " + callException.getMessage()
                    + "\n error explanation: " + callException.getExplanation());
                String callbackResult;
                String finalNetworkType = getNetworkType();
                String finalNetworkQuality = getNetworkQuality();
                if(callException.getErrorCode() == CallException.CanNotProcessCallRequest.getErrorCode()){
                    multipleCallAttempts++;
                    if(multipleCallAttempts > 4){
                        updateFallbackDialer();
                    }
                    sendJavaScriptCallback(callback, "", "MULTIPLE_VOIP_CALL_ATTEMPTS", rideId, callException.getErrorCode(), IsDriver, finalNetworkType, finalNetworkQuality, merchantId);
                    return;
                } else if(callException.getErrorCode() == CallException.MicrophonePermissionNotGrantedException.getErrorCode()) {
                    if (activity != null && ActivityCompat.checkSelfPermission(context, RECORD_AUDIO) != PackageManager.PERMISSION_GRANTED) {
                        ActivityCompat.requestPermissions(activity, new String[]{RECORD_AUDIO}, REQUEST_MICROPHONE);
                    }
                    mFirebaseAnalytics.logEvent("voip_call_failed_NO_MIC_PERM_CALLER",bundle);
                    callbackResult = "MIC_PERMISSION_DENIED";
                } else if(callException.getErrorCode() == CallException.BadNetworkException.getErrorCode() || callException.getErrorCode() == CallException.ContactNotReachableException.getErrorCode()){
                    showDialer(exoPhone);
                    mFirebaseAnalytics.logEvent("voip_call_failed_BAD_NETWORK_CALLER",bundle);
                    callbackResult = "NETWORK_ERROR";
                } else if(callException.getErrorCode() == CallException.NoInternetException.getErrorCode()){
                    showDialer(exoPhone);
                    mFirebaseAnalytics.logEvent("voip_call_failed_NO_INTERNET_CALLER",bundle);
                    callbackResult = "NO_INTERNET";
                } else if(callException.getErrorCode() == CallException.CallFeatureNotAvailable.getErrorCode()) {
                    showDialer(exoPhone);
                    mFirebaseAnalytics.logEvent("voip_call_failed_INIT_NOT_DONE",bundle);
                    callbackResult = "SDK_NOT_INIT";
                } else {
                    showDialer(exoPhone);
                    mFirebaseAnalytics.logEvent("voip_call_failed_CALLER",bundle);
                    callbackResult = "UNKNOWN_ERROR";
                }
                updateFallbackDialer();
                sendJavaScriptCallback(callback, "", callbackResult, rideId, callException.getErrorCode(), IsDriver, finalNetworkType, finalNetworkQuality, merchantId);
            }
        };
    
        if (checkAudioPermission()) {
            SignedCallAPI.getInstance().call(context, receiverCuid, callContext, callOptions, outgoingCallResponseListener);
            checkAndAskBluetoothPermission();
        } else {
            boolean hasAskedMicPermission = sharedPrefs.getBoolean("MIC_PERMISSION_ASKED", false);
            mFirebaseAnalytics.logEvent("voip_call_failed_NO_MIC_PERM_CALLER",bundle);
            if (activity != null && ActivityCompat.checkSelfPermission(context, RECORD_AUDIO) != PackageManager.PERMISSION_GRANTED) {
                if (!hasAskedMicPermission) {
                    ActivityCompat.requestPermissions(activity, new String[]{RECORD_AUDIO}, REQUEST_MICROPHONE);
                    sharedPrefs.edit().putBoolean("MIC_PERMISSION_ASKED", true).apply();
                } else {
                    showCustomBottomSheet(isDriver);
                }
            }
            String finalNetworkType = getNetworkType();
            String finalNetworkQuality = getNetworkQuality();
            sendJavaScriptCallback(callback, "", "MIC_PERMISSION_DENIED", rideId, CallException.MicrophonePermissionNotGrantedException.getErrorCode(), isDriver, finalNetworkType, finalNetworkQuality, merchantId);
            Log.e("signed call error :", "Signed call Audio Permission not given" );
        }
    }

    @SuppressLint("RestrictedApi")
    public void initSignedCall(String configJson){
        if (activity != null && ActivityCompat.checkSelfPermission(context, POST_NOTIFICATIONS) != PackageManager.PERMISSION_GRANTED) {
            ActivityCompat.requestPermissions(activity, new String[]{POST_NOTIFICATIONS}, REQUEST_CODE_NOTIFICATION_PERMISSION);
        }
        if(isSignedCallInitialized()) return;
        JSONObject initOptions = new JSONObject(), config = null;

        try {
            config = new JSONObject(configJson);
            String cuid = config.getString("cuid");
            boolean isDriverBool = config.getBoolean("isDriver");
            rideId = config.optString("rideId");
            isDriver = isDriverBool;
            initOptions.put("accountId", SC_ACCOUNT_ID);
            initOptions.put("apiKey", SC_API_KEY);
            initOptions.put("cuid", cuid);
    
        } catch (JSONException e) {
            Log.d("SignedCall", "JSON error at setting up SignedCall data: " + e.getMessage());
        }
        Bundle bundle = new Bundle();
        bundle.putString("ride_id", rideId);
        FirebaseAnalytics mFirebaseAnalytics = FirebaseAnalytics.getInstance(context);

        SignedCallInitResponse signedCallInitListener = new SignedCallInitResponse() {
            @Override
            public void onSuccess() {
                Log.d("SignedCallInitiation: ", "Successfully initiated Signed Call (voip)");
                mFirebaseAnalytics.logEvent("voip_init_success",bundle);
            }
            @Override
            public void onFailure(@NonNull InitException initException) {
                Log.d("SignedCall: ", "error code: " + initException.getErrorCode()
                        + "\n error message: " + initException.getMessage()
                        + "\n error explanation: " + initException.getExplanation());
                mFirebaseAnalytics.logEvent("voip_init_failed",bundle);
                SharedPreferences.Editor editor = sharedPrefs.edit();
                editor.putInt("USE_FALLBACK_DIALER", 2);
                editor.apply();
            }
        };
        List<MissedCallAction> missedCallActionsList = new ArrayList<>();
        missedCallActionsList.add(new MissedCallAction("callback", "Callback"));
        missedCallActionsList.add(new MissedCallAction("dismiss", "Dismiss"));
        int colorInt = context.getColor(R.color.colorVoipBackground);
        String colorString = String.format("#%06X", (0xFFFFFF & colorInt));
        SignedCallScreenBranding callScreenBranding = new SignedCallScreenBranding(
                colorString, "#ffffff", context.getString(R.string.voip_logo), SignedCallScreenBranding.ButtonTheme.LIGHT, "#FF453A");
        callScreenBranding.setShowPoweredBySignedCall(false);

        SignedCallInitConfiguration initConfiguration = new SignedCallInitConfiguration.Builder(initOptions, false)
                                                            .setSwipeOffBehaviourInForegroundService(SignedCallInitConfiguration.SCSwipeOffBehaviour.PERSIST_CALL)
                                                            .overrideDefaultBranding(callScreenBranding)
                                                            .setMissedCallActions(missedCallActionsList)
                                                            .setNotificationPermissionRequired(false)
                                                            .build();
        CleverTapAPI cleverTapAPI = CleverTapAPI.getDefaultInstance(context);
        SignedCallAPI.getInstance().init(context, initConfiguration, cleverTapAPI, signedCallInitListener);
    }

    public void destroySignedCall() {
        SharedPreferences.Editor editor = sharedPrefs.edit();
        editor.putInt("USE_FALLBACK_DIALER", 0);
        editor.putInt("VOIP_CALL_ATTEMPTS", 0);
        editor.apply();
        multipleCallAttempts = 0;
        SignedCallAPI.getInstance().disconnectSignallingSocket(context);
        SignedCallAPI.getInstance().logout(context);
    }

    public void showCustomBottomSheet(boolean isDriver) {
        if (activity == null || context == null) return;
        activity.runOnUiThread(() -> {
                if (bottomSheetDialog != null && bottomSheetDialog.isShowing()) {
                    return; 
                }
                LayoutInflater inflater = activity.getLayoutInflater();
                int resourceValue = isDriver ? R.layout.bottom_sheet_permission_callmiss_driver : R.layout.bottom_sheet_permission;
                ConstraintLayout constraintLayout = (ConstraintLayout) inflater.inflate(resourceValue, null);
                bottomSheetDialog = new BottomSheetDialog(activity);
                bottomSheetDialog.setContentView(constraintLayout);
                bottomSheetDialog.setCancelable(false);
                Button btnGoToSettings = constraintLayout.findViewById(R.id.btn_go_to_settings);
                Button btnCancel = constraintLayout.findViewById(R.id.btn_cancel);
                btnGoToSettings.setOnClickListener(v -> {
                    bottomSheetDialog.dismiss();
                    Intent intent = new Intent(Settings.ACTION_APPLICATION_DETAILS_SETTINGS);
                    Uri uri = Uri.fromParts("package", activity.getPackageName(), null);
                    intent.setData(uri);
                    activity.startActivity(intent);
                });
                btnCancel.setOnClickListener(v -> bottomSheetDialog.dismiss());
                bottomSheetDialog.show();
        });

    }
    private void updateCallAttempts(int increment) {
        int callAttempts = sharedPrefs.getInt("VOIP_CALL_ATTEMPTS", 0);
        sharedPrefs.edit().putInt("VOIP_CALL_ATTEMPTS", callAttempts + increment).apply();
        Log.d("signedcall", "Updated callAttempts: " + (callAttempts + increment));
    }
    
    private void updateFallbackDialer() {
        sharedPrefs.edit().putInt("USE_FALLBACK_DIALER", 2).apply();
        Log.d("signedcall", "Fallback dialer updated");
    }

    private void sendJavaScriptCallback(String callback, String callId, String status, String rideId, int errorCode, boolean isDriver, String networkType, String networkQuality, String merchantId) {
        if (callback == null || callId == null || status == null || rideId == null || networkType == null || networkQuality == null || merchantId == null) {
            Log.d("signedcall", "JavaScript callback not sent due to null values.");
            return;
        }
        String javascript = String.format(Locale.ENGLISH,
                "window.callUICallback('%s','%s', '%s', '%s', %d, %d, '%s', '%s', '%s');",
                callback, callId, status, rideId, errorCode, isDriver ? 1 : 0, networkType, networkQuality, merchantId);
        if (bridgeComponent != null) {
            bridgeComponent.getJsCallback().addJsToWebView(javascript);
        }
        Log.d("signedcall", "JavaScript callback sent: " + javascript);
    }

    private void handleIncomingCallPermissions(boolean isDriver) {
        if (context != null && (ActivityCompat.checkSelfPermission(context, RECORD_AUDIO) != PackageManager.PERMISSION_GRANTED || ActivityCompat.checkSelfPermission(context, POST_NOTIFICATIONS) != PackageManager.PERMISSION_GRANTED)) {
           showCustomBottomSheet(isDriver);
        }
    }
    public void signedCallListener() {
        SignedCallAPI.getInstance().registerVoIPCallStatusListener(new SCVoIPCallStatusListener() {
            @Override
            public void callStatus(final SCCallStatusDetails callStatusDetails) {
                // App is notified on the main thread to notify the changes in the call-state
                Log.d("signedcall", "callStatus is invoked with: " + callStatusDetails.toString());
    
                SCCallStatusDetails.CallDirection direction = callStatusDetails.getDirection();
                VoIPCallStatus callStatus = callStatusDetails.getCallStatus();
                CallDetails callDetails = callStatusDetails.getCallDetails();
                boolean isDriverReceiver = callDetails.calleeCuid.contains("driver");
                String finalNetworkType = getNetworkType();
                String finalNetworkQuality = getNetworkQuality();
                String callId = callDetails.callId;
    
                if (direction == SCCallStatusDetails.CallDirection.OUTGOING) {
                    // Handle events for initiator of the call
                    switch (callStatus) {

                        case CALL_IS_PLACED:
                            sendJavaScriptCallback(callback, callId, "CALL_IS_PLACED", rideId, -2, isDriver, finalNetworkType, finalNetworkQuality, merchantId);
                            break;
                        
                        case CALL_RINGING:
                            sendJavaScriptCallback(callback, callId, "CALL_RINGING", rideId, -2, isDriver, finalNetworkType, finalNetworkQuality, merchantId);
                            break;
                        
                        case CALL_MISSED:
                            sendJavaScriptCallback(callback, callId, "CALL_MISSED", rideId, -2, isDriver, finalNetworkType, finalNetworkQuality, merchantId);
                            updateCallAttempts(2);
                            break;

                        case CALL_CANCELLED:
                            sendJavaScriptCallback(callback, callId, "CALL_CANCELLED", rideId, -2, isDriver, finalNetworkType, finalNetworkQuality, merchantId);
                            updateCallAttempts(1);
                            break;
                        
                        case CALL_DECLINED_DUE_TO_BUSY_ON_PSTN:
                            sendJavaScriptCallback(callback, callId, "CALL_DECLINED_DUE_TO_BUSY_ON_PSTN", rideId, -2, isDriver, finalNetworkType, finalNetworkQuality, merchantId);
                            updateCallAttempts(1);
                            break;
                        
                        case CALL_DECLINED_DUE_TO_BUSY_ON_VOIP:
                            sendJavaScriptCallback(callback, callId, "CALL_DECLINED_DUE_TO_BUSY_ON_VOIP", rideId, -2, isDriver, finalNetworkType, finalNetworkQuality, merchantId);
                            updateCallAttempts(1);
                            break;
                        
                        case CALL_OVER_DUE_TO_NETWORK_DELAY_IN_MEDIA_SETUP:
                            sendJavaScriptCallback(callback, callId, "CALL_OVER_DUE_TO_NETWORK_DELAY_IN_MEDIA_SETUP", rideId, -2, isDriver, finalNetworkType, finalNetworkQuality, merchantId);
                            updateFallbackDialer();
                            break;
                        
                        case CALL_OVER_DUE_TO_PROTOCOL_MISMATCH:
                            sendJavaScriptCallback(callback, callId, "CALL_OVER_DUE_TO_PROTOCOL_MISMATCH", rideId, -2, isDriver, finalNetworkType, finalNetworkQuality, merchantId);
                            updateCallAttempts(2);
                            break;
                        
                        case CALL_OVER_DUE_TO_REMOTE_NETWORK_LOSS:
                            sendJavaScriptCallback(callback, callId, "CALL_OVER_DUE_TO_REMOTE_NETWORK_LOSS", rideId, -2, isDriver, finalNetworkType, finalNetworkQuality, merchantId);
                            updateFallbackDialer();
                            break;
                        
                        case CALL_OVER_DUE_TO_LOCAL_NETWORK_LOSS:
                            sendJavaScriptCallback(callback, callId, "CALL_OVER_DUE_TO_LOCAL_NETWORK_LOSS", rideId, -2, isDriver, finalNetworkType, finalNetworkQuality, merchantId);
                            updateFallbackDialer();
                            break;
                        
                        case CALL_IN_PROGRESS:
                            sendJavaScriptCallback(callback, callId, "CALL_IN_PROGRESS", rideId, -2, isDriver, finalNetworkType, finalNetworkQuality, merchantId);
                            break;
                        
                        case CALL_ANSWERED:
                            sendJavaScriptCallback(callback, callId, "CALL_ANSWERED", rideId, -2, isDriver, finalNetworkType, finalNetworkQuality, merchantId);
                            break;
                            
                        case CALL_CANCELLED_DUE_TO_RING_TIMEOUT:
                            sendJavaScriptCallback(callback, callId, "CALL_CANCELLED_DUE_TO_RING_TIMEOUT", rideId, -2, isDriver, finalNetworkType, finalNetworkQuality, merchantId);
                            updateCallAttempts(3);
                            break;
    
                        case CALL_DECLINED:
                            sendJavaScriptCallback(callback, callId, "CALL_DECLINED", rideId, -2, isDriver, finalNetworkType, finalNetworkQuality, merchantId);
                            updateCallAttempts(2);
                            break;
    
                        case CALL_OVER:
                            sendJavaScriptCallback(callback, callId, "CALL_OVER", rideId, -2, isDriver, finalNetworkType, finalNetworkQuality, merchantId);
                            break;
    
                        case CALL_DECLINED_DUE_TO_LOGGED_OUT_CUID:
                            sendJavaScriptCallback(callback, callId, "CALL_DECLINED_DUE_TO_LOGGED_OUT_CUID", rideId, -2, isDriver, finalNetworkType, finalNetworkQuality, merchantId);
                            updateFallbackDialer();
                            break;
    
                        case CALL_DECLINED_DUE_TO_NOTIFICATIONS_DISABLED:
                            updateFallbackDialer();
                            sendJavaScriptCallback(callback, callId, "CALL_DECLINED_DUE_TO_NOTIFICATIONS_DISABLED", rideId, -2, isDriver, finalNetworkType, finalNetworkQuality, merchantId);
                            break;
    
                        case CALLEE_MICROPHONE_PERMISSION_NOT_GRANTED:
                        case CALLEE_MICROPHONE_PERMISSION_BLOCKED:
                            updateFallbackDialer();
                            sendJavaScriptCallback(callback, callId,"CALLEE_MICROPHONE_PERMISSION_BLOCKED", rideId, -2, isDriver, finalNetworkType, finalNetworkQuality, merchantId);
                            break;
    
                        case CALL_FAILED_DUE_TO_INTERNAL_ERROR:
                            sendJavaScriptCallback(callback, callId, "CALL_FAILED_DUE_TO_INTERNAL_ERROR", rideId, -2, isDriver, finalNetworkType, finalNetworkQuality, merchantId);
                            updateFallbackDialer();
                            break;
                    }
                } else if (direction == SCCallStatusDetails.CallDirection.INCOMING) {
                    // Handle events for receiver of the call
                    switch (callStatus) {
                        case CALL_MISSED:
                        case CALLEE_MICROPHONE_PERMISSION_NOT_GRANTED:
                        case CALLEE_MICROPHONE_PERMISSION_BLOCKED:
                        case CALL_DECLINED_DUE_TO_NOTIFICATIONS_DISABLED:
                            handleIncomingCallPermissions(isDriverReceiver);
                            break;
                    }
                }
            }
        });
    }    
}