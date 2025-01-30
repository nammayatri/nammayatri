package in.juspay.mobility.app;

import static android.Manifest.permission.BLUETOOTH_CONNECT;
import static android.Manifest.permission.POST_NOTIFICATIONS;
import static android.Manifest.permission.RECORD_AUDIO;

import android.bluetooth.BluetoothAdapter;
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

    private BottomSheetDialog bottomSheetDialog;
    private static SharedPreferences sharedPrefs;
    private static BridgeComponents bridgeComponent;
    private MobilityRemoteConfigs remoteConfig;
    
    public CleverTapSignedCall(Context cxt, Activity act){
        context = cxt;
        activity = act;
        sharedPrefs = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        remoteConfig = new MobilityRemoteConfigs(true, false);
        merchantId = context.getResources().getString(R.string.merchant_id);
    }

    public CleverTapSignedCall(Context cxt, Activity act, boolean isListenerOn){
        context = cxt;
        activity = act;
        if(isListenerOn)signedCallListener();
        sharedPrefs = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        remoteConfig = new MobilityRemoteConfigs(true, false);
        merchantId = context.getResources().getString(R.string.merchant_id);
    }

    public static int useFallbackDialer;
    public static int callAttempts = 0;
    private boolean isDriver;

    public void showDialer(String phoneNum) {
       Intent intent = new Intent(Intent.ACTION_DIAL);
       intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
       intent.setData(Uri.parse("tel:" + phoneNum));
       context.startActivity(intent);
    }

    protected boolean checkAudioPermission (){
        return activity == null || ActivityCompat.checkSelfPermission(context, RECORD_AUDIO) == PackageManager.PERMISSION_GRANTED;
    }

    protected boolean checkAndAskBluetoothPermission (){
        if (Build.VERSION.SDK_INT < 30) {
            if (activity != null && ActivityCompat.checkSelfPermission(context, BLUETOOTH_CONNECT) != PackageManager.PERMISSION_GRANTED) {
                ActivityCompat.requestPermissions(activity, new String[]{BLUETOOTH_CONNECT}, REQUEST_BLUETOOTH);
                return false;
            } else {
                return true;
            }
        }
        return true;
    }

    public boolean isSignedCallInitialized(){
        return sharedPrefs.getBoolean("SIGNED_CALL_INITIALIZED", false);
    }

    private String getNetworkType() {
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

    private String getNetworkQuality() {
        ConnectivityManager cm = (ConnectivityManager) context.getSystemService(Context.CONNECTIVITY_SERVICE);
        if (cm != null) {
            NetworkCapabilities capabilities = cm.getNetworkCapabilities(cm.getActiveNetwork());
            if (capabilities != null) {
                if (capabilities.hasCapability(NetworkCapabilities.NET_CAPABILITY_INTERNET)) {
                    int downSpeed = capabilities.getLinkDownstreamBandwidthKbps();
                    if (downSpeed > 5000) {
                        return "Good";
                    } else if (downSpeed > 1000) {
                        return "Moderate";
                    } else {
                        return "Poor";
                    }
                }
            }
        }
        return "Unknown";
    }

    public void voipDialer(String cuid, boolean isDriver, String phoneNum, boolean isMissed, String callback, BridgeComponents bridgeComponents) {
        phone = phoneNum;
        this.isDriver = isDriver;
        CleverTapSignedCall.callback = callback;
        this.bridgeComponent = bridgeComponents;
        useFallbackDialer = sharedPrefs.getInt("USE_FALLBACK_DIALER", 0);
        callAttempts = sharedPrefs.getInt("VOIP_CALL_ATTEMPTS", 0);
        sendJavaScriptCallback(callback, "CALL_VOIP", cuid, -1, isDriver, "wifi", "good", "nammayatri");
        if(useFallbackDialer > 1 || callAttempts > 3 ){
            showDialer(phone);
            return;
        }
        BluetoothAdapter bluetoothAdapter = BluetoothAdapter.getDefaultAdapter();
        String receiverCuid, callContext, remoteContext, rideId = cuid;
        String ny_customer = context.getString(R.string.namma_yatri_customer);
        String ny_driver = context.getString(R.string.namma_yatri_driver);
        JSONObject callOptions = new JSONObject();
        FirebaseAnalytics mFirebaseAnalytics = FirebaseAnalytics.getInstance(context);
        Bundle bundle = new Bundle();
        bundle.putString(isMissed ? "missed_voip_callback" : "ride_id", rideId);
        
        if (!isMissed) {
            cuid = cuid.replace("-", "");
            if(cuid.length() > 10) cuid = cuid.substring(0, 10);
            receiverCuid = (isDriver ? "customer" : "driver") + cuid;
        } else receiverCuid = cuid;
        
        callContext = isDriver
                ? (ny_customer.isEmpty() ? "Namma Yatri Customer" : ny_customer)
                : (ny_driver.isEmpty() ? "Namma Yatri Driver" : ny_driver);
        
        remoteContext = isDriver
                ? (ny_driver.isEmpty() ? "Namma Yatri Driver" : ny_driver)
                : (ny_customer.isEmpty() ? "Namma Yatri Customer" : ny_customer);
        
        try {
            callOptions.put("remote_context", remoteContext);
        } catch (JSONException e) {
            e.printStackTrace();
        }
        String tempPhone = phone;
        OutgoingCallResponse outgoingCallResponseListener = new OutgoingCallResponse() {
            @Override
            public void onSuccess() {
                mFirebaseAnalytics.logEvent("voip_call_success",bundle);
                String finalNetworkType = getNetworkType();
                String finalNetworkQuality = getNetworkQuality();
                sendJavaScriptCallback(callback, "CALL_INITIATED_SUCCESSFULLY", rideId, -1, isDriver, finalNetworkType, finalNetworkQuality, merchantId);
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
                    // Do nothing (user trying to spam call button, sdk still processing first click)
                    return;
                } else if(callException.getErrorCode() == CallException.MicrophonePermissionNotGrantedException.getErrorCode()) {
                    if (activity != null && ActivityCompat.checkSelfPermission(context, RECORD_AUDIO) != PackageManager.PERMISSION_GRANTED) {
                        ActivityCompat.requestPermissions(activity, new String[]{RECORD_AUDIO}, REQUEST_MICROPHONE);
                    }
                    mFirebaseAnalytics.logEvent("voip_call_failed_NO_MIC_PERM_CALLER",bundle);
                    callbackResult = "MIC_PERMISSION_DENIED";
                } else if(callException.getErrorCode() == CallException.BadNetworkException.getErrorCode() || callException.getErrorCode() == CallException.ContactNotReachableException.getErrorCode()){
                    showDialer(tempPhone);
                    mFirebaseAnalytics.logEvent("voip_call_failed_BAD_NETWORK_CALLER",bundle);
                    callbackResult = "NETWORK_ERROR";
                } else if(callException.getErrorCode() == CallException.NoInternetException.getErrorCode()){
                    showDialer(tempPhone);
                    mFirebaseAnalytics.logEvent("voip_call_failed_NO_INTERNET_CALLER",bundle);
                    callbackResult = "NO_INTERNET";
                } else if(callException.getErrorCode() == CallException.CallFeatureNotAvailable.getErrorCode()) {
                    showDialer(tempPhone);
                    mFirebaseAnalytics.logEvent("voip_call_failed_INIT_NOT_DONE",bundle);
                    callbackResult = "SDK_NOT_INIT";
                } else {
                    showDialer(tempPhone);
                    mFirebaseAnalytics.logEvent("voip_call_failed_CALLER",bundle);
                    callbackResult = "UNKNOWN_ERROR";
                }
                updateFallbackDialer();
                sendJavaScriptCallback(callback, callbackResult, rideId, callException.getErrorCode(), isDriver, finalNetworkType, finalNetworkQuality, merchantId);
            }
        };

        if(checkAudioPermission()){
            if((bluetoothAdapter == null || !bluetoothAdapter.isEnabled())){
                SignedCallAPI.getInstance().call(context, receiverCuid, callContext, callOptions, outgoingCallResponseListener);
            } else if(checkAndAskBluetoothPermission()){
                SignedCallAPI.getInstance().call(context, receiverCuid, callContext, callOptions, outgoingCallResponseListener);
            } else {
                Log.e("signed call error :", "Bluetooth Permission not given" );
            }
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
            sendJavaScriptCallback(callback, "MIC_PERMISSION_DENIED", rideId, CallException.MicrophonePermissionNotGrantedException.getErrorCode(), isDriver, finalNetworkType, finalNetworkQuality, merchantId);
            Log.e("signed call error :", "Signed call Audio Permission not given" );
        }
    }

    public void initSignedCall(String cuid, boolean isDriver){

        if (activity != null && ActivityCompat.checkSelfPermission(context, POST_NOTIFICATIONS) != PackageManager.PERMISSION_GRANTED) {
            ActivityCompat.requestPermissions(activity, new String[]{POST_NOTIFICATIONS}, REQUEST_CODE_NOTIFICATION_PERMISSION);
        }
        if(isSignedCallInitialized()) return;
        JSONObject initOptions = new JSONObject();
        rideId = cuid;
        String Cuid = cuid.replace("-", "");
        if (Cuid.length() > 10) {
            Cuid = Cuid.substring(0, 10);
        }
        Cuid = (isDriver) ? "driver" + Cuid : "customer" + Cuid;
        Bundle bundle = new Bundle();
        bundle.putString("ride_id",cuid);
        FirebaseAnalytics mFirebaseAnalytics = FirebaseAnalytics.getInstance(context);
        try {
            initOptions.put("accountId", "6715f01d5b143458ec1c1200");
            initOptions.put("apiKey", "C82XhWtydCwnmFPRicYXcs9q6FUINbEiBCJBsUxhyB0fW1i5JOjkm2nTUwmGWV7j");
            initOptions.put("cuid", Cuid);
        } catch (JSONException e) {
            e.printStackTrace();
        }

        SignedCallInitResponse signedCallInitListener = new SignedCallInitResponse() {
            @Override
            public void onSuccess() {
                Log.d("SignedCallInitiation: ", "Successfully initiated Signed Call (voip)");
                SharedPreferences.Editor editor = sharedPrefs.edit();
                editor.putBoolean("SIGNED_CALL_INITIALIZED", true);
                editor.apply();
                SignedCallAPI.setDebugLevel(SignedCallAPI.LogLevel.VERBOSE);
                mFirebaseAnalytics.logEvent("voip_init_success",bundle);
            }
            @Override
            public void onFailure(@NonNull InitException initException) {
                Log.d("SignedCall: ", "error code: " + initException.getErrorCode()
                        + "\n error message: " + initException.getMessage()
                        + "\n error explanation: " + initException.getExplanation());
                mFirebaseAnalytics.logEvent("voip_init_failed",bundle);
                SharedPreferences.Editor editor = sharedPrefs.edit();
                editor.putBoolean("SIGNED_CALL_INITIALIZED", false);
                editor.putInt("USE_FALLBACK_DIALER", 2);
                editor.apply();
            }
        };
        List<MissedCallAction> missedCallActionsList = new ArrayList<>();
        missedCallActionsList.add(new MissedCallAction("callback", "Callback"));
        missedCallActionsList.add(new MissedCallAction("dismiss", "Dismiss"));
        SignedCallScreenBranding callScreenBranding = new SignedCallScreenBranding(
            context.getString(R.color.colorVoipBackground), "#ffffff", context.getString(R.string.voip_logo), SignedCallScreenBranding.ButtonTheme.LIGHT, "#FF453A");
        callScreenBranding.setShowPoweredBySignedCall(false);

        SignedCallInitConfiguration initConfiguration = new SignedCallInitConfiguration.Builder(initOptions, false)
                                                            .setSwipeOffBehaviourInForegroundService(SignedCallInitConfiguration.SCSwipeOffBehaviour.PERSIST_CALL)
                                                            .overrideDefaultBranding(callScreenBranding)
                                                            .setMissedCallActions(missedCallActionsList)
                                                            .setNotificationPermissionRequired(false)
                                                            // .callScreenOnSignalling(true)
                                                            .build();
        CleverTapAPI cleverTapAPI = CleverTapAPI.getDefaultInstance(context);
        SignedCallAPI.getInstance().init(context, initConfiguration, cleverTapAPI, signedCallInitListener);
    }

    public void destroySignedCall() {
        SharedPreferences.Editor editor = sharedPrefs.edit();
        editor.putBoolean("SIGNED_CALL_INITIALIZED", false);
        editor.putInt("USE_FALLBACK_DIALER", 0);
        editor.putInt("VOIP_CALL_ATTEMPTS", 0);
        editor.apply();
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
        int callAttempts = sharedPrefs.getInt("VOIP_CALL_ATTEMPTS", 4);
        sharedPrefs.edit().putInt("VOIP_CALL_ATTEMPTS", callAttempts + increment).apply();
        Log.d("signedcall", "Updated callAttempts: " + (callAttempts + increment));
    }
    
    private void updateFallbackDialer() {
        sharedPrefs.edit().putInt("USE_FALLBACK_DIALER", 2).apply();
        Log.d("signedcall", "Fallback dialer updated");
    }

    private void sendJavaScriptCallback(String callback, String status, String rideId, int errorCode, boolean isDriver, String networkType, String networkQuality, String merchantId) {
        String javascript = String.format(Locale.ENGLISH,
                "window.callUICallback('%s', '%s', '%s', %d, %d, '%s', '%s', '%s');",
                callback, status, rideId, errorCode, isDriver ? 1 : 0, networkType, networkQuality, merchantId);
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
    public void signedCallListener(){
        SignedCallAPI.getInstance().registerVoIPCallStatusListener(new SCVoIPCallStatusListener() {
            @Override
            public void callStatus(final SCCallStatusDetails callStatusDetails) {
                //App is notified on the main thread to notify the changes in the call-state
                Log.d("signedcall", "callStatus is invoked with: " + callStatusDetails.toString());

                SCCallStatusDetails.CallDirection direction = callStatusDetails.getDirection();
                VoIPCallStatus callStatus = callStatusDetails.getCallStatus();
                CallDetails callDetails = callStatusDetails.getCallDetails();
                boolean isDriver = callDetails.calleeCuid.contains("driver");
                String finalNetworkType = getNetworkType();
                String finalNetworkQuality = getNetworkQuality();

                if (direction.equals(SCCallStatusDetails.CallDirection.OUTGOING)) {
                    //Handle events for initiator of the call
                    if (callStatus == VoIPCallStatus.CALL_CANCELLED) {
                        updateCallAttempts(1);
                    } else if (callStatus == VoIPCallStatus.CALL_CANCELLED_DUE_TO_RING_TIMEOUT) {
                        sendJavaScriptCallback(callback, "CALL_CANCELLED_DUE_TO_RING_TIMEOUT", rideId, -2, isDriver, finalNetworkType, finalNetworkQuality, merchantId);
                        updateCallAttempts(3);
                    } else if (callStatus == VoIPCallStatus.CALL_DECLINED) {
                        sendJavaScriptCallback(callback, "CALL_DECLINED", rideId, -2, isDriver, finalNetworkType, finalNetworkQuality, merchantId);
                        updateCallAttempts(2);
                    } else if (callStatus == VoIPCallStatus.CALL_MISSED) {
                        updateCallAttempts(2);
                    } else if (callStatus == VoIPCallStatus.CALL_OVER) {
                        sendJavaScriptCallback(callback, "CALL_OVER", rideId, -2, isDriver, finalNetworkType, finalNetworkQuality, merchantId);
                    } else if (callStatus == VoIPCallStatus.CALL_DECLINED_DUE_TO_LOGGED_OUT_CUID) {
                        updateFallbackDialer();
                        showDialer(phone);
                    } else if (callStatus == VoIPCallStatus.CALL_DECLINED_DUE_TO_NOTIFICATIONS_DISABLED) {
                        updateFallbackDialer();
                        sendJavaScriptCallback(callback, "CALL_DECLINED_DUE_TO_NOTIFICATIONS_DISABLED", rideId, -2, isDriver, finalNetworkType, finalNetworkQuality, merchantId);
                        showDialer(phone);
                    } else if (callStatus == VoIPCallStatus.CALLEE_MICROPHONE_PERMISSION_NOT_GRANTED || callStatus == VoIPCallStatus.CALLEE_MICROPHONE_PERMISSION_BLOCKED) {
                        updateFallbackDialer();
                        sendJavaScriptCallback(callback, "CALLEE_MICROPHONE_PERMISSION_BLOCKED", rideId, -2, isDriver, finalNetworkType, finalNetworkQuality, merchantId);
                        showDialer(phone);
                    } else if (callStatus == VoIPCallStatus.CALL_FAILED_DUE_TO_INTERNAL_ERROR) {
                        updateFallbackDialer();
                        showDialer(phone);
                    }
                } else if (direction.equals(SCCallStatusDetails.CallDirection.INCOMING)) {
                        //Handle events for receiver of the call
                    if (callStatus == VoIPCallStatus.CALL_MISSED) {
                        handleIncomingCallPermissions(isDriver);
                    } else if (callStatus == VoIPCallStatus.CALLEE_MICROPHONE_PERMISSION_NOT_GRANTED) {
                        handleIncomingCallPermissions(isDriver);
                    } else if (callStatus == VoIPCallStatus.CALLEE_MICROPHONE_PERMISSION_BLOCKED) {
                        handleIncomingCallPermissions(isDriver);
                    } else if (callStatus == VoIPCallStatus.CALL_DECLINED_DUE_TO_NOTIFICATIONS_DISABLED) {
                        handleIncomingCallPermissions(isDriver);
                    }
                }
             }
        });
    }
}