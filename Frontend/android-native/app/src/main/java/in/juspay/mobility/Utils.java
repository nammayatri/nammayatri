package in.juspay.mobility;

import static android.app.Activity.RESULT_OK;

import static in.juspay.mobility.BuildConfig.MERCHANT_TYPE;
import static in.juspay.mobility.common.MobilityCommonBridge.isClassAvailable;
import in.juspay.mobility.app.CleverTapSignedCall;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.util.Log;

import androidx.activity.result.ActivityResult;
import androidx.activity.result.ActivityResultLauncher;

import com.clevertap.android.sdk.CleverTapAPI;
import com.clevertap.android.signedcall.fcm.SignedCallNotificationHandler;
import com.clevertap.android.signedcall.init.SignedCallAPI;
import com.clevertap.android.signedcall.interfaces.SCNetworkQualityHandler;
import in.juspay.mobility.app.RemoteConfigs.MobilityRemoteConfigs;
import com.google.android.play.core.splitinstall.SplitInstallHelper;
import com.google.android.play.core.splitinstall.SplitInstallManager;
import com.google.android.play.core.splitinstall.SplitInstallManagerFactory;
import com.google.android.play.core.splitinstall.SplitInstallRequest;
import com.google.android.play.core.splitinstall.SplitInstallStateUpdatedListener;
import com.google.android.play.core.splitinstall.model.SplitInstallErrorCode;
import com.google.android.play.core.splitinstall.model.SplitInstallSessionStatus;
import com.google.firebase.crashlytics.FirebaseCrashlytics;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.UUID;

import in.juspay.hypersdk.core.PaymentConstants;
import in.juspay.hypersdk.data.KeyValueStore;
import in.juspay.mobility.app.MissedCallActionsHandler;
import in.juspay.services.HyperServices;

public class Utils {

    public static void onGullakEvent(JSONObject jsonObject, Context context, SharedPreferences sharedPref, ActivityResultLauncher<Intent> activityResultLauncher){
        try {
            if (jsonObject.has("action") && jsonObject.has("innerPayload")) {
                JSONObject innerPayload = new JSONObject(jsonObject.getString("innerPayload"));
                String token = innerPayload.getString("param1");
                String cbIdentifier = jsonObject.getString("action");
                boolean installOnly = innerPayload.getString("param2").equals("true");
                if (sharedPref != null &&
                        (sharedPref.getString("GLSDK_INSTALLED", "false").equals("true") || sharedPref.getBoolean("GLSDK_INSTALLED", false)) &&
                        !installOnly){
                    Intent intent = new Intent();
                    intent.putExtra("token", token);
                    intent.putExtra("cbIdentifier", cbIdentifier);
                    intent.setClassName(context.getPackageName(), "in.juspay.mobility.dynamicfeature.DynamicActivity");
                    activityResultLauncher.launch(intent);
                }else {
                    initGlSdk(cbIdentifier,token, installOnly, context, activityResultLauncher, sharedPref);
                }
            }
        } catch (Exception exception) {
            FirebaseCrashlytics.getInstance().recordException(exception);
            exception.printStackTrace();
        }
    }


    public static void handleGlResp(ActivityResult result, Context context){
        if (result.getResultCode() == RESULT_OK) {
            Intent data = result.getData();
            if (data != null) {
                String responseJson = data.getStringExtra("responseJson"); // Parse the string response to a java class
                String cbIdentifier = data.getStringExtra("cbIdentifier");
                try {
                    JSONObject processPL = new JSONObject();
                    JSONObject innerPayload = getInnerPayload(new JSONObject(),"gl_process", context);
                    innerPayload.put("callback", cbIdentifier)
                            .put("value", responseJson);
                    processPL.put(PaymentConstants.PAYLOAD, innerPayload)
                            .put("requestId", UUID.randomUUID())
                            .put("service", getService());
                    MobilityServiceHolder.getInstance(context).process(processPL);
                } catch (JSONException e) {
                    FirebaseCrashlytics.getInstance().recordException(e);
                }
            }
        }
    }

    public static String getService() {
        if (MERCHANT_TYPE.equals("USER")) {
            return "in.yatri.consumer";
        } else {
            return "in.yatri.provider";
        }
    }

    public static JSONObject getInnerPayload(JSONObject payload, String action, Context context) throws JSONException{
        String appName = "";
        boolean loadDynamicModule = BuildConfig.includeDynamicFeature;
        try{
            appName = context.getApplicationInfo().loadLabel(context.getPackageManager()).toString();
        }catch (Exception e){
            e.printStackTrace();
        }
        payload.put("clientId", context.getResources().getString(R.string.client_id));
        payload.put("merchantId", context.getResources().getString(R.string.merchant_id));
        payload.put("appName", appName);
        payload.put("action", action);
        payload.put("logLevel",1);
        payload.put("isBootable",true);
        payload.put(PaymentConstants.ENV, "prod");
        int bundleTimeOut = Integer.parseInt(KeyValueStore.read(context,context.getString(in.juspay.mobility.app.R.string.preference_file_key),"BUNDLE_TIME_OUT","500"));
        payload.put("bundleTimeOut",bundleTimeOut);
        payload.put("loadDynamicModule", loadDynamicModule);
        return payload;
    }

    public static void initGlSdk(String cbIdentifier, String token, boolean installOnly, Context context, ActivityResultLauncher<Intent> activityResultLauncher, SharedPreferences sharedPref){
        SplitInstallManager splitInstallManager =
                SplitInstallManagerFactory.create(context);


// Creates a request to install a module.
        SplitInstallRequest request =
                SplitInstallRequest
                        .newBuilder()
                        .addModule("dynamicfeature")
                        .build();

// Initializes a variable to later track the session ID for a given request.
        int mSessionId = 0;

// Creates a listener for request status updates.
        SplitInstallStateUpdatedListener listener = state -> {
//            if (state.sessionId() == mSessionId) {
                // Read the status of the request to handle the state update.
                if (state.status() == SplitInstallSessionStatus.FAILED
                        && state.errorCode() == SplitInstallErrorCode.SERVICE_DIED) {
                    // Retry the request.
                    return;
                }
//                if (state.sessionId() == mSessionId) {
                    switch (state.status()) {
                        case SplitInstallSessionStatus.DOWNLOADING:
                            long totalBytes = state.totalBytesToDownload();
                            long progress = state.bytesDownloaded();
                            // Update progress bar.
                            Log.d("SplitInstallSessionStatus", "totalbytes->"+totalBytes+" progress->"+progress);
                            break;

                        case SplitInstallSessionStatus.INSTALLED:
                            if (sharedPref!= null) sharedPref.edit().putString("GLSDK_INSTALLED", "true").apply();
                            if (!installOnly){
                                Intent intent = new Intent();
                                intent.putExtra("token", token);
                                intent.setClassName(context.getPackageName(), "in.juspay.mobility.dynamicfeature.DynamicActivity");
                                intent.putExtra("cbIdentifier", cbIdentifier);
                                activityResultLauncher.launch(intent);
                            }
                            break;
                    }
//                }
//            }
        };

        // Registers the listener.
        splitInstallManager.registerListener(listener);
        splitInstallManager
                .startInstall(request)
                .addOnSuccessListener(sessionId -> {
                    Log.d("successSessionId", "" + sessionId);
                    SplitInstallHelper.loadLibrary(context, "jsinspector");
                    SplitInstallHelper.loadLibrary(context, "jscexecutor");
                    SplitInstallHelper.loadLibrary(context, "turbomodulejsijni");
                    SplitInstallHelper.loadLibrary(context, "imagepipeline");
                    SplitInstallHelper.loadLibrary(context, "reactnativeblob");
                    SplitInstallHelper.loadLibrary(context, "native-imagetranscoder");
                    SplitInstallHelper.loadLibrary(context, "logger");
                    SplitInstallHelper.loadLibrary(context, "yoga");
                    SplitInstallHelper.loadLibrary(context, "fbjni");
                    SplitInstallHelper.loadLibrary(context, "reactnativejni");
                })
                .addOnFailureListener(exception -> FirebaseCrashlytics.getInstance().recordException(exception));
    }
    public static void initCTSignedCall(Context context, Activity activity, MobilityRemoteConfigs remoteConfigs){
        String SC_ACCOUNT_ID = in.juspay.mobility.BuildConfig.CLEVERTAP_SC_ACCOUNT_ID;
        String SC_API_KEY = in.juspay.mobility.BuildConfig.CLEVERTAP_SC_API_KEY;
        CleverTapSignedCall cleverTapSignedCall = new CleverTapSignedCall(context, activity, true, SC_API_KEY, SC_ACCOUNT_ID);
        CleverTapAPI.setSignedCallNotificationHandler(new SignedCallNotificationHandler());
        if (BuildConfig.DEBUG) {
            SignedCallAPI.setDebugLevel(SignedCallAPI.LogLevel.VERBOSE);
        }        
        SignedCallAPI.getInstance().setMissedCallNotificationOpenedHandler(new MissedCallActionsHandler(context,activity));
        SignedCallAPI.getInstance().setNetworkQualityCheckHandler(new SCNetworkQualityHandler() {
            @Override
            public boolean onNetworkQualityResponse(final int score) {
                Log.d("SC", "Signed Call Network quality score: " + score);
                JSONObject voipCallConfig = null;
                int scoreThreshold = 70;
                try {
                    voipCallConfig = new JSONObject(remoteConfigs.getString("voip_call_config"));
                    scoreThreshold = voipCallConfig.optInt("score",70);
                } catch (JSONException e) {
                    Log.d("SC","Failed to fetch voip call config");
                }
                return score >= scoreThreshold;
            }
        });  
    }
}
