package in.juspay.mobility;

import static in.juspay.hyper.core.JuspayCoreLib.getApplicationContext;
import static in.juspay.mobility.Utils.getInnerPayload;
import static in.juspay.mobility.Utils.getService;

import android.content.Context;
import android.content.Intent;
import android.util.Log;
import android.view.View;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.fragment.app.FragmentActivity;

import com.google.firebase.analytics.FirebaseAnalytics;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.UUID;

import in.juspay.hypersdk.core.PaymentConstants;
import in.juspay.hypersdk.data.JuspayResponseHandler;
import in.juspay.hypersdk.ui.HyperPaymentsCallbackAdapter;
import in.juspay.services.HyperServices;

public class SDKHolder {
    private static HyperServices hyperServices;

    private boolean isInitiated = false;
    private Context context;
    private AppCompatActivity activity;
    private static  SDKHolder instance;

    private SDKHolder(Context context){
        hyperServices = new HyperServices(context);
    }

    public boolean isHSInitialized() {
        return isInitiated || hyperServices.isInitialised();
    }

    public void process(AppCompatActivity activity, JSONObject payload) {
        this.activity = activity;
        this.context = activity.getApplicationContext();
        if (isHSInitialized()) {
            hyperServices.process(activity,activity.findViewById(R.id.cl_dui_container) , payload);
        }else{

        }
    }

    public static SDKHolder getInstance(Context context){
        if (instance == null)
            instance = new SDKHolder(context);
        return instance;
    }

    public void terminateHyperService(){
        if(isHSInitialized()) {
            hyperServices.terminate();
        }else{

        }
    }

    public boolean onBackPressed(){
        return hyperServices.onBackPressed();
    }

    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults){
        hyperServices.onRequestPermissionsResult(requestCode, permissions, grantResults);
    }

    public void onActivityResult(int requestCode, int resultCode, @Nullable Intent data){
        hyperServices.onActivityResult(requestCode, resultCode, data);
    }

    public static JSONObject getInitiatePayload(){
        long initiateTimeStamp = System.currentTimeMillis();
        final JSONObject json = new JSONObject();
        JSONObject payload = new JSONObject();
        try {
            json.put("requestId", UUID.randomUUID());
            json.put("service", getService());
            json.put("betaAssets", false);
            payload = getInnerPayload(payload,"initiate", getApplicationContext());
            payload.put("onCreateTimeStamp", System.currentTimeMillis());
            payload.put("initiateTimeStamp" , initiateTimeStamp);
            json.put(PaymentConstants.PAYLOAD, payload);
        } catch (JSONException e) {
            e.printStackTrace();
        }
        return json;
    }

    public void initApp() {

        Log.i("APP_PERF", "INIT_APP_START : " + System.currentTimeMillis());
        Log.i("APP_PERF", "INIT_APP_HYPER_SERVICE_END : " + System.currentTimeMillis());

//        FirebaseAnalytics mFirebaseAnalytics = FirebaseAnalytics.getInstance(con);



//        mFirebaseAnalytics.logEvent("ny_hyper_initiate",null);
        Log.i("APP_PERF", "INIT_HYPER_SERVICE : " + System.currentTimeMillis());
        hyperServices.initiate(getInitiatePayload(), new HyperPaymentsCallbackAdapter() {
            @Override
            public void onEvent(JSONObject jsonObject, JuspayResponseHandler juspayResponseHandler) {
                Log.d("LOG_TAG", "onEvent: " + jsonObject.toString());
                String event = jsonObject.optString("event");
//                mFirebaseAnalytics.logEvent("ny_hyper_" + event,null);
                switch (event) {
                    case "initiate_result":
                        System.out.println("INITIATE RESULT CALL " + System.currentTimeMillis());
//                        Log.i("APP_PERF", "INITIATE_RESULT : " + System.currentTimeMillis());
//                        try {
//                            JSONObject innerPayload = json.getJSONObject(PaymentConstants.PAYLOAD);
//
//                            String viewParam = null, deepLinkJSON = null;
//                            JSONObject driverInfoResponse = null;
//                            if(preInitFutureTaskResult != null) {
//                                Log.i("APP_PERF", "PRE_INIT : " + System.currentTimeMillis());
//                                viewParam = preInitFutureTaskResult.optString("viewParam");
//                                deepLinkJSON = preInitFutureTaskResult.optString("deepLinkJSON");
//                            } else {
//                                try {
//                                    JSONObject preInitFutureTaskResult = preInitFutureTask.get(4500, TimeUnit.MILLISECONDS);
//                                    Log.i("APP_PERF", "PRE_INIT_NO_EXCEPTION : " + System.currentTimeMillis());
//                                    viewParam = preInitFutureTaskResult.optString("viewParam");
//                                    deepLinkJSON = preInitFutureTaskResult.optString("deepLinkJSON");
//                                } catch (InterruptedException | ExecutionException | TimeoutException e) {
//                                    preInitFutureTask.cancel(true);
//                                    JSONObject preInitFutureTaskResult = preInitFlow();
//                                    Log.i("APP_PERF", "PRE_INIT_EXCEPTION : " + System.currentTimeMillis());
//                                    viewParam = preInitFutureTaskResult.optString("viewParam");
//                                    deepLinkJSON = preInitFutureTaskResult.optString("deepLinkJSON");
//                                }
//                                try {
//                                    JSONObject driverInfoFutureTaskResult = driverInfoFutureTask.get(4500, TimeUnit.MILLISECONDS);
//                                    driverInfoResponse = driverInfoFutureTaskResult.optJSONObject("driverInfoResponse");
//                                    Log.i("APP_PERF", "PRE_INIT_CALL_API_NO_EXCEPTION : " + System.currentTimeMillis());
//                                } catch (InterruptedException | ExecutionException | TimeoutException e) {
//                                    driverInfoFutureTask.cancel(true);
//                                    Log.i("APP_PERF", "PRE_INIT_CALL_API_EXCEPTION : " + System.currentTimeMillis());
//                                    e.printStackTrace();
//                                }
//                            }
//                            Log.i("APP_PERF", "INIT_FUTURE_TASK_RESULT : " + System.currentTimeMillis());
//
//                            innerPayload.put("action", "process");
//                            innerPayload.put("viewParam", viewParam);
//                            innerPayload.put("view_param", viewParam);
//                            innerPayload.put("deepLinkJSON", deepLinkJSON);
//                            innerPayload.put("driverInfoResponse", driverInfoResponse);
//                            innerPayload.put("currentLocation", currentLocationRes);
//
//                            if (getIntent() != null) {
//                                setNotificationData(innerPayload, getIntent());
//                                handleGeoSchemeData(innerPayload, getIntent());
//                            }
//                            json.put(PaymentConstants.PAYLOAD, innerPayload);
//                            mFirebaseAnalytics.logEvent("ny_hyper_process", null);
//                            Log.i("APP_PERF", "INIT_HYPER_SERVICE_INITIATE_RESULT : " + System.currentTimeMillis());
//                            hyperServices.process(json);
//                        } catch (JSONException e) {
//                            throw new RuntimeException(e);
//                        }
                        break;
                    case "hide_loader":
                    case "hide_splash":
                        hideSplash();
                        break;
                    case "show_splash":
                        showSplash();
                        break;
//                    case "reboot":
//                        Log.i(LOG_TAG, "event reboot");
//                        mFirebaseAnalytics.logEvent("ny_hyper_terminate",null);
//                        hyperServices.terminate();
//                        hyperServices = null;
//                        initApp();
//                        break;
//                    case "gl_sdk" :
//                        in.juspay.mobility.Utils.onGullakEvent(jsonObject, MainActivity.this, sharedPref, startForResult );
//                        break;
//                    case "in_app_notification":
//                        showInAppNotificationApp(jsonObject, context);
//                        break;
//                    case "process_result":
//                        try {
//                            JSONObject innerPayload = jsonObject.getJSONObject(PaymentConstants.PAYLOAD);
//                            if (innerPayload.getString("action").equals("terminate")) {
//                                minimizeApp(context);
//                            }
//                        } catch (Exception ignored) {
//                        }
//                        break;
//                    case "log_stream":
//                        JSONObject payload;
//                        try {
//                            payload = jsonObject.getJSONObject("payload");
//                            HashMap<String, String> params = new HashMap<>();
//                            switch (payload.optString("label")) {
//                                case "current_screen":
//                                    params.put("screen_name", payload.getJSONObject("value").getString("screen_name"));
//                                    in.juspay.mobility.app.Utils.logEventWithParams("ny_driver_payment_current_screen", params ,context);
//                                    break;
//                                case "button_clicked":
//                                    params.put("button_name",payload.getJSONObject("value").getString("button_name"));
//                                    in.juspay.mobility.app.Utils.logEventWithParams("ny_driver_payment_button_clicked",params ,context);
//                                    break;
//                                case "upi_apps":
//                                    params.put("app_name",payload.getJSONObject("value").getString("appName"));
//                                    params.put("package_name",payload.getJSONObject("value").getString("packageName"));
//                                    in.juspay.mobility.app.Utils.logEventWithParams("ny_driver_payment_upi_app_selected",params ,context);
//                                    break;
//                                default:
//                            }
//                        } catch (JSONException e) {
//                            Log.e(LOG_TAG, "empty payload" + json);
//                        }
//                        break;
//                    case "launchHyperVerge":
//                        try {
//                            String cb = jsonObject.getString("callback");
//                            registeredCallBackForHV = cb;
//                            initHyperVergeSdk(jsonObject.getString("accessToken"), jsonObject.getString("workFlowId"), jsonObject.getString("transactionId"), jsonObject.getBoolean("useLocation"), jsonObject.getString("defLanguageCode"),jsonObject.getString("inputJson"));
//                        }
//                        catch (JSONException e) {
//                            Log.e("Error Occurred while calling Hyperverge SDK ", e.toString());
//                        }
//                        break;
//
                    default:
//                        Log.e(LOG_TAG, "json_payload" + json);
                }
            }
        });
        Log.i("APP_PERF", "INIT_HYPER_SERVICE_END : " + System.currentTimeMillis());
    }

    private void hideSplash() {
        View v = activity.findViewById(in.juspay.mobility.app.R.id.cl_dui_container);
        if (v != null) {
            activity.findViewById(in.juspay.mobility.app.R.id.cl_dui_container).setVisibility(View.VISIBLE);
        }
        View splashView = activity.findViewById(R.id.splash);
        if (splashView != null) {
            splashView.setVisibility(View.GONE);
        }
    }

    private void showSplash(){
        View v = activity.findViewById(R.id.splash);
        if (v != null) {
            activity.findViewById(R.id.splash).setVisibility(View.VISIBLE);
        }
    }
}
