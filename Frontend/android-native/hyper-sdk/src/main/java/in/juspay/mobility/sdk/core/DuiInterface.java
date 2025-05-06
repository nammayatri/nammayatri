package in.juspay.mobility.sdk.core;

import static java.lang.Integer.parseInt;
import static in.juspay.mobility.sdk.core.PaymentConstants.DELIVER_SMS;
import static in.juspay.mobility.sdk.core.PaymentConstants.NETWORK_STATUS;
import static in.juspay.mobility.sdk.core.PaymentConstants.SEND_SMS;
import static in.juspay.mobility.sdk.core.PaymentConstants.SMS_CONSENT;
import static in.juspay.mobility.sdk.core.PaymentConstants.SMS_RECEIVE;
import static in.juspay.mobility.sdk.core.PaymentConstants.SMS_RETRIEVER;

import android.Manifest;
import android.app.Activity;
import android.app.KeyguardManager;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.util.Base64;
import android.view.View;
import android.view.ViewGroup;
import android.view.WindowManager;
import android.view.inputmethod.InputMethodManager;
import android.webkit.CookieSyncManager;
import android.webkit.JavascriptInterface;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import in.juspay.mobility.sdk.hyper.constants.Labels;
import in.juspay.mobility.sdk.hyper.constants.LogCategory;
import in.juspay.mobility.sdk.hyper.constants.LogLevel;
import in.juspay.mobility.sdk.hyper.constants.LogSubCategory;
import in.juspay.mobility.sdk.hyper.core.CallbackInvoker;
import in.juspay.mobility.sdk.hyper.core.ExecutorManager;
import in.juspay.mobility.sdk.hyper.core.JuspayLogger;
import in.juspay.mobility.sdk.hyper.core.ResultAwaitingDuiHook;
import in.juspay.mobility.sdk.services.RemoteAssetService;
import in.juspay.mobility.sdk.ui.HyperPaymentsCallback;
import in.juspay.mobility.sdk.data.JuspayResponseHandler;
import in.juspay.mobility.sdk.data.KeyValueStore;
import in.juspay.mobility.sdk.data.SessionInfo;
import in.juspay.mobility.sdk.hypersmshandler.JuspayDuiHook;
import in.juspay.mobility.sdk.hypersmshandler.OnResultHook;
import in.juspay.mobility.sdk.hypersmshandler.SmsEventInterface;


/**
 * Created by sahil on 14/03/17.
 */

@SuppressWarnings("WeakerAccess")
public class DuiInterface extends HyperJsInterface implements CallbackInvoker {
    private static final String LOG_TAG = "DuiInterface";

    @Nullable
    protected Activity activity;
    @Nullable
    private ViewGroup container;
    @NonNull
    private final Context context;

    @NonNull
    private final SdkTracker sdkTracker;
    @NonNull
    protected final SessionInfo sessionInfo;
    @NonNull
    private final RemoteAssetService remoteAssetService;

    @NonNull
    private final ArrayList<Integer> merchantViewIds = new ArrayList<>();

    @NonNull
    protected Map<String, Object> listenerMap;

    @NonNull
    protected Map<String, String> callBackMapper;

    private int lastFocusedEditView = -1;

    public DuiInterface(@NonNull JuspayServices juspayServices) {
        super(juspayServices);

        this.context = juspayServices.getContext();
        this.sdkTracker = juspayServices.getSdkTracker();
        this.sessionInfo = juspayServices.getSessionInfo();
        this.remoteAssetService = juspayServices.getRemoteAssetService();

        listenerMap = new HashMap<>();
        callBackMapper = new HashMap<>();
    }

    SmsEventInterface getSmsEventInterface() {
        return new SmsEventInterface() {
            @Override
            public void onSmsReceiverEvent(@NonNull String data) {
                invokeCallbackInDUIWebview(callBackMapper.get(PaymentConstants.SMS_RECEIVE), data);
            }

            @Override
            public void onSmsConsentEvent(@NonNull Intent intent, int requestCode, @Nullable Bundle bundle) {
                juspayServices.startActivityForResult(intent, requestCode, bundle);
                invokeFnInDUIWebview("onSMSConsentShown", "{ }");
            }

            @Override
            public void onActivityResultEvent(@NonNull String data) {
                invokeCallbackInDUIWebview(callBackMapper.get(PaymentConstants.SMS_CONSENT), data);
            }

            @Override
            public void onSentReceiverEvent(int resultCode) {
                if (callBackMapper.get(PaymentConstants.SEND_SMS) != null) {
                    String callBackCommand = "window.callUICallback(\"" + callBackMapper.get(PaymentConstants.SEND_SMS);
                    switch (resultCode) {
                        case Activity.RESULT_OK:
                            Toast.makeText(context, "SMS SENT", Toast.LENGTH_SHORT).show();
                            callBackCommand += "\", \"SUCCESS\")";
                            break;
                        case android.telephony.SmsManager.RESULT_ERROR_GENERIC_FAILURE:
                            Toast.makeText(context, "SMS GENERIC FAILURE", Toast.LENGTH_SHORT).show();
                            callBackCommand += "\", \"Generic failure\", \"837\")";
                            break;
                        case android.telephony.SmsManager.RESULT_ERROR_NO_SERVICE:
                            Toast.makeText(context, "SMS NO SERVICE", Toast.LENGTH_SHORT).show();
                            callBackCommand += "\", \"No service\", \"838\")";
                            break;
                        case android.telephony.SmsManager.RESULT_ERROR_NULL_PDU:
                            Toast.makeText(context, "SMS NULL PDU", Toast.LENGTH_SHORT).show();
                            callBackCommand += "\", \"Null PDU\", \"839\")";
                            break;
                        case android.telephony.SmsManager.RESULT_ERROR_RADIO_OFF:
                            Toast.makeText(context, "SMS RADIO OFF", Toast.LENGTH_SHORT).show();
                            callBackCommand += "\", \"Radio off\", \"840\")";
                            break;
                        default:
                            callBackCommand += "\", \"Unable to Send SMS\", \"837\")";
                    }
                    invokeFnInDUIWebview(callBackCommand);
                }
            }

            @Override
            public void onSmsRetrieverEvent(@NonNull RetrieverEvents event, @NonNull String data) {
                switch (event) {
                    case ON_ATTACH:
                        invokeCallbackInDUIWebview(callBackMapper.get(PaymentConstants.SMS_RETRIEVER + RetrieverEvents.ON_ATTACH), data);
                        break;
                    case ON_EXECUTE:
                        invokeCallbackInDUIWebview(callBackMapper.get(PaymentConstants.SMS_RETRIEVER + RetrieverEvents.ON_RECEIVE), data);
                        break;
                    case ON_RECEIVE:
                        invokeCallbackInDUIWebview(callBackMapper.get(PaymentConstants.SMS_RETRIEVER + RetrieverEvents.ON_RECEIVE), data);
                        if (!"TIMEOUT".equals(data)) {
                            callBackMapper.put(PaymentConstants.SMS_RETRIEVER + RetrieverEvents.ON_RECEIVE, null);
                        }
                        break;
                }
            }
        };
    }

    public void setActivity(@Nullable Activity activity) {
        this.activity = activity;
    }

    public void setContainer(@Nullable ViewGroup container) {
        this.container = container;
    }

    @JavascriptInterface
    public double getPixels() {
        return context.getResources().getDisplayMetrics().density;
    }

    @JavascriptInterface
    public void closeBrowser(String reason) {
        reset();
    }

    @JavascriptInterface
    public String checkReadSMSPermission() {
        JSONObject result = new JSONObject();
        try {
            result.put(Manifest.permission.READ_SMS.replace("android.permission.", ""), PaymentUtils.checkIfGranted(juspayServices, Manifest.permission.READ_SMS));
            result.put(Manifest.permission.RECEIVE_SMS.replace("android.permission.", ""), PaymentUtils.checkIfGranted(juspayServices, Manifest.permission.RECEIVE_SMS));
        } catch (JSONException e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.READ_SMS_PERMISSION, "Error while inserting in json", e);
        }
        return result.toString();
    }

    @JavascriptInterface
    public String checkPermission(String[] permissions) {
        JSONObject result = new JSONObject();
        try {
            for (String permission : permissions) {
                // Adding replace for backward compatibility with old interfaces
                result.put(permission.replace("android.permission.", ""), PaymentUtils.checkIfGranted(juspayServices, permission));
            }
        } catch (JSONException e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.READ_SMS_PERMISSION, "Error while inserting in json", e);
        }
        return result.toString();
    }

    @JavascriptInterface
    public void requestSMSPermission(String callbackFunctionName) {
        requestPermission(new String[]{Manifest.permission.READ_SMS, Manifest.permission.RECEIVE_SMS}, String.valueOf(PaymentConstants.REQUEST_SMS_PERMISSION), callbackFunctionName);
    }

    @JavascriptInterface
    public void revokePermissions(String[] permissions) {
        try {
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
                Collection<String> permissionsArr = new ArrayList<>();
                for (String permission : permissions) {
                    permissionsArr.add(permission);
                }
                context.revokeSelfPermissionsOnKill(permissionsArr);
            }
        } catch (Exception e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.PERMISSION, "Error while revoking permission", e);
        }
    }

    @JavascriptInterface
    public void requestPermission(String[] permissions, String permissionId, String callbackFunctionName) {
        juspayServices.requestPermission(permissions, parseInt(permissionId));
        listenerMap.put(PaymentConstants.REQUEST_PERMISSION_PREFIX + permissionId, callbackFunctionName);
    }

    public void requestPermission(String[] permissions, String permissionId, Handler.Callback callback) {
        juspayServices.requestPermission(permissions, parseInt(permissionId));
        listenerMap.put(PaymentConstants.REQUEST_PERMISSION_PREFIX + permissionId, callback);
    }

    public void onRequestPermissionsResult(int requestCode, String[] permissions, int[] grantResults) {
        try {
            Object subscriber = listenerMap.get(PaymentConstants.REQUEST_PERMISSION_PREFIX + requestCode);
            if (subscriber instanceof String) {
                JSONObject permissionResult = new JSONObject();
                for (int i = 0; i < permissions.length; i++) {
                    permissionResult.put(permissions[i].replace("android.permission.", ""), (grantResults[i] == PackageManager.PERMISSION_GRANTED));
                }
                String callbackFunctionName = (String) subscriber;
                invokeCallbackInDUIWebview(callbackFunctionName, permissionResult.toString());
            } else if (subscriber instanceof Handler.Callback) {
                Message msg = Message.obtain();
                msg.obj = grantResults;
                ((Handler.Callback) subscriber).handleMessage(msg);
            } else {
                JuspayLogger.e(LOG_TAG, "callback instance not understandable");
                sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.ERROR, Labels.System.ON_REQUEST_PERMISSION_RESULT, subscriber + " : onRequestPermissionsResult callback instance not understandable", JSONObject.NULL);
            }
        } catch (Exception e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.ON_REQUEST_PERMISSION_RESULT, "Error while inserting in json", e);
        }
    }

    // Can return `"true"`, `"false"` or `null`. Make the `null` case is handled
    @JavascriptInterface
    @Nullable
    public String shouldShowRequestPermissionRationale(String permission) {
        try {
            if (activity != null && Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
                return String.valueOf(activity.shouldShowRequestPermissionRationale(permission));
            }
        } catch (Exception e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.PERMISSION, "Exception while checking shouldShowRequestPermissionRationale", e);
        }

        return null;
    }

    public void onActivityResult(int requestCode, int resultCode, @Nullable Intent data) {
        for (Object listener : listenerMap.values()) {
            if (listener instanceof ResultAwaitingDuiHook) {
                if (((ResultAwaitingDuiHook) listener).onActivityResult(requestCode, resultCode, data)) {
                    // Hook consumed the result. Do not pass it to other hooks or the micro-app.
                    sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.JBRIDGE, "on_activity_result", "Result consumed by ResultAwaitingDuiHook " + listener.getClass().getName());
                    return;
                }
            }
            if (listener instanceof OnResultHook) {
                if (((OnResultHook) listener).onActivityResult(requestCode, resultCode, data)) {
                    // Hook consumed the result. Do not pass it to other hooks or the micro-app.
                    sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.JBRIDGE, "on_activity_result", "Result consumed by OnResultHook " + listener.getClass().getName());
                    return;
                }
            }
        }

        if (data != null) {
            JSONObject jsonObject = PaymentUtils.toJSON(data.getExtras());
            String encoded = Base64.encodeToString(jsonObject.toString().getBytes(), Base64.NO_WRAP);

            sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.JBRIDGE, "on_activity_result", "Passing data to micro-app. Data is " + jsonObject);
            invokeFnInDUIWebview("window.onActivityResult('" + requestCode + "', '" + resultCode + "', atob('" + encoded + "'))");
        } else {
            sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.JBRIDGE, "on_activity_result", "Got empty data in onActivityResult. Passing callback to micro-app.");
            invokeFnInDUIWebview("window.onActivityResult('" + requestCode + "', '" + resultCode + "', '{}')");
        }
    }

    @JavascriptInterface
    public boolean isHookSupported(String hookId) {
        switch (hookId) {
            case SMS_RECEIVE:
            case SMS_CONSENT:
            case NETWORK_STATUS:
            case SEND_SMS:
            case DELIVER_SMS:
            case SMS_RETRIEVER:
                return true;

            default:
                return false;
        }
    }

    @JavascriptInterface
    public void attach(String id, String argumentsJson, String callbackFunctionName) {
        if (!isHookSupported(id)) {
            JuspayLogger.e(LOG_TAG, "Unsupported hook: " + id);
            return;
        }

        try {
            detach(new String[]{id});
            JuspayDuiHook juspayDuiHook = null;

            switch (id) {
                case SMS_CONSENT:
                    juspayDuiHook = juspayServices.getSmsServices().createSmsReceiverForConsent();
                    break;
                case SMS_RECEIVE:
                    juspayDuiHook = juspayServices.getSmsServices().createSMSReceiver();
                    break;
                case NETWORK_STATUS:
                    juspayDuiHook = PaymentUtils.getConnectivityReceiver(juspayServices);
                    break;
                case SEND_SMS:
                    juspayDuiHook = juspayServices.getSmsServices().createSendSMSReceiver();
                    break;
                case DELIVER_SMS:
                    juspayDuiHook = juspayServices.getSmsServices().createDeliveredSMSReceiver();
                    break;
                case SMS_RETRIEVER:
                    juspayDuiHook = juspayServices.getSmsServices().createSmsRetriever();
                    break;
                default:
                    juspayServices.sdkDebug(LOG_TAG, "Unknown Hook: " + id);
                    break;
            }

            if (juspayDuiHook != null && activity != null) {
                listenerMap.put(id, juspayDuiHook);
                if (SMS_RETRIEVER.equals(id)) {
                    callBackMapper.put(id + SmsEventInterface.RetrieverEvents.ON_ATTACH, callbackFunctionName);
                } else {
                    callBackMapper.put(id, callbackFunctionName);
                }
                juspayDuiHook.attach(activity);
            } else {
                juspayServices.sdkDebug(LOG_TAG, "Nothing to attach");
            }
        } catch (Exception e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.JBRIDGE, "Error while retrieving arguments", e);
        }
    }

    @JavascriptInterface
    public String execute(String id, String operation, String argumentsJson, String callbackFunctionName) {
        try {
            JSONObject jsonObject = new JSONObject(argumentsJson);
            if (listenerMap.containsKey(id) && activity != null) {
                JuspayDuiHook hook = ((JuspayDuiHook) listenerMap.get(id));
                if (hook == null) {
                    return "__failed";
                }
                if (SMS_RETRIEVER.equals(id)) {
                    if ("getOtp".equals(operation)) {
                        callBackMapper.put(SMS_RETRIEVER + SmsEventInterface.RetrieverEvents.ON_RECEIVE, callbackFunctionName);
                    } else if ("cancel".equals(operation)) {
                        callBackMapper.put(SMS_RETRIEVER + SmsEventInterface.RetrieverEvents.ON_RECEIVE, null);
                    }
                }
                return hook.execute(activity, operation, jsonObject);
            }
        } catch (JSONException e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.JBRIDGE, "Error while executing " + operation + " with args " + argumentsJson, e);
        }
        return "";
    }

    @JavascriptInterface
    public void detach(String[] ids) {
        for (String id : ids) {
            if (listenerMap.containsKey(id) && activity != null) {
                if (listenerMap.get(id) instanceof JuspayDuiHook) {
                    JuspayDuiHook hook = ((JuspayDuiHook) listenerMap.get(id));
                    if (hook != null) {
                        hook.detach(activity);
                    }
                }
                listenerMap.remove(id);
            }
        }
    }

    /**
     * Function is to be called on activity switch, as we do not get any other place where we can clear the merchantViews
     */
    public void clearMerchantViews(final Activity a) {
        if (a == null) return;
        ExecutorManager.runOnMainThread(() -> {
            for (Integer i : merchantViewIds) {
                View merchantViewParent = a.findViewById(i);
                if (merchantViewParent instanceof ViewGroup) {
                    ((ViewGroup) merchantViewParent).removeAllViews();
                }
            }
        });
    }

    @JavascriptInterface
    public void attachMerchantView(final int containerId, final String viewType) {
        if (juspayServices.getHyperCallback() != null) {
            ExecutorManager.runOnMainThread(() -> {
                try {
                    ViewGroup viewGroup = null;
                    if (activity != null) {
                        viewGroup = activity.findViewById(containerId);
                    }
                    if (viewGroup == null && container != null) {
                        viewGroup = container.findViewById(containerId);
                    }
                    final HyperPaymentsCallback juspayCallback = juspayServices.getHyperCallback();
                    if (viewGroup != null && juspayCallback != null) {
                        merchantViewIds.add(containerId);
                        MerchantViewType merchantViewType = MerchantViewType.valueOf(viewType);
                        View merchantFragment = juspayCallback.getMerchantView(viewGroup, merchantViewType);
                        if (merchantFragment != null) {
                            viewGroup.addView(merchantFragment);
                        }
                    }
                } catch (Exception e) {
                    sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.JBRIDGE, "Error while attaching merchant view", e);
                }
            });
        }
    }

    @JavascriptInterface
    public String findViewType(String id) {
        try {
            View view = null;
            if (juspayServices.getContainer() != null) {
                view = juspayServices.getContainer().findViewById(parseInt(id));
            }
            if (view == null && activity != null) {
                view = activity.findViewById(parseInt(id));
            }
            if (view != null) {
                return view.getClass().getName();
            }
        } catch (Exception ignored) {
        }
        return "";
    }

    @Override
    @JavascriptInterface
    public String getSessionInfo() {
        juspayServices.getSessionInfo().createSessionDataMap();
        return sessionInfo.getSessionData().toString();
    }

    @Override
    @JavascriptInterface
    public void invokeCallbackInDUIWebview(@Nullable String methodName, @Nullable String argumentsJson) {
        if (methodName == null) {
            return;
        }
        if (argumentsJson == null) {
            argumentsJson = "null";
        }
        String encoded = Base64.encodeToString(argumentsJson.getBytes(), Base64.NO_WRAP);
        String command = String.format("window.callUICallback('%s',atob('%s'));", methodName, encoded);
        juspayServices.getDynamicUI().addJsToWebView(command);
    }

    @JavascriptInterface
    public void invokeFnInDUIWebview(String methodName, String argumentsJson) {
        String encoded = Base64.encodeToString(argumentsJson.getBytes(), Base64.NO_WRAP);
        String command = String.format("window[\"onEvent'\"]('%s',atob('%s'))", methodName, encoded);
        juspayServices.getDynamicUI().addJsToWebView(command);
    }

    @JavascriptInterface
    public void invokeCustomFnInDUIWebview(String command) {
        juspayServices.getDynamicUI().addJsToWebView(command);
    }

    @Override
    @JavascriptInterface
    public void invokeFnInDUIWebview(@NonNull String cmd) {
        invokeCustomFnInDUIWebview(cmd);
    }

    @JavascriptInterface
    public String getSessionAttribute(String key) {
        return getSessionAttribute(key, "");
    }

    @JavascriptInterface
    public void setSessionInfo() {
    }

    @JavascriptInterface
    public String fetchFromInbox(String query) {
        return juspayServices.getSmsServices().fetchSms(query, "inbox", null);
    }

    @JavascriptInterface
    public void fetchSMS(String query, String smsContentUri, String additionalQuery, String callback) {
        String result = juspayServices.getSmsServices().fetchSms(query, smsContentUri, additionalQuery);
        invokeCallbackInDUIWebview(callback, result);
    }

    @JavascriptInterface
    public String getClipboardItems() {
        // Removed ClipboardListener class as it is flagged by google as violation
        return "[]";
    }

    public void reset() {
        try {
            ArrayList<String> idsToRemove = new ArrayList<>();
            for (String id : listenerMap.keySet()) {
                if (listenerMap.get(id) instanceof JuspayDuiHook) {
                    idsToRemove.add(id);
                }
            }
            detach(idsToRemove.toArray(new String[0]));
            container = null;
        } catch (Exception e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.JBRIDGE, "Exception while removing Dui Hooks", e);
        }
    }

    @JavascriptInterface
    public boolean isOnline() {
        ConnectivityManager connMgr = null;
        if (activity != null) {
            connMgr = (ConnectivityManager) activity.getSystemService(Context.CONNECTIVITY_SERVICE);
        }
        if (connMgr != null) {
            NetworkInfo networkInfo = connMgr.getActiveNetworkInfo();
            return (networkInfo != null && networkInfo.isConnected());
        }
        return false;
    }

    @JavascriptInterface
    public void addDataToSharedPrefs(String key, String value) {
        KeyValueStore.write(juspayServices, key, value);
    }

    @JavascriptInterface
    public String getDataFromSharedPrefs(String key, String defaultValue) {
        return KeyValueStore.read(juspayServices, key, defaultValue);
    }

    @JavascriptInterface
    public String getKeysInSharedPrefs() {
        try {
            JSONArray array = new JSONArray();
            for (String key : KeyValueStore.getAll(juspayServices).keySet()) {
                array.put(key);
            }
            return array.toString();
        } catch (Exception e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.SHARED_PREF, "Exception while getting all keys from shared prefs", e);
            return "[]";
        }
    }

    @JavascriptInterface
    public String getNetworkType() {
        return sessionInfo.getNetworkInfo();
    }

    @JavascriptInterface
    public void updateLoaded(String args, String status) {

        final SdkTracker sdkTracker = juspayServices.getSdkTracker();
        String fileName = "";
        try {
            JSONObject json = new JSONObject(args);
            if (json.has("fileName")) {
                fileName = json.getString("fileName");
            }

            // have to store that this mapp is successfully loaded
            String has = getDataFromSharedPrefs(PaymentConstants.JP_HASH_AND_STATUS, "{}");
            JSONObject hash_and_status = new JSONObject(has);

            JSONObject mappStatus = new JSONObject();
            if (hash_and_status.has(fileName)) {
                mappStatus = hash_and_status.getJSONObject(fileName);
            } else {
                sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.CRITICAL, Labels.HyperSdk.AUTO_FALLBACK, "loaded", "hash doesn't have a filename");
                // this should never happen
            }

            mappStatus.put("status", status); // this updates the status of the particular file

            hash_and_status.put(fileName, mappStatus);
            addDataToSharedPrefs(PaymentConstants.JP_HASH_AND_STATUS, hash_and_status.toString());
            JuspayLogger.d(LOG_TAG, "udpateLoaded: ");
        } catch (Exception e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.LIFECYCLE, LogSubCategory.LifeCycle.HYPER_SDK, Labels.HyperSdk.AUTO_FALLBACK, "Exception while updating the loaded status for file " + fileName, e);
        }
    }

    private void trackWebViewEvent(String label) {
        sdkTracker.trackLifecycle(
                LogSubCategory.LifeCycle.HYPER_SDK,
                LogLevel.INFO,
                label,
                "message",
                "Received event from web view."
        );
    }

    @JavascriptInterface
    public void storeActivityData(String key, String value) {
        juspayServices.getDynamicUI().storeActivityData(key, value);
    }

    @JavascriptInterface
    public String getActivityData(String key) {
        return juspayServices.getDynamicUI().getActivityData(key);
    }

    @JavascriptInterface
    public void enableWebViewRecreate(String bool) {
        juspayServices.getDynamicUI().setWebViewRecreate(bool.equals("true"));
    }

    @JavascriptInterface
    public void runInJuspayBrowser(final String method, final String args, final String cb) {
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();
        switch (method) {
            case "onError":
                updateLoaded(args, "not_loaded");
                break;
            case "onScriptError":
                try {
                    final JSONObject jsonArgs = new JSONObject(args);
                    final String app = jsonArgs.optString("app", "");
                    final String serializedError = jsonArgs.optString("serializedError", "");
                    String message = String.format(
                            "Failed to load script for app: '%s', due to error: %s",
                            app,
                            serializedError
                    );
                    sdkTracker.trackLifecycle(
                            LogSubCategory.LifeCycle.HYPER_SDK,
                            LogLevel.ERROR,
                            Labels.HyperSdk.ON_SCRIPT_ERROR,
                            "message",
                            message
                    );
                } catch (JSONException e) {
                    sdkTracker.trackAndLogException(
                            LOG_TAG,
                            LogCategory.LIFECYCLE,
                            LogSubCategory.LifeCycle.HYPER_SDK,
                            Labels.HyperSdk.RUN_IN_JUSPAY_BROWSER,
                            "error while dealing with json onEvent", e
                    );
                }
                break;
            case "onBundleLoaded":
                updateLoaded(args, "loaded");
                break;
            case "onStartWaitingDialogCreated":
                ExecutorManager.runOnMainThread(() -> {
                    if (activity != null) {
                        try {
                            View view = activity.findViewById(parseInt(args));
                            if (juspayServices.getHyperCallback() != null) {
                                juspayServices.getHyperCallback().onStartWaitingDialogCreated(view);
                            }
                        } catch (Exception e) {
                            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.LIFECYCLE, LogSubCategory.LifeCycle.HYPER_SDK, Labels.HyperSdk.RUN_IN_JUSPAY_BROWSER, "Exception while trying to find a view", e);
                        }
                    }
                });
                break;
            /*
             * This event is fired once the HTML for JOS is loaded & indicates that JOS is ready to
             * boot. JOS is booted once initiate is called by the merchant, & in-case the merchant
             * initiates before this event is received, the boot is blocked until this event is
             * emitted.
             * */
            case "onHtmlReady":
                trackWebViewEvent(Labels.HyperSdk.ON_HTML_READY);
                juspayServices.getDynamicUI().setWebViewActive();
                break;
            case "onEvent":
                if (!juspayServices.isPrefetch()) {
                    try {
                        JSONObject json;
                        json = new JSONObject(args);
                        /*
                         * `onJOSReady` is emitted once the JS for JOS is evaluated & it's ready
                         * to receive tasks.
                         * */
                        if (json.optString("event", "").equals("onJOSReady")) {
                            trackWebViewEvent(Labels.HyperSdk.ON_JOS_READY);
                            if (juspayServices.getBundleParameters() != null) {
                                juspayServices.onMerchantEvent("initiate", juspayServices.getBundleParameters());
                            }
                            break;
                            /*
                             * `DUI_READY` is sent once the root micro-app(i.e. the micro-app who's
                             * service was requested) is booted.
                             * */
                        } else if (json.optString("action", "").equals("DUI_READY")) {
                            trackWebViewEvent(Labels.HyperSdk.DUI_READY);
                        }

                    } catch (JSONException e) {
                        sdkTracker.trackAndLogException(LOG_TAG, LogCategory.LIFECYCLE, LogSubCategory.LifeCycle.HYPER_SDK, Labels.HyperSdk.RUN_IN_JUSPAY_BROWSER, "error while dealing with json onEvent", e);
                    }

                    ExecutorManager.runOnMainThread(() -> {
                        try {
                            if (juspayServices.getHyperCallback() != null) {
                                JSONObject json = new JSONObject(args);
                                juspayServices.getHyperCallback().onEvent(json, new JuspayResponseHandler() {
                                    @Override
                                    public void onResponse(String response) {
                                        try {
                                            JSONObject jsonObject = new JSONObject();
                                            jsonObject.put("status", "onResponse");

                                            try {
                                                JSONObject content = new JSONObject(response);
                                                jsonObject.put("payload", content);
                                            } catch (Exception e) {
                                                jsonObject.put("payload", response);
                                            }

                                            invokeCallbackInDUIWebview(cb, jsonObject.toString());
                                        } catch (Exception e) {
                                            sdkTracker.trackException(LogCategory.LIFECYCLE, LogSubCategory.LifeCycle.HYPER_SDK, Labels.HyperSdk.RUN_IN_JUSPAY_BROWSER, "Exception while manipulating JSON", e);
                                        }
                                    }

                                    @Override
                                    public void onResponse(Bundle bundle) {
                                        JSONObject jsonObject = PaymentUtils.toJSON(bundle);
                                        onResponse(jsonObject.toString());
                                    }

                                    @Override
                                    public void onError(String error) {
                                        try {
                                            JSONObject jsonObject = new JSONObject();
                                            jsonObject.put("status", "onError");

                                            try {
                                                JSONObject content = new JSONObject(error);
                                                jsonObject.put("payload", content);
                                            } catch (Exception e) {
                                                jsonObject.put("payload", error);
                                            }

                                            invokeCallbackInDUIWebview(cb, jsonObject.toString());
                                        } catch (Exception e) {
                                            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.LIFECYCLE, LogSubCategory.LifeCycle.HYPER_SDK, Labels.HyperSdk.RUN_IN_JUSPAY_BROWSER, "Exception while manipulating JSON", e);
                                        }
                                    }

                                    @Override
                                    public void run() {

                                    }
                                });
                            }
                        } catch (Exception e) {
                            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.LIFECYCLE, LogSubCategory.LifeCycle.HYPER_SDK, Labels.HyperSdk.RUN_IN_JUSPAY_BROWSER, "Exception in onEvent handler", e);
                        }
                    });
                } else {
                    //It should be pre-fetch
                    try {
                        JSONObject json;
                        json = new JSONObject(args);
                        if (Objects.equals(json.optString("event"), "prefetch_result")) {
                            ExecutorManager.runOnMainThread(juspayServices::terminate);
                        }
                        if (json.getString("event").equals("onJOSReady")) {
                            JSONObject params = juspayServices.getBundleParameters();
                            if (params != null) {
                                String command = String.format("window.onMerchantEvent('%s',atob('%s'));", "prefetch", Base64.encodeToString(params.toString().getBytes(), Base64.NO_WRAP));
                                invokeCustomFnInDUIWebview(command);
                            }
                        }
                    } catch (JSONException e) {
                        sdkTracker.trackAndLogException(LOG_TAG, LogCategory.LIFECYCLE, LogSubCategory.LifeCycle.HYPER_SDK, Labels.HyperSdk.RUN_IN_JUSPAY_BROWSER, "error while dealing with json onEvent " + args, e);
                    }

                }
                break;
            default:
                sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.ERROR, Labels.HyperSdk.RUN_IN_JUSPAY_BROWSER, "missing", method);
                break;
        }
    }

    @JavascriptInterface
    public void runInJuspayWebView(final String methodName, final String callbackName) {
    }

    @JavascriptInterface
    public String isDeviceSecure() {
        try {
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
                KeyguardManager keyguardManager = (KeyguardManager) context.getSystemService(Context.KEYGUARD_SERVICE);
                if (keyguardManager != null) {
                    return keyguardManager.isDeviceSecure() ? "SECURE" : "NOT_SECURE";
                }
            }
        } catch (Exception e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.JBRIDGE, "Exception while checking KeyguardManager.isDeviceSecure()", e);
        }

        return "UNKNOWN";
    }

    @JavascriptInterface
    public void requestKeyboardShow(final String id) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                if (activity != null) {
                    int currentId = parseInt(id);
                    InputMethodManager inputMethodManager = (InputMethodManager) activity.getSystemService(Context.INPUT_METHOD_SERVICE);
                    View editText = activity.findViewById(currentId);
                    View prevEditText = null;
                    if (lastFocusedEditView != -1) {
                        prevEditText = activity.findViewById(lastFocusedEditView);
                    }

                    if (inputMethodManager != null && editText != null) {
                        if (prevEditText != null && lastFocusedEditView != currentId) {
                            prevEditText.clearFocus();
                        }
                        editText.requestFocus();
                        inputMethodManager.showSoftInput(editText, InputMethodManager.SHOW_IMPLICIT);
                    }
                    if (currentId != lastFocusedEditView) {
                        lastFocusedEditView = parseInt(id);
                    }
                }
            } catch (Exception e) {
                sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.KEYBOARD, "Show Keyboard Exception", e);
            }
        });
    }

    @JavascriptInterface
    public void suppressKeyboard() {
        ExecutorManager.runOnMainThread(() -> {
            if (activity != null) {
                activity.getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_ALWAYS_HIDDEN);
            }
        });
    }

    @JavascriptInterface
    public void requestKeyboardHide() {
        ExecutorManager.runOnMainThread(() -> {
            try {
                if (activity != null) {
                    View focusedView = activity.getCurrentFocus();

                    if (focusedView == null) {
                        focusedView = activity.getWindow().getDecorView();
                    }

                    InputMethodManager inputMethodManager = (InputMethodManager) activity.getSystemService(Context.INPUT_METHOD_SERVICE);

                    if (inputMethodManager != null && focusedView.getRootView() != null) {
                        inputMethodManager.hideSoftInputFromWindow(focusedView.getRootView().getWindowToken(), InputMethodManager.RESULT_UNCHANGED_SHOWN);
                        sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.KEYBOARD, "hidden", "success");
                    } else {
                        sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.ERROR, Labels.System.KEYBOARD, "hidden", "failed");
                    }
                }
            } catch (Exception e) {
                sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.KEYBOARD, "Hide Keyboard Exception", e);
            }
        });
    }

    @JavascriptInterface
    public String getConfigVariables() {
        try {
            return PaymentUtils.getConfigVariableDeclarations(juspayServices.getContext(), juspayServices.getSessionInfo());
        } catch (JSONException e) {
            JuspayLogger.e(LOG_TAG, "", e);
            return "var clientId = null;" + "var juspayDeviceId = null;" + "var juspayAndroidId = null;" + "var godelRemotesVersion = null;" + "var godelVersion = null;" + "var buildVersion = null;" + "var os_version = null;";
        }
    }

    @JavascriptInterface
    public String getIndexBundleHash(String location) {
        String sourceHash = null;
        location = location.replace(".zip", ".jsa");
        int index = location.lastIndexOf("/");
        String fileName = location.substring(index + 1);
        String unzippedFileName = fileName.replace(".zip", ".jsa");
        try {
            JSONObject assetMetadata = remoteAssetService.getMetadata(unzippedFileName);
            sourceHash = assetMetadata.getString(PaymentConstants.ATTR_HASH_IN_DISK);
        } catch (Exception e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.JBRIDGE, "exception during IndexBundleHash", e);
        }
        return sourceHash;
    }

    @JavascriptInterface
    public int getResourceIdentifier(String name, String defType) {
        try {
            return context.getResources().getIdentifier(name, defType, context.getPackageName());
        } catch (Exception e) {
            return 0;
        }
    }

    @JavascriptInterface
    public void openAppWithExplicitIntent(String packageName, String className, String payload, int requestCode, int flag) {
        try {
            Bundle bundle = new Bundle();
            bundle.putString("data", payload);

            Intent i = new Intent();
            if (flag >= 0) {
                i.setFlags(flag);
            }
            i.putExtras(bundle);
            i.setComponent(new ComponentName(packageName, className));

            requestCode = Math.max(requestCode, -1);
            juspayServices.startActivityForResult(i, requestCode, null);
        } catch (Exception e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.JBRIDGE, "on method openAppWithExplicitIntent: ", e);
        }
    }

    @JavascriptInterface
    public String isAppInstalled(String packageName) {
        PackageManager pm = juspayServices.getContext().getPackageManager();
        try {
            @SuppressWarnings("unused") PackageInfo info = pm.getPackageInfo(packageName, PackageManager.GET_META_DATA);
        } catch (PackageManager.NameNotFoundException e) {
            return "false";
        }
        return "true";
    }

    @JavascriptInterface
    public String getPackageInfo(String packagName) {
        try {
            PackageManager pm = juspayServices.getContext().getPackageManager();
            PackageInfo packageInfo = pm.getPackageInfo(packagName, 0);

            JSONObject packageInfoJson = new JSONObject();

            packageInfoJson.put("packageName", packageInfo.packageName);
            packageInfoJson.put("versionName", packageInfo.versionName);
            packageInfoJson.put("versionCode", packageInfo.versionCode);

            return packageInfoJson.toString();
        } catch (Exception e) {
            sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.DEBUG, Labels.System.JBRIDGE, "Exception at getPackageInfo", e);
            return "{}";
        }
    }

    @JavascriptInterface
    public void storeCookies() {
        CookieSyncManager.getInstance().sync();
    }
}
