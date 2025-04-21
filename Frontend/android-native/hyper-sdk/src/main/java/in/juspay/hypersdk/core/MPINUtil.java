package in.juspay.hypersdk.core;

import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.os.Bundle;
import android.os.Handler;
import android.os.IBinder;
import android.os.Message;
import android.os.Messenger;
import android.os.RemoteException;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import org.json.JSONObject;

import java.util.HashMap;
import java.util.Set;

import in.juspay.hyper.constants.Labels;
import in.juspay.hyper.constants.LogCategory;
import in.juspay.hyper.constants.LogLevel;
import in.juspay.hyper.constants.LogSubCategory;

public class MPINUtil {
    private static final String TAG = "MPINUtil";

    @Nullable
    private static HashMap<String, MPINUtil> orchestrator;

    @Nullable
    private GodelServiceConnection connection;
    @NonNull
    private final ComponentName component;

    private MPINUtil(JuspayServices juspayServices, String packageName, String className) {
        connection = new GodelServiceConnection(juspayServices);
        component = new ComponentName(packageName, className);
    }

    public static void communicate(String packageName, String className, int reqCode, Bundle bundle, @NonNull JuspayServices juspayServices, String callBackFnName) {
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();

        try {
            sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.MPIN_UTIL, "mpinutil_communicate", "Attempting to communicate to " + packageName + "/" + className);

            if (orchestrator == null) {
                orchestrator = new HashMap<>();
            }

            MPINUtil mpinUtil;

            if (orchestrator.containsKey(packageName)) {
                sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.MPIN_UTIL, "mpinutil_communicate", "Fetching existing instance from orchestrator");
                mpinUtil = orchestrator.get(packageName);
            } else {
                mpinUtil = new MPINUtil(juspayServices, packageName, className);
                sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.MPIN_UTIL, "mpinutil_communicate", "Creating new MPINUtil instance in orchestrator");
                boolean willBind = mpinUtil.bind(juspayServices.getContext());

                if (!willBind) {
                    sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.MPIN_UTIL, "mpinutil_communicate", "Failed to bind to MPIN SDK. Reporting Bind Failure back to mApp");
                    reportBindFailure(reqCode, juspayServices, callBackFnName);
                    return;
                }

                orchestrator.put(packageName, mpinUtil);
            }

            if (mpinUtil == null || mpinUtil.connection == null) {
                String nullObject = mpinUtil == null ? "mpinUtil" : "mpinUtil.connection";
                sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.MPIN_UTIL, "mpinutil_communicate", nullObject + " is null. Reporting Bind Failure back to mApp");
                reportBindFailure(reqCode, juspayServices, callBackFnName);
                return;
            }

            sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.MPIN_UTIL, "mpinutil_communicate", "Requesting a connection with MPIN SDK");
            mpinUtil.connection.request(reqCode, bundle, new GodelServiceResponseHandler(callBackFnName, juspayServices));
        } catch (Exception e) {
            sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.MPIN_UTIL, "mpinutil_communicate", "Failed to bind to MPIN SDK. Reporting Bind Failure back to mApp");
            sdkTracker.trackAndLogException(TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.MPIN_UTIL, "Exception while trying to connect", e);
            reportBindFailure(reqCode, juspayServices, callBackFnName);
        }
    }

    static void reportBindFailure(int reqCode, @NonNull JuspayServices juspayServices, String callbackFnName) {
        if (callbackFnName != null) {
            // juspayServices will be null; if sdk is terminated, this can happen if sdk activity is destroyed.
            final SdkTracker sdkTracker = juspayServices.getSdkTracker();

            JSONObject errorResponse = new JSONObject();
            try {
                errorResponse.put("code", reqCode);
                errorResponse.put("error", true);
                errorResponse.put("message", "BIND_FAILURE"); //JS code will look for this specific string and try a fallback
            } catch (Exception e) {
                sdkTracker.trackAndLogException(TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.MPIN_UTIL, "Exception while creating bind failure response", e);
            }

            juspayServices.getJBridge().invokeCallbackInDUIWebview(callbackFnName, errorResponse.toString());
        }
    }

    public static void closeAllConnections(Context context) {
        if (orchestrator != null) {
            Set<String> keys = orchestrator.keySet();
            for (String key : keys) {
                closeConnection(key, context);
            }
        }
        orchestrator = null;
    }

    public static void closeConnection(String packageName, Context context) {
        if (orchestrator != null && orchestrator.containsKey(packageName)) {
            MPINUtil mpinUtil = orchestrator.get(packageName);
            if (mpinUtil != null) {
                mpinUtil.unbind(context);
            }
            orchestrator.remove(packageName);
        }
    }

    private boolean bind(Context context) {
        Intent intent = new Intent();
        intent.setComponent(component);
        return context.bindService(intent, connection, Context.BIND_AUTO_CREATE);
    }

    private void unbind(Context context) {
        if (connection != null && connection.isBound) {
            try {
                context.unbindService(connection);
            } catch (Exception e) {
                //Already unbound
            }
            connection = null;
        }
    }
}

class GodelServiceConnection implements ServiceConnection {
    private static final String TAG = "GodelServiceConnection";

    boolean isBound = false;
    private Messenger messenger = null;
    private Message pendingMsg = null;

    private final JuspayServices juspayServices;

    GodelServiceConnection(JuspayServices juspayServices) {
        this.juspayServices = juspayServices;
    }

    @Override
    public void onServiceConnected(ComponentName name, IBinder service) {
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();

        try {
            sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.GODEL_SERVICE_CONNECTION, "gsc_on_service_connected", "Successfully connected to " + name.getPackageName() + "/" + name.getClassName());

            messenger = new Messenger(service);
            isBound = true;

            request(pendingMsg);
        } catch (Exception e) {
            sdkTracker.trackAndLogException(TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.MPIN_UTIL, "Exception while trying to send message", e);
        }
    }

    @Override
    public void onServiceDisconnected(ComponentName name) {
        messenger = null;
        isBound = false;
    }

    public void request(int code, Bundle bundle, Handler respHandler) throws RemoteException {
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();

        sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.GODEL_SERVICE_CONNECTION, "gsc_request", "Sending request to MPIN SDK");

        Message msg = Message.obtain(null, code);
        msg.setData(bundle);
        msg.replyTo = new Messenger(respHandler);

        request(msg);
    }

    public void request(Message msg) throws RemoteException {
        if (isBound) {
            messenger.send(msg);
        } else {
            pendingMsg = msg;
        }
    }
}

class GodelServiceResponseHandler extends Handler {
    private static final String TAG = "GodelServiceResponseHandler";

    @NonNull
    private final JuspayServices juspayServices;
    private String callBackFnName;

    GodelServiceResponseHandler(String callBackFnName, @NonNull JuspayServices juspayServices) {
        this.callBackFnName = callBackFnName;
        this.juspayServices = juspayServices;
    }

    @Override
    public void handleMessage(Message msg) {
        if (callBackFnName != null) {
            final SdkTracker sdkTracker = juspayServices.getSdkTracker();

            try {
                JSONObject json = PaymentUtils.toJSON(msg.getData());
                json.put("code", msg.what);

                sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.GODEL_SERVICE_RESPONSE_HANDLER, "gsrh_handle_message", "Got response from the MPIN SDK: " + json.toString(2));

                sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.GODEL_SERVICE_RESPONSE_HANDLER, "gsrh_handle_message", "Sending response back to micro-app");
                juspayServices.getJBridge().invokeCallbackInDUIWebview(callBackFnName, json.toString());
            } catch (Exception e) {
                sdkTracker.trackAndLogException(TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.MPIN_UTIL, "Exception while trying to handle message", e);
            }
        }
        callBackFnName = null;
    }
}
