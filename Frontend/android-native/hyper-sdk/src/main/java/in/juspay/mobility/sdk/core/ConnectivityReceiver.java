package in.juspay.mobility.sdk.core;

import android.Manifest;
import android.app.Activity;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.PackageManager;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.os.Build;
import android.provider.Settings;
import android.telephony.TelephonyManager;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.content.ContextCompat;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.Map;
import java.util.WeakHashMap;

import in.juspay.mobility.sdk.hyper.constants.Labels;
import in.juspay.mobility.sdk.hyper.constants.LogCategory;
import in.juspay.mobility.sdk.hyper.constants.LogSubCategory;
import in.juspay.mobility.sdk.hypersmshandler.JuspayDuiHook;

/**
 * Created by dharanikumarkdk on 25/01/18.
 */
public class ConnectivityReceiver extends BroadcastReceiver implements JuspayDuiHook {
    private static final String LOG_TAG = ConnectivityReceiver.class.getSimpleName();
    @NonNull
    private final JuspayServices juspayServices;
    @NonNull
    private final Map<Activity, Boolean> attachedMap = new WeakHashMap<>();

    public ConnectivityReceiver(@NonNull JuspayServices juspayServices) {
        this.juspayServices = juspayServices;
    }

    @Override
    public void onReceive(Context context, Intent intent) {
        JSONObject result = new JSONObject();
        try {
            result.put("connected", String.valueOf(isNetworkAvailable()));
            result.put("networkType", getNetworkType());
            result.put("isMobileDataOn", String.valueOf(isMobileDataOn()));
        } catch (JSONException ignored) {
        }
        juspayServices.getJBridge().invokeFnInDUIWebview("onNetworkChange", result.toString());
    }

    private boolean isNetworkAvailable() {
        try {
            ConnectivityManager connectivityManager = (ConnectivityManager) juspayServices.getContext()
                    .getSystemService(Context.CONNECTIVITY_SERVICE);
            NetworkInfo activeNetworkInfo = null;
            if (connectivityManager != null) {
                activeNetworkInfo = connectivityManager.getActiveNetworkInfo();
            }
            return activeNetworkInfo != null && activeNetworkInfo.isConnected();
        } catch (Exception e) {
            juspayServices.getSdkTracker().trackAndLogException(LOG_TAG, LogCategory.ACTION,
                    LogSubCategory.Action.SYSTEM, Labels.System.IS_NETWORK_AVAILABLE, "network failure", e);
            return false;
        }
    }

    private boolean isMobileDataOn() {
        try {
            Context context = juspayServices.getContext();
            TelephonyManager telephonyManager = (TelephonyManager) context
                    .getSystemService(Context.TELEPHONY_SERVICE);
            boolean isAirplaneModeOff = Settings.Global.getInt(context.getContentResolver(), Settings.Global.AIRPLANE_MODE_ON, 0) == 0;
            boolean isMobileDataTurnedOn = false;
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
                if (ContextCompat.checkSelfPermission(context,
                        Manifest.permission.READ_BASIC_PHONE_STATE) == PackageManager.PERMISSION_GRANTED) {
                    isMobileDataTurnedOn = telephonyManager.isDataEnabled();
                }
            } else {
                if (telephonyManager.getSimState() == TelephonyManager.SIM_STATE_READY) {
                    isMobileDataTurnedOn = Settings.Global.getInt(context.getContentResolver(), "mobile_data", 1) == 1;
                }
            }
            return isMobileDataTurnedOn && isAirplaneModeOff;
        } catch (Exception e) {
            return false;
        }
    }

    private String getNetworkType() {
        String networkType = "";
        if (juspayServices.getSessionInfo().getNetworkInfo() != null) {
            networkType = juspayServices.getSessionInfo().getNetworkInfo();
        }
        return networkType;
    }

    @Override
    public void attach(@NonNull Activity activity) {
        Boolean isAttached = attachedMap.get(activity);
        if (isAttached == null || !isAttached) {
            activity.registerReceiver(this, new IntentFilter(ConnectivityManager.CONNECTIVITY_ACTION));
            juspayServices.sdkDebug(LOG_TAG, "Attaching the " + LOG_TAG);
            attachedMap.put(activity, true);
        }
    }

    @Override
    public String execute(@NonNull Activity activity, @Nullable String operation, @Nullable JSONObject args) {
        return String.valueOf(isNetworkAvailable());
    }

    @Override
    public void detach(@NonNull Activity activity) {
        Boolean isAttached = attachedMap.get(activity);
        if (isAttached != null && isAttached) {
            activity.unregisterReceiver(this);
            juspayServices.sdkDebug(LOG_TAG, "Detaching the " + LOG_TAG);
            attachedMap.put(activity, false);
        }
    }
}
