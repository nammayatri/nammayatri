package in.juspay.mobility.sdk.data;

import android.Manifest;
import android.content.Context;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.res.Configuration;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.os.Build;
import android.provider.Settings;
import android.telephony.TelephonyManager;
import android.util.DisplayMetrics;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.content.ContextCompat;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.File;
import java.util.Locale;
import java.util.Objects;
import java.util.UUID;

import in.juspay.mobility.sdk.hyper.constants.Labels;
import in.juspay.mobility.sdk.hyper.constants.LogCategory;
import in.juspay.mobility.sdk.hyper.constants.LogLevel;
import in.juspay.mobility.sdk.hyper.constants.LogSubCategory;
import in.juspay.mobility.sdk.hyper.core.JuspayCoreLib;
import in.juspay.mobility.sdk.hyper.core.JuspayLogger;
import in.juspay.mobility.sdk.hyper.core.SessionInfoInterface;
import in.juspay.mobility.sdk.core.JuspayServices;
import in.juspay.mobility.sdk.core.PaymentConstants;
import in.juspay.mobility.sdk.security.EncryptionHelper;
import in.juspay.mobility.sdk.utils.Utils;

/**
 * Class to manage the session data.
 *
 * @author Veera Manohara Subbaiah
 * @author Sri Harsha Chilakapati
 * @since 15-11-2017
 */
public class SessionInfo implements SessionInfoInterface {
    private static final String LOG_TAG = SessionInfo.class.getName();
    @NonNull
    private JSONObject sessionInfo;

    @Nullable
    private String sessionId;
    @NonNull
    private JSONObject bundleParams;

    @NonNull
    private final JuspayServices juspayServices;
    @Nullable
    private DisplayMetrics displayMetrics;
    @NonNull
    private final Context context;

    @NonNull
    private final String androidId;
    @NonNull
    private final String deviceId;

    public SessionInfo(@NonNull JuspayServices juspayServices) {
        this.juspayServices = juspayServices;
        this.context = juspayServices.getContext().getApplicationContext();
        this.sessionInfo = new JSONObject();
        this.bundleParams = new JSONObject();
        this.androidId = generateId("juspay_android_id");
        this.deviceId = generateId("juspay_device_id");
    }

    private static boolean isRooted() {
        String buildTags = Build.TAGS;
        if (buildTags != null && buildTags.contains("test-keys")) {
            return true;
        }

        try {
            File file = new File("/system/app/Superuser.apk");
            if (file.exists()) {
                return true;
            }
        } catch (Exception e1) {
            // ignore
        }
        return false;
    }

    public static String getOSVersion() {
        return Build.VERSION.RELEASE;
    }

    public void updateSessionData(JSONObject sessionData) {
        sessionInfo.remove("sessionData");

        try {
            sessionInfo.put("sessionData", sessionData);
        } catch (JSONException e) {
            juspayServices.sdkDebug(LOG_TAG, "Unable to update sessionInfo: " + e);
        }
    }

    @NonNull
    public JSONObject getBundleParams() {
        return bundleParams;
    }

    public void setBundleParams(JSONObject bundleParams) {
        try {
            JSONArray names = this.bundleParams.names();

            if (names != null) {
                for (int i = 0; i < names.length(); i++) {
                    String name = names.getString(i);
                    this.bundleParams.remove(name);
                }
            }

            names = bundleParams.names();

            if (names != null) {
                for (int i = 0; i < names.length(); i++) {
                    String name = names.getString(i);
                    this.bundleParams.put(name, bundleParams.get(name));
                }
            }

            set("bundleParams", this.bundleParams.toString());
        } catch (JSONException ignored) {
        }
    }

    // TODO: This is used in other places, and also not related to session. Should be moved to an util class
    public String getNetworkInfo() {
        try {
            ConnectivityManager connManager = (ConnectivityManager) context.getSystemService(Context.CONNECTIVITY_SERVICE);

            if (connManager == null) {
                return "cellular";
            }

            NetworkInfo mWifi = connManager.getNetworkInfo(ConnectivityManager.TYPE_WIFI);

            if (mWifi != null && mWifi.isConnected()) {
                return "wifi";
            } else {
                return "cellular";
            }
        } catch (Exception e) {
            return null;
        }
    }

    public int getNetworkType() {
        try {
            if (ContextCompat.checkSelfPermission(context, Manifest.permission.READ_BASIC_PHONE_STATE) != PackageManager.PERMISSION_GRANTED) {
                return -1;
            }
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.N) {
                TelephonyManager telephonyManager = (TelephonyManager) context.getSystemService(Context.TELEPHONY_SERVICE);
                return telephonyManager != null ? telephonyManager.getDataNetworkType() : -1;
            }
            return -1;
        } catch (Exception e) {
            return -1;
        }
    }

    private String getVersionName() {
        try {
            PackageManager manager = context.getPackageManager();
            PackageInfo info = manager.getPackageInfo(context.getPackageName(), 0);
            return info.versionName;
        } catch (PackageManager.NameNotFoundException e) {
            return null;
        }
    }

    private int getVersionCode() {
        try {
            PackageManager manager = context.getPackageManager();
            PackageInfo info = manager.getPackageInfo(context.getPackageName(), 0);
            return info.versionCode;
        } catch (PackageManager.NameNotFoundException e) {
            return -1;
        }
    }

    private DisplayMetrics getDisplayMetrics() {
        try {
            if (displayMetrics == null) {
                displayMetrics = context.getResources().getDisplayMetrics();
            }
            return displayMetrics;
        } catch (Exception e) {
            return null;
        }
    }

    @NonNull
    public JSONObject getSessionData() {
        JSONObject sessionData = sessionInfo.optJSONObject("sessionData");
        if (sessionData != null) {
            return sessionData;
        }
        return new JSONObject();
    }

    public String getScreenHeight() {
        DisplayMetrics displayMetrics = getDisplayMetrics();
        if (displayMetrics != null) {
            return String.valueOf(displayMetrics.heightPixels);
        }
        return null;
    }

    public String getScreenWidth() {
        DisplayMetrics displayMetrics = getDisplayMetrics();
        if (displayMetrics != null) {
            return String.valueOf(displayMetrics.widthPixels);
        }
        return null;
    }

    private String getScreenPpi() {
        DisplayMetrics displayMetrics = getDisplayMetrics();
        if (displayMetrics != null) {
            return String.valueOf(displayMetrics.xdpi);
        }
        return null;
    }

    @NonNull
    @Override
    public String getDeviceId() {
        return deviceId;
    }

    private boolean devOptionsEnabled() {
        try {
            int devEnabled;
            devEnabled = Settings.Secure.getInt(context.getContentResolver(),
                    Settings.Global.DEVELOPMENT_SETTINGS_ENABLED, 0);
            return devEnabled == 1;
        } catch (Exception ignored) {
        }
        return false;
    }

    public void logDeviceIdentifiers() {
        JSONObject deviceIdentifiers = new JSONObject();

        try {
            deviceIdentifiers.put("device_id", getDeviceId());
            deviceIdentifiers.put("android_id", this.getAndroidId());
            juspayServices.getSdkTracker().trackContext(LogSubCategory.Context.DEVICE, LogLevel.INFO, Labels.Device.IDENTIFIERS, deviceIdentifiers);
        } catch (JSONException ignored) {
        }
    }

    public void logSessionInfo() {
        try {
            juspayServices.getSdkTracker().trackContext(LogSubCategory.Context.DEVICE, LogLevel.INFO, Labels.Device.SESSION_INFO, sessionInfo);
        } catch (Exception exception) {
            juspayServices.getSdkTracker().trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.SESSION_INFO, "Exception while logging ", exception);
        }
    }

    public boolean isNetworkAvailable() {
        ConnectivityManager cm = (ConnectivityManager) context.getSystemService(Context.CONNECTIVITY_SERVICE);
        NetworkInfo activeNetwork = cm.getActiveNetworkInfo();
        return activeNetwork != null && activeNetwork.isConnectedOrConnecting();
    }


    private String getOrderIdFromPayload(JSONObject payload, String fallback) {
        if (payload.has(PaymentConstants.ORDER_ID_CAMEL)) {
            return payload.optString(PaymentConstants.ORDER_ID_CAMEL);
        } else if (payload.has(PaymentConstants.ORDER_ID)) {
            return payload.optString(PaymentConstants.ORDER_ID);
        } else {
            return fallback;
        }
    }

    private void addOrUpdateOrderId(String orderId) {
        JSONObject json = this.getSessionData();
        if (!json.optString("order_id").equals(orderId) && !orderId.equals("")) {
            try {
                json.put("order_id", orderId);
            } catch (JSONException ignored) {
            }
        }
    }

    public void addOrderIdInSessionData(JSONObject payload) {
        JSONObject innerPayload = payload.optJSONObject("payload");
        try {
            if (innerPayload != null) {
                if (innerPayload.has(PaymentConstants.SIGNATURE_PAYLOAD_CAMEL)) {
                    String signaturePayload = innerPayload.optString(PaymentConstants.SIGNATURE_PAYLOAD_CAMEL, "{}");
                    JSONObject object = new JSONObject(signaturePayload);
                    String id = getOrderIdFromPayload(object, innerPayload.optString(PaymentConstants.ORDER_ID_CAMEL));
                    addOrUpdateOrderId(id);
                } else if (innerPayload.has(PaymentConstants.ORDER_DETAILS_CAMEL)) {
                    JSONObject orderDetails = new JSONObject(innerPayload.optString(PaymentConstants.ORDER_DETAILS_CAMEL, "{}"));
                    addOrUpdateOrderId(getOrderIdFromPayload(orderDetails, innerPayload.optString(PaymentConstants.ORDER_ID_CAMEL)));
                } else {
                    addOrUpdateOrderId(getOrderIdFromPayload(innerPayload, ""));
                }
            }
        } catch (JSONException ignored) {
        }
    }

    @SuppressWarnings("unused")
    public String getNetworkName() {
        int networkTypeCode = getNetworkType();
        if ("wifi".equals(getNetworkInfo())) {
            return "WIFI";
        }
        switch (networkTypeCode) {
            case TelephonyManager.NETWORK_TYPE_GPRS:
            case TelephonyManager.NETWORK_TYPE_EDGE:
            case TelephonyManager.NETWORK_TYPE_CDMA:
            case TelephonyManager.NETWORK_TYPE_1xRTT:
            case TelephonyManager.NETWORK_TYPE_IDEN:
                return "2G";
            case TelephonyManager.NETWORK_TYPE_UMTS:
            case TelephonyManager.NETWORK_TYPE_EVDO_0:
            case TelephonyManager.NETWORK_TYPE_EVDO_A:
            case TelephonyManager.NETWORK_TYPE_HSDPA:
            case TelephonyManager.NETWORK_TYPE_HSUPA:
            case TelephonyManager.NETWORK_TYPE_HSPA:
            case TelephonyManager.NETWORK_TYPE_EVDO_B:
            case TelephonyManager.NETWORK_TYPE_EHRPD:
            case TelephonyManager.NETWORK_TYPE_HSPAP:
                return "3G";
            case TelephonyManager.NETWORK_TYPE_LTE:
            default:
                return "OTHER";
        }
    }

    public void createSessionDataMap() {
        JSONObject sessionData = new JSONObject();

        try {
            //Device_info
            sessionData.put("brand", Build.BRAND);
            sessionData.put("model", Build.MODEL);
            sessionData.put("manufacturer", Build.MANUFACTURER);
            sessionData.put("device_id", getDeviceId());
            sessionData.put("android_id", EncryptionHelper.getSHA256Hash(this.getAndroidId()));

            //OS_info
            sessionData.put("os", "android");
            sessionData.put("os_version", Build.VERSION.RELEASE);
            sessionData.put("android_api_level", String.valueOf(Build.VERSION.SDK_INT));
            sessionData.put("locale", Locale.getDefault().getDisplayLanguage());

            //Client_related_info
            sessionData.put("app_name", context.getApplicationInfo().loadLabel(context.getPackageManager()));
            sessionData.put("app_version", getVersionName());
            sessionData.put("app_version_code", getVersionCode());
            String client_id = getClientId();
            if (!Objects.equals(client_id, "")) {
                sessionData.put("client_id", client_id);
            }
            String merchant_id = getMerchantId();
            if (!Objects.equals(merchant_id, "")) {
                sessionData.put("merchant_id", merchant_id);
            }
            sessionData.put("dir_name", context.getApplicationInfo().sourceDir);
            sessionData.put("package_name", context.getApplicationInfo().packageName);

            //Network_info
            sessionData.put("network_info", getNetworkInfo());
            sessionData.put("network_type", String.valueOf(getNetworkType()));
            sessionData.put("ip_address", Utils.getIPAddress(juspayServices));

            //Hack_info
            sessionData.put("is_rooted", String.valueOf(isRooted()));
            sessionData.put("is_dev_enabled", String.valueOf(devOptionsEnabled()));
            sessionData.put("app_debuggable", JuspayCoreLib.isAppDebuggable());
            sessionData.put("sdk_debuggable", juspayServices.getSdkInfo().isSdkDebuggable());

            //Display_info
            sessionData.put("screen_width", getScreenWidth());
            sessionData.put("screen_height", getScreenHeight());
            sessionData.put("screen_ppi", getScreenPpi());

            updateSessionData(sessionData);

        } catch (Throwable ignored) {
        }
    }
    @NonNull
    public String getClientId() {
        if (bundleParams.has("payload")) {
            JSONObject payload = bundleParams.optJSONObject("payload");
            if (payload != null) {
                if (payload.has(PaymentConstants.CLIENT_ID_CAMEL)) {
                    return payload.optString(PaymentConstants.CLIENT_ID_CAMEL);
                } else if (payload.has(PaymentConstants.CLIENT_ID)) {
                    return payload.optString(PaymentConstants.CLIENT_ID);
                }
            }
        }
        return "";
    }

    @Nullable
    public String tryGetClientId() {
        String clientId = getClientId();
        if (clientId.equals("")) {
            return null;
        }
        return clientId;
    }

    public String getPackageName() {
        return context.getPackageName();
    }

    @SuppressWarnings("unused")
    public String getAppName() {
        String appName = sessionInfo.optString("app_name");
        return !appName.equals("") ? appName : getClientId();
    }

    @Nullable
    public String getSessionId() {
        return sessionId;
    }

    public void setSessionId() {
        this.sessionId = UUID.randomUUID().toString();
        JuspayLogger.d(LOG_TAG, "Session ID: " + sessionId);
    }

    public void resetSession() {
        sessionId = null;
        sessionInfo = new JSONObject();
        bundleParams = new JSONObject();
    }

    @NonNull
    public String getMerchantId() {
        if (bundleParams.has("payload")) {
            JSONObject payload = bundleParams.optJSONObject("payload");
            if (payload != null) {
                try {
                    if (payload.has(PaymentConstants.SIGNATURE_PAYLOAD_CAMEL)) {
                        String signaturePayload = payload.optString(PaymentConstants.SIGNATURE_PAYLOAD_CAMEL, "{}");
                        JSONObject object = new JSONObject(signaturePayload);
                        if (object.has(PaymentConstants.MERCHANT_ID_CAMEL)) {
                            return object.optString(PaymentConstants.MERCHANT_ID_CAMEL);
                        } else if (object.has(PaymentConstants.MERCHANT_ID)) {
                            return object.optString(PaymentConstants.MERCHANT_ID);
                        }
                    }
                } catch (Exception ignored) {
                }

                if (payload.has(PaymentConstants.MERCHANT_ID_CAMEL)) {
                    return payload.optString(PaymentConstants.MERCHANT_ID_CAMEL);
                } else if (payload.has(PaymentConstants.MERCHANT_ID)) {
                    return payload.optString(PaymentConstants.MERCHANT_ID);
                }
            }
        }
        return "";
    }

    @NonNull
    public String getOrderId() {
        JSONObject json = this.getSessionData();
        if (json.has("order_id")) {
            return json.optString("order_id");
        }
        return "";
    }

    @Nullable
    public String tryGetMerchantId() {
        String merchantId = getMerchantId();
        if (merchantId.equals("")) {
            return null;
        }
        return merchantId;
    }

    @SuppressWarnings("unused")
    public String getScreenSizeDensity() {
        try {
            DisplayMetrics displayMetrics = getDisplayMetrics();
            if (displayMetrics != null) {
                float density = displayMetrics.density;
                Configuration config = context.getResources().getConfiguration();
                int size = config.screenLayout & Configuration.SCREENLAYOUT_SIZE_MASK;
                return size + "-" + density;
            }
            throw new Exception("display metrics null");
        } catch (Exception e) {
            return null;
        }
    }

    public void removeAttribute(String key) {
        sessionInfo.remove(key);
    }

    public void set(String key, String value) {
        try {
            sessionInfo.put(key, value);
        } catch (Exception ignored) {
        }
    }

    public String get(String key, String defaultValue) {
        return sessionInfo.optString(key, defaultValue);
    }


    @NonNull
    private String generateId(String key) {
        Context context = juspayServices.getContext();
        String namespace = juspayServices.getSdkInfo().getSdkName();
        String id = KeyValueStore.read(context, namespace, key, null);
        if (id == null) {
            id = UUID.randomUUID().toString();
            KeyValueStore.write(context, namespace, key, id);
        }
        return id;
    }

    @NonNull
    public String getAndroidId() {
        return androidId;
    }
}
