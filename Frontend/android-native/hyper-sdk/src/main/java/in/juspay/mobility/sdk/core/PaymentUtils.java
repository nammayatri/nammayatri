package in.juspay.mobility.sdk.core;


import android.app.ActivityManager;
import android.content.Context;
import android.telephony.TelephonyManager;
import android.util.Base64;
import android.webkit.CookieManager;
import android.webkit.CookieSyncManager;

import androidx.annotation.NonNull;

import org.json.JSONException;

import java.io.File;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.Set;

import in.juspay.mobility.sdk.hyper.constants.Labels;
import in.juspay.mobility.sdk.hyper.constants.LogCategory;
import in.juspay.mobility.sdk.hyper.constants.LogLevel;
import in.juspay.mobility.sdk.hyper.constants.LogSubCategory;
import in.juspay.mobility.sdk.hyper.core.JuspayLogger;
import in.juspay.mobility.sdk.data.SessionInfo;
import in.juspay.mobility.sdk.utils.IntegrationUtils;
import in.juspay.mobility.sdk.utils.Utils;

/**
 * Created by Veera.Subbiah on 19/04/17.
 */

public class PaymentUtils extends Utils {
    private static final String LOG_TAG = PaymentUtils.class.getSimpleName();

    public static ConnectivityReceiver getConnectivityReceiver(JuspayServices juspayServices) {
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();

        try {
            return new ConnectivityReceiver(juspayServices);
        } catch (Throwable e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.UTIL, "Failed to register Connectivity receiver (Ignoring)", e);
        }
        return null;
    }

    public static String getConfigVariableDeclarations(@NonNull Context context, SessionInfo sessionInfo) throws JSONException {
        String deviceId = sessionInfo.getDeviceId();
        String androidId = sessionInfo.getAndroidId();

        if (deviceId == null || deviceId.isEmpty()) {
            deviceId = "";
        }

        return "var clientId = '" + sessionInfo.getClientId() + "';" +
                "var juspayDeviceId = '" + deviceId + "';" +
                "var juspayAndroidId = '" + androidId + "';" +
                "var godelVersion = '" + IntegrationUtils.getGodelVersion(context) + "';" +
                "var buildVersion = '" + IntegrationUtils.getGodelBuildVersion(context) + "';" +
                "var os_version = '" + SessionInfo.getOSVersion() + "';";
    }

    public static void clearCookies(JuspayServices juspayServices) {
        final Context context = juspayServices.getContext();
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();

        try {
            CookieSyncManager.createInstance(context).sync();
            CookieManager.getInstance().removeAllCookie();
        } catch (Exception e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.UTIL, "Failed to clear cookies", e);
        }
    }


    private static void logMemoryInfo(SdkTracker sdkTracker, ActivityManager.MemoryInfo memoryInfo) {
        sdkTracker.trackContext(LogSubCategory.Context.DEVICE, LogLevel.INFO, Labels.Device.MEMORY, "available_memory", memoryInfo.availMem);
        sdkTracker.trackContext(LogSubCategory.Context.DEVICE, LogLevel.INFO, Labels.Device.MEMORY, "threshold_memory", memoryInfo.threshold);
        sdkTracker.trackContext(LogSubCategory.Context.DEVICE, LogLevel.INFO, Labels.Device.MEMORY, "total_memory", memoryInfo.totalMem);
    }

    public static boolean hasTelephonyService(JuspayServices juspayServices) {
        final Context context = juspayServices.getContext();
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();

        try {
            return (((TelephonyManager) context
                    .getSystemService(Context.TELEPHONY_SERVICE)).getPhoneType() != TelephonyManager.PHONE_TYPE_NONE);

        } catch (Throwable e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.UTIL, "Exception while trying to get telephony service. Returning false.", e);
            return false;
        }
    }


    @SuppressWarnings("unused")
    public static void deleteRecursive(File fileOrDirectory) {
        if (!fileOrDirectory.exists()) {
            return;
        }
        if (fileOrDirectory.isDirectory()) {
            File[] files = fileOrDirectory.listFiles();
            if (files == null) {
                files = new File[0];
            }
            for (File child : files) {
                deleteRecursive(child);
            }
        }

        //noinspection ResultOfMethodCallIgnored
        fileOrDirectory.delete();
    }

    public static boolean isClassAvailable(String className) {
        try {
            Class.forName(className);
            return true;
        } catch (Exception ignored) {
        }
        return false;
    }

    public static boolean validatePinning(X509Certificate[] chain, Set<String> validPins) throws CertificateException {
        MessageDigest md;
        StringBuilder certChainMsg = new StringBuilder();
        try {
            md = MessageDigest.getInstance("SHA-256");
        } catch (NoSuchAlgorithmException e) {
            throw new CertificateException("couldn't create digest");
        }

        for (X509Certificate cert : chain) {
            byte[] publicKey = cert.getPublicKey().getEncoded();
            md.update(publicKey, 0, publicKey.length);
            String pin = Base64.encodeToString(md.digest(), Base64.NO_WRAP);
            certChainMsg.append("    sha256/").append(pin).append(" : ").append(cert.getSubjectDN().toString()).append("\n");
            return !validPins.contains(pin);
        }
        JuspayLogger.d(LOG_TAG, certChainMsg.toString());
        return true;
    }
}
