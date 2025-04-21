package in.juspay.hypersdk.core;


import android.app.ActivityManager;
import android.content.Context;
import android.telephony.TelephonyManager;
import android.util.Base64;
import android.webkit.CookieManager;
import android.webkit.CookieSyncManager;

import androidx.annotation.Keep;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import org.json.JSONException;
import org.json.JSONObject;

import java.io.File;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Set;

import in.juspay.hyper.constants.Labels;
import in.juspay.hyper.constants.LogCategory;
import in.juspay.hyper.constants.LogLevel;
import in.juspay.hyper.constants.LogSubCategory;
import in.juspay.hyper.core.JuspayLogger;
import in.juspay.hypersdk.data.PaymentSessionInfo;
import in.juspay.hypersdk.data.SessionInfo;
import in.juspay.hypersdk.safe.Godel;
import in.juspay.hypersdk.safe.JuspayWebView;
import in.juspay.hypersdk.utils.IntegrationUtils;
import in.juspay.hypersdk.utils.Utils;

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
                "var godelRemotesVersion = '" + PaymentSessionInfo.getGodelRemotesVersion(context) + "';" +
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

    public static void switchOffGodelIfLowOnMemory(Godel godelManager, JuspayServices juspayServices, PaymentSessionInfo paymentSessionInfo) {
        try {
            final Context context = juspayServices.getContext();
            final SdkTracker sdkTracker = juspayServices.getSdkTracker();

            ActivityManager am = (ActivityManager) context.getSystemService(Context.ACTIVITY_SERVICE);
            ActivityManager.MemoryInfo memoryInfo = new ActivityManager.MemoryInfo();
            int shouldUseMemoryThreshold = 4;

            try {
                JSONObject weblabRules = godelManager.getWebLabRules();
                if (weblabRules != null) {
                    shouldUseMemoryThreshold = weblabRules.getInt("shouldUseMemory");
                    sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.UTIL, "weblab_shouldUseMemory", shouldUseMemoryThreshold + " MB");
                }
            } catch (Exception e) {
                sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.UTIL, "Exception while fetching shouldUseMemory from config", e);
            }

            if (am != null) {
                am.getMemoryInfo(memoryInfo);
                int memoryClass = am.getMemoryClass();
                if (memoryClass < shouldUseMemoryThreshold) {
                    paymentSessionInfo.setGodelDisabled(PaymentConstants.GodelOffReasons.LOW_ON_MEMORY);
                    sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.UTIL, "switching_godel_off", "low on memory - Available memory : " + memoryClass + " MB");
                }
                logMemoryInfo(sdkTracker, memoryInfo);
                sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.UTIL, "switchoffgodeliflowonmemory", memoryClass + " MB <" + shouldUseMemoryThreshold);
            }
        } catch (Exception e) {
            // Ignore
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

    public static void refreshPage(JuspayWebView juspayWebView) {
        final String jsCommand = "window.location.reload(true);";
        if (juspayWebView != null) {
            juspayWebView.addJsToWebView(jsCommand);
        }
    }

    @Keep
    public static String toJavascriptArray(@Nullable ArrayList<String> arr) {
        if (arr == null) return "[]";
        StringBuilder sb = new StringBuilder();
        sb.append("[");
        for (Iterator<String> i = arr.iterator(); i.hasNext(); ) {
            sb.append("\"").append(i.next()).append("\"");
            if (i.hasNext()) {
                sb.append(",");
            }
        }
        sb.append("]");
        return sb.toString();
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
