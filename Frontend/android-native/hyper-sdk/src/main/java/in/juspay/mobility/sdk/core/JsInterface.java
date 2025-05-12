package in.juspay.mobility.sdk.core;

import android.content.Context;
import android.webkit.JavascriptInterface;
import android.widget.Toast;

import androidx.annotation.NonNull;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.Arrays;
import java.util.Objects;

import in.juspay.mobility.sdk.hyper.constants.Labels;
import in.juspay.mobility.sdk.hyper.constants.LogCategory;
import in.juspay.mobility.sdk.hyper.constants.LogSubCategory;
import in.juspay.mobility.sdk.hyper.core.ExecutorManager;
import in.juspay.mobility.sdk.hyper.core.JuspayLogger;
import in.juspay.mobility.sdk.analytics.LogConstants;
import in.juspay.mobility.sdk.analytics.LogPusher;
import in.juspay.mobility.sdk.analytics.LogPusherExp;
import in.juspay.mobility.sdk.analytics.LogSessioniser;
import in.juspay.mobility.sdk.services.RemoteAssetService;
import in.juspay.mobility.sdk.data.KeyValueStore;
import in.juspay.mobility.sdk.data.SessionInfo;
import in.juspay.mobility.sdk.security.EncryptionHelper;
import in.juspay.mobility.sdk.services.FileProviderService;

/**
 * Class that provides the bridge between JavaScript running in Mystique's {@link DynamicUI} and the Java world.
 *
 * @author Shubham Agrawal [subham.agrawal@juspay.in]
 * @author Sri Harsha Chilakapati [sri.harsha@juspay.in]
 * @since 04/09/2017
 */
public class JsInterface {
    private static final String LOG_TAG = "JsInterface";
    @NonNull
    protected final JuspayServices juspayServices;
    @NonNull
    private final Context context;
    @NonNull
    private final SessionInfo sessionInfo;
    private final SdkTracker sdkTracker;
    @NonNull
    private final FileProviderService fileProviderService;
    @NonNull
    private final RemoteAssetService remoteAssetService;

    /**
     * Construct a {@code JsInterface} instance. Once created, this can be added to any {@link android.webkit.WebView}
     * as a {@link JavascriptInterface}. For the Juspay's WebView's created by using {@link JuspayServices}, this will
     * be added by default. DUI code running in DUI WebView can find the functions in the {@code JBridge} global object.
     *
     * @param juspayServices The JuspayServices object that represents the SDK currently active.
     */
    public JsInterface(@NonNull JuspayServices juspayServices) {
        this.context = juspayServices.getContext();
        this.juspayServices = juspayServices;

        sessionInfo = juspayServices.getSessionInfo();
        sdkTracker = juspayServices.getSdkTracker();
        remoteAssetService = juspayServices.getRemoteAssetService();
        fileProviderService = juspayServices.getFileProviderService();
    }

    @JavascriptInterface
    public String getResourceByName(String resName) {
        return getResourceById(context.getResources().getIdentifier(resName, "string", context.getPackageName()));
    }

    @JavascriptInterface
    public String getResourceById(int resId) {
        if (resId == 0) return "";
        return context.getResources().getString(resId);
    }

    @JavascriptInterface
    public void setInSharedPrefs(String key, String value) {
        KeyValueStore.write(juspayServices, key, value);
    }

    @JavascriptInterface
    public String getFromSharedPrefs(String key) {
        return KeyValueStore.read(juspayServices, key, "__failed");
    }

    @JavascriptInterface
    public void removeFromSharedPrefs(String key) {
        KeyValueStore.remove(juspayServices.getContext(), juspayServices.getSdkInfo().getSdkName(), key);
    }

    @JavascriptInterface
    public void renewFile(String location) {
        renewFile(location, null, null);
    }

    @JavascriptInterface
    public void renewFile(String location, String fileName) {
        renewFile(location, fileName, null);
    }

    @JavascriptInterface
    public void renewFile(String location, String fileName, String callback) {
        remoteAssetService.renewFile(context, location, callback, fileName, System.currentTimeMillis());
    }

    @JavascriptInterface
    public String loadFileInDUI(String fileName, int maxSecondsToLoad) {
        return fileProviderService.readFromFile(context, fileName);
    }

    @JavascriptInterface
    public String loadFileInDUI(String fileName) {
        return fileProviderService.readFromFile(context, fileName);
    }

    @JavascriptInterface
    public String getSessionInfo() {
        sessionInfo.createSessionDataMap();
        return sessionInfo.toString();
    }

    @JavascriptInterface
    public void setSessionAttribute(String key, String value) {
        sessionInfo.set(key, value);
    }

    @JavascriptInterface
    public String getSessionAttribute(String key, String defaultValue) {
        return sessionInfo.get(key, defaultValue);
    }

    @JavascriptInterface
    public void removeDataFromSharedPrefs(String key) {
        KeyValueStore.remove(juspayServices, key);
    }

    @JavascriptInterface
    public void removeAttribute(String key) {
        sessionInfo.removeAttribute(key);
    }

    @JavascriptInterface
    public String getMd5(String val) {
        return EncryptionHelper.md5(val.getBytes());
    }

    @JavascriptInterface
    public void setSessionId(String sessionId) {
        JuspayLogger.d(LOG_TAG, "JBridge.setSessionId() is intended for changing the Session ID of the SDK. Not to be called by each micro-app");
        JuspayLogger.d(LOG_TAG, "Attempted Session ID: " + sessionId);
    }

    @JavascriptInterface
    public String getSessionId() {
        return sessionInfo.getSessionId();
    }

    @JavascriptInterface
    public void toast(String msg) {
        ExecutorManager.runOnMainThread(() -> {
            Toast.makeText(context, msg, Toast.LENGTH_LONG).show();
        });
    }

    @JavascriptInterface
    public boolean isNetworkAvailable() {
        return sessionInfo.isNetworkAvailable();
    }

    @JavascriptInterface
    public void addToLogList(String logLine) {
        if (sessionInfo.getSessionId() != null) {
            try {
                JSONObject log = new JSONObject(logLine);
                sdkTracker.track(log);
            } catch (JSONException e) {
                sdkTracker.trackException(LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.LOG_PUSHER, "Exception while parsing the JSON", e);
            }
        } else {
            SdkTracker.addToBootLogs(logLine);
        }
    }

    @JavascriptInterface
    public void submitAllLogs() {
    }

    /**
     * Get a JSON string with the logs that aren't yet uploaded to the server. This JSON starts with array notation and
     * doesn't contain any object.
     *
     * @return A JSON string with the logs.
     */
    @JavascriptInterface
    public String getLogList() {
        JuspayLogger.e(LOG_TAG, "No one should call JBridge.getLogList() method. It will be removed in future.");
        return "[]";
    }

    /**
     * Updates the logs that aren't yet uploaded with the JSON array string of logs provided.
     *
     * @param logs The logs in the stringified JSON array format.
     */
    @JavascriptInterface
    public void updateLogList(String logs) {
        JuspayLogger.e(LOG_TAG, "No one should call JBridge.updateLogList() method. It will be removed in future.");
    }

    @JavascriptInterface
    public void postLogs(final String endPoint, final String logs) {
        JuspayLogger.e(LOG_TAG, "No one should call JBridge.postLogs() method. It will be removed in future.");
    }

    @JavascriptInterface
    public void setAnalyticsEndPoint(String newEndPoint) {
        JuspayLogger.e(LOG_TAG, "No one should call JBridge.setAnalyticsEndPoint() method. It will be removed in future.");
    }

    @JavascriptInterface
    public boolean setAnalyticsHeader(String headers) {
        JSONObject json;
        try {
            json = new JSONObject(headers);
        } catch (JSONException e) {
            // Unable to parse headers
            return false;
        }
        ExecutorManager.runOnLogPusherThread(() -> {
            if (Objects.equals(juspayServices.getWorkingLogger(), "json-array") || Objects.equals(juspayServices.getWorkingLogger(), "both")) {
                LogPusher.setHeaders(json, LogConstants.DEFAULT_CHANNEL);
            }
            if (juspayServices.getLogSessioniserExp() != null) {
                LogPusherExp.setHeaders(json, LogConstants.DEFAULT_CHANNEL);
            }
        });
        return true;
    }

    @JavascriptInterface
    public void addLogProperties(String properties) {
        ExecutorManager.runOnSdkTrackerPool(() -> {
            try {
                JSONObject logProperties = new JSONObject(properties);
                sdkTracker.addLogProperties(logProperties);
            } catch (Exception ignored) {
            }
        });
    }

    @JavascriptInterface
    public void startPushingLogs() {
        if (juspayServices.getLogSessioniserExp() != null) {
            juspayServices.getLogSessioniserExp().startPushing();
        }
    }

    @JavascriptInterface
    public boolean addChannel(String jsonObj, String channelName) {
        if (Objects.equals(juspayServices.getWorkingLogger(), "json-array") || Objects.equals(juspayServices.getWorkingLogger(), "both")) {
            return LogPusher.addChannelFromJS(jsonObj, channelName);
        }
        return false;
    }

    @JavascriptInterface
    public boolean addChannelExp(String jsonObj, String channelName) {
        if (juspayServices.getLogSessioniserExp() != null) {
            return LogPusherExp.addChannelFromJS(jsonObj, channelName);
        }
        return false;
    }

    @JavascriptInterface
    public String getChannelNames() {
        if (Objects.equals(juspayServices.getWorkingLogger(), "json-array") || Objects.equals(juspayServices.getWorkingLogger(), "both")) {
            return Arrays.toString(LogPusher.getChannelNames());
        }
        return "[]";
    }

    @JavascriptInterface
    public String getChannelNamesExp() {
        if (juspayServices.getLogSessioniserExp() != null) {
            return Arrays.toString(LogPusherExp.getChannelNames());
        }
        return "[]";
    }

    @JavascriptInterface
    public boolean setAnalyticsHeader(String headers, String channelName) {
        JSONObject json;
        try {
            json = new JSONObject(headers);
        } catch (JSONException e) {
            // Unable to parse headers
            return false;
        }
        ExecutorManager.runOnLogPusherThread(() -> {
            if (Objects.equals(juspayServices.getWorkingLogger(), "json-array") || Objects.equals(juspayServices.getWorkingLogger(), "both")) {
                LogPusher.setHeaders(json, channelName);
            }
            if (juspayServices.getLogSessioniserExp() != null) {
                LogPusherExp.setHeaders(json, channelName);
            }
        });
        return true;
    }

    @JavascriptInterface
    public boolean isFilePresent(String path) {
        return fileProviderService.isFilePresent(context, path);
    }

    @JavascriptInterface
    public String getFilePath(String fileName) {
        return fileProviderService.appendSdkNameAndVersion(fileName);
    }

    @JavascriptInterface
    public String getRootFragmentSize() {
        JSONObject sizeJSONObject = new JSONObject();

        if (juspayServices.getContainer() != null) {
            try {
                sizeJSONObject.put("height", String.valueOf(juspayServices.getContainer().getHeight()));
                sizeJSONObject.put("width", String.valueOf(juspayServices.getContainer().getWidth()));
            } catch (JSONException e) {
                try {
                    sizeJSONObject.put("height", sessionInfo.getScreenHeight() != null ? sessionInfo.getScreenHeight() : "");
                    sizeJSONObject.put("width", sessionInfo.getScreenWidth() != null ? sessionInfo.getScreenWidth() : "");
                } catch (JSONException ex) {
                    ex.printStackTrace();
                }
            }
        } else {
            try {
                sizeJSONObject.put("height", sessionInfo.getScreenHeight() != null ? sessionInfo.getScreenHeight() : "");
                sizeJSONObject.put("width", sessionInfo.getScreenWidth() != null ? sessionInfo.getScreenWidth() : "");
            } catch (JSONException ignored) {
            }
        }

        return sizeJSONObject.toString();
    }

    @JavascriptInterface
    public String requestPendingLogs(String request) {
        try {
            if (Objects.equals(juspayServices.getWorkingLogger(), "json-array") || Objects.equals(juspayServices.getWorkingLogger(), "both")) {
                return LogSessioniser.getLogsFromSessionId(new JSONObject(request));
            }
        } catch (JSONException ignored) {
        }
        return "{}";
    }

    @JavascriptInterface
    public void requestPendingLogs(String request, String callback) {
        ExecutorManager.runOnLogsPool(() -> {
            String result = "{}";
            try {
                if (Objects.equals(juspayServices.getWorkingLogger(), "json-array") || Objects.equals(juspayServices.getWorkingLogger(), "both")) {
                    result = LogSessioniser.getLogsFromSessionId(new JSONObject(request));
                }
            } catch (Exception exception) {
                // Ignore
            }
            juspayServices.getJBridge().invokeCallbackInDUIWebview(callback, result);
        });
    }

    @JavascriptInterface
    public void sessioniseLogs(String request) {
        try {
            if (Objects.equals(juspayServices.getWorkingLogger(), "json-array") || Objects.equals(juspayServices.getWorkingLogger(), "both")) {
                LogSessioniser.sessioniseLogs(new JSONObject(request));
            }
        } catch (JSONException e) {
            sdkTracker.trackException(LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.JBRIDGE, "Logs request has invalid format" + request, e);
        }
    }

    @JavascriptInterface
    public String getFileDownloadTimes() {
        return remoteAssetService.getFileDownloadTimes().toString();
    }

}
