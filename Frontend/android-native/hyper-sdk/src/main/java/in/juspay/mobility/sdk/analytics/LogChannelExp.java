package in.juspay.mobility.sdk.analytics;

import androidx.annotation.NonNull;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.Map;


public class LogChannelExp extends LogChannel {
    @NonNull
    private final JSONArray fallBackUrls;
    @NonNull
    private final JSONArray fallBackPublicKeys;
    @NonNull
    private final String errorUrl;

    @NonNull
    private final ArrayList<String> logsQueue = new ArrayList<>();

    public LogChannelExp(int retryAttempts, long batchCount, @NonNull String name, @NonNull String endPointProd, @NonNull String endpointSBX, @NonNull JSONObject keyProd, @NonNull JSONObject keySBX, @NonNull Map<String, String> headers, int priority, @NonNull String environment, @NonNull String encryptionLevel, @NonNull JSONArray fallBackUrls, @NonNull JSONArray fallBackPublicKeys, @NonNull String errorUrl) {
        super(retryAttempts, batchCount, name, endPointProd, endpointSBX, keyProd, keySBX, headers, priority, environment, encryptionLevel);
        this.fallBackUrls = fallBackUrls;
        this.fallBackPublicKeys = fallBackPublicKeys;
        this.errorUrl = errorUrl;
    }

    @NonNull
    public ArrayList<String> getLogsQueueExp() {
        return logsQueue;
    }

    public void pollLogsQueue(String fileName) {
        logsQueue.remove(fileName);
    }

    public void addToLogsQueue(String fileName) {
        logsQueue.add(fileName);
    }

    @NonNull
    public JSONArray getFallBackUrls() {
        return fallBackUrls;
    }

    @NonNull
    public JSONArray getFallBackPublicKeys() {
        return fallBackPublicKeys;
    }

    @NonNull
    public String getErrorUrl() {
        return errorUrl;
    }

    @NonNull
    @Override
    public String toString() {
        JSONObject json = new JSONObject();
        try {
            json.put("channelName", channelName)
                    .put("endPointProd", endPointProd)
                    .put("headers", headers)
                    .put("endpointSBX", endpointSBX)
                    .put("keyProd", keyProd)
                    .put("keySBX", keySBX)
                    .put("encryptionLevel", encryptionLevel)
                    .put("priority", priority)
                    .put("environment", environment)
                    .put("retryAttempts", retryAttempts)
                    .put("batchCount", batchCount)
                    .put("fallBackUrls", fallBackUrls)
                    .put("fallBackPublicKeys", fallBackUrls)
                    .put("errorUrl", errorUrl);

        } catch (JSONException ignore) {}
        return json.toString();
    }
}
