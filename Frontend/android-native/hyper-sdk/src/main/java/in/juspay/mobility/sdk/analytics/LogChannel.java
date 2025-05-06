package in.juspay.mobility.sdk.analytics;

import androidx.annotation.NonNull;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.Map;
import java.util.concurrent.ConcurrentLinkedQueue;


public class LogChannel {
    @NonNull
    final String channelName;
    @NonNull
    final String endPointProd;
    @NonNull
    final String endpointSBX;
    final JSONObject keyProd;
    final JSONObject keySBX;
    @NonNull
    final String encryptionLevel;
    @NonNull
    Map<String, String> headers;
    final int priority;
    @NonNull
    final String environment;
    final int retryAttempts;
    final long batchCount;
    private int currentBatchRetryAttempts;

    @NonNull
    private final ConcurrentLinkedQueue<byte[]> logsQueue;

    public LogChannel(int retryAttempts, long batchCount, @NonNull String name, @NonNull String endPointProd, @NonNull String endpointSBX, JSONObject keyProd, JSONObject keySBX, @NonNull Map<String, String> headers, int priority, @NonNull String environment, @NonNull String encryptionLevel) {
        this.channelName = name;
        this.endPointProd = endPointProd;
        this.endpointSBX = endpointSBX;
        this.keyProd = keyProd;
        this.keySBX = keySBX;
        this.headers = headers;
        this.priority = priority;
        this.environment = environment;
        this.encryptionLevel = encryptionLevel;
        this.retryAttempts = retryAttempts;
        this.batchCount = batchCount;
        this.currentBatchRetryAttempts = 0;
        this.logsQueue = new ConcurrentLinkedQueue<>();
    }

    @NonNull
    public ConcurrentLinkedQueue<byte[]> getLogsQueue() {
        return logsQueue;
    }

    public void pollLogsQueue() {
        logsQueue.poll();
    }

    public int getCurrentBatchRetryAttempts() {
        return currentBatchRetryAttempts;
    }

    public void setCurrentBatchRetryAttempts(int currentBatchRetryAttempts) {
        this.currentBatchRetryAttempts = currentBatchRetryAttempts;
    }

    public void addToLogsQueue(byte[] byteArr) {
        logsQueue.add(byteArr);
    }

    @NonNull
    public String getChannelName() {
        return channelName;
    }

    public int getRetryAttempts() {
        return retryAttempts;
    }

    public long getBatchCount() {
        return batchCount;
    }

    @NonNull
    public String getEndPointProd() {
        return endPointProd;
    }

    @NonNull
    public String getEndpointSBX() {
        return endpointSBX;
    }

    public JSONObject getKeyProd() {
        return keyProd;
    }

    public JSONObject getKeySBX() {
        return keySBX;
    }

    @NonNull
    public String getEncryptionLevel() {
        return encryptionLevel;
    }

    @NonNull
    public Map<String, String> getHeaders() {
        return headers;
    }

    public void setHeaders(@NonNull Map<String, String> headers) {
        this.headers = headers;
    }

    public int getPriority() {
        return priority;
    }

    @NonNull
    public String getEnvironment() {
        return environment;
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
                    .put("batchCount", batchCount);

        } catch (JSONException ignore) {
        }
        return json.toString();
    }
}
