package in.juspay.mobility.sdk.utils;

import androidx.annotation.Nullable;

import org.json.JSONObject;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import in.juspay.mobility.sdk.hyper.core.ExecutorManager;
import in.juspay.mobility.sdk.core.JuspayServices;
import in.juspay.mobility.sdk.utils.network.NetUtils;

import okhttp3.Response;

public class TrackerFallback {
    @Nullable
    private NetUtils netUtils;
    @Nullable
    private List<String> requiredKeysList;
    private boolean enableTrackerFallback;
    private int count = 3;

    public TrackerFallback(JSONObject sdkConfig) {
        if (sdkConfig != null) {
            if (sdkConfig.has("enableTrackerFallback")) {
                try {
                    this.netUtils = new NetUtils(0, 0);
                    this.enableTrackerFallback = sdkConfig.optBoolean("enableTrackerFallback", true);
                    this.count = sdkConfig.optInt("trackerFallbackAttempts", 3);
                    this.requiredKeysList = Arrays.asList("action",
                            "orderId",
                            "clientId",
                            "merchantId",
                            "customerId",
                            "os",
                            "os_version",
                            "app_version",
                            "requestId"
                    );
                } catch (Exception ignored) {
                }
            }
        }
    }

    private HashMap<String, String> getQueryParams(JuspayServices juspayServices, JSONObject payload, LogType logtype) {

        ArrayList<String> additionalParams = new ArrayList<>();
        if (logtype == LogType.PROCESS_END) {
            additionalParams.add("errorMessage");
            additionalParams.add("errorCode");
        }

        if (logtype == LogType.INITIATE_RESULT || logtype == LogType.PROCESS_END) {
            additionalParams.add("client_id");
            additionalParams.add("merchant_id");
        }

        JSONObject innerPayload = payload.optJSONObject("payload");
        JSONObject sessionData = juspayServices.getSessionInfo().getSessionData();
        HashMap<String, String> queryParams = new HashMap<>();
        if (this.requiredKeysList != null) {
            for (String key : this.requiredKeysList) {
                if (innerPayload != null && innerPayload.has(key)) {
                    queryParams.put(key, innerPayload.optString(key, ""));
                } else if (sessionData.has(key)) {
                    queryParams.put(key, sessionData.optString(key, ""));
                } else if (payload.has(key)) {
                    queryParams.put(key, payload.optString(key, ""));
                }
            }
        }

        for (String key : additionalParams) {
            if (innerPayload != null && innerPayload.has(key)) {
                queryParams.put(key, innerPayload.optString(key, ""));
            } else if (payload.has(key)) {
                queryParams.put(key, payload.optString(key, ""));
            }
        }

        queryParams.put("sessionId", juspayServices.getSessionInfo().getSessionId());
        queryParams.put("logtype", logtype.name());

        return queryParams;
    }

    public void log(JSONObject payload, JuspayServices juspayServices, LogType logtype) {
        ExecutorManager.runOnBackgroundThread(() -> {
            try {
                if (this.enableTrackerFallback && netUtils != null) {
                    for (int i = 0; i < count; i++) {
                        HashMap<String, String> queryParams = getQueryParams(juspayServices, payload, logtype);
                        try(Response resp = netUtils.doGet("https://assets.juspay.in/a.html", new HashMap<>(), queryParams, new JSONObject(), null)) {
                            if (resp.code() == 200) {
                                break;
                            }
                        }
                    }
                }
            } catch (Exception ignored) {
            }
        });
    }
}
