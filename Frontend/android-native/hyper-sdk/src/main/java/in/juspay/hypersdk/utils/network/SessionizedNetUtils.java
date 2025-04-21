package in.juspay.hypersdk.utils.network;

import androidx.annotation.IntRange;
import androidx.annotation.NonNull;

import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import in.juspay.hypersdk.data.SessionInfo;

public class SessionizedNetUtils extends NetUtils {
    @NonNull
    private final SessionInfo sessionInfo;

    public SessionizedNetUtils(@NonNull SessionInfo sessionInfo,
                               @IntRange(from = 0, to = Integer.MAX_VALUE) int connectionTimeout,
                               @IntRange(from = 0, to = Integer.MAX_VALUE) int readTimeout,
                               boolean sslPinningRequired
    ) throws NoSuchAlgorithmException, KeyManagementException {
        super(connectionTimeout, readTimeout, sslPinningRequired);
        this.sessionInfo = sessionInfo;
    }

    protected Map<String, String> getDefaultSDKHeaders() {
        Map<String, String> defaultHeaders = super.getDefaultSDKHeaders();

        defaultHeaders.put("x-merchant-id", sessionInfo.tryGetMerchantId());
        String clientId = sessionInfo.tryGetClientId();
        if (clientId != null) {
            clientId = trimClientId(clientId);
        }
        defaultHeaders.put("x-client-id", clientId);

        return defaultHeaders;
    }

    private String trimClientId(@NonNull String clientId) {
        Pattern regex = Pattern.compile("^(.*)_android$", Pattern.CASE_INSENSITIVE);
        Matcher match = regex.matcher(clientId);
        if (match.matches() && match.groupCount() > 0) {
            return match.group(1);
        }

        return clientId;
    }
}
