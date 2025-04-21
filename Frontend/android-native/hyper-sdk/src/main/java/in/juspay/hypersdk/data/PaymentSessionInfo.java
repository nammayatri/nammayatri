package in.juspay.hypersdk.data;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import org.json.JSONException;
import org.json.JSONObject;

import java.lang.ref.WeakReference;

import in.juspay.hyper.constants.Labels;
import in.juspay.hyper.constants.LogCategory;
import in.juspay.hyper.constants.LogLevel;
import in.juspay.hyper.constants.LogSubCategory;
import in.juspay.hypersdk.R;
import in.juspay.hypersdk.core.JuspayServices;
import in.juspay.hypersdk.core.PaymentConstants;
import in.juspay.hypersdk.core.SdkTracker;
import in.juspay.hypersdk.safe.Godel;
import in.juspay.hypersdk.utils.IntegrationUtils;


/**
 * Created by Veera.Subbiah on 25/04/17.
 */

public class PaymentSessionInfo {
    private static final String LOG_TAG = "PaymentSessionInfo";
    @NonNull
    private final JSONObject sessionDetails = new JSONObject();

    @Nullable
    private JSONObject paymentDetails;
    private boolean godelDisabled;
    @Nullable
    private String acsJsHash;

    @NonNull
    private final SessionInfo sessionInfo;
    @NonNull
    private final SdkTracker sdkTracker;
    @NonNull
    private WeakReference<Godel> godelManager;
    @NonNull
    private final JuspayServices juspayServices;

    public PaymentSessionInfo(JuspayServices juspayServices) {
        this.juspayServices = juspayServices;
        this.godelManager = new WeakReference<>(null);
        sessionInfo = juspayServices.getSessionInfo();
        sdkTracker = juspayServices.getSdkTracker();
    }

    public static String getGodelRemotesVersion(Context context) {
        return context.getString(R.string.godel_remotes_version);
    }

    public void setPaymentDetails(String key, String value) {
        try {
            if (paymentDetails == null) {
                paymentDetails = new JSONObject();
            }

            paymentDetails.put(key, value);
        } catch (JSONException e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.PAYMENT_SESSION_INFO, "Exception while trying to set payment details", e);
        }
    }

    @Nullable
    public String getAcsJsHash() {
        return acsJsHash;
    }

    public void setAcsJsHash(@NonNull String acsJsHash) {
        this.acsJsHash = acsJsHash;
    }

    public void addToSessionDetails(String key, String value) {
        try {
            sessionDetails.put(key, value);
        } catch (JSONException e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.PAYMENT_SESSION_INFO, "Exception when adding to sessionDetails", e);
        }
    }

    @NonNull
    public JSONObject getSessionDetails() {
        return sessionDetails;
    }

    public void setGodelManager(@Nullable Godel godelManager) {
        this.godelManager = new WeakReference<>(godelManager);
    }

    public boolean isGodelEnabled() {
        if (godelDisabled) {
            return false;
        }

        Godel godel = godelManager.get();
        if (godel == null) {
            return false;
        }

        try {
            JSONObject configs = new JSONObject();
            if (godel.getConfig().has("weblab")) {
                configs = godel.getConfig().getJSONObject("weblab");
            }
            JSONObject weblab = configs;
            return !weblab.has(PaymentConstants.GODEL) || (Integer.parseInt(String.valueOf(weblab.get(PaymentConstants.GODEL))) != 0);
        } catch (JSONException e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.PAYMENT_SESSION_INFO, "Exception while retrieving Godel value", e);
        }
        return false;
    }

    public void setGodelDisabled(String reason) {
        godelDisabled = true;
        sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.PAYMENT_SESSION_INFO, "godel_switching_off", reason);
    }

    public JSONObject getPaymentDetails() {
        if (paymentDetails != null) {
            return paymentDetails;
        } else {
            return new JSONObject();
        }
    }

    public void setPaymentDetails(@NonNull JSONObject object) {
        paymentDetails = object;
    }

    public void createSessionDataMap() {
        try {
            sessionInfo.createSessionDataMap();

            JSONObject sessionData = sessionInfo.getSessionData();

            //Godel_library_info
            sessionData.put("godel_version", IntegrationUtils.getGodelVersion(juspayServices.getContext()));
            sessionData.put("godel_build_version", IntegrationUtils.getGodelBuildVersion(juspayServices.getContext()));
            sessionData.put("godel_remotes_version", getGodelRemotesVersion(juspayServices.getContext()));
            sessionData.put("is_godel", isGodelEnabled());

            sessionInfo.updateSessionData(sessionData);
        } catch (Exception e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.PAYMENT_SESSION_INFO, "Exception while creatingSession Data Map", e);
        }
    }
}
