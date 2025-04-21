package in.juspay.hypersdk.core;

import android.webkit.JavascriptInterface;

import androidx.annotation.NonNull;

import org.json.JSONException;
import org.json.JSONObject;

import in.juspay.hyper.constants.Labels;
import in.juspay.hyper.constants.LogCategory;
import in.juspay.hyper.constants.LogSubCategory;
import in.juspay.hypersdk.ui.HyperPaymentsCallback;

/**
 * A special Javascript Interface that is only registered in Godel WebView. Allows merchants to pass
 * some message from WebView to their native clients.
 *
 * @author Sri Harsha Chilakapati [sri.harsha@juspay.in]
 * @since 18/04/2020
 */
public class GodelJsInterface {
    private static final String LOG_TAG = "GodelJsInterface";

    @NonNull
    private final JuspayServices juspayServices;

    public GodelJsInterface(@NonNull JuspayServices juspayServices) {
        this.juspayServices = juspayServices;
    }

    @JavascriptInterface
    public void sendMessage(String message) {
        if (juspayServices.getHyperCallback() == null) {
            return;
        }

        HyperPaymentsCallback callback = juspayServices.getHyperCallback();

        JSONObject event = new JSONObject();

        try {
            event.put("event", "godel_merchant_message");
            event.put("payload", message);
        } catch (JSONException e) {
            final SdkTracker sdkTracker = juspayServices.getSdkTracker();
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.JBRIDGE, "exception on godelJsInterface", e);
        }

        callback.onEvent(event, null);
    }
}
