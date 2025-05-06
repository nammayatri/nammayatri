package in.juspay.mobility.sdk.core;

import android.webkit.JavascriptInterface;

import androidx.annotation.NonNull;

import org.json.JSONException;
import org.json.JSONObject;

import in.juspay.mobility.sdk.hyper.constants.Labels;
import in.juspay.mobility.sdk.hyper.constants.LogCategory;
import in.juspay.mobility.sdk.hyper.constants.LogSubCategory;
import in.juspay.mobility.sdk.hyper.core.JuspayLogger;
import in.juspay.mobility.sdk.utils.Utils;

/**
 * Class that adds {@code JBridge} methods that are related to Hyper SDK.
 *
 * @author Veera Manohara Subbiah [veera.subbiah@juspay.in]
 * @author Sri Harsha Chilakapati [sri.harsha@juspay.in]
 * @since 02/04/2018
 */
public class HyperJsInterface extends JsInterface {
    private static final String LOG_TAG = "HyperJsInterface";

    public HyperJsInterface(@NonNull JuspayServices juspayServices) {
        super(juspayServices);
    }

    @JavascriptInterface
    public void setClickFeedback(String id) {
        JuspayLogger.e(LOG_TAG, "Method setClickFeedback(String) has empty body");
    }

    @JavascriptInterface
    public void exitApp(int requestCode, String reason) {
    }

    @JavascriptInterface
    public void hideKeyboard() {
        JuspayLogger.e(LOG_TAG, "Method hideKeyboard() has empty body");
    }

    @JavascriptInterface
    public void downloadApps(String location) {
        JuspayLogger.e(LOG_TAG, "Method downloadApps() has empty body");
    }

    @JavascriptInterface
    public String checkPermission(String[] permissions) {
        final JSONObject result = new JSONObject();
        final SdkTracker tracker = juspayServices.getSdkTracker();

        for (String permission : permissions) {
            try {
                result.put(permission, Utils.checkIfGranted(juspayServices, permission));
            } catch (JSONException e) {
                tracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.JBRIDGE, "Caught this exception while setting in JSON: ", e);
            }
        }

        return result.toString();
    }

    @JavascriptInterface
    public void requestPermission(String[] permissions, String permissionId) {
        juspayServices.requestPermission(permissions, Integer.parseInt(permissionId));
    }

    @JavascriptInterface
    public void onRequestPermissionsResult(int requestCode, String[] permissions, int[] grantResults) {
        JuspayLogger.e(LOG_TAG, "Please override onRequestPermissionsResult");
    }
}
