package in.juspay.hypersdk.core;

import android.webkit.JavascriptInterface;

import androidx.annotation.NonNull;

import org.json.JSONException;
import org.json.JSONObject;

import in.juspay.hyper.constants.Labels;
import in.juspay.hyper.constants.LogCategory;
import in.juspay.hyper.constants.LogSubCategory;
import in.juspay.hyper.core.JuspayLogger;
import in.juspay.hypersdk.utils.Utils;
import com.caoccao.javet.annotations.V8Function;

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

    @JavascriptInterface @V8Function
    public void setClickFeedback(String id) {
        JuspayLogger.e(LOG_TAG, "Method setClickFeedback(String) has empty body");
    }

    @JavascriptInterface @V8Function
    public void exitApp(int requestCode, String reason) {
    }

    @JavascriptInterface @V8Function
    public void hideKeyboard() {
        JuspayLogger.e(LOG_TAG, "Method hideKeyboard() has empty body");
    }

    @JavascriptInterface @V8Function
    public void downloadApps(String location) {
        JuspayLogger.e(LOG_TAG, "Method downloadApps() has empty body");
    }

    @JavascriptInterface @V8Function
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

    @JavascriptInterface @V8Function
    public void requestPermission(String[] permissions, String permissionId) {
        juspayServices.requestPermission(permissions, Integer.parseInt(permissionId));
    }

    @JavascriptInterface @V8Function
    public void onRequestPermissionsResult(int requestCode, String[] permissions, int[] grantResults) {
        JuspayLogger.e(LOG_TAG, "Please override onRequestPermissionsResult");
    }
}
