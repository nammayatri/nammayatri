package in.juspay.mobility.common;

import android.util.Log;
import android.view.View;
import android.webkit.JavascriptInterface;

import androidx.fragment.app.FragmentActivity;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.Objects;

import in.juspay.mobility.common.MobilityCommonBridge;
import in.org.npci.bbps.BBPSService;
import in.juspay.mobility.common.BBPSAgent;

public class BBPSUtils {
    private BBPSService bbpsService;

    @JavascriptInterface
    public void initiateBBPS(String bootData) {
        JSONObject initPayload = new JSONObject();
        try {
            initPayload = new JSONObject(bootData);
            bbpsService = new BBPSService(bridgeComponent.getContext().getApplicationContext(), initPayload, new BBPSAgent());
            setBBPSinPP(bbpsService);
            Log.d(LOG_TAG, "All completed in initiate BBPS");
        } catch (Exception e) {
            Log.e(LOG_TAG, "Unable to get initial payload for BBPS");
        }
    }

    private void setBBPSinPP(BBPSService bbpsService) {

    }

}