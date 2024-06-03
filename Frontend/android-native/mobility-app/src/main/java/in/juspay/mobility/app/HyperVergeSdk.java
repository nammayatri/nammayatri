package in.juspay.mobility.app;

import android.content.Intent;
import android.util.Log;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import co.hyperverge.hyperkyc.HyperKyc;
import co.hyperverge.hyperkyc.data.models.HyperKycConfig;
import in.juspay.hyper.core.BridgeComponents;

public class HyperVergeSdk {
    public void initHyperVergeSdk(String accessToken,  String workFlowId, String transactionId, boolean useLocation, String defLanguageCode, String inputsJson, BridgeComponents bridgeComponents) {
        HyperKycConfig config = new HyperKycConfig(accessToken, workFlowId, transactionId);
        config.setUseLocation(useLocation);
        config.setDefaultLangCode(defLanguageCode);
        if (inputsJson.length() > 0) {
            Map<String, String> inpMap = new HashMap<>();
            JSONObject jsonObject;
            try {
                jsonObject = new JSONObject(inputsJson);
                for (Iterator<String> it = jsonObject.keys(); it.hasNext(); ) {
                        String key = it.next();
                        inpMap.put(key, jsonObject.getString(key));
                    }
                }
            catch (JSONException e) {
                Log.e("Unable find Specified Key, So returning config without setting inputs.", inputsJson);
                e.printStackTrace();
                return;
            }
            if (inpMap.size() > 0)  config.setInputs(inpMap);
            else Log.d("HyperKycConfig Inputs JSON: ", "Empty json passed as input so not initializing inputs in config");
        }
        else Log.d("HyperKycConfig Inputs JSON: ", "Not initializing inputs as inputs json passed is null");
        Intent hyperKycIntent = new HyperKyc.Contract().createIntent(bridgeComponents.getContext(), config);
        try {
            bridgeComponents.getActivity().startActivityForResult(hyperKycIntent, 54, null);
        }
        catch (NullPointerException e) {
            Log.e("Error startActivityForResult", "HyperVerge Sdk's startActivityForResult returned null pointer exception. Returning !!!!");
        }
    }
}
