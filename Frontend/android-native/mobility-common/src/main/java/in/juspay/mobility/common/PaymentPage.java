package in.juspay.mobility.common;

import android.content.Context;
import android.util.Base64;
import android.util.Log;
import android.view.View;
import android.view.ViewGroup;
import android.webkit.WebViewClient;

import androidx.annotation.Nullable;
import androidx.fragment.app.FragmentActivity;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.UUID;

import in.juspay.hyper.core.BridgeComponents;
import in.juspay.hyper.core.ExecutorManager;
import in.juspay.hypersdk.core.MerchantViewType;
import in.juspay.hypersdk.core.PaymentConstants;
import in.juspay.hypersdk.data.JuspayResponseHandler;
import in.juspay.hypersdk.ui.HyperPaymentsCallback;
import in.juspay.mobility.app.Utils;
import in.juspay.services.HyperServices;

public class PaymentPage {

    private HyperServices hyperInstance;
    private final BridgeComponents bridgeComponents;

    private static final String LOG_TAG = "PAYMENT_PAGE";


    public PaymentPage(BridgeComponents bridgeComponents) {
        this.bridgeComponents = bridgeComponents;
    }

    public void initiate (String bootData) {
        ExecutorManager.runOnMainThread(() -> {
            if (bridgeComponents.getActivity() != null) {
                if (hyperInstance != null && hyperInstance.isInitialised()) {
                    return;
                } // Checking whether it is already initiated.
                JSONObject initiatePayload = new JSONObject();
                try {
                    initiatePayload = new JSONObject(bootData);
                } catch (Exception e) {
                    try {
                        initiatePayload = getInitiatePayload(bridgeComponents.getContext());
                    } catch (JSONException ex) {
                        Log.e(LOG_TAG, "Unable to get initiate payload");
                    }
                }
                hyperInstance = new HyperServices((FragmentActivity) bridgeComponents.getActivity());
                hyperInstance.initiate(initiatePayload, new HyperPaymentsCallback() {

                    @Override
                    public void onStartWaitingDialogCreated(@Nullable View view) {

                    }

                    @Override
                    public void onEvent(JSONObject jsonObject, JuspayResponseHandler juspayResponseHandler) {
                        String encoded = Base64.encodeToString(jsonObject.toString().getBytes(), Base64.NO_WRAP);
                        String command = String.format("window[\"onEvent\"](atob('%s'))", encoded);
                        bridgeComponents.getJsCallback().addJsToWebView(command);
                    }

                    @Nullable
                    @Override
                    public View getMerchantView(ViewGroup viewGroup, MerchantViewType merchantViewType) {
                        return null;
                    }

                    @Nullable
                    @Override
                    public WebViewClient createJuspaySafeWebViewClient() {
                        return null;
                    }
                });
            }
        });
    }

    public void process (String payload) {
        if (hyperInstance != null) {
            try {
                hyperInstance.process(new JSONObject(payload));
            } catch (Exception e) {
                Log.e(LOG_TAG,e.toString());
            }
        }
    }

    public boolean onBackPressed () {
        return hyperInstance != null && hyperInstance.onBackPressed();
    }

    public void terminate () {
        if (hyperInstance != null) {
            hyperInstance.terminate();
            hyperInstance = null;
        }
    }

    public boolean initiateStatus() {
        if (hyperInstance != null) {
            return hyperInstance.isInitialised();
        }
        return false;
    }

    public JSONObject getInitiatePayload(Context context) throws JSONException {
        JSONObject payload = new JSONObject();
        JSONObject innerPayload = new JSONObject();
        payload.put("requestId", UUID.randomUUID());
        payload.put("service", "in.juspay.hyperpay");
        payload.put("betaAssets", false);
        innerPayload.put("clientId", context.getResources().getString(context.getResources().getIdentifier("client_id","string",context.getPackageName())));
        innerPayload.put("merchantId", context.getResources().getString(context.getResources().getIdentifier("merchant_id","string",context.getPackageName())));
        innerPayload.put("action", "initiate");
        innerPayload.put(PaymentConstants.ENV, "prod");
        payload.put(PaymentConstants.PAYLOAD,innerPayload);
        return payload;
    }

}
