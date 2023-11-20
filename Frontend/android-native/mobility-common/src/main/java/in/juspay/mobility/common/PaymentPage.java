/*
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */
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

import in.juspay.hyper.constants.LogCategory;
import in.juspay.hyper.constants.LogSubCategory;
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

    private static final String LOG_TAG = PaymentPage.class.getSimpleName();


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
                        bridgeComponents.getTrackerInterface().trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.USER, "initiate", "Exception in Initiate", e);
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
                bridgeComponents.getTrackerInterface().trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.USER, "process", "Exception in process", e);
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

        JSONObject payload = new JSONObject()
            .put("requestId", UUID.randomUUID())
            .put("service", "in.juspay.hyperpay")
            .put("betaAssets", false);
        JSONObject innerPayload = new JSONObject()
            .put("clientId", context.getResources().getString(context.getResources().getIdentifier("client_id","string",context.getPackageName())))
            .put("merchantId", context.getResources().getString(context.getResources().getIdentifier("merchant_id","string",context.getPackageName())))
            .put("action", "initiate")
            .put(PaymentConstants.ENV, "prod");
        payload.put(PaymentConstants.PAYLOAD,innerPayload);
        return payload;
    }

}
