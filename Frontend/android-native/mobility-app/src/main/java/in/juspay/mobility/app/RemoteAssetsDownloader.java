/*
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.app;

import android.app.Service;
import android.content.Intent;
import android.os.IBinder;
import android.view.View;
import android.view.ViewGroup;
import android.webkit.WebViewClient;

import androidx.annotation.Nullable;

import org.json.JSONObject;

import java.util.UUID;

import in.juspay.hypersdk.core.MerchantViewType;
import in.juspay.hypersdk.core.PaymentConstants;
import in.juspay.hypersdk.data.JuspayResponseHandler;
import in.juspay.hypersdk.ui.HyperPaymentsCallback;
import in.juspay.services.HyperServices;


public class RemoteAssetsDownloader extends Service {
    HyperServices hyperServices;

    @Override
    public void onCreate() {
        super.onCreate();
        hyperServices = new HyperServices(this);
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        JSONObject payload = new JSONObject();
        JSONObject outerPayload = new JSONObject();
        String merchantType = intent.getStringExtra("merchantType");
       if (merchantType != null && hyperServices != null && !hyperServices.isInitialised()) {
        try {
            payload.put("clientId", getResources().getString(R.string.client_id));
            payload.put("merchantId", getResources().getString(R.string.merchant_id));
            payload.put("action", "initiate");
            payload.put("logLevel", 1);
            payload.put("isBootable", false);
            payload.put("bundleTimeOut",-1);
            payload.put(PaymentConstants.ENV, "prod");
            outerPayload.put("requestId", UUID.randomUUID());
            outerPayload.put("service", getService(merchantType));
            outerPayload.put("betaAssets", false);
            outerPayload.put(PaymentConstants.PAYLOAD, payload);
        } catch (Exception e){
            e.printStackTrace();
        }
        hyperServices.initiate(outerPayload, new HyperPaymentsCallback() {
            @Override
            public void onStartWaitingDialogCreated(@Nullable View view) {

            }

            @Override
            public void onEvent(JSONObject jsonObject, JuspayResponseHandler juspayResponseHandler) {
                String event = jsonObject.optString("event");
                System.out.println("onEvent -> "+ jsonObject);
                if (event.equals("terminate")) {
                    hyperServices.terminate();
                    onDestroy();
                }
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
        return START_NOT_STICKY;
    }

    @Override
    public void onDestroy() {
        stopSelf();
        hyperServices = null;
        super.onDestroy();

    }

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }
    public String getService(String service) {
        if (service.equals("USER")) {
            return "in.yatri.consumer";
        } else {
            return "in.yatri.provider";
        }
    }
    }