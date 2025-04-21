package in.juspay.hypersdk.ui;

import android.view.View;
import android.view.ViewGroup;
import android.webkit.WebViewClient;

import androidx.annotation.Keep;
import androidx.annotation.Nullable;

import org.json.JSONObject;

import in.juspay.hypersdk.core.MerchantViewType;
import in.juspay.hypersdk.data.JuspayResponseHandler;

@Keep
public interface HyperPaymentsCallback {
    void onStartWaitingDialogCreated(@Nullable View parent);

    void onEvent(JSONObject event, JuspayResponseHandler handler);

    @Nullable
    View getMerchantView(ViewGroup parent, MerchantViewType viewType);

    @Nullable
    WebViewClient createJuspaySafeWebViewClient();
}
