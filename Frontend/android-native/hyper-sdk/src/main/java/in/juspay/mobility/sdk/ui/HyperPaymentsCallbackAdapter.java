package in.juspay.mobility.sdk.ui;

import android.view.View;
import android.view.ViewGroup;
import android.webkit.WebViewClient;

import androidx.annotation.Nullable;

import in.juspay.mobility.sdk.core.MerchantViewType;

/**
 * An easy to use adapter class that only requires the user to override only the methods being used instead of all.
 *
 * @author Sri Harsha Chilakapati [sri.harsha@juspay.in]
 */
public abstract class HyperPaymentsCallbackAdapter implements HyperPaymentsCallback {
    @Override
    public void onStartWaitingDialogCreated(@Nullable View parent) {
    }

    @Nullable
    @Override
    public WebViewClient createJuspaySafeWebViewClient() {
        return null;
    }

    @Nullable
    @Override
    public View getMerchantView(ViewGroup parent, MerchantViewType viewType) {
        return null;
    }
}
