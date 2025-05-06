package in.juspay.mobility.sdk.core;

import android.webkit.WebView;

/**
 * A callback which merchants can use to configure the JuspayWebView. Will only be called for merchants
 * who are whitelisted by Juspay.
 */
public interface JuspayWebViewConfigurationCallback {
    void configureJuspayWebView(WebView juspayWebView);
}
