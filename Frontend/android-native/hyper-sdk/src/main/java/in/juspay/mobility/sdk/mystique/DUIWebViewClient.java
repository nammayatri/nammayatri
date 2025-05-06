package in.juspay.mobility.sdk.mystique;

import android.webkit.RenderProcessGoneDetail;
import android.webkit.WebView;
import android.webkit.WebViewClient;

/**
 * Created by sushobhith.sharma on 25/04/18.
 */

public class DUIWebViewClient extends WebViewClient {

    private WebClientCallback callback;

    public DUIWebViewClient(WebClientCallback callback) {
        this.callback = callback;
    }

    @Override
    public boolean shouldOverrideUrlLoading(WebView view, String url) {
        view.loadUrl(url);
        return true;
    }

    @Override
    public boolean onRenderProcessGone(WebView view, RenderProcessGoneDetail detail) {
        callback.onRenderProcessGone(view);
        return true;
    }
}
