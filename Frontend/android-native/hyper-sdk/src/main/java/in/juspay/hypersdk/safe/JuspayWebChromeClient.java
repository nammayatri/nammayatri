package in.juspay.hypersdk.safe;

import android.webkit.WebChromeClient;
import android.webkit.WebView;

import androidx.annotation.CallSuper;
import androidx.annotation.NonNull;

/**
 * Created by Veera.Subbiah on 03/04/17.
 */

public class JuspayWebChromeClient extends WebChromeClient {
    @NonNull
    private final Godel godelManager;

    public JuspayWebChromeClient(@NonNull Godel godelManager) {
        super();
        this.godelManager = godelManager;
    }

    @Override
    @CallSuper
    public void onProgressChanged(WebView view, int newProgress) {
        super.onProgressChanged(view, newProgress);
        godelManager.getDuiInterface().invokeFnInDUIWebview("onProgressChanged", String.valueOf(newProgress));
    }
}
