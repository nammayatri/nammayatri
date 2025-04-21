package in.juspay.hypersdk.safe;

import android.content.Context;
import android.text.InputType;
import android.util.AttributeSet;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputConnection;
import android.view.inputmethod.InputMethodManager;
import android.webkit.WebChromeClient;
import android.webkit.WebView;
import android.webkit.WebViewClient;

import androidx.annotation.Keep;
import androidx.annotation.Nullable;

import in.juspay.hyper.core.ExecutorManager;
import in.juspay.hyper.core.JuspayLogger;


/**
 * Created by Veera.Subbiah on 04/04/17.
 */

public class JuspayWebView extends WebView {
    private static final String LOG_TAG = JuspayWebView.class.getSimpleName();
    private WebViewClient webViewClient;
    private WebChromeClient webChromeClient;
    private Integer keyboardInputType = null;
    private long lastKeyboardTypeSetTime = 0;

    public JuspayWebView(Context context, AttributeSet attrs) {
        super(context, attrs);
    }

    public JuspayWebView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }


    @Keep
    @Override
    public void loadData(String data, @Nullable String mimeType, @Nullable String encoding) {
        super.loadData(data, mimeType, encoding);
    }

    @Keep
    @Override
    public void loadDataWithBaseURL(@Nullable String baseUrl, String data, @Nullable String mimeType, @Nullable String encoding, @Nullable String historyUrl) {
        super.loadDataWithBaseURL(baseUrl, data, mimeType, encoding, historyUrl);
    }

    public JuspayWebView(Context context, AttributeSet attrs, int defStyleAttr, int defStyleRes) {
        super(context, attrs, defStyleAttr, defStyleRes);
    }


    public JuspayWebView(Context context) {
        super(context);
    }

    @Override
    public void setWebViewClient(WebViewClient client) {
        this.webViewClient = client;
        super.setWebViewClient(client);
    }

    @Override
    public void setWebChromeClient(WebChromeClient client) {
        this.webChromeClient = client;
        super.setWebChromeClient(client);
    }

    // is this okay to be in public
    public void setDefaultWebViewClient(WebViewClient client) {
        super.setWebViewClient(client);
    }

    public void setDefaultWebChromeClient(WebChromeClient client) {
        super.setWebChromeClient(client);
    }

    public WebChromeClient getWebChromeClient() {
        return webChromeClient;
    }

    public WebViewClient getWebViewClient() {
        return webViewClient;
    }

    public void addJsToWebView(final String js) {
        ExecutorManager.runOnMainThread(() -> evaluateJavascript(js, null));
    }

    @Override
    public InputConnection onCreateInputConnection(EditorInfo outAttrs) {
        JuspayLogger.d(LOG_TAG, "Creating input connection");
        InputConnection connection = super.onCreateInputConnection(outAttrs);
        if (keyboardInputType != null) {
            int keyboardStickinessTime = 300;
            if (System.currentTimeMillis() - lastKeyboardTypeSetTime <= keyboardStickinessTime) {
                outAttrs.inputType |= keyboardInputType;
            }
        }
        if ((outAttrs.inputType & InputType.TYPE_CLASS_TEXT) == InputType.TYPE_CLASS_TEXT) {
            outAttrs.inputType = outAttrs.inputType | InputType.TYPE_TEXT_VARIATION_WEB_PASSWORD;
        }
        return connection;
    }

    public void setLastKeyboardTypeSetTime() {
        lastKeyboardTypeSetTime = System.currentTimeMillis();
    }

    public void requestNumericKeyboardShow() {
        keyboardInputType = InputType.TYPE_CLASS_NUMBER | InputType.TYPE_NUMBER_VARIATION_NORMAL;
        InputMethodManager inputMethodManager = (InputMethodManager) this.getContext().getSystemService(Context.INPUT_METHOD_SERVICE);
        setLastKeyboardTypeSetTime();
        inputMethodManager.restartInput(this);
        inputMethodManager.showSoftInput(this, InputMethodManager.SHOW_IMPLICIT);
    }

    public void requestPhoneKeyboardShow() {
        keyboardInputType = InputType.TYPE_CLASS_PHONE | InputType.TYPE_NUMBER_VARIATION_NORMAL;
        InputMethodManager inputMethodManager = (InputMethodManager) this.getContext().getSystemService(Context.INPUT_METHOD_SERVICE);
        setLastKeyboardTypeSetTime();
        inputMethodManager.restartInput(this);
        inputMethodManager.showSoftInput(this, InputMethodManager.SHOW_IMPLICIT);
    }

    public void requestPasswordKeyboardShow() {
        keyboardInputType = InputType.TYPE_CLASS_TEXT | InputType.TYPE_TEXT_VARIATION_WEB_PASSWORD;
        InputMethodManager inputMethodManager = (InputMethodManager) this.getContext().getSystemService(Context.INPUT_METHOD_SERVICE);
        setLastKeyboardTypeSetTime();
        inputMethodManager.restartInput(this);
        inputMethodManager.showSoftInput(this, InputMethodManager.SHOW_IMPLICIT);
    }
}
