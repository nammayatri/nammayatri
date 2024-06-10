package in.juspay.mobility.app;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.webkit.WebView;
import android.webkit.WebViewClient;

import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;

public class WebViewOnActivity extends AppCompatActivity {
    protected WebView webView;

    @Override
    public void onBackPressed() {
        if (webView.canGoBack()) {
            webView.post(webView::goBack);
        }
        else super.onBackPressed();
    }

    @SuppressLint("SetJavaScriptEnabled")
    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.web_view_on_activity);
        webView = findViewById(R.id.web);
        Intent intent = getIntent();
        if (intent != null) {
            String webViewUrl = intent.getStringExtra("WebViewUrl");
            webView.loadUrl(webViewUrl);
            webView.getSettings().setJavaScriptEnabled(true);
            webView.getSettings().setDomStorageEnabled(true);
            webView.getSettings().setDatabaseEnabled(true);
            webView.setWebViewClient(new WebViewClient());
        }
    }

    public void goBackToRegistrationScreen(View view) {
        super.onBackPressed();
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
    }
}
