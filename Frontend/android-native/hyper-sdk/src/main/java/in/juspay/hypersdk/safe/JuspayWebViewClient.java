package in.juspay.hypersdk.safe;

import android.annotation.SuppressLint;
import android.annotation.TargetApi;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import android.graphics.Bitmap;
import android.net.Uri;
import android.os.Build;
import android.os.Message;
import android.view.KeyEvent;
import android.webkit.ClientCertRequest;
import android.webkit.CookieSyncManager;
import android.webkit.HttpAuthHandler;
import android.webkit.RenderProcessGoneDetail;
import android.webkit.SafeBrowsingResponse;
import android.webkit.WebResourceError;
import android.webkit.WebResourceRequest;
import android.webkit.WebResourceResponse;
import android.webkit.WebView;
import android.webkit.WebViewClient;

import androidx.annotation.CallSuper;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.annotation.RequiresApi;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.List;

import in.juspay.hyper.constants.Labels;
import in.juspay.hyper.constants.LogCategory;
import in.juspay.hyper.constants.LogLevel;
import in.juspay.hyper.constants.LogSubCategory;
import in.juspay.hyper.core.JuspayLogger;
import in.juspay.hypersdk.core.JuspayServices;
import in.juspay.hypersdk.core.PaymentConstants;
import in.juspay.hypersdk.core.SdkTracker;
import in.juspay.hypersdk.data.PaymentSessionInfo;
import in.juspay.hypersdk.security.EncryptionHelper;
import in.juspay.hypersdk.services.FileProviderService;
import in.juspay.hypersdk.utils.Utils;

/**
 * Created by Veera.Subbiah on 03/04/17.
 */

public class JuspayWebViewClient extends WebViewClient {
    private static final String LOG_TAG = JuspayWebViewClient.class.getName();

    @Nullable
    public String latestStartUrl;
    @NonNull
    private final JuspayWebView juspayWebView;
    @NonNull
    private final Godel godelManager;
    @Nullable
    private WebViewClient delegate;

    public JuspayWebViewClient(@NonNull Godel godelManager, @NonNull JuspayWebView juspayWebView) {
        super();

        this.godelManager = godelManager;
        this.juspayWebView = juspayWebView;
        if (godelManager.getJuspayServices().getHyperCallback() != null) {
            delegate = godelManager.getJuspayServices().getHyperCallback().createJuspaySafeWebViewClient();
        }
    }

    @Override
    @CallSuper
    public void onPageStarted(WebView view, String url, Bitmap favicon) {
        super.onPageStarted(view, url, favicon);

        final JuspayServices juspayServices = godelManager.getJuspayServices();
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();

        godelManager.isRupaySupportedAdded = false;

        if (godelManager.isDuiLoaded()) {
            JSONObject tempObject = new JSONObject();

            try {
                tempObject.put("url", url);
            } catch (JSONException e) {
                sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.JUSPAY_WEBVIEW_CLIENT, "Exception while creating ACS onPageStarted event", e);
            }

            godelManager.getAcsInterface().invoke("onPageStarted", tempObject.toString());
        }

        latestStartUrl = url;
        juspayServices.getSessionInfo().set("currentUrl", url);

        if (delegate != null) {
            delegate.onPageStarted(view, url, favicon);
        }

        // Check if there is any app that can handle this URL
        try {
            List<String> allowedDeepLinkPackages = godelManager.getAllowedDeeplinkPackages();

            if (allowedDeepLinkPackages.size() == 0) {
                return;
            }

            Intent intent = new Intent(Intent.ACTION_VIEW);
            intent.setData(Uri.parse(url));

            PackageManager packageManager = godelManager.getContext().getPackageManager();

            @SuppressLint("QueryPermissionsNeeded")
            List<ResolveInfo> resolvedActivities = packageManager.queryIntentActivities(intent, PackageManager.MATCH_DEFAULT_ONLY);

            if (resolvedActivities == null) {
                return;
            }

            String resolvedPackage = null;

            for (ResolveInfo resolveInfo : resolvedActivities) {
                JuspayLogger.d(LOG_TAG, resolveInfo.activityInfo.packageName);

                if (resolvedPackage == null && allowedDeepLinkPackages.contains(resolveInfo.activityInfo.packageName)) {
                    resolvedPackage = resolveInfo.activityInfo.packageName;
                }
            }

            if (resolvedPackage == null && resolvedActivities.size() > 1) {
                JuspayLogger.e(LOG_TAG, "Too many activities");
                return;
            }

            if (resolvedActivities.size() == 0 || !allowedDeepLinkPackages.contains(resolvedActivities.get(0).activityInfo.packageName)) {
                return;
            }

            intent.setPackage(resolvedPackage);
            godelManager.getJuspayServices().startActivityForResult(intent, -1, null);

            final String callbackFire = "if (window.onDeepLinkUrlAppOpen != null) { window.onDeepLinkUrlAppOpen('{}'); }";
            godelManager.getJuspayServices().addJsToWebView(callbackFire);
        } catch (Exception e) {
            sdkTracker.trackException(PaymentConstants.Category.GODEL, LogSubCategory.ApiCall.SDK, Labels.SDK.WEBVIEW_CLIENT, "Exception when trying to launch deeplink activity for " + url, e);
        }
    }

    @Override
    @CallSuper
    public void onPageFinished(WebView view, String url) {
        super.onPageFinished(view, url);
        final JuspayServices juspayServices = godelManager.getJuspayServices();

        try {
            if (godelManager.isDuiLoaded()) {
                JSONObject tempObject = new JSONObject();
                tempObject.put("url", url);
                tempObject.put("title", view.getTitle());
                godelManager.getAcsInterface().invoke("onPageFinished", tempObject.toString());
            }

            CookieSyncManager.getInstance().sync();
            insertACS();
        } catch (JSONException e) {
            juspayServices.getSdkTracker().trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.JUSPAY_WEBVIEW_CLIENT, "Exception while creating ACS onPageFinished event", e);
        }

        if (delegate != null) {
            delegate.onPageFinished(view, url);
        }
    }

    @Nullable
    @Override
    public WebResourceResponse shouldInterceptRequest(WebView view, WebResourceRequest request) {
        WebResourceResponse response = godelManager.shouldInterceptRequest(request);

        if (delegate != null && response == null) {
            response = delegate.shouldInterceptRequest(view, request);
        }

        return response;
    }

    @Override
    @CallSuper
    public WebResourceResponse shouldInterceptRequest(WebView view, String url) {
        WebResourceResponse response = godelManager.shouldInterceptRequest(url);

        if (delegate != null && response == null) {
            response = delegate.shouldInterceptRequest(view, url);
        }

        return response;
    }

    @Override
    @CallSuper
    public void onLoadResource(WebView view, String url) {
        super.onLoadResource(view, url);

        if (delegate != null) {
            delegate.onLoadResource(view, url);
        }
    }

    @TargetApi(Build.VERSION_CODES.M)
    @Override
    @CallSuper
    public void onReceivedError(WebView view, WebResourceRequest request, WebResourceError error) {
        super.onReceivedError(view, request, error);
        final JuspayServices juspayServices = godelManager.getJuspayServices();

        try {
            if (godelManager.isDuiLoaded()) {
                JSONObject tempObject = new JSONObject();
                tempObject.put("url", view.getUrl());
                tempObject.put("title", view.getTitle());
                tempObject.put("statusCode", error.getErrorCode());

                godelManager.getAcsInterface().invoke("onReceivedError", tempObject.toString());

                insertACS();
                CookieSyncManager.getInstance().sync();
            }
        } catch (JSONException e) {
            juspayServices.getSdkTracker().trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.JUSPAY_WEBVIEW_CLIENT, "Exception while creating ACS onReceivedError event", e);
        }

        if (delegate != null) {
            delegate.onReceivedError(view, request, error);
        }
    }

    private void insertACS() throws JSONException {
        final JuspayServices juspayServices = godelManager.getJuspayServices();
        final FileProviderService fileProviderService = juspayServices.getFileProviderService();
        final PaymentSessionInfo paymentSessionInfo = godelManager.getPaymentSessionInfo();

        if (paymentSessionInfo.isGodelEnabled()) {
            godelManager.getDuiInterface().setSessionAttribute("config", godelManager.getConfig().toString());
            String command = "window.juspayContext = {}; juspayContext['web_lab_rules'] = " + godelManager.getConfig().getJSONObject("weblab");
            juspayWebView.addJsToWebView(command);
        }
        if (paymentSessionInfo.isGodelEnabled()) {
            String acs = fileProviderService.readFromFile(juspayServices.getContext(), PaymentConstants.ACS);
            juspayWebView.addJsToWebView(acs);
            juspayServices.sdkDebug(LOG_TAG, "Tracking weblab rules in acs");
            juspayWebView.addJsToWebView("__juspay.trackWebLabRules();");
            if (paymentSessionInfo.getAcsJsHash() == null) {
                paymentSessionInfo.setAcsJsHash(EncryptionHelper.md5(acs));
                juspayServices.getSdkTracker().trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.JUSPAY_WEBVIEW_CLIENT, "hash_of_inserted_acs_min_script", paymentSessionInfo.getAcsJsHash());
            }
        } else {
            juspayServices.sdkDebug(LOG_TAG, "disabling_insertion_of_java_script_since_jb_is_disabled");
        }
    }

    @Override
    public void onReceivedError(WebView view, int errorCode, String description, String failingUrl) {
        super.onReceivedError(view, errorCode, description, failingUrl);
        final JuspayServices juspayServices = godelManager.getJuspayServices();

        try {
            if (godelManager.isDuiLoaded()) {
                JSONObject tempObject = new JSONObject();
                tempObject.put("url", view.getUrl());
                tempObject.put("title", view.getTitle());
                tempObject.put("statusCode", errorCode);

                godelManager.getAcsInterface().invoke("onReceivedError", tempObject.toString());

                insertACS();
            }
        } catch (JSONException e) {
            juspayServices.getSdkTracker().trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.JUSPAY_WEBVIEW_CLIENT, "Exception while creating ACS onReceivedError event", e);
        }

        if (delegate != null) {
            delegate.onReceivedError(view, errorCode, description, failingUrl);
        }
    }

    @TargetApi(Build.VERSION_CODES.M)
    @Override
    @CallSuper
    public void onReceivedHttpError(WebView view, WebResourceRequest request, WebResourceResponse errorResponse) {
        super.onReceivedHttpError(view, request, errorResponse);

        if (delegate != null) {
            delegate.onReceivedHttpError(view, request, errorResponse);
        }
    }

    @Override
    @CallSuper
    public void onFormResubmission(WebView view, Message dontResend, Message resend) {
        super.onFormResubmission(view, dontResend, resend);

        // Don't call delegate here. This will cause security issue.
    }

    @Override
    public boolean shouldOverrideUrlLoading(WebView view, String url) {
        if (delegate != null) {
            return delegate.shouldOverrideUrlLoading(view, url);
        }

        boolean didOpenIntentApp = openIntentFromGodel(url);
        if (didOpenIntentApp) {
            return true;
        }
        return super.shouldOverrideUrlLoading(view, url);
    }

    @RequiresApi(api = Build.VERSION_CODES.N)
    @Override
    public boolean shouldOverrideUrlLoading(WebView view, WebResourceRequest request) {
        if (delegate != null) {
            return delegate.shouldOverrideUrlLoading(view, request);
        }
        boolean didOpenIntentApp = openIntentFromGodel(request.getUrl().toString());
        if (didOpenIntentApp) {
            return true;
        }
        return super.shouldOverrideUrlLoading(view, request);
    }

    @RequiresApi(api = Build.VERSION_CODES.M)
    @Override
    public void onPageCommitVisible(WebView view, String url) {
        super.onPageCommitVisible(view, url);

        if (delegate != null) {
            delegate.onPageCommitVisible(view, url);
        }
    }

    @Override
    @Deprecated
    @SuppressWarnings("deprecation")
    public void onTooManyRedirects(WebView view, Message cancelMsg, Message continueMsg) {
        super.onTooManyRedirects(view, cancelMsg, continueMsg);

        if (delegate != null) {
            delegate.onTooManyRedirects(view, cancelMsg, continueMsg);
        }
    }

    @Override
    public void doUpdateVisitedHistory(WebView view, String url, boolean isReload) {
        super.doUpdateVisitedHistory(view, url, isReload);

        // Don't call delegate here. This will cause security issue.
    }

    @Override
    public void onReceivedClientCertRequest(WebView view, ClientCertRequest request) {
        super.onReceivedClientCertRequest(view, request);

        // Don't call delegate here. This will cause security issue.
    }

    @Override
    public void onReceivedHttpAuthRequest(WebView view, HttpAuthHandler handler, String host, String realm) {
        super.onReceivedHttpAuthRequest(view, handler, host, realm);

        // Don't call delegate here. This will cause security issue.
    }

    @Override
    public boolean shouldOverrideKeyEvent(WebView view, KeyEvent event) {
        // Don't call delegate here. This will cause security issue.

        return super.shouldOverrideKeyEvent(view, event);
    }

    @Override
    public void onUnhandledKeyEvent(WebView view, KeyEvent event) {
        super.onUnhandledKeyEvent(view, event);

        // Don't call delegate here. This will cause security issue.
    }

    @Override
    public void onScaleChanged(WebView view, float oldScale, float newScale) {
        super.onScaleChanged(view, oldScale, newScale);

        if (delegate != null) {
            delegate.onScaleChanged(view, oldScale, newScale);
        }
    }

    @Override
    public void onReceivedLoginRequest(WebView view, String realm, @Nullable String account, String args) {
        super.onReceivedLoginRequest(view, realm, account, args);

        // Don't call delegate here. This will cause security issue.
    }

    @Override
    public boolean onRenderProcessGone(WebView view, RenderProcessGoneDetail detail) {

        if (view == juspayWebView) {
            godelManager.getJuspayServices().getSdkTracker().trackLifecycle(LogSubCategory.Action.SYSTEM, LogLevel.ERROR, Labels.System.ON_RENDER_PROCESS_GONE, "error", "godel render process is gone");
            godelManager.getAcsInterface().invoke("onRenderProcessGone", JSONObject.NULL.toString());
            godelManager.getDuiInterface().onWebViewReleased();
        }
        return true;
    }

    @RequiresApi(api = Build.VERSION_CODES.O_MR1)
    @Override
    public void onSafeBrowsingHit(WebView view, WebResourceRequest request, int threatType, SafeBrowsingResponse callback) {
        super.onSafeBrowsingHit(view, request, threatType, callback);

        // Don't call delegate here. This will cause security issue.
    }

    private JSONArray getIntentUris() {
        final JuspayServices juspayServices = godelManager.getJuspayServices();
        JSONObject sdkConfig = juspayServices.getSdkConfigService().getSdkConfig();
        return Utils.optJSONArray(sdkConfig, "intentURIs");
    }

    private boolean openIntentFromGodel(String url) {
        JSONArray intentUris = getIntentUris();
        try {
            for (int i = 0; i < intentUris.length(); i++) {
                String currentUri = intentUris.getString(i);
                if (url.startsWith(currentUri)) {
                    godelManager.getJuspayServices().getSdkTracker().trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.JUSPAY_WEBVIEW_CLIENT, "intent_uri", url);
                    Intent intent = new Intent(Intent.ACTION_VIEW, Uri.parse(url));
                    godelManager.getJuspayServices().startActivityForResult(intent, -1, null);

                    JSONObject tempObj = new JSONObject();
                    tempObj.put("url", url);
                    godelManager.getAcsInterface().invoke("openIntentFromGodel", tempObj.toString());
                    return true;
                }
            }
        } catch (Exception e) {
            godelManager.getJuspayServices().getSdkTracker().trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.JUSPAY_WEBVIEW_CLIENT, "Exception in shouldOverrideUrlLoading", e);
        }
        return false;
    }
}
