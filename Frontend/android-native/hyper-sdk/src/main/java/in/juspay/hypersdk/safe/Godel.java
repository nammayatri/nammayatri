package in.juspay.hypersdk.safe;

import static in.juspay.hypersdk.core.PaymentConstants.NETWORK_STATUS;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.graphics.Bitmap;
import android.view.ViewGroup;
import android.view.ViewManager;
import android.view.ViewParent;
import android.webkit.CookieManager;
import android.webkit.URLUtil;
import android.webkit.WebResourceRequest;
import android.webkit.WebResourceResponse;
import android.webkit.WebSettings;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.annotation.UiThread;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.SequenceInputStream;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.GZIPInputStream;

import in.juspay.hyper.constants.Labels;
import in.juspay.hyper.constants.LogCategory;
import in.juspay.hyper.constants.LogLevel;
import in.juspay.hyper.constants.LogSubCategory;
import in.juspay.hyper.core.ExecutorManager;
import in.juspay.hyper.core.JuspayLogger;
import in.juspay.hypersdk.R;
import in.juspay.hypersdk.core.AcsInterface;
import in.juspay.hypersdk.core.DuiInterface;
import in.juspay.hypersdk.core.GodelJsInterface;
import in.juspay.hypersdk.core.JuspayServices;
import in.juspay.hypersdk.core.JuspayWebViewConfigurationCallback;
import in.juspay.hypersdk.core.PaymentConstants;
import in.juspay.hypersdk.core.PaymentUtils;
import in.juspay.hypersdk.core.SdkTracker;
import in.juspay.hypersdk.data.KeyValueStore;
import in.juspay.hypersdk.data.PaymentSessionInfo;
import in.juspay.hypersdk.data.SessionInfo;
import in.juspay.hypersdk.services.FileProviderService;
import in.juspay.hypersdk.utils.Utils;
import in.juspay.hypersdk.utils.network.NetUtils;

import okhttp3.Response;

public class Godel {
    @NonNull
    private final JuspayWebView juspayWebView;
    @NonNull
    private final JuspayWebViewClient juspayWebViewClient;
    @NonNull
    private final JuspayWebChromeClient juspayWebChromeClient;
    @Nullable
    private final JuspayWebViewConfigurationCallback juspayWebViewConfigurationCallback;
    @NonNull
    private final JuspayServices juspayServices;
    @NonNull
    private final AcsInterface acsInterface;
    @NonNull
    private final SdkTracker sdkTracker;
    @NonNull
    private final Context context;
    @NonNull
    private final PaymentSessionInfo paymentSessionInfo;
    private static final long ON_EXCEPTION_GODEL_OFF_STICKINESS = 24 * 60 * 60 * 1000;
    @NonNull
    private final DuiInterface duiInterface;
    @Nullable
    private final JSONObject processPayload;
    @NonNull
    private JSONObject config;
    @NonNull
    private final JSONObject bundleParameters;
    @NonNull
    private final List<String> allowedDeeplinkPackages = new ArrayList<>();
    private static final String GODEL = "Godel";
    private static final String LOG_TAG = PaymentUtils.class.getSimpleName();
    public boolean isRupaySupportedAdded = false;

    @UiThread
    public Godel(@NonNull JuspayServices juspayServices) {
        this.context = juspayServices.getContext();
        this.juspayServices = juspayServices;
        Activity activity = juspayServices.getActivity();
        this.juspayWebView = new JuspayWebView(activity == null ? this.context : activity);
        this.juspayWebViewClient = new JuspayWebViewClient(this, juspayWebView);
        this.juspayWebChromeClient = new JuspayWebChromeClient(this);
        this.acsInterface = new AcsInterface(juspayServices);
        this.juspayWebViewConfigurationCallback = juspayServices.getWebViewConfigurationCallback();
        this.sdkTracker = juspayServices.getSdkTracker();
        this.duiInterface = juspayServices.getJBridge();
        this.paymentSessionInfo = juspayServices.getPaymentSessionInfo();
        this.bundleParameters = juspayServices.getSessionInfo().getBundleParams();
        this.processPayload = juspayServices.getLastProcessPayload();
        this.config = new JSONObject();
    }

    public void onDuiReady() {
        this.paymentSessionInfo.setGodelManager(this);
        juspayServices.getJBridge().attach(NETWORK_STATUS, "{}", "");
        this.setupAllowedDeeplinkPackages();
        sdkTracker.trackLifecycle(LogSubCategory.LifeCycle.HYPER_SDK, LogLevel.INFO, Labels.HyperSdk.ON_DUI_READY, "class", "HyperFragment");
    }

    public void setIsRupaySupportedAdded(boolean isRupaySupportedAdded) {
        this.isRupaySupportedAdded = isRupaySupportedAdded;
    }

    @Nullable
    private HashMap<String, String> toMap(@NonNull String jsonString) throws JSONException {
        HashMap<String, String> map = new HashMap<>();
        JSONObject json;
        try {
            json = new JSONObject(jsonString);
        } catch (JSONException e) {
            JuspayLogger.d(LOG_TAG, "Not a json string. Passing as such");
            return null;
        }
        Iterator<?> keys = json.keys();

        while (keys.hasNext()) {
            String key = (String) keys.next();
            String value = json.getString(key);
            map.put(key, value);
        }

        return map;
    }

    @Nullable
    public JSONObject getWebLabRules() {
        try {
            return config.getJSONObject("weblab");
        } catch (Exception e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.UTIL, "Unable to find weblab key in config", e);
            return null;
        }
    }

    @NonNull
    private String getAcsScript() {
        final FileProviderService fileProviderService = juspayServices.getFileProviderService();

        String juspayContextScript = "window.juspayContext = {}; juspayContext['web_lab_rules'] = " + getWebLabRules();
        return juspayContextScript + ", " + fileProviderService.readFromFile(juspayServices.getContext(), PaymentConstants.ACS);
    }

    @Nullable
    private Response getPostRequestConnection(@NonNull JSONObject timeOuts, @NonNull JSONObject postHeaders, @NonNull WebResourceRequest request) {
        try {
            Response response;
            String postParams = juspayServices.getJBridge().getSessionAttribute("iframe_form_data", "wait_for_me");
            JSONObject waitingTime = config.getJSONObject("waiting_time_postparams");
            int totalTime = waitingTime.optInt("total_time", 1000);
            int interval = waitingTime.optInt("interval", 50);
            while (postParams.equals("wait_for_me") && totalTime > 0) {
                TimeUnit.MILLISECONDS.sleep(interval);
                postParams = juspayServices.getJBridge().getSessionAttribute("iframe_form_data", "wait_for_me");
                totalTime = totalTime - interval;
            }

            if (postParams.equals("wait_for_me")) {
                return null;
            }
            juspayServices.getJBridge().setSessionAttribute("iframe_form_data", "wait_for_me");
            HashMap<String, String> postParamsMap = toMap(postParams);

            NetUtils netUtils = new NetUtils(timeOuts.optInt("connection_timeout", 10000), timeOuts.optInt("read_timeout", 10000));
            Map<String, String> headers = new HashMap<>();
            Iterator<String> keys = postHeaders.keys();
            while (keys.hasNext()) {
                String key = keys.next();
                String value = postHeaders.getString(key);
                headers.put(key, value);
            }
            response = netUtils.postUrl(new URL(request.getUrl().toString()), headers, postParamsMap, new JSONObject(), null);
            return response;
        } catch (Exception e) {
            return null;
        }
    }

    @Nullable
    private String getConnectionData(Reader reader, int bufferSize) {
        try {
            StringBuilder textBuilder = new StringBuilder();
            char[] buffer = new char[bufferSize];
            int charsRead;
            while ((charsRead = reader.read(buffer)) != -1) {
                textBuilder.append(buffer, 0, charsRead);
            }
            return textBuilder.toString();
        } catch (Exception e) {
            return null;
        }
    }

    @Nullable
    private InputStream handleHtmlFile(@Nullable Response response) {
        try {
            if (response == null) {
                return null;
            }
            String acsScript = String.format(
                    "<script>{ %s } </script></body>"
                    , getAcsScript());
            String encodingType = response.header("content-encoding");
            int bufferSize = 8000;
            String connectionData = null;
            if (encodingType != null && encodingType.equalsIgnoreCase("gzip")) {
                GZIPInputStream gzipInputStream = new GZIPInputStream(response.body().byteStream());
                try (Reader reader = new BufferedReader(new InputStreamReader(gzipInputStream, StandardCharsets.UTF_8), bufferSize)) {
                    connectionData = getConnectionData(reader, bufferSize);
                }
            } else if (encodingType == null || encodingType.equals("")) {
                try (Reader reader = new BufferedReader(new InputStreamReader(response.body().byteStream()), bufferSize)) {
                    connectionData = getConnectionData(reader, bufferSize);
                }
            }
            if (connectionData != null) {
                String dataWithACS = connectionData.replace("</body>", acsScript);
                return new ByteArrayInputStream(dataWithACS.getBytes(StandardCharsets.UTF_8));
            }
            return null;
        } catch (Exception e) {
            return null;
        }
    }

    @Nullable
    private InputStream handleJsFile(@Nullable Response response) {
        try {
            if (response == null) {
                return null;
            }
            InputStream data = response.body().byteStream();
            String acsScript = String.format(
                    "window.addEventListener('load', function() { if(!window.GK) { %s } });"
                    , getAcsScript()
            );
            InputStream acsStream = new ByteArrayInputStream(acsScript.getBytes(StandardCharsets.UTF_8));
            return new SequenceInputStream(acsStream, data);
        } catch (Exception e) {
            return null;
        }
    }

    @Nullable
    private InputStream getDataAcsFromPostRequest(@Nullable Response response, @NonNull String fileType) {
        try {
            if (".html".matches(fileType)) {
                return handleHtmlFile(response);
            } else if (".js".matches(fileType) || ".jsp".matches(fileType)) {
                return handleJsFile(response);
            }
            return null;
        } catch (Exception e) {
            return null;
        }
    }

    @Nullable
    private InputStream getDataFromGetRequest(@NonNull NetUtils netUtils, @NonNull WebResourceRequest request) {
        try {
            Response response = netUtils.doGet(request.getUrl().toString(), request.getRequestHeaders(), null, null, null);
            InputStream data = response.body().byteStream();
            String acsScript = String.format(
                    "window.addEventListener('load', function() { if(!window.GK) { %s } });"
                    , getAcsScript()
            );
            InputStream acsStream = new ByteArrayInputStream(acsScript.getBytes(StandardCharsets.UTF_8));
            return new SequenceInputStream(acsStream, data);
        } catch (Exception e) {
            return null;
        }
    }

    //Simulate a webview requesting for a js file but inject acs before the rest of the js script
    //Useful for injecting acs into any iframe
    @Nullable
    private WebResourceResponse addAcsToJSFile(@NonNull WebResourceRequest request, @NonNull String method, @NonNull String fileType, @NonNull JSONObject postHeaders, @NonNull JSONObject timeOuts) {
        try {
            InputStream dataWithACSStream;
            Response response = null;
            if (method.equals("POST")) {
                response = getPostRequestConnection(timeOuts, postHeaders, request);
                dataWithACSStream = getDataAcsFromPostRequest(response, fileType);
            } else if (method.equals("GET")) {
                NetUtils netUtils = new NetUtils(
                        timeOuts.optInt("connection_timeout", 10000),
                        timeOuts.optInt("read_timeout", 10000));
                dataWithACSStream = getDataFromGetRequest(netUtils, request);
            } else {
                return null;
            }
            if (response == null) {
                return null;
            }
            String contentType = response.header("content-type");
            String mimeType;
            if (contentType != null && contentType.indexOf(';') > -1) {
                mimeType = contentType.substring(0, contentType.indexOf(';'));
            } else {
                mimeType = contentType;
            }

            String encoding;
            if (contentType != null && contentType.indexOf(';') > -1) {
                Pattern p = Pattern.compile("charset=([\\w-_]+)");
                Matcher matcher = p.matcher(contentType);
                if (matcher.find()) {
                    encoding = matcher.group(1);
                } else {
                    encoding = null;
                }
            } else {
                encoding = null;
            }
            Map<String, String> responseHeaders = new HashMap<>();
            Map<String, List<String>> responseHeadersMM = response.headers().toMultimap();
            for (Map.Entry<String, List<String>> entry: responseHeadersMM.entrySet()) {
                String field = entry.getKey();
                List<String> values = entry.getValue();
                StringBuilder valuesStr = new StringBuilder();
                for (String value : values) {
                    if (valuesStr.length() == 0) {
                        valuesStr = new StringBuilder(value);
                    } else {
                        valuesStr.append(",").append(value);
                    }
                }
                responseHeaders.put(field, valuesStr.toString());
            }

            int statusCode = response.code();
            String reasonPhrase = response.message();
            if (dataWithACSStream != null) {
                return new WebResourceResponse(mimeType, encoding, statusCode, reasonPhrase, responseHeaders, dataWithACSStream);
            }
            return null;
        } catch (Exception e) {
            return null;
        }
    }

    public void onBrowserReady(final Activity activity, final String url, final String postData, final String id) {
        ExecutorManager.runOnMainThread(() -> {
            addWebView(activity, id);
            loadPage(url, postData);
        });
    }

    public void onBrowserReady(final Activity activity, final String url, final String htmlContent, final String mimeType, final String encoding, final String prevUrl, final String id) {
        ExecutorManager.runOnMainThread(() -> {
            addWebView(activity, id);
            juspayWebView.loadDataWithBaseURL(url, htmlContent, mimeType, encoding, prevUrl);
        });
    }

    public void addWebView(@Nullable Activity activity, String id) {
        initializeJuspayWebView(context);
        FrameLayout view = null;

        if (juspayServices.getContainer() != null) {
            view = juspayServices.getContainer().findViewById(Integer.parseInt(id));
        }
        if (view == null && activity != null) {
            view = activity.findViewById(Integer.parseInt(id));
        } else {
            sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.ERROR, Labels.System.ADD_WEBVIEW, "missing", "activity");
        }

        if (view == null) {
            sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.ERROR, Labels.System.ADD_WEBVIEW, "missing", "view");
            return;
        }

        if (juspayWebView.getParent() == view) {
            sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.WARNING, Labels.System.ADD_WEBVIEW, "parent", "parent view is same as before");
            return;
        }

        turnOffGodelIfNeeded();

        ViewParent parent = juspayWebView.getParent();
        if (parent != null) {
            sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.WARNING, Labels.System.ADD_WEBVIEW, "parent", "already present");
            if (!(parent instanceof ViewGroup)) {
                sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.ERROR, Labels.System.ADD_WEBVIEW, "parent", "not a ViewGroup");
                return;
            }
            ((ViewGroup) parent).removeView(juspayWebView);
        }

        view.addView(juspayWebView, 0);
    }

    @UiThread
    private void initializeJuspayWebView(@NonNull Context context) {
        sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.INITIALISE_JUSPAY_WEBVIEW, "started", context);
        FrameLayout.LayoutParams params = new FrameLayout.LayoutParams(FrameLayout.LayoutParams.MATCH_PARENT, FrameLayout.LayoutParams.MATCH_PARENT);

        juspayWebView.setId(R.id.juspay_browser_view);
        juspayWebView.setLayoutParams(params);
        juspayWebView.setHorizontalScrollBarEnabled(false);
        juspayWebView.setVerticalScrollBarEnabled(false);

        juspayWebView.addJavascriptInterface(acsInterface, "ACSGatekeeper");

        final FileProviderService fileProviderService = juspayServices.getFileProviderService();

        paymentSessionInfo.setPaymentDetails(bundleParameters);
        fileProviderService.addToFileCacheWhiteList("acs.jsa");

        prepareWebView();
    }


    private void turnOffGodelIfNeeded() {

        if (shouldDisableGodel(context)) {
            paymentSessionInfo.setGodelDisabled(PaymentConstants.GodelOffReasons.ON_GODEL_EXCEPTION);
        }

        if (!PaymentUtils.hasTelephonyService(juspayServices)) {
            juspayServices.sdkDebug(GODEL, "No telephony service found.. disabling JB");
            paymentSessionInfo.setGodelDisabled(PaymentConstants.GodelOffReasons.TELEPHONY_NOT_FOUND);
        }

        PaymentUtils.switchOffGodelIfLowOnMemory(this, juspayServices, paymentSessionInfo);
    }

    private boolean shouldDisableGodel(Context context) {
        if (context != null) {

            if (KeyValueStore.contains(juspayServices, "GODEL_EXCEPTION_OFF")) {
                long stickiness;
                JSONObject jsonObject = getConfig();

                stickiness = jsonObject.optLong("ON_EXCEPTION_GODEL_OFF_STICKINESS", ON_EXCEPTION_GODEL_OFF_STICKINESS);

                long diffInMillis;

                try {
                    diffInMillis = System.currentTimeMillis() - Long.parseLong(KeyValueStore.read(juspayServices, "GODEL_EXCEPTION_OFF", String.valueOf(System.currentTimeMillis())));
                } catch (NumberFormatException ne) {
                    diffInMillis = System.currentTimeMillis();
                    sdkTracker.trackAndLogException(GODEL, LogCategory.ACTION, LogSubCategory.Action.USER, Labels.User.SHOULD_DISABLE_GODEL, "Failed while parsing number", ne);
                }

                String exceptionInfo = KeyValueStore.read(juspayServices, "EXCEPTION_INFO", null);
                sdkTracker.trackAction(LogSubCategory.Action.USER, LogLevel.INFO, Labels.User.SHOULD_DISABLE_GODEL, "exception_info", exceptionInfo);

                if (diffInMillis <= stickiness) {
                    return true;
                }

                KeyValueStore.remove(juspayServices, "GODEL_EXCEPTION_OFF");
                KeyValueStore.remove(juspayServices, "EXCEPTION_OFF");
            }
        }

        return false;
    }

    @NonNull
    public JSONObject getConfig() {
        return config;
    }


    public void setConfig(@NonNull JSONObject configJson) {
        this.config = configJson;
    }

    @SuppressLint("SetJavaScriptEnabled")
    protected void prepareWebView() {
        juspayWebView.getSettings().setJavaScriptEnabled(true);
        juspayWebView.getSettings().setDomStorageEnabled(true);

        JSONObject payload = null;

        try {
            payload = bundleParameters.getJSONObject("payload");

            if (payload.optBoolean("godel_receive_merchant_messages")) {
                juspayWebView.addJavascriptInterface(new GodelJsInterface(juspayServices), "GodelInterface");
            }
        } catch (JSONException e) {
            sdkTracker.trackAndLogException(GODEL, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.INITIALISE_JUSPAY_WEBVIEW, "Initiate payload is missing", e);
        }

        juspayWebView.setDefaultWebViewClient(juspayWebViewClient);
        juspayWebView.setDefaultWebChromeClient(juspayWebChromeClient);
        juspayWebView.getSettings().setAllowFileAccess(true);

        juspayWebView.getSettings().setCacheMode(WebSettings.LOAD_DEFAULT); // load online by default

        sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.INITIALISE_JUSPAY_WEBVIEW, "enabling_third_party_cookies", true);
        CookieManager.getInstance().setAcceptThirdPartyCookies(juspayWebView, true);

        if (!juspayServices.getJBridge().execute(NETWORK_STATUS, "", "{}", "").equals(String.valueOf(true))) { // loading offline
            sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.INITIALISE_JUSPAY_WEBVIEW, "no_network", "Setting web view to load from cache if there is no network");
            juspayWebView.getSettings().setCacheMode(WebSettings.LOAD_CACHE_ELSE_NETWORK);
        }

        if (bundleParameters.has("clearCookies") && bundleParameters.optBoolean("clearCookies")) {
            sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.INITIALISE_JUSPAY_WEBVIEW, "clearing", "cookies");

            PaymentUtils.clearCookies(juspayServices);
        }

        if (payload != null) {
            final String clientId = payload.optString("clientId", "");

            if (juspayWebViewConfigurationCallback != null && isClientWhitelistedForWebViewAccess(clientId)) {
                juspayWebViewConfigurationCallback.configureJuspayWebView(juspayWebView);
                sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.GODEL_WEBVIEW_WHITELIST, "configured", "JuspayWebView");
            }
        } else {
            sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.ERROR, Labels.System.INITIALISE_JUSPAY_WEBVIEW, "missing", "JuspayWebView");
        }
    }


    private boolean isClientWhitelistedForWebViewAccess(String clientId) {
        JSONObject sdkConfig;
        sdkConfig = juspayServices.getSdkConfigService().getSdkConfig();

        JSONObject godelConfig = Utils.optJSONObject(sdkConfig, "godelConfig");
        JSONObject webViewAccess = Utils.optJSONObject(godelConfig, "webViewAccess");
        JSONArray whitelistedClientIds = Utils.optJSONArray(webViewAccess, "whitelistedClientIds");

        try {
            for (int i = 0; i < whitelistedClientIds.length(); i++) {
                if (clientId.contains(whitelistedClientIds.getString(i))) {
                    return true;
                }
            }
        } catch (JSONException e) {
            sdkTracker.trackAndLogException(GODEL, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.GODEL_WEBVIEW_WHITELIST, "Failed to read whitelisted config", e);
        }

        return false;
    }

    public void loadPage() {
        if (!bundleParameters.has("url")) {
            loadPage(bundleParameters.optString("url"), bundleParameters.optString("postData"));
        } else {
            loadPage("file:///android_assets/juspay/acs_blank.html", null);
        }
    }

    public void loadPage(final String url, final String postData) {
        try {
            bundleParameters.put("url", url);
            bundleParameters.put("postData", postData);
        } catch (JSONException e) {
            sdkTracker.trackAndLogException(GODEL, LogCategory.LIFECYCLE, LogSubCategory.LifeCycle.HYPER_SDK, Labels.System.LOAD_PAGE, "Failed to write to JSON bundle parameters", e);
        }

        if (postData != null)
            juspayWebView.postUrl(url, postData.getBytes());
        else
            juspayWebView.loadUrl(url);
    }

    public void onDuiReleased() {
        sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.ON_DUI_RELEASED, "exit_sdk", JSONObject.NULL);
        exit();
        this.paymentSessionInfo.setGodelManager(null);
    }

    public void exit() {
        ExecutorManager.runOnMainThread(this::resetWebView);
    }

    protected void resetWebView() {
        juspayWebView.setDefaultWebChromeClient(juspayWebView.getWebChromeClient());
        juspayWebView.setDefaultWebViewClient(juspayWebView.getWebViewClient());
        juspayWebView.stopLoading();
        juspayWebView.removeJavascriptInterface("ACSGatekeeper");
        juspayWebView.clearHistory();
        juspayWebView.destroy();

        if (juspayWebView.getParent() != null) {
            ((ViewManager) juspayWebView.getParent()).removeView(juspayWebView);
        }
    }

    @NonNull
    public AcsInterface getAcsInterface() {
        return acsInterface;
    }

    @NonNull
    public PaymentSessionInfo getPaymentSessionInfo() {
        return paymentSessionInfo;
    }

    @NonNull
    public JuspayServices getJuspayServices() {
        return juspayServices;
    }

    @NonNull
    public DuiInterface getDuiInterface() {
        return duiInterface;
    }

    @NonNull
    public Context getContext() {
        return context;
    }

    public boolean isDuiLoaded() {
        return true;
    }


    @NonNull
    public List<String> getAllowedDeeplinkPackages() {
        return allowedDeeplinkPackages;
    }

    public void setupAllowedDeeplinkPackages() {
        JSONObject payload = null;
        if (processPayload != null) {
            payload = processPayload.optJSONObject("payload");
        }

        if (payload == null) {
            return;
        }

        JSONArray allowedDeeplinkPackages = payload.optJSONArray("allowedDeepLinkPackages");
        this.allowedDeeplinkPackages.clear();

        if (allowedDeeplinkPackages != null) {
            for (int i = 0; i < allowedDeeplinkPackages.length(); i++) {
                String pkgName = allowedDeeplinkPackages.optString(i);

                if (pkgName != null) {
                    this.allowedDeeplinkPackages.add(pkgName);
                }
            }
        }
    }

    @NonNull
    public JuspayWebView getJuspayWebView() {
        return juspayWebView;
    }

    @Nullable
    public WebResourceResponse shouldInterceptRequest(@NonNull final String url) {
        try {
            juspayServices.sdkDebug(LOG_TAG, String.format("Intercepted URL: %s", url));

            if (URLUtil.isValidUrl(url) && isAcsToBeAddedToResource(new URL(url)) && !isRupaySupportedAdded) {
                URL streamUrl = new URL(url);
                juspayServices.sdkDebug(LOG_TAG, String.format("Intercepted URL and modified: %s", url));
                setIsRupaySupportedAdded(true);
                String scriptContent = getAcsScript();
                try (InputStream is = new ByteArrayInputStream(scriptContent.getBytes(StandardCharsets.UTF_8))) {
                    try (SequenceInputStream sequenceInputStream = new SequenceInputStream(is, streamUrl.openStream())) {
                        return new WebResourceResponse("text/javascript", "utf-8", sequenceInputStream);
                    }
                }
            } else {
                WebResourceResponse excludeResource = shouldExcludeResource(url);
                if (excludeResource != null) {
                    sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.UTIL, "url_excluded", url);
                    return excludeResource;
                }
                return null;
            }

        } catch (Exception e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.UTIL, "Error while Caching Files", e);
        }
        return null;
    }

    @Nullable
    private List<Pattern> getExcludeUrlsPatternList() {
        List<Pattern> excludeUrlsPatternList = null;
        try {
            excludeUrlsPatternList = new LinkedList<>();
            JSONArray excludeUrlPatterns = config.getJSONArray("exclude_url_patterns");
            if (isNotNull(excludeUrlPatterns))
                for (int i = 0; i < excludeUrlPatterns.length(); i++) {
                    excludeUrlsPatternList.add(Pattern.compile(excludeUrlPatterns.get(i).toString()));
                }
        } catch (JSONException e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.UTIL, "Json Exception while fetching excludeUrlPatterns from config", e);
        } catch (Exception e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.UTIL, "Exception while compiling patterns in excludeUrlPatterns from config", e);
        }
        return excludeUrlsPatternList;
    }

    @Nullable
    private WebResourceResponse shouldExcludeResource(@NonNull String url) {
        Pattern imgURLPattern = Pattern.compile(".*\\.(gif|jpg|jpeg|png)([;?].*)?$");

        Bitmap bmp = Bitmap.createBitmap(1, 1, Bitmap.Config.ARGB_4444);
        ByteArrayOutputStream stream = new ByteArrayOutputStream();
        bmp.compress(Bitmap.CompressFormat.PNG, 100, stream);
        byte[] onePixelImg = stream.toByteArray();

        List<Pattern> excludeURLPatterns = getExcludeUrlsPatternList();

        if (excludeURLPatterns == null) {
            excludeURLPatterns = new ArrayList<>();
        }

        for (Pattern elp : excludeURLPatterns) {
            if (elp.matcher(url).matches()) {
                byte[] resp;
                String contentType;

                if (imgURLPattern.matcher(url).matches()) {
                    resp = onePixelImg;
                    contentType = "text/html";
                } else {
                    contentType = "text/plain";
                    resp = "[blocked]".getBytes();
                }

                return new WebResourceResponse(contentType, "utf-8", new ByteArrayInputStream(resp));
            }
        }

        // let the webview load the url
        return null;
    }

    private boolean isAcsToBeAddedToResource(@NonNull URL url) {
        List<String> includeURLPatterns = getRupaySpecificDomains();
        if (includeURLPatterns == null) {
            return false;
        }
        for (String elp : includeURLPatterns) {
            if (url.toString().toLowerCase(Locale.getDefault()).contains(elp) && url.getPath().toLowerCase(Locale.getDefault()).endsWith(".js") && !url.getPath().toLowerCase(Locale.getDefault()).endsWith(".jsp")) {
                return true;
            }
        }
        return false;
    }

    private static boolean isNotNull(JSONArray jsonArray) {
        return jsonArray != null && jsonArray != JSONObject.NULL;
    }

    @Nullable
    private List<String> getRupaySpecificDomains() {
        ArrayList<String> rupayUrlList = null;
        try {
            JSONArray urlArray = config.getJSONArray("rupay_specific_domains");
            juspayServices.sdkDebug(LOG_TAG, "printing urlArray" + urlArray);
            if (isNotNull(urlArray)) {
                int urlLength = urlArray.length();
                rupayUrlList = new ArrayList<>(urlLength);
                for (int i = 0; i < urlLength; i++) {
                    rupayUrlList.add(urlArray.get(i).toString());
                }
            }
        } catch (JSONException e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.UTIL, "Json Exception while fetching Rupay Urls from config", e);
        } catch (Exception e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.UTIL, "Exception while getting rupay urls from config", e);
        }
        return rupayUrlList;
    }

    @Nullable
    public WebResourceResponse shouldInterceptRequest(@NonNull WebResourceRequest request) {
        final SessionInfo sessionInfo = juspayServices.getSessionInfo();

        try {
            //Hijack any js file request and inject ACS into it. Set this session attribute to "true" in any flow
            //that has bank pages loading inside an iframe. Set it back to "false" once the OTP is submitted
            //and the flow is over.
            boolean injectIntoIframes = sessionInfo.get("inject_acs_into_iframes", "false").equals("true");
            if (injectIntoIframes && request.getMethod().equals("GET")) {
                JSONArray bankJSUrls = config.optJSONArray("bank_js_urls_v2");
                if (bankJSUrls == null) {
                    bankJSUrls = config.optJSONArray("bank_js_urls");
                    if (bankJSUrls != null) {
                        for (int i = 0; i < bankJSUrls.length(); i++) {
                            Pattern pattern = Pattern.compile(bankJSUrls.getString(i));
                            Matcher matcher = pattern.matcher(request.getUrl().toString());
                            if (matcher.find()) {
                                return addAcsToJSFile(request, "GET", ".*\\.jsp?$", new JSONObject(), new JSONObject());
                            }
                        }
                    }
                } else {
                    for (int i = 0; i < bankJSUrls.length(); i++) {
                        JSONArray bankJSUrl = bankJSUrls.getJSONArray(i);
                        Pattern pattern = Pattern.compile(bankJSUrl.getString(0));
                        Matcher matcher = pattern.matcher(request.getUrl().toString());
                        if (matcher.find()) {
                            return addAcsToJSFile(request, "GET", bankJSUrl.getString(1), new JSONObject(), new JSONObject());
                        }
                    }
                }
                return null;
            } else if (request.getMethod().equals("POST")) {
                JSONArray postUrls = config.optJSONArray("post_urls");
                if (postUrls == null) {
                    return null;
                }
                for (int i = 0; i < postUrls.length(); i++) {
                    JSONObject postUrl = postUrls.getJSONObject(i);
                    Pattern pattern = Pattern.compile(postUrl.getString("url"));
                    Matcher matcher = pattern.matcher(request.getUrl().toString());
                    if (matcher.find()) {
                        return addAcsToJSFile(request, "POST", postUrl.getString("file_type"), postUrl.getJSONObject("headers"), postUrl.getJSONObject("timeout"));
                    }
                }
                return null;
            }
        } catch (Exception e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.UTIL, "Exception while adding ACS to js file", e);
            return null;
        }
        return null;
    }

}
