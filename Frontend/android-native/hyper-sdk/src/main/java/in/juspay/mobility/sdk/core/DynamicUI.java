/*
 * Copyright (c) 2012-2017 "JUSPAY Technologies"
 * JUSPAY Technologies Pvt. Ltd. [https://www.juspay.in]
 *
 * This file is part of JUSPAY Platform.
 *
 * JUSPAY Platform is free software: you can redistribute it and/or modify
 * it for only educational purposes under the terms of the GNU Affero General
 * Public License (GNU AGPL) as published by the Free Software Foundation,
 * either version 3 of the License, or (at your option) any later version.
 * For Enterprise/Commerical licenses, contact <info@juspay.in>.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  The end user will
 * be liable for all damages without limitation, which is caused by the
 * ABUSE of the LICENSED SOFTWARE and shall INDEMNIFY JUSPAY for such
 * damages, claims, cost, including reasonable attorney fee claimed on Juspay.
 * The end user has NO right to claim any indemnification based on its use
 * of Licensed Software. See the GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/agpl.html>.
 */

package in.juspay.mobility.sdk.core;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.util.Base64;
import android.util.Log;
import android.view.Choreographer;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;

import androidx.annotation.Keep;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import org.json.JSONArray;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;

import in.juspay.mobility.sdk.hyper.core.BridgeComponents;
import in.juspay.mobility.sdk.hyper.core.ExecutorManager;
import in.juspay.mobility.sdk.hyper.core.JsCallback;
import in.juspay.mobility.sdk.hyper.core.JuspayLogger;
import in.juspay.mobility.sdk.mystique.Callback;

/**
 * Created by sahebjot on 22/06/16.
 */

final class DynamicUI implements JsCallback {
    @Nullable
    private Exception webViewCrashException = null;


    @NonNull
    private final HashMap<String, Object> globalState = new HashMap<>();

    @NonNull
    private final HashMap<String, String> activityData = new HashMap<>();

    private boolean isForeGround = true;
    private boolean isInitiated = false;
    @Nullable
    private QuickJSEngine browser;
    @NonNull
    private final DuiLogger mLogger;
    @Nullable
    private Activity activity;
    @NonNull
    private Context appContext;

    @NonNull
    Choreographer choreographer;

    @NonNull
    private int totalWebViewFailure = 0;


    @NonNull
    private final Callback callback;
    @NonNull
    private final AndroidInterface androidInterface;
    @Nullable
    private FrameLayout container;
    @NonNull
    private HashMap<String, ViewGroup> fragments;
    @NonNull
    private final Renderer renderer;
    @NonNull
    private final InflateView inflateView;
    @NonNull
    private final Map<String, Object> screenMap;
    @NonNull
    private final BridgeComponents bridgeComponents;

    @NonNull
    private final AtomicReference<WebViewState> webViewState;

    @NonNull
    private final HashMap<String, JSONArray> storedFunctions;

    private boolean enableWebViewRecreate = false;

    @NonNull
    final Map<String, Object> jsInterfaces = new HashMap<>();
    /**
     * @param context     This should be the Activity directly rather than the context. But changing it now, affects the backward compatibility. So we must do this later at a point where we are sure to go aggressive on the changes.
     * @param errCallback Logging interface
     */
    public DynamicUI(@NonNull Context context, @NonNull DuiLogger duiLogger, @NonNull Callback errCallback, @NonNull BridgeComponents bridgeComponents, Map<String, Object> javaScriptInterfaces) {
        this.mLogger = duiLogger;
        this.callback = errCallback;
        this.bridgeComponents = bridgeComponents;
//        this.choreographer = Choreographer.getInstance();
        this.webViewState = new AtomicReference<>(WebViewState.Null);
        this.storedFunctions = new HashMap<>();
        if (context instanceof Activity) {
            this.activity = (Activity) context;
        }
        this.appContext = context.getApplicationContext();
        this.screenMap = new HashMap<>();
        this.fragments = new HashMap<>();

        this.androidInterface = new AndroidInterface(this);
        this.renderer = new Renderer(this);
        this.inflateView = new InflateJSON(this);
        this.jsInterfaces.put("Android", androidInterface);
        this.jsInterfaces.putAll(javaScriptInterfaces);
        ExecutorManager.runOnJSThread(this::createWebView);
    }

    public void storeActivityData(String key, String value) {
        activityData.put(key, value);
    }

    public String getActivityData(String key) {
        if (activityData.containsKey(key)) {
            return activityData.get(key);
        }
        return "";
    }


    public void setWebViewRecreate(boolean b) {
        enableWebViewRecreate = b;
    }


    public Object getGlobalState(String key) {
        return globalState.get(key);
    }

    @Keep // will be used by reflection
    @SuppressWarnings("unused")
    public HashMap<String, Object> getAllGlobalState() {
        return globalState;
    }

    public void setGlobalState(String key, Object value) {
        globalState.put(key, value);
    }

    public String encodeUtfAndWrapDecode(String str, String LOG_TAG) {
        try {
            String encodedData = URLEncoder.encode(str, "UTF-8").replace("+", "%20");
            return String.format("decodeURIComponent('%s')", encodedData);
        } catch (UnsupportedEncodingException e) {
            JuspayLogger.e(LOG_TAG, "Failed to encode using URLEncoder");
            String encodedData = Base64.encodeToString(str.getBytes(), Base64.NO_WRAP);
            return String.format("atob('%s')", encodedData);
        }
    }

    private boolean isWebViewBroken() {
        return totalWebViewFailure > 3;
    }

    @SuppressLint({"SetJavaScriptEnabled", "JavascriptInterface"})
    private void createWebView() {
        try {
            browser = new QuickJSEngine(appContext);
            webViewState.set(WebViewState.Created);
//            setupWebView();
//            browser.getSettings().setJavaScriptEnabled(true);
            Log.i("TEST", "Reached QuickJSEngine  " + System.currentTimeMillis());
            for (Map.Entry<String, Object> interfaceEntry : jsInterfaces.entrySet()) {
                browser.addJavascriptInterface(interfaceEntry.getValue(), interfaceEntry.getKey());
            }
            Log.i("TEST", "Reached addJavascriptInterface  " + System.currentTimeMillis());
//            this.loadBaseHtml();
            browser.loadDataWithBaseURL(null, "", "text/html", "utf-8", null);
            Log.i("TEST", "Reached loadDataWithBaseURL  " + System.currentTimeMillis());
            // Inform juspay services that there was no crash
            callback.webViewLoaded(null);
            totalWebViewFailure = 0;
        } catch (Exception e) {
            e.printStackTrace();
//            totalWebViewFailure++;
//            if (isWebViewBroken()) {
//                webViewState.set(WebViewState.Broken);
//                webViewCrashException = e;
//                callback.webViewLoaded(e);
//                mLogger.logLifeCycleException(Labels.Android.WEBVIEW, "WebView creation failed " + totalWebViewFailure, e);
//
//            } else {
//                browser = null;
//                webViewState.set(WebViewState.Recreating);
//                mLogger.logLifeCycleException(Labels.Android.WEBVIEW, "Webview crashed, recreating " + totalWebViewFailure, e);
//                ExecutorManager.runOnJSThread( this::createWebView);
//            }

        }
    }


    @Keep // will be used by reflection
    @SuppressWarnings("unused")
    public void putFunction(String namespace, JSONArray function) {
        storedFunctions.put(namespace, function);
    }

    @Keep // will be used by reflection
    @SuppressWarnings("unused")
    public HashMap<String, JSONArray> getAllFunctions() {
        return storedFunctions;
    }

    public JSONArray getFunction(String fnName) {
        return storedFunctions.get(fnName);
    }

    private void logError(String message) {
        mLogger.e("DynamicUI", message);
    }

    boolean initiate() {
        totalWebViewFailure = 0;
        isInitiated = true;
        switch (webViewState.get()) {
            case Recreating:
                return true;
            case Broken:
                return false;
            case Crashed:
//                ExecutorManager.runOnJSThread(this::createWebView);
            case Null:
//                ExecutorManager.runOnJSThread(this::createWebView);
            case Created:
                return true;
            case Active:
                // Fire the active callback function
                addJsToWebView("window.bootLoad()");
                return true;
        }
        return false;
    }

    @Nullable
    Activity getActivity() {
        return this.activity;
    }

    @Nullable
    ViewGroup getContainer(String namespace) {
        if (namespace == null) return this.container;
        else return fragments.get(namespace);
    }

    @NonNull
    Context getAppContext() {
        return this.appContext;
    }

    @NonNull
    Renderer getRenderer() {
        return this.renderer;
    }

    @NonNull
    InflateView getInflateView() {
        return this.inflateView;
    }

    @NonNull
    public BridgeComponents getBridgeComponents() {
        return bridgeComponents;
    }

    /**
     * This is a setter for the case where Merchant wants to have different activities across
     * different "process" calls or prefetch/initiate calls.
     *
     * @param activity New activity on which context DUI should work.
     */
    public void setActivity(@NonNull Activity activity) {
        if (this.activity != activity) {
            fragments = new HashMap<>();
            getInflateView().resetState();
        }
        this.activity = activity;
        this.appContext = activity.getApplicationContext();
    }

    public void resetActivity() {
        this.activity = null;
        getInflateView().resetState();
    }

    public void setContainer(@Nullable FrameLayout container) {
        this.container = container;
        if (this.container != null && this.container.isHardwareAccelerated()) {
            this.container.setLayerType(View.LAYER_TYPE_HARDWARE, null);
        }
    }

    private void recreateWebView() {
        if (enableWebViewRecreate) {
            callback.setWebViewRecreatePayload();
            browser = null;
            webViewState.set(WebViewState.Null);
            if (isForeGround) {
//                ExecutorManager.runOnJSThread(this::createWebView);
            } else {
                webViewState.set(WebViewState.Crashed);
            }
        }

    }

    private void setupWebView() {
//        if (browser != null) {
//            boolean isDebuggable = appContext.getResources().getBoolean(R.bool.godel_debuggable);
//            if (isDebuggable) {
//                browser.setWebChromeClient(new WebChromeClient());
//            } else {
//                browser.setWebChromeClient(new WebChromeClient() {
//                    @Override
//                    public boolean onConsoleMessage(ConsoleMessage consoleMessage) {
//                        return true;
//                    }
//
//                });
//            }
//            WebViewClient webViewClient = new DUIWebViewClient((view) -> {
//                if (view == browser) {
//                    recreateWebView();
//                }
//
//            });
//            browser.setWebViewClient(webViewClient);
//        }
    }

    public void terminate() {
        if (browser != null) {
            isInitiated = false;
            webViewState.set(WebViewState.Null);
//            browser.loadDataWithBaseURL("http://juspay.in", "<html></html>", "text/html", "utf-8", null);
//            browser.stopLoading();
//            browser.destroy();
//            browser = null;
        } else {
            logError("Browser is not present");
        }
    }

    @Override
    public void addJsToWebView(@NonNull final String js) {
        try {
            if (browser != null) {
                browser.evaluateJavascript(js);
            } else {
                logError("browser null, call start first");
            }
        } catch (OutOfMemoryError e) {
            logError("OutOfMemoryError :" + getStringStackTrace(e));
            callback.onError("addJsToWebView", "" + getStringStackTrace(e));
        } catch (Exception e) {
            logError("Exception :" + getStringStackTrace(e));
            callback.onError("addJsToWebView", "" + getStringStackTrace(e));
        }
    }

    public void invokeFunctionInJS(@NonNull final String fnName, @NonNull final Object... args) {
        try {
            if (browser != null) {
                browser.invokeFnInJs(fnName,args);
            } else {
                logError("browser null, call start first");
            }
        } catch (OutOfMemoryError e) {
            logError("OutOfMemoryError :" + getStringStackTrace(e));
            callback.onError("addJsToWebView", "" + getStringStackTrace(e));
        } catch (Exception e) {
            logError("Exception :" + getStringStackTrace(e));
            callback.onError("addJsToWebView", "" + getStringStackTrace(e));
        }
    }

    private String getStringStackTrace(Object e) {
        StringBuilder error = new StringBuilder();
        StackTraceElement[] trace = ((Exception) e).getStackTrace();
        for (StackTraceElement stackTraceElement : trace) {
            error.append(stackTraceElement.toString()).append("\n");
        }
        return error.toString();
    }

    private void loadBaseHtml() {
//        ExecutorManager.runOnMainThread(this::loadData);
    }

    private void loadData() {
//        if (browser != null) {
//            browser.loadDataWithBaseURL(null, this.baseContent, "text/html", "utf-8", null);
//            mLogger.logLifeCycleInfo("url_loaded", "base.html");
//        }
    }

    @NonNull
    public AndroidInterface getAndroidInterface() {
        return this.androidInterface;
    }

    @SuppressLint({"JavascriptInterface", "AddJavascriptInterface"})
    public void addJavascriptInterface(Object object, String interfaceName) {
        ExecutorManager.runOnMainThread(() -> {
            if (browser != null) {
                browser.addJavascriptInterface(object, interfaceName);
            }
            jsInterfaces.put(interfaceName, object);
        });
    }

    public DuiLogger getLogger() {
        return mLogger;
    }

    public Callback getErrorCallback() {
        return callback;
    }

    public String getState() {
        return androidInterface.getState();
    }

    public void setState(String state) {
        androidInterface.setState(state);
    }

    public void addToScreenMap(String screenName, Object obj) {
        screenMap.put(screenName, obj);
    }

    public Object getViewFromScreenName(String screenName) {
        if (screenMap.containsKey(screenName)) {
            return screenMap.get(screenName);
        } else {
            return null;
        }
    }

    public String addToContainerList(@NonNull ViewGroup container) {
        String key = UUID.randomUUID().toString();
        fragments.put(key, container);
        return key;
    }

    @Nullable
    public Exception getWebViewCrashException() {
        return webViewCrashException;
    }

    public void setWebViewActive() {
        if (isInitiated) {
            addJsToWebView("window.bootLoad()");
        }
        webViewState.set(WebViewState.Active);
    }

    public void onPauseCallback() {
        isForeGround = false;
    }

    public void onResumeCallback() {
        isForeGround = true;
        if (webViewState.get() == WebViewState.Crashed) {
//            ExecutorManager.runOnJSThread(this::createWebView);
        }
    }
}
