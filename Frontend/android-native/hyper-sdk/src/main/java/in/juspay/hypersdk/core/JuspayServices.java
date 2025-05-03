package in.juspay.hypersdk.core;

import android.app.Activity;
import android.app.ActivityManager;
import android.content.Context;
import android.content.Intent;
import android.content.IntentSender;
import android.content.pm.PackageManager;
import android.os.Build;
import android.os.Bundle;
import android.security.keystore.KeyGenParameterSpec.Builder;
import android.security.keystore.KeyInfo;
import android.security.keystore.KeyProperties;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewParent;
import android.view.WindowInsets;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.FragmentActivity;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;

import org.json.JSONException;
import org.json.JSONObject;

import java.io.File;
import java.lang.reflect.Constructor;
import java.security.Key;
import java.security.KeyFactory;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;

import in.juspay.hyper.bridge.BridgeList;
import in.juspay.hyper.bridge.HyperBridge;
import in.juspay.hyper.constants.Labels;
import in.juspay.hyper.constants.LogCategory;
import in.juspay.hyper.constants.LogLevel;
import in.juspay.hyper.constants.LogSubCategory;
import in.juspay.hyper.core.BridgeComponents;
import in.juspay.hyper.core.CallbackInvoker;
import in.juspay.hyper.core.ExecutorManager;
import in.juspay.hyper.core.FileProviderInterface;
import in.juspay.hyper.core.FragmentHooks;
import in.juspay.hyper.core.JsCallback;
import in.juspay.hyper.core.JuspayLogger;
import in.juspay.hyper.core.SessionInfoInterface;
import in.juspay.hyper.core.TrackerInterface;
import in.juspay.hypersdk.R;
import in.juspay.hypersdk.analytics.LogConstants;
import in.juspay.hypersdk.analytics.LogPusher;
import in.juspay.hypersdk.analytics.LogPusherExp;
import in.juspay.hypersdk.analytics.LogSessioniserExp;
import in.juspay.hypersdk.data.KeyValueStore;
import in.juspay.hypersdk.data.PaymentSessionInfo;
import in.juspay.hypersdk.data.SdkInfo;
import in.juspay.hypersdk.data.SessionInfo;
import in.juspay.hypersdk.lifecycle.FragmentEvent;
import in.juspay.hypersdk.lifecycle.HyperActivityLaunchDelegate;
import in.juspay.hypersdk.lifecycle.HyperFragment;
import in.juspay.hypersdk.lifecycle.HyperIntentSenderDelegate;
import in.juspay.hypersdk.lifecycle.HyperRequestPermissionDelegate;
import in.juspay.hypersdk.mystique.Callback;
import in.juspay.hypersdk.services.FileProviderService;
import in.juspay.hypersdk.services.RemoteAssetService;
import in.juspay.hypersdk.services.SdkConfigService;
import in.juspay.hypersdk.ui.ActivityLaunchDelegate;
import in.juspay.hypersdk.ui.HyperPaymentsCallback;
import in.juspay.hypersdk.ui.IntentSenderDelegate;
import in.juspay.hypersdk.ui.RequestPermissionDelegate;
import in.juspay.hypersdk.utils.IntegrationUtils;
import in.juspay.hypersdk.utils.Utils;
import in.juspay.hypersmshandler.SmsComponents;
import in.juspay.hypersmshandler.SmsEventInterface;
import in.juspay.hypersmshandler.SmsServices;
import in.juspay.hypersmshandler.Tracker;
import in.juspay.mobility.app.MobilityAppBridge;
import in.juspay.mobility.customer.MobilityCustomerBridge;
import in.juspay.mobility.driver.MobilityDriverBridge;
import in.juspay.services.TenantParams;

/**
 * Core class for all the Juspay SDKs. All the SDKs should define an implementation of this class.
 *
 * @author Veera Manohara Subbiah [veera.subbiah@juspay.in]
 * @author Sri Harsha Chilakapati [sri.harsha@juspay.in]
 * @since 15/11/2017
 */
public class JuspayServices implements FragmentHooks {
    private static final String fragmentTag = "JuspayServiceFragment";

    @NonNull
    private final SdkInfo sdkInfo;

    @NonNull
    private final SessionInfo sessionInfo;
    @NonNull
    private final SdkConfigService sdkConfigService;
    private final String LOG_TAG = getClass().getSimpleName();

    @NonNull
    private final DynamicUI dynamicUI;
    @NonNull
    private final SdkTracker sdkTracker;
    @NonNull
    private final FileProviderService fileProviderService;
    @NonNull
    private final RemoteAssetService remoteAssetService;
    @NonNull
    final private PaymentSessionInfo paymentSessionInfo;
    @NonNull
    private final Context context;
    @NonNull
    private final JBridge jBridge;
    @NonNull
    private final BridgeList bridgeList = new BridgeList();
    @NonNull
    private final BridgeComponents bridgeComponents;
    @NonNull
    private final EnumSet<FragmentEvent> fragmentEvents;
    @Nullable
    private final TenantParams tenantParams;
    @NonNull
    private final String buildId;
    @NonNull
    private final SmsServices smsServices;
    @Nullable
    Runnable webViewCrashCallback;
    @Nullable
    HyperFragment fragment = null;
    @Nullable
    private String merchantClientId;
    @Nullable
    private FrameLayout container;
    @Nullable
    private JSONObject bundleParameters;
    @Nullable
    private HyperPaymentsCallback hyperCallback;
    @NonNull
    private ActivityLaunchDelegate activityLaunchDelegate;
    SmsComponents smsComponents = new SmsComponents() {
        @NonNull
        @Override
        public Tracker getTracker() {
            return sdkTracker;
        }

        @NonNull
        @Override
        public SmsEventInterface getSmsEventInterface() {
            return jBridge.getSmsEventInterface();
        }

        @NonNull
        @Override
        public Context getContext() {
            return context;
        }
    };
    @NonNull
    private IntentSenderDelegate intentSenderDelegate;
    @NonNull
    private RequestPermissionDelegate requestPermissionDelegate;
    @Nullable
    private JuspayWebViewConfigurationCallback webViewConfigurationCallback;
    @Nullable
    private FragmentActivity activity;
    @Nullable
    private JSONObject lastProcessPayload;
    @Nullable
    private LogSessioniserExp logSessioniserExp;
    private boolean paused = false;
    private boolean isPrefetch = false;
    private boolean isWebViewAvailable = true;
    @NonNull
    private String workingLogger = "json-array";

    public JuspayServices(@NonNull Context context, @Nullable TenantParams tenantParams) {
        this.tenantParams = tenantParams;
        this.sdkInfo = IntegrationUtils.getSdkInfo(context);
        this.context = context.getApplicationContext();
        this.buildId = "jus_" + IntegrationUtils.getSdkVersion(context, "_") + "_" + IntegrationUtils.getAssetAarVersion(context);

        Callback errorCallback = new Callback() {
            @Override
            public void onError(String type, String message) {
                JuspayLogger.e("DynamicUI", String.format("%s %s", type, message));
                sdkTracker.trackAction(LogSubCategory.Action.DUI, LogLevel.ERROR, Labels.HyperSdk.MYSTIQUE, type.toLowerCase(Locale.getDefault()), message);
            }

            @Override
            public void onException(String type, String message, Throwable throwable) {
                JuspayLogger.e("DynamicUI", String.format("%s %s", type, message));
                sdkTracker.trackException(LogCategory.ACTION, LogSubCategory.Action.DUI, Labels.HyperSdk.MYSTIQUE, message, throwable);
            }

            @Override
            public void webViewLoaded(Exception e) {
                if (e != null && webViewCrashCallback != null) {
                    isWebViewAvailable = false;
                    webViewCrashCallback.run();
                }
                webViewCrashCallback = null;
            }

            @Override
            public void setWebViewRecreatePayload() {
                if (bundleParameters != null) {
                    try {
                        bundleParameters.put("isWebViewRecreated", true);
                    } catch (JSONException ignored) {

                    }
                }
            }
        };

        DuiLogger duiLogger = new DuiLogger() {
            @Override
            public void d(String tag, String message) {
            }

            @Override
            public void logLifeCycleException(String label, String message, Exception e) {
                sdkTracker.trackAndLogException(LOG_TAG, LogCategory.LIFECYCLE, LogSubCategory.LifeCycle.ANDROID, label, message, e);

            }

            @Override
            public void e(String tag, String message) {
                JuspayLogger.e(tag, message);
                sdkTracker.trackAction(LogSubCategory.Action.DUI, LogLevel.ERROR, Labels.HyperSdk.MYSTIQUE, tag.toLowerCase(Locale.getDefault()), message);
            }

            @Override
            public void logLifeCycleInfo(String key, String value) {
                sdkTracker.trackLifecycle(LogSubCategory.LifeCycle.HYPER_SDK, LogLevel.INFO, Labels.HyperSdk.JUSPAY_SERVICES, key, value);
            }

            @Override
            public void i(String tag, String message) {
            }
        };

        sdkTracker = new SdkTracker(this);
        sdkConfigService = new SdkConfigService(this);
        sessionInfo = new SessionInfo(this);
        fileProviderService = new FileProviderService(this);
        remoteAssetService = new RemoteAssetService(this);
        jBridge = new JBridge(this);
        activityLaunchDelegate = new HyperActivityLaunchDelegate(this);
        intentSenderDelegate = new HyperIntentSenderDelegate(this);
        requestPermissionDelegate = new HyperRequestPermissionDelegate(this);
        sdkConfigService.renewConfig(context);
        bridgeComponents = createBridgeComponents();
        String baseContent = tenantParams != null ? tenantParams.getBaseContent() : null;
        dynamicUI = new DynamicUI(context, duiLogger, errorCallback, bridgeComponents, baseContent, getJavaScriptInterfaces());
        paymentSessionInfo = new PaymentSessionInfo(this);
        logMemoryInfo(sdkTracker, context);
        logEncryptionSupport(sdkTracker, context);
        fragmentEvents = EnumSet.allOf(FragmentEvent.class);
        smsServices = new SmsServices(smsComponents);
        JSONObject logsConfig = sdkConfigService.getSdkConfig().optJSONObject("logsConfig");
        if (logsConfig != null) {
            ExecutorManager.runOnBackgroundThread(() -> sdkTracker.setLabelsToDrop(logsConfig));
            workingLogger = logsConfig.optString("workingLogger", "json-array");
            if (Objects.equals(workingLogger, "byte-d-json") || Objects.equals(workingLogger, "both")) {
                logSessioniserExp = new LogSessioniserExp();
            }
        }
    }

    private static void deleteFiles(Context context) {
        try {
            File directory = new File(String.valueOf(context.getCacheDir()));
            File[] files = directory.listFiles();

            if (files != null) {
                for (File file : files) {
                    String fileName = file.getName();
                    if (fileName.startsWith(LogConstants.PERSISTENT_LOGS_FILE) || fileName.startsWith(LogConstants.LOGS_FILE) || fileName.startsWith(LogConstants.TEMP_LOGS_FILE)) {
                        //noinspection ResultOfMethodCallIgnored
                        file.delete();
                    }
                }
            }
        } catch (Exception e) {
            // Ignore
        }
    }

    Map<String, HyperBridge> getJBridgeList() {
        return bridgeList.getBridgeList();
    }

    protected void logEncryptionSupport(final SdkTracker sdkTracker, Context context) {
        ExecutorManager.runOnBackgroundThread(() -> {
            final JSONObject payload = new JSONObject();
            try {
                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.P) {
                    payload.put("isStrongBoxBacked", context.getPackageManager().hasSystemFeature(PackageManager.FEATURE_STRONGBOX_KEYSTORE));
                }
            } catch (Exception e) {
                // Ignored
                sdkDebug(LOG_TAG, Arrays.toString(e.getStackTrace()));
            }
            try {
                String isHardwareBacked = KeyValueStore.read(JuspayServices.this, "isHardwareBacked", null);
                if (isHardwareBacked == null) {
                    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
                        KeyPairGenerator kpg = KeyPairGenerator.getInstance("RSA", "AndroidKeyStore");
                        kpg.initialize(new Builder("juspay", KeyProperties.PURPOSE_ENCRYPT | KeyProperties.PURPOSE_DECRYPT).build());
                        KeyPair kp = kpg.generateKeyPair();
                        Key k = kp.getPrivate();
                        KeyFactory kf = KeyFactory.getInstance(k.getAlgorithm(), "AndroidKeyStore");
                        KeyInfo ki = kf.getKeySpec(k, KeyInfo.class);
                        payload.put("isHardwareBacked", ki.isInsideSecureHardware());
                        KeyValueStore.write(JuspayServices.this, "isHardwareBacked", ki.isInsideSecureHardware() + "");
                    } else {
                        payload.put("isHardwareBacked", false);
                        KeyValueStore.write(JuspayServices.this, "isHardwareBacked", "false");
                    }
                    sdkTracker.trackContext(LogSubCategory.Context.DEVICE, LogLevel.INFO, Labels.Device.PHONE_STATE, payload);
                }
            } catch (Exception ignored) {
            }
        });
    }

    private void logMemoryInfo(SdkTracker sdkTracker, Context context) {
        ExecutorManager.runOnBackgroundThread(() -> {
            ActivityManager.MemoryInfo memoryInfo = Utils.getMemoryInfo(context);
            if (memoryInfo == null) {
                return;
            }
            JSONObject memoryObj = new JSONObject();
            try {
                memoryObj.put("available_memory", memoryInfo.availMem);
                memoryObj.put("threshold_memory", memoryInfo.threshold);
                memoryObj.put("total_memory", memoryInfo.totalMem);
                sdkTracker.trackContext(LogSubCategory.Context.DEVICE, LogLevel.INFO, Labels.Device.MEMORY, "memory_info", memoryObj);
            } catch (Exception exception) {
                sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.SESSION_INFO, "Exception while logging memory_info", exception);
            }
        });
    }

    @Nullable
    public FrameLayout getContainer() {
        return container;
    }

    @NonNull
    public final SdkInfo getSdkInfo() {
        return sdkInfo;
    }

    @NonNull
    public SessionInfo getSessionInfo() {
        return sessionInfo;
    }

    @NonNull
    DynamicUI getDynamicUI() {
        return dynamicUI;
    }

    @Nullable
    public HyperFragment getFragment() {
        return fragment;
    }

    @Nullable
    public LogSessioniserExp getLogSessioniserExp() {
        return logSessioniserExp;
    }

    @NonNull
    public String getWorkingLogger() {
        return workingLogger;
    }

    @Override
    public void startActivityForResult(@NonNull Intent intent, int requestCode, @Nullable Bundle bundle) {
        activityLaunchDelegate.startActivityForResult(intent, requestCode, bundle);
    }

    @Override
    public void startIntentSenderForResult(@NonNull IntentSender intentSender, int requestCode, Intent fillInIntent, int flagMask, int flagValues, int extraFlags, Bundle options) {
        intentSenderDelegate.startIntentSenderForResult(intentSender, requestCode, fillInIntent, flagMask, flagValues, extraFlags, options);
    }

    @Override
    public void requestPermission(@NonNull String[] permissions, int requestCode) {
        requestPermissionDelegate.requestPermission(permissions, requestCode);
    }

    public void onActivityResult(int requestCode, int resultCode, @Nullable Intent data) {
        if (resultCode == Activity.RESULT_OK) {
            sdkTracker.trackLifecycle(LogSubCategory.LifeCycle.HYPER_SDK, LogLevel.INFO, Labels.Android.ON_ACTIVITY_RESULT, "result_code", "RESULT_OK");

            if (data != null && data.getExtras() != null) {
                sdkTracker.trackLifecycle(LogSubCategory.LifeCycle.HYPER_SDK, LogLevel.INFO, Labels.Android.ON_ACTIVITY_RESULT, "result_code", Utils.toJSON(data.getExtras()));
            }
        } else if (resultCode == Activity.RESULT_CANCELED) {
            sdkTracker.trackLifecycle(LogSubCategory.LifeCycle.HYPER_SDK, LogLevel.INFO, Labels.Android.ON_ACTIVITY_RESULT, "result_code", "RESULT_CANCELLED");
        }

        for (HyperBridge bridge : bridgeList.getBridgeList().values()) {
            if (bridge.onActivityResult(requestCode, resultCode, data)) {
                return;
            }
        }
        jBridge.onActivityResult(requestCode, resultCode, data);
    }

    public void onRequestPermissionsResult(int requestCode, String[] permissions, int[] grantResults) {
        sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.ON_REQUEST_PERMISSION_RESULT, "data", "requestCode = [" + requestCode + "]," + "permissions = [" + Arrays.toString(permissions) + "], grantResults = [" + Arrays.toString(grantResults) + "]");
        for (HyperBridge bridge : bridgeList.getBridgeList().values()) {
            if (bridge.onRequestPermissionResult(requestCode, permissions, grantResults)) {
                return;
            }
        }
        jBridge.onRequestPermissionsResult(requestCode, permissions, grantResults);
    }

    public void addJsToWebView(String js) {
        dynamicUI.addJsToWebView(js);
    }

    public void setBundleParameter(@NonNull JSONObject params) {
        try {
            params.put(PaymentConstants.SDK_NAME, sdkInfo.getSdkName());
            params.put(PaymentConstants.SDK_VERSION, sdkInfo.getSdkVersion());
            this.bundleParameters = params;

            JSONObject payload = params.getJSONObject("payload");

            boolean isSandbox = false;
            if (payload.has("environment")) {
                isSandbox = payload.getString("environment").equalsIgnoreCase("sandbox");
            }
            sdkTracker.setEndPointSandbox(isSandbox);
            if (payload.has("clientId")) {
                merchantClientId = payload.getString("clientId");
            }

            setUpMerchantFragments(payload);

            sessionInfo.setBundleParams(params);
            ExecutorManager.runOnBackgroundThread(() -> {
                try {
                    sessionInfo.createSessionDataMap();
                    sessionInfo.logDeviceIdentifiers();
                    JSONObject sessionData = sessionInfo.getSessionData();
                    sessionData.put("merchant_id", sessionInfo.getMerchantId());
                    sessionData.put("client_id", sessionInfo.getClientId().split("_")[0].toLowerCase(Locale.getDefault()));
                    sessionData.put("session_id", sessionInfo.getSessionId());
                    sessionInfo.logSessionInfo();
                    ExecutorManager.runOnLogSessioniserThread(() -> {
                        if (Objects.equals(workingLogger, "json-array") || Objects.equals(workingLogger, "both")) {
                            LogPusher.setLogHeaderValues(sessionData, LogConstants.DEFAULT_CHANNEL);
                        }
                        if (logSessioniserExp != null) {
                            LogPusherExp.setLogHeaderValues(sessionData, LogConstants.DEFAULT_CHANNEL);
                        }
                    });
                } catch (Exception e) {
                    sdkTracker.trackAndLogException(LOG_TAG, LogCategory.LIFECYCLE, LogSubCategory.LifeCycle.HYPER_SDK, Labels.HyperSdk.SET_BUNDLE_PARAMS, "Exception while setting bundle parameter", e);
                }
            });
        } catch (Exception e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.LIFECYCLE, LogSubCategory.LifeCycle.HYPER_SDK, Labels.HyperSdk.SET_BUNDLE_PARAMS, "Exception while setting bundle parameter", e);
        }
    }

    @Nullable
    public JSONObject getBundleParameters() {
        return bundleParameters;
    }

    @NonNull
    public JBridge getJBridge() {
        return jBridge;
    }

    @NonNull
    public SmsServices getSmsServices() {
        return smsServices;
    }

    public void setUpMerchantFragments(JSONObject payload) {
        if (payload.has(PaymentConstants.FRAGMENT_VIEW_GROUPS)) {
            if (activity != null) {
                try {
                    JSONObject fragments = payload.getJSONObject(PaymentConstants.FRAGMENT_VIEW_GROUPS);
                    Iterator<String> viewKeys = fragments.keys();
                    while (viewKeys.hasNext()) {
                        String key = viewKeys.next();
                        Object containerLayout = fragments.opt(key);
                        if (containerLayout instanceof ViewGroup) {
                            FrameLayout layout = createSubLayout(activity);
                            ((ViewGroup) containerLayout).addView(layout);
                            fragments.put(key, dynamicUI.addToContainerList(layout));
                        }
                    }
                } catch (JSONException ignored) {
                }
            }
        }
    }

    private FrameLayout createSubLayout(Activity activity) {
        FrameLayout duiContainer = new FrameLayout(activity);
        FrameLayout.LayoutParams layoutParams = new FrameLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.MATCH_PARENT);
        duiContainer.setLayoutParams(layoutParams);
        duiContainer.setVisibility(View.VISIBLE);
        return duiContainer;
    }

    private void firstTimeSetup() {
        final String namespace = sdkInfo.getSdkName();
        String prevBuildId = KeyValueStore.read(context, namespace, PaymentConstants.BUILD_ID, "__failed");

        if (!prevBuildId.equals(buildId)) {
            sdkTracker.trackLifecycle(LogSubCategory.LifeCycle.HYPER_SDK, LogLevel.INFO, Labels.HyperSdk.FIRST_TIME_SETUP, "started", null);
            KeyValueStore.write(context, namespace, PaymentConstants.BUILD_ID, buildId);
            KeyValueStore.remove(context, namespace, "asset_metadata.json");

            File juspayDir = context.getDir("juspay", Context.MODE_PRIVATE);

            if (juspayDir.exists()) {
                Utils.deleteRecursive(juspayDir);
            }

            try {
                File cacheFile = new File(context.getCacheDir(), "juspay-logs-queue.dat");
                //noinspection ResultOfMethodCallIgnored
                cacheFile.delete();
                cacheFile = new File(context.getCacheDir(), "temp-logs-queue.dat");
                //noinspection ResultOfMethodCallIgnored
                cacheFile.delete();
                cacheFile = new File(context.getCacheDir(), "juspay-pre-logs-queue.dat");
                //noinspection ResultOfMethodCallIgnored
                cacheFile.delete();
                deleteFiles(context);

                sdkTracker.trackLifecycle(LogSubCategory.LifeCycle.HYPER_SDK, LogLevel.INFO, Labels.HyperSdk.FIRST_TIME_SETUP, "completed", null);

            } catch (Exception e) {
                // Handles JSONException and Security exception
                sdkTracker.trackAndLogException(LOG_TAG, LogCategory.LIFECYCLE, LogSubCategory.LifeCycle.HYPER_SDK, Labels.HyperSdk.FIRST_TIME_SETUP, "Exception while fetching meta-data for manifest.json file", e);
            }
        }
    }

    public void initiate(Runnable onWebViewCrashed) {
        firstTimeSetup();
        boolean useLocalAssets = context.getResources().getBoolean(R.bool.use_local_assets);
        JSONObject bundleParams = getBundleParameters();
        if (bundleParams != null) {
            useLocalAssets = bundleParams.optBoolean("useLocalAssets", useLocalAssets);
        }
        prefetchBootLoaderFile(bundleParameters, !useLocalAssets);

        webViewCrashCallback = onWebViewCrashed;

        boolean willHandleInitiate = dynamicUI.initiate();
        if (!willHandleInitiate) {
            webViewCrashCallback = null;
            isWebViewAvailable = false;
            onWebViewCrashed.run();
            return;
        }
        // Keeping callback in case WebView crashes at a later point
        // In case initiate is called after terminate WebView is created on demand
    }

    @NonNull
    private BridgeComponents createBridgeComponents() {
        return new BridgeComponents() {
            @NonNull
            @Override

            public Context getContext() {
                return context;
            }

            @Nullable
            @Override
            public Activity getActivity() {
                return activity;
            }

            @NonNull
            @Override
            public FragmentHooks getFragmentHooks() {
                return JuspayServices.this;
            }

            @NonNull
            @Override
            public TrackerInterface getTrackerInterface() {
                return sdkTracker;
            }

            @NonNull
            @Override
            public CallbackInvoker getCallbackInvoker() {
                return jBridge;
            }

            @NonNull
            @Override
            public FileProviderInterface getFileProviderInterface() {
                return fileProviderService;
            }

            @NonNull
            @Override
            public JsCallback getJsCallback() {
                return dynamicUI;
            }

            @NonNull
            @Override
            public String getSdkName() {
                return sdkInfo.getSdkName();
            }

            @NonNull
            @Override
            public JSONObject getSdkConfig() {
                return sdkConfigService.getSdkConfig();
            }

            @Nullable
            @Override
            public String getClientId() {
                String clientId = null;
                try {
                    clientId = getSessionInfo().getClientId();
                } catch (Exception ignored) {
                }
                return clientId;
            }

            @NonNull
            @Override
            public SessionInfoInterface getSessionInfoInterface() {
                return sessionInfo;
            }
        };
    }

    private Map<String, Object> getJavaScriptInterfaces() {
        Map<String, Object> bridges = new HashMap<>();
        bridges.put("JBridgeActual", jBridge);
        bridges.put(bridgeList.getInterfaceName(), bridgeList);

        // Mobility
        if (PaymentUtils.isClassAvailable("in.juspay.mobility.customer.MobilityCustomerBridge")) {
            bridgeList.addHyperBridge(new MobilityCustomerBridge(bridgeComponents));
        }

        if (PaymentUtils.isClassAvailable("in.juspay.mobility.driver.MobilityDriverBridge")) {
            bridgeList.addHyperBridge(new MobilityDriverBridge(bridgeComponents));
        }

        if (PaymentUtils.isClassAvailable("in.juspay.mobility.app.MobilityAppBridge")) {
            bridgeList.addHyperBridge(new MobilityAppBridge(bridgeComponents));
        }

        try {
            if (tenantParams != null) {
                for (Class<? extends HyperBridge> tenantClass : tenantParams.getBridgeClasses()) {
                    Constructor<?>[] constructors = tenantClass.getConstructors();
                    for (Constructor<?> constructor : constructors) {
                        if (constructor.getParameterTypes().length == 1 && constructor.getParameterTypes()[0].equals(BridgeComponents.class)) {
                            bridgeList.addHyperBridge((HyperBridge) constructor.newInstance(bridgeComponents));
                        }
                    }
                }
            }
        } catch (Exception exception) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.LIFECYCLE, LogSubCategory.LifeCycle.HYPER_SDK, Labels.HyperSdk.ADD_BRIDGE, "Exception while trying to add tenant bridge", exception);
        }

        bridges.putAll(bridgeList.getBridgeList());
        return bridges;
    }

    private void prefetchBootLoaderFile(@Nullable JSONObject bundleParams, boolean shouldDownload) {
        if (!shouldDownload) return;
        remoteAssetService.renewFile(context, getBootloaderEndpoint(bundleParams), null, "v1-boot_loader.zip", System.currentTimeMillis());
    }

    @NonNull
    private String getBootloaderEndpoint(@Nullable JSONObject bundleParams) {
        if (this.tenantParams != null) {
            String endpoint = tenantParams.getBootLoaderEndpoint();
            if (endpoint != null) {
                return endpoint;
            }
        }
        String sandboxPrefix = "";
        String clientId = "common";
        if (bundleParams != null && bundleParams.optBoolean(PaymentConstants.BETA_ASSETS, false)) {
            sandboxPrefix = "sandbox.";
        } else if (merchantClientId != null) {
            clientId = merchantClientId.toLowerCase(Locale.getDefault()).split("_")[0];
        }
        return String.format(Constants.BOOTLOADER_REMOTE_ASSET_PATH_FORMAT, sandboxPrefix, clientId);
    }

    @Nullable
    public HyperPaymentsCallback getHyperCallback() {
        return hyperCallback;
    }

    public void setHyperCallback(@NonNull HyperPaymentsCallback hyperCallback) {
        this.hyperCallback = hyperCallback;
    }

    public void setActivityLaunchDelegate(@NonNull ActivityLaunchDelegate activityLaunchDelegate) {
        this.activityLaunchDelegate = activityLaunchDelegate;
    }

    public void setIntentSenderDelegate(@NonNull IntentSenderDelegate intentSenderDelegate) {
        this.intentSenderDelegate = intentSenderDelegate;
    }

    public void setRequestPermissionDelegate(@NonNull RequestPermissionDelegate requestPermissionDelegate) {
        this.requestPermissionDelegate = requestPermissionDelegate;
    }

    public void process(@NonNull FragmentActivity activity, @NonNull ViewGroup container) {
        smsServices.createSMSConsent();
        if (activity == this.activity && this.container != null && container == this.container.getParent()) {
            return;
        }
        if (this.activity != activity || (this.container != null && this.container.getParent() != container)) {
            jBridge.clearMerchantViews(this.activity);
            jBridge.clearMerchantViews(activity);
        }
        if (activity != this.activity) {
            removeFragment();
            addFragment(activity);
            this.activity = activity;
            jBridge.setActivity(activity);
            dynamicUI.setActivity(activity);
        }
        ExecutorManager.runOnMainThread(() -> {

            if (this.container == null || this.container.getParent() != container) {
                FrameLayout duiContainer = createSubLayout(activity);
                container.addView(duiContainer);
                if (this.container != null) {
                    ViewParent parent = this.container.getParent();
                    if (parent instanceof ViewGroup) {
                        ((ViewGroup) parent).removeView(this.container);
                    }
                }
                dynamicUI.setContainer(duiContainer);

                container.setOnApplyWindowInsetsListener((view, windowInsets) -> {
                    view.onApplyWindowInsets(windowInsets);
                    insetUpdated(windowInsets);

                    return windowInsets;
                });

                this.container = duiContainer;
                jBridge.setContainer(container);
                dynamicUI.setContainer(duiContainer);
            }
        });
    }

    public void reset() {
        ExecutorManager.runOnMainThread(() -> {
            jBridge.clearMerchantViews(this.activity);
            removeFragment();
            this.activity = null;
            jBridge.setActivity(null);
            dynamicUI.resetActivity();
            jBridge.reset();
            resetBridges();

            if (this.container != null) {
                ViewParent parent = this.container.getParent();
                if (parent instanceof ViewGroup) {
                    ((ViewGroup) parent).removeView(this.container);
                }
            }
            this.container = null;
            dynamicUI.setContainer(null);
        });
    }

    public void terminate() {
        MPINUtil.closeAllConnections(getContext());
        jBridge.reset();
        for (HyperBridge bridge : bridgeList.getBridgeList().values()) {
            bridge.terminate();
        }
        if (activityLaunchDelegate instanceof HyperActivityLaunchDelegate) {
            ((HyperActivityLaunchDelegate) activityLaunchDelegate).clearQueue();
        }
        if (intentSenderDelegate instanceof HyperIntentSenderDelegate) {
            ((HyperIntentSenderDelegate) intentSenderDelegate).clearQueue();
        }
        if (requestPermissionDelegate instanceof HyperRequestPermissionDelegate) {
            ((HyperRequestPermissionDelegate) requestPermissionDelegate).clearQueue();
        }
        dynamicUI.terminate();
        smsServices.unregisterSmsConsent();
    }

    private void addFragment(@NonNull FragmentActivity activity) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                fragment = new HyperFragment();
                FragmentTransaction transaction = activity.getSupportFragmentManager().beginTransaction().add(fragment, fragmentTag);
                commitFragmentTransaction(transaction);
                for (FragmentEvent event : fragmentEvents) {
                    fragment.registerForEvent(event, (payload, subscribedFragment) -> {
                        switch (event) {
                            case ON_PAUSE:
                                if (subscribedFragment == fragment) {
                                    paused = true;
                                    getDynamicUI().onPauseCallback();
                                }
                                break;
                            case ON_RESUME:
                                if (subscribedFragment == fragment) {
                                    paused = false;
                                    getDynamicUI().onResumeCallback();
                                }
                                break;
                            case ON_ATTACH:
                                if (subscribedFragment == fragment) {
                                    if (activityLaunchDelegate instanceof HyperActivityLaunchDelegate) {
                                        ((HyperActivityLaunchDelegate) activityLaunchDelegate).fragmentAttached();
                                    }
                                    if (requestPermissionDelegate instanceof HyperRequestPermissionDelegate) {
                                        ((HyperRequestPermissionDelegate) requestPermissionDelegate).fragmentAttached();
                                    }
                                    if (intentSenderDelegate instanceof HyperIntentSenderDelegate) {
                                        ((HyperIntentSenderDelegate) intentSenderDelegate).fragmentAttached();
                                    }
                                }
                                break;
                            case ON_DESTROY:
                                if (subscribedFragment == fragment) {
                                    fragment = null;
                                }
                                break;
                        }
                        jBridge.invokeFnInDUIWebview(event.getKey(), payload);
                    });
                }
                fragment.registerOnActivityResult(this::onActivityResult);
                fragment.registerOnRequestPermissionResult(this::onRequestPermissionsResult);
            } catch (Exception e) {
                sdkTracker.trackException(LogCategory.LIFECYCLE, LogSubCategory.LifeCycle.ANDROID, Labels.Android.FRAGMENT_OPERATION, "Exception while committing fragment", e);
            }
        });
    }

    public boolean isPaused() {
        return paused;
    }

    private void removeFragment() {
        ExecutorManager.runOnMainThread(() -> {
            if (activity != null && fragment != null && fragment.isAdded()) {
                try {
                    FragmentManager fragmentManager = activity.getSupportFragmentManager();
                    if (!fragmentManager.isDestroyed() && fragmentManager.findFragmentByTag(fragmentTag) != null) {
                        FragmentTransaction transaction = fragmentManager.beginTransaction().remove(fragment);
                        commitFragmentTransaction(transaction);
                    }
                } catch (Exception e) {
                    sdkTracker.trackException(LogCategory.LIFECYCLE, LogSubCategory.LifeCycle.ANDROID, Labels.Android.FRAGMENT_OPERATION, "Exception while removing fragment", e);
                }
            }
            fragment = null;
        });
    }

    private boolean useCommit() {
        if (merchantClientId == null) {
            return true;
        }

        JSONObject sdkConfig = sdkConfigService.getSdkConfig();

        JSONObject useCommitNowClientIds = Utils.optJSONObject(sdkConfig, "useCommitNowClientIds");
        String clientId = merchantClientId.toLowerCase(Locale.getDefault()).split("_")[0];
        return !useCommitNowClientIds.optString(clientId).equals("true");
    }

    private void commitFragmentTransaction(FragmentTransaction transaction) {
        if (useCommit()) {
            transaction.commitAllowingStateLoss();
        } else {
            transaction.commitNowAllowingStateLoss();
        }
    }

    public void sdkDebug(String tag, String description) {
        if (sdkInfo.isSdkDebuggable()) {
            JuspayLogger.d(tag, description);
        }
    }

    private void insetUpdated(WindowInsets insets) {

        if (activity == null) {
            return;
        }

        float density = activity.getResources().getDisplayMetrics().density;
        float windowTop = insets.getSystemWindowInsetTop() / density;
        float windowRight = insets.getSystemWindowInsetRight() / density;
        float windowBottom = insets.getSystemWindowInsetBottom() / density;
        float windowLeft = insets.getSystemWindowInsetLeft() / density;

        float stableTop = insets.getStableInsetTop() / density;
        float stableRight = insets.getStableInsetRight() / density;
        float stableBottom = insets.getStableInsetBottom() / density;
        float stableLeft = insets.getStableInsetLeft() / density;
        String cmd = "window.insetUpdated(" + windowTop + "," + windowRight + "," + windowBottom + "," + windowLeft + "," + stableTop + "," + stableRight + "," + stableBottom + "," + stableLeft + ",)";
        dynamicUI.addJsToWebView(cmd);
    }

    private void resetBridges() {
        for (HyperBridge bridge : bridgeList.getBridgeList().values()) {
            bridge.reset();
        }
    }

    @NonNull
    public SdkTracker getSdkTracker() {
        return sdkTracker;
    }

    @NonNull
    public Context getContext() {
        return context;
    }

    @Nullable
    public Activity getActivity() {
        return activity;
    }

    @NonNull
    public FileProviderService getFileProviderService() {
        return fileProviderService;
    }

    @NonNull
    public RemoteAssetService getRemoteAssetService() {
        return remoteAssetService;
    }

    @NonNull
    public SdkConfigService getSdkConfigService() {
        return sdkConfigService;
    }

    @SuppressWarnings("unused")
    public void onMerchantEvent(@NonNull JSONObject json) {
        onMerchantEvent("default", json);
    }

    public void onMerchantEvent(String key, @NonNull JSONObject json) {
        if (key.equals("process")) {
            setLastProcessPayload(json);
        }
        String command;
        command = String.format("window.onMerchantEvent('%s',%s);", key, dynamicUI.encodeUtfAndWrapDecode(json.toString(), LOG_TAG));

        jBridge.invokeCustomFnInDUIWebview(command);
    }

    public void onBackPressed() {
        sdkTracker.trackLifecycle(LogSubCategory.LifeCycle.ANDROID, LogLevel.INFO, Labels.Android.BACK_PRESSED, "class", "juspayServices");
        jBridge.requestKeyboardHide();
        jBridge.invokeFnInDUIWebview("onBackPressed", "{\"shouldShowBackPressDialog\":" + true + "}");
    }

    @Nullable
    public JSONObject getLastProcessPayload() {
        return lastProcessPayload;
    }

    private void setLastProcessPayload(@NonNull JSONObject json) {
        lastProcessPayload = json;
    }

    @Nullable
    public JuspayWebViewConfigurationCallback getWebViewConfigurationCallback() {
        return webViewConfigurationCallback;
    }

    public void setWebViewConfigurationCallback(@Nullable JuspayWebViewConfigurationCallback webViewConfigurationCallback) {
        this.webViewConfigurationCallback = webViewConfigurationCallback;
    }

    public boolean isWebViewAvailable() {
        return isWebViewAvailable;
    }

    public boolean isPrefetch() {
        return isPrefetch;
    }

    public void setPrefetch(boolean prefetch) {
        isPrefetch = prefetch;
    }

    @NonNull
    public PaymentSessionInfo getPaymentSessionInfo() {
        return paymentSessionInfo;
    }
}
