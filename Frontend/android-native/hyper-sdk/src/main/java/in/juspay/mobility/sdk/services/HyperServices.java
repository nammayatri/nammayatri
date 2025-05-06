package in.juspay.mobility.sdk.services;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.IntentSender;
import android.os.Bundle;
import android.view.View;
import android.view.ViewGroup;
import android.webkit.WebViewClient;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.Keep;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.FragmentActivity;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.lang.ref.WeakReference;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import java.util.Objects;
import java.util.Queue;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicReference;

import in.juspay.mobility.sdk.hyper.constants.Labels;
import in.juspay.mobility.sdk.hyper.constants.LogCategory;
import in.juspay.mobility.sdk.hyper.constants.LogLevel;
import in.juspay.mobility.sdk.hyper.constants.LogSubCategory;
import in.juspay.mobility.sdk.hyper.core.ExecutorManager;
import in.juspay.mobility.sdk.hyper.core.JuspayCoreLib;
import in.juspay.mobility.sdk.hyper.core.JuspayLogger;
import in.juspay.mobility.sdk.R;
import in.juspay.mobility.sdk.analytics.LogPusher;
import in.juspay.mobility.sdk.analytics.LogSessioniserExp;
import in.juspay.mobility.sdk.core.JuspayServices;
import in.juspay.mobility.sdk.core.JuspayWebViewConfigurationCallback;
import in.juspay.mobility.sdk.core.MerchantViewType;
import in.juspay.mobility.sdk.core.PrefetchServices;
import in.juspay.mobility.sdk.core.SdkTracker;
import in.juspay.mobility.sdk.data.JuspayResponseHandler;
import in.juspay.mobility.sdk.data.JuspayResponseHandlerDummyImpl;
import in.juspay.mobility.sdk.ui.ActivityLaunchDelegate;
import in.juspay.mobility.sdk.ui.HyperPaymentsCallback;
import in.juspay.mobility.sdk.ui.HyperPaymentsCallbackAdapter;
import in.juspay.mobility.sdk.ui.IntentSenderDelegate;
import in.juspay.mobility.sdk.ui.RequestPermissionDelegate;
import in.juspay.mobility.sdk.utils.IntegrationUtils;
import in.juspay.mobility.sdk.utils.LogType;
import in.juspay.mobility.sdk.utils.TrackerFallback;
import in.juspay.mobility.sdk.utils.network.NetUtils;

@Keep
public class HyperServices {
    private static final String LOG_TAG = "HyperServices";
    private static final String REQUEST_ID = "requestId";

    @NonNull
    private final Context context;

    @Nullable
    protected FragmentActivity activity;

    @NonNull
    private final OnBackPressedCallback onBackPressedCallback;

    @NonNull
    private final HashMap<WeakReference<FragmentActivity>, String> activityIds;

    @Nullable
    private String currentActivityId;

    @Nullable
    protected ViewGroup container;
    private HyperExceptionHandler hyperExceptionHandler;

    @Nullable
    protected HyperPaymentsCallback merchantCallback;

    @NonNull
    private final JuspayServices juspayServices;

    protected boolean jpConsumingBackPress;

    private final Queue<Runnable> processWaitingQueue = new LinkedList<>();

    private final AtomicReference<SDKState> sdkStateReference;

    @NonNull
    private final TrackerFallback trackerFallBack;

    private final Set<String> onBackPressedCallbackSet = Collections.newSetFromMap(new ConcurrentHashMap<>());


    /**
     * Instantiate {@code HyperService} with application context.
     * <br>
     * {@link FragmentActivity} reference should be provided later in
     * {@link #initiate(FragmentActivity, JSONObject, HyperPaymentsCallback)} /
     * {@link #initiate(FragmentActivity, ViewGroup, JSONObject, HyperPaymentsCallback)}
     * or {@link #process(FragmentActivity, JSONObject)} /
     * {@link #process(FragmentActivity, ViewGroup, JSONObject)} call
     *
     * @param context application
     */
    @Keep
    public HyperServices(@NonNull Context context) {
        this(context, null);
    }

    protected HyperServices(@NonNull Context context, @Nullable TenantParams tenantParams) {
        this.context = context;
        JuspayCoreLib.setApplicationContext(context.getApplicationContext());
        NetUtils.setApplicationHeaders(context);
        this.activityIds = new HashMap<>();
        this.jpConsumingBackPress = false;
        juspayServices = new JuspayServices(context, tenantParams);
        hyperExceptionHandler = new HyperExceptionHandler(this);
        sdkStateReference = new AtomicReference<>(SDKState.INSTANTIATED);
        this.trackerFallBack = new TrackerFallback(juspayServices.getSdkConfigService().getSdkConfig());
        onBackPressedCallback = new OnBackPressedCallback(false) {
            @Override
            public void handleOnBackPressed() {
                JSONObject value = new JSONObject();
                try {
                    value.put("triggered_on", "onBackPressedCallback.handleOnBackPressed()");
                } catch (Exception ignored) {
                }
                juspayServices.getSdkTracker().trackLifecycle(LogSubCategory.LifeCycle.ANDROID, LogLevel.INFO, Labels.Android.BACK_PRESSED, value);

                juspayServices.onBackPressed();
            }
        };
    }

    /**
     * Instantiate {@code HyperService} with {@link Activity}.
     *
     * @param activity current activity
     * @throws InstantiationException This constructor always throws an exception as using
     *                                {@link Activity} is not allowed. Pass {@link FragmentActivity}
     *                                instead using {@link #HyperServices(FragmentActivity)} constructor.
     * @deprecated This constructor is not allowed, and will always throw {@link InstantiationException}.
     * Use {@link #HyperServices(FragmentActivity)}
     */
    @Keep
    @Deprecated
    public HyperServices(Activity activity) throws InstantiationException {
        String name = "";
        if (activity != null) {
            name = String.format(" (%s)", activity.getClass().getName());
        }
        throw new InstantiationException("Instantiating HyperServices with plain Activity" + name + " is not allowed, please pass FragmentActivity");
    }

    /**
     * Instantiate {@code HyperServices} with {@link FragmentActivity}.
     * <br>
     * UI will be rendered in {@code android.R.id.content} ViewGroup, unless it is passed in
     * {@link #initiate(FragmentActivity, ViewGroup, JSONObject, HyperPaymentsCallback)}
     * or {@link #process(FragmentActivity, ViewGroup, JSONObject)} call
     *
     * @param activity current activity
     */
    @Keep
    public HyperServices(@NonNull FragmentActivity activity) {
        this(activity, (ViewGroup) activity.getWindow().getDecorView().findViewById(android.R.id.content));
        juspayServices.getSdkTracker().trackLifecycle(LogSubCategory.LifeCycle.HYPER_SDK, LogLevel.INFO, Labels.HyperSdk.HYPER_SERVICE, "view_group", false);
    }

    /**
     * Instantiate {@code HyperServices} with {@link FragmentActivity} and {@link ViewGroup}.
     *
     * @param activity  current activity
     * @param container the {@link ViewGroup} in which SDK's UI is supposed to be rendered
     */
    @Keep
    public HyperServices(@NonNull FragmentActivity activity, @NonNull ViewGroup container) {
        this(activity.getApplicationContext());

        this.activity = activity;
        this.container = container;
        this.currentActivityId = getIdForActivity(activity);
        juspayServices.getSdkTracker().trackLifecycle(LogSubCategory.LifeCycle.HYPER_SDK, LogLevel.INFO, Labels.HyperSdk.HYPER_SERVICE, "sdk_create", "success");
    }

    private String getIdForActivity(FragmentActivity fragmentActivity) {
        for (Map.Entry<WeakReference<FragmentActivity>, String> entry : activityIds.entrySet()) {
            FragmentActivity activity = entry.getKey().get();
            if (activity == fragmentActivity) {
                return entry.getValue();
            }
        }

        String id = UUID.randomUUID().toString();
        activityIds.put(new WeakReference<>(fragmentActivity), id);
        return id;
    }

    /**
     * Downloads latest assets and js code containing business logic, keeps the SDK up-to-date.
     *
     * @param context application context
     * @param payload contains {@link JSONObject} in format:
     *                <br>
     *                {@code { service: "in.juspay.hyperpay", payload: { clientId: "clientId" } } }
     */
    @Keep
    public static void preFetch(@NonNull final Context context, @NonNull final JSONObject payload) {
        PrefetchServices.preFetch(context, payload);
    }

    /**
     * This API is used to handle hardware back press.
     * <br>
     * This should be called in {@link Activity#onBackPressed()}.
     *
     * @return {@code true} if SDK is handling the back press,<br> {@code false} otherwise
     */
    @Keep
    public boolean onBackPressed() {
        JSONObject value = new JSONObject();
        try {
            value.put("consuming_backpress", jpConsumingBackPress);
            value.put("triggered_on", "HyperServices.onBackPressed()");
        } catch (Exception ignored) {
        }
        juspayServices.getSdkTracker().trackLifecycle(LogSubCategory.LifeCycle.ANDROID, LogLevel.INFO, Labels.Android.BACK_PRESSED, value);

        if (jpConsumingBackPress) {
            juspayServices.onBackPressed();
            return true;
        }
        return false;
    }

    /**
     * Initiate API starts up the JS engine and
     * enables it to improve the performance and experience of the next SDK API calls.
     *
     * @param activity  Current activity (should extend {@link FragmentActivity})
     * @param container the {@link ViewGroup} in which SDK's UI is supposed to be rendered
     * @param params    initiate payload
     * @param callback  interface to handle events sent from SDK would be received
     */
    @Keep
    public void initiate(@NonNull FragmentActivity activity, @NonNull ViewGroup container, @NonNull final JSONObject params, final HyperPaymentsCallback callback) {
        this.container = container;
        initiate(activity, params, callback);
    }

    /**
     * Initiate API starts up the JS engine and
     * enables it to improve the performance and experience of the next SDK API calls.
     *
     * @param activity Current activity (should extend {@link FragmentActivity})
     * @param params   initiate payload
     * @param callback interface to handle events sent from SDK would be received
     */
    @Keep
    public void initiate(@NonNull FragmentActivity activity, @NonNull final JSONObject params, final HyperPaymentsCallback callback) {
        if (this.activity != activity) {
            juspayServices.getSdkTracker().trackLifecycle(
                    LogSubCategory.LifeCycle.HYPER_SDK,
                    LogLevel.INFO,
                    Labels.HyperSdk.INITIATE,
                    "activity_changed",
                    "true");
        }
        this.activity = activity;
        this.currentActivityId = getIdForActivity(activity);
        initiate(params, callback);
    }

    protected boolean handleOnEvent(JSONObject event) {
        try {
            JSONObject payload = event.optJSONObject("payload");

            switch (event.optString("event")) {
                case "jp_consuming_backpress":
                    if (payload == null) {
                        jpConsumingBackPress = true;
                    } else {
                        jpConsumingBackPress = payload.getBoolean("jp_consuming_backpress");
                    }
                    juspayServices.getSdkTracker().trackLifecycle(LogSubCategory.LifeCycle.HYPER_SDK, LogLevel.INFO, Labels.HyperSdk.JP_CONSUMING_BACK_PRESS, "jp_consuming_backpress", jpConsumingBackPress);
                    onBackPressedCallback.setEnabled(jpConsumingBackPress);
                    return false;
                case "onJOSReady":
                    return false;
            }

            try {
                switch (event.optString("event", "default")) {
                    case "process_result":
                        logSafeEvents(LogLevel.INFO, Labels.HyperSdk.PROCESS, "ended", event);
                        trackerFallBack.log(event, juspayServices, LogType.PROCESS_END);
                        break;
                    case "initiate_result":
                        trackerFallBack.log(event, juspayServices, LogType.INITIATE_RESULT);
                        logSafeEvents(LogLevel.INFO, Labels.HyperSdk.INITIATE, "ended", event);
                        break;
                    default:
                        logSafeEvents(LogLevel.INFO, Labels.HyperSdk.EVENT, "payload", event);
                }
            } catch (Exception e) {
                // Ignored
            }

            String action = event.optString("action", "");

            if (action.equals("DUI_READY")) { // This has to passed from the bundle. to make sure that bundle has been attached.
                sdkStateReference.set(SDKState.INITIATE_COMPLETED);
                runProcessWaitQueue();
                return false;
            }
        } catch (Exception e) {
            logSafeExceptions(LogSubCategory.LifeCycle.ANDROID, Labels.Android.ON_EVENT, "on_event_failed_during_evaluation", e);
        }

        return true;
    }

    protected void modifyParams(JSONObject params) {
        try {
            params.put("service_based", true);
            params.put("use_local_assets", params.optBoolean("useLocalAssets", context.getResources().getBoolean(R.bool.use_local_assets)));
            params.getJSONObject("payload").put("currentActivityId", currentActivityId);
        } catch (Exception e) {
            JuspayLogger.e(LOG_TAG, "Failed to write to JSON", e);
        }
    }

    private void setupJuspayServices(@NonNull JSONObject params, @NonNull HyperPaymentsCallback callback) {
        merchantCallback = callback;
        modifyParams(params);
        juspayServices.setBundleParameter(params);
        juspayServices.setHyperCallback(new HyperPaymentsCallbackAdapter() {
            @Override
            public void onStartWaitingDialogCreated(@Nullable View parent) {
                merchantCallback.onStartWaitingDialogCreated(parent);
            }

            @Override
            public void onEvent(JSONObject event, JuspayResponseHandler handler) {
                boolean invokeMerchant = handleOnEvent(event);
                if (invokeMerchant) {
                    merchantCallback.onEvent(event, handler);
                }
            }

            @Override
            @Nullable
            public View getMerchantView(ViewGroup parent, MerchantViewType viewType) {
                return merchantCallback.getMerchantView(parent, viewType);
            }

            @Nullable
            @Override
            public WebViewClient createJuspaySafeWebViewClient() {
                return merchantCallback.createJuspaySafeWebViewClient();
            }
        });

        juspayServices.initiate(() -> {
            sdkStateReference.set(SDKState.INITIATE_COMPLETED);
            notifyMerchant("JP_020", "No web view is present in the device", "initiate_result", params);
        });
    }

    protected boolean checkAndStartInitiate(JSONObject payload) {
        SDKState sdkState = sdkStateReference.get();
        if (sdkState == SDKState.INITIATE_STARTED || sdkState == SDKState.INITIATE_COMPLETED) {
            notifyMerchant("JP_017", "initiate() can only be called once without terminate()", "initiate_result", payload);
            logSafeEvents(LogLevel.ERROR, Labels.HyperSdk.INITIATE, "interrupted", "initiate() can only be called once without terminate()");
            return false;
        }

        sdkStateReference.set(SDKState.INITIATE_STARTED);

        juspayServices.getSdkTracker().trackLifecycle(
                LogSubCategory.LifeCycle.HYPER_SDK,
                LogLevel.INFO,
                Labels.HyperSdk.INITIATE,
                "started",
                "Started initiating the SDK");
        return true;
    }

    /**
     * Initiate API starts up the JS engine and
     * enables it to improve the performance and experience of the next SDK API calls.
     *
     * @param params   initiate payload
     * @param callback interface to handle events sent from SDK would be received
     */
    @Keep
    public void initiate(@NonNull final JSONObject params, @NonNull final HyperPaymentsCallback callback) {
        try {
            JSONObject innerPayload = params.getJSONObject("payload");
            innerPayload.put("initiateStartedTime", System.currentTimeMillis());
            params.put("payload", innerPayload);
        } catch (JSONException ignored) {
        }

        if (checkAndStartInitiate(params)) {
            trackerFallBack.log(params, juspayServices, LogType.INITIATE_START);
            juspayServices.getSdkTracker().resetSerialNumber();
            juspayServices.getSessionInfo().setSessionId();
            if (Objects.equals(juspayServices.getWorkingLogger(), "json-array") || Objects.equals(juspayServices.getWorkingLogger(), "both")) {
                LogPusher.startLogPusherTimer();
            }
            ExecutorManager.runOnLogSessioniserThread(() -> {
                if (juspayServices.getLogSessioniserExp() != null) {
                    juspayServices.getLogSessioniserExp().startLogSessioniser();
                    shouldPushLogs("initiate", params);
                }
            });
            juspayServices.getSdkTracker().trackLifecycle(LogSubCategory.LifeCycle.HYPER_SDK, LogLevel.INFO, Labels.HyperSdk.INITIATE, "started", params);
            juspayServices.getSdkTracker().trackLifecycle(LogSubCategory.LifeCycle.HYPER_SDK, LogLevel.INFO, Labels.HyperSdk.INITIATE, "fragment_activity_used", String.valueOf(activity != null));
            ExecutorManager.runOnBackgroundThread(() -> {
                if (hyperExceptionHandler == null) {
                    hyperExceptionHandler = new HyperExceptionHandler(HyperServices.this);
                }
                hyperExceptionHandler.setAsDefaultExceptionHandler();
            });
            long waitStartTime = System.currentTimeMillis();
            ExecutorManager.runOnMainThread(() -> {
                long waitEndTime = System.currentTimeMillis();
                juspayServices.getSdkTracker().trackLifecycle(LogSubCategory.LifeCycle.HYPER_SDK, LogLevel.DEBUG, Labels.HyperSdk.INITIATE, "main_thread_handover", waitEndTime - waitStartTime);
                setupJuspayServices(params, callback);
            });
        }
    }

    private void notifyMerchant(String errorCode, String errorMessage, String event, JSONObject reqPayload) {
        if (merchantCallback != null) {
            JSONObject payload = notifyMerchant(merchantCallback, errorCode, errorMessage, event, reqPayload);
            String label = event.equals("initiate_result") ? Labels.HyperSdk.INITIATE : Labels.HyperSdk.PROCESS;
            logSafeEvents(LogLevel.ERROR, label, "ended", payload);
        }
    }

    private JSONObject notifyMerchant(@NonNull final HyperPaymentsCallback merchantCallback,
                                      @NonNull String errorCode, @NonNull String errorMessage,
                                      @NonNull String event, @NonNull JSONObject reqPayload) {
        JSONObject payload = new JSONObject();

        try {
            payload.put(REQUEST_ID, reqPayload.optString(REQUEST_ID, ""));
            payload.put("service", reqPayload.optString("service", "service"));
            payload.put("error", true);
            payload.put("errorCode", errorCode);
            payload.put("errorMessage", errorMessage);
            payload.put("event", event);
            payload.put("payload", new JSONObject());
            merchantCallback.onEvent(payload, new JuspayResponseHandlerDummyImpl());
        } catch (Exception e) {
            SdkTracker.trackAndLogBootException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.HyperSdk.EXIT_SDK_ERROR, "Error while sending response to merchant", e);
        }
        return payload;
    }

    private void runProcessWaitQueue() {
        ExecutorManager.runOnBackgroundThread(() -> {
            logSafeEvents(LogLevel.INFO, Labels.HyperSdk.PROCESS_WAIT_QUEUE, "pending_processes", processWaitingQueue.size());
            while (!processWaitingQueue.isEmpty()) {
                Runnable task = processWaitingQueue.poll();
                if (task != null) {
                    task.run();
                }
            }
        });
    }


    // STRUCTURE OF PAYLOAD (payload {String [] requestIdArray, String service} )
    public void terminate(JSONObject payload) {
        logSafeEvents(LogLevel.INFO, Labels.HyperSdk.TERMINATE_PROCESS, "request", payload);
        juspayServices.onMerchantEvent("terminate", payload);
    }

    /**
     * Utility method to be used across this class. Will prefer the regular logger if available, else
     * uses the static boot logger.
     *
     * @param level The log level for this event.
     * @param label The label of the LifeCycle event. This is the short descriptor, and should always be in snake case.
     * @param key   The label identifier for this event.
     * @param value The message describing the event.
     */
    private void logSafeEvents(String level, String label, String key, Object value) {
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();
        sdkTracker.trackLifecycle(LogSubCategory.LifeCycle.HYPER_SDK, level, label, key, value);
    }

    /**
     * Utility method to be used across this class. Will prefer the regular logger if available, else
     * uses the static boot logger.
     *
     * @param subCategory The sub-category of the exception. SubCategory is defined according to the category.
     * @param label       The label of the exception. This is the short descriptor, and should always be in snake case.
     * @param description a message for the exception
     * @param throwable   stacktrace of the exception
     */
    @SuppressWarnings("SameParameterValue")
    private void logSafeExceptions(String subCategory, String label, String description, Throwable throwable) {
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();
        sdkTracker.trackAndLogException(LOG_TAG, LogCategory.LIFECYCLE, subCategory, label, description, throwable);

    }

    /**
     * This API should be triggered for all operations required from the HyperSDK.
     * This API can be triggered any number of times based on requirements or app flow structure.
     *
     * @param activity Current activity (should extend {@link FragmentActivity})
     * @param payload  process payload
     */
    @Keep
    public void process(@NonNull FragmentActivity activity, @NonNull JSONObject payload) {
        process(activity, activity.getWindow().getDecorView().findViewById(android.R.id.content), payload);
    }

    /**
     * This API should be triggered for all operations required from the HyperSDK.
     * This API can be triggered any number of times based on requirements or app flow structure.
     *
     * @param activity  Current activity (should extend {@link FragmentActivity})
     * @param viewGroup the {@link ViewGroup} in which SDK's UI is supposed to be rendered
     * @param payload   process payload
     */
    @Keep
    public void process(@NonNull final FragmentActivity activity, @NonNull final ViewGroup viewGroup, @NonNull final JSONObject payload) {
        SDKState sdkState = sdkStateReference.get();

        switch (sdkState) {
            case INSTANTIATED:
                logSafeEvents(LogLevel.ERROR, Labels.HyperSdk.PROCESS, "called_before_initiate", payload);
                initiateNotCalled();
                break;

            case INITIATE_STARTED:
                trackerFallBack.log(payload, juspayServices, LogType.PROCESS_QUEUED);
                logSafeEvents(LogLevel.INFO, Labels.HyperSdk.PROCESS, "queued", payload);
                processWaitingQueue.add(() -> process(activity, viewGroup, payload));
                break;

            case INITIATE_COMPLETED:
                logSafeEvents(LogLevel.INFO, Labels.HyperSdk.PROCESS, "called_and_started", payload);
                // WebView creation failed in initiate
                if (!juspayServices.isWebViewAvailable()) {
                    notifyMerchant("JP_020", "No web view is present in the device", "process_result", payload);
                    return;
                }

                if (activity != HyperServices.this.activity) {
                    logSafeEvents(LogLevel.INFO, Labels.HyperSdk.PROCESS, "activity_changed", "true");
                }

                String activityId = getIdForActivity(activity);
                if (onBackPressedCallbackSet.add(activityId)) {
                    ExecutorManager.runOnMainThread(() -> {
                        try {
                            activity.getOnBackPressedDispatcher().addCallback(onBackPressedCallback);
                        } catch (Exception e) {
                            logSafeExceptions(LogSubCategory.LifeCycle.HYPER_SDK, Labels.HyperSdk.ON_BACKPRESSED_CALLBACK, "Exception while adding onBackPressedCallback", e);
                        }
                    });
                }

                container = viewGroup;
                HyperServices.this.activity = activity;
                currentActivityId = getIdForActivity(activity);
                juspayServices.getSessionInfo().addOrderIdInSessionData(payload);
                juspayServices.process(activity, container);
                if (juspayServices.getLogSessioniserExp() != null) {
                    shouldPushLogs("process", payload);
                }
                doProcess(payload);
                break;

            case TERMINATED:
                logSafeEvents(LogLevel.ERROR, Labels.HyperSdk.PROCESS, "called_after_terminate", payload);
                initiateTerminated(payload);
                break;
        }
    }

    /**
     * This API should be triggered for all operations required from the HyperSDK.
     * This API can be triggered any number of times based on requirements or app flow structure.
     *
     * @param payload process payload
     */
    @Keep
    public void process(@NonNull final JSONObject payload) {
        // activity is not an instance of FragmentActivity
        SDKState sdkState = sdkStateReference.get();

        if (sdkState == SDKState.INSTANTIATED) {
            initiateNotCalled();
            return;
        }

        if (sdkState == SDKState.TERMINATED) {
            initiateTerminated(payload);
            return;
        }

        if (activity == null) {
            notifyMerchant("JP_003", "FragmentActivity needs to be send in process", "process_result", payload);
            return;
        }

        if (container != null) {
            process(activity, container, payload);
        } else {
            process(activity, payload);
        }
    }

    private void initiateNotCalled() {
        throw new IllegalStateException("initiate() must be called before calling process()");
    }

    private void initiateTerminated(JSONObject payload) {
        notifyMerchant("JP_017", "process() called after terminate()", "process_result", payload);
        logSafeEvents(LogLevel.ERROR, Labels.HyperSdk.PROCESS, "interrupted", "process() called after terminate()");
    }


    /**
     * Checks whether initiate was called or not
     *
     * @return {@code true} if initiate was called before terminate being called,<br> {@code false} otherwise
     */
    @Keep
    public boolean isInitialised() {
        final SDKState sdkState = sdkStateReference.get();
        final boolean isInitiateCalled = sdkState == SDKState.INITIATE_STARTED || sdkState == SDKState.INITIATE_COMPLETED;
        JSONObject logValue = new JSONObject();
        try {
            logValue.put("sdkState", String.valueOf(sdkState));
            logValue.put("isInitialised", isInitiateCalled);
        } catch (JSONException ignored) {
        }
        juspayServices.getSdkTracker().trackLifecycle(
                LogSubCategory.LifeCycle.HYPER_SDK,
                LogLevel.INFO,
                Labels.HyperSdk.INITIATE,
                "isInitialised()",
                logValue
        );
        return isInitiateCalled;
    }

    /**
     * Clears the references to the activity and other related objects
     *
     * @param activity the activity reference which needs to be reset
     */
    @Keep
    public void resetActivity(FragmentActivity activity) {
        if (activity != this.activity) {
            return;
        }
        juspayServices.getSdkTracker().trackLifecycle(
                LogSubCategory.LifeCycle.HYPER_SDK,
                LogLevel.INFO,
                Labels.HyperSdk.TERMINATE,
                "resetActivity()",
                "called");

        juspayServices.reset();
        ExecutorManager.runOnMainThread(onBackPressedCallback::remove);
        onBackPressedCallbackSet.clear();

        this.activity = null;
        currentActivityId = null;
        container = null;
    }

    private void doProcess(@NonNull JSONObject payload) {
        try {
            //this is the container id passed inside the payload.. which can be used by any of service for blur effects
            logSafeEvents(LogLevel.INFO, Labels.HyperSdk.PROCESS, "started", payload);
            trackerFallBack.log(payload, juspayServices, LogType.PROCESS_START);
            JSONObject innerPayload = payload.getJSONObject("payload");
            innerPayload.put("merchant_root_view", container != null ? String.valueOf(container.getId()) : -1);
            innerPayload.put("merchant_keyboard_mode", activity != null ? activity.getWindow().getAttributes().softInputMode : -1);
            innerPayload.put("processStartedTime", System.currentTimeMillis());
            innerPayload.put("currentActivityId", currentActivityId);
            payload.put("payload", innerPayload);
            juspayServices.setUpMerchantFragments(innerPayload);
        } catch (JSONException ignored) {
        }

        if (!payload.has("requestId")) {
            logSafeEvents(LogLevel.ERROR, Labels.HyperSdk.PROCESS, "request_id_present", false);
            return;
        }

        long waitStartTime = System.currentTimeMillis();
        ExecutorManager.runOnMainThread(() -> {
            long waitEndTime = System.currentTimeMillis();
            juspayServices.getSdkTracker().trackLifecycle(LogSubCategory.LifeCycle.HYPER_SDK, LogLevel.DEBUG, Labels.HyperSdk.PROCESS, "main_thread_handover", waitEndTime - waitStartTime);
            juspayServices.onMerchantEvent("process", payload);
        });
    }

    /**
     * This API frees up any system used resources and closes active connections and helps clean-up.
     */
    @Keep
    public void terminate() {
        final SDKState sdkState = sdkStateReference.get();
        if (sdkState == SDKState.TERMINATED) {
            juspayServices.getSdkTracker().trackLifecycle(LogSubCategory.LifeCycle.HYPER_SDK, LogLevel.WARNING, Labels.HyperSdk.TERMINATE, "started", "Terminate called again, skipping");
            return;
        }
        if (sdkState == SDKState.INSTANTIATED) {
            juspayServices.getSdkTracker().trackLifecycle(LogSubCategory.LifeCycle.HYPER_SDK, LogLevel.WARNING, Labels.HyperSdk.TERMINATE, "started", "Terminate called without initiate, skipping");
            return;
        }

        sdkStateReference.set(SDKState.TERMINATED);
        juspayServices.getSdkTracker().trackLifecycle(LogSubCategory.LifeCycle.HYPER_SDK, LogLevel.INFO, Labels.HyperSdk.TERMINATE, "started", "Terminating the SDK");

        jpConsumingBackPress = false;

        // Removing exception handler objects
        if (hyperExceptionHandler != null) {
            hyperExceptionHandler.clearHyperExceptionHandler();
            hyperExceptionHandler = null;
        }

        resetActivity(this.activity);
        long waitStartTime = System.currentTimeMillis();
        ExecutorManager.runOnMainThread(() -> {
            long waitEndTime = System.currentTimeMillis();
            juspayServices.getSdkTracker().trackLifecycle(LogSubCategory.LifeCycle.HYPER_SDK, LogLevel.DEBUG, Labels.HyperSdk.TERMINATE, "main_thread_handover", waitEndTime - waitStartTime);
            try {
                juspayServices.terminate();
            } catch (Exception e) {
                juspayServices.getSdkTracker().trackAndLogException(LOG_TAG, LogCategory.LIFECYCLE, LogSubCategory.LifeCycle.HYPER_SDK, Labels.HyperSdk.TERMINATE, "Failed to remove the fragment", e);
            }
            container = null;
            activity = null;
        });
        juspayServices.getSessionInfo().resetSession();
        if (Objects.equals(juspayServices.getWorkingLogger(), "json-array") || Objects.equals(juspayServices.getWorkingLogger(), "both")) {
            LogPusher.stopLogPusherOnTerminate();
        }
        if (juspayServices.getLogSessioniserExp() != null) {
            juspayServices.getLogSessioniserExp().stopLogSessioniserOnTerminate();
        }
    }

    /**
     * This function is to be called only by merchants using reactActivity
     * or a similar case where is it is not possible to forward the callback to super.onActivityResult
     *
     * @param requestCode The integer request code originally supplied to
     *                    startActivityForResult(), allowing you to identify who this
     *                    result came from.
     * @param resultCode  The integer result code returned by the child activity
     *                    through its setResult().
     * @param intent      An Intent, which can return result data to the caller
     *                    (various data can be attached to Intent "extras").
     */
    @Keep
    public void onActivityResult(int requestCode, int resultCode, Intent intent) {
        // Bitwise AND with 0xffff as the requestCode logic for activity and fragment differs
        juspayServices.onActivityResult(requestCode & 0xffff, resultCode, intent);
    }

    /**
     * This function is to be called only by merchants using reactActivity
     * or a similar case where is it is not possible to forward the callback to super.onRequestPermissionsResult
     *
     * @param requestCode  The request code passed in requestPermissions().
     * @param permissions  The requested permissions. Never null.
     * @param grantResults The grant results for the corresponding permissions
     *                     which is either {@link android.content.pm.PackageManager#PERMISSION_GRANTED}
     *                     or {@link android.content.pm.PackageManager#PERMISSION_DENIED}. Never null.
     */
    @Keep
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        // Bitwise AND with 0xffff as the requestCode logic for activity and fragment differs
        juspayServices.onRequestPermissionsResult(requestCode & 0xffff, permissions, grantResults);
    }

    private void shouldPushLogs(String event, JSONObject payload) {
        LogSessioniserExp logSessioniserExp = juspayServices.getLogSessioniserExp();
        if (logSessioniserExp != null) {
            try {
                JSONObject logsConfig = juspayServices.getSdkConfigService().getSdkConfig().optJSONObject("logsConfig");
                if (logsConfig != null) {
                    String bufferLogsTill = logsConfig.optString("bufferLogsTill", event);
                    if (event.equals(bufferLogsTill) && shouldStopBuffer(logsConfig, payload)) {
                        logSessioniserExp.startPushing();
                    }
                } else {
                    logSessioniserExp.startPushing();
                }
            } catch (Exception exception) {
                logSessioniserExp.startPushing();
            }
        }
    }

    private boolean shouldStopBuffer(JSONObject logsConfig, JSONObject payload) {
        if (logsConfig.has("dontStopBufferOn")) {
            try {
                JSONArray dontStopBufferOn = logsConfig.getJSONArray("dontStopBufferOn");
                for (int i = 0; i < dontStopBufferOn.length(); i++) {
                    if (objectMatch(payload, dontStopBufferOn.get(i))) {
                        return false;
                    }
                }
            } catch (Exception ignored) {
            }
        }
        return true;
    }

    private boolean objectMatch(Object payload, Object obj) {

        if (Objects.equals(payload, null) || Objects.equals(obj, null)) {
            return false;
        }

        if (!payload.getClass().equals(obj.getClass())) {
            return false;
        }

        if (obj instanceof JSONObject) {
            JSONObject obj1 = (JSONObject) obj;
            if (obj1.length() == 0) {
                return false;
            }
            JSONObject payload1 = (JSONObject) payload;
            for (Iterator<String> it = obj1.keys(); it.hasNext(); ) {
                String key = it.next();
                if (!objectMatch(payload1.opt(key), obj1.opt(key))) {
                    return false;
                }
            }
            return true;
        } else if (obj instanceof String) {
            return obj.equals(payload);
        } else {
            return payload == obj;
        }
    }

    /**
     * Fetching a JSON of various versions used in the SDK.
     * This can be used to add custom keys in Crashlytics.
     * <p>
     * Usage:
     * {@code HyperServices.getVersions(context).getString("sdkVersion")}
     *
     * @param context application context
     * @return JSONObject containing versions.
     * @see <a href="https://firebase.google.com/docs/crashlytics/customize-crash-reports?platform=android#add-keys"> Customize your Firebase Crashlytics crash reports</a>
     */
    @Keep
    public static JSONObject getVersions(@NonNull final Context context) {
        JSONObject version = new JSONObject();
        try {
            version.put("sdkVersion", IntegrationUtils.getSdkVersion(context));
        } catch (JSONException ignore) {
        }
        return version;
    }

    private void uncaughtException(Throwable e) {
        ExecutorManager.runOnSdkTrackerPool(() -> {
            final SdkTracker sdkTracker = juspayServices.getSdkTracker();
            JSONObject logToTrack = sdkTracker.getExceptionLog(LogCategory.LIFECYCLE, LogSubCategory.LifeCycle.HYPER_SDK, Labels.System.SDK_CRASHED, "SDK Crashed Uncaught exception handler", e);
            sdkTracker.addLogToPersistedQueue(logToTrack);
        });
    }

    /**
     * This API is used to set a delegate to provide a custom implementation for
     * {@code startActivityForResult}.
     * <br>
     * If not set, {@link android.app.Fragment#startActivityForResult(Intent, int, Bundle)}
     * will be used.
     *
     * @param activityLaunchDelegate Interface which provides {@link ActivityLaunchDelegate#startActivityForResult(Intent, int, Bundle) startActivityForResult}
     *                               override.
     */
    @Keep
    public void setActivityLaunchDelegate(@NonNull ActivityLaunchDelegate activityLaunchDelegate) {
        juspayServices.setActivityLaunchDelegate(activityLaunchDelegate);
    }

    /**
     * This API is used to set a delegate to provide a custom implementation for
     * {@code startIntentSenderForResult}.
     * <br>
     * If not set, {@link android.app.Fragment#startIntentSenderForResult(IntentSender, int, Intent, int, int, int, Bundle)}
     * will be used.
     *
     * @param intentSenderDelegate Interface which provides {@link IntentSenderDelegate#startIntentSenderForResult(IntentSender, int, Intent, int, int, int, Bundle)}  startIntentSenderForResult}
     *                             override.
     */
    @Keep
    public void setIntentSenderDelegate(@NonNull IntentSenderDelegate intentSenderDelegate) {
        juspayServices.setIntentSenderDelegate(intentSenderDelegate);
    }

    /**
     * This API is used to set a delegate to provide a custom implementation for
     * {@code requestPermissions}.
     * <br>
     * If not set, {@link android.app.Fragment#requestPermissions(String[], int)} will be used.
     *
     * @param requestPermissionDelegate Interface which provides {@link RequestPermissionDelegate#requestPermission(String[], int) requestPermission}
     *                                  override.
     */
    @Keep
    public void setRequestPermissionDelegate(@NonNull RequestPermissionDelegate requestPermissionDelegate) {
        juspayServices.setRequestPermissionDelegate(requestPermissionDelegate);
    }

    /**
     * This API is used to set a callback to access Bank WebView.
     * <br>
     * <b>Note:</b> The callback would be triggered only for merchants whitelisted by Juspay.
     *
     * @param webViewConfigurationCallback An interface which provides a callback with the WebView.
     *                                     See {@link JuspayWebViewConfigurationCallback} for more information.
     */
    @Keep
    public void setWebViewConfigurationCallback(@NonNull JuspayWebViewConfigurationCallback webViewConfigurationCallback) {
        juspayServices.setWebViewConfigurationCallback(webViewConfigurationCallback);
    }

    private static class HyperExceptionHandler implements Thread.UncaughtExceptionHandler {
        private WeakReference<HyperServices> hyperServices;
        private Thread.UncaughtExceptionHandler merchantHandler;
        private static final String LOG_TAG = "UncaughtExceptionHandler";

        HyperExceptionHandler(HyperServices hyperServices) {
            this.hyperServices = new WeakReference<>(hyperServices);
            SdkTracker.trackBootLifecycle(
                    LogSubCategory.LifeCycle.HYPER_SDK,
                    LogLevel.INFO,
                    Labels.HyperSdk.EXCEPTION_HANDLER,
                    "ExceptionHandler",
                    "created HyperExceptionHandler"
            );
        }

        @Override
        public void uncaughtException(@NonNull Thread t, @NonNull Throwable e) {
            HyperServices services = hyperServices.get();

            // TODO :: Add a filter and send only Hyper SDK crashes to our server
            if (services != null) {
                JuspayLogger.w(LOG_TAG, "sending crash to tracker");
                services.uncaughtException(e);
            }

            if (merchantHandler != null) {
                JuspayLogger.w(LOG_TAG, "forwarding crash to merchant");
                merchantHandler.uncaughtException(t, e);
            } else {
                JuspayLogger.e(LOG_TAG, "merchant exception handler not found, exiting");
                System.exit(1);
            }
        }

        public void setAsDefaultExceptionHandler() {
            merchantHandler = Thread.getDefaultUncaughtExceptionHandler();
            Thread.setDefaultUncaughtExceptionHandler(this);
            SdkTracker.trackBootLifecycle(
                    LogSubCategory.LifeCycle.HYPER_SDK,
                    LogLevel.INFO,
                    Labels.HyperSdk.EXCEPTION_HANDLER,
                    "ExceptionHandler",
                    "registered HyperExceptionHandler as default uncaught exception handler"
            );
        }

        public void clearHyperExceptionHandler() {
            if (merchantHandler == null || !(merchantHandler instanceof HyperExceptionHandler)) {
                Thread.setDefaultUncaughtExceptionHandler(merchantHandler);
            }

            hyperServices = new WeakReference<>(null);
            merchantHandler = null;
            SdkTracker.trackBootLifecycle(
                    LogSubCategory.LifeCycle.HYPER_SDK,
                    LogLevel.INFO,
                    Labels.HyperSdk.EXCEPTION_HANDLER,
                    "ExceptionHandler",
                    "destroyed HyperExceptionHandler and registered merchant's exception handler as default"
            );
        }
    }
}
