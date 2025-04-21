package in.juspay.hypersdk.core;

import android.app.ActivityManager;
import android.content.Context;
import android.util.Log;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Locale;
import java.util.Objects;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicInteger;

import in.juspay.hyper.constants.Labels;
import in.juspay.hyper.constants.LogCategory;
import in.juspay.hyper.constants.LogLevel;
import in.juspay.hyper.constants.LogSubCategory;
import in.juspay.hyper.core.ExecutorManager;
import in.juspay.hyper.core.JuspayLogger;
import in.juspay.hyper.core.TrackerInterface;
import in.juspay.hypersdk.analytics.LogConstants;
import in.juspay.hypersdk.analytics.LogPusher;
import in.juspay.hypersdk.analytics.LogPusherExp;
import in.juspay.hypersdk.analytics.LogSessioniser;
import in.juspay.hypersdk.data.SessionInfo;
import in.juspay.hypersdk.utils.IntegrationUtils;
import in.juspay.hypersdk.utils.Utils;
import in.juspay.hypersmshandler.Tracker;

/**
 * A tracker (logger) for the SDK events. Always use this class instead of the built-in Log class in Android. This
 * automatically disables logging the messages to the Logcat if running under production.
 *
 * @author Veera Manohara Subbiah [veera.subbiah@juspay.in]
 * @author Sri Harsha Chilakapati [sri.harsha@juspay.in]
 * @since 02/06/17
 */

// All public methods will run on Background Thread, enforcing private methods automatically to be in LogsThread.

public final class SdkTracker implements TrackerInterface, Tracker {
    private static final String LOG_TAG = "SdkTracker";

    // Size is 22kb to support double of worst case crash log
    private static final int MAX_LOG_SIZE = 22528;

    private static final Queue<JSONObject> bootLogs = new ConcurrentLinkedQueue<>();
    private AtomicInteger serialNumberCounter = new AtomicInteger(1);
    @NonNull
    private String hyperSdkVersion = "";
    @NonNull
    private String godelVersion = "";
    @NonNull
    private String godelBuildVersion = "";
    @NonNull
    private final JuspayServices juspayServices;
    private final JSONObject logProperties = new JSONObject();
    private final HashSet<String> labelsToDrop = new HashSet<>();

    /**
     * Constructor. Don't ever expose this constructor outside this package.
     *
     * @param juspayServices The {@link JuspayServices} instance which constructed this object.
     */
    SdkTracker(@NonNull JuspayServices juspayServices) {
        this.juspayServices = juspayServices;
        try {
            hyperSdkVersion = IntegrationUtils.getSdkVersion(juspayServices.getContext());
            godelVersion = IntegrationUtils.getGodelVersion(juspayServices.getContext());
            godelBuildVersion = IntegrationUtils.getGodelBuildVersion(juspayServices.getContext());
        } catch (Exception exception) {
            // Ignore
        }
        ExecutorManager.runOnSdkTrackerPool(() -> ExecutorManager.setTrackerThreadId(Thread.currentThread().getId()));
    }

    public void resetSerialNumber() {
        serialNumberCounter = new AtomicInteger(1);
    }

    /**
     * Implementation method that tracks events. An event is defined as a happening. Anything that happens can be an event.
     *
     * @param category    The category of the event. Can be LIFECYCLE, ACTION, API_CALL, CONTEXT.
     * @param subCategory The sub-category of the event. SubCategory is defined according to the category.
     * @param level       The level of the event. Can be one of INFO, DEBUG, WARNING, ERROR, EXCEPTION.
     * @param label       The label of the event. This is the short descriptor, and should always be in snake case.
     * @param key         The key of the final Value in the log. It represents the value being passed in method call.
     * @param value       The message of the event. Can be as descriptive as you want.
     * @return A JSON object which can be posted.
     */
    private static JSONObject createLog(final String category, final String subCategory, final String level, final String label, final String key, final Object value) {
        JSONObject json = new JSONObject();
        JSONObject valueJson = new JSONObject();
        try {
            valueJson.put(key, (value == null ? JSONObject.NULL : cloneObject(value)));

            json.put("category", category);
            json.put("subcategory", subCategory);
            json.put("level", level);
            json.put("label", label);
            json.put("value", valueJson);
            json.put("at", System.currentTimeMillis());
            json.put("service", PaymentConstants.Category.SDK);
        } catch (JSONException e) {
            JuspayLogger.e(LOG_TAG, "Error while adding boot log: ", e);
        }

        return json;
    }

    /**
     * Implementation method that tracks events. An event is defined as a happening. Anything that happens can be an event.
     *
     * @param category    The category of the event. Can be LIFECYCLE, ACTION, API_CALL, CONTEXT.
     * @param subCategory The sub-category of the event. SubCategory is defined according to the category.
     * @param level       The level of the event. Can be one of INFO, DEBUG, WARNING, ERROR, EXCEPTION.
     * @param label       The label of the event. This is the short descriptor, and should always be in snake case.
     * @param value       The message of the event. Can be as descriptive as you want.
     * @return A JSON object which can be posted.
     */
    @NonNull
    private static JSONObject createLogWithValue(@SuppressWarnings("SameParameterValue") final String category, final String subCategory, final String level, final String label, final Object value) {
        JSONObject json = new JSONObject();
        try {
            json.put("category", category);
            json.put("subcategory", subCategory);
            json.put("level", level);
            json.put("label", label);
            json.put("value", cloneObject(value));
            json.put("at", System.currentTimeMillis());
            json.put("service", PaymentConstants.Category.SDK);
        } catch (JSONException e) {
            JuspayLogger.e(LOG_TAG, "Error while adding boot log: ", e);
        }

        return json;
    }

    @NonNull
    private static String getStackTraceAsString(@NonNull Throwable e) {
        StringBuilder output = new StringBuilder(e.toString());
        for (StackTraceElement se : e.getStackTrace()) {
            output.append("\n\tat ").append(se.toString());
        }
        return output.toString();
    }

    @NonNull
    private static String formatThrowable(Throwable e) {
        StringBuilder output = new StringBuilder(getStackTraceAsString(e));
        Throwable cause = e.getCause();
        while (cause != null) {
            output.append("\nCaused by ");
            output.append(getStackTraceAsString(cause));
            cause = cause.getCause();
        }
        return output.toString();
    }

    /**
     * Implementation method that tracks exceptions. An exception is defined as a happening which is unexpected.
     *
     * @param category    The category of the exception. Can be LIFECYCLE, ACTION, API_CALL, CONTEXT.
     * @param subCategory The sub-category of the exception. SubCategory is defined according to the category.
     * @param label       The label of the exception. This is the short descriptor, and should always be in snake case.
     * @param description Any extra information that you need apart from the throwable's message.
     * @param throwable   The throwable that describes the exception.
     * @return A JSON object which can be posted.
     */
    @NonNull
    private static JSONObject createExceptionLog(final String category, final String subCategory, final String label, @Nullable String description, @NonNull Throwable throwable, boolean printFullStack) {
        JSONObject json = new JSONObject();
        JSONObject valueJson = new JSONObject();

        try {
            valueJson.put("message", description + ". " + throwable.getLocalizedMessage());
            if (printFullStack) {
                valueJson.put("stacktrace", formatThrowable(throwable));
            } else {
                valueJson.put("stacktrace", Log.getStackTraceString(throwable));
            }
            json.put("category", category);
            json.put("subcategory", subCategory);
            json.put("level", "exception");
            json.put("label", label + "_" + Utils.getLogLevelFromThrowable(throwable));
            json.put("value", valueJson);
            json.put("service", PaymentConstants.Category.SDK);
            json.put("at", System.currentTimeMillis());
        } catch (JSONException e) {
            JuspayLogger.e(LOG_TAG, "Error while adding log: ", e);
        }

        return json;
    }

    @NonNull
    private static JSONObject createExceptionLog(final String category, final String subCategory, final String label, @Nullable String description, Throwable throwable) {
        return createExceptionLog(category, subCategory, label, description, throwable, false);
    }

    @NonNull
    private static JSONObject createApiLog(final String subCategory, final String level, final String label, @Nullable final Integer statusCode, final String url, final Long startTime, final Long endTime, final Object payload, final Object response, final String method) {
        JSONObject json = new JSONObject();
        JSONObject valueJson = new JSONObject();
        try {
            valueJson.put("url", url);
            valueJson.put("status_code", statusCode);
            valueJson.put("start_time", startTime);
            valueJson.put("end_time", endTime);
            valueJson.put("payload", (payload == null ? JSONObject.NULL : cloneObject(payload)));
            valueJson.put("response", cloneObject(response));
            valueJson.put("method", method);

            json.put("category", LogCategory.API_CALL);
            json.put("subcategory", subCategory);
            json.put("level", level);
            json.put("label", label);
            json.put("value", valueJson);
            json.put("at", System.currentTimeMillis());
            json.put("service", PaymentConstants.Category.SDK);
        } catch (JSONException e) {
            JuspayLogger.e(LOG_TAG, "Error while adding boot log: ", e);
        }

        return json;
    }

    @NonNull
    private static JSONObject createApiLog(final String subCategory, final String level, final String label, @Nullable final Integer statusCode, final String url, final String tag, final Long startTime, final Long endTime, final Object payload, final Object response, final String method, final JSONArray channels, final JSONObject rootLogFields) {
        JSONObject json = new JSONObject();
        JSONObject valueJson = new JSONObject();
        try {
            valueJson.put("url", url);
            valueJson.put("status_code", statusCode);
            valueJson.put("start_time", startTime);
            valueJson.put("end_time", endTime);
            valueJson.put("payload", (payload == null ? JSONObject.NULL : payload));
            valueJson.put("response", response);
            valueJson.put("method", method);
            if (tag != null) {
                valueJson.put("api_tag", tag);
            }

            json.put("category", LogCategory.API_CALL);
            json.put("subcategory", subCategory);
            json.put("level", level);
            json.put("label", label);
            json.put("channels", channels);
            json.put("value", valueJson);
            json.put("at", System.currentTimeMillis());
            json.put("service", PaymentConstants.Category.SDK);

            if (rootLogFields != null) {
                JSONArray keys = rootLogFields.names();
                if (keys != null) {
                    for (int i = 0; i < keys.length(); i++) {
                        String key = keys.getString(i);
                        String value = rootLogFields.getString(key);
                        json.put(key, value);
                    }
                }
            }

        } catch (JSONException e) {
            JuspayLogger.e(LOG_TAG, "Error while adding boot log: ", e);
        }

        return json;
    }

    public static void addToBootLogs(String json) {
        ExecutorManager.runOnSdkTrackerPool(() -> {
            JuspayLogger.log(LOG_TAG, "DEBUG", json);
            try {
                JSONObject log = new JSONObject(json);
                if (!log.has("at")) {
                    log.put("at", System.currentTimeMillis());
                }
                bootLogs.add(log);
            } catch (Exception e) {
                JuspayLogger.e(LOG_TAG, "addToBootLogs", e);
            }
        });
    }

    /**
     * Implementation method that tracks Lifecycle without juspayservice.
     *
     * @param subCategory The sub-category of the LifeCycle event. Can be one of HYPER_SDK, ANDROID.
     * @param level       The level of the LifeCycle event. Can be one of INFO, DEBUG, WARNING, ERROR, EXCEPTION.
     * @param label       The label of the LifeCycle event. This is the short descriptor, and should always be in snake case.
     * @param key         The key of the final Value in the log. It represents the value being passed in method call.
     * @param value       The message of the LifeCycle event. Can be as descriptive as you want.
     */
    public static void trackBootLifecycle(final String subCategory, final String level, final String label, @Nullable final String key, final Object value) {
        ExecutorManager.runOnSdkTrackerPool(() -> {
            JSONObject json = createLog(LogCategory.LIFECYCLE, subCategory, level, label, key, value);
            bootLogs.add(json);
        });
    }

    /**
     * Implementation method that tracks Action without juspayservice.
     *
     * @param subCategory The sub-category of the Action event. Can be one of SYSTEM, USER.
     * @param level       The level of the Action event. Can be one of INFO, DEBUG, WARNING, ERROR, EXCEPTION.
     * @param label       The label of the Action event. This is the short descriptor, and should always be in snake case.
     * @param key         The key of the final Value in the log. It represents the value being passed in method call.
     * @param value       The message of the Action event. Can be as descriptive as you want.
     */
    public static void trackBootAction(final String subCategory, final String level, final String label, @Nullable final String key, final Object value) {
        ExecutorManager.runOnSdkTrackerPool(() -> {
            JSONObject json = createLog(LogCategory.ACTION, subCategory, level, label, key, value);
            bootLogs.add(json);
        });
    }

    /**
     * Implementation method that tracks Exception without juspayservice.
     *
     * @param category    The category of the exception. Can be LIFECYCLE, ACTION, API_CALL, CONTEXT.
     * @param subCategory The sub-category of the exception. SubCategory is defined according to the category.
     * @param label       The label of the exception. This is the short descriptor, and should always be in snake case.
     * @param description a message for the exception
     * @param throwable   stacktrace of the exception
     */
    public static void trackBootException(final String category, final String subCategory, final String label, @Nullable final String description, final Throwable throwable) {
        ExecutorManager.runOnSdkTrackerPool(() -> {
            JSONObject json = createExceptionLog(category, subCategory, label, description, throwable);
            bootLogs.add(json);
        });
    }

    /**
     * Implementation method that logs the Exception without juspayservice before tracking it.
     *
     * @param tag         The class name where this exception is being caught. Only used to log in Log Cat.
     * @param category    The category of the exception. Can be LIFECYCLE, ACTION, API_CALL, CONTEXT.
     * @param subCategory The sub-category of the exception. SubCategory is defined according to the category.
     * @param label       The label of the exception. This is the short descriptor, and should always be in snake case.
     * @param description A message for the exception
     * @param throwable   Stacktrace of the exception
     */
    public static void trackAndLogBootException(final String tag, final String category, final String subCategory, final String label, @Nullable final String description, final Throwable throwable) {
        ExecutorManager.runOnSdkTrackerPool(() -> {
            JuspayLogger.e(tag, description, throwable);
            JSONObject json = createExceptionLog(category, subCategory, label, description, throwable);
            bootLogs.add(json);
        });
    }

    /**
     * Utility method to sign a log. Signing sets the session ID and the serial number.
     *
     * @param json The JSON for the log line.
     * @throws JSONException If the JSON manipulation fails.
     */
    private void signLog(@NonNull JSONObject json) throws JSONException {
        final SessionInfo sessionInfo = juspayServices.getSessionInfo();

        if (!json.has("session_id")) {
            json.put("session_id", sessionInfo.getSessionId());
        }

        String clientId = sessionInfo.getClientId();
        if ((!json.has("client_id")) && !clientId.isEmpty()) {
            String[] arr = clientId.split("_", 2);
            if (arr.length > 0) {
                json.put("client_id", arr[0].toLowerCase(Locale.getDefault()));
            }
        }


        String merchantId = sessionInfo.getMerchantId();
        if (!merchantId.isEmpty() && !json.has("merchant_id")) {
            json.put("merchant_id", merchantId);
        }

        String orderId = sessionInfo.getOrderId();
        if (!orderId.isEmpty() && !json.has("order_id")) {
            json.put("order_id", orderId);
        }


        if (!json.has("package_name")) {
            json.put("package_name", sessionInfo.getPackageName());
        }

        if (!json.has("log_version")) {
            json.put("log_version", PaymentConstants.LOG_VERSION);
        }

        json.put("sn", serialNumberCounter.getAndIncrement());

        if (!json.has("hyper_sdk_version")) {
            json.put("hyper_sdk_version", hyperSdkVersion);
        }

        if (!json.has("godel_version")) {
            json.put("godel_version", godelVersion);
        }

        if (!json.has("godel_build_version")) {
            json.put("godel_build_version", godelBuildVersion);
        }

        for (Iterator<String> it = logProperties.keys(); it.hasNext(); ) {
            String key = it.next();
            json.put(key, logProperties.optString(key));
        }
    }

    private boolean shouldDropLog(String label) {
        return labelsToDrop.contains(label);
    }

    /**
     * Implementation method that tracks Lifecycle.
     *
     * @param subCategory The sub-category of the LifeCycle event. Can be one of HYPER_SDK, ANDROID.
     * @param level       The level of the LifeCycle event. Can be one of INFO, DEBUG, WARNING, ERROR, EXCEPTION.
     * @param label       The label of the LifeCycle event. This is the short descriptor, and should always be in snake case.
     * @param key         The key of the final Value in the log. It represents the value being passed in method call.
     * @param value       The message of the LifeCycle event. Can be as descriptive as you want.
     */
    @Override
    public void trackLifecycle(final @NonNull String subCategory, final @NonNull String level, final @NonNull String label, @Nullable final String key, final @Nullable Object value) {
        ExecutorManager.runOnSdkTrackerPool(() -> {
            if (shouldDropLog(label)) {
                return;
            }
            JSONObject json = createLog(LogCategory.LIFECYCLE, subCategory, level, label, key, value);
            if (juspayServices.getSessionInfo().getSessionId() != null) {
                trackParsed(json);
            } else {
                bootLogs.add(json);
            }
        });
    }

    public void trackLifecycle(final String subCategory, final String level, final String label, final JSONObject value) {
        ExecutorManager.runOnSdkTrackerPool(() -> {
            if (shouldDropLog(label)) {
                return;
            }
            JSONObject json = createLogWithValue(LogCategory.LIFECYCLE, subCategory, level, label, value);
            if (juspayServices.getSessionInfo().getSessionId() != null) {
                trackParsed(json);
            } else {
                bootLogs.add(json);
            }
        });
    }


    /**
     * Implementation method that tracks Action.
     *
     * @param subCategory The sub-category of the Action event. Can be one of SYSTEM, USER.
     * @param level       The level of the Action event. Can be one of INFO, DEBUG, WARNING, ERROR, EXCEPTION.
     * @param label       The label of the Action event. This is the short descriptor, and should always be in snake case.
     * @param key         The key of the final Value in the log. It represents the value being passed in method call.
     * @param value       The message of the Action event. Can be as descriptive as you want.
     */
    @Override
    public void trackAction(@NonNull final String subCategory, @NonNull final String level, @NonNull final String label, @Nullable final String key, @NonNull final Object value) {
        ExecutorManager.runOnSdkTrackerPool(() -> {
            if (shouldDropLog(label)) {
                return;
            }
            JSONObject json = createLog(LogCategory.ACTION, subCategory, level, label, key, value);
            if (juspayServices.getSessionInfo().getSessionId() != null) {
                trackParsed(json);
            } else {
                bootLogs.add(json);
            }
        });
    }

    /**
     * Implementation method that tracks Apicalls.
     *
     * @param subCategory The sub-category of the Apicall event. Can be one of NETWORK, SDK.
     * @param level       The level of the Apicall event. Can be one of INFO, DEBUG, WARNING, ERROR, EXCEPTION.
     * @param label       The label of the Apicall event. This is the short descriptor, and should always be in snake case.
     * @param statusCode  The status code returned by the Api Call.
     * @param url         The URL of the Apicall event. This is url to which Api call was made.
     * @param startTime   The start time of the Apicall event. The Unix TimeStamp at which Api Call was made.
     * @param endTime     The end time of the Apicall event. The Unix TimeStamp at which Api Call was returned.
     * @param payload     The payload of the Apicall event. This is payload recieved by Api Call
     * @param response    The response of the Apicall event. This is reponse returned by Api call
     * @param method      The HTTP method of the Apicall event. Can be GET, POST, PUT, etc.
     */
    @Override
    public void trackApiCalls(final @NonNull String subCategory, final @NonNull String level, final @NonNull String label, @Nullable final Integer statusCode, final @Nullable String url, final @Nullable Long startTime, final @Nullable Long endTime, final @Nullable Object payload, final @Nullable Object response, final String method) {
        ExecutorManager.runOnSdkTrackerPool(() -> {
            if (shouldDropLog(label)) {
                return;
            }
            JSONObject json = createApiLog(subCategory, level, label, statusCode, url, startTime, endTime, payload, response, method);
            if (juspayServices.getSessionInfo().getSessionId() != null) {
                trackParsed(json);
            } else {
                bootLogs.add(json);
            }
        });
    }

    public void trackApiCalls(final String subCategory, final String level, final String label, @Nullable final Integer statusCode, final String url, final String tag, final long startTime, final Long endTime, final Object payload, final Object response, final String method, final JSONArray channels, final JSONObject rootLogFields) {
        ExecutorManager.runOnSdkTrackerPool(() -> {
            JSONObject json = createApiLog(subCategory, level, label, statusCode, url, tag, startTime, endTime, payload, response, method, channels, rootLogFields);
            if (juspayServices.getSessionInfo().getSessionId() != null) {
                trackParsed(json);
            } else {
                bootLogs.add(json);
            }
        });
    }

    /**
     * Implementation method that tracks Context.
     *
     * @param subCategory The sub-category of the Context event. Can be DEVICE.
     * @param level       The level of the Context event. Can be one of INFO, DEBUG, WARNING, ERROR, EXCEPTION.
     * @param label       The label of the Context event. This is the short descriptor, and should always be in snake case.
     * @param key         The key of the final Value in the log. It represents the value being passed in method call.
     * @param value       The message of the Context event. Can be as descriptive as you want.
     */
    public void trackContext(final String subCategory, final String level, final String label, @Nullable final String key, final Object value) {
        ExecutorManager.runOnSdkTrackerPool(() -> {
            if (shouldDropLog(label)) {
                return;
            }
            JSONObject json = createLog(LogCategory.CONTEXT, subCategory, level, label, key, value);
            if (juspayServices.getSessionInfo().getSessionId() != null) {
                trackParsed(json);
            } else {
                bootLogs.add(json);
            }
        });
    }

    /**
     * Implementation method that tracks Context.
     *
     * @param subCategory The sub-category of the Context event. Can be DEVICE.
     * @param level       The level of the Context event. Can be one of INFO, DEBUG, WARNING, ERROR, EXCEPTION.
     * @param label       The label of the Context event. This is the short descriptor, and should always be in snake case.
     * @param value       The message of the Context event. Can be as descriptive as you want.
     */
    public void trackContext(final String subCategory, final String level, final String label, final Object value) {
        ExecutorManager.runOnSdkTrackerPool(() -> {
            if (shouldDropLog(label)) {
                return;
            }
            JSONObject json = createLogWithValue(LogCategory.CONTEXT, subCategory, level, label, value);
            if (juspayServices.getSessionInfo().getSessionId() != null) {
                trackParsed(json);
            } else {
                bootLogs.add(json);
            }
        });
    }

    /**
     * Implementation method that tracks Exception.
     *
     * @param category    The category of the exception. Can be LIFECYCLE, ACTION, API_CALL, CONTEXT.
     * @param subCategory The sub-category of the exception. SubCategory is defined according to the category.
     * @param label       The label of the exception. This is the short descriptor, and should always be in snake case.
     * @param description a message for the exception
     * @param throwable   stacktrace of the exception
     */
    public void trackException(final String category, final String subCategory, final String label, final String description, final Throwable throwable) {
        ExecutorManager.runOnSdkTrackerPool(() -> {
            if (shouldDropLog(label)) {
                return;
            }
            JSONObject json = createExceptionLog(category, subCategory, label, description, throwable);
            if (juspayServices.getSessionInfo().getSessionId() != null) {
                trackParsed(json);
            } else {
                bootLogs.add(json);
            }
        });
    }

    @NonNull
    public JSONObject getExceptionLog(final String category, final String subCategory, final String label, final String description, final Throwable throwable) {
        JSONObject json = createExceptionLog(category, subCategory, label, description, throwable, true);
        try {
            signLog(json);
        } catch (Exception e) {
            JuspayLogger.e(LOG_TAG, "getExceptionLog failed", e);
        }
        return json;
    }

    @Override
    public void addLogToPersistedQueue(@NonNull JSONObject logLine) {
        ExecutorManager.runOnSdkTrackerPool(() -> {
            if (Objects.equals(juspayServices.getWorkingLogger(), "json-array") || Objects.equals(juspayServices.getWorkingLogger(), "both")) {
                LogPusher.addLogsToPersistedQueue(logLine);
            }
            if (juspayServices.getLogSessioniserExp() != null) {
                LogPusherExp.addLogsToPersistedQueue(logLine);
            }
        });
    }

    /**
     * Implementation method that logs the exception before tracking it.
     *
     * @param tag         The class name where this exception is being caught. Only used to log in Log Cat.
     * @param category    The category of the exception. Can be LIFECYCLE, ACTION, API_CALL, CONTEXT.
     * @param subCategory The sub-category of the exception. SubCategory is defined according to the category.
     * @param label       The label of the exception. This is the short descriptor, and should always be in snake case.
     * @param description A message for the exception
     * @param throwable   Stacktrace of the exception
     */
    @Override
    public void trackAndLogException(@NonNull final String tag, @NonNull final String category, @NonNull final String subCategory, @NonNull final String label, @NonNull final String description, @NonNull final Throwable throwable) {
        ExecutorManager.runOnSdkTrackerPool(() -> {
            if (shouldDropLog(label)) {
                return;
            }
            JuspayLogger.e(tag, description, throwable);
            trackPhoneState();
            JSONObject json = createExceptionLog(category, subCategory, label, description, throwable);

            if (juspayServices.getSessionInfo().getSessionId() != null) {
                trackParsed(json);
            } else {
                bootLogs.add(json);
            }
        });
    }

    private void trackPhoneState() {
        try {
            JSONObject phoneState = new JSONObject();
            final SessionInfo sessionInfo = juspayServices.getSessionInfo();

            final Context context = juspayServices.getContext();

            ActivityManager.MemoryInfo memoryInfo = Utils.getMemoryInfo(context);

            if (memoryInfo != null) {
                phoneState.put("available_memory", memoryInfo.availMem);
                phoneState.put("threshold_memory", memoryInfo.threshold);

                phoneState.put("total_memory", memoryInfo.totalMem);
            }

            phoneState.put("network_info", sessionInfo.getNetworkInfo());
            phoneState.put("network_type", String.valueOf(sessionInfo.getNetworkType()));
            phoneState.put("ip_address", Utils.getIPAddress(juspayServices));
            trackContext(LogSubCategory.Context.DEVICE, LogLevel.INFO, Labels.Device.PHONE_STATE, phoneState);
        } catch (Exception ignored) {

        }
    }


    public void trackAndLogApiException(final String tag, final String category, final String subCategory, final String label, final Long startTime, final Long endTime, final Object payload, final String url, final String method, final String description, final Throwable throwable, final JSONArray channels, final JSONObject rootLogFields) {
        ExecutorManager.runOnSdkTrackerPool(() -> {
            JuspayLogger.e(tag, description, throwable);
            if (shouldDropLog(label)) {
                return;
            }
            trackPhoneState();
            JSONObject json = createApiExceptionLog(category, subCategory, label, startTime, endTime, payload, url, method, description, throwable, channels, rootLogFields);

            if (juspayServices.getSessionInfo().getSessionId() != null) {
                trackParsed(json);
            } else {
                bootLogs.add(json);
            }
        });
    }

    @NonNull
    private JSONObject createApiExceptionLog(final String category, final String subCategory, final String label, final Long startTime, final Long endTime, final Object payload, final String url, final String method, final String description, final Throwable throwable, final JSONArray channels, final JSONObject rootLogFields) {

        JSONObject json = new JSONObject();
        JSONObject valueJson = new JSONObject();
        try {
            valueJson.put("url", url);
            valueJson.put("start_time", startTime);
            valueJson.put("end_time", endTime);
            valueJson.put("payload", (payload == null ? JSONObject.NULL : cloneObject(payload)));
            valueJson.put("method", method);

            valueJson.put("message", description + ". " + throwable.getLocalizedMessage());
            valueJson.put("stacktrace", formatThrowable(throwable));

            json.put("category", category);
            json.put("subcategory", subCategory);
            json.put("level", "exception");
            json.put("label", label + "_" + Utils.getLogLevelFromThrowable(throwable));
            json.put("value", valueJson);
            json.put("at", System.currentTimeMillis());
            json.put("service", PaymentConstants.Category.SDK);
            json.put("channels", channels);

            if (rootLogFields != null) {
                JSONArray keys = rootLogFields.names();
                if (keys != null) {
                    for (int i = 0; i < keys.length(); i++) {
                        String key = keys.getString(i);
                        String value = rootLogFields.getString(key);
                        json.put(key, value);
                    }
                }
            }


        } catch (Exception exception) {
            JuspayLogger.e(LOG_TAG, "Error while adding API exception log: ", exception);
        }
        return json;
    }

    /**
     * Tracks a log line. This accepts a stringify'ed JSON object which corresponds to the logger structure.
     *
     * @param log The log line that needs to be tracked.
     */
    @Override
    public void track(@NonNull JSONObject log) {
        ExecutorManager.runOnSdkTrackerPool(() -> {
            try {
                if (shouldDropLog(log.optString("label", ""))) {
                    return;
                }
                log.put("at", System.currentTimeMillis()); //
                trackParsed(log);
            } catch (JSONException e) {
                trackException(LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.LOG_PUSHER, "Exception while parsing the JSON", e);
            }
        });
    }

    /**
     * A helper method that checks if there are any logs available in the boot tracker, and consumes them into the
     * current session.
     */
    private void processBootLogs() {
        while (!bootLogs.isEmpty()) {
            JSONObject bootLog = bootLogs.poll();
            if (bootLog != null) {
                try {
                    if (shouldDropLog(bootLog.optString("label", ""))) {
                        continue;
                    }
                    signLog(bootLog);
                    if (Objects.equals(juspayServices.getWorkingLogger(), "json-array") || Objects.equals(juspayServices.getWorkingLogger(), "both")) {
                        LogSessioniser.addLogLine(juspayServices.getSessionInfo().getSessionId(), bootLog);
                    }
                    if (juspayServices.getLogSessioniserExp() != null) {
                        juspayServices.getLogSessioniserExp().addLogLine(bootLog);
                    }
                } catch (Exception e) {
                    trackException(LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.LOG_PUSHER, "Exception while signing log line", e);
                }
            }
        }
    }

    private static Object cloneObject(Object obj) {
        try {
            if (obj instanceof JSONObject) {
                return cloneJSON((JSONObject) obj);
            } else if (obj instanceof JSONArray) {
                return cloneJSONArray((JSONArray) obj);
            } else {
                return obj;
            }
        } catch (Exception exception) {
            return obj;
        }
    }

    @NonNull
    private static JSONObject cloneJSON(@NonNull JSONObject jsonObject) throws JSONException, IndexOutOfBoundsException {
        JSONArray keys = jsonObject.names();
        keys = keys == null ? new JSONArray() : keys;
        JSONObject json = new JSONObject();
        for (int i = 0; i < keys.length(); ++i) {
            String str = (String) keys.opt(i);
            Object obj = jsonObject.opt(str);
            if (obj instanceof JSONObject) {
                json.put(str, cloneJSON((JSONObject) obj));
            } else if (obj instanceof JSONArray) {
                json.put(str, cloneJSONArray((JSONArray) obj));
            } else {
                json.put(str, obj);
            }
        }
        return json;
    }

    @NonNull
    private static JSONArray cloneJSONArray(@NonNull JSONArray jsonArray) throws JSONException, IndexOutOfBoundsException {
        JSONArray json = new JSONArray();
        for (int j = 0; j < jsonArray.length(); ++j) {
            Object obj = jsonArray.opt(j);
            if (obj instanceof JSONObject) {
                json.put(cloneJSON((JSONObject) obj));
            } else if (obj instanceof JSONArray) {
                json.put(cloneJSONArray((JSONArray) obj));
            } else {
                json.put(obj);
            }
        }
        return json;
    }


    /**
     * Tracks a log line. This accepts a JSON object which corresponds to the logger structure.
     *
     * @param logLine The log line that needs to be tracked.
     */
    private void trackParsed(JSONObject logLine) {
        try {
            if (!LogConstants.shouldPush) {
                bootLogs.clear();
                return;
            }

            truncateLog(logLine);
            signLog(logLine);
//                juspayServices.sdkDebug(LOG_TAG, logLine.toString()); // This can be removed once all the logger changes are finalised and merged.
            if (Objects.equals(juspayServices.getWorkingLogger(), "json-array") || Objects.equals(juspayServices.getWorkingLogger(), "both")) {
                LogSessioniser.addLogLine(juspayServices.getSessionInfo().getSessionId(), logLine);
            }
            if (juspayServices.getLogSessioniserExp() != null) {
                juspayServices.getLogSessioniserExp().addLogLine(logLine);
            }

            processBootLogs();
        } catch (Exception e) {
            trackException(LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.LOG_PUSHER, "Exception while signing log line", e);
        }
    }

    private void truncateLog(@NonNull JSONObject logLine) throws JSONException {
        for (Iterator<String> it = logLine.keys(); it.hasNext(); ) {
            String log = it.next();
            Object line = logLine.get(log);
            if (line instanceof String) {
                String str = (String) line;
                if (str.length() > MAX_LOG_SIZE) {
                    logLine.put(log, str.substring(0, MAX_LOG_SIZE));
                }
            } else if (line instanceof JSONObject) {
                truncateLog((JSONObject) line);
            }
        }
    }

    public void setEndPointSandbox(Boolean isSandbox) {
        ExecutorManager.runOnLogSessioniserThread(() -> {
            if (Objects.equals(juspayServices.getWorkingLogger(), "json-array") || Objects.equals(juspayServices.getWorkingLogger(), "both")) {
                LogPusher.setEndPointSandbox(isSandbox);
            }
            if (juspayServices.getLogSessioniserExp() != null) {
                LogPusherExp.setEndPointSandbox(isSandbox);
            }
        });
    }

    public void setLabelsToDrop(JSONObject logsConfig) {
        try {
            JSONArray array = logsConfig.getJSONArray("labelsToDrop");
            for (int i = 0; i < array.length(); i++) {
                labelsToDrop.add(array.getString(i));
            }
        } catch (Exception ignored) {
        }
    }

    public void addLogProperties(JSONObject properties) {
        try {
            JSONObject logPropertiesConfig = LogConstants.logProperties;
            Iterator<String> keys = logPropertiesConfig.keys();
            while (keys.hasNext()) {
                String key = keys.next();
                String value = logPropertiesConfig.getString(key);
                int dollarIdx = value.indexOf('$');
                int startIdx = value.indexOf('{');
                int lastIdx = value.lastIndexOf('}');
                if (dollarIdx != -1) {
                    if (startIdx != -1 && lastIdx != -1 && startIdx - dollarIdx == 1 && startIdx < lastIdx) {
                        String prop = value.substring(startIdx + 1, lastIdx);
                        if (!properties.has(prop)) {
                            continue;
                        }
                        String toReplace = "${" + prop + "}";
                        logProperties.put(key, value.replace(toReplace, properties.optString(prop)));
                    }
                }
            }
        } catch (Exception ignored) {
        }
    }
}
