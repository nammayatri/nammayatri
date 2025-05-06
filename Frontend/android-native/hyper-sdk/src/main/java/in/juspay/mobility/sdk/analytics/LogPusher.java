package in.juspay.mobility.sdk.analytics;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.File;
import java.io.FileOutputStream;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.security.interfaces.RSAPublicKey;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Objects;
import java.util.Queue;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.atomic.AtomicInteger;

import in.juspay.mobility.sdk.hyper.constants.Labels;
import in.juspay.mobility.sdk.hyper.constants.LogCategory;
import in.juspay.mobility.sdk.hyper.constants.LogLevel;
import in.juspay.mobility.sdk.hyper.constants.LogSubCategory;
import in.juspay.mobility.sdk.hyper.core.ExecutorManager;
import in.juspay.mobility.sdk.hyper.core.JuspayCoreLib;
import in.juspay.mobility.sdk.hyper.core.JuspayLogger;
import in.juspay.mobility.sdk.core.SdkTracker;
import in.juspay.mobility.sdk.security.EncryptionHelper;
import in.juspay.mobility.sdk.security.JOSEUtils;
import in.juspay.mobility.sdk.utils.Utils;
import in.juspay.mobility.sdk.utils.network.JuspayHttpsResponse;
import in.juspay.mobility.sdk.utils.network.NetUtils;
import kotlin.Triple;

/**
 * A class that does takes care of pushing the logs that are tracked. This class also has disk-caching to avoid
 * the logs from missing.
 *
 * @author Sri Harsha Chilakapati [sri.harsha@juspay.in]
 * @author Parth Vora [parth.vora@juspay.in]
 * @since 14/02/2020
 */

// All public methods will run on Background Thread, enforcing private methods automatically to be in LogsThread.
public class LogPusher {
    private static final String TAG = "LogPusher";
    final private static Map<String, LogChannel> channels = new HashMap<>();
    @NonNull
    private static final AtomicInteger logPusherNumCounter = new AtomicInteger(0);
    @NonNull
    private static final AtomicInteger logPushIteration = new AtomicInteger(1);
    private static TimerTask logPushTimerTask;
    private static int getLogsToPushErrorCounter = 0;
    private static int logFlushTimerTaskErrorCounter = 0;
    private static int logPushTimerTaskErrorCounter = 0;
    private static int setHeaderParametersErrorCounter = 0;
    private static boolean isSandboxEnv = false;
    private static boolean stopPushingLogs = false;
    @NonNull
    private static JSONObject channelsFromSdkConfig = new JSONObject();
    @NonNull
    private static JSONArray logChannelsConfig = new JSONArray();
    @NonNull
    private static Timer logPushTimer = new Timer();

    static {
        ExecutorManager.runOnLogsPool(() -> {
            if (!LogConstants.shouldPush) {
                return;
            }

            channelsFromSdkConfig = LogConstants.channels;
            if (channelsFromSdkConfig != null) {
                for (Iterator<String> it = channelsFromSdkConfig.keys(); it.hasNext(); ) {
                    String channelName = it.next();
                    try {
                        addChannelFromJS(channelsFromSdkConfig.get(channelName).toString(), channelName);
                    } catch (JSONException e) {
                        e.printStackTrace();
                    }
                }
            }

            logChannelsConfig = LogConstants.logChannelsConfig;
            if (logChannelsConfig != null) {
                try {
                    for (int i = 0; i < logChannelsConfig.length(); i++) {
                        JSONObject channelConfig = logChannelsConfig.getJSONObject(i);
                        addChannelFromJS(channelConfig.toString(), channelConfig.getString("channel"));
                    }
                } catch (JSONException ignore) {
                }
            }

            ExecutorManager.setLogsThreadId(Thread.currentThread().getId());
            String[] previousChannels = LogUtils.getAnyFromSharedPreference(LogConstants.LOG_CHANNEL_NAMES, "").split(",");
            LogUtils.writeToSharedPreference(LogConstants.LOG_CHANNEL_NAMES, "");
            addChannel(LogConstants.DEFAULT_CHANNEL, LogConstants.maxRetryPerBatch, LogConstants.maxLogsPerPush, LogConstants.prodLogUrl, LogConstants.sandboxLogUrl, LogConstants.publicKey, LogConstants.publicKeySandbox, new HashMap<>(), LogConstants.defaultPriority, "all", LogConstants.encryptionLevel);
            LogChannel defaultChannel = getChannelObject(LogConstants.DEFAULT_CHANNEL);
            for (String channelName : previousChannels) {
                String chInfo = LogUtils.getAnyFromSharedPreference(LogConstants.LOG_CHANNEL_INFO + "_" + channelName, "");
                if (!channels.containsKey(channelName)) {
                    LogUtils.removeFromSharedPreference(LogConstants.LOG_CHANNEL_INFO + "_" + channelName);
                }
                LogChannel channel = null;
                if (chInfo.length() != 0) {
                    try {
                        JSONObject channelInfo = new JSONObject(chInfo);
                        int retryAttempts = channelInfo.optInt("retryAttempts", LogConstants.maxRetryPerBatch);
                        long batchCount = channelInfo.optLong("batchCount", LogConstants.maxLogsPerPush);
                        String endPointProd = channelInfo.optString("logsUrlKey", LogConstants.prodLogUrl);
                        String endpointSBX = channelInfo.optString("logsUrlKeySandbox", LogConstants.sandboxLogUrl);
                        JSONObject keyProd = channelInfo.getJSONObject("publicKey");
                        JSONObject keySBX = channelInfo.getJSONObject("publicKeySandbox");
                        Map<String, String> channelHeaders = LogUtils.toMap(new JSONObject(channelInfo.getString("headers")));
                        int priority = channelInfo.optInt("priority", LogConstants.defaultPriority);
                        String environment = channelInfo.optString("environment", "all");
                        String encryptionLevel = channelInfo.optString("encryptionLevelKey", LogConstants.encryptionLevel);
                        channel = makeChannel(channelName, retryAttempts, batchCount, endPointProd, endpointSBX, keyProd, keySBX, channelHeaders, priority, environment, encryptionLevel);
                    } catch (JSONException ignore) {
                    }
                }
                if (channel == null)
                    channel = makeChannel(channelName, LogConstants.maxRetryPerBatch, LogConstants.maxLogsPerPush, LogConstants.prodLogUrl, LogConstants.sandboxLogUrl, defaultChannel.getKeyProd(), defaultChannel.getKeySBX(), defaultChannel.getHeaders(), 1, "all", LogConstants.encryptionLevel);
                pushOldChannelLogs(channel);
            }

            LogChannel oldLogChannel = makeChannel("", LogConstants.maxRetryPerBatch, LogConstants.maxLogsPerPush, LogConstants.prodLogUrl, LogConstants.sandboxLogUrl, defaultChannel.getKeyProd(), defaultChannel.getKeySBX(), defaultChannel.getHeaders(), 1, "all", LogConstants.encryptionLevel);
            pushOldChannelLogs(oldLogChannel);
        });

    }

    private static void pushOldChannelLogs(LogChannel channel) {

        int readingIndex = LogUtils.getFromSharedPreference(LogConstants.PERSISTENT_LOGS_READING_FILE + channel.getChannelName());
        if (readingIndex == -1) {
            readingIndex = 0;
            LogUtils.writeToSharedPreference(LogConstants.PERSISTENT_LOGS_READING_FILE + channel.getChannelName(), String.valueOf(readingIndex));
        }

        int writingIndex = LogUtils.getFromSharedPreference(LogConstants.PERSISTENT_LOGS_WRITING_FILE + channel.getChannelName());
        if (writingIndex == -1) {
            writingIndex = 0;
            LogUtils.writeToSharedPreference(LogConstants.PERSISTENT_LOGS_WRITING_FILE + channel.getChannelName(), String.valueOf(writingIndex));
        }

        // load all the files and send them.

        pushAllFiles(readingIndex, writingIndex, channel);
    }


    public static String[] getChannelNames() {
        return channels.keySet().toArray(new String[0]);
    }

    public static boolean addChannelFromJS(String jsonObj, String channelName) {
        try {
            JSONObject channelInfo = new JSONObject(jsonObj);
            int retryAttempts = channelInfo.optInt("retryAttempts", LogConstants.maxRetryPerBatch);
            long batchCount = channelInfo.optLong("batchCount", LogConstants.maxLogsPerPush);
            String endPointProd = channelInfo.optString("logsUrlKey", LogConstants.prodLogUrl);
            String endpointSBX = channelInfo.optString("logsUrlKeySandbox", LogConstants.sandboxLogUrl);
            JSONObject keyProd = channelInfo.has("publicKey") ? channelInfo.getJSONObject("publicKey") : LogConstants.publicKey;
            JSONObject keySBX = channelInfo.has("publicKeySandbox") ? channelInfo.getJSONObject("publicKeySandbox") : LogConstants.publicKeySandbox;
            Map<String, String> channelHeaders = channelInfo.has("channelHeaders") ? LogUtils.toMap(channelInfo.getJSONObject("channelHeaders")) : new HashMap<>();
            int priority = channelInfo.optInt("priority", LogConstants.defaultPriority);
            String environment = channelInfo.optString("environment", "all");
            String encryptionLevel = channelInfo.optString("encryptionLevelKey", LogConstants.encryptionLevel);
            return addChannel(channelName, retryAttempts, batchCount, endPointProd, endpointSBX, keyProd, keySBX, channelHeaders, priority, environment, encryptionLevel);
        } catch (JSONException ignore) {
        }
        return false;

    }

    private static LogChannel makeChannel(String name, int retryAttempts, long batchCount,
                                          String endPointProd, String endpointSBX,
                                          JSONObject keyProd, JSONObject keySBX, Map<String, String> channelHeaders, int priority,
                                          String environment, String encryptionLevel) {
        return new LogChannel(retryAttempts, batchCount, name, endPointProd, endpointSBX, keyProd, keySBX, channelHeaders, priority, environment, encryptionLevel);
    }

    private static boolean addChannel(String name, int retryAttempts, long batchCount,
                                      String endPointProd, String endpointSBX,
                                      JSONObject keyProd, JSONObject keySBX, Map<String, String> channelHeaders, int priority,
                                      String environment, String encryptionLevel) {
        boolean alreadyExists = channels.containsKey(name);

        LogChannel ch = makeChannel(name, retryAttempts, batchCount, endPointProd, endpointSBX, keyProd, keySBX, channelHeaders, priority, environment, encryptionLevel);
        channels.put(name, ch);

        if (!alreadyExists) {
            String savedChannels = LogUtils.getAnyFromSharedPreference(LogConstants.LOG_CHANNEL_NAMES, "");
            String updatedChannels = savedChannels + (savedChannels.length() == 0 ? "" : ",") + name;
            LogUtils.writeToSharedPreference(LogConstants.LOG_CHANNEL_NAMES, updatedChannels);
        }
        LogUtils.writeToSharedPreference(LogConstants.LOG_CHANNEL_INFO + "_" + name, ch.toString());
        return true;
    }


    public static void startLogPusherTimer() {
        ExecutorManager.runOnLogsPool(() -> {
            if (!LogConstants.shouldPush) {
                return;
            }
            try {
                if (0 == logPusherNumCounter.getAndIncrement()) {
                    stopPushingLogs = false;
                    LogSessioniser.startLogSessioniser();
                    logPushTimer = new Timer();
                    logPushTimerTask = new LogPushTimerTask();
                    logPushTimer.scheduleAtFixedRate(logPushTimerTask, LogConstants.logPostInterval, LogConstants.logPostInterval);
                }
            } catch (Exception exception) {
                // Ignore
            }
        });
    }

    public static void stopLogPusherOnTerminate() {
        ExecutorManager.runOnLogsPool(() -> {
            if (!LogConstants.shouldPush) {
                return;
            }
            if (logPusherNumCounter.decrementAndGet() <= 0) {
                logPusherNumCounter.set(0);
                try {
                    LogSessioniser.stopLogSessioniserOnTerminate();
                    logPushTimer.cancel();
                    logPushTimer = new Timer();
                    logPushTimerTask = new LogPushTimerTask();
                    logPushTimerTask.run();
                    stopPushingLogs = true;
                } catch (Exception exception) {
                    // Ignore
                }
            }
        });
    }

    public static void addLogsToPersistedQueue(JSONObject logToAdd) {
        ExecutorManager.runOnLogsPool(() -> {
            if (!LogConstants.shouldPush) {
                return;
            }
            try {
                if (JuspayCoreLib.getApplicationContext() != null) {
                    File crashFile = new File(JuspayCoreLib.getApplicationContext().getCacheDir(), LogConstants.CRASH_LOGS_FILE);
                    String toWrite = logToAdd.toString() + LogConstants.LOG_DELIMITER;
                    byte[] dataToAdd = toWrite.getBytes(StandardCharsets.UTF_8);
                    if (dataToAdd.length < LogConstants.maxLogLineSize) {
                        FileOutputStream fos = new FileOutputStream(crashFile, true);
                        fos.write(dataToAdd);
                        fos.close();
                    }
                }
            } catch (Exception ex) {
                JuspayLogger.e(TAG, "addLogsToPersistedQueue failed", ex);
            }
        });
    }

    private static LogChannel getChannelObject(String channelName) {
        if (channels.containsKey(channelName)) {
            return channels.get(channelName);
        }
        return channels.get(LogConstants.DEFAULT_CHANNEL);
    }

    private static boolean shouldDropLog(JSONObject log) {
        try {
            if (!log.has("channel")) {
                return false;
            }
            return !channels.containsKey(log.getString("channel"));
        } catch (JSONException e) {
            return false;
        }
    }


    private static JSONArray getLogChannels(JSONObject log) throws JSONException {
        JSONArray logChannels = new JSONArray();
        if (log.has("channels")) {
            logChannels = log.optJSONArray("channels");
        }
        if ((logChannels == null || logChannels.length() == 0) && log.has("channel")) {
            if (logChannels == null) {
                logChannels = new JSONArray();
            }
            logChannels.put(log.optString("channel", LogConstants.DEFAULT_CHANNEL));
        }
        if (logChannels == null || logChannels.length() == 0) {
            if (LogConstants.defaultChannels != null) {
                logChannels = new JSONArray(LogConstants.defaultChannels.toString());
            } else {
                logChannels = new JSONArray();
            }
            logChannels.put("default");
        }
        return logChannels;
    }

    static void addLogsFromSessioniser(final Queue<JSONObject> logsFromSessioniser) {
        ExecutorManager.runOnLogsPool(() -> {
            try {
                if (!LogConstants.shouldPush) {
                    return;
                }

                for (JSONObject log : logsFromSessioniser) {
                    if (shouldDropLog(log)) {
                        continue;
                    }
                    JSONArray logChannels = getLogChannels(log);

                    for (int i = 0; i < logChannels.length(); i++) {
                        String channelName = logChannels.getString(i);
                        if (!channels.containsKey(channelName)) {
                            continue;
                        }
                        LogChannel channel = getChannelObject(channelName);
                        byte[] byteArr = log.toString().getBytes(StandardCharsets.UTF_8);
                        if (byteArr.length <= LogConstants.maxLogLineSize) {
                            channel.addToLogsQueue(byteArr);
                        }
                    }

                }
            } catch (Exception ignored) {
            }
        });
    }

    static void addLogLines(JSONArray logs) {
        if (stopPushingLogs || !LogConstants.shouldPush) {
            return;
        }
        addLogLines(new IterableJSONArray(logs));
    }

    static void addLogLines(final Iterable<JSONObject> logs) {
        if (stopPushingLogs) {
            return;
        }

        ExecutorManager.runOnLogsPool(() -> {
            try {
                for (JSONObject log : logs) {
                    try {
                        if (shouldDropLog(log)) {
                            continue;
                        }
                        JSONArray logChannels = getLogChannels(log);

                        for (int i = 0; i < logChannels.length(); i++) {
                            String channelName = logChannels.getString(i);
                            if (!channels.containsKey(channelName)) {
                                continue;
                            }
                            LogChannel channel = getChannelObject(channelName);

                            int writingIndex = LogUtils.getFromSharedPreference(LogConstants.PERSISTENT_LOGS_WRITING_FILE + channelName);
                            if (writingIndex == -1) {
                                writingIndex = 0;
                            }

                            // Adding null check on log, since we have added a case where next() can return null
                            if (channel != null) {
                                String toWrite = log + LogConstants.LOG_DELIMITER;
                                byte[] logByteArr = toWrite.getBytes(StandardCharsets.UTF_8);
                                long fileSize = LogUtils.getFileLength(LogConstants.PERSISTENT_LOGS_FILE + channelName + writingIndex + LogConstants.PERSISTENT_LOGS_FILE_EXTENSION);
                                byte[] byteArr = log.toString().getBytes(StandardCharsets.UTF_8);
                                channel.addToLogsQueue(byteArr);
                                if (fileSize + logByteArr.length <= LogConstants.maxLogFileSize) {
                                    File logFile = LogUtils.getFile(LogConstants.PERSISTENT_LOGS_FILE + channelName + writingIndex + LogConstants.PERSISTENT_LOGS_FILE_EXTENSION);
                                    FileOutputStream fos = new FileOutputStream(logFile, true);
                                    fos.write(logByteArr);
                                    fos.close();
                                } else if (logByteArr.length <= LogConstants.maxLogLineSize) {
                                    LogUtils.writeToSharedPreference(LogConstants.PERSISTENT_LOGS_WRITING_FILE + channelName, String.valueOf(writingIndex));
                                    File logFile = LogUtils.getFile(LogConstants.PERSISTENT_LOGS_FILE + channelName + writingIndex + LogConstants.PERSISTENT_LOGS_FILE_EXTENSION);
                                    FileOutputStream fos = new FileOutputStream(logFile, true);
                                    fos.write(logByteArr);
                                    fos.close();
                                }
                            }
                        }
                    } catch (Exception e) {
                        // Ignore, if element is not a JSON Object. it is not a valid log
                        logFlushTimerTaskErrorCounter++;
                        if (logFlushTimerTaskErrorCounter <= 2) {
                            SdkTracker.trackAndLogBootException(TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.LOG_PUSHER, "Exception while flushing the logs to persisted queue file", e);
                        }
                    }
                }

            } catch (Exception e) {
                // Source queue file may throw IOException on calling next
                // Using the same counter as above to log error only twice
                logFlushTimerTaskErrorCounter++;
                if (logFlushTimerTaskErrorCounter <= 2) {
                    SdkTracker.trackAndLogBootException(TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.LOG_PUSHER, "Exception while flushing the logs to persisted queue file", e);
                }
            }
        });
    }

    /**
     * Helper method to fetch the logs to be pushed to the server. We iterate till the max logs threshold for the push
     * or till the end of the queue. Tries to fetch from the file first, and if not, then goes to the in memory queue.
     *
     * @return A {@link JSONArray} containing the logs that can be pushed to the server.
     */
    private static JSONArray getLogsToPush(LogChannel channel) {


        JSONArray logs = new JSONArray();
        Iterator<byte[]> queueIterator = channel.getLogsQueue().iterator();

        while ((channel.getBatchCount() == -1 || logs.length() < channel.getBatchCount()) && queueIterator.hasNext()) {
            try {
                JSONObject dataJson = new JSONObject(new String(queueIterator.next()));
                logs.put(dataJson);
            } catch (JSONException e) {

                queueIterator.remove();         // Remove the current log if it causes JsonException.
                getLogsToPushErrorCounter++;
                if (getLogsToPushErrorCounter <= 2) {
                    SdkTracker.trackAndLogBootException(TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.LOG_PUSHER, "Bad JSON while reading the Persisted Queue for Logs", e);
                } else {
                    JuspayLogger.e(TAG, "Bad JSON while reading the Persisted Queue for Logs", e);
                }
            }
        }
        return logs;
    }

    /**
     * Helper method to remove the logs from the queues once the push is successful. Will properly use in memory block
     * if in case creating the cache file failed.
     *
     * @param numLogsToRemove The number of logs that have been pushed from the queue.
     */
    private static void acknowledgeLogsPushed(int numLogsToRemove, LogChannel channel) {
        try {
            while (numLogsToRemove > 0) {
                channel.pollLogsQueue();
                numLogsToRemove--;
            }
        } catch (Exception e) {
            logPushTimerTaskErrorCounter++;

            if (logPushTimerTaskErrorCounter <= 2) {
                SdkTracker.trackAndLogBootException(TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.LOG_PUSHER, "Exception in removal of logs from persisted Queue file", e);
            } else {
                JuspayLogger.e(TAG, "Exception in removal of logs from persisted Queue file", e);
            }
        }
    }

    /**
     * Helper method to get the endpoint to push to. Ideally this should come from the {@link SdkTracker} and be
     * written to cache along with each log line. We keep a single backend entry point for all log lines instead.
     *
     * @return The HTTP endpoint where we will push the logs to.
     */
    private static String getEndPoint(LogChannel channel) {
        return isSandboxEnv ? channel.getEndpointSBX() : channel.getEndPointProd();
    }

    /**
     * Helper method to get the log encryption level. Defaults to "gzip".
     *
     * @return The log encryption method.
     */
    private static String getLogEncryptionLevel(LogChannel channel) {
        return channel.getEncryptionLevel();
    }

    /**
     * Helper method to get the public key from config. The config has the public key in form of JWK.
     *
     * @return The public key or null if not found.
     */
    @Nullable
    private static RSAPublicKey getLogEncryptionKey(LogChannel channel) {

        JSONObject k = isSandboxEnv ? channel.getKeySBX() : channel.getKeyProd();
        try {
            return JOSEUtils.JWKtoRSAPublicKey(k);
        } catch (Exception e) {
            return null;
        }

    }

    public static void setEndPointSandbox(Boolean isSandbox) {
        isSandboxEnv = isSandbox;
    }

    private static void pushAllFiles(int readingIndex, int writingIndex, LogChannel channel) {

        for (int i = readingIndex; i <= writingIndex; i++) {
            File logFile = LogUtils.getFile(LogConstants.PERSISTENT_LOGS_FILE + channel.getChannelName() + i + LogConstants.PERSISTENT_LOGS_FILE_EXTENSION);
            if (logFile != null) {
                if (logFile.exists() && LogUtils.isFileEligibleToPush(logFile)) {
                    pushFileContentToServer(logFile, channel);
                } else {
                    //noinspection ResultOfMethodCallIgnored
                    logFile.delete();
                }
            }
        }

        if (JuspayCoreLib.getApplicationContext() != null) {
            LogChannel defaultChannel = getChannelObject(LogConstants.DEFAULT_CHANNEL);
            File crashLogFile = new File(JuspayCoreLib.getApplicationContext().getCacheDir(), LogConstants.CRASH_LOGS_FILE);
            // Logic if file is modified less than 7 days
            if (crashLogFile.exists() && LogUtils.isFileEligibleToPush(crashLogFile)) {
                pushFileContentToServer(crashLogFile, defaultChannel);
            } else {
                //noinspection ResultOfMethodCallIgnored
                crashLogFile.delete();
            }
        }
    }

    private static void pushFileContentToServer(final File logFile, LogChannel channel) {
        if (logFile != null) {

            Triple<Queue<JSONObject>,Integer,Integer> logsPair = LogUtils.getLogsFromFileInBatch(logFile,0);
            while (logsPair.getThird() <= logsPair.getSecond()) {
                try {
                    boolean result = true;
                    JSONArray logsToPush = new JSONArray();
                    Queue<JSONObject> logs = logsPair.getFirst();
                    while (logs.size() > 0) {
                        while ((channel.getBatchCount() == -1 || logsToPush.length() < channel.getBatchCount()) && logs.size() > 0) {
                            logsToPush.put(logs.poll());
                        }
                        int responseCode = pushLogsToServer(logsToPush, channel);
                        if (responseCode != 200) {
                            result = false;
                        }
                        logsToPush = new JSONArray();
                    }

                    if (result) {
                        //noinspection ResultOfMethodCallIgnored
                        logFile.delete();
                    }
                    if (Objects.equals(logsPair.getThird(), logsPair.getSecond())) break;
                    logsPair = LogUtils.getLogsFromFileInBatch(logFile,logsPair.getThird());
                } catch (Exception ignored) {

                }
            }
        }
    }

    public static void setHeaders(JSONObject headers, String channelName) {

        ExecutorManager.runOnLogsPool(() -> {
            LogChannel channel = getChannelObject(channelName);
            for (Iterator<String> it = headers.keys(); it.hasNext(); ) {
                String key = it.next();
                try {
                    channel.getHeaders().put(key, headers.getString(key));
                } catch (JSONException e) {
                    // Skip headers that are not strings
                }
            }
        });
    }

    public static void setLogHeaderValues(JSONObject logHeaderValues, String channelName) {
        ExecutorManager.runOnLogsPool(() -> {
            LogChannel channel = getChannelObject(channelName);
            JSONObject logHeaderProperties = LogConstants.logHeaders;
            Iterator<String> keys = logHeaderProperties.keys();

            try {
                while (keys.hasNext()) {
                    String key = keys.next();
                    String value = logHeaderProperties.optString(key);
                    int dollarIdx = value.indexOf('$');
                    int startIdx = value.indexOf('{');
                    int lastIdx = value.lastIndexOf('}');
                    if (dollarIdx != -1) {
                        if (startIdx != -1 && lastIdx != -1 && startIdx - dollarIdx == 1 && startIdx < lastIdx) {
                            String prop = value.substring(startIdx + 1, lastIdx);
                            String toReplace = "${" + prop + "}";
                            if (!logHeaderValues.has(prop)) {
                                continue;
                            }
                            value = value.replace(toReplace, logHeaderValues.optString(prop));
                        }
                    }
                    channel.getHeaders().put(key, value);
                }
            } catch (Exception exception) {
                setHeaderParametersErrorCounter++;
                if (setHeaderParametersErrorCounter <= 2) {
                    SdkTracker.trackBootAction(LogSubCategory.LifeCycle.HYPER_SDK, LogLevel.WARNING, Labels.System.LOG_PUSHER, "sdk_config", "Unable log header properties in log headers");
                }
            }
        });
    }

    // Keep this as private or call this only from LogsThread.
    private static int pushLogsToServer(JSONArray logs, LogChannel channel) throws Exception {

        JSONObject logJson = new JSONObject();
        logJson.put("data", logs);
        byte[] logBytes = logJson.toString().getBytes(StandardCharsets.UTF_8);

        String logEncryptionLevel = getLogEncryptionLevel(channel);
        RSAPublicKey logEncryptionKey = getLogEncryptionKey(channel);
        JuspayHttpsResponse response;

        NetUtils netUtils = new NetUtils(10000, 10000);
        Map<String, String> headers = channel.getHeaders();
        headers.put("x-logscount", String.valueOf(logs.length()));
        headers.put("channel", channel.getChannelName());
        // Build version check is there because the current implementation of JWE
        // does not support lower versions.
        if ("encryption".equals(logEncryptionLevel) && logEncryptionKey != null) {
            byte[] data = EncryptionHelper.gzipThenEncrypt(logBytes, logEncryptionKey);
            response = new JuspayHttpsResponse(netUtils.doPost(new URL(getEndPoint(channel)), data, "application/x-godel-gzip-pubkey-encrypted", headers, new JSONObject(), null));
        } else if ("gzip".equals(logEncryptionLevel)) {
            byte[] data = Utils.gzipContent(logBytes);
            headers.put("Content-Encoding", "gzip");
            response = new JuspayHttpsResponse(netUtils.doPost(new URL(getEndPoint(channel)), data, "application/gzip", headers, new JSONObject(), null));
        } else { // plain
            response = new JuspayHttpsResponse(netUtils.doPost(new URL(getEndPoint(channel)), logBytes, "application/json", headers, new JSONObject(), null));
        }

        return response.responseCode;
    }

    /**
     * Helper utility class to provide an iterable for a JSONArray
     * while iterating over an iterable.
     */
    static class IterableJSONArray implements Iterable<JSONObject> {

        JSONArray original;

        IterableJSONArray(JSONArray array) {
            original = array;
        }

        @NonNull
        @Override
        public Iterator<JSONObject> iterator() {
            return new Iterator<JSONObject>() {
                int curr = 0;

                @Override
                public boolean hasNext() {
                    return curr < IterableJSONArray.this.original.length();
                }

                @Override
                @Nullable
                public JSONObject next() {
                    try {
                        return IterableJSONArray.this.original.optJSONObject(curr++);
                    } catch (ArrayIndexOutOfBoundsException e) {
                        // Returning null in case where we get ArrayIndexOutOfBoundsException
                        // Array Index out of bounds is thrown in some versions of android
                        return null;
                    }
                }

                @Override
                public void remove() {
                    // not needed for now
                }
            };
        }
    }

    /**
     * Class for the timer task that attempts to push every 10 seconds.
     *
     * @author Sri Harsha Chilakapati [sri.harsha@juspay.in]
     * @author Parth Vora [parth.vora@juspay.in]
     * @since 19/02/2020
     */
    private static class LogPushTimerTask extends TimerTask {
        private static final String TAG = "LogPushTimerTask";
        private static boolean isExceptionTracked = false;

        @Override
        public void run() {
            ExecutorManager.runOnLogsPool(() -> {
                if (!LogConstants.shouldPush || !LogUtils.isMinMemoryAvailable()) {
                    return;
                }
                int iteration = logPushIteration.getAndIncrement();


                for (Map.Entry<String, LogChannel> entry : channels.entrySet()) {
                    LogChannel channel = entry.getValue();
                    String channelName = entry.getKey();

                    if (iteration % channel.getPriority() != 0) {
                        continue;
                    }

                    JSONArray logs;

                    int readingIndex = LogUtils.getFromSharedPreference(LogConstants.PERSISTENT_LOGS_READING_FILE + channelName);
                    int writingIndex = LogUtils.getFromSharedPreference(LogConstants.PERSISTENT_LOGS_WRITING_FILE + channelName);

                    if (JuspayCoreLib.getApplicationContext() != null && writingIndex - readingIndex + 1 > LogConstants.maxFilesAllowed) {
                        while (writingIndex - readingIndex + 1 > LogConstants.numFilesToLeaveIfMaxFilesExceeded) {
                            File logFile = LogUtils.getFile(LogConstants.PERSISTENT_LOGS_FILE + channelName + readingIndex + LogConstants.PERSISTENT_LOGS_FILE_EXTENSION);
                            try {
                                if (logFile != null) {
                                    //noinspection ResultOfMethodCallIgnored
                                    logFile.delete();
                                }
                            } catch (Exception exception) {
                                // Ignore
                            }
                            readingIndex++;
                        }
                        LogUtils.writeToSharedPreference(LogConstants.PERSISTENT_LOGS_READING_FILE + channelName, String.valueOf(readingIndex));
                    }

                    try {
                        boolean shouldContinue = false;
                        while (channel.getLogsQueue().size() > 0) {
                            logs = getLogsToPush(channel);
                            if (logs.length() > 0) {
                                int responseCode = pushLogsToServer(logs, channel);
                                if (responseCode != 200 && (channel.getRetryAttempts() == -1 || channel.getCurrentBatchRetryAttempts() < channel.getRetryAttempts())) {
                                    SdkTracker.trackBootAction(LogSubCategory.Action.SYSTEM, LogLevel.ERROR, Labels.System.LOG_PUSHER, "error_response", "" + responseCode);
                                    channel.setCurrentBatchRetryAttempts(channel.getCurrentBatchRetryAttempts() + 1);
                                    shouldContinue = true;
                                    break;
                                }
                                channel.setCurrentBatchRetryAttempts(0);
                                acknowledgeLogsPushed(logs.length(), channel);
                            }
                        }
                        if (shouldContinue) {
                            continue;
                        }
                    } catch (Exception e) {
                        if (!LogPushTimerTask.isExceptionTracked)
                            SdkTracker.trackAndLogBootException(TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.LOG_PUSHER, "Error while creating the payload to post", e);
                        LogPushTimerTask.isExceptionTracked = true;
                    }

                    readingIndex = LogUtils.getFromSharedPreference(LogConstants.PERSISTENT_LOGS_READING_FILE + channelName);
                    writingIndex = LogUtils.getFromSharedPreference(LogConstants.PERSISTENT_LOGS_WRITING_FILE + channelName);

                    if (JuspayCoreLib.getApplicationContext() != null) {
                        for (int index = readingIndex; index <= writingIndex; index++) {
                            File logFile = new File(JuspayCoreLib.getApplicationContext().getCacheDir(), LogConstants.PERSISTENT_LOGS_FILE + channelName + index + LogConstants.PERSISTENT_LOGS_FILE_EXTENSION);
                            //noinspection ResultOfMethodCallIgnored
                            logFile.delete();
                        }
                    }

                    LogUtils.writeToSharedPreference(LogConstants.PERSISTENT_LOGS_READING_FILE + channelName, String.valueOf(0));
                    LogUtils.writeToSharedPreference(LogConstants.PERSISTENT_LOGS_WRITING_FILE + channelName, String.valueOf(0));
                }
            });

        }
    }
}
