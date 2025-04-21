package in.juspay.hypersdk.analytics;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.util.Pair;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.InputStream;
import java.net.URL;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.security.interfaces.RSAPublicKey;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Objects;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

import in.juspay.hyper.constants.Labels;
import in.juspay.hyper.constants.LogCategory;
import in.juspay.hyper.constants.LogLevel;
import in.juspay.hyper.constants.LogSubCategory;
import in.juspay.hyper.core.ExecutorManager;
import in.juspay.hyper.core.JuspayCoreLib;
import in.juspay.hyper.core.JuspayLogger;
import in.juspay.hypersdk.core.SdkTracker;
import in.juspay.hypersdk.security.EncryptionHelper;
import in.juspay.hypersdk.security.JOSEUtils;
import in.juspay.hypersdk.utils.Utils;
import in.juspay.hypersdk.utils.network.JuspayHttpsResponse;
import in.juspay.hypersdk.utils.network.NetUtils;

import okhttp3.Response;

/**
 * A class that does takes care of pushing the logs that are tracked. This class also has disk-caching to avoid
 * the logs from missing.
 *
 * @author Sri Harsha Chilakapati [sri.harsha@juspay.in]
 * @author Parth Vora [parth.vora@juspay.in]
 * @since 14/02/2020
 */

// All public methods will run on Background Thread, enforcing private methods automatically to be in LogsThread.
public class LogPusherExp {
    private static final String TAG = "LogPusher";
    @NonNull
    final private static ConcurrentHashMap<String, LogChannelExp> channels = new ConcurrentHashMap<>();
    @NonNull
    private static final AtomicInteger logPusherNumCounter = new AtomicInteger(0);
    @Nullable
    private static TimerTask logPushTimerTask;
    private static int logFlushTimerTaskErrorCounter = 0;
    private static int setHeaderParametersErrorCounter = 0;
    private static boolean isSandboxEnv = false;
    @NonNull
    private static Timer logPushTimer = new Timer();
    @NonNull
    private static final ConcurrentHashMap<String, Integer> fileCountMap = new ConcurrentHashMap<>();

    static {

        if (LogConstants.shouldPush) {

            File tempFolder = LogUtils.getFileExp("temp/");
            if (tempFolder != null && tempFolder.exists()) {
                File[] files = tempFolder.listFiles();
                File pushFile = LogUtils.getFileExp("temp/push.json");

                if (pushFile != null && pushFile.exists()) {
                    try (FileInputStream fis = new FileInputStream(pushFile)) {
                        byte[] arr = new byte[(int) pushFile.length()];
                        //noinspection ResultOfMethodCallIgnored
                        fis.read(arr);
                        JSONObject json = new JSONObject(new String(arr));
                        for (Iterator<String> it = json.keys(); it.hasNext(); ) {
                            String fileName = it.next();
                            File tempFile = LogUtils.getFileExp("temp/" + fileName);
                            if (tempFile != null && tempFile.exists()) {
                                if (tempFile.length() > 0 && LogUtils.isFileEligibleToPush(tempFile)) {
                                    File toRename = LogUtils.getFileExp(fileName);
                                    if (toRename != null) {
                                        //noinspection ResultOfMethodCallIgnored
                                        tempFile.renameTo(toRename);
                                    }
                                } else {
                                    //noinspection ResultOfMethodCallIgnored
                                    tempFile.delete();
                                }
                            }
                        }
                    } catch (Exception e) {
                        if (files != null) {
                            for (File file : files) {
                                String fileName = file.getName();
                                if (!"push.json".equals(fileName)) {
                                    File tempFile = LogUtils.getFileExp("temp/" + fileName);
                                    if (tempFile != null && tempFile.exists()) {
                                        if (tempFile.length() > 0 && LogUtils.isFileEligibleToPush(tempFile)) {
                                            File toRename = LogUtils.getFileExp(fileName);
                                            if (toRename != null) {
                                                //noinspection ResultOfMethodCallIgnored
                                                tempFile.renameTo(toRename);
                                            }
                                        } else {
                                            //noinspection ResultOfMethodCallIgnored
                                            tempFile.delete();
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                files = tempFolder.listFiles();
                if (files != null) {
                    for (File file : files) {
                        if (file != null && file.exists()) {
                            //noinspection ResultOfMethodCallIgnored
                            file.delete();
                        }
                    }
                }
            }

            File originalFolder = LogUtils.getFileExp("original/");
            if (originalFolder != null && originalFolder.exists()) {
                File[] files = originalFolder.listFiles();
                if (files != null) {
                    for (File file : files) {
                        if (file.exists() && file.isFile()) {
                            if (file.length() > 0 && LogUtils.isFileEligibleToPush(file)) {
                                String fileName = file.getName();
                                File toRename = LogUtils.getFileExp(fileName);
                                if (toRename != null) {
                                    //noinspection ResultOfMethodCallIgnored
                                    file.renameTo(toRename);
                                }
                            } else {
                                //noinspection ResultOfMethodCallIgnored
                                file.delete();
                            }
                        }
                    }
                }
            }

            String[] previousChannels = LogUtils.getAnyFromSharedPreference(LogConstants.LOG_CHANNEL_NAMES, "").split(",");
            LogUtils.writeToSharedPreference(LogConstants.LOG_CHANNEL_NAMES, "");
            addChannel(LogConstants.DEFAULT_CHANNEL, LogConstants.maxRetryPerBatch, LogConstants.maxLogsPerPush, LogConstants.prodLogUrl, LogConstants.sandboxLogUrl, LogConstants.publicKey, LogConstants.publicKeySandbox, new HashMap<>(), LogConstants.defaultPriority, "all", LogConstants.encryptionLevel, LogConstants.fallBackUrl, LogConstants.fallBackPublicKeys, LogConstants.errorUrl);
            ExecutorManager.runOnLogPusherThread(LogPusherExp::pushCrashLogFile);
            LogChannelExp defaultChannel = getChannelObject(LogConstants.DEFAULT_CHANNEL);
            for (String channelName : previousChannels) {
                String chInfo = LogUtils.getAnyFromSharedPreference(LogConstants.LOG_CHANNEL_INFO + "_" + channelName, "{}");
                LogChannelExp channel = null;
                if (!chInfo.isEmpty()) {
                    try {
                        JSONObject channelInfo = new JSONObject(chInfo);
                        int retryAttempts = channelInfo.optInt("retryAttempts", LogConstants.maxRetryPerBatch);
                        long batchCount = channelInfo.optLong("batchCount", LogConstants.maxLogsPerPush);
                        String endPointProd = channelInfo.optString("endPointProd", LogConstants.prodLogUrl);
                        String endpointSBX = channelInfo.optString("endpointSBX", LogConstants.sandboxLogUrl);
                        JSONObject keyProd = channelInfo.getJSONObject("keyProd");
                        JSONObject keySBX = channelInfo.getJSONObject("keySBX");
                        Map<String, String> channelHeaders = LogUtils.toMap(new JSONObject(channelInfo.getString("headers")));
                        int priority = channelInfo.optInt("priority", LogConstants.defaultPriority);
                        String environment = channelInfo.optString("environment", "all");
                        String encryptionLevel = channelInfo.optString("encryptionLevel", LogConstants.encryptionLevel);
                        JSONArray fallBackUrls = channelInfo.optJSONArray("fallBackUrls") == null ? LogConstants.fallBackUrl : channelInfo.optJSONArray("fallBackUrls");
                        JSONArray fallBackPublicKeys = channelInfo.optJSONArray("fallBackPublicKeys") == null ? LogConstants.fallBackPublicKeys : channelInfo.optJSONArray("fallBackPublicKeys");
                        String errorUrl = channelInfo.optString("errorUrl", LogConstants.errorUrl);
                        channel = makeChannel(channelName, retryAttempts, batchCount, endPointProd, endpointSBX, keyProd, keySBX, channelHeaders, priority, environment, encryptionLevel, fallBackUrls, fallBackPublicKeys, errorUrl);
                    } catch (JSONException ignore) {
                    }
                }
                if (channel == null) {
                    channel = makeChannel(channelName, LogConstants.maxRetryPerBatch, LogConstants.maxLogsPerPush, LogConstants.prodLogUrl, LogConstants.sandboxLogUrl, defaultChannel.getKeyProd(), defaultChannel.getKeySBX(), defaultChannel.getHeaders(), 1, "all", LogConstants.encryptionLevel, LogConstants.fallBackUrl, LogConstants.fallBackPublicKeys, LogConstants.errorUrl);
                }
                final LogChannelExp channel1 = channel;
                ExecutorManager.runOnLogPusherThread(() -> {
                    pushChannelFiles(channelName, channel1);
                    LogUtils.removeFromSharedPreference(LogConstants.LOG_CHANNEL_INFO + "_" + channelName);
                });
            }

            if (channels.containsKey(LogConstants.DEFAULT_CHANNEL)) {
                ExecutorManager.runOnLogPusherThread(() -> pushChannelFiles(LogConstants.DEFAULT_CHANNEL, channels.get(LogConstants.DEFAULT_CHANNEL)));
            }
        }
    }

    static void startLogPusher() {
        if (!LogConstants.shouldPush) {
            return;
        }
        try {
            if (0 == logPusherNumCounter.getAndIncrement()) {
                logPushTimer = new Timer();
                logPushTimerTask = new LogPushTimerTask();
                logPushTimer.schedule(logPushTimerTask, 0, LogConstants.logPostInterval);
            }
        } catch (Exception ignored) {
        }
    }

    static void stopLogPusherOnTerminate() {
        if (!LogConstants.shouldPush) {
            return;
        }
        if (logPusherNumCounter.decrementAndGet() <= 0) {
            logPusherNumCounter.set(0);
            try {
                logPushTimer.cancel();
                logPushTimer = new Timer();
                logPushTimerTask = new LogPushTimerTask();
                logPushTimerTask.run();
            } catch (Exception ignored) {
            }
        }
    }

    public static String[] getChannelNames() {
        return channels.keySet().toArray(new String[0]);
    }

    public static boolean addChannelFromJS(String jsonObj, String channelName) {
        try {
            JSONObject channelInfo = new JSONObject(jsonObj);
            int retryAttempts = channelInfo.optInt("retryAttempts", LogConstants.maxRetryPerBatch);
            long batchCount = channelInfo.optLong("batchCount", LogConstants.maxLogsPerPush);
            String endPointProd = channelInfo.optString("endPointProd", LogConstants.prodLogUrl);
            String endpointSBX = channelInfo.optString("endpointSBX", LogConstants.sandboxLogUrl);
            JSONObject keyProd = channelInfo.has("keyProd") ? channelInfo.getJSONObject("keyProd") : LogConstants.publicKey;
            JSONObject keySBX = channelInfo.has("keySBX") ? channelInfo.getJSONObject("keySBX") : LogConstants.publicKeySandbox;
            Map<String, String> channelHeaders = channelInfo.has("channelHeaders") ? LogUtils.toMap(channelInfo.getJSONObject("channelHeaders")) : new HashMap<>();
            int priority = channelInfo.optInt("priority", LogConstants.defaultPriority);
            String environment = channelInfo.optString("environment", "all");
            String encryptionLevel = channelInfo.optString("encryptionLevel", LogConstants.encryptionLevel);
            JSONArray fallBackUrls = channelInfo.has("fallBackUrls") ? channelInfo.optJSONArray("fallBackUrls") : LogConstants.fallBackUrl;
            JSONArray fallBackPublicKeys = channelInfo.has("fallBackPublicKeys") ? channelInfo.optJSONArray("fallBackPublicKeys") : LogConstants.fallBackPublicKeys;
            String errorUrl = channelInfo.optString("errorUrl", LogConstants.errorUrl);
            return addChannel(channelName, retryAttempts, batchCount, endPointProd, endpointSBX, keyProd, keySBX, channelHeaders, priority, environment, encryptionLevel, fallBackUrls, fallBackPublicKeys, errorUrl);
        } catch (JSONException ignore) {
        }
        return false;
    }

    private static LogChannelExp makeChannel(String name, int retryAttempts, long batchCount,
                                             String endPointProd, String endpointSBX,
                                             JSONObject keyProd, JSONObject keySBX, Map<String, String> channelHeaders, int priority,
                                             String environment, String encryptionLevel, JSONArray fallBackUrls, JSONArray fallBackPublicKeys, String errorUrl) {
        return new LogChannelExp(retryAttempts, batchCount, name, endPointProd, endpointSBX, keyProd, keySBX, channelHeaders, priority, environment, encryptionLevel, fallBackUrls, fallBackPublicKeys, errorUrl);
    }

    private static boolean addChannel(String name, int retryAttempts, long batchCount,
                                      String endPointProd, String endpointSBX,
                                      JSONObject keyProd, JSONObject keySBX, Map<String, String> channelHeaders, int priority,
                                      String environment, String encryptionLevel, JSONArray fallBackUrls, JSONArray fallBackPublicKeys, String errorUrl) {
        boolean alreadyExists = channels.containsKey(name);

        LogChannelExp ch = makeChannel(name, retryAttempts, batchCount, endPointProd, endpointSBX, keyProd, keySBX, channelHeaders, priority, environment, encryptionLevel, fallBackUrls, fallBackPublicKeys, errorUrl);
        channels.put(name, ch);

        if (!alreadyExists) {
            String savedChannels = LogUtils.getAnyFromSharedPreference(LogConstants.LOG_CHANNEL_NAMES, "");
            String updatedChannels = savedChannels + (savedChannels.isEmpty() ? "" : ",") + name;
            LogUtils.writeToSharedPreference(LogConstants.LOG_CHANNEL_NAMES, updatedChannels);
        }
        LogUtils.writeToSharedPreference(LogConstants.LOG_CHANNEL_INFO + "_" + name, ch.toString());
        return true;
    }

    public static void addLogsToPersistedQueue(JSONObject logToAdd) {
        ExecutorManager.runOnLogSessioniserThread(() -> {
            if (!LogConstants.shouldPush) {
                return;
            }
            try {
                File crashFile = LogUtils.getFileExp(LogConstants.CRASH_LOGS_FILE);
                LogUtils.writeLogToFileExp(logToAdd, crashFile);
            } catch (Exception ex) {
                JuspayLogger.e(TAG, "addLogsToPersistedQueue failed", ex);
            }
        });
    }

    private static LogChannelExp getChannelObject(String channelName) {
        if (channels.containsKey(channelName)) {
            return channels.get(channelName);
        }
        return channels.get(LogConstants.DEFAULT_CHANNEL);
    }

    static void addLogLines(final String channelName, final String fileName) {
        ExecutorManager.runOnLogPusherThread(() -> {
            if (!LogConstants.shouldPush) {
                return;
            }

            try {
                LogChannelExp channel = getChannelObject(channelName);
                if (channel == null) {
                    channel = getChannelObject(LogConstants.DEFAULT_CHANNEL);
                }

                File file = LogUtils.getFileExp(fileName);
                if (file != null && file.exists() && file.length() > 0) {
                    channel.addToLogsQueue(fileName);   // Check channel not created in this case.
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
     * Helper method to remove the logs from the queues once the push is successful. Will properly use in memory block
     * if in case creating the cache file failed.
     */
    private static void acknowledgeLogsPushed(ArrayList<String> list, LogChannelExp channel) {
        for (String fileName : list) {
            channel.pollLogsQueue(fileName);
        }
    }

    /**
     * Helper method to get the endpoint to push to. Ideally this should come from the {@link SdkTracker} and be
     * written to cache along with each log line. We keep a single backend entry point for all log lines instead.
     *
     * @return The HTTP endpoint where we will push the logs to.
     */
    private static String getEndPoint(LogChannelExp channel) {
        return isSandboxEnv ? channel.getEndpointSBX() : channel.getEndPointProd();
    }

    /**
     * Helper method to get the log encryption level. Defaults to "gzip".
     *
     * @return The log encryption method.
     */
    private static String getLogEncryptionLevel(LogChannelExp channel) {
        return channel.getEncryptionLevel();
    }

    /**
     * Helper method to get the public key from config. The config has the public key in form of JWK.
     *
     * @return The public key or null if not found.
     */
    @Nullable
    private static RSAPublicKey getLogEncryptionKey(LogChannelExp channel, int index) {

        JSONObject k = null;
        if (channel.getFallBackPublicKeys().length() != 0) {
            try {
                if (index >= 0) {
                    k = (JSONObject) channel.getFallBackPublicKeys().get(index);
                }
            } catch (Exception ignored) {
            }
        }

        if (k == null ) {
            k = isSandboxEnv ? channel.getKeySBX() : channel.getKeyProd();
        }

        try {
            return JOSEUtils.JWKtoRSAPublicKey(k);
        } catch (Exception e) {
            return null;
        }

    }

    public static void setEndPointSandbox(Boolean isSandbox) {
        isSandboxEnv = isSandbox;
    }

    private static void pushCrashLogFile() {
        if (JuspayCoreLib.getApplicationContext() != null) {
            LogChannelExp defaultChannel = getChannelObject(LogConstants.DEFAULT_CHANNEL);
            File crashLogFile = LogUtils.getFileExp(LogConstants.CRASH_LOGS_FILE);
            if (crashLogFile != null && crashLogFile.exists()) {
                if (crashLogFile.length() > 0 && LogUtils.isFileEligibleToPush(crashLogFile)) {
                    pushFileContentToServer(crashLogFile, defaultChannel);
                } else {
                    //noinspection ResultOfMethodCallIgnored
                    crashLogFile.delete();
                }
            }
        }
    }

    private static void pushFileContentToServer(final File logFile, LogChannelExp channel) {
        byte[] logsArr = LogUtils.getLogsFromFileExp(logFile);
        int logCount = getFileCount(logFile.getName(), logFile);
        try {
            int responseCode = pushLogsToServer(logsArr, logCount, new JSONArray().put(1), channel, false);
            if (responseCode == 200) {
                //noinspection ResultOfMethodCallIgnored
                logFile.delete();
            }
        } catch (Exception ignored) {
        }
    }

    public static void setHeaders(JSONObject headers, String channelName) {
        LogChannelExp channel = getChannelObject(channelName);
        for (Iterator<String> it = headers.keys(); it.hasNext(); ) {
            String key = it.next();
            try {
                channel.getHeaders().put(key, headers.getString(key));
            } catch (JSONException e) {
                // Skip headers that are not strings
            }
        }
    }

    public static void setLogHeaderValues(JSONObject logHeaderValues, String channelName) {
        LogChannelExp channel = getChannelObject(channelName);
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
    }

    // Keep this as private or call this only from LogsThread.
    private static int pushLogsToServer(byte[] logsArr, int logCount, JSONArray batchNum, LogChannelExp channel, boolean isNdJson) throws Exception {

        String logEncryptionLevel = getLogEncryptionLevel(channel);
        RSAPublicKey logEncryptionKey = getLogEncryptionKey(channel, channel.getRetryAttempts()-1);
        JuspayHttpsResponse response;

        NetUtils netUtils = new NetUtils(10000, 10000);
        Map<String, String> headers = channel.getHeaders();
        headers.put("x-logscount", String.valueOf(logCount));
        headers.put("channel", channel.getChannelName());
        headers.put("x-log-format", isNdJson ? "ndjson" : "byte-d-json");
        headers.put("x-batch-no", batchNum.toString());
        String endPoint = getEndPoint(channel);
        JSONArray fallBackUrls = channel.getFallBackUrls();
        if (fallBackUrls.length() != 0) {
            int attempts = channel.getCurrentBatchRetryAttempts();
            if (attempts > 0) {
                endPoint = fallBackUrls.getString((attempts - 1) % fallBackUrls.length());
            }
        }
        // Build version check is there because the current implementation of JWE
        // does not support lower versions.
        if ("encryption".equals(logEncryptionLevel) && logEncryptionKey != null) {
            byte[] data = EncryptionHelper.gzipThenEncryptExp(logsArr, logEncryptionKey, headers);
            response = new JuspayHttpsResponse(netUtils.doPost(new URL(endPoint), data, "application/x-godel-gzip-pubkey-encrypted", headers, new JSONObject(), null));
        } else if ("gzip".equals(logEncryptionLevel)) {
            byte[] data = Utils.gzipContent(logsArr);
            headers.put("Content-Encoding", "gzip");
            response = new JuspayHttpsResponse(netUtils.doPost(new URL(endPoint), data, "application/gzip", headers, new JSONObject(), null));
        } else { // plain
            response = new JuspayHttpsResponse(netUtils.doPost(new URL(endPoint), logsArr, "application/json", headers, new JSONObject(), null));
        }

        return response.responseCode;
    }

    private static Pair<byte[], Integer> getFilesContent(ArrayList<String> list) {
        int totalLength = 0, totalCount = 0;
        ArrayList<byte[]> byteList = new ArrayList<>();
        for (String fileName : list) {
            File file = LogUtils.getFileExp(fileName);
            if (file != null && file.exists() && file.length() > 0) {
                byte[] arr = LogUtils.getLogsFromFileExp(file);
                totalLength += arr.length;
                byteList.add(arr);
                totalCount += getFileCount(fileName, file);
            }
        }
        ByteBuffer buffer = ByteBuffer.allocate(totalLength);
        for (byte[] byteArr : byteList) {
            buffer.put(byteArr);
        }
        return Pair.create(buffer.array(), totalCount);
    }

    private static int getFileCount(String fileName, File file) {
        if (fileCountMap.containsKey(fileName)) {
            Integer count = fileCountMap.get(fileName);
            if (count != null) {
                return count;
            }
        }
        int lastIdx = fileName.lastIndexOf('.');
        int res;
        if (fileName.charAt(lastIdx - 5) == '-') {
            String count = fileName.substring(lastIdx - 4, lastIdx);
            res = Integer.parseInt(count);
        } else {
            res = traverseTheFile(fileName, file);
        }
        fileCountMap.put(fileName, res);
        return res;
    }

    static int traverseTheFile(String fileName, File file) {
        if (fileName.contains(".dat")) {
            long fileLength = file.length();
            int index = 0, count = 0;
            try (FileInputStream fis = new FileInputStream(file)) {
                while (index < fileLength) {
                    byte[] num = new byte[4];
                    //noinspection ResultOfMethodCallIgnored
                    fis.read(num);
                    ByteBuffer buffer = ByteBuffer.allocate(4);
                    buffer.put(num);
                    buffer.rewind();
                    int value = buffer.getInt();
                    if (value > 100 * 1024) {
                        return count;
                    }
                    long res = fis.skip(value);
                    if (res < value) {
                        return count;
                    }
                    index += value + 4;
                    count++;
                }
            } catch (Exception ignored) {
            }
            return count;
        } else if (fileName.contains(".ndjson")) {
            int lines = 0;
            try (InputStream is = new BufferedInputStream(new FileInputStream(LogUtils.getFileExp(fileName)))) {
                byte[] c = new byte[1024];
                int count = 0;
                int readChars;
                boolean endsWithoutNewLine = false;
                while ((readChars = is.read(c)) != -1) {
                    for (int i = 0; i < readChars; ++i) {
                        if (c[i] == '\n')
                            ++count;
                    }
                    endsWithoutNewLine = (c[readChars - 1] != '\n');
                }
                if (endsWithoutNewLine) {
                    ++count;
                }
                lines = count;
            } catch (Exception ignored) {
            }
            return lines;
        }
        return 1;
    }

    private static JSONArray getBatchNumArray(ArrayList<String> list) {
        JSONArray batchNum = new JSONArray();
        for (String fileName : list) {
            batchNum.put(getBatchNum(fileName));
        }
        return batchNum;
    }

    static int getBatchNum(String fileName) {
        int lastIdx = fileName.lastIndexOf('.');
        String count;
        if (fileName.charAt(lastIdx - 5) == '-') {
            count = fileName.substring(lastIdx - 8, lastIdx - 5);
        } else {
            count = fileName.substring(lastIdx - 3, lastIdx);
        }
        return Integer.parseInt(count);
    }

    private static JSONObject getFirstLog(String fileName) {
        JSONObject json = new JSONObject();
        if (fileName.contains(".ndjson")) {
            File file = LogUtils.getFileExp(fileName);
            try (BufferedReader bufferedReader = new BufferedReader(new FileReader(file))) {
                return new JSONObject(bufferedReader.readLine());
            } catch (Exception ignored) {
            }
        } else if (fileName.contains(".dat")) {
            byte[] arr = new byte[4];
            File file = LogUtils.getFileExp(fileName);
            if (file != null && file.exists()) {
                try (FileInputStream fis = new FileInputStream(file)) {
                    //noinspection ResultOfMethodCallIgnored
                    fis.read(arr);
                    ByteBuffer buffer = ByteBuffer.allocate(4);
                    buffer.put(arr);
                    buffer.rewind();
                    int length = buffer.getInt();
                    if (length < 100 * 1024) {
                        arr = new byte[length];
                        //noinspection ResultOfMethodCallIgnored
                        fis.read(arr);
                        return new JSONObject(new String(arr, StandardCharsets.UTF_8));
                    }
                } catch (Exception ignored) {
                }
            }
        }
        return json;
    }

    private static void hitErrorUrl(String fileName, LogChannelExp channel, int responseCode) {
        try {
            if (!Objects.equals(channel.getErrorUrl(), "")) {
                JSONObject json = getFirstLog(fileName);
                Map<String, String> queryParams = new HashMap<>();
                try {
                    queryParams.put("session_id", json.optString("session_id", ""));
                    queryParams.put("start_with", String.valueOf(json.optInt("sn", 0)));
                    queryParams.put("total_count", fileCountMap.containsKey(fileName) ? String.valueOf(fileCountMap.get(fileName)) : "unknown");
                    queryParams.put("channel_name", channel.getChannelName());
                    queryParams.put("response_code", String.valueOf(responseCode));
                } catch (Exception ignored) {
                }
                try(Response ignored = new NetUtils(30000, 30000).doGet(channel.getErrorUrl(), new HashMap<>(), queryParams, new JSONObject(), null)){}
            }
        } catch (Exception ignored) {
        }
    }

    private static void checkFolderLimit() {
        File folder = LogUtils.getFileExp("");
        long folderSize = 0;
        if (folder != null && folder.exists()) {
            File[] files = folder.listFiles();
            if (files != null) {
                for (File file : files) {
                    if (file != null && file.exists() && file.isFile()) {
                        folderSize += file.length();
                    }
                }
                if (folderSize < LogConstants.folderSizeLimit) {
                    return;
                }

                Arrays.sort(files, (f1, f2) -> Long.compare(f1.lastModified(), f2.lastModified()));
                long toReach = (long) (LogConstants.folderSizeLimit * 0.8);
                for (File file : files) {
                    if (file != null && file.exists() && file.isFile()) {
                        //noinspection ResultOfMethodCallIgnored
                        file.delete();
                        folderSize -= file.length();
                    }
                    if (folderSize <= toReach) {
                        break;
                    }
                }
            }
        }
    }

    private static void pushChannelFiles(String channelName, LogChannelExp channel) {
        try {
            File folder = LogUtils.getFileExp("");
            if (folder != null && folder.exists() && folder.isDirectory()) {
                File[] files = folder.listFiles((dir, name) -> name.contains(channelName));
                ArrayList<File> ndJsonFiles = new ArrayList<>();
                ArrayList<File> datFiles = new ArrayList<>();
                if (files != null) {
                    for (File file : files) {
                        if (file != null && file.exists() && file.isFile()) {
                            if (file.length() > 0 && LogUtils.isFileEligibleToPush(file)) {
                                if (file.getName().contains(".ndjson")) {
                                    ndJsonFiles.add(file);
                                } else {
                                    datFiles.add(file);
                                }
                            } else {
                                //noinspection ResultOfMethodCallIgnored
                                file.delete();
                            }
                        }
                    }
                }
                pushNdJsonFiles(ndJsonFiles, channel, false);
                pushDatFiles(datFiles, channel, false);
            }
        } catch (Exception ignored) {
        }
    }

    static void pushNdJsonFiles(ArrayList<File> ndJsonFiles, LogChannelExp channel, boolean shouldAck) {
        try {
            int startIndex = 0;
            int endIndex = 0;
            ArrayList<String> toAck = new ArrayList<>();
            while (endIndex < ndJsonFiles.size()) {
                long currentCount = 0;
                for (; endIndex < ndJsonFiles.size(); endIndex++) {
                    File file = ndJsonFiles.get(endIndex);
                    if (file != null && file.exists()) {
                        int fileCount = getFileCount(file.getName(), file);
                        if (currentCount == 0 || currentCount + fileCount <= channel.getBatchCount()) {
                            currentCount += fileCount;
                        } else {
                            break;
                        }
                    }
                }

                if (currentCount != 0) {
                    ArrayList<String> ndJsonFileNames = new ArrayList<>();
                    for (int i = startIndex; i < endIndex; i++) {
                        ndJsonFileNames.add(ndJsonFiles.get(i).getName());
                    }
                    Pair<byte[], Integer> pair = getFilesContent(ndJsonFileNames);
                    JSONArray batchNum = getBatchNumArray(ndJsonFileNames);
                    if (pair.first != null && pair.first.length > 0) {
                        int responseCode = pushLogsToServer(pair.first, pair.second == null ? 1 : pair.second, batchNum, channel, true);
                        if (responseCode != 200 && (channel.getRetryAttempts() == -1 || channel.getCurrentBatchRetryAttempts() < channel.getRetryAttempts())) {
                            SdkTracker.trackBootAction(LogSubCategory.Action.SYSTEM, LogLevel.ERROR,
                                    Labels.System.LOG_PUSHER, "error_response", responseCode);
                            channel.setCurrentBatchRetryAttempts(channel.getCurrentBatchRetryAttempts() + 1);
                        }
                        if (channel.getRetryAttempts() != -1 && channel.getCurrentBatchRetryAttempts() >= channel.getRetryAttempts()) {
                            for (int i = startIndex; i < endIndex; i++) {
                                String fileName = ndJsonFiles.get(i).getName();
                                hitErrorUrl(fileName, channel, responseCode);
                                if (shouldAck) {
                                    toAck.add(fileName);
                                }
                            }
                            channel.setCurrentBatchRetryAttempts(0);
                        }
                        if (responseCode == 200) {
                            channel.setCurrentBatchRetryAttempts(0);
                            for (int i = startIndex; i < endIndex; i++) {
                                if (shouldAck) {
                                    toAck.add(ndJsonFiles.get(i).getName());
                                }
                                //noinspection ResultOfMethodCallIgnored
                                ndJsonFiles.get(i).delete();
                            }
                        }
                    } else {
                        for (int i = startIndex; i < endIndex; i++) {
                            if (shouldAck) {
                                toAck.add(ndJsonFiles.get(i).getName());
                            }
                            //noinspection ResultOfMethodCallIgnored
                            ndJsonFiles.get(i).delete();
                        }
                    }
                } else {
                    for (int i = startIndex; i < endIndex; i++) {
                        if (shouldAck) {
                            toAck.add(ndJsonFiles.get(i).getName());
                        }
                        //noinspection ResultOfMethodCallIgnored
                        ndJsonFiles.get(i).delete();
                    }
                }
                startIndex = endIndex;
            }
            acknowledgeLogsPushed(toAck, channel);
        } catch (Exception ignored) {
        }
    }

    static void pushDatFiles(ArrayList<File> datFiles, LogChannelExp channel, boolean shouldAck) {
        try {
            ArrayList<String> toAck = new ArrayList<>();
            for (File file : datFiles) {
                if (file != null && file.exists() && file.length() > 0) {
                    String fileName = file.getName();
                    if (file.length() > 0) {
                        byte[] arr = LogUtils.getLogsFromFileExp(file);
                        int fileCount = getFileCount(fileName, file);
                        JSONArray batchNum = new JSONArray().put(getBatchNum(fileName));
                        int responseCode = pushLogsToServer(arr, fileCount, batchNum, channel, false);
                        if (responseCode != 200 && (channel.getRetryAttempts() == -1 || channel.getCurrentBatchRetryAttempts() < channel.getRetryAttempts())) {
                            SdkTracker.trackBootAction(LogSubCategory.Action.SYSTEM, LogLevel.ERROR,
                                    Labels.System.LOG_PUSHER, "error_response", responseCode);
                            channel.setCurrentBatchRetryAttempts(channel.getCurrentBatchRetryAttempts() + 1);
                        }
                        if (channel.getRetryAttempts() != -1 && channel.getCurrentBatchRetryAttempts() >= channel.getRetryAttempts()) {
                            hitErrorUrl(fileName, channel, responseCode);
                            channel.setCurrentBatchRetryAttempts(0);
                            if (shouldAck) {
                                toAck.add(fileName);
                            }
                        }
                        if (responseCode == 200) {
                            channel.setCurrentBatchRetryAttempts(0);
                            if (shouldAck) {
                                toAck.add(fileName);
                            }
                            //noinspection ResultOfMethodCallIgnored
                            file.delete();
                        }
                    } else {
                        if (shouldAck) {
                            toAck.add(fileName);
                        }
                        //noinspection ResultOfMethodCallIgnored
                        file.delete();
                    }
                } else if (file != null) {
                    if (shouldAck) {
                        toAck.add(file.getName());
                    }
                }
            }
            acknowledgeLogsPushed(toAck, channel);
        } catch (Exception ignored) {
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
            ExecutorManager.runOnLogPusherThread(() -> {
                if (!LogConstants.shouldPush || !LogUtils.isMinMemoryAvailable()) {
                    return;
                }

                for (Map.Entry<String, LogChannelExp> entry : channels.entrySet()) {
                    LogChannelExp channel = entry.getValue();

                    checkFolderLimit();
                    try {
                        ArrayList<File> list = new ArrayList<>();
                        for (String fileName : channel.getLogsQueueExp()) {
                            if (fileName.contains(".ndjson")) {
                                File file = LogUtils.getFileExp(fileName);
                                if (file != null && file.exists()) {
                                    list.add(file);
                                }
                            }
                        }
                        pushNdJsonFiles(list, channel, true);
                        list.clear();

                        for (String fileName : channel.getLogsQueueExp()) {
                            if (fileName.contains(".dat")) {
                                File file = LogUtils.getFileExp(fileName);
                                if (file != null && file.exists()) {
                                    list.add(file);
                                }
                            }
                        }
                        pushDatFiles(list, channel, true);

                    } catch (Exception e) {
                        if (!LogPushTimerTask.isExceptionTracked)
                            SdkTracker.trackAndLogBootException(TAG, LogCategory.ACTION,
                                    LogSubCategory.Action.SYSTEM, Labels.System.LOG_PUSHER,
                                    "Error while creating the payload to post", e);
                        LogPushTimerTask.isExceptionTracked = true;
                    }
                }

                for (Map.Entry<String, LogChannelExp> entry : channels.entrySet()) {
                    String channelName = entry.getKey();
                    LogChannelExp channel = entry.getValue();
                    pushChannelFiles(channelName, channel);
                }
            });
        }
    }
}
