package in.juspay.hypersdk.analytics;

import androidx.annotation.NonNull;

import org.json.JSONArray;
import org.json.JSONObject;

import in.juspay.hypersdk.services.SdkConfigService;

public class LogConstants {

    public static final String LOG_CHANNEL_NAMES = "LOG_CHANNEL_NAMES";
    public static final String LOG_CHANNEL_INFO = "LOG_CHANNEL_INFO";
    public static final String DEFAULT_CHANNEL = "default";
    public static final String PERSISTENT_LOGS_READING_FILE = "PERSISTENT_LOGS_READING_FILE";
    public static final String PERSISTENT_LOGS_WRITING_FILE = "PERSISTENT_LOGS_WRITING_FILE";
    public static final String TEMP_LOGS_READING_FILE = "TEMP_LOGS_READING_FILE";
    public static final String TEMP_LOGS_WRITING_FILE = "TEMP_LOGS_WRITING_FILE";
    public static final String LOGS_READING_FILE = "LOGS_READING_FILE";
    public static final String LOGS_WRITING_FILE = "LOGS_WRITING_FILE";
    public static final String LOG_DELIMITER = "LOG_DELIMITER";  // Don't log this constant anywhere, Logging this will lead to bug

    public static final String PERSISTENT_LOGS_FILE = "juspay-logs-queue-";
    public static final String PERSISTENT_LOGS_FILE_EXTENSION = ".dat";
    public static final String LOGS_FILE = "juspay-pre-logs-queue-";
    public static final String LOGS_FILE_EXTENSION = ".dat";
    public static final String TEMP_LOGS_FILE = "temp-logs-queue-";
    public static final String TEMP_LOGS_FILE_EXTENSION = ".dat";
    public static final String CRASH_LOGS_FILE = "juspay-crash-logFile.dat";

    static long minMemoryRequired = 16384;
    static long maxLogLineSize = 20971520;
    static long maxLogFileSize = 20971520;
    static long maxLogValueSize = 32768;
    static int maxFilesAllowed = 7;
    static int numFilesToLeaveIfMaxFilesExceeded = 5;
    static long dontPushIfFileIsLastModifiedBeforeInHours = 720L;
    static int logPostInterval = 2 * 1000;
    static int logSessioniseInterval = 2000;
    static String encryptionLevel = "encryption";
    static long maxLogsPerPush = 75;
    static long maxSizeLimitPerPush = 204800;
    static int maxRetryPerBatch = -1;
    static int defaultPriority = 5;
    static long folderSizeLimit = 52428800;
    static long filesCountLimit = 200;
    static JSONObject publicKeySandbox = new JSONObject();
    static JSONObject publicKey = new JSONObject();
    static JSONObject channels = new JSONObject();
    static JSONArray logChannelsConfig = new JSONArray();
    static JSONArray defaultChannels = new JSONArray();
    static String sandboxLogUrl = "https://debug.logs.juspay.net/godel/analytics";
    static String prodLogUrl = "https://logs.juspay.in/godel/analytics";
    public static boolean shouldPush = true;
    public static String fileFormat = "prefixByte";
    public static JSONArray fallBackUrl = new JSONArray();
    public static JSONArray fallBackPublicKeys = new JSONArray();
    public static String errorUrl = "";
    @NonNull
    static JSONObject logHeaders = new JSONObject();
    @NonNull
    public static JSONObject logProperties = new JSONObject();

    static JSONArray allowWhileBuffering = new JSONArray();

    static {

        final JSONObject config = SdkConfigService.getCachedSdkConfig();

        try {
            if (config != null) {
                JSONObject logsConfig = config.getJSONObject("logsConfig");
                maxLogLineSize = logsConfig.optLong("maxLogLineSize", 20971520);  // Maximum Log Line size can be 20 MB
                maxLogFileSize = logsConfig.optLong("maxLogFileSize", 20971520);  // Maximum Log File size can be 20 MB
                minMemoryRequired = logsConfig.optLong("minMemoryRequired", 16384);
                maxFilesAllowed = logsConfig.optInt("maxFilesAllowed", 7);    // Maximum number of Log Files allowed = 7
                maxLogValueSize = logsConfig.optLong("maxLogValueSize", 32768);
                folderSizeLimit = logsConfig.optLong("folderSizeLimit", 52428800);
                filesCountLimit = logsConfig.optLong("filesCountLimit", 200);
                maxSizeLimitPerPush = logsConfig.optLong("maxSizeLimitPerPush", 204800);
                encryptionLevel = logsConfig.optString("encryptionLevelKey", "encryption");
                publicKeySandbox = logsConfig.optJSONObject("publicKeySandbox");
                publicKey = logsConfig.optJSONObject("publicKey");
                channels = logsConfig.optJSONObject("channels");
                defaultChannels = config.optJSONArray("defaultChannels");
                // Number of files to leave if number of log files exceeded maxFilesAllowed
                numFilesToLeaveIfMaxFilesExceeded = logsConfig.optInt("numFilesToLeaveIfMaxFilesExceeded", 5);
                dontPushIfFileIsLastModifiedBeforeInHours = logsConfig.optLong("dontPushIfFileIsLastModifiedBeforeInHours", 720L);
                shouldPush = logsConfig.optBoolean("shouldPush", true);

                sandboxLogUrl = logsConfig.optString("logsUrlKeySandbox", "https://debug.logs.juspay.net/godel/analytics");
                prodLogUrl = logsConfig.optString("logsUrlKey", "https://logs.juspay.in/godel/analytics");
                defaultPriority = logsConfig.optInt("defaultPriority", 5);
                maxRetryPerBatch = logsConfig.optInt("retryAttempts", -1);
                maxLogsPerPush = logsConfig.optLong("batchCount", 75);
                logPostInterval = logsConfig.optInt("logPusherTimerWithChannel", 2000);
                logSessioniseInterval = logsConfig.optInt("sessioniseTimer", 2000);

                logChannelsConfig = config.optJSONArray("logChannelsConfig");
                // CutOff Time to push logs in the file based on when the file is last modified. By default 7 days.
                if (logsConfig.has("logHeaders")) {
                    logHeaders = logsConfig.getJSONObject("logHeaders");
                }

                if (logsConfig.has("logProperties")) {
                    logProperties = logsConfig.getJSONObject("logProperties");
                }
                if (logsConfig.has("allowWhileBuffering")) {
                    allowWhileBuffering = logsConfig.getJSONArray("allowWhileBuffering");
                }
                if (logsConfig.has("fileFormat")) {
                    fileFormat = logsConfig.optString("fileFormat", "prefixByte");
                }
                if (logsConfig.has("fallBackUrl")) {
                    fallBackUrl = logsConfig.optJSONArray("fallBackUrl");
                }
                if (logsConfig.has("fallBackPublicKeys")) {
                    fallBackPublicKeys = logsConfig.optJSONArray("fallBackPublicKeys");
                }
                if (logsConfig.has("errorUrl")) {
                    errorUrl = logsConfig.optString("errorUrl", "");
                }
            }
        } catch (Exception exception) {
            // Ignore
        }
    }
}
