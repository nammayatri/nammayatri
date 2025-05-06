package in.juspay.mobility.sdk.analytics;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.File;
import java.io.FileOutputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Objects;
import java.util.Queue;
import java.util.Timer;
import java.util.TimerTask;

import in.juspay.mobility.sdk.hyper.core.ExecutorManager;
import in.juspay.mobility.sdk.hyper.core.JuspayCoreLib;
import in.juspay.mobility.sdk.hyper.core.JuspayLogger;
import kotlin.Triple;

/**
 * A class that ensures data tracked is in the correct format and removes all sensitive information if any.
 * <p>
 * Discussion Summary :
 * This Single task reads X number of logs from juspay-logs-pre-queue.dat and maintains the logs in memory
 * for Sanitising/Sessionising Executors to get their logs replaced by the time Manager times out.
 * On Timeout, regardless of anything, Manager will push the logs available in memory to juspay-logs-queue.dat
 * and on successful push, deletes X number of logs from juspay-logs-pre-queue.dat
 * <p>
 * There will be Sanitising/Sessionising Executors that reads from the Sanitising/Sessionising Manager and replaces the logs
 * <p>
 * Implementation Logic :
 * Files are used only as a replica storage.
 * Files will be used only to fetch logs in start of the sessioniser.
 * Main logic revolves around local variables that hold data in sessionised manner.
 */

// All public methods will run on Background Thread, enforcing private methods automatically to be in LogsThread.

public class LogSessioniser {

    /**
     * Object maintains current batch of logs.
     * Logs in this variable are subject to being replaced by JS sessioniser
     */
    private static JSONObject logs = new JSONObject();

    /**
     * Object maintains current batch of logs.
     * Maintains all logs
     */
    private static JSONObject rawLogs = new JSONObject();

    /**
     * activeRequestIDs is intentionally decoupled from requestId
     * This will allow capability to delete all logs from a particular session
     * and replace with a completely different session.
     * Enabling reordering/ recreating the entire session
     */
    private static final ArrayList<String> activeRequestIDs = new ArrayList<>();

    private static int timerModulus = 0;

    private static Timer moveToPusherTimer = new Timer();

    private static TimerTask moveToPusher;

    private static boolean stopPushingLogs = false;
    private static boolean timerStopped = false;

    static {

        ExecutorManager.runOnLogsPool(() -> {
            if (!LogConstants.shouldPush) {
                return;
            }
            ExecutorManager.setLogsThreadId(Thread.currentThread().getId());
            int logsWriteIndex = LogUtils.getFromSharedPreference(LogConstants.LOGS_WRITING_FILE);
            int logsReadIndex = LogUtils.getFromSharedPreference(LogConstants.LOGS_READING_FILE);
            if (logsWriteIndex == -1) {
                logsWriteIndex = 0;
                LogUtils.writeToSharedPreference(LogConstants.LOGS_WRITING_FILE, String.valueOf(logsWriteIndex));
            }
            if (logsReadIndex == -1) {
                logsReadIndex = 0;
                LogUtils.writeToSharedPreference(LogConstants.LOGS_READING_FILE, String.valueOf(logsReadIndex));
            }

            int tempLogsWriteIndex = LogUtils.getFromSharedPreference(LogConstants.TEMP_LOGS_WRITING_FILE);
            int tempLogsReadIndex = LogUtils.getFromSharedPreference(LogConstants.TEMP_LOGS_READING_FILE);
            if (tempLogsWriteIndex == -1) {
                tempLogsWriteIndex = 0;
                LogUtils.writeToSharedPreference(LogConstants.TEMP_LOGS_WRITING_FILE, String.valueOf(tempLogsWriteIndex));
            }
            if (tempLogsReadIndex == -1) {
                tempLogsReadIndex = 0;
                LogUtils.writeToSharedPreference(LogConstants.TEMP_LOGS_READING_FILE, String.valueOf(tempLogsReadIndex));
            }

            deleteOldFileIfNecessary(LogConstants.LOGS_READING_FILE, LogConstants.LOGS_WRITING_FILE, LogConstants.LOGS_FILE, LogConstants.LOGS_FILE_EXTENSION);
            deleteOldFileIfNecessary(LogConstants.TEMP_LOGS_READING_FILE, LogConstants.TEMP_LOGS_WRITING_FILE, LogConstants.TEMP_LOGS_FILE, LogConstants.TEMP_LOGS_FILE_EXTENSION);

            for (int i = tempLogsReadIndex; i <= tempLogsWriteIndex; i++) {
                try {
                    String filePath = LogConstants.TEMP_LOGS_FILE + i + LogConstants.TEMP_LOGS_FILE_EXTENSION;
                    if (JuspayCoreLib.getApplicationContext() != null) {
                        File tempFile = new File(JuspayCoreLib.getApplicationContext().getCacheDir(), filePath);
                        if (tempFile.length() <= LogConstants.maxLogFileSize && tempFile.exists() && LogUtils.isFileEligibleToPush(tempFile)) {
                            Triple<Queue<JSONObject>,Integer,Integer> logsPair = LogUtils.getLogsFromFileInBatch(tempFile,0);
                            while (logsPair.getThird() <= logsPair.getSecond()) {
                                LogPusher.addLogsFromSessioniser(logsPair.getFirst());
                                if (Objects.equals(logsPair.getThird(), logsPair.getSecond())) break;
                                logsPair = LogUtils.getLogsFromFileInBatch(tempFile,logsPair.getThird());
                            }

                        }
                        //noinspection ResultOfMethodCallIgnored
                        tempFile.delete();
                    }
                } catch (Exception exception) {
                    // Ignore
                }
            }

            LogUtils.writeToSharedPreference(LogConstants.TEMP_LOGS_READING_FILE, String.valueOf(0));
            LogUtils.writeToSharedPreference(LogConstants.TEMP_LOGS_WRITING_FILE, String.valueOf(0));


            for (int i = logsReadIndex; i <= logsWriteIndex; i++) {
                try {
                    String filePath = LogConstants.LOGS_FILE + i + LogConstants.LOGS_FILE_EXTENSION;
                    if (JuspayCoreLib.getApplicationContext() != null) {
                        File logFile = new File(JuspayCoreLib.getApplicationContext().getCacheDir(), filePath);
                        if (logFile.length() <= LogConstants.maxLogFileSize && logFile.exists() && LogUtils.isFileEligibleToPush(logFile)) {
                            Triple<Queue<JSONObject>,Integer,Integer> logsPair = LogUtils.getLogsFromFileInBatch(logFile,0);
                            while (logsPair.getThird() <= logsPair.getSecond()) {
                                LogPusher.addLogsFromSessioniser(logsPair.getFirst());
                                if (Objects.equals(logsPair.getThird(), logsPair.getSecond())) break;
                                logsPair = LogUtils.getLogsFromFileInBatch(logFile,logsPair.getThird());
                            }
                        }
                        //noinspection ResultOfMethodCallIgnored
                        logFile.delete();
                    }
                } catch (Exception exception) {
                    // Ignore
                }
            }

            LogUtils.writeToSharedPreference(LogConstants.LOGS_READING_FILE, String.valueOf(0));
            LogUtils.writeToSharedPreference(LogConstants.LOGS_WRITING_FILE, String.valueOf(0));
        });
    }

    static void startLogSessioniser() {
        ExecutorManager.runOnLogsPool(() -> {
            try {
                stopPushingLogs = false;
                moveToPusherTimer = new Timer();
                moveToPusher = new LogSessioniserTimerTask();
                moveToPusherTimer.scheduleAtFixedRate(moveToPusher, 0L, LogConstants.logSessioniseInterval);
                timerStopped = false;
            } catch (Exception exception) {
                // Ignore
            }
        });
    }

    private static void startLogSessioniserOnLogCount() {
        ExecutorManager.runOnLogsPool(() -> {
            try {
                if (timerStopped) {
                    moveToPusherTimer = new Timer();
                    moveToPusher = new LogSessioniserTimerTask();
                    moveToPusherTimer.scheduleAtFixedRate(moveToPusher, 0L, LogConstants.logSessioniseInterval);
                    timerStopped = false;
                }
            } catch (Exception exception) {
                // Ignore
            }
        });
    }

    static void stopLogSessioniserOnTerminate() {
        ExecutorManager.runOnLogsPool(() -> {
            try {
                moveToPusherTimer.cancel();
                if (pushLogsFromJsonToPusher(logs)) {
                    logs = new JSONObject();
                    int tempReadIndex = LogUtils.getFromSharedPreference(LogConstants.TEMP_LOGS_READING_FILE);
                    int tempWriteIndex = LogUtils.getFromSharedPreference(LogConstants.TEMP_LOGS_WRITING_FILE);
                    clearAllLogFiles(LogConstants.TEMP_LOGS_FILE, LogConstants.TEMP_LOGS_FILE_EXTENSION, tempReadIndex, tempWriteIndex);
                    LogUtils.writeToSharedPreference(LogConstants.TEMP_LOGS_READING_FILE, String.valueOf(0));
                    LogUtils.writeToSharedPreference(LogConstants.TEMP_LOGS_WRITING_FILE, String.valueOf(0));
                }
                if (pushLogsFromJsonToPusher(rawLogs)) {
                    rawLogs = new JSONObject();
                    int logReadIndex = LogUtils.getFromSharedPreference(LogConstants.LOGS_READING_FILE);
                    int logWriteIndex = LogUtils.getFromSharedPreference(LogConstants.LOGS_WRITING_FILE);
                    clearAllLogFiles(LogConstants.LOGS_FILE, LogConstants.LOGS_FILE_EXTENSION, logReadIndex, logWriteIndex);
                    LogUtils.writeToSharedPreference(LogConstants.LOGS_READING_FILE, String.valueOf(0));
                    LogUtils.writeToSharedPreference(LogConstants.LOGS_WRITING_FILE, String.valueOf(0));
                }
                timerStopped = true;
                stopPushingLogs = true;
            } catch (Exception exception) {
                // Ignore
            }
        });
    }

    private static boolean pushLogsFromJsonToPusher(JSONObject logs) {

        try {
            Iterator<String> keys = logs.keys();
            while (keys.hasNext()) {
                String key = keys.next();
                LogPusher.addLogLines(logs.getJSONArray(key));
            }
            return true;
        } catch (Exception e) {
            // Ignore, exception should not block adding rest of the sessions
        }
        return false;
    }

    private static void pushJsonToFile(JSONObject logs, String fileName, String fileExt, String sharedPreferenceWritingFile, int writingIndex) throws Exception {
        Iterator<String> keys = logs.keys();
        File file = LogUtils.getFile(fileName + writingIndex + fileExt);
        FileOutputStream fos = new FileOutputStream(file, true);
        while (keys.hasNext()) {
            String key = keys.next();
            JSONArray logsArr = logs.getJSONArray(key);
            for (int i = 0; i < logsArr.length(); i++) {
                JSONObject log = logsArr.getJSONObject(i);
                String toWrite = log.toString() + LogConstants.LOG_DELIMITER;
                byte[] byteArr = toWrite.getBytes(StandardCharsets.UTF_8);
                long jsonSize = byteArr.length;
                long fileSize = LogUtils.getFileLength(fileName + writingIndex + fileExt);
                if (fileSize + jsonSize <= LogConstants.maxLogFileSize) {
                    fos.write(byteArr);
                } else if (jsonSize <= LogConstants.maxLogLineSize) {
                    LogUtils.writeToSharedPreference(sharedPreferenceWritingFile, String.valueOf(++writingIndex));
                    file = LogUtils.getFile(fileName + writingIndex + fileExt);
                    fos = new FileOutputStream(file, true);
                    fos.write(byteArr);
                }
            }
        }
        fos.close();
    }

    /**
     * Function to get logs from the SDK tracker
     *
     * @param sessionId of the hyperService instance calling add logs
     * @param logLine   to be sessionised
     */
    public static void addLogLine(final String sessionId, final JSONObject logLine) {
        if (stopPushingLogs || !LogConstants.shouldPush) {
            return;
        }

        ExecutorManager.runOnLogsPool(() -> {
            try {
                JSONObject value = logLine.getJSONObject("value");
                if (value.toString().getBytes().length > LogConstants.maxLogValueSize) {
                    logLine.put("value", "Filtered");
                    JuspayLogger.i("LogSessioniser", "Filtering the value of log as the size of value is greater than 32 KB");
                }
                startLogSessioniserOnLogCount();
                if (!rawLogs.has(sessionId)) {
                    JSONArray logArr = new JSONArray();
                    logArr.put(logLine);
                    rawLogs.put(sessionId, logArr);
                } else {
                    rawLogs.accumulate(sessionId, logLine);
                }
            } catch (Exception exception) {
                // Ignore
            }
        });
    }

    /**
     * Function to return raw logs to any JS sessioniser
     *
     * @param request contains request object to get logs from JS.
     *                request contains requestId and sessionId.
     *                requestId : is required to ensure logs returned from JS
     *                are meant for the correct session
     *                sessionId : is to ensure only logs of the correct instance
     *                is returned to any HyperService instance
     * @return A string JSONObject with requestId and Array of logs
     */
    public static String getLogsFromSessionId(JSONObject request) {
        if (request != null) {
            String requestId = "", sessionId = "";
            try {
                requestId = request.getString("requestId");
                sessionId = request.getString("sessionId");
                activeRequestIDs.add(requestId);
                JSONArray logsArray = logs.optJSONArray(sessionId);
                if (logsArray != null) {
                    return new JSONObject()
                            .put("requestId", requestId)
                            .put("error", false)
                            .put("logs", logsArray).toString();
                }
            } catch (JSONException e) {
                String message =
                        requestId.equals("") ? "RequestId not sent"
                                : sessionId.equals("") ? "SessionId not sent"
                                : "Request invalid";
                return constructErrorMessage(message, requestId);
            }
            return constructErrorMessage("No logs saved to file", requestId);
        }
        return constructErrorMessage("Request Invalid", "");
    }

    /**
     * Function to construct response JSON.
     * This function uses a string, so that it does not have to handle JSON Exceptions
     */
    private static String constructErrorMessage(String message, String requestId) {
        return "{\"requestId\":\"" + requestId + "\",\"error\":true,\"logs\":{},\"errorMessage\":\"" + message + "\"}";
    }

    /**
     * Function to accept logs from JS side sessioniser to
     *
     * @param request from JS containing sessionised logs. RequestId and SessionId
     */
    public static void sessioniseLogs(final JSONObject request) {
        if (stopPushingLogs || !LogConstants.shouldPush) {
            return;
        }

        ExecutorManager.runOnLogsPool(() -> {
            try {
                String sessionId = request.getString("sessionId");
                String requestId = request.getString("requestId");
                JSONArray sessionisedLogs = request.getJSONArray("logs");
                if (activeRequestIDs.contains(requestId)) {
                    if (sessionisedLogs.toString().getBytes().length <= LogConstants.maxLogLineSize) {
                        startLogSessioniserOnLogCount();
                        logs.put(sessionId, sessionisedLogs);
                    }
                }
            } catch (JSONException e) {
                //Ignored will happen if request does not have requestId or SessionId
            }
        });
    }

    private static void clearAllLogFiles(String fileName, String fileExt, int startIndex, int endIndex) {
        for (int i = startIndex; i <= endIndex; i++) {
            try {
                String filePath = fileName + i + fileExt;
                File file = LogUtils.getFile(filePath);
                if (file != null) {
                    //noinspection ResultOfMethodCallIgnored
                    file.delete();
                }
            } catch (Exception exception) {
                // Ignore
            }
        }
    }

    private static void deleteOldFileIfNecessary(String readingConstant, String writingConstant, String fileName, String fileExt) {
        int readingIndex = LogUtils.getFromSharedPreference(readingConstant);
        int writingIndex = LogUtils.getFromSharedPreference(writingConstant);
        if (JuspayCoreLib.getApplicationContext() != null && writingIndex - readingIndex + 1 > LogConstants.maxFilesAllowed) {
            while (writingIndex - readingIndex + 1 > LogConstants.numFilesToLeaveIfMaxFilesExceeded) {
                String filePath = fileName + readingIndex + fileExt;
                File file = LogUtils.getFile(filePath);
                try {
                    if (file != null) {
                        //noinspection ResultOfMethodCallIgnored
                        file.delete();
                    }
                } catch (Exception exception) {
                    // Ignore
                }
                readingIndex++;
            }
            LogUtils.writeToSharedPreference(readingConstant, String.valueOf(readingIndex));
        }
    }

    /*
       A class containing logic to push logs from LogSessioniser to LogPuhser.
     */
    private static class LogSessioniserTimerTask extends TimerTask {

        @Override
        public void run() {

            ExecutorManager.runOnLogsPool(() -> {

                if (!LogConstants.shouldPush || !LogUtils.isMinMemoryAvailable()) {
                    return;
                }
                boolean shouldMoveToPusher = timerModulus == 1;

                deleteOldFileIfNecessary(LogConstants.LOGS_READING_FILE, LogConstants.LOGS_WRITING_FILE, LogConstants.LOGS_FILE, LogConstants.LOGS_FILE_EXTENSION);
                deleteOldFileIfNecessary(LogConstants.TEMP_LOGS_READING_FILE, LogConstants.TEMP_LOGS_WRITING_FILE, LogConstants.TEMP_LOGS_FILE, LogConstants.TEMP_LOGS_FILE_EXTENSION);

                // Part A send logs to pusher
                if (shouldMoveToPusher) {
                    // Empty activeRequestIds to invalidate old requests
                    activeRequestIDs.clear();

                    pushLogsFromJsonToPusher(logs);
                    // Clearing temp file after moving to logPusher
                    int tempReadIndex = LogUtils.getFromSharedPreference(LogConstants.TEMP_LOGS_READING_FILE);
                    int tempWriteIndex = LogUtils.getFromSharedPreference(LogConstants.TEMP_LOGS_WRITING_FILE);
                    clearAllLogFiles(LogConstants.TEMP_LOGS_FILE, LogConstants.TEMP_LOGS_FILE_EXTENSION, tempReadIndex, tempWriteIndex);
                    LogUtils.writeToSharedPreference(LogConstants.TEMP_LOGS_READING_FILE, String.valueOf(0));
                    LogUtils.writeToSharedPreference(LogConstants.TEMP_LOGS_WRITING_FILE, String.valueOf(0));

                }

                // Part B :: moveLogsToFile
                if (shouldMoveToPusher) {
                    // Move rawLogs to logs
                    logs = rawLogs;
                    rawLogs = new JSONObject();
                } else {

                    int logReadIndex = LogUtils.getFromSharedPreference(LogConstants.LOGS_READING_FILE);
                    int logWriteIndex = LogUtils.getFromSharedPreference(LogConstants.LOGS_WRITING_FILE);
                    clearAllLogFiles(LogConstants.LOGS_FILE, LogConstants.LOGS_FILE_EXTENSION, logReadIndex, logWriteIndex);
                    LogUtils.writeToSharedPreference(LogConstants.LOGS_READING_FILE, String.valueOf(0));
                    LogUtils.writeToSharedPreference(LogConstants.LOGS_WRITING_FILE, String.valueOf(0));
                    logWriteIndex = 0;

                    // Add logs to main file
                    try {
                        pushJsonToFile(rawLogs, LogConstants.LOGS_FILE, LogConstants.LOGS_FILE_EXTENSION, LogConstants.LOGS_WRITING_FILE, logWriteIndex);
                    } catch (Exception exception) {
                        // Ignore
                    }
                }

                if (shouldMoveToPusher) {
                    // Moving logs to temp-file
                    int tempLogsWriteIndex = LogUtils.getFromSharedPreference(LogConstants.TEMP_LOGS_WRITING_FILE);
                    if (tempLogsWriteIndex == -1) {
                        tempLogsWriteIndex = 0;
                    }
                    try {
                        pushJsonToFile(logs, LogConstants.TEMP_LOGS_FILE, LogConstants.TEMP_LOGS_FILE_EXTENSION, LogConstants.TEMP_LOGS_WRITING_FILE, tempLogsWriteIndex);
                    } catch (Exception exception) {
                        // Ignore
                    }
                }

                if (logs.length() == 0 && rawLogs.length() == 0) {
                    moveToPusherTimer.cancel();
                    timerStopped = true;
                }

                ++timerModulus;
                timerModulus %= 5;
            });
        }
    }
}
