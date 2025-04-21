package in.juspay.hypersdk.analytics;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.util.Pair;

import org.json.JSONArray;
import org.json.JSONObject;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import in.juspay.hyper.core.ExecutorManager;

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

public class LogSessioniserExp {

    @NonNull
    private String rawLogsrequestId = LogUtils.generateUUID().replace("-", "");
    @Nullable
    private TimerTask moveToPusher;
    @NonNull
    private Timer moveToPusherTimer = new Timer();
    @NonNull
    private final AtomicInteger batchNumber = new AtomicInteger(0);
    private boolean tempFlipDone = false;
    @NonNull
    private final AtomicBoolean pushFileCreated = new AtomicBoolean(false);

    @NonNull
    private LoggerState loggerState = LoggerState.IDLE;

    @NonNull
    private ConcurrentHashMap<String, FileOutputStream> fosMap = new ConcurrentHashMap<>();
    @NonNull
    private ConcurrentHashMap<String, ArrayList<String>> currentFilesObj = new ConcurrentHashMap<>();
    @NonNull
    private final ConcurrentLinkedQueue<String> filesObj = new ConcurrentLinkedQueue<>();
    @NonNull
    private final ConcurrentHashMap<String, Pair<Integer, Integer>> logsCount = new ConcurrentHashMap<>();

    public void startLogSessioniser() {
        if (!LoggerState.PUSHING.equals(loggerState)) {
            loggerState = LoggerState.BUFFERING; // race
            try {
                LogPusherExp.startLogPusher();
                File file = LogUtils.getFileExp("temp/");
                if (file != null) {
                    //noinspection ResultOfMethodCallIgnored
                    file.mkdirs();
                }
                file = LogUtils.getFileExp("original/");
                if (file != null) {
                    //noinspection ResultOfMethodCallIgnored
                    file.mkdirs();
                }
                moveToPusherTimer = new Timer();
                moveToPusher = new LogSessioniserTimerTask();
                moveToPusherTimer.schedule(moveToPusher, 0L, LogConstants.logSessioniseInterval);
            } catch (Exception ignored) {
            }
        }
    }

    public void stopLogSessioniserOnTerminate() {
        ExecutorManager.runOnLogSessioniserThread(() -> {
            try {
                if (moveToPusher != null) {
                    moveToPusher.cancel();
                }
                moveToPusherTimer.cancel();
                if (LogConstants.shouldPush) {
                    pushToPusher();
                }

                loggerState = LoggerState.TERMINATED;
            } catch (Exception ignored) {
            }
            ExecutorManager.runOnLogPusherThread(LogPusherExp::stopLogPusherOnTerminate);
        });
    }

    /**
     * Function to get logs from the SDK tracker
     *
     * @param logLine to be sessionised
     */
    public void addLogLine(final JSONObject logLine) {
        ExecutorManager.runOnLogSessioniserThread(() -> {
            if (!LogConstants.shouldPush) {
                return;
            }

            String channel = logLine.optString("channel", "default");
            if (LoggerState.BUFFERING.equals(loggerState)) {
                if (shouldAllowLog(logLine)) {
                    String logExtension;
                    if (LogConstants.fileFormat.equals("ndJson")) {
                        logExtension = ".ndjson";
                    } else {
                        logExtension = ".dat";
                    }
                    String fileName = "logs-" + channel + "-" + rawLogsrequestId + String.format(Locale.US, "-%03d", batchNumber.incrementAndGet()) + "-0001" + logExtension;

                    try {
                        logLine.put("batch_number", batchNumber.get());
                    } catch (Exception ignored) {
                    }

                    File file = LogUtils.getFileExp("original/" + fileName);
                    LogUtils.writeLogToFileExp(logLine, file);
                    File file1 = LogUtils.getFileExp(fileName);
                    if (file != null && file1 != null) {
                        //noinspection ResultOfMethodCallIgnored
                        file.renameTo(file1);
                    }
                    LogPusherExp.addLogLines(channel, fileName);
                    return;
                }
            }
            addToLogs(channel, rawLogsrequestId, logLine);
        });
    }

    private void addToLogs(String channel, String requestId, JSONObject logLine) {

        try {
            FileOutputStream fos = null;
            String logExtension;

            if (LogConstants.fileFormat.equals("ndJson")) {
                logExtension = ".ndjson";
            } else {
                logExtension = ".dat";
            }

            String fileName;
            if (currentFilesObj.containsKey(channel)) {
                ArrayList<String> list = currentFilesObj.get(channel);
                if (list != null) {
                    fileName = list.get(list.size() - 1);
                    if (logsCount.get(fileName) != null) {
                        Pair<Integer, Integer> pair = logsCount.get(fileName);
                        if (pair != null && pair.first != null && pair.first >= LogConstants.maxLogsPerPush) {
                            fileName = "logs-" + channel + '-' + requestId + '-' + String.format(Locale.US, "%03d", batchNumber.incrementAndGet()) + logExtension;
                        }
                    }
                } else {
                    fileName = "logs-" + channel + '-' + requestId + '-' + String.format(Locale.US, "%03d", batchNumber.incrementAndGet()) + logExtension;
                }
            } else {
                fileName = "logs-" + channel + '-' + requestId + '-' + String.format(Locale.US, "%03d", batchNumber.incrementAndGet()) + logExtension;
            }
            if (fosMap.containsKey(fileName)) {
                fos = fosMap.get(fileName);
            }

            if (fos == null) {
                File file = LogUtils.getFileExp(!tempFlipDone ? "temp/" + fileName : "original/" + fileName);
                fos = new FileOutputStream(file, true);
                fosMap.put(fileName, fos);

                if (currentFilesObj.containsKey(channel)) {
                    ArrayList<String> list = currentFilesObj.get(channel);
                    if (list != null) {
                        list.add(fileName);
                    }
                } else {
                    ArrayList<String> list = new ArrayList<>();
                    list.add(fileName);
                    currentFilesObj.put(channel, list);
                }
                if (pushFileCreated.get() && !tempFlipDone) {
                    ExecutorManager.runOnBackgroundThread(this::updatePushFile);
                }
            }

            if (logsCount.containsKey(fileName)) {
                Pair<Integer, Integer> pair = logsCount.get(fileName);
                int logCount, batchNum;
                if (pair != null) {
                    logCount = pair.first == null ? 0 : pair.first;
                    batchNum = pair.second == null ? LogPusherExp.getBatchNum(fileName) : pair.second;
                } else {
                    logCount = 0;
                    batchNum = LogPusherExp.getBatchNum(fileName);
                }
                logsCount.put(fileName, Pair.create(logCount + 1, batchNum));
                logLine.put("batch_number", batchNumber);
            } else {
                int batchNum = LogPusherExp.getBatchNum(fileName);
                logsCount.put(fileName, Pair.create(1, batchNum));
                logLine.put("batch_number", batchNum);
            }

            String toWrite = logLine.toString();
            if (fileName.contains(".ndjson")) {
                toWrite = toWrite + '\n';
            }
            byte[] arr = toWrite.getBytes(StandardCharsets.UTF_8);
            if (fileName.contains(".dat")) {
                int len = arr.length;
                byte[] lenNum = ByteBuffer.allocate(4).putInt(len).array();
                fos.write(lenNum);
            }
            fos.write(arr);
        } catch (Exception ignored) {
        }
    }

    private boolean shouldAllowLog(JSONObject logLine) {
        JSONArray arr = LogConstants.allowWhileBuffering;
        for (int i = 0; i < arr.length(); i++) {
            try {
                JSONObject allowJSON = arr.getJSONObject(i);
                if (allowJSON.length() > 0) {
                    boolean toAllow = true;
                    for (Iterator<String> it = allowJSON.keys(); it.hasNext(); ) {
                        String key = it.next();
                        boolean toAllow1 = false;
                        if (logLine.has(key)) {
                            JSONArray values = allowJSON.getJSONArray(key);
                            for (int j = 0; j < values.length(); j++) {
                                if (Objects.equals(values.get(j), logLine.get(key))) {
                                    toAllow1 = true;
                                    break;
                                }
                            }
                        }
                        if (!toAllow1) {
                            toAllow = false;
                            break;
                        }
                    }
                    if (toAllow) {
                        return true;
                    }
                }
            } catch (Exception ignored) {
            }
        }
        return false;
    }

    private void getAllTempFiles(JSONObject json) {
        for (String fileName : filesObj) {
            try {
                json.put(fileName, "");
            } catch (Exception ignored) {
            }
        }
        for (Map.Entry<String, ArrayList<String>> entry : currentFilesObj.entrySet()) {
            try {
                for (String fileName : entry.getValue()) {
                    json.put(fileName, "");
                }
            } catch (Exception ignored) {
            }
        }
    }

    private void updatePushFile() {
        File file = LogUtils.getFileExp("temp/push.json");
        if (file != null) {
            JSONObject presentFiles = new JSONObject();
            if (file.exists()) {
                try (FileInputStream fis = new FileInputStream(file)) {
                    byte[] arr = new byte[(int) file.length()];
                    //noinspection ResultOfMethodCallIgnored
                    fis.read(arr);
                    presentFiles = new JSONObject(new String(arr));
                } catch (Exception ignored) {
                }
            }
            getAllTempFiles(presentFiles);
            try (FileOutputStream fos = new FileOutputStream(file)) {
                fos.write(presentFiles.toString().getBytes(StandardCharsets.UTF_8));
            } catch (Exception ignored) {
            }
        }
    }

    public void startPushing() {
        ExecutorManager.runOnLogSessioniserThread(() -> {
            if (!LoggerState.PUSHING.equals(loggerState)) {

                // Have to write filesObj, currentFiles
                pushFileCreated.set(true);
                updatePushFile();
                synchronized (loggerState) {
                    loggerState = LoggerState.PUSHING;
                }
            }
        });
    }

    private int getLogCount(String fileName) {
        int logCount = 0;
        if (logsCount.get(fileName) != null) {
            Pair<Integer, Integer> pair = logsCount.get(fileName);
            if (pair != null && pair.first != null) {
                logCount = pair.first;
            } else {
                File file = LogUtils.getFileExp(fileName);
                if (file != null) {
                    logCount = LogPusherExp.traverseTheFile(fileName, file);
                }
            }
        } else {
            File file = LogUtils.getFileExp(fileName);
            if (file != null) {
                logCount = LogPusherExp.traverseTheFile(fileName, file);
            }
        }
        return logCount;
    }

    private void pushToPusher() {
        rawLogsrequestId = LogUtils.generateUUID().replace("-", "");
        for (Map.Entry<String, FileOutputStream> entry : fosMap.entrySet()) {
            try {
                entry.getValue().close();
            } catch (Exception ignored) {
            }
        }
        fosMap = new ConcurrentHashMap<>();
        ConcurrentHashMap<String, ArrayList<String>> currentFiles = currentFilesObj;
        currentFilesObj = new ConcurrentHashMap<>();

        synchronized (loggerState) {
            boolean doFlip;
            if (LoggerState.PUSHING.equals(loggerState) && !tempFlipDone) {
                tempFlipDone = true;
                doFlip = true;
            } else {
                doFlip = false;
            }

            ExecutorManager.runOnBackgroundThread(() -> {

                if (LoggerState.BUFFERING.equals(loggerState)) {
                    for (Map.Entry<String, ArrayList<String>> entry : currentFiles.entrySet()) {
                        ArrayList<String> value = entry.getValue();
                        ArrayList<String> toPut = new ArrayList<>();
                        for (String fileName : value) {
                            int ind = fileName.lastIndexOf('.');
                            String toRenameStr = fileName.substring(0, ind) + String.format(Locale.US, "-%04d", getLogCount(fileName)) + fileName.substring(ind);
                            File file = LogUtils.getFileExp("temp/" + fileName);
                            File toRename = LogUtils.getFileExp("temp/" + toRenameStr);
                            if (file != null && file.exists()) {
                                if (toRename != null) {
                                    //noinspection ResultOfMethodCallIgnored
                                    file.renameTo(toRename);
                                }
                                toPut.add(toRenameStr);
                            }
                        }

                        filesObj.addAll(toPut);
                    }
                }

                if (LoggerState.PUSHING.equals(loggerState)) {
                    try {
                        for (String fileName : filesObj) {
                            File file = LogUtils.getFileExp("temp/" + fileName);
                            if (file != null && file.exists()) {
                                File toRename = LogUtils.getFileExp(fileName);
                                if (toRename != null) {
                                    //noinspection ResultOfMethodCallIgnored
                                    file.renameTo(toRename);
                                }
                            }
                        }
                    } catch (Exception ignored) {
                    }

                    for (Map.Entry<String, ArrayList<String>> entry : currentFiles.entrySet()) {
                        try {
                            for (String fileName : entry.getValue()) {
                                File file = LogUtils.getFileExp(doFlip ? "temp/" + fileName : "original/" + fileName);
                                if (file != null && file.exists()) {
                                    int ind = fileName.lastIndexOf('.');
                                    File toRename = LogUtils.getFileExp(fileName.substring(0, ind) + String.format(Locale.US, "-%04d", getLogCount(fileName)) + fileName.substring(ind));
                                    if (toRename != null) {
                                        //noinspection ResultOfMethodCallIgnored
                                        file.renameTo(toRename);
                                    }
                                }
                            }
                        } catch (Exception ignored) {
                        }
                    }
                }
            });
        }
    }

    /*
       A class containing logic to push logs from LogSessioniser to LogPusher.
     */
    private class LogSessioniserTimerTask extends TimerTask {

        @Override
        public void run() {
            if (!LogConstants.shouldPush || !LogUtils.isMinMemoryAvailable()) {
                return;
            }

            ExecutorManager.runOnLogSessioniserThread(LogSessioniserExp.this::pushToPusher);
        }
    }
}
