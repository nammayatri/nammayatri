package in.juspay.mobility.app;

import android.content.Context;
import android.os.Environment;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import in.juspay.mobility.app.RemoteConfigs.MobilityRemoteConfigs;

/**
 * Custom logging utility for Android applications.
 * <p>
 * Logs messages to both Android logcat and persistent log files in the device's Downloads directory.
 * Supports log levels, asynchronous writing, and log file retention policy.
 */
public class Log {
    private static final String LOG_DIR_NAME = ".logs";
    private static final Object lock = new Object();
    private static Context appContext;
    private static int logRetentionDays = 7; // Default retention
    private static final ExecutorService logExecutor;
    private static boolean isFileLoggingEnabled = false;

    static {
        logExecutor = Executors.newSingleThreadExecutor();
        android.util.Log.d("CustomLog", "Log executor initialized");
    }

    /**
     * Initializes the logger with the application context and clears old logs.
     *
     * @param context The application context to use for file operations.
     */
    public static void init(Context context) {
        appContext = context.getApplicationContext();
        android.util.Log.d("CustomLog", "Logger initialized with context: " + appContext);
        isFileLoggingEnabled = new MobilityRemoteConfigs(true, false).getBoolean("is_file_logging_enabled");
        logExecutor.execute(Log::clearOldLogs);
    }

    /**
     * Sets the number of days to retain log files and clears old logs.
     *
     * @param days Number of days to retain log files.
     */
    public static void setLogRetentionDays(int days) {
        logRetentionDays = days;
        android.util.Log.d("CustomLog", "Log retention days set to: " + days);
        logExecutor.execute(Log::clearOldLogs);
    }

    /**
     * Logs an informational message.
     *
     * @param tag     Tag identifying the source of the log message.
     * @param message The message to log.
     */
    public static void i(String tag, String message) {
        android.util.Log.i(tag,message);
        Date now = new Date();
        writeLog("INFO", tag, message, null, now);
    }

    /**
     * Logs a debug message.
     *
     * @param tag     Tag identifying the source of the log message.
     * @param message The message to log.
     */
    public static void d(String tag, String message) {
        android.util.Log.d(tag,message);
        Date now = new Date();

        writeLog("DEBUG", tag, message, null, now);
    }

    /**
     * Logs a warning message.
     *
     * @param tag     Tag identifying the source of the log message.
     * @param message The message to log.
     */
    public static void w(String tag, String message) {
        android.util.Log.w(tag,message);
        Date now = new Date();
        writeLog("WARN", tag, message, null, now);
    }

    /**
     * Logs an error message.
     *
     * @param tag     Tag identifying the source of the log message.
     * @param message The message to log.
     */
    public static void e(String tag, String message) {
        android.util.Log.e(tag,message);
        Date now = new Date();
        writeLog("ERROR", tag, message, null, now);
    }

    /**
     * Logs an error message with an associated Throwable.
     *
     * @param tag       Tag identifying the source of the log message.
     * @param message   The message to log.
     * @param throwable The Throwable to log (stack trace will be included).
     */
    public static void e(String tag, String message, Throwable throwable) {
        android.util.Log.e(tag,message,throwable);
        Date now = new Date();
        writeLog("ERROR", tag, message, throwable, now);
    }

    /**
     * Writes a log entry to the log file asynchronously.
     * <p>
     * The log entry includes a log timestamp (event time), write timestamp (write time), log level, tag, message, and optional stack trace.
     * Log files are organized by write date in the Downloads/.logs directory.
     *
     * @param level        The log level (e.g., INFO, DEBUG, WARN, ERROR).
     * @param tag          Tag identifying the source of the log message.
     * @param message      The message to log.
     * @param throwable    Optional Throwable whose stack trace will be logged.
     * @param logTimestamp The timestamp when the log event occurred.
     */
    private static void writeLog(String level, String tag, String message, Throwable throwable, Date logTimestamp) {
        if (!isFileLoggingEnabled) return;
        if (appContext == null) {
            android.util.Log.e("CustomLog", "Logger not initialized: appContext is null");
            return;
        }
        logExecutor.submit(() -> {
            clearOldLogs();
            Date writeTimestamp = new Date();
            String date = new SimpleDateFormat("yyyy-MM-dd", Locale.US).format(writeTimestamp);
            File downloadsDir = Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_DOWNLOADS);
            File logDir = new File(downloadsDir, LOG_DIR_NAME + File.separator + date);
            if (!logDir.exists()) {
                boolean created = logDir.mkdirs();
                android.util.Log.d("CustomLog", "Created log directory: " + logDir.getAbsolutePath() + " success: " + created);
            }
            File logFile = new File(logDir, "log.txt");
            String logTime = new SimpleDateFormat("HH:mm:ss.SSS", Locale.US).format(logTimestamp);
            String writeTime = new SimpleDateFormat("HH:mm:ss.SSS", Locale.US).format(writeTimestamp);
            StringBuilder sb = new StringBuilder();
            sb.append("[logTime: ").append(logTime).append("] ");
            sb.append("[writeTime: ").append(writeTime).append("] ");
            sb.append("[packageName: ").append(appContext.getPackageName()).append("] ");
            sb.append(level).append("/").append(tag).append(": ").append(message);
            if (throwable != null) {
                sb.append("\n").append(android.util.Log.getStackTraceString(throwable));
            }
            synchronized (lock) {
                try (FileWriter writer = new FileWriter(logFile, true)) {
                    writer.write(sb.toString());
                    writer.write("\n");
                    android.util.Log.d("CustomLog", "Wrote log to file: " + logFile.getAbsolutePath());
                } catch (IOException e) {
                    android.util.Log.e("CustomLog", "Failed to write log to file: " + logFile.getAbsolutePath(), e);
                }
            }
        });
    }

    /**
     * Deletes log directories older than the configured retention period.
     * <p>
     * Only directories with names matching the date format yyyy-MM-dd are considered.
     * Non-date directories are skipped.
     */
    private static void clearOldLogs() {
        if (appContext == null) {
            android.util.Log.e("CustomLog", "Logger not initialized: appContext is null (clearOldLogs)");
            return;
        }
        File downloadsDir = Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_DOWNLOADS);
        File logsRoot = new File(downloadsDir, LOG_DIR_NAME);
        if (!logsRoot.exists() || !logsRoot.isDirectory()) {
            android.util.Log.d("CustomLog", "No logs directory to clean: " + logsRoot.getAbsolutePath());
            return;
        }
        File[] dateDirs = logsRoot.listFiles();
        if (dateDirs == null) {
            android.util.Log.d("CustomLog", "No date directories found in logs root: " + logsRoot.getAbsolutePath());
            return;
        }
        long now = System.currentTimeMillis();
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd", Locale.US);
        for (File dateDir : dateDirs) {
            if (!dateDir.isDirectory()) continue;
            try {
                Date dirDate = sdf.parse(dateDir.getName());
                if (dirDate == null) continue;
                Calendar cal = Calendar.getInstance();
                cal.setTime(dirDate);
                cal.add(Calendar.DAY_OF_YEAR, logRetentionDays);
                if (cal.getTimeInMillis() < now) {
                    deleteDirectory(dateDir);
                    android.util.Log.d("CustomLog", "Deleted old log directory: " + dateDir.getAbsolutePath());
                }
            } catch (ParseException e) {
                android.util.Log.d("CustomLog", "Skipping non-date directory: " + dateDir.getName());
            }
        }
    }

    /**
     * Recursively deletes a directory and all its contents.
     *
     * @param dir The directory to delete.
     */
    private static void deleteDirectory(File dir) {
        if (dir == null || !dir.exists()) return;
        File[] files = dir.listFiles();
        if (files != null) {
            for (File f : files) {
                if (f.isDirectory()) deleteDirectory(f);
                else f.delete();
            }
        }
        dir.delete();
    }
} 
