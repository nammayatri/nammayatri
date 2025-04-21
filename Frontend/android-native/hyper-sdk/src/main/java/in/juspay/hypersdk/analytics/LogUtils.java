package in.juspay.hypersdk.analytics;

import static android.content.Context.ACTIVITY_SERVICE;

import android.app.ActivityManager;
import android.content.Context;
import android.util.Log;

import org.json.JSONException;
import org.json.JSONObject;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import java.util.Queue;
import java.util.UUID;

import in.juspay.hyper.core.JuspayCoreLib;
import in.juspay.hypersdk.data.KeyValueStore;
import in.juspay.hypersdk.utils.IntegrationUtils;
import in.juspay.hypersdk.utils.Utils;
import kotlin.Deprecated;
import kotlin.Triple;

public class LogUtils {

    static boolean isFileEligibleToPush(File file) {
        if (file != null) {
            long lastTime = file.lastModified();
            long curTime = System.currentTimeMillis();
            long seconds = (curTime - lastTime) / (1000L);
            long hours = seconds / 3600L;

            return hours < LogConstants.dontPushIfFileIsLastModifiedBeforeInHours;
        }
        return false;
    }

    public static Map<String, String> toMap(JSONObject jsonobj) throws JSONException {
        Map<String, String> map = new HashMap<>();
        Iterator<String> keys = jsonobj.keys();
        while (keys.hasNext()) {
            String key = keys.next();
            String value = jsonobj.getString(key);
            map.put(key, value);
        }
        return map;
    }

    static Boolean isMinMemoryAvailable() {
        final Context context = JuspayCoreLib.getApplicationContext();
        try {
            if (context != null) {
                ActivityManager.MemoryInfo memoryInfo = Utils.getMemoryInfo(context);
                if (memoryInfo != null) {
                    return (memoryInfo.availMem >= LogConstants.minMemoryRequired);
                }
            }
        } catch (Exception exception) {
            //Ignored
        }
        return true;
    }

    static String getAnyFromSharedPreference(String key, String defaultValue) {
        final Context context = JuspayCoreLib.getApplicationContext();
        if (context != null) {
            final String sdkName = IntegrationUtils.getAppName(context);
            return (KeyValueStore.read(context, sdkName, key, defaultValue));
        }
        return defaultValue;
    }

    static int getFromSharedPreference(String key) {
        final Context context = JuspayCoreLib.getApplicationContext();
        if (context != null) {
            final String sdkName = IntegrationUtils.getAppName(context);
            return Integer.parseInt(KeyValueStore.read(context, sdkName, key, "-1"));
        }
        return -1;
    }

    static void removeFromSharedPreference(String key) {
        final Context context = JuspayCoreLib.getApplicationContext();
        if (context != null) {
            final String sdkName = IntegrationUtils.getAppName(context);
            KeyValueStore.remove(context, sdkName, key);

        }
    }

    static void writeToSharedPreference(String key, String value) {
        final Context context = JuspayCoreLib.getApplicationContext();
        if (context != null) {
            final String sdkName = IntegrationUtils.getAppName(context);
            KeyValueStore.write(context, sdkName, key, value, false);
        }
    }

    static File getFile(String filePath) {
        if (JuspayCoreLib.getApplicationContext() == null)
            return null;

        return new File(JuspayCoreLib.getApplicationContext().getCacheDir(), filePath);
    }

    static File getFileExp(String filePath) {
        if (JuspayCoreLib.getApplicationContext() == null)
            return null;

        return new File(JuspayCoreLib.getApplicationContext().getCacheDir(), "juspay_logs/" + filePath);
    }

    static long getFileLength(String fileName) {
        long length = 0;
        if (JuspayCoreLib.getApplicationContext() != null) {
            File file = new File(JuspayCoreLib.getApplicationContext().getCacheDir(), fileName);
            length = file.length();
        }
        return length;
    }

    @Deprecated(message = "Causing OutOfMemoryError")
    static Queue<JSONObject> getLogsFromFile(File logFile) {
        Queue<JSONObject> logs = new LinkedList<>();
        byte[] toRead = new byte[(int) logFile.length()];
        try {
            FileInputStream fis = new FileInputStream(logFile);
            //noinspection ResultOfMethodCallIgnored
            fis.read(toRead);
            fis.close();

            String logString = new String(toRead, StandardCharsets.UTF_8);
            String[] contentArray = logString.split(LogConstants.LOG_DELIMITER);

            for (String element : contentArray) {
                try {
                    logs.add(new JSONObject(element));
                } catch (Exception ignored) {
                    // A bad log should not stop other.
                }
            }
        } catch (Exception ignored) {

        }
        return logs;
    }

    static Triple<Queue<JSONObject>,Integer, Integer> getLogsFromFileInBatch(File logFile, int lastReadIndex) {
        Queue<JSONObject> logs = new LinkedList<>();
        int readIndex = lastReadIndex;
        int totalLength = lastReadIndex;
        try {

            byte[] toRead = new byte[(int) logFile.length()];
            FileInputStream fis = new FileInputStream(logFile);
            //noinspection ResultOfMethodCallIgnored
            fis.read(toRead);
            fis.close();

            String logString = new String(toRead, StandardCharsets.UTF_8);
            toRead = new byte[0];
            String[] contentArray = logString.split(LogConstants.LOG_DELIMITER);
            totalLength = contentArray.length;
            boolean isOutOfMemory = isOutOfMemory();
            if (isOutOfMemory) System.gc();
            while ( readIndex < contentArray.length && !isOutOfMemory) {
                try {
                    logs.add(new JSONObject(contentArray[readIndex]));
                    readIndex++;
                } catch (Exception ignored) {
                    // A bad log should not stop other.
                } catch (OutOfMemoryError ignored) {
                    break;
                }
            }
        } catch (Exception | OutOfMemoryError ignored) {

        }
        return new Triple<>(logs,totalLength,readIndex);
    }

    public static boolean isOutOfMemory() {
        final Context context = JuspayCoreLib.getApplicationContext();
        if (context != null) {
            ActivityManager activityManager = (ActivityManager) context.getSystemService(ACTIVITY_SERVICE);
            ActivityManager.MemoryInfo memoryInfo = new ActivityManager.MemoryInfo();
            activityManager.getMemoryInfo(memoryInfo);
            return memoryInfo.lowMemory;
        }
        return false;
    }

    static byte[] getLogsFromFileExp(File logFile) {
        byte[] toRead = new byte[(int) logFile.length()];
        try {
            FileInputStream fis = new FileInputStream(logFile);
            //noinspection ResultOfMethodCallIgnored
            fis.read(toRead);
            fis.close();
        } catch (Exception ignored) {

        }
        return toRead;
    }

    static String generateUUID() {
        return UUID.randomUUID().toString();
    }

    static void writeLogToFileExp(JSONObject logLine, File file) {
        if (file != null) {
            try (FileOutputStream fos = new FileOutputStream(file, true)) {
                byte[] byteArr = logLine.toString().getBytes(StandardCharsets.UTF_8);
                int length = byteArr.length;
                byte[] lenNum = ByteBuffer.allocate(4).putInt(length).array();
                fos.write(lenNum);
                fos.write(byteArr);
            } catch (Exception ignored) {
            }
        }
    }
}
