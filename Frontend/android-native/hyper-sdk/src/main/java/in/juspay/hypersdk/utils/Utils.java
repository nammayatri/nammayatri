package in.juspay.hypersdk.utils;

import android.app.ActivityManager;
import android.content.Context;
import android.content.pm.PackageManager;
import android.os.Build;
import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.content.PermissionChecker;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.net.InetAddress;
import java.net.NetworkInterface;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.zip.GZIPOutputStream;

import in.juspay.hyper.constants.Labels;
import in.juspay.hyper.constants.LogCategory;
import in.juspay.hyper.constants.LogLevel;
import in.juspay.hyper.constants.LogSubCategory;
import in.juspay.hyper.core.JuspayLogger;
import in.juspay.hypersdk.core.JuspayServices;

/**
 * Created by Veera.Subbiah on 13/04/18.
 */

public class Utils {
    private static final String LOG_TAG = "Utils";

    public static boolean checkIfGranted(JuspayServices juspayServices, String permission) {
        final Context context = juspayServices.getContext();

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M)
            return (PermissionChecker.checkSelfPermission(context, permission) == PermissionChecker.PERMISSION_GRANTED);
        else {
            try {
                PackageManager packageManager = context.getPackageManager();
                String packageName = context.getPackageName();
                if (packageManager.checkPermission(permission, packageName) == PackageManager.PERMISSION_GRANTED) {
                    return true;
                } else {
                    juspayServices.sdkDebug(LOG_TAG, "permissions not found:" + permission);
                    return false;
                }
            } catch (Throwable error) {
                juspayServices.getSdkTracker().trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.UTIL, "Exception trying to fetch permission info: " + permission + " returning FALSE", error);
                return false;
            }
        }
    }

    public static String getIPAddress(JuspayServices juspayServices) {
        try {
            List<NetworkInterface> interfaces = Collections.list(NetworkInterface.getNetworkInterfaces());
            for (NetworkInterface intf : interfaces) {
                List<InetAddress> addrs = Collections.list(intf.getInetAddresses());
                for (InetAddress addr : addrs) {
                    if (!addr.isLoopbackAddress()) {
                        String address = addr.getHostAddress();
                        if (address != null) {
                            String sAddr = addr.getHostAddress().toUpperCase(Locale.getDefault());
                            boolean isIPv4 = isIPv4Address(sAddr);
                            if (isIPv4) {
                                return sAddr;
                            }
                        }
                    }
                }
            }
        } catch (Exception ex) {
            juspayServices.getSdkTracker().trackException(LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.UTIL, "Failed to Retreive IP address", ex);
        }
        return "";
    }

    @SuppressWarnings("WeakerAccess")
    public static boolean isIPv4Address(String hostname) {
        String[] ipSections = hostname.split("\\.");
        if (ipSections.length != 4) {
            return false;
        }
        for (String ipSection : ipSections) {
            try {
                int num = Integer.parseInt(ipSection);
                if (num < 0 || num > 255) {
                    return false;
                }
            } catch (NumberFormatException e) {
                return false;
            }
        }
        return true;
    }

    @SuppressWarnings({"ResultOfMethodCallIgnored", "WeakerAccess"})
    public static void deleteRecursive(File fileOrDirectory) {
        if (!fileOrDirectory.exists()) return;
        if (fileOrDirectory.isDirectory()) {
            File[] files = fileOrDirectory.listFiles();
            if (files == null) {
                files = new File[0];
            }
            for (File child : files) {
                deleteRecursive(child);
            }
        }

        //noinspection ResultOfMethodCallIgnored
        fileOrDirectory.delete();
    }

    @SuppressWarnings("unchecked")
    public static JSONArray toJSONArray(ArrayList<Object> array) {
        JSONArray jsonArray = new JSONArray();
        for (Object obj : array) {
            if (obj instanceof ArrayList) {
                jsonArray.put(toJSONArray((ArrayList<Object>) obj));
            } else if (obj instanceof JSONObject) {
                jsonArray.put(obj);
            } else {
                jsonArray.put(String.valueOf(obj));
            }
        }
        return jsonArray;
    }

    @SuppressWarnings("unchecked")
    public static JSONObject toJSON(Bundle bundle) {
        JSONObject json = new JSONObject();

        try {
            if (bundle != null) {
                Set<String> keys = bundle.keySet();

                for (String key : keys) {
                    Object value = bundle.get(key);

                    if (value == null) {
                        json.put(key, JSONObject.NULL);
                    } else if (value instanceof ArrayList) {
                        json.put(key, toJSONArray((ArrayList<Object>) value));
                    } else if (value instanceof Bundle) {
                        json.put(key, toJSON((Bundle) value));
                    } else {
                        json.put(key, String.valueOf(value));
                    }
                }
            }
        } catch (Exception ignored) {
        }

        return json;
    }

    public static JSONObject optJSONObject(JSONObject parent, String key) {
        JSONObject result = parent.optJSONObject(key);
        return result == null ? new JSONObject() : result;
    }

    public static JSONArray optJSONArray(JSONObject parent, String key) {
        JSONArray result = parent.optJSONArray(key);
        return result == null ? new JSONArray() : result;
    }

    public static <T> boolean includes(JSONArray array, T element) {
        if (array == null) {
            return false;
        }

        for (int idx = 0; idx < array.length(); idx++) {
            if (element.equals(array.opt(idx))) {
                return true;
            }
        }

        return false;
    }

    public static String getLogLevelFromThrowable(Throwable throwable) {
        if (throwable instanceof Error) {
            return LogLevel.CRITICAL;
        } else if (throwable instanceof Exception) {
            return LogLevel.ERROR;
        } else {
            return LogLevel.WARNING;
        }
    }

    public static JSONObject defaultNonNull(@Nullable JSONObject jsonObject) {
        return jsonObject == null ? new JSONObject() : jsonObject;
    }

    public static JSONArray defaultNonNull(@Nullable JSONArray jsonArray) {
        return jsonArray == null ? new JSONArray() : jsonArray;
    }

    public static boolean contains(@NonNull JSONArray jsonArray, Object element) {
        try {
            for (int i = 0; i < jsonArray.length(); i++) {
                if (jsonArray.get(i).equals(element)) {
                    return true;
                }
            }
        } catch (JSONException e) {
            // Ignored
        }
        return false;
    }


    public static byte[] gzipContent(byte[] uncompressed) throws IOException {
        try (ByteArrayOutputStream os = new ByteArrayOutputStream(uncompressed.length)) {
            try (GZIPOutputStream gzos = new GZIPOutputStream(os)) {
                gzos.write(uncompressed);
            }
            JuspayLogger.d(LOG_TAG, "Gzipping complete");
            return os.toByteArray();
        }
    }

    public static ActivityManager.MemoryInfo getMemoryInfo(Context context) {
        try {
            ActivityManager.MemoryInfo memoryInfo = new ActivityManager.MemoryInfo();
            ActivityManager am = null;
            if (context != null) {
                am = (ActivityManager) context.getSystemService(Context.ACTIVITY_SERVICE);
            }
            if (am != null) {
                am.getMemoryInfo(memoryInfo);
                return memoryInfo;
            }
        } catch (Exception exception) {
            // Ignore
        }
        return null;
    }
}
