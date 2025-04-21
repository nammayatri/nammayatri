package in.juspay.hypersdk.data;

import android.app.Activity;
import android.content.Context;
import android.content.SharedPreferences;

import androidx.annotation.NonNull;

import java.util.Map;

import in.juspay.hypersdk.core.JuspayServices;

/**
 * A class that provides an easy to use interface for using {@link SharedPreferences} in Android. This class uses the
 * application context to read and write the preferences.
 * <p>
 * <strong>Note:</strong> This class is the same across all the Juspay SDKs, and hence, if multiple SDKs are used in an
 * app, the store name will collide. This happens even for micro-apps. To prevent that, we now take a namespace along
 * with a key.
 *
 * @author Sri Harsha Chilakapati [sri.harsha@juspay.in]
 * @author Veera Manohara Subbiah [veera.subbiah@juspay.in]
 * @since 18/04/2017
 */
public final class KeyValueStore {
    private KeyValueStore() {
    }

    public static void write(JuspayServices juspayServices, String key, String value) {
        write(juspayServices.getContext(), juspayServices.getSdkInfo().getSdkName(), key, value);
    }

    public static void write(@NonNull Context context, String namespace, String key, String value, boolean commit) {

        SharedPreferences preferences = context.getSharedPreferences(namespace, Activity.MODE_PRIVATE);
        SharedPreferences.Editor editor = preferences.edit();
        editor.putString(key, value);
        if (commit)
            editor.commit();
        else
            editor.apply();
    }

    public static void write(@NonNull Context context, String namespace, String key, String value) {
        write(context, namespace, key, value, false);
    }

    public static boolean contains(JuspayServices juspayServices, String key) {
        return contains(juspayServices.getContext(), juspayServices.getSdkInfo().getSdkName(), key);
    }

    public static boolean contains(@NonNull Context context, String namespace, String key) {
        SharedPreferences preferences = context.getSharedPreferences(namespace, Activity.MODE_PRIVATE);
        return preferences.contains(key);
    }

    public static String read(JuspayServices juspayServices, String key, String defaultValue) {
        return read(juspayServices.getContext(), juspayServices.getSdkInfo().getSdkName(), key, defaultValue);
    }

    public static String read(@NonNull Context context, String namespace, String key, String defaultValue) {
        SharedPreferences preferences = context.getSharedPreferences(namespace, Activity.MODE_PRIVATE);
        return preferences.getString(key, defaultValue);
    }

    public static void remove(JuspayServices juspayServices, String key) {
        remove(juspayServices.getContext(), juspayServices.getSdkInfo().getSdkName(), key);
    }

    public static void remove(@NonNull Context context, String namespace, String key, boolean commit) {
        if (contains(context, namespace, key)) {
            SharedPreferences preferences = context.getSharedPreferences(namespace, Activity.MODE_PRIVATE);
            SharedPreferences.Editor editor = preferences.edit();
            editor.remove(key);
            if (commit) {
                editor.commit();
            } else {
                editor.apply();
            }
        }
    }

    public static void remove(@NonNull Context context, String namespace, String key) {
        remove(context, namespace, key, false);
    }

    public static Map<String, ?> getAll(JuspayServices juspayServices) {
        return getAll(juspayServices.getContext(), juspayServices.getSdkInfo().getSdkName());
    }

    public static Map<String, ?> getAll(@NonNull Context context, String namespace) {
        SharedPreferences preferences = context.getSharedPreferences(namespace, Activity.MODE_PRIVATE);
        return preferences.getAll();
    }
}
