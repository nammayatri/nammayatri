package in.juspay.mobility.app;

import static androidx.core.app.ActivityCompat.requestPermissions;

import android.app.Activity;
import android.content.Context;
import android.content.pm.PackageManager;
import android.webkit.JavascriptInterface;

import androidx.core.app.ActivityCompat;

public class PermissionUtils {

    private static final int GENERIC_REQUEST_CODE = 100;

    public interface PermissionCallback {
        void onPermissionsGranted();
        void onPermissionsDenied();
    }

    public static boolean checkIfAllPermissionsGranted(Context context, String[] requests) {
        for (String request : requests) {
            if (ActivityCompat.checkSelfPermission(context, request) != PackageManager.PERMISSION_GRANTED) {
                return false;
            }
        }
        return true;
    }

    // Pass the activity, context and the permissions to be requested as arguments . Eg - ["android.permission.CAMERA", "android.permission.READ_EXTERNAL_STORAGE"]
    public static void askRequestedPermissions(Activity activity, Context context, String[] requests, PermissionCallback callback) {
        if (activity == null) {
            return;
        }

        if (!checkIfAllPermissionsGranted(context, requests)) {
            requestPermissions(activity, requests, GENERIC_REQUEST_CODE, callback);
        } else if (callback != null) {
            callback.onPermissionsGranted();
        }
    }

    private static void requestPermissions(Activity activity, String[] requests, int requestCode, PermissionCallback callback) {
        ActivityCompat.requestPermissions(activity, requests, requestCode);
    }
}
