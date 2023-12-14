package in.juspay.mobility.app;

import static androidx.core.app.ActivityCompat.requestPermissions;

import android.app.Activity;
import android.content.Context;
import android.content.pm.PackageManager;
import android.webkit.JavascriptInterface;

import androidx.core.app.ActivityCompat;

public class PermissionUtils {

    private static final int GENERIC_REQUEST_CODE = 100;

    public static boolean checkIfPermissionsNotGranted(Context context, String[] requests) {
        for (int i = 0; i < requests.length; i++) {
            boolean isAllowed = ActivityCompat.checkSelfPermission(context, requests[i]) != PackageManager.PERMISSION_GRANTED;
            if (isAllowed)
                return true;
        }
        return false;
    }

    public static void askRequestedPermissions(Activity activity, Context context, String[] requests) {
        if(activity == null)
            return;
        if (checkIfPermissionsNotGranted(context, requests)) {
            requestPermissions(activity, requests, GENERIC_REQUEST_CODE);
        }
    }
}
