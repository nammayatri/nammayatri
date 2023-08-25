package in.juspay.mobility.common;

import android.app.ActivityManager;
import android.content.Context;
import android.content.Intent;
import android.content.res.Configuration;
import android.os.Handler;
import android.util.Log;

import java.util.List;
import java.util.Locale;

import in.juspay.mobility.app.LocationUpdateService;

public class Utils {

    public static boolean isServiceRunning(String serviceClassName, Context context){
        final ActivityManager activityManager = (ActivityManager) context.getSystemService(Context.ACTIVITY_SERVICE);
        final List<ActivityManager.RunningServiceInfo> services = activityManager.getRunningServices(Integer.MAX_VALUE);

        for (ActivityManager.RunningServiceInfo runningServiceInfo : services) {
            if (runningServiceInfo.service.getClassName().equals(serviceClassName)){
                return true;
            }
        }
        return false;
    }

    public static void updateLocaleResource(String languageKey, Context context) {
        Locale locale;
        switch (languageKey) {
            case "HI_IN":
                locale = new Locale("hi");
                break;
            case "KN_IN":
                locale = new Locale("kn");
                break;
            case "EN_US":
                locale = new Locale("en");
                break;
            case "TA_IN":
                locale = new Locale("ta");
                break;
            case "BN_IN":
                locale = new Locale("bn");
                break;
            case "ML_IN":
                locale = new Locale("ml");
                break;
            case "FR_FR":
                locale = new Locale("fr");
                break;
            default:
                return;
        }
        Locale.setDefault(locale);
        Configuration configuration = context.getResources().getConfiguration();
        configuration.setLocale(locale);
        context.getResources().updateConfiguration(configuration, context.getResources().getDisplayMetrics());
    }

    public static boolean stopLocationService (Context context) {
            Intent locationUpdateService = new Intent(context, LocationUpdateService.class);
            if (in.juspay.mobility.common.Utils.isServiceRunning(LocationUpdateService.class.getName(), context)) {
                Handler stopServiceHandler = new Handler(context.getMainLooper());
                stopServiceHandler.postDelayed(() -> {
                    context.stopService(locationUpdateService);
                }, 5000);
                return true;
            } else {
                return false;
        }
    }

}
