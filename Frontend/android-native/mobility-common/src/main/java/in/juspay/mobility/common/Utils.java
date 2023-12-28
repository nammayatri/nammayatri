package in.juspay.mobility.common;

import android.app.ActivityManager;
import android.content.Context;
import android.content.Intent;
import android.content.res.Configuration;
import android.os.Handler;
import android.util.Log;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.TimeZone;


public class Utils {

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
            case "TE_IN" :
                locale = new Locale("te");
                break;
            default:
                return;
        }
        Locale.setDefault(locale);
        Configuration configuration = context.getResources().getConfiguration();
        configuration.setLocale(locale);
        context.getResources().updateConfiguration(configuration, context.getResources().getDisplayMetrics());
    }

    public static String getUTCTimeStampFromMills(long time) {
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'", new Locale("en", "US"));
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
        Date locTime = new Date(time);
        return sdf.format(locTime);
    }



}
