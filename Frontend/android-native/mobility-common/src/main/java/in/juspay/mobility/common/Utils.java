package in.juspay.mobility.common;

import android.content.Context;
import android.content.res.Configuration;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;

import org.json.JSONObject;

import java.text.SimpleDateFormat;
import java.util.Date;
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

    public static CircleRippleEffectOptions getCircleOptionsFromJSON(JSONObject config, CircleRippleEffectOptions defOptions) {
        long delay = config.optLong("delay", defOptions.getDelay());
        long duration = config.optLong("duration", defOptions.getDuration());
        long pause = config.optLong("pause", defOptions.getPause());
        int repeatMode = config.optInt("repeatMode", defOptions.getRepeatMode());
        double maxRadius = config.optDouble("maxRadius", defOptions.getMaxRadius());
        double radius = config.optDouble("radius", defOptions.getRadius());
        double strokeWidth = config.optDouble("strokeWidth", defOptions.getStrokeWidth());
        double maxStrokeWidth = config.optDouble("maxStrokeWidth", defOptions.getMaxStrokeWidth());
        String fromStrokeColor = config.optString("fromStrokeColor", defOptions.getFromStrokeColor());
        String toStrokeColor = config.optString("toStrokeColor", fromStrokeColor);
        defOptions.radius((float) radius)
                .delay(delay)
                .duration(duration)
                .repeatMode(repeatMode)
                .pause(pause)
                .maxRadius((float) maxRadius)
                .strokeWidth((float) strokeWidth)
                .maxStrokeWidth((float) maxStrokeWidth)
                .fromStrokeColor(fromStrokeColor)
                .toStrokeColor(toStrokeColor);
        return defOptions;
    }


    public static Bitmap drawableToBitmap (Drawable drawable) {
        Bitmap bitmap = null;

        if (drawable instanceof BitmapDrawable) {
            BitmapDrawable bitmapDrawable = (BitmapDrawable) drawable;
            if(bitmapDrawable.getBitmap() != null) {
                return bitmapDrawable.getBitmap();
            }
        }

        if(drawable.getIntrinsicWidth() <= 0 || drawable.getIntrinsicHeight() <= 0) {
            bitmap = Bitmap.createBitmap(1, 1, Bitmap.Config.ARGB_8888); // Single color bitmap will be created of 1x1 pixel
        } else {
            bitmap = Bitmap.createBitmap(drawable.getIntrinsicWidth(), drawable.getIntrinsicHeight(), Bitmap.Config.ARGB_8888);
        }

        Canvas canvas = new Canvas(bitmap);
        drawable.setBounds(0, 0, canvas.getWidth(), canvas.getHeight());
        drawable.draw(canvas);
        return bitmap;
    }


}
