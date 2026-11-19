package in.juspay.mobility.app;

import android.content.Context;
import android.graphics.Color;

import org.json.JSONObject;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Iterator;

public class SheetTheme {
    private static final HashMap<String, SheetTheme> themes = new HashMap<>();
    private int primaryBackground;
    private int primaryText;
    private int secondaryText;
    private int secondaryBackground;

    private SheetTheme(String primaryBackground, String primaryText, String secondaryText, String secondaryBackground) {
        this.primaryBackground = Color.parseColor(primaryBackground);
        this.primaryText = Color.parseColor(primaryText);
        this.secondaryText = Color.parseColor(secondaryText);
        this.secondaryBackground = Color.parseColor(secondaryBackground);
    }

    public int getPrimaryBackground() {
        return primaryBackground;
    }

    public void setPrimaryBackground(int primaryBackground) {
        this.primaryBackground = primaryBackground;
    }

    public int getPrimaryText() {
        return primaryText;
    }

    public void setPrimaryText(int primaryText) {
        this.primaryText = primaryText;
    }

    public int getSecondaryBackground() {
        return secondaryBackground;
    }

    public void setSecondaryBackground(int secondaryBackground) {
        this.secondaryBackground = secondaryBackground;
    }

    public int getSecondaryText() {
        return secondaryText;
    }

    public void setSecondaryText(int secondaryText) {
        this.secondaryText = secondaryText;
    }

    public static SheetTheme getTheme(String rideType, Context context) {
        if (themes.isEmpty()) {
            JSONObject config = new JSONObject();
            try {
                StringBuilder returnString = new StringBuilder();
                File cache = new File(context.getDir("juspay", Context.MODE_PRIVATE), RemoteAssetsDownloader.appendSdkNameAndVersion("ride_request_theme.json", context));
                if (cache.exists()) {
                    FileInputStream fis = new FileInputStream(cache);
                    int num;
                    while ((num = fis.read()) != -1) {
                        returnString.append((char) num);
                    }
                } else {
                    InputStream is = context.getAssets().open("juspay/ride_request_theme.json");
                    ByteArrayOutputStream bos = new ByteArrayOutputStream();
                    byte[] buffer = new byte[4096];
                    int read;

                    while ((read = is.read(buffer)) != -1) {
                        bos.write(buffer, 0, read);
                    }
                    returnString.append(bos.toString());
                }
                config = new JSONObject(returnString.toString());
            } catch (Exception e) {

            }

            Iterator<String> keys = config.keys();
            while (keys.hasNext()) {
                String key = keys.next();
                JSONObject object = config.optJSONObject(key);
                if (object != null) {
                    themes.put(key, new SheetTheme(object.optString("primaryBackground", "#FFFFFF"), object.optString("primaryText", "#454545"), object.optString("secondaryText", "#868B98"), object.optString("secondaryBackground", "#E5E7EB")));
                }
            }
        }
        SheetTheme theme = themes.get(rideType);
        if (theme != null) {
            return theme;
        } else {
            return new SheetTheme("#FFFFFF", "#454545", "#868B98", "#E5E7EB");
        }
    }

}
