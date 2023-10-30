package in.juspay.mobility.app.carousel;

import android.util.Log;

import org.json.JSONException;
import org.json.JSONObject;

public class ViewPagerItem {
    int imageID, gravity;
    JSONObject imageConfig, descriptionConfig, titleConfig, videoData;
    String contentType;

    public ViewPagerItem(int imageID, JSONObject imageConfig, JSONObject descriptionConfig, JSONObject titleConfig, String contentType, JSONObject videoData , int gravity) {
        this.imageID = imageID;
        this.imageConfig =  imageConfig;
        this.descriptionConfig = descriptionConfig;
        this.titleConfig = titleConfig;
        this.videoData = videoData;
        this.contentType = contentType;
        this.gravity = gravity;
    }


    public ViewPagerItem(int imageID, String heading, String description) {
        this.imageID = imageID;
        try {
            titleConfig = new JSONObject().put("text", heading);
            descriptionConfig = new JSONObject().put("text", description);
        } catch (JSONException e) {
            Log.e("ViewPagerItem","Exception in ViewPagerItem");
        }
    }



    public int getImageHeight() { return imageConfig.optInt("height", 260);}

    public String getImageBgColor() { return imageConfig.optString("bgColor", "#FFFFFF");}

    public JSONObject getDescriptionMargin() {return descriptionConfig.optJSONObject("margin");}

    public String getDescriptionColor() {return descriptionConfig.optString("textColor", "#000000");}

    public int getDescriptionTextSize() {return descriptionConfig.optInt("textSize", 14);}

    public String getDescriptionText() {return descriptionConfig.optString("text", "");}

    public String getDescriptionGravity() {return descriptionConfig.optString("gravity" , "CENTER");}

    public JSONObject getTitleMargin() {return titleConfig.optJSONObject("margin");}

    public String getTitleColor() {return titleConfig.optString("textColor", "#000000");}

    public int getTitleTextSize() {return titleConfig.optInt("textSize", 14);}

    public String getTitleText() {return titleConfig.optString("text", "");}

    public String getTitleGravity() {return titleConfig.optString("gravity" , "CENTER");}

    public JSONObject getVideoData() { return videoData;}

    public String getContentType() { return contentType;}

    public int getImageID() { return imageID;}

    public int getCarouselGravity() {return gravity;}


}