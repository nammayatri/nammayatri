package in.juspay.mobility.app.carousel;

import org.json.JSONObject;

public class ViewPagerItem {
    int imageID;
    JSONObject imageConfig, descriptionConfig, titleConfig;

    public ViewPagerItem(int imageID, JSONObject imageConfig, JSONObject descriptionConfig, JSONObject titleConfig) {
        this.imageID = imageID;
        this.imageConfig =  imageConfig;
        this.descriptionConfig = descriptionConfig;
        this.titleConfig = titleConfig;
    }
}