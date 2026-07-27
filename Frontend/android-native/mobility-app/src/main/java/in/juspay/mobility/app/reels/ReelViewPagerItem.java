package in.juspay.mobility.app.reels;

import org.json.JSONArray;
import org.json.JSONObject;

public class ReelViewPagerItem {
    public JSONObject reelVideoConfig;
    public ReelViewPagerItem(JSONObject reelVideoConfig){
       this.reelVideoConfig = reelVideoConfig;
    }

    public String getVideoUrl() { return reelVideoConfig.isNull("videoUrl") ? "" : reelVideoConfig.optString("videoUrl", "");}
    public String getThumbnailImageUrl() { return reelVideoConfig.isNull("thumbnailImageUrl") ? "" : reelVideoConfig.optString("thumbnailImageUrl", "");}
    public String getReelViewPagerItemId() { return reelVideoConfig.isNull("id") ? "" : reelVideoConfig.optString("id", ""); }
    public String getTitle() { return reelVideoConfig.isNull("title") ? "" : reelVideoConfig.optString("title", "");}
    public String getDescription() { return reelVideoConfig.isNull("description") ? "" : reelVideoConfig.optString("description", "");}
    public String getShareLink() { return reelVideoConfig.isNull("shareLink") ? "" : reelVideoConfig.optString("shareLink", "");}
    public String getCarouselBigImageUrl() { return reelVideoConfig.isNull("carouselBigImageUrl") ? "" : reelVideoConfig.optString("carouselBigImageUrl", "");}
    public String getCarouselSmallImageUrl() { return reelVideoConfig.isNull("carouselSmallImageUrl") ? "" : reelVideoConfig.optString("carouselSmallImageUrl", "");}
    public String getCarouselTextString() { return reelVideoConfig.isNull("carouselTextString") ? "" : reelVideoConfig.optString("carouselTextString", "");}
    public String getCarouselTextColor() { return reelVideoConfig.isNull("videoUrl") ? "" : reelVideoConfig.optString("carouselTextColor", "");}
    public String getBottomButtonConfig() {return reelVideoConfig.isNull("bottomButtonConfig") ? "[[]]" : reelVideoConfig.optString("bottomButtonConfig", "[[]]");}
    public String getSideButtonConfig() {return reelVideoConfig.isNull("sideButtonConfig") ? "[[]]" : reelVideoConfig.optString("sideButtonConfig", "[[]]");}
    public JSONObject getThresholdConfig() { return reelVideoConfig.isNull("thresholdConfig") ? new JSONObject() : reelVideoConfig.optJSONObject("thresholdConfig");}
}
