package in.juspay.mobility.app.reels;

import org.json.JSONObject;

public class ReelViewPagerItem {
    public JSONObject reelVideoConfig;
    public ReelViewPagerItem(JSONObject reelVideoConfig){
       this.reelVideoConfig = reelVideoConfig;
    }

    public String getVideoUrl() { return reelVideoConfig.optString("videoUrl", "");}
    public String getThumbnailImageUrl() { return reelVideoConfig.optString("thumbnailImageUrl", "");}
    public String getReelViewPagerItemId() { return reelVideoConfig.optString("id", "");}
    public String getTitle() { return reelVideoConfig.optString("title", "");}
    public String getDescription() { return reelVideoConfig.optString("description", "");}
    public String getShareLink() { return reelVideoConfig.optString("shareLink", "");}
    public String getCarouselBigImageUrl() { return reelVideoConfig.optString("carouselBigImageUrl", "");}
    public String getCarouselSmallImageUrl() { return reelVideoConfig.optString("carouselSmallImageUrl", "");}
    public String getCarouselTextString() { return reelVideoConfig.optString("carouselTextString", "");}
    public String getCarouselTextColor() { return reelVideoConfig.optString("carouselTextColor", "");}

    public String getBottomButtonConfig() {return reelVideoConfig.optString("bottomButtonConfig", "[[]]");}
    public String getSideButtonConfig() {return reelVideoConfig.optString("sideButtonConfig", "[[]]");}

}
