package in.juspay.mobility.app.carousel;

public class ViewPagerItem {
    float descriptionTextSize, titleTextSize ;
    int imageID, imageHeight, imageWidth;
    String heading, description, titleColor, descriptionColor;

    public ViewPagerItem(int imageID, String heading, String description, int imageHeight, int imageWidth, String titleColor, float titleTextSize, String descriptionColor, float descriptionTextSize ) {
        this.imageID = imageID;
        this.heading = heading;
        this.description = description;
        this.imageHeight = imageHeight;
        this.imageWidth = imageWidth;
        this.titleColor = titleColor;
        this.titleTextSize = titleTextSize;
        this.descriptionColor = descriptionColor;
        this.descriptionTextSize = descriptionTextSize;
    }
}