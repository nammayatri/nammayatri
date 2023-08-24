package in.juspay.mobility.app.carousel;

import android.view.View;
import android.widget.LinearLayout;

public class ViewPagerItem {
    int imageID;
    String heading, description;
//    LinearLayout view;

    public ViewPagerItem(int imageID, String heading, String description) {
        this.imageID = imageID;
        this.heading = heading;
        this.description = description;
//        this.view = view;
    }
}