package in.juspay.mobility.app;

import android.graphics.drawable.GradientDrawable;

public class DrawableUtil {

    /**
     * Creates a GradientDrawable with the specified color and corner radius.
     *
     * @param color         The background color (e.g., Color.RED or 0xFFFF0000).
     * @param cornerRadius  The corner radius in pixels.
     * @return A configured GradientDrawable.
     */
    public static GradientDrawable createRoundedDrawable(int color, float cornerRadius) {
        GradientDrawable drawable = new GradientDrawable();
        drawable.setShape(GradientDrawable.RECTANGLE);
        drawable.setColor(color);
        drawable.setCornerRadius(cornerRadius);
        return drawable;
    }
}
