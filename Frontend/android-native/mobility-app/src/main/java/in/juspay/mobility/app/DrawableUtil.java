package in.juspay.mobility.app;

import android.content.res.ColorStateList;
import android.graphics.Color;
import android.graphics.drawable.GradientDrawable;
import android.graphics.drawable.RippleDrawable;

public class DrawableUtil {

    /**
     * Creates a GradientDrawable with the specified color and corner radius.
     *
     * @param color         The background color (e.g., Color.RED or 0xFFFF0000).
     * @param cornerRadius  The corner radius in pixels.
     * @return A configured GradientDrawable.
     */
    public static GradientDrawable createRoundedDrawable(int color, float cornerRadius, int strokeWidth, int strokeColor) {
        GradientDrawable drawable = new GradientDrawable();
        drawable.setShape(GradientDrawable.RECTANGLE);
        drawable.setColor(color);
        if (strokeWidth != -1 && strokeColor != -1) {
            drawable.setStroke(strokeWidth,strokeColor);
        }
        drawable.setCornerRadius(cornerRadius);
        return drawable;
    }

    public static GradientDrawable createRoundedDrawable(int color, float cornerRadius) {
        return createRoundedDrawable(color,cornerRadius, -1,-1);
    }

    /**
     * Creates a RippleDrawable with the specified color and corner radius.
     *
     * @param color         The background color (e.g., Color.RED or 0xFFFF0000).
     * @param cornerRadius  The corner radius in pixels.
     * @return A configured GradientDrawable.
     */
    public static RippleDrawable createRippleDrawable(int color, float cornerRadius, int strokeWidth, int strokeColor) {
        return new RippleDrawable(ColorStateList.valueOf(Color.parseColor("#8e8e8e")),createRoundedDrawable(color,cornerRadius, strokeWidth,strokeColor),null);
    }
}
