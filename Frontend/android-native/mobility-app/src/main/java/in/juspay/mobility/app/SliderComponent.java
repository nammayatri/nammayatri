package in.juspay.mobility.app;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.graphics.Color;
import android.graphics.drawable.Drawable;
import android.graphics.drawable.GradientDrawable;
import android.graphics.drawable.LayerDrawable;
import android.graphics.drawable.ShapeDrawable;
import android.graphics.drawable.shapes.OvalShape;
import android.os.Build;
import android.view.Gravity;
import android.view.View;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.SeekBar;
import android.widget.TextView;

import androidx.core.graphics.drawable.DrawableCompat;

import java.util.Locale;

import in.juspay.hyper.core.BridgeComponents;

public class SliderComponent {
    private SeekBar seekBar;

    public SliderComponent(){

    }

    private int dpToPx(Context context, int dp) {
        float density = context.getResources().getDisplayMetrics().density;
        return Math.round((float) dp * density);
    }

    private void updateTooltipPosition(View tooltipLayout, int x, int y, boolean check) {
        if (tooltipLayout != null) {

            LinearLayout.LayoutParams layoutParams = (LinearLayout.LayoutParams) tooltipLayout.getLayoutParams();
            layoutParams.leftMargin = x;
            layoutParams.topMargin = y;
            System.out.println("check " + check);
            if (check)
                tooltipLayout.setLayoutParams(layoutParams);
        }
    }

    public void updateSliderValue (int sliderValue){
        if (seekBar!=null && seekBar.getProgress() != sliderValue && sliderValue <= seekBar.getMax() && sliderValue >= 0){
            seekBar.setProgress(sliderValue);
        }
    }
    public void addSlider(String id, String callback, int stepFunctionForCoinConversion, float conversionRate, int minLimit, int maxLimit, int defaultValue, String toolTipId, Boolean enableToolTip, String progressColor, String thumbColor, String bgColor, int bgAlpha, Boolean getCallbackOnProgressChanged, BridgeComponents bridgeComponents){
        Activity activity = bridgeComponents.getActivity();
        Context context = bridgeComponents.getContext();
        if (activity != null) {
            LinearLayout layout = activity.findViewById(Integer.parseInt(id));
            LinearLayout toolTipView = activity.findViewById(Integer.parseInt(toolTipId));
            if (layout == null || (toolTipView == null && enableToolTip))
                return;
            seekBar = new SeekBar(context);
            ShapeDrawable thumbDrawable = new ShapeDrawable(new OvalShape());
            thumbDrawable.getPaint().setColor(Color.parseColor(thumbColor));
            thumbDrawable.setIntrinsicHeight(dpToPx(context, 20));
            thumbDrawable.setIntrinsicWidth(dpToPx(context, 20));
            seekBar.setThumb(thumbDrawable);
            LayerDrawable progressDrawable = (LayerDrawable) seekBar.getProgressDrawable().mutate();
            Drawable backgroundDrawable = progressDrawable.findDrawableByLayerId(android.R.id.background);
            Drawable progress = progressDrawable.findDrawableByLayerId(android.R.id.progress);
            if (backgroundDrawable != null) {
                DrawableCompat.setTint(backgroundDrawable, Color.parseColor(bgColor));
                backgroundDrawable.setAlpha(bgAlpha);
            }
            if (progress != null) {
                DrawableCompat.setTint(progress, Color.parseColor(progressColor));
            }
            seekBar.setProgressDrawable(progressDrawable);
            seekBar.setMax(maxLimit);
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                seekBar.setMin(minLimit);
            }
            seekBar.setProgress(defaultValue);
            LinearLayout.LayoutParams params = new LinearLayout.LayoutParams(
                    LinearLayout.LayoutParams.MATCH_PARENT,
                    LinearLayout.LayoutParams.WRAP_CONTENT
            );
            seekBar.setLayoutParams(params);
            LinearLayout tooltipLayout = new LinearLayout(context);
            GradientDrawable roundedBg = new GradientDrawable();
            roundedBg.setShape(GradientDrawable.RECTANGLE);
            roundedBg.setColor(Color.BLACK);
            roundedBg.setCornerRadius(dpToPx(context, 20));
            tooltipLayout.setBackground(roundedBg);
            tooltipLayout.setLayoutParams(new LinearLayout.LayoutParams(
                    LinearLayout.LayoutParams.WRAP_CONTENT,
                    LinearLayout.LayoutParams.WRAP_CONTENT));
            tooltipLayout.setPadding(22, 10, 22, 13);
            tooltipLayout.setGravity(Gravity.CENTER_VERTICAL);
            TextView prefTextView = new TextView(context);
            prefTextView.setLayoutParams(new LinearLayout.LayoutParams(
                    LinearLayout.LayoutParams.WRAP_CONTENT,
                    LinearLayout.LayoutParams.WRAP_CONTENT));
            prefTextView.setTextColor(Color.WHITE);
            TextView suffTextView = new TextView(context);
            suffTextView.setLayoutParams(new LinearLayout.LayoutParams(
                    LinearLayout.LayoutParams.WRAP_CONTENT,
                    LinearLayout.LayoutParams.WRAP_CONTENT));
            suffTextView.setTextColor(Color.WHITE);
            ImageView imageView = new ImageView(context);
            LinearLayout.LayoutParams imageParams = new LinearLayout.LayoutParams(30, 30);
            params.setMargins(10, 0, 0, 0);
            imageView.setLayoutParams(imageParams);
            imageView.setImageResource(R.drawable.ny_ic_yatri_coin);
            tooltipLayout.addView(prefTextView);
            tooltipLayout.addView(imageView);
            tooltipLayout.addView(suffTextView);
            int nearestMultiple = Math.round(seekBar.getProgress() / stepFunctionForCoinConversion) * stepFunctionForCoinConversion;
            float minVal = (nearestMultiple * conversionRate);
            String minValueToShow = Math.ceil(minVal) == Math.floor(minVal) ? String.valueOf((int)minVal) : String.valueOf(minVal);
            prefTextView.setText(String.valueOf(nearestMultiple));
            suffTextView.setText(" = €" + minValueToShow);
            int seekBarPosition = seekBar.getThumb().getBounds().centerX();
            int thumbWidth = thumbDrawable.getIntrinsicWidth();
            final int[] tooltipX = {seekBarPosition - thumbWidth + dpToPx(context, 12)};
            int tooltipY = seekBar.getHeight() - tooltipLayout.getHeight();
            if (enableToolTip) {
                toolTipView.addView(tooltipLayout);
                updateTooltipPosition(tooltipLayout, tooltipX[0], tooltipY, true);
            }
            layout.addView(seekBar);
            seekBar.setOnSeekBarChangeListener(new SeekBar.OnSeekBarChangeListener() {
                @Override
                public void onProgressChanged(SeekBar seekBar, int progress, boolean fromUser) {
                    int thumbPosition = seekBar.getThumb().getBounds().centerX();
                    if (enableToolTip){
                        tooltipX[0] = thumbPosition - thumbWidth + dpToPx(context, 12);
                        updateTooltipPosition(tooltipLayout, tooltipX[0], tooltipY, (int) (toolTipView.getX()+toolTipView.getWidth()) > tooltipX[0] + tooltipLayout.getWidth());
                    }
                    float newVal = (progress * conversionRate);
                    String valueToShow = Math.ceil(newVal) == Math.floor(newVal) ? String.valueOf((int) newVal) : String.format("%.2f", newVal);
                    prefTextView.setText(String.valueOf(progress));
                    suffTextView.setText(" = €" + valueToShow);
                    int nearestMultiple = Math.round(seekBar.getProgress() / stepFunctionForCoinConversion) * stepFunctionForCoinConversion;
                    String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                            callback, nearestMultiple);
                    if (getCallbackOnProgressChanged){
                        bridgeComponents.getJsCallback().addJsToWebView(javascript);}
                }

                @Override
                public void onStartTrackingTouch(SeekBar seekBar) {
                    int thumbPosition = seekBar.getThumb().getBounds().centerX();
                    if (enableToolTip){
                        tooltipX[0] = thumbPosition - thumbWidth + dpToPx(context, 12);
                        updateTooltipPosition(tooltipLayout, tooltipX[0], tooltipY, (int) (toolTipView.getX()+toolTipView.getWidth()) > tooltipX[0] + tooltipLayout.getWidth());
                    }
                    }

                @Override
                public void onStopTrackingTouch(SeekBar seekBar) {
                    int thumbPosition = seekBar.getThumb().getBounds().centerX();
                    if(enableToolTip){
                        tooltipX[0] =thumbPosition - thumbWidth + dpToPx(context, 12);
                        updateTooltipPosition(tooltipLayout, tooltipX[0], tooltipY, (int) (toolTipView.getX()+toolTipView.getWidth()) > tooltipX[0] + tooltipLayout.getWidth());
                    }
                    int nearestMultiple = Math.round(seekBar.getProgress() / stepFunctionForCoinConversion) * stepFunctionForCoinConversion;
                    seekBar.setProgress(nearestMultiple);
                    String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                            callback, nearestMultiple);
                    if (!getCallbackOnProgressChanged)
                        bridgeComponents.getJsCallback().addJsToWebView(javascript);
                }
            });

            seekBar.post(() -> {
                int seekBarPosition1 = seekBar.getThumb().getBounds().centerX();
                int thumbWidth1 = thumbDrawable.getIntrinsicWidth();
                int tooltipX1 = seekBarPosition1 - thumbWidth1 / 2 + dpToPx(context, 12);
                int tooltipY1 = seekBar.getHeight() - tooltipLayout.getHeight();
                updateTooltipPosition(tooltipLayout, tooltipX1, tooltipY1 + 5, true);

                int nearestMultiple1 = Math.round(seekBar.getProgress() / stepFunctionForCoinConversion) * stepFunctionForCoinConversion;
                seekBar.setProgress(nearestMultiple1);
            });

        }
    }
}
