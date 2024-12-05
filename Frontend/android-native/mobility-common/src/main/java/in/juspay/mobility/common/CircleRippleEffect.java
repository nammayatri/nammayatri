package in.juspay.mobility.common;

import android.animation.Animator;
import android.animation.ArgbEvaluator;
import android.animation.FloatEvaluator;
import android.animation.ValueAnimator;
import android.graphics.Color;
import android.util.Log;
import android.view.animation.LinearInterpolator;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.google.android.gms.maps.CameraUpdateFactory;
import com.google.android.gms.maps.GoogleMap;
import com.google.android.gms.maps.model.Circle;
import com.google.android.gms.maps.model.CircleOptions;
import com.google.android.gms.maps.model.Dash;
import com.google.android.gms.maps.model.Dot;
import com.google.android.gms.maps.model.Gap;
import com.google.android.gms.maps.model.LatLng;
import com.google.android.gms.maps.model.PatternItem;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import in.juspay.hyper.core.BridgeComponents;
import in.juspay.hyper.core.ExecutorManager;

public class CircleRippleEffect {

    private final int mId;
    @NonNull
    private final CircleRippleEffectOptions mOptions;
    private final ArgbEvaluator mArgbEvaluator = new ArgbEvaluator();
    private final FloatEvaluator mFloatEvaluator = new FloatEvaluator();
    private Circle lastClickedCircle = null;
    @Nullable private Circle circle;
    @Nullable private ValueAnimator mAnimator;

    public CircleRippleEffect(int id, @NonNull CircleRippleEffectOptions options) {
        this.mOptions = options;
        this.mId = id;
    }

    public void updatePosition(LatLng center) {
        if (circle != null) {
            ExecutorManager.runOnMainThread(()-> circle.setCenter(center));
        }
    }

    public void draw(@NonNull GoogleMap googleMap, LatLng center, BridgeComponents bridgeComponents, String callback) {
        ExecutorManager.runOnMainThread(() -> {
            this.circle = googleMap.addCircle(new CircleOptions()
                    .center(center)
                    .fillColor(mOptions.getFillColor())
                    .strokeColor(Color.parseColor(mOptions.getFromStrokeColor()))
                    .strokeWidth(mOptions.getStrokeWidth())
                    .visible(true)
                    .radius(mOptions.getRadius()));
            updateStrokePatternForCircle(mOptions);
            if (mOptions.getIsCircleClickable()) {
                this.circle.setClickable(true);
                googleMap.setOnCircleClickListener(clickedCircle -> {
                    if (lastClickedCircle != null) {
                        resetCircleStroke(lastClickedCircle);
                    }
                    changeCircleStroke(clickedCircle);
                    LatLng circleCenter = clickedCircle.getCenter();
                    String circleColor = "#" + Integer.toHexString(clickedCircle.getFillColor()).substring(2);
                    lastClickedCircle = clickedCircle;
                    if (callback != null && bridgeComponents != null) {
                        String javascript = String.format("window.callUICallback('%s','%s','%s','%s');", callback, circleCenter.latitude, circleCenter.longitude, circleColor);
                        googleMap.animateCamera(CameraUpdateFactory.newLatLngZoom(circleCenter, 14), 400, null);
                        bridgeComponents.getJsCallback().addJsToWebView(javascript);
                    }
                });
            }
        });
    }

    private void changeCircleStroke(Circle circle) {
        List<PatternItem> dashedPattern = Arrays.asList(
                new Dash(10),
                new Gap(0)
        );
        circle.setStrokePattern(dashedPattern);
    }

    private void resetCircleStroke(Circle circle) {
        List<PatternItem> dashedPattern = Arrays.asList(
                new Dash(20),  // Length of dash
                new Gap(20)    // Length of gap
        );
        circle.setStrokePattern(dashedPattern);
    }
    public void updateCircle (CircleRippleEffectOptions config) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                if (circle != null) {
                    if (config.getRadius() != 0) circle.setRadius(config.getRadius());
                    if (!config.getFromStrokeColor().equals(""))
                        circle.setStrokeColor(Color.parseColor(config.getFromStrokeColor()));
                    if (config.getFillColor() != 0) circle.setFillColor(config.getFillColor());
                    if (config.getStrokeWidth() != 0) circle.setStrokeWidth(config.getStrokeWidth());
                    updateStrokePatternForCircle(config);
                }
            } catch (Exception e) {
                Log.i("updateCircle error for ", String.valueOf(e));
            }
        });
    }

    public void updateStrokePatternForCircle(CircleRippleEffectOptions config) {
        CircleRippleEffectOptions.StrokePattern circleStroke = config.getStrokePattern();
        List<PatternItem> strokePattern = null;
        switch (circleStroke){
            case DASHED:
                strokePattern = Arrays.asList(
                        new Dash(20),
                        new Gap(20)
                );
                break;
            case DOTTED:
                strokePattern = Arrays.asList(
                        new Dot(),
                        new Gap(20)
                );
                break;
            default: break;
        }
        if(strokePattern != null) this.circle.setStrokePattern(strokePattern);
    }

    public void startAnimation() {
        mAnimator = ValueAnimator.ofFloat(0, mOptions.getMaxRadius(), 0);
        long mDelay = mOptions.getFactor() * mOptions.getDelay();
        if (mAnimator != null) {
            mAnimator.setStartDelay(mDelay);
            mAnimator.setRepeatCount(mOptions.getRepeatMode());
            mAnimator.setDuration(mOptions.getDuration() - mDelay);
            mAnimator.setInterpolator(new LinearInterpolator());
            mAnimator.addUpdateListener(animation -> ExecutorManager.runOnMainThread(() -> {
                if (circle != null) {
                    double radius = (mOptions.getRadius()) + Double.parseDouble(animation.getAnimatedValue().toString());
                    circle.setRadius(radius);
                    int color;
                    float stroke;
                    if (animation.getAnimatedFraction() < 0.5) {
                        float rangedFraction = getValueBtwRange(animation.getAnimatedFraction(), 0.0f, 0.5f, 0.0f, 1.0f);
                        color = (int) mArgbEvaluator.evaluate(rangedFraction, Color.parseColor(mOptions.getFromStrokeColor()), Color.parseColor(mOptions.getToStrokeColor()));
                        stroke = mFloatEvaluator.evaluate(rangedFraction, mOptions.getStrokeWidth(), mOptions.getMaxStrokeWidth());
                        circle.setStrokeColor(color);
                        circle.setStrokeWidth(stroke);
                    } else {
                        float rangedFraction = getValueBtwRange(animation.getAnimatedFraction(), 0.5f, 1.0f, 0.0f, 1.0f);
                        color = (int) mArgbEvaluator.evaluate(rangedFraction, Color.parseColor(mOptions.getToStrokeColor()), Color.parseColor(mOptions.getFromStrokeColor()));
                        stroke = mFloatEvaluator.evaluate(rangedFraction, mOptions.getMaxStrokeWidth(), mOptions.getStrokeWidth());
                    }
                    circle.setStrokeColor(color);
                    circle.setStrokeWidth(stroke);
                }
            }));
            mAnimator.addListener(new Animator.AnimatorListener() {
                @Override
                public void onAnimationStart(@NonNull Animator animation) {

                }

                @Override
                public void onAnimationEnd(@NonNull Animator animator) {
                }

                @Override
                public void onAnimationCancel(@NonNull Animator animation) {

                }

                @Override
                public void onAnimationRepeat(@NonNull Animator animation) {
                    animation.pause();
                    animation.setStartDelay(mOptions.getFactor() * mOptions.getDelay() + mOptions.getPause());
                    animation.start();
                }
            });
            mAnimator.start();
        }
    }

    /**
     * Cancels the animation without removing the circle from map.
     */
    public void cancelAnimation() {
        ExecutorManager.runOnMainThread(() -> {
            if (mAnimator != null) {
                mAnimator.removeAllUpdateListeners();
                mAnimator.removeAllListeners();
                mAnimator.cancel();
            }
        });
    }

    /**
     * removes the circle from and cancels the animations
     */
    public void remove() {
        ExecutorManager.runOnMainThread(() -> {
            if (circle != null) {
                circle.remove();
            }
            cancelAnimation();
        });
    }

    private float getValueBtwRange(float x,
                                   float in_min,
                                   float in_max,
                                   float out_min,
                                   float out_max) {
        return (x - in_min) * (out_max - out_min) / (in_max - in_min) + out_min;
    }

}
