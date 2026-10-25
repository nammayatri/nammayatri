package in.juspay.mobility.common;

import android.animation.ValueAnimator;

import androidx.annotation.NonNull;

import com.google.android.gms.maps.model.Circle;

public class CircleRippleEffectOptions implements Cloneable {
    public enum StrokePattern {
        NORMAL, DOTTED, DASHED
    }
    private long mDelay = 100, mDuration = 1000, mPause = 1;
    private int mRepeatMode = ValueAnimator.INFINITE;
    private float mMaxRadius = 5, mRadius = 10.0f;
    private int mFactor = 0;
    private float mStrokeWidth = 5.0f, mMaxStrokeWidth = 8.0f;
    private String mFromStrokeColor = "#000000", mToStrokeColor = "#000000";
    private int fillColor = 0;
    private StrokePattern strokePattern = StrokePattern.NORMAL;
    private boolean isCircleClickable = false;
    public float getStrokeWidth() {
        return mStrokeWidth;
    }

    public CircleRippleEffectOptions strokeWidth(float strokeWidth) {
        this.mStrokeWidth = strokeWidth;
        return this;
    }

    public Integer getFillColor () { return fillColor; }

    public CircleRippleEffectOptions fillColor(int color) {
        this.fillColor = color;
        return this;
    }

    public CircleRippleEffectOptions isCircleClickable(boolean isCircleClickable){
        this.isCircleClickable = isCircleClickable;
        return this;
    }
    public CircleRippleEffectOptions strokePattern(String pattern) {
        this.strokePattern = pattern.equals("NORMAL") ? StrokePattern.NORMAL: pattern.equals("DASHED") ? StrokePattern.DASHED : StrokePattern.DOTTED;
        return this;
    }

    public boolean getIsCircleClickable() { return isCircleClickable;}
    public StrokePattern getStrokePattern() { return strokePattern;}
    public float getMaxStrokeWidth() {
        return mMaxStrokeWidth;
    }

    public CircleRippleEffectOptions maxStrokeWidth(float maxStrokeWidth) {
        this.mMaxStrokeWidth = maxStrokeWidth;
        return this;
    }

    public String getFromStrokeColor() {
        return mFromStrokeColor;
    }

    public CircleRippleEffectOptions fromStrokeColor(String fromStrokeColor) {
        this.mFromStrokeColor = fromStrokeColor;
        return this;
    }

    public String getToStrokeColor() {
        return mToStrokeColor;
    }

    public CircleRippleEffectOptions toStrokeColor(String toStrokeColor) {
        this.mToStrokeColor = toStrokeColor;
        return this;
    }

    public long getDelay() {
        return mDelay;
    }

    /**
     * Delay in start of animation, for each animation cycle in ms.
     */
    public CircleRippleEffectOptions delay(long delay) {
        this.mDelay = delay;
        return this;
    }

    public long getPause() {
        return mPause;
    }

    public CircleRippleEffectOptions pause(long pause) {
        this.mPause = pause;
        return this;
    }

    public long getDuration() {
        return mDuration;
    }

    public CircleRippleEffectOptions duration(long duration) {
        this.mDuration = duration;
        return this;
    }

    public int getRepeatMode() {
        return mRepeatMode;
    }

    public CircleRippleEffectOptions repeatMode(int repeatMode) {
        this.mRepeatMode = repeatMode;
        return this;
    }

    public float getMaxRadius() {
        return mMaxRadius;
    }

    public CircleRippleEffectOptions maxRadius(float maxRadius) {
        this.mMaxRadius = maxRadius;
        return this;
    }

    public int getFactor() {
        return mFactor;
    }

    /**
     * If multiple circles are drawn assign the index of the current circle to the mfactor.
     */

    public CircleRippleEffectOptions factor(int factor) {
        this.mFactor = factor;
        return this;
    }

    public float getRadius() {
        return mRadius;
    }

    public CircleRippleEffectOptions radius(float radius) {
        this.mRadius = radius;
        return this;
    }


    @NonNull
    @Override
    public CircleRippleEffectOptions clone() {
        try {
            return (CircleRippleEffectOptions) super.clone();
        } catch (CloneNotSupportedException e) {
            throw new AssertionError();
        }
    }
}