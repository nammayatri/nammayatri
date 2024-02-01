package in.juspay.mobility.common;

import android.animation.Animator;
import android.animation.ArgbEvaluator;
import android.animation.FloatEvaluator;
import android.animation.ValueAnimator;
import android.graphics.Color;
import android.view.animation.LinearInterpolator;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.google.android.gms.maps.GoogleMap;
import com.google.android.gms.maps.model.Circle;
import com.google.android.gms.maps.model.CircleOptions;
import com.google.android.gms.maps.model.LatLng;

import in.juspay.hyper.core.ExecutorManager;

public class CircleRippleEffect {

    private final int mId;
    @NonNull
    private final CircleRippleEffectOptions mOptions;
    private final ArgbEvaluator mArgbEvaluator = new ArgbEvaluator();
    private final FloatEvaluator mFloatEvaluator = new FloatEvaluator();
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

    public void draw(@NonNull GoogleMap googleMap, LatLng center) {
        ExecutorManager.runOnMainThread(() -> this.circle = googleMap.addCircle(new CircleOptions()
                .center(center)
                .strokeColor(Color.parseColor(mOptions.getFromStrokeColor()))
                .strokeWidth(mOptions.getStrokeWidth())
                .visible(true)
                .radius(mOptions.getRadius())));
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
