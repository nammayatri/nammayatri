package in.juspay.hypersdk.mystique;


import android.annotation.SuppressLint;
import android.content.Context;
import android.view.MotionEvent;
import android.view.View;
import android.widget.FrameLayout;

import androidx.annotation.Keep;
import androidx.interpolator.view.animation.FastOutLinearInInterpolator;

import java.lang.ref.WeakReference;

@Keep
public class SwypeLayout extends FrameLayout {

    public static WeakReference<SwypeLayout> partialSwypeWeakReference, activeLayoutWeakReference;

    private static final String TAG = "SwypeLayout";

    private float mX = 0;
    private int leftEdge = 0;
    private int rightEdge = 250;
    private float mDisplaceX = 0;
    private boolean didDisplace = false;

    private View mContent = null;
    private View mLeftOption = null;
    private View mRightOption = null;

    private boolean mEnabled = false;


    public SwypeLayout(Context context) {
        super(context);
    }

    private void handleSwype(float x, boolean commit) {
        if (leftEdge == 0 && rightEdge == 0) {
            return;
        }
        float maxDisplacement = leftEdge;
        boolean negate = false;
        float deltaX = x - mX;
        float displaceX = mDisplaceX + deltaX;
        if (displaceX < 0) {
            negate = true;
            maxDisplacement = rightEdge;
            displaceX *= -1;
        }

        if (displaceX > maxDisplacement) {
            displaceX = maxDisplacement;
        }

        if (commit) {
            displaceX = ((displaceX / maxDisplacement) > 0.4) ? maxDisplacement : 0;
        }

        SwypeLayout partialSwype = partialSwypeWeakReference.get();
        if (partialSwype != null && partialSwype != this) {
            partialSwype.reset();
        }

        partialSwypeWeakReference = new WeakReference<>(this);

        if (negate) {
            displaceX *= -1;
        }

        if ((mDisplaceX - displaceX) > 20 || (mDisplaceX - displaceX) < -20) {
            didDisplace = true;
            SwypeLayout activeLayout = activeLayoutWeakReference.get();
            if (activeLayout != null && activeLayout != this) {
                activeLayout.reset();
                activeLayoutWeakReference = new WeakReference<>(null);
            }
        }

        if (commit) {
            mDisplaceX = displaceX;
            mContent.animate().setDuration(150).setInterpolator(new FastOutLinearInInterpolator()).translationX(displaceX);
            if (displaceX != 0) {
                activeLayoutWeakReference = new WeakReference<>(this);
            }
            partialSwypeWeakReference = new WeakReference<>(null);
        } else {
            mContent.setTranslationX(displaceX);
        }
    }

    @Override
    public boolean onInterceptTouchEvent(MotionEvent event) {
        return mEnabled;
    }

    public void setSwypeEnabled(boolean enabled) {
        if (mEnabled != enabled && mEnabled) {
            reset();
        }
        mEnabled = enabled;
    }

    @SuppressLint("ClickableViewAccessibility")
    @Override
    public boolean onTouchEvent(MotionEvent event) {
        switch (event.getActionMasked()) {
            case MotionEvent.ACTION_DOWN:
                mX = event.getX();
                break;

            case MotionEvent.ACTION_MOVE:
                handleSwype(event.getX(), false);
                break;

            case MotionEvent.ACTION_UP:
                handleSwype(event.getX(), true);
                mX = 0;
                if (didDisplace) {
                    didDisplace = false;
                    return false;
                } else {
                    processClick(event);
                }
                break;
        }
        return true;
    }

    private void processClick(MotionEvent event) {
        float height = this.getMeasuredHeight();
        float width = this.getMeasuredWidth();
        float y = event.getY();
        float x = event.getX();
        if (height < y || y < 0 || x < 0 || x > width) {
            return;
        }

        if (mDisplaceX == 0) {
            mContent.callOnClick();
        } else if (mDisplaceX < 0 && x >= (width - rightEdge)) {
            mRightOption.callOnClick();
        } else if (mDisplaceX > 0 && x <= (leftEdge)) {
            mLeftOption.callOnClick();
        } else {
            reset();
        }
    }

    private boolean tagChildren() {
        if (getChildCount() != 3) {
            //JuspayLogger.e(TAG, "Layout requires three child views");
            return false;
        }
        View content = getChildAt(0);
        View leftOption = getChildAt(1);
        View rightOption = getChildAt(2);
        if (content != mContent || leftOption != mLeftOption || rightOption != mRightOption) {
            mContent = content;
            mLeftOption = leftOption;
            mRightOption = rightOption;
            reset();
        }
        return true;
    }

    public void reset() {
        mDisplaceX = 0;
        leftEdge = mLeftOption.getMeasuredWidth();
        rightEdge = mRightOption.getMeasuredWidth();
        mContent.setTranslationZ(2);
        mContent.animate().setDuration(150).translationX(0);
    }

    @Override
    protected void onLayout(boolean changed, int left, int top, int right, int bottom) {
        if (mContent == null && !tagChildren()) {
            return;
        }
        int containerRight = right - left;
        mContent.layout(0, 0, mContent.getMeasuredWidth(), mContent.getMeasuredHeight());
        mLeftOption.layout(0, 0, mLeftOption.getMeasuredWidth(), mContent.getMeasuredHeight());
        mRightOption.layout(containerRight - mRightOption.getMeasuredWidth(), 0, containerRight, mRightOption.getMeasuredWidth());
        mContent.bringToFront();
    }

    @Keep
    public static void clear() {
        SwypeLayout activeLayout = activeLayoutWeakReference.get();
        if (activeLayout != null) {
            activeLayout.reset();
            activeLayoutWeakReference = new WeakReference<>(null);
        }
        SwypeLayout partialSwype = partialSwypeWeakReference.get();
        if (partialSwype != null) {
            partialSwype.reset();
            partialSwypeWeakReference = new WeakReference<>(null);
        }
    }
}
