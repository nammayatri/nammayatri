package in.juspay.hypersdk.mystique;

import android.animation.ValueAnimator;
import android.app.Activity;
import android.content.Context;
import android.os.Looper;
import android.util.AttributeSet;
import android.view.View;
import android.view.ViewParent;
import android.view.animation.AccelerateDecelerateInterpolator;
import android.widget.FrameLayout;
import android.widget.ScrollView;

import androidx.annotation.Keep;

@Keep
public class AccordionLayout extends FrameLayout {

    private final static int EXPAND = 1;
    private Context context;
    private final static int COLLAPSE = 0;

    private float delta = EXPAND;
    private float target = EXPAND;

    private float parallaxDelta = 0.6f;

    private float alphaDelta = 1.0f;

    private int animationDuration = 300;

    private ValueAnimator animator;

    private int parentScrollViewId = -1;

    private boolean postLayout = false;

    public AccordionLayout(Context context) {
        super(context);
        this.context = context;
    }

    public AccordionLayout(Context context, AttributeSet attrs) {
        super(context, attrs);
        this.context = context;
    }

    public AccordionLayout(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        this.context = context;
    }

    public AccordionLayout(Context context, AttributeSet attrs, int defStyleAttr, int defStyleRes) {
        super(context, attrs, defStyleAttr, defStyleRes);
        this.context = context;
    }

    @Override
    protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
        super.onMeasure(widthMeasureSpec, heightMeasureSpec);
        int height = this.getMeasuredHeight();
        int finalHeight = (int) (delta * height);
        if (finalHeight < 0) {
            finalHeight = 0;
        }
        setVisibility((finalHeight == 0) && (target == 0) ? GONE : VISIBLE);
        for (int i = 0; i < getChildCount(); i++) {
            View view = getChildAt(i);
            view.setTranslationY((int) ((finalHeight - height) * parallaxDelta));
            if (alphaDelta != 0) {
                view.setAlpha(delta * alphaDelta);
                if (target == 1 && delta == 1) {
                    view.setAlpha(1);
                }
            }
        }
        this.setMeasuredDimension(getMeasuredWidth(), finalHeight);
        postLayout = true;
    }

    public void setDefaultExpand(boolean shouldExpand) {
        float requiredTarget = shouldExpand ? EXPAND : COLLAPSE;
        target = requiredTarget;
        delta = requiredTarget;
        if (requiredTarget == COLLAPSE) {
            setVisibility(GONE);
        }
    }

    public void setExpandAlpha(float coef) {
        alphaDelta = coef;
    }

    public void setExpandParallax(float coef) {
        parallaxDelta = coef;
    }

    public void setExpandDuration(int duration) {
        animationDuration = duration;
    }

    public void setScrollParent(int id) {
        parentScrollViewId = id;
    }

    public void setExpand(boolean shouldExpand) {
        float requiredTarget = shouldExpand ? EXPAND : COLLAPSE;
        if (target == requiredTarget) {
            return;
        }
        target = requiredTarget;
        if (Looper.getMainLooper().getThread() == Thread.currentThread()) {
            /* On UI thread.
              When Accordion Layout is called normally on UI thread, run with animation
             */
            if (animator != null) {
                animator.cancel();
            }
            if (target == EXPAND) {
                setVisibility(VISIBLE);
            }
            int finalDuration = (int) (((target - delta) < 0 ? delta - target : target - delta) *
                    animationDuration);
            animator = ValueAnimator.ofFloat(delta, target);
            animator.setDuration(finalDuration);
            animator.setInterpolator(new AccelerateDecelerateInterpolator());
            postLayout = false;

            animator.addUpdateListener(new ValueAnimator.AnimatorUpdateListener() {
                @Override
                public void onAnimationUpdate(ValueAnimator animation) {
                    delta = (float) animation.getAnimatedValue();
                    if (postLayout)
                        scrollParent();
                    requestLayout();
                }
            });
            animator.start();
        } else {
            /** Not on UI thread.
             * When Accordion Layout is called on background thread , run without animation.
             * Modification is done to support background rendering.
             */
            if(shouldExpand){
                setVisibility(VISIBLE);
                delta = requiredTarget;
                scrollParent();
            } else {
                setVisibility(GONE);
            }
        }

    }

    private int getRelativeTop(View myView, ScrollView rootV) {
        ViewParent vp = myView.getParent();
        if (vp == rootV || !(vp instanceof View))
            return myView.getTop();
        else {
            return myView.getTop() + getRelativeTop((View) vp, rootV);
        }
    }

    private void scrollParent() {
        if (target == EXPAND && parentScrollViewId != -1) {
            if (!(getContext() instanceof Activity)) {
                return;
            }
            ScrollView parent = ((Activity) getContext()).findViewById(parentScrollViewId);
            if (parent == null) {
                return;
            }
            int scrollTop = parent.getScrollY();
            int scrollBottom = scrollTop + parent.getHeight();
            int top = getRelativeTop(this, parent);
            int bottom = top + this.getHeight();

            if (top < scrollTop || bottom > scrollBottom) {
                int diff = (top < scrollTop) ? top-scrollTop : bottom - scrollBottom;
                parent.scrollTo(0, parent.getScrollY() + diff);
            }
        }
    }
}
