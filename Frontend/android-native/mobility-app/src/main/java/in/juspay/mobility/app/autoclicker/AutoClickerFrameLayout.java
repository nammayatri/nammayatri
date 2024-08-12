package in.juspay.mobility.app.autoclicker;

import android.content.Context;
import android.util.AttributeSet;
import android.view.MotionEvent;
import android.widget.FrameLayout;

public class AutoClickerFrameLayout extends FrameLayout {

    public AutoClickerFrameLayout(Context context) {
        super(context);
    }

    public AutoClickerFrameLayout(Context context, AttributeSet attrs) {
        super(context, attrs);
    }

    public AutoClickerFrameLayout(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    public boolean onInterceptTouchEvent(MotionEvent event) {
        AutoClickerDetector.recordEvent(event);
        // If return true in this method, touches will be blocked
        return false;
    }
}