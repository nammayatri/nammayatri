package in.juspay.hypersdk.mystique;

import android.content.Context;
import android.view.MotionEvent;
import android.widget.ScrollView;

import androidx.annotation.Keep;

@Keep
public class SwypeScroll extends ScrollView {
    private float xDistance, yDistance, lastX, lastY;

    public SwypeScroll(Context context) {
        super(context);
    }

    @Override
    public boolean onInterceptTouchEvent(MotionEvent ev) {
        switch (ev.getAction()) {
            case MotionEvent.ACTION_DOWN:
                xDistance = yDistance = 0f;
                lastX = ev.getX();
                lastY = ev.getY();
                break;
            case MotionEvent.ACTION_MOVE:
                final float curX = ev.getX();
                final float curY = ev.getY();
                xDistance += Math.abs(curX - lastX);
                yDistance += Math.abs(curY - lastY);
                lastX = curX;
                lastY = curY;
                if (xDistance > yDistance)
                    return false;
                else {
                    SwypeLayout partialSwype = SwypeLayout.partialSwypeWeakReference.get();
                    SwypeLayout activeLayout = SwypeLayout.activeLayoutWeakReference.get();
                    if (partialSwype != null && partialSwype != activeLayout) {
                        partialSwype.reset();
                    }
                }
        }

        return super.onInterceptTouchEvent(ev);
    }
}
