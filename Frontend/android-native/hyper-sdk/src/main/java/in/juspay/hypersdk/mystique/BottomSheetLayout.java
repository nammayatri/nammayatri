package in.juspay.hypersdk.mystique;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;

import androidx.annotation.Keep;
import androidx.annotation.NonNull;
import androidx.coordinatorlayout.widget.CoordinatorLayout;

import com.google.android.material.bottomsheet.BottomSheetBehavior;

import java.util.Locale;

import in.juspay.hypersdk.core.DuiCallback;

@Keep
public class BottomSheetLayout extends FrameLayout {
    private final BottomSheetCallback bottomSheetCallback;
    private final BottomSheetBehavior bottomSheetBehavior;
    private boolean enableShift = true;
    private boolean overridePeakHeight = true;

    public BottomSheetLayout(@NonNull Context context) {
        super(context);
        bottomSheetBehavior = new BottomSheetBehavior();
        bottomSheetCallback = new BottomSheetCallback();
        bottomSheetBehavior.addBottomSheetCallback(bottomSheetCallback);
    }

    public void setStateChangeCallback(DuiCallback dui, String callback) {
        bottomSheetCallback.setDuiCallback(dui);
        bottomSheetCallback.setStateChangeCallback(callback);
    }

    public void setSlideCallback(DuiCallback dui, String callback) {
        bottomSheetCallback.setDuiCallback(dui);
        bottomSheetCallback.setSlideCallback(callback);
    }

    @Keep
    public void setEnableShift(boolean enableShift) {
        this.enableShift = enableShift;
    }

    @Keep
    public void setHideable(boolean hideable) {
        bottomSheetBehavior.setHideable(hideable);
    }

    @Override
    public void setLayoutParams(ViewGroup.LayoutParams params) {
        if (params instanceof CoordinatorLayout.LayoutParams) {
            ((CoordinatorLayout.LayoutParams) params).setBehavior(bottomSheetBehavior);
        }
        super.setLayoutParams(params);
    }

    @Override
    protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
        super.onMeasure(widthMeasureSpec, heightMeasureSpec);
        if (overridePeakHeight) {
            int height = getMeasuredHeight();
            bottomSheetBehavior.setPeekHeight(height);
        }
    }

    @Keep
    public void setState(int state) {
        bottomSheetBehavior.setState(state);
    }

    @Keep
    public void setPeakHeight(int state) {
        bottomSheetBehavior.setPeekHeight(state);
        overridePeakHeight = false;
    }

    @Keep
    public void setHalfExpandedRatio(float ratio) {
        bottomSheetBehavior.setHalfExpandedRatio(ratio);
    }

    @Keep
    public void setTopShift(float ratio) {
        bottomSheetCallback.setTopShift(ratio);
    }

    @Keep
    public void setBottomShift(float ratio) {
        bottomSheetCallback.setBottomShift(ratio);
    }

    class BottomSheetCallback extends BottomSheetBehavior.BottomSheetCallback {

        private DuiCallback duiCallback;
        private String stateChangeCallback;
        private String stateSlideCallback;
        float topShift;
        float bottomShift;
        boolean topShiftOverridden = false, bottomShiftOverridden = false;
        private float lastReceivedScroll;

        @Override
        public void onStateChanged(@NonNull View bottomSheet, int newState) {
            if (newState == BottomSheetBehavior.STATE_SETTLING && enableShift) {
                if (!topShiftOverridden || !bottomShiftOverridden) {
                    float half, peak;
                    half = BottomSheetLayout.this.bottomSheetBehavior.getHalfExpandedRatio();
                    peak = (float) BottomSheetLayout.this.bottomSheetBehavior.getPeekHeight() / (float) bottomSheet.getHeight();
                    if (!topShiftOverridden)
                        topShift = half / 2.0f + 0.5f;
                    if (!bottomShiftOverridden)
                        bottomShift = half / 2.0f - peak / 2.0f;
                }
                if (bottomShift > lastReceivedScroll) {
                    BottomSheetLayout.this.bottomSheetBehavior.setState(BottomSheetBehavior.STATE_COLLAPSED);
                } else if (lastReceivedScroll > bottomShift && lastReceivedScroll < topShift) {
                    BottomSheetLayout.this.bottomSheetBehavior.setState(BottomSheetBehavior.STATE_HALF_EXPANDED);
                } else {
                    BottomSheetLayout.this.bottomSheetBehavior.setState(BottomSheetBehavior.STATE_EXPANDED);
                }
            }
            if (duiCallback != null && stateChangeCallback != null) {
                String callback = String.format(Locale.ENGLISH,"window.callUICallback('%s','%d');", stateChangeCallback , newState);
                duiCallback.addJsToWebView(callback);
            }
        }

        @Override
        public void onSlide(@NonNull View bottomSheet, float slideOffset) {
            lastReceivedScroll = slideOffset;
            if (duiCallback != null && stateSlideCallback != null) {
                String callback = String.format(Locale.ENGLISH,"window.callUICallback('%s','%f');", stateSlideCallback , slideOffset);
                duiCallback.addJsToWebView(callback);
            }
        }

        public void setDuiCallback(DuiCallback duiCallback) {
            this.duiCallback = duiCallback;
        }

        public void setStateChangeCallback(String callback) {
            this.stateChangeCallback = callback;
        }

        public void setSlideCallback(String callback) {
            this.stateSlideCallback = callback;
        }

        public void setTopShift(float topShift) {
            this.topShiftOverridden = true;
            this.topShift = topShift;
        }

        public void setBottomShift(float bottomShift) {
            this.bottomShiftOverridden = true;
            this.bottomShift = bottomShift;
        }
    }
}
