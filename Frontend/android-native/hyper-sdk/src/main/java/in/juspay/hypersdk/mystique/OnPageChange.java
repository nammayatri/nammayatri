package in.juspay.hypersdk.mystique;

import androidx.viewpager2.widget.ViewPager2;

import java.util.Locale;

import in.juspay.hypersdk.core.DuiCallback;


public class OnPageChange extends ViewPager2.OnPageChangeCallback {

    private String onPageScrolledCallBack;
    private String onPageSelectedCallBack;
    private String onPageScrollStateCallBack;
    private final DuiCallback duiCallback;

    public OnPageChange(DuiCallback duiCallback) {
        this.duiCallback = duiCallback;
    }

    public void setOnPageScrolledCallBack (String pageScrolledCallBack) {
        this.onPageScrolledCallBack = pageScrolledCallBack;
    }
    public void setOnPageSelectedCallBack (String pageScrolledCallBack) {
        this.onPageSelectedCallBack = pageScrolledCallBack;
    }
    public void setOnPageScrollStateChangedCallBack (String pageScrolledCallBack) {
        this.onPageScrollStateCallBack = pageScrolledCallBack;
    }

    @Override
    public void onPageScrolled(int position, float positionOffset, int positionOffsetPixels) {
        if (onPageScrolledCallBack != null) {
            String callback = String.format(Locale.ENGLISH,"window.callUICallback('%s','%s');", onPageScrolledCallBack ,(position + "," + positionOffset + "," + positionOffsetPixels));
            duiCallback.addJsToWebView(callback);
            super.onPageScrolled(position, positionOffset, positionOffsetPixels);
        }
    }

    @Override
    public void onPageSelected(int position) {
        if (onPageSelectedCallBack != null) {
            String callback = String.format(Locale.ENGLISH, "window.callUICallback('%s','%d');", onPageSelectedCallBack, position);
            duiCallback.addJsToWebView(callback);
            super.onPageSelected(position);
        }
    }

    @Override
    public void onPageScrollStateChanged(int state) {
        if (onPageScrollStateCallBack != null) {
            String callback = String.format(Locale.ENGLISH,"window.callUICallback('%s','%d');", onPageScrollStateCallBack ,state);
            duiCallback.addJsToWebView(callback);
            super.onPageScrollStateChanged(state);
        }
    }
}
