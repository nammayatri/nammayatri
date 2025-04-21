package in.juspay.hypersdk.mystique;

import android.widget.AbsListView;

import in.juspay.hypersdk.core.DuiCallback;


public class OnScroll implements AbsListView.OnScrollListener {

    private String scrollChangeCallback;
    private String scrollCallback;
    private final DuiCallback duiCallback;

    public OnScroll(DuiCallback duiCallback) {
        this.duiCallback = duiCallback;
    }

    public void setScrollChangeCallback(String scrollChangeCallback) {
        this.scrollChangeCallback = scrollChangeCallback;
    }



    public void setScrollCallback(String scrollCallback) {
        this.scrollCallback = scrollCallback;
    }




    @Override
    public void onScrollStateChanged(AbsListView absListView, int i) {
        duiCallback.addJsToWebView("window.callUICallback('" + scrollChangeCallback + "'," + i + ");");
    }

    @Override
    public void onScroll(AbsListView absListView, int firstVisibleItem, int visibleItemCount, int totalItemCount) {
        duiCallback.addJsToWebView("window.callUICallback('" + scrollCallback + "','" + firstVisibleItem + "," + visibleItemCount + "," + totalItemCount + "');");
    }
}
