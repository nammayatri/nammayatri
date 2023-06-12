package in.juspay.mobility.app.callbacks;

import android.content.Context;

public interface ShowNotificationCallBack {
    void showInAppNotification(String title, String message, String onTapAction, String action1Text, String action2Text, String action1Image, String action2Image, String channelId, int durationInMilliSeconds, Context context);
}
