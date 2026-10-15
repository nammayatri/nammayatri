package in.juspay.mobility.app.callbacks;

import android.content.Context;

import org.json.JSONObject;

public interface ShowNotificationCallBack {
    void showInAppNotification(JSONObject jsonObject, Context context);
    void hideInAppNotification(String channelId);
}
