package in.juspay.mobility.app.callbacks;

import org.json.JSONObject;

public interface CallBack {
    void customerCallBack(String notificationType, String notificationData);

    void driverCallBack(String notificationType, String notificationData);

    void addStopCallBack(String newStopLocation);

    void chatCallBack(String message, String sentBy, String time, String len);

    void inAppCallBack(String onTapAction);

    void bundleUpdatedCallBack(String event, JSONObject description);
}