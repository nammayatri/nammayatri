package in.juspay.mobility.app.callbacks;

public interface CallBack {
    void customerCallBack(String notificationType);

    void driverCallBack(String notificationType);

    void imageUploadCallBack(String encImage, String filename, String filePath);

    void chatCallBack(String message, String sentBy, String time, String len);

    void inAppCallBack(String onTapAction);
}