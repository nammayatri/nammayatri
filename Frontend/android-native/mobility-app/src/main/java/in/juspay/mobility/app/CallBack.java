package in.juspay.mobility.app;

public interface CallBack {
    void customerCallBack(String notificationType);
    void driverCallBack(String notificationType);
    void imageUploadCallBack(String encImage, String filename);
    void internetCallBack(String isPermission);
}