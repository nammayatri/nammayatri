package in.juspay.hypersdk.ui;

public interface RequestPermissionDelegate {
    void requestPermission(String[] permissions, int requestCode);
}
