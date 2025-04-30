package in.juspay.mobility.app.overlayMessage;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.graphics.PixelFormat;
import android.os.IBinder;
import android.provider.Settings;
import android.util.Log;
import android.view.Gravity;
import android.view.View;
import android.view.WindowManager;

import in.juspay.mobility.app.MyFirebaseMessagingService.NotificationTypes;

import androidx.annotation.Nullable;

public class Service extends android.app.Service {
    private WindowManager windowManager;
    private ViewInterface overlayView;
    private static final String TAG = "OverlayMessage_Service";

    @Override
    public void onCreate() {
        if (!Settings.canDrawOverlays(this)) return;
        super.onCreate();
    }

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    @SuppressLint("InflateParams")
    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        Log.i(TAG, "Service started");
        String intentMessage = intent != null && intent.hasExtra("payload") ? intent.getStringExtra("payload") : null;
        if (!Settings.canDrawOverlays(this) || intentMessage == null) return START_STICKY;

        String notificationType = intent.hasExtra("notificationType") ? intent.getStringExtra("notificationType") : "";

        if (windowManager != null) {
            removeOverlayView();
            setViewByNotificationType(notificationType, intentMessage);

            return START_STICKY;
        }

        windowManager = (WindowManager) getSystemService(WINDOW_SERVICE);
        removeOverlayView();
        setViewByNotificationType(notificationType, intentMessage);

        return START_STICKY;
    }

    private void removeOverlayView() {
        if(overlayView != null){
            Log.i(TAG, "Removing older overlay view");
            windowManager.removeView(overlayView.getView());
            overlayView.destroyView();
        }
    }

    private void setViewByNotificationType(String notificationType, String payload){
        switch (notificationType){
            case NotificationTypes.EKD_LIVE_CALL_FEEDBACK:
                overlayView = new CallFeedbackView();
                break;
            default:
                overlayView = new MessagingView();
        }

        View view = overlayView.createView(payload, this, new ServiceInterface() {
            @Override
            public void killService() {
                Log.i(TAG, "Killing the service");
                stopSelf();
            }
        });

        windowManager.addView(view, getWindowLayoutParams());
    }

    private WindowManager.LayoutParams getWindowLayoutParams(){
        int layoutParamsType = android.os.Build.VERSION.SDK_INT >= android.os.Build.VERSION_CODES.O ? WindowManager.LayoutParams.TYPE_APPLICATION_OVERLAY : WindowManager.LayoutParams.TYPE_PHONE;
        WindowManager.LayoutParams params = new WindowManager.LayoutParams(
                WindowManager.LayoutParams.MATCH_PARENT,
                WindowManager.LayoutParams.MATCH_PARENT,
                layoutParamsType,
                WindowManager.LayoutParams.FLAG_TURN_SCREEN_ON | WindowManager.LayoutParams.FLAG_DIM_BEHIND,
                PixelFormat.TRANSPARENT);
        params.dimAmount = 0.6f;
        params.gravity = Gravity.CENTER;
        params.screenOrientation = ActivityInfo.SCREEN_ORIENTATION_PORTRAIT;
        return params;
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        Log.i(TAG, "Service Destroyed");
        if (windowManager != null && overlayView != null) {
            if(overlayView.getView() != null) {
                windowManager.removeView(overlayView.getView());
            }
            overlayView = null;
        }
    }
}
