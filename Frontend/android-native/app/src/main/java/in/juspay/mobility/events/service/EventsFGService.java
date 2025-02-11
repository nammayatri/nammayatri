package in.juspay.mobility.events.service;

import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.Service;
import android.content.Intent;
import android.content.pm.ServiceInfo;
import android.os.Build;
import android.os.IBinder;
import android.util.Log;

import androidx.core.app.NotificationCompat;
import androidx.work.OneTimeWorkRequest;
import androidx.work.OutOfQuotaPolicy;
import androidx.work.WorkManager;

import in.juspay.mobility.R;
import in.juspay.mobility.events.worker.EventsWorker;


// This is just the foreground service running constantly to check for app closure
public class EventsFGService extends Service {
    private final String LOG_TAG = this.getClass().getSimpleName();
    private static final String CHANNEL_ID = "EventsServiceChannel";

    @Override
    public void onCreate() {
        super.onCreate();
        createNotificationChannel();
        Notification notification = null;
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            notification = new NotificationCompat.Builder(this, CHANNEL_ID).setContentTitle("")
                    .setContentText("")
                    .setSmallIcon(R.drawable.ny_ic_permission_notification).setPriority(NotificationCompat.PRIORITY_LOW).setAutoCancel(true).build();
        }
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
            startForeground(1, notification, ServiceInfo.FOREGROUND_SERVICE_TYPE_DATA_SYNC);
        } else {
            startForeground(1, notification);
        }
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        return START_STICKY;
    }

    @Override
    public void onTaskRemoved(Intent rootIntent) {
        Log.i(LOG_TAG, "App removed from recents");
        OneTimeWorkRequest workRequest = new OneTimeWorkRequest.Builder(EventsWorker.class).setExpedited(OutOfQuotaPolicy.RUN_AS_NON_EXPEDITED_WORK_REQUEST).build();
        WorkManager.getInstance(getApplicationContext()).enqueue(workRequest);
        stopSelf();
    }

    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    private void createNotificationChannel() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            int importance = NotificationManager.IMPORTANCE_LOW;
            String name = "Events Foreground Service";
            NotificationChannel channel = new NotificationChannel(CHANNEL_ID, name, importance);
            NotificationManager notificationManager = getSystemService(NotificationManager.class);
            notificationManager.createNotificationChannel(channel);
        }
    }
}