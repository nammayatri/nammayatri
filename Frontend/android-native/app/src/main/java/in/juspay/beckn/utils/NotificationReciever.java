package in.juspay.mobility.utils;

import android.app.Notification;
import android.app.NotificationManager;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.util.Log;

public class NotificationReciever extends BroadcastReceiver {
    public static String NOTIFICATION_ID = "notification-id" ;
    public static String NOTIFICATION = "notification" ;

    public void onReceive (Context context , Intent intent) {
        Log.i("Beckn_Notification", "Local Notification Reciever - onReceive called");
        NotificationManager notificationManager = (NotificationManager)context.getSystemService(Context.NOTIFICATION_SERVICE ) ;
        Notification notification = intent.getParcelableExtra( NOTIFICATION ) ;
        // TODO :: Ask Daya to check
        int id = intent.getIntExtra( NOTIFICATION_ID , NotificationUtils.notificationId);
        NotificationUtils.notificationId++;
        assert notificationManager != null;
        notificationManager.notify(id , notification) ;
    }
}
