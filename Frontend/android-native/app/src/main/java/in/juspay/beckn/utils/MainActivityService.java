package in.juspay.mobility.utils;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.os.Build;
import android.widget.Toast;

import in.juspay.mobility.MainActivity;

public class MainActivityService extends BroadcastReceiver {
    @Override
    public void onReceive(Context context, Intent intent) {
        Intent notificatinStart = new Intent(context, MainActivity.class);
        notificatinStart.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        System.out.println("It is in MainActivityService");
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            context.startActivity(notificatinStart);
        }else
        {
            Toast.makeText(context, "Service restarted else", Toast.LENGTH_SHORT).show();
            context.startActivity(notificatinStart);
        }
    }
}
