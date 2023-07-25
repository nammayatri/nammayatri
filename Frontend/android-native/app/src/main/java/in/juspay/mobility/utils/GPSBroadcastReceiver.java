package in.juspay.mobility.utils;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.os.Build;
import android.util.Log;
import com.google.firebase.analytics.FirebaseAnalytics;

public class GPSBroadcastReceiver extends BroadcastReceiver {

    @Override
    public void onReceive(Context context, Intent intent) {
        try {
            Intent gpsListeningService = new Intent(context, GpsListeningService.class);
            gpsListeningService.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK | Intent.FLAG_ACTIVITY_NEW_TASK);
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                context.startForegroundService(gpsListeningService);
            }else {
                context.startService(gpsListeningService);
            }
        } catch (Exception e) {
            FirebaseAnalytics.getInstance(context).logEvent("Exception_in_GPSBroadcastReceiver",null);
            Log.e("GPSBroadcastReceiver", "Error in startGPSListeningService " + e);
        }
    }
}
