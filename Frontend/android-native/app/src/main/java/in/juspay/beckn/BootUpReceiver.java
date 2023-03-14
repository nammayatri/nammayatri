package in.juspay.mobility;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.os.Build;
import android.widget.Toast;

import in.juspay.mobility.utils.LocationUpdateService;

public class BootUpReceiver extends BroadcastReceiver {

    @Override
    public void onReceive(Context context, Intent intent) {
        Intent locationUpdateService = new Intent(context, LocationUpdateService.class);
        locationUpdateService.putExtra("action", "start");
        locationUpdateService.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        if ((context.getResources().getString(R.string.service)).equals("nammayatripartner")) {
            try {
                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                    context.startForegroundService(locationUpdateService);
                } else {
                    context.startService(locationUpdateService);
                }
            }catch (Exception e){
                e.printStackTrace();
            }
        }
    }
}