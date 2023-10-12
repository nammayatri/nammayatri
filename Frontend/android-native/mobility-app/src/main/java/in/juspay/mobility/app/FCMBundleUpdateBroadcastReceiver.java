package in.juspay.mobility.app;


import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

public class FCMBundleUpdateBroadcastReceiver extends BroadcastReceiver {
    @Override
    public void onReceive(Context context, Intent intent) {
        String merchantType = intent.getStringExtra("merchantType");
        String payload = intent.getStringExtra("payload");

        Intent fcmBundle = new Intent(context,RemoteAssetsDownloader.class);
        fcmBundle.putExtra("merchantType",merchantType);
        fcmBundle.putExtra("bundleType","FCM");
        fcmBundle.putExtra("payload",payload);
        context.startService(fcmBundle);
    }
}
