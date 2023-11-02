package in.juspay.mobility.app;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;

public class InstallReferrerReceiver extends BroadcastReceiver {
    @Override
    public void onReceive(Context context, Intent intent) {
        Bundle extras = intent.getExtras();
        String referrer = extras != null ? extras.getString("referrer") : "";
        System.out.println("debug refer InstallReferrerReceiver extras : " + extras);
        System.out.println("debug refer InstallReferrerReceiver referrer : " + referrer);
    }
}
