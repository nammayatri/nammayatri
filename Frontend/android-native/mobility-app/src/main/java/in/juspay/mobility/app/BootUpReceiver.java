/*
 * Copyright 2022-23, Juspay
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 * as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 * is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 * the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */
package in.juspay.mobility.app;

import static android.content.Intent.ACTION_BOOT_COMPLETED;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Build;
import android.os.Handler;
import android.provider.Settings;
import android.util.Log;

public class BootUpReceiver extends BroadcastReceiver {

    @Override
    public void onReceive(Context context, Intent intent) {
        if (intent.getAction() != null && intent.getAction().equals(ACTION_BOOT_COMPLETED)) {
            Intent locationUpdateService = new Intent(context, LocationUpdateService.class);
            locationUpdateService.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK | Intent.FLAG_ACTIVITY_NEW_TASK);
            Intent widgetReloadService = new Intent(context, WidgetService.class);
            widgetReloadService.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK | Intent.FLAG_ACTIVITY_NEW_TASK);
            Intent restartIntent = context.getPackageManager().getLaunchIntentForPackage(context.getPackageName());
            restartIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK | Intent.FLAG_ACTIVITY_NEW_TASK);
            SharedPreferences sharedPrefs = context.getApplicationContext().getSharedPreferences(context.getApplicationContext().getString(R.string.preference_file_key), Context.MODE_PRIVATE);
            String driverStatus = sharedPrefs.getString("DRIVER_STATUS", "__failed");
            String key = context.getString(R.string.service);
            String merchant = key.contains("partner") || key.contains("driver") || key.contains("provider") ? "DRIVER" : "USER";
            if (merchant.equals("DRIVER")) {
                if (driverStatus.equals("true")) {
                    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                        context.startForegroundService(locationUpdateService);
                    } else {
                        context.startService(locationUpdateService);
                    }
                }
                if (Settings.canDrawOverlays(context)) {
                    try {
                        context.startService(widgetReloadService);
                        Handler handler = new Handler();
                        handler.postDelayed(() -> {
                            context.startActivity(restartIntent);
                            Utils.minimizeApp(context);
                        }, 5000);
                    } catch (Exception e) {
                        Log.e("BootUpReceiver", "Unable to Start Widget Service");
                    }
                }
            }
        }
    }
}
