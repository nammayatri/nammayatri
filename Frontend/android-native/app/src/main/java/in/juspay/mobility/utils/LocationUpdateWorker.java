/* 
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.utils;

import android.app.KeyguardManager;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.os.Build;
import android.os.Bundle;
import android.os.PowerManager;

import androidx.annotation.NonNull;
import androidx.work.Worker;
import androidx.work.WorkerParameters;

import in.juspay.mobility.BootUpReceiver;

public class LocationUpdateWorker extends Worker {
    public LocationUpdateWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);
    }
    private SharedPreferences sharedPrefs;

    @NonNull
    @Override
    public Result doWork() 
    {
        String driverStatus = sharedPrefs!=null ? sharedPrefs.getString("DRIVER_STATUS", "__failed"): "";
        if (driverStatus.equals("true")) {
            Context context = getApplicationContext();
            // creates intent for main activity
            final PackageManager pm = context.getPackageManager();
            if(isScreenLocked()) {
                final Intent intent = pm.getLaunchIntentForPackage(context.getPackageName());
                intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                context.startActivity(intent);
            }
            Intent locationService = new Intent(context,LocationUpdateService.class);
            locationService.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK | Intent.FLAG_ACTIVITY_NEW_TASK);
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                context.startForegroundService(locationService);
            } else {
                context.startService(locationService);
            }
        }
        return Result.success();
    }

    private boolean isScreenLocked()
    {
        Context context = getApplicationContext();
        PowerManager powerManager = (PowerManager)context.getSystemService(Context.POWER_SERVICE);
        KeyguardManager myKM = (KeyguardManager) context.getSystemService(Context.KEYGUARD_SERVICE);
        boolean isPhoneLocked = myKM.inKeyguardRestrictedInputMode();
        return  isPhoneLocked || !(Build.VERSION.SDK_INT < 20? powerManager.isScreenOn():powerManager.isInteractive());
    }
}
