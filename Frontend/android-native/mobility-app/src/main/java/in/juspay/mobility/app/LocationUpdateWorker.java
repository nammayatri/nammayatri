/*
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.app;

import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Build;
import android.os.Handler;
import android.os.Looper;
import android.provider.Settings;
import android.util.Log;
import androidx.annotation.NonNull;
import androidx.work.Worker;
import androidx.work.WorkerParameters;
import com.google.firebase.crashlytics.FirebaseCrashlytics;

public class LocationUpdateWorker extends Worker {
    public LocationUpdateWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);
        this.sharedPrefs = context.getSharedPreferences(context.getString(R.string.preference_file_key),Context.MODE_PRIVATE);
    }

    private final SharedPreferences sharedPrefs;
    private final String TAG = "LocationUpdateWorker";
    private String driverId = "empty";
    @NonNull
    @Override
    public Result doWork() {
        try {
            String driverStatus = sharedPrefs != null ? sharedPrefs.getString("DRIVER_STATUS_N", "__failed") : "__failed";
            if (!driverStatus.isEmpty() && !driverStatus.equals("null") && !driverStatus.equals("__failed") && !driverStatus.equals("Offline")) {
                Context context = getApplicationContext();
                Intent locationService = new Intent(context, LocationUpdateService.class);
                locationService.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK | Intent.FLAG_ACTIVITY_NEW_TASK);
                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                    context.startForegroundService(locationService);
                } else {
                    context.startService(locationService);
                }
                Intent restartIntent = context.getPackageManager().getLaunchIntentForPackage(context.getPackageName());
                restartIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK | Intent.FLAG_ACTIVITY_NEW_TASK);
                String activityStatus = sharedPrefs.getString("ACTIVITY_STATUS", "null");
                if(Settings.canDrawOverlays(context) && activityStatus.equals("onDestroy")){
                    try{
                        new Handler(Looper.getMainLooper()).postDelayed(() -> {
                            context.startActivity(restartIntent);
                            Utils.minimizeApp(context);
                        }, 5000);
                    } catch (Exception e) {
                        Log.e(TAG, "Unable to Start Widget Service");
                        Exception exception = new Exception("Exception in LocationUpdateWorker$minimizeApp for ID : " + driverId + " $ Error : " + e);
                        FirebaseCrashlytics.getInstance().recordException(exception);
                    }
                }
            }
        } catch (Exception e) {
            Log.e(TAG, "Error in LocationUpdateWorker " + e);
            if(sharedPrefs != null) driverId = sharedPrefs.getString("DRIVER_ID", "null");
            Exception exception = new Exception("Exception in LocationUpdateWorker for ID : " + driverId + " $ Error : " + e);
            FirebaseCrashlytics.getInstance().recordException(exception);
        }
        return Result.success();
    }
}
