/*
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.app;

import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.content.pm.ServiceInfo;
import android.net.Uri;
import android.os.Build;
import android.os.Handler;
import android.os.IBinder;
import android.os.Looper;
import android.provider.Settings;
import android.util.Log;
import androidx.annotation.NonNull;
import androidx.core.app.NotificationCompat;
import androidx.core.content.ContextCompat;
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
    private final String LOCATION_UPDATES = "LOCATION_UPDATES";

    private void sendMissingPermissionNotification() {
        Context context = getApplicationContext();

        String channelId = "location_permission_channel";
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            NotificationChannel channel = new NotificationChannel(
                    channelId,
                    "Permissions",
                    NotificationManager.IMPORTANCE_LOW
            );
            channel.setDescription("Notifies user that location permission is missing.");
            NotificationManager nm = context.getSystemService(NotificationManager.class);
            if (nm != null) {
                nm.createNotificationChannel(channel);
            }
        }

        // Intent to open App Settings
        Intent settingsIntent = new Intent(Settings.ACTION_APPLICATION_DETAILS_SETTINGS);
        settingsIntent.setData(Uri.parse("package:" + context.getPackageName()));

        PendingIntent pendingIntent = PendingIntent.getActivity(
                context,
                0,
                settingsIntent,
                PendingIntent.FLAG_IMMUTABLE | PendingIntent.FLAG_UPDATE_CURRENT
        );

        NotificationCompat.Builder builder = new NotificationCompat.Builder(context, channelId)
                .setContentTitle("Location Permission Needed")
                .setSmallIcon(Utils.getResIdentifier(this.getApplicationContext(), (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) ? "ic_launcher_small_icon" : "ny_ic_launcher", "drawable"))
                .setContentText("Tap here to enable location access in settings.")
                .setPriority(NotificationCompat.PRIORITY_DEFAULT)
                .setAutoCancel(true)
                .setContentIntent(pendingIntent);

        NotificationManager notificationManager = (NotificationManager) context.getSystemService(Context.NOTIFICATION_SERVICE);
        if (notificationManager != null) {
            notificationManager.notify(1001, builder.build());
        }
    }


    private boolean hasLocationPermission(Context context) {
        int fineLocPermission = ContextCompat.checkSelfPermission(context, android.Manifest.permission.ACCESS_FINE_LOCATION);
        int coarseLocPermission = ContextCompat.checkSelfPermission(context, android.Manifest.permission.ACCESS_COARSE_LOCATION);
        boolean backGroundPermission = true;

        if(Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
            String appState = sharedPrefs.getString(this.getApplicationContext().getString(in.juspay.mobility.app.R.string.ACTIVITY_STATUS), "onCreate");
            if (!appState.equals("onCreate") && !appState.equals("onResume") && !appState.equals("onPause")) {
                backGroundPermission = ContextCompat.checkSelfPermission(context, android.Manifest.permission.ACCESS_BACKGROUND_LOCATION) == PackageManager.PERMISSION_GRANTED;
            }
        }
        int foregroundPermission = 0;
        if(Build.VERSION.SDK_INT >= Build.VERSION_CODES.UPSIDE_DOWN_CAKE) foregroundPermission = ContextCompat.checkSelfPermission(context, android.Manifest.permission.FOREGROUND_SERVICE_LOCATION);
        return ((fineLocPermission == PackageManager.PERMISSION_GRANTED
                || coarseLocPermission == PackageManager.PERMISSION_GRANTED) && foregroundPermission == PackageManager.PERMISSION_GRANTED && backGroundPermission);
    }

    private String driverId = "empty";
    @NonNull
    @Override
    public Result doWork() {
        if (hasLocationPermission(getApplicationContext())) {
            Log.e(TAG, "Error in LocationUpdateWorker " + "SUCCESS");
            try {
                // Create the service connection.
                Context context = getApplicationContext();
                ServiceConnection connection = new ServiceConnection() {
                    @Override
                    public void onServiceConnected(ComponentName name, IBinder service) {
                        try {
                            if (hasLocationPermission(getApplicationContext())) {
                                if (sharedPrefs.getString("LOCATION_SERVICE_VERSION", "V2").equals("V1")) {
                                    System.out.println("OnServiceConnected called");
                                    LocationUpdateService.LocalBinder binder = (LocationUpdateService.LocalBinder) service;

                                    LocationUpdateService myService = binder.getService();

                                    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                                        context.startForegroundService(getServiceIntent(context));
                                    } else {
                                        context.startService(getServiceIntent(context));
                                    }
                                    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
                                        myService.startForeground(15082022, getNotification(), ServiceInfo.FOREGROUND_SERVICE_TYPE_LOCATION);
                                    } else {
                                        myService.startForeground(15082022, getNotification());
                                    }

                                    context.unbindService(this);
                                    Exception exception = new Exception("Location Update Service onServiceConnected " + driverId);
                                    FirebaseCrashlytics.getInstance().recordException(exception);
                                } else {
                                    System.out.println("OnServiceConnected called");
                                    LocationUpdateServiceV2.LocalBinder binder = (LocationUpdateServiceV2.LocalBinder) service;

                                    LocationUpdateServiceV2 myService = binder.getService();

                                    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                                        context.startForegroundService(getServiceIntent(context));
                                    } else {
                                        context.startService(getServiceIntent(context));
                                    }
                                    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
                                        myService.startForeground(15082022, getNotification(), ServiceInfo.FOREGROUND_SERVICE_TYPE_LOCATION);
                                    } else {
                                        myService.startForeground(15082022, getNotification());
                                    }

                                    context.unbindService(this);
                                    Exception exception = new Exception("Location Update Service onServiceConnected " + driverId);
                                    FirebaseCrashlytics.getInstance().recordException(exception);
                                }
                            }
                        }catch (Exception e){
                            Exception exception = new Exception("Location Update Service onServiceConnected Exception " + driverId + "exception = " + e);
                            FirebaseCrashlytics.getInstance().recordException(exception);
                        }
                    }

                    @Override
                    public void onBindingDied(ComponentName name) {
                        Log.w(TAG, "Binding has dead.");
                    }

                    @Override
                    public void onNullBinding(ComponentName name) {
                        Log.w(TAG, "Bind was null.");
                    }

                    @Override
                    public void onServiceDisconnected(ComponentName name) {
                        Log.w(TAG, "Service is disconnected..");
                    }
                };
                String driverStatus = sharedPrefs != null ? sharedPrefs.getString("DRIVER_STATUS_N", "__failed") : "__failed";
                if (!driverStatus.isEmpty() && !driverStatus.equals("null") && !driverStatus.equals("__failed") && !driverStatus.equals("Offline")) {
                    try {
                        context.bindService(getServiceIntent(context), connection,
                                Context.BIND_AUTO_CREATE);
                    } catch (Exception e) {
                        Exception exception = new Exception("Exception in Binding Not working for ID : " + driverId + " $ Error : " + e);
                        FirebaseCrashlytics.getInstance().recordException(exception);
                        Intent locationService;
                        if (sharedPrefs.getString("LOCATION_SERVICE_VERSION", "V2").equals("V1")) {
                            locationService = new Intent(context, LocationUpdateService.class);
                        } else {
                            locationService = new Intent(context, LocationUpdateServiceV2.class);
                        }
                        locationService.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK | Intent.FLAG_ACTIVITY_NEW_TASK);
                        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                            context.startForegroundService(locationService);
                        } else {
                            context.startService(locationService);
                        }
                    }

                    Intent restartIntent = context.getPackageManager().getLaunchIntentForPackage(context.getPackageName());
                    restartIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK | Intent.FLAG_ACTIVITY_NEW_TASK);
                    String activityStatus = sharedPrefs.getString("ACTIVITY_STATUS", "null");
                    if (sharedPrefs.getString("LOCATION_SERVICE_VERSION","V2").equals("V1")) {
                        if (Settings.canDrawOverlays(context) && activityStatus.equals("onDestroy")) {
                            try {
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
                }
            } catch (Exception e) {
                Log.e(TAG, "Error in LocationUpdateWorker " + e);
                if (sharedPrefs != null) driverId = sharedPrefs.getString("DRIVER_ID", "null");
                Exception exception = new Exception("Exception in LocationUpdateWorker for ID : " + driverId + " $ Error : " + e);
                FirebaseCrashlytics.getInstance().recordException(exception);
            }
        }else{
            if (sharedPrefs != null) {
                String appState = sharedPrefs.getString(this.getApplicationContext().getString(in.juspay.mobility.app.R.string.ACTIVITY_STATUS), "onCreate");
                Log.e(TAG, "Error in LocationUpdateWorker " + "No Location permission" + appState);
                if (!appState.equals("onCreate") && !appState.equals("onResume")) sendMissingPermissionNotification();
                Exception exception = new Exception("Exception in LocationUpdateWorker Permission Not for ID : " + driverId);
                FirebaseCrashlytics.getInstance().recordException(exception);
            }
            return Result.retry();
        }
        return Result.success();
    }

    private Notification getNotification() {
        createNotificationChannel();
        Intent notificationIntent = this.getApplicationContext().getPackageManager().getLaunchIntentForPackage(this.getApplicationContext().getPackageName());
        PendingIntent pendingIntent = PendingIntent.getActivity(this.getApplicationContext(), 10, notificationIntent, PendingIntent.FLAG_IMMUTABLE);
        NotificationCompat.Builder notification =
                new NotificationCompat.Builder(this.getApplicationContext(), LOCATION_UPDATES)
                        .setContentTitle("Updating")
                        .setContentText(this.getApplicationContext().getString(R.string.your_location_is_being_updated))
                        .setSmallIcon(Utils.getResIdentifier(this.getApplicationContext(), (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) ? "ic_launcher_small_icon" : "ny_ic_launcher", "drawable"))
                        .setPriority(NotificationCompat.PRIORITY_MIN)
                        .setOngoing(true)
                        .setContentIntent(pendingIntent);
        return notification.build();
    }

    private void createNotificationChannel() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            String LOCATION_DESCRIPTION = "LOCATION_IS_UPDATING";
            NotificationChannel channel = new NotificationChannel(LOCATION_UPDATES, "Location Update Service", NotificationManager.IMPORTANCE_MIN);
            channel.setDescription(LOCATION_DESCRIPTION);
            channel.setGroup("3_services");
            NotificationManager notificationManager = this.getApplicationContext().getSystemService(NotificationManager.class);
            notificationManager.createNotificationChannel(channel);
        }
    }

    private Intent getServiceIntent(Context context) {
        Intent locationService;
        if (sharedPrefs.getString("LOCATION_SERVICE_VERSION", "V2").equals("V1")) {
            locationService = new Intent(context, LocationUpdateService.class);
        } else {
            locationService = new Intent(context, LocationUpdateServiceV2.class);
        }
        locationService.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK | Intent.FLAG_ACTIVITY_NEW_TASK);
        return locationService;
    }
}
