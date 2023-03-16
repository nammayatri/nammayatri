/* 
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.utils;

import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.SharedPreferences;
import android.graphics.BitmapFactory;
import android.location.LocationManager;
import android.os.Build;
import android.os.IBinder;
import android.util.Log;

import androidx.annotation.Nullable;
import androidx.core.app.NotificationCompat;
import androidx.core.app.NotificationManagerCompat;

import java.net.HttpURLConnection;
import java.net.URL;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.net.ssl.HttpsURLConnection;

import in.juspay.mobility.BuildConfig;
import in.juspay.mobility.MainActivity;
import in.juspay.mobility.R;

public class GpsListeningService extends Service {
    private IntentFilter intentFilter;
    private String channelId= "GPS_LISTENER";
    final int alertNotificationId = 27081999;
    private String TAG = "GpsListeningService";
    final int gpsForegroundServiceId = 01112022;


    private BroadcastReceiver gpsReceiver = new BroadcastReceiver(){
        @Override
        public void onReceive(Context context, Intent intent) {
            LocationManager locationManager = (LocationManager) context.getSystemService(Context.LOCATION_SERVICE);
            Boolean isGpsEnabled = locationManager.isProviderEnabled(LocationManager.GPS_PROVIDER);
            if (isGpsEnabled){ //LocationEnabled
                ExecutorService executor = Executors.newSingleThreadExecutor();
                executor.execute(() -> {
                    Boolean isApiSuccess = updateDriverStatus(true);
                    if (isApiSuccess){
                        startLocationService();
                        showAlertNotification();
                    }
                });
                onDestroy();
            }
        }
    };

    private void startLocationService() {
        Intent locationUpdateService = new Intent(this, LocationUpdateService.class);
        locationUpdateService.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK | Intent.FLAG_ACTIVITY_NEW_TASK);
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            this.getApplicationContext().startForegroundService(locationUpdateService);
        }else {
            this.startService(locationUpdateService);
        }
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        this.startForeground(gpsForegroundServiceId, createNotification());
        return START_STICKY;
    }

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    @Override
    public void onCreate() {
        super.onCreate();
        createNotificationChannel();
        this.startForeground(gpsForegroundServiceId, createNotification());
        intentFilter = new IntentFilter(LocationManager.PROVIDERS_CHANGED_ACTION);
        registerReceiver(gpsReceiver,intentFilter);
    }

    @Override
    public void onDestroy() {
        try {
            unregisterReceiver(gpsReceiver);
        }catch (IllegalArgumentException e){
            e.printStackTrace();
        }
        stopForeground(true);
        stopSelf();
        super.onDestroy();
    }

    private void createNotificationChannel() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            NotificationChannel channel = new NotificationChannel(channelId, "GPS_SERVICE", NotificationManager.IMPORTANCE_MIN);
            NotificationManager notificationManager = getSystemService(NotificationManager.class);
            notificationManager.createNotificationChannel(channel);
        }
    }

    private Notification createNotification() {
        Intent notificationIntent = new Intent(this, MainActivity.class);
        PendingIntent pendingIntent = PendingIntent.getActivity(this, 10, notificationIntent, PendingIntent.FLAG_IMMUTABLE);
        NotificationCompat.Builder notification =
                new NotificationCompat.Builder(this, channelId)
                        .setContentTitle("GPS")
                        .setContentText(getString(R.string.waiting_for_gps_signal))
                        .setSmallIcon(R.drawable.ny_ic_launcher)
                        .setProgress(100, 0 , true)
                        .setPriority(NotificationCompat.PRIORITY_MIN)
                        .setOngoing(true)
                        .setContentIntent(pendingIntent);
        return notification.build();
    }

    private void showAlertNotification() {
        Intent notificationIntent = new Intent(this, MainActivity.class);
        PendingIntent pendingIntent = PendingIntent.getActivity(this, alertNotificationId, notificationIntent, PendingIntent.FLAG_IMMUTABLE);
        NotificationCompat.Builder mBuilder = new NotificationCompat.Builder(this,"General") ;
        mBuilder.setLargeIcon(BitmapFactory.decodeResource(this.getResources(), R.drawable.ny_ic_launcher));
        mBuilder.setContentTitle(getString(R.string.we_made_you_online))
                .setSmallIcon((R.drawable.ny_ic_launcher))
                .setContentText(getString(R.string.location_is_turned_on))
                .setAutoCancel(true)
                .setPriority(NotificationCompat.PRIORITY_MAX);
        mBuilder.setContentIntent(pendingIntent);
        NotificationManagerCompat notificationManager = NotificationManagerCompat.from(this);
        notificationManager.notify(alertNotificationId, mBuilder.build());
    }

    private boolean updateDriverStatus(Boolean status) {
            SharedPreferences sharedPref = this.getSharedPreferences(this.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
            String token = sharedPref.getString("REGISTERATION_TOKEN", "null");
            String bundle_version = sharedPref.getString("BUNDLE_VERSION","null");
            String baseUrl = sharedPref.getString("BASE_URL", "null");
            try {
                String orderUrl = baseUrl + "/driver/setActivity?active=" + status;
                HttpURLConnection connection = (HttpURLConnection) (new URL(orderUrl).openConnection());
                if (connection instanceof HttpsURLConnection)
                    ((HttpsURLConnection) connection).setSSLSocketFactory(new TLSSocketFactory());
                connection.setRequestMethod("POST");
                connection.setRequestProperty("Content-Type", "application/json");
                connection.setRequestProperty("x-client-version", BuildConfig.VERSION_NAME);
                connection.setRequestProperty("token", token);
                connection.setRequestProperty("x-bundle-version", bundle_version);
                connection.setDoOutput(true);
                connection.connect();

                int respCode = connection.getResponseCode();
                if ((respCode < 200 || respCode >= 300) && respCode != 302) {
                    return false; //Error
                } else {
                    SharedPreferences.Editor editor = sharedPref.edit();
                    editor.putString("DRIVER_STATUS","true");
                    editor.apply();
                    return true; //Api Success
                }
            } catch (Exception error) {
                Log.d(TAG, "Exception in updateDriverStatus : "+error);
                return false;
            }
    }
}
