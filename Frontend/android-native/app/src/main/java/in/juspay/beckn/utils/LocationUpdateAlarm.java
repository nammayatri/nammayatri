package in.juspay.mobility.utils;

import android.app.AlarmManager;
import android.content.Context;
import android.content.Intent;
import android.app.PendingIntent;
import java.util.Calendar;
import in.juspay.mobility.BootUpReceiver;

public class LocationUpdateAlarm {
    private static final int REQUEST_CODE_ALARM = 28022001;

    public void startAlarm(Context context) {
        System.out.println("start alarm manager");
        AlarmManager alarmManager = (AlarmManager) context.getSystemService(Context.ALARM_SERVICE);
        Intent intent = new Intent(context, BootUpReceiver.class);
        PendingIntent pendingIntent = PendingIntent.getBroadcast(context.getApplicationContext(), REQUEST_CODE_ALARM, intent, PendingIntent.FLAG_UPDATE_CURRENT);
        Calendar calendar = Calendar.getInstance();
        alarmManager.setInexactRepeating(AlarmManager.RTC_WAKEUP, calendar.getTimeInMillis(), 1000*60, pendingIntent);
    }

    public void stopAlarm (Context context){
        AlarmManager alarmManager = (AlarmManager) context.getSystemService(Context.ALARM_SERVICE);
        Intent intent = new Intent(context, BootUpReceiver.class);
        PendingIntent pendingIntent = PendingIntent.getBroadcast(context.getApplicationContext(), REQUEST_CODE_ALARM, intent, PendingIntent.FLAG_UPDATE_CURRENT);
        alarmManager.cancel(pendingIntent);
    }

}