package in.juspay.mobility.app.workers;

import android.content.Context;

import androidx.work.Data;
import androidx.work.OneTimeWorkRequest;
import androidx.work.WorkManager;

import java.util.concurrent.TimeUnit;

public class WorkScheduler {

    public static void startWork(Context context) {
        long startTime = System.currentTimeMillis();

        Data initialData = new Data.Builder()
                .putLong("startTime", startTime)
                .build();

        OneTimeWorkRequest initialWorkRequest = new OneTimeWorkRequest.Builder(MyLocationWorker.class)
                .setInitialDelay(1, TimeUnit.SECONDS)
                .setInputData(initialData)
                .build();

        WorkManager.getInstance(context).enqueue(initialWorkRequest);
    }
}
