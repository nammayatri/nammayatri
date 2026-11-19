package in.juspay.mobility.app.workers;

import android.content.Context;
import android.util.Log;

import androidx.annotation.NonNull;
import androidx.work.Data;
import androidx.work.OneTimeWorkRequest;
import androidx.work.WorkManager;
import androidx.work.Worker;
import androidx.work.WorkerParameters;
import java.util.concurrent.TimeUnit;

public class MyLocationWorker extends Worker {

    public MyLocationWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);
    }

    @NonNull
    @Override
    public Result doWork() {
        long startTime = getInputData().getLong("startTime", System.currentTimeMillis());
        long currentTime = System.currentTimeMillis();

        // Perform your work here
        Log.e("LocationServices", "Work started at: " + startTime + " and ended at: " + currentTime);
        // ...

        // Check if 2 hours have passed
        if (currentTime - startTime < 2 * 60 * 60 * 1000) {
            // Schedule next work request after 1 minute
            Data nextInputData = new Data.Builder()
                    .putLong("startTime", startTime)
                    .build();

            OneTimeWorkRequest nextWorkRequest = new OneTimeWorkRequest.Builder(MyLocationWorker.class)
                    .setInitialDelay(1, TimeUnit.SECONDS)
                    .setInputData(nextInputData)
                    .build();

            WorkManager.getInstance(getApplicationContext()).enqueue(nextWorkRequest);
        } else {
            // Schedule the next batch after a pause (assuming you want a specific break time, like 1 hour)
            long pauseDuration = 1 * 60 * 60 * 1000; // 1 hour
            long nextBatchStartTime = currentTime + pauseDuration;

            Data nextBatchInputData = new Data.Builder()
                    .putLong("startTime", nextBatchStartTime)
                    .build();

            OneTimeWorkRequest nextBatchWorkRequest = new OneTimeWorkRequest.Builder(MyLocationWorker.class)
                    .setInitialDelay(pauseDuration, TimeUnit.MILLISECONDS)
                    .setInputData(nextBatchInputData)
                    .build();

            WorkManager.getInstance(getApplicationContext()).enqueue(nextBatchWorkRequest);
        }

        return Result.success();
    }
}
