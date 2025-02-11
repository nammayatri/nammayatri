package in.juspay.mobility.events.worker;

import android.content.Context;
import android.util.Log;

import androidx.annotation.NonNull;
import androidx.work.Worker;
import androidx.work.WorkerParameters;

import in.juspay.mobility.events.store.EventsStore;

// The EventsWorker is used for sending events when the app is closed.
// This work is only scheduled when the app is force removed from the Recents by the user.
public class EventsWorker extends Worker {

    private final String LOG_TAG = this.getClass().getSimpleName();

    public EventsWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);
    }

    @NonNull
    @Override
    public Result doWork() {
        Log.i(LOG_TAG, "EventsWorker has started it's job");
        EventsStore.getInstance(getApplicationContext()).trySendAndClearEvents();
        Log.i(LOG_TAG, "EventsWorker has completed it's job");
        return Result.success();
    }
}