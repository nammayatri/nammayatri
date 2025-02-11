package in.juspay.mobility;

import android.content.Context;
import android.util.Log;
import androidx.annotation.NonNull;
import java.util.concurrent.CountDownLatch;

import in.juspay.mobility.events.store.EventsStore;

// The CrashHandler class is used for handling uncaught exceptions in the runtime of the application
// It catches the exception sends the events and continues with the exception
public class CrashHandler implements Thread.UncaughtExceptionHandler {
    private final Thread.UncaughtExceptionHandler defaultHandler;
    private final Context context;
    private final String LOG_TAG = this.getClass().getSimpleName();

    public CrashHandler(Context context) {
        this.defaultHandler = Thread.getDefaultUncaughtExceptionHandler();
        this.context = context.getApplicationContext();
    }

    private void sendEventsSync() {
        final CountDownLatch latch = new CountDownLatch(1);
        new Thread(() -> {
            EventsStore.getInstance(context.getApplicationContext()).trySendAndClearEvents();
            latch.countDown();
        }).start();
        try {
            latch.await();
        } catch (InterruptedException ex) {
            Log.e(LOG_TAG, "Interrupted while waiting for crash events response", ex);
        }
    }

    @Override
    public void uncaughtException(@NonNull Thread t, @NonNull Throwable e) {
        Log.e(LOG_TAG, "uncaughtException: App is crashing!", e);
        sendEventsSync();
        if (defaultHandler != null) {
            defaultHandler.uncaughtException(t, e);
        }
    }
}
