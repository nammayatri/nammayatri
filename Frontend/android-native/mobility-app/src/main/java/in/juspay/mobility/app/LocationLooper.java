package in.juspay.mobility.app;

import android.os.Handler;
import android.os.HandlerThread;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

public class LocationLooper {

    private final BlockingQueue<Runnable> mQueue = new LinkedBlockingQueue<>();

    private final HandlerThread mHandlerThread;
    private final Handler mHandler;

    public LocationLooper() {
        mHandlerThread = new HandlerThread("LocationLooper");
        mHandlerThread.start();

        mHandler = new Handler(mHandlerThread.getLooper());
        processQueue();
    }

    private void processQueue() {
        mHandler.post(() -> {
            try {
                while (true) {
                    try {
                        Runnable task = mQueue.take();  // Take the next task (blocks if empty)
                        task.run();  // Execute API call
                    } catch (Exception e) {
                        Thread.currentThread().interrupt();
                        break;
                    }
                }
            } catch (Exception e) {
                Thread.currentThread().interrupt();
            }
        });
    }


    public void enqueue(Runnable runnable) {
        mQueue.add(runnable);
    }

    public void stop() {
        mHandlerThread.quitSafely();
    }

}
