package in.juspay.hypersdk.core;

import android.os.Handler;
import android.os.Looper;
import androidx.annotation.Nullable;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

public class TimerPolyfillHermes {
    private final Handler handler = new Handler(Looper.getMainLooper());
    private final Map<Integer, Runnable> timers = new HashMap<>();
    private final AtomicInteger nextId = new AtomicInteger(1);
    private boolean isActive = true;

    public TimerPolyfillHermes() {

    }

    public int setTimeout(Runnable callback, int delay) {
        int id = nextId.getAndIncrement();
        Runnable r = () -> {
            if (isActive) {
                callback.run();
                timers.remove(id);
            }
        };
        handler.postDelayed(r, delay);
        timers.put(id, r);
        return id;
    }

    public int setInterval(Runnable callback, int interval) {
        int id = nextId.getAndIncrement();
        Runnable[] wrapper = new Runnable[1];
        wrapper[0] = () -> {
            if (isActive) {
                callback.run();
                handler.postDelayed(wrapper[0], interval);
            }
        };
        handler.postDelayed(wrapper[0], interval);
        timers.put(id, wrapper[0]);
        return id;
    }

    public void clearTimer(int id) {
        Runnable r = timers.remove(id);
        if (r != null) {
            handler.removeCallbacks(r);
        }
    }

//    @Override
//    public void onHostResume() {
//        isActive = true;
//    }
//
//    @Override
//    public void onHostPause() {
//        isActive = false;
//    }
//
//    @Override
//    public void onHostDestroy() {
//        isActive = false;
//        timers.clear();
//        handler.removeCallbacksAndMessages(null);
//    }
}
