package in.juspay.hypersdk.core;

import android.os.Handler;
import android.os.HandlerThread;
import android.os.Looper;

import com.whl.quickjs.wrapper.JSFunction;
import com.whl.quickjs.wrapper.QuickJSContext;

import java.util.Arrays;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

import in.juspay.hyper.core.ExecutorManager;

public class TimerPolyfill {

    private static final AtomicInteger timerIdGenerator = new AtomicInteger(1);
    private static final Map<Integer, Runnable> timeoutTasks = new ConcurrentHashMap<>();

    public static void addTimerFunctions(QuickJSContext runtime) {
        Handler backgroundHandler = new Handler(Looper.getMainLooper());

        // setTimeout
        runtime.getGlobalObject().setProperty("setTimeout", args -> {
            JSFunction callback = (JSFunction) args[0];
            int delayMs = ((Number) args[1]).intValue();
            int timerId = timerIdGenerator.getAndIncrement();
            Runnable task = () -> {
                timeoutTasks.remove(timerId); // Remove from map once executed
                ExecutorManager.runOnJSThread(() -> {
                    try {
                        Object[] remainingArgs = Arrays.copyOfRange(args, 2, args.length);
                        callback.call(remainingArgs);
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                });
            };

            timeoutTasks.put(timerId, task);
            backgroundHandler.postDelayed(task, delayMs);

            return timerId;
        });

        // clearTimeout
        runtime.getGlobalObject().setProperty("clearTimeout", args -> {
            int timerId = ((Number) args[0]).intValue();
            Runnable task = timeoutTasks.remove(timerId);
            if (task != null) {
                backgroundHandler.removeCallbacks(task);
            }
            return null;
        });

        // setInterval
        runtime.getGlobalObject().setProperty("setInterval", args -> {
            JSFunction callback = (JSFunction) args[0];
            int delayMs = ((Number) args[1]).intValue();
            int intervalId = timerIdGenerator.getAndIncrement();


            Runnable[] intervalTask = new Runnable[1];
            intervalTask[0] = () -> {
                ExecutorManager.runOnJSThread(() -> {
                    try {
                        Object[] remainingArgs = Arrays.copyOfRange(args, 2, args.length);
                        callback.call(remainingArgs);
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                });
                backgroundHandler.postDelayed(intervalTask[0], delayMs);
            };

            timeoutTasks.put(intervalId, intervalTask[0]);
            backgroundHandler.postDelayed(intervalTask[0], delayMs);

            return intervalId;
        });

        // clearInterval
        runtime.getGlobalObject().setProperty("clearInterval", args -> {
            int intervalId = ((Number) args[0]).intValue();
            Runnable task = timeoutTasks.remove(intervalId);
            if (task != null) {
                backgroundHandler.removeCallbacks(task);
            }
            return null;
        });

        // setImmediate
        runtime.getGlobalObject().setProperty("setImmediate", args -> {
            JSFunction callback = (JSFunction) args[0];
            ExecutorManager.runOnJSThread(() -> {
                try {
                    Object[] remainingArgs = Arrays.copyOfRange(args, 2, args.length);
                    callback.call(remainingArgs);
                } catch (Exception e) {
                    e.printStackTrace();
                }
            });
            return null;
        });
    }
}
