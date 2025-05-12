package in.juspay.mobility.sdk.core;

import android.util.Log;

import com.whl.quickjs.android.QuickJSLoader;
import com.whl.quickjs.wrapper.QuickJSContext;

import in.juspay.mobility.sdk.hyper.core.ExecutorManager;


public class QuickJSRuntime {

    private static final String LOG_TAG = "QuickJSEngine";
    private static long threadID;
    private static QuickJSContext runtime;

    public static void initiateRuntime(QuickJSRuntimeCallback callback) {
        ExecutorManager.runOnJSThread(() -> {
            QuickJSLoader.init();
            threadID = Thread.currentThread().getId();
            runtime = QuickJSContext.create();
            runtime.setMaxStackSize(1024 * 1024);
            runtime.setEnableStackTrace(true);
            runtime.setProperty(runtime.getGlobalObject(), "window",runtime.getGlobalObject());
            runtime.setConsole(new QuickJSContext.Console() {
                @Override
                public void log(String info) {
                    if (info != null) Log.i(LOG_TAG, info);
                }

                @Override
                public void info(String info) {
                    if (info != null) Log.i(LOG_TAG, info);

                }

                @Override
                public void warn(String info) {
                    if (info != null) Log.w(LOG_TAG, info);

                }

                @Override
                public void error(String info) {
                    if (info != null) Log.e(LOG_TAG, info);
                }
            });
            TimerPolyfill.addTimerFunctions(runtime);
            callback.onDone();
        });
    }

    public static QuickJSContext getInstance() {
        if (threadID != Thread.currentThread().getId()) throw new RuntimeException("Wrong thread");
        if (runtime == null) runtime = QuickJSContext.create();
        return runtime;
    }

    public static interface QuickJSRuntimeCallback {
        void onDone();
    }
}
