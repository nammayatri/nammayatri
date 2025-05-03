package in.juspay.hypersdk.core;


import android.content.Context;
import android.view.Choreographer;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.facebook.hermes.test.JSRuntime;
import com.facebook.hermes.test.TimerManager;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

import in.juspay.hyper.core.ExecutorManager;

public class HermesEngine implements JSEngine {
//    @NonNull
//    private final Choreographer choreographer;
    private final Choreographer.FrameCallback frameCallback = new Choreographer.FrameCallback() {
        @Override
        public void doFrame(long frameTimeNanos) {
            System.out.println("Frame Time -> " + frameTimeNanos);
            if (runtime != null) {
                // Call into native to tick timers
//                ExecutorManager.runOnJSThread(runtime::nativeTickJSI);
//                choreographer.postFrameCallback(this); // reschedule
            }
        }
    };
    private final Context context;
    @Nullable
    private final JSRuntime runtime;

    private final TimerManager timerManager;

    public HermesEngine(Context context) {
//        this.choreographer = choreographer;
        this.context = context;
        this.runtime = JSRuntime.makeHermesRuntimeWithHeapSpec(16 * 1024 * 1024,16 * 1024 * 1024);
        runtime.enableDebugger();
        runtime.evaluateJavaScript("var window = this;");
        timerManager = new TimerManager();
        timerManager.setCallback(new TimerManager.TimerCallback() {
            @Override
            public void onTimerTriggered(int i) {
                runtime.evaluateJavaScript(String.format("this.__triggerTimerCallback(%d)",i));
            }
        });
        runtime.setTimers(timerManager);
//        ExecutorManager.runOnMainThread(() -> {
//            choreographer.postFrameCallback(frameCallback);
//        });
    }


    @Override
    public void addJavascriptInterface(Object value, String key) {
        runtime.setGlobalProperty(key, value);
    }


    @Override
    public void loadDataWithBaseURL(@Nullable String baseUrl, @NonNull String data, @Nullable String mimeType, @Nullable String encoding, @Nullable String historyUrl) {
        runtime.updateJBridge();
//        try (ByteArrayOutputStream bos = new ByteArrayOutputStream()) {
//            try (InputStream is = context.getAssets().open("date.polyfill.bundle")) {
//                byte[] buffer = new byte[4096];
//                int read;
//
//                while ((read = is.read(buffer)) != -1) {
//                    bos.write(buffer, 0, read);
//                }
//            }
//            runtime.evaluateJavaScript(bos.toByteArray());
//        } catch (IOException e) {
//            e.printStackTrace();
//        }
        try (ByteArrayOutputStream bos = new ByteArrayOutputStream()) {
            try (InputStream is = context.getAssets().open("index.android.bundle")) {
                byte[] buffer = new byte[4096];
                int read;

                while ((read = is.read(buffer)) != -1) {
                    bos.write(buffer, 0, read);
                }
            }
            runtime.evaluateJavaScript(bos.toByteArray());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }


    @Override
    public void evaluateJavascript(String s) {
        System.out.println("String -> " + s);
        try {
            runtime.evaluateJavaScript(s);
        } catch (Exception e) {
            e.printStackTrace();
        }

    }


    public void invokeFnInJs(String s, String[] args) {
//        runtime
    }
}
