package in.juspay.hypersdk.core;

import static com.caoccao.javet.enums.JSRuntimeType.V8;

import android.content.Context;
import android.webkit.WebView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.caoccao.javet.enums.JSRuntimeType;
import com.caoccao.javet.exceptions.JavetException;
import com.caoccao.javet.interop.V8Host;
import com.caoccao.javet.interop.V8Runtime;
import com.caoccao.javet.interop.engine.IJavetEngine;
import com.caoccao.javet.interop.engine.JavetEngineConfig;
import com.caoccao.javet.interop.engine.JavetEnginePool;
import com.caoccao.javet.interop.executors.BaseV8Executor;
import com.caoccao.javet.interop.executors.IV8Executor;
import com.caoccao.javet.interop.options.RuntimeOptions;
import com.caoccao.javet.interop.options.V8RuntimeOptions;
import com.caoccao.javet.javenode.JNEventLoop;
import com.caoccao.javet.javenode.enums.JNModuleType;
import com.caoccao.javet.values.reference.V8Script;
import com.caoccao.javet.values.reference.V8ValueFunction;
import com.caoccao.javet.values.reference.V8ValueObject;

import org.json.JSONArray;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import in.juspay.hyper.core.ExecutorManager;
import in.juspay.hypersdk.mystique.DUIWebViewClient;

public class JavetJSEngine implements JSEngine {
    private static final String LOG_TAG = JavetJSEngine.class.toString();
    Context context;
    private byte[] cacheCode;
    @Nullable
    private V8Runtime runtime;
    @Nullable
    private JavetEnginePool<V8Runtime> javetEnginePool;
    @Nullable
    private IJavetEngine<V8Runtime> engine;

    private final ScheduledExecutorService scheduler = Executors.newScheduledThreadPool(1);
    private final Map<Integer, ScheduledFuture<?>> timers = new HashMap<>();
    private int timerIdCounter = 0;

    public JavetJSEngine(Context context) {
        this.context = context;
        try {
            JavetLogger javetLogger = new JavetLogger("TestLogger");
//            JavetEngineConfig javetEngineConfig = new JavetEngineConfig();
//            javetEngineConfig.setJSRuntimeType(V8);
//            javetEngineConfig.setJavetLogger(javetLogger);
//            javetEngineConfig.setGlobalName("window");
//            javetEngineConfig.setPoolMaxSize(1);
//            JavetEnginePool<V8Runtime> javetEnginePool = new JavetEnginePool<>(javetEngineConfig);
//            this.javetEnginePool = javetEnginePool;
//            engine = javetEnginePool.getEngine();
//            this.runtime = engine.getV8Runtime();
//            this.runtime.getGlobalObject().setProperty("setTimeout",)
            V8RuntimeOptions options = new V8RuntimeOptions();
            options.setCreateSnapshotEnabled(false);
            byte[] cache = CacheUtils.readBytesFromCache(context,"index.bundle");
            if (cache != null) {
                cacheCode = cache;
                options.setSnapshotBlob(cache);
            }
            options.setGlobalName("window");
            this.runtime = V8Host.getV8Instance().createV8Runtime(options);
            JNEventLoop eventLoop = new JNEventLoop(this.runtime);
            eventLoop.loadStaticModules(JNModuleType.Console, JNModuleType.Timers);
            runtime.allowEval(true);
        } catch (Exception e) {
            e.printStackTrace();
//            JuspayLogger.e(LOG_TAG, e.toString());
        }
    }

    public String copyAssetToInternalStorage(Context context, String filename) {
        try (ByteArrayOutputStream bos = new ByteArrayOutputStream()) {
            try (InputStream is = context.getAssets().open("juspay/index_bundle.js")) {
                byte[] buffer = new byte[4096];
                int read;

                while ((read = is.read(buffer)) != -1) {
                    bos.write(buffer, 0, read);
                }
            }
            return bos.toString();
        } catch (IOException e) {
            e.printStackTrace();
        }
        return ""; // Returns the full path
    }

    @Override
    public void addJavascriptInterface(Object value, String key) {
        if (key.equals("JBridgeActual")) key = "JBridge";
        try {
            if (this.runtime != null) {
                V8ValueObject v8ValueObject = runtime.createV8ValueObject();
                v8ValueObject.bind(value);
                this.runtime.getGlobalObject().set(key, v8ValueObject);
            }
        } catch (JavetException e) {
            e.printStackTrace();
//            JuspayLogger.e(LOG_TAG, e.toString());
        }
    }

    public void mergeBridgeModules() throws Exception {
        V8ValueObject globalObject = runtime.getGlobalObject();

        // Ensure window.JBridge exists
        if (!globalObject.has("JBridge")) {
            globalObject.set("JBridge", runtime.createV8ValueObject());
        }

        V8ValueObject jBridge = globalObject.get("JBridge");

        // Check if window.BridgeList.getBridgeKeys exists
        if (globalObject.has("BridgeList")) {
            V8ValueObject bridgeList = globalObject.get("BridgeList");

            if (bridgeList.has("getBridgeKeys")) {
                // Call getBridgeKeys and parse JSON result
//                String bridgeKeysJson = bridgeList.invokeString("getBridgeKeys");
                List<String> bridgeKeys = new ArrayList<>();
                bridgeKeys.add("MobilityAppBridge");
                bridgeKeys.add("MobilityDriverBridge");
                for (String key : bridgeKeys) {
                    if (!globalObject.has(key)) continue;

                    V8ValueObject moduleObj = globalObject.getObject(key);

                    for (String methodName : moduleObj.getOwnPropertyNameStrings()) {
                        if (!jBridge.has(methodName)) {
                            V8ValueFunction method = moduleObj.getProperty(methodName);

// Evaluate: (function(f, ctx) { return f.bind(ctx); })(method, moduleObj)
                            try (V8ValueFunction binder = runtime.getExecutor("(function(f, ctx) { return f.bind(ctx); })").execute()) {
                                V8ValueFunction boundMethod = (V8ValueFunction) binder.call(null, method, moduleObj);
                                jBridge.set(methodName, boundMethod);
                                boundMethod.close();
                            }
                            method.close();

                            // Bind original module as `this` using arrow function-like closure
//                            V8ValueFunction boundMethod = method.bind(moduleObj);
//                            V8ValueFunction boundFunc = runtime.createV8ValueFunction((v8Runtime, thisObj, parameters) ->
//                                    method.call(moduleObj, parameters) // bind module as `this`
//                            );
//                            jBridge.set(methodName, boundFunc);
                        }
                    }

                    moduleObj.close(); // cleanup
                }

                bridgeList.close();
            }
        }

        jBridge.close();
    }


    @Override
    public void loadDataWithBaseURL(@Nullable String baseUrl, @NonNull String data, @Nullable String mimeType, @Nullable String encoding, @Nullable String historyUrl) {
        try {
            mergeBridgeModules();
            System.out.println("before loadData -> " + System.currentTimeMillis());
//            byte[] code =

            if (cacheCode == null) {
                try (ByteArrayOutputStream bos = new ByteArrayOutputStream()) {
                    try (InputStream is = context.getAssets().open("index_bundle.js")) {
                        byte[] buffer = new byte[4096];
                        int read;

                        while ((read = is.read(buffer)) != -1) {
                            bos.write(buffer, 0, read);
                        }
                    }
                    runtime.getExecutor("window.JOS = {};").executeVoid();
                    runtime.getExecutor("console.log(window.JBridge);").executeVoid();
                    String codes = bos.toString();
                    try {
                        runtime.getExecutor(codes).executeVoid();
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                    createSnapShot(codes);

                } catch (IOException e) {
                    e.printStackTrace();
                }
            } else {
                runtime.getExecutor("console.log (window.JBridge);").executeVoid();
                runtime.getExecutor(new String(cacheCode)).executeVoid();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void createSnapShot(String file) {
        ExecutorManager.runOnBackgroundThread(() -> {
            V8RuntimeOptions options = new V8RuntimeOptions();
            options.setCreateSnapshotEnabled(true);
            options.setGlobalName("window");
            try (V8Runtime v8Runtime = V8Host.getV8Instance().createV8Runtime(options)) {
                V8Script script = v8Runtime.compileV8Script(file,new byte[0],null,false);
//                byte[] cache = v8Runtime.createSnapshot();

                CacheUtils.saveBytesToCache(context, "index.bundle",script.getCachedData());
//                File f = new File(context.getFilesDir().getPath() + "/juspay/index_bundle.blob");
//                File dir = new File(context.getFilesDir(), "juspay");
//                if (!dir.exists()) {
//                    dir.mkdirs(); // Ensure the directory exists
//                }
//
//                File fs = new File(dir, "index_bundle.blob");
//                if (!f.exists()) {
//                    fs.createNewFile(); // Create file if it doesn't exist
//                }
//
//                try (FileOutputStream fos = new FileOutputStream(fs)) {
//                    fos.write(cache); // Assuming ex.getCachedData() returns byte[]
//                    System.out.println("File saved successfully at: " + f.getAbsolutePath());
//                } catch (Exception err) {
//                    err.printStackTrace();
//                }

            } catch (Exception e) {
                e.printStackTrace();
            }
        });
    }

    @Override
    public void evaluateJavascript(String s) {
        try {
            if (this.runtime != null) {
                this.runtime.getExecutor(s).executeString();
            }
        } catch (JavetException e) {
            e.printStackTrace();
//            JuspayLogger.e(LOG_TAG, e.toString());
        }
    }

    public int setTimeout(Runnable task, long delay) {
        int timerId = timerIdCounter++;
        ScheduledFuture<?> future = scheduler.schedule(task, delay, TimeUnit.MILLISECONDS);
        timers.put(timerId, future);
        return timerId;
    }

    public void clearTimeout(int timerId) {
        ScheduledFuture<?> future = timers.remove(timerId);
        if (future != null) {
            future.cancel(false);
        }
    }

    public int setInterval(Runnable task, long interval) {
        int timerId = timerIdCounter++;
        ScheduledFuture<?> future = scheduler.scheduleAtFixedRate(task, interval, interval, TimeUnit.MILLISECONDS);
        timers.put(timerId, future);
        return timerId;
    }

    public void clearInterval(int timerId) {
        clearTimeout(timerId);
    }

    @Override
    public void finalize() {
        scheduler.shutdown();
    }
}
