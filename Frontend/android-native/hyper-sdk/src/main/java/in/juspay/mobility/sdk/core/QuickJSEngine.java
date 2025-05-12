package in.juspay.mobility.sdk.core;

import android.content.Context;
import android.os.Build;
import android.util.Log;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.whl.quickjs.android.QuickJSLoader;
import com.whl.quickjs.wrapper.JSFunction;
import com.whl.quickjs.wrapper.JSObject;
import com.whl.quickjs.wrapper.QuickJSContext;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import in.juspay.mobility.sdk.hyper.bridge.HyperBridge;
import in.juspay.mobility.sdk.hyper.core.ExecutorManager;

public class QuickJSEngine implements JSEngine {
    private final Context context;
    private final QuickJSContext runtime;

    static {
        QuickJSLoader.init();
    }

    public QuickJSEngine(Context context) {
        this.context = context;
        this.runtime = QuickJSRuntime.getInstance();

    }

    @Override
    public void addJavascriptInterface(Object value, String key) {
        JSObject jsObj = runtime.createNewJSObject();

        Map<String, List<Method>> methodMap = new HashMap<>();
        for (Method method : value.getClass().getMethods()) {
            methodMap.computeIfAbsent(method.getName(), k -> new ArrayList<>()).add(method);
        }

        for (Map.Entry<String, List<Method>> entry : methodMap.entrySet()) {
            String methodName = entry.getKey();
            List<Method> overloads = entry.getValue();

            jsObj.setProperty(methodName, args -> {
                
                for (Method method : overloads) {
                    int paramCount = 0;
                    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                        paramCount = method.getParameterCount();
                    } else {
                        // Fallback: count manually
                        paramCount = method.getParameterTypes().length;
                    }
                    if (paramCount == args.length) {
                        Object[] safeArgs = new Object[args.length];
                        Class<?>[] paramTypes = method.getParameterTypes();

                        for (int i = 0; i < args.length; i++) {
                            if (args[i] == null && paramTypes[i].isPrimitive()) {
                                if (paramTypes[i] == boolean.class) {
                                    safeArgs[i] = false;
                                } else if (paramTypes[i] == int.class) {
                                    safeArgs[i] = 0;
                                } else if (paramTypes[i] == long.class) {
                                    safeArgs[i] = 0L;
                                } else if (paramTypes[i] == float.class) {
                                    safeArgs[i] = 0f;
                                } else if (paramTypes[i] == double.class) {
                                    safeArgs[i] = 0d;
                                } else if (paramTypes[i] == char.class) {
                                    safeArgs[i] = '\0';
                                } else if (paramTypes[i] == byte.class) {
                                    safeArgs[i] = (byte) 0;
                                } else if (paramTypes[i] == short.class) {
                                    safeArgs[i] = (short) 0;
                                } else {
                                    safeArgs[i] = 0; // fallback
                                }
                            } else {
                                safeArgs[i] = args[i];
                            }
                        }

                        try {
                            return method.invoke(value, safeArgs);
                        } catch (Exception e) {
                            // Try next overload
                        }
                    }
                }

                runtime.throwJSException("No matching overload found for method: " + methodName);
                return null;
            });
        }
        runtime.getGlobalObject().setProperty(key, jsObj);
        updateJbridge(value, key);
        jsObj.release();
    }


    private void updateJbridge(Object value, String key) {
        Object jbridge = runtime.getGlobalObject().getProperty("JBridge");
        if (jbridge == null) {
            JSObject jsObj = runtime.createNewJSObject();
            runtime.getGlobalObject().setProperty("JBridge", jsObj);
            jbridge = jsObj;
        }

        JSObject jbridgejs = (JSObject) jbridge;

        Map<String, List<Method>> methodMap = new HashMap<>();
        for (Method m : value.getClass().getMethods()) {
            methodMap.computeIfAbsent(m.getName(), k -> new ArrayList<>()).add(m);
        }

        for (Map.Entry<String, List<Method>> entry : methodMap.entrySet()) {
            String methodName = entry.getKey();
            List<Method> overloads = entry.getValue();

            if (jbridgejs.getProperty(methodName) == null) {
                jbridgejs.setProperty(methodName, args -> {
                    for (Method method : overloads) {
                        int paramCount = 0;
                        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                            paramCount = method.getParameterCount();
                        } else {
                            // Fallback: count manually
                            paramCount = method.getParameterTypes().length;
                        }
                        if (paramCount == args.length) {
                            Object[] safeArgs = new Object[args.length];
                            Class<?>[] paramTypes = method.getParameterTypes();

                            for (int i = 0; i < args.length; i++) {
                                if (args[i] == null && paramTypes[i].isPrimitive()) {
                                    if (paramTypes[i] == boolean.class) {
                                        safeArgs[i] = false;
                                    } else if (paramTypes[i] == int.class) {
                                        safeArgs[i] = 0;
                                    } else if (paramTypes[i] == long.class) {
                                        safeArgs[i] = 0L;
                                    } else if (paramTypes[i] == float.class) {
                                        safeArgs[i] = 0f;
                                    } else if (paramTypes[i] == double.class) {
                                        safeArgs[i] = 0d;
                                    } else if (paramTypes[i] == char.class) {
                                        safeArgs[i] = '\0';
                                    } else if (paramTypes[i] == byte.class) {
                                        safeArgs[i] = (byte) 0;
                                    } else if (paramTypes[i] == short.class) {
                                        safeArgs[i] = (short) 0;
                                    } else {
                                        safeArgs[i] = 0; // fallback
                                    }
                                } else {
                                    safeArgs[i] = args[i];
                                }
                            }

                            try {
                                Object res = method.invoke(value, safeArgs);
//                                System.out.println("Invoking Jbridge -> " + methodName + " " + Arrays.toString(safeArgs) + " result " + res);
                                return res;
                            } catch (Exception e) {
                                // Try next overload
                            }
                        }
                    }

                    runtime.throwJSException("No matching overload found for method: " + methodName);
                    return null;
                });
            }
        }
        jbridgejs.release();
        ((JSObject) jbridge).release();
    }

    @Override
    public void loadDataWithBaseURL(@Nullable String baseUrl, @NonNull String data, @Nullable String mimeType, @Nullable String encoding, @Nullable String historyUrl) {
//        runtime.updateJBridge();
//        new Thread(() -> {
//            try {
//
//                MobilityCallAPI mobilityApiHandler = MobilityCallAPI.getInstance(context);
//                MobilityAPIResponse resp = mobilityApiHandler.callAPI("http://192.168.11.122:8083/dist/index_bundle.js");
//                ExecutorManager.runOnMainThread(() -> {
//                    try {
//                        runtime.evaluateJavaScript(resp.getResponseBody().getBytes());
//                    } catch (Exception e) {
//                        e.printStackTrace();
//                    }
//                });
//            } catch (Exception e){
//
//            }
//        }).start();

//        runtime.updateJBridge();
        System.out.println("before loadData -> " + System.currentTimeMillis());
        runtime.evaluate("window.JOS = {};");
        byte[] code = CacheUtils.readBytesFromCache(context,"index.bundle");

        if (code == null) {
            try (ByteArrayOutputStream bos = new ByteArrayOutputStream()) {
                try (InputStream is = context.getAssets().open("index_bundle.js")) {
                    byte[] buffer = new byte[4096];
                    int read;

                    while ((read = is.read(buffer)) != -1) {
                        bos.write(buffer, 0, read);
                    }
                }
                runtime.evaluate(bos.toString());
                code = runtime.compile(bos.toString());
                CacheUtils.saveBytesToCache(context,"index.bundle",code);
            } catch (IOException e) {
                e.printStackTrace();
            }
        } else {
            runtime.execute(code);

        }
        try {

        } catch (Exception e){
            e.printStackTrace();
        }
        System.out.println("after loadData -> " + System.currentTimeMillis());
    }

    @Override
    public void evaluateJavascript(String s) {

        ExecutorManager.runOnJSThread(() -> {
            try {
                System.out.println("evaluateJavascript -> " + s);
                runtime.evaluate(s);
            } catch (Exception e) {
                e.printStackTrace();
            }
        });
    }

    public void invokeFnInJs(String s, Object... args) {
        Object js = runtime.getProperty(runtime.getGlobalObject(), s);
        if (js instanceof JSFunction){
            ((JSFunction) js).callVoid(args);
        }

    }
}
