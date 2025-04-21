package in.juspay.hypersdk.core;

import androidx.annotation.Keep;
import androidx.annotation.Nullable;

import org.json.JSONObject;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;

@Keep
@SuppressWarnings("unused") // used in reflection
public class CallbackHandler implements InvocationHandler {
    private final JSONObject obj;
    private final InflateJSON infl;

    public CallbackHandler(InflateJSON infl, JSONObject methodProps) {
        this.obj = methodProps;
        this.infl = infl;
    }

    @Override
    @Nullable
    public Object invoke(Object proxy, Method method, Object[] args) {
        try {
            String methodName = method.getName();
            if (!obj.has(methodName)) {
                return method.invoke(this, args);
            }
            infl.putInState("proxy", proxy);
            infl.putInState("method", method);
            infl.putInState("args", args);
            return infl.runProps(obj, methodName, proxy);
        } catch (Throwable e) {
            infl.getDUI().getLogger().e("ERROR", "error in callback handler" + e.getMessage());
            // ignore this warning cos it happens
            // when the reflection is bad that it failed the command block and the catch block
            //noinspection SuspiciousInvocationHandlerImplementation
            return new Object();
        }


    }
}
