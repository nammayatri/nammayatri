package in.juspay.mobility.driver;
import android.app.Application;
import java.lang.reflect.Method;
import com.facebook.react.ReactApplication;
import com.facebook.react.ReactNativeHost;

public class MainApplication extends Application implements ReactApplication {

    @Override
    public ReactNativeHost getReactNativeHost() {
        try {
            Class<?> clazz = Class.forName("in.juspay.mobility.dynamicfeature.DynamicActivity");
            Class<?>[] paramTypes = { Application.class };
            Method method = clazz.getMethod("createReactNativeHost", paramTypes);
            Object[] args = { this };
            return (ReactNativeHost) method.invoke(null, this);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }
}
