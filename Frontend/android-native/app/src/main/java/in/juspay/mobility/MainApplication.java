package in.juspay.mobility;

import android.app.Application;

import com.facebook.react.PackageList;
import com.facebook.react.ReactApplication;
import com.facebook.react.ReactNativeHost;
import com.facebook.react.ReactPackage;
import com.facebook.react.defaults.DefaultReactNativeHost;
import com.facebook.soloader.SoLoader;
import com.finternet.sdk.MyEventPackage;

import java.util.List;

public class MainApplication extends Application implements ReactApplication {

    @Override
    public ReactNativeHost getReactNativeHost() {
        return createReactNativeHost();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        SoLoader.init(this, false);
    }


    public ReactNativeHost createReactNativeHost () {
        return new DefaultReactNativeHost(this) {
            @Override
            public boolean getUseDeveloperSupport() {
                return false;
            }

            @Override
            protected List<ReactPackage> getPackages() {
                @SuppressWarnings("UnnecessaryLocalVariable")
                List<ReactPackage> packages = new
                        PackageList(this).getPackages();
                packages.add(new MyEventPackage());
                return packages;
            }
            @Override
            protected String getJSMainModuleName() {
                return "index";
            }
            @Override
            protected boolean isNewArchEnabled() {
                return false;
            }
            @Override
            protected Boolean isHermesEnabled() {
                return false;
            }
        };
    }
}
