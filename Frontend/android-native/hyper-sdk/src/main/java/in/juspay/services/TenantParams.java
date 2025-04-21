package in.juspay.services;

import androidx.annotation.Keep;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import java.util.List;

import in.juspay.hyper.bridge.HyperBridge;

@Keep
public interface TenantParams {

    @Nullable
    default String getBootLoaderEndpoint() {
        return null;
    }

    @Nullable
    default String getBaseContent() {
        return null;
    }

    @NonNull
    String getNamespace();

    @NonNull
    List<Class<? extends HyperBridge>> getBridgeClasses();
}
