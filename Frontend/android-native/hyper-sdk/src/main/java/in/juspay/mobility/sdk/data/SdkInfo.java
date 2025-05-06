package in.juspay.mobility.sdk.data;

import androidx.annotation.Keep;
import androidx.annotation.NonNull;

import in.juspay.mobility.sdk.core.JuspayServices;

/**
 * Class containing the information about a Juspay SDK. Every SDK in Juspay will expose an instance of this class from
 * its own class which extends from the {@link JuspayServices} class.
 *
 * @author Sri Harsha Chilakapati [sri.harsha@juspay.in]
 * @since 03/06/2019
 */
@Keep
public final class SdkInfo {
    private final String sdkName;
    private final String sdkVersion;

    private final boolean sdkDebuggable;
    private final boolean usesLocalAssets;

    /**
     * Constructor for the SdkInfo class. Simple constructor that stores all the fields inside it.
     *
     * @param sdkName         The name of the Juspay SDK.
     * @param sdkVersion      The version string of the Juspay SDK.
     * @param sdkDebuggable   Is the SDK debuggable.
     * @param usesLocalAssets Whether the SDK uses local assets.
     */
    public SdkInfo(@NonNull String sdkName, @NonNull String sdkVersion, boolean sdkDebuggable, boolean usesLocalAssets) {
        this.sdkName = sdkName;
        this.sdkVersion = sdkVersion;
        this.sdkDebuggable = sdkDebuggable;
        this.usesLocalAssets = usesLocalAssets;
    }

    /**
     * @return The name of this Juspay SDK.
     */
    public String getSdkName() {
        return sdkName;
    }

    /**
     * @return The version string of this Juspay SDK.
     */
    public String getSdkVersion() {
        return sdkVersion;
    }

    /**
     * @return Whether the SDK is debuggable.
     */
    public boolean isSdkDebuggable() {
        return sdkDebuggable;
    }

    /**
     * @return Whether the SDK uses local assets.
     */
    public boolean usesLocalAssets() {
        return usesLocalAssets;
    }
}
