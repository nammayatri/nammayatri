package in.juspay.hypersdk.utils;

import android.content.Context;

import androidx.annotation.NonNull;

import in.juspay.hypersdk.R;
import in.juspay.hypersdk.core.PaymentConstants;
import in.juspay.hypersdk.data.SdkInfo;

public class IntegrationUtils {

    public static SdkInfo getSdkInfo(@NonNull Context context) {
        final String sdkName = getAppName(context);
        final String sdkVersion = getGodelVersion(context);
        return new SdkInfo(sdkName, sdkVersion, isSdkDebuggable(context), usesLocalAssets(context));
    }

    public static String getAppName(@NonNull final Context context) {
        return context.getString(R.string.godel_app_name);
    }

    private static String getVersion(@NonNull final Context context, String key) {
        int resId = context.getResources().getIdentifier(key, "string", context.getPackageName());
        if (resId == 0) {
            return "undefined";
        }
        return context.getString(resId);
    }

    public static String getGodelVersion(@NonNull final Context context) {
        return getVersion(context, PaymentConstants.GODEL_VERSION);
    }

    public static String getGodelBuildVersion(@NonNull final Context context) {
        return getVersion(context, PaymentConstants.GODEL_BUILD_VERSION);
    }

    public static String getAssetAarVersion(@NonNull final Context context) {
        return getVersion(context, PaymentConstants.ASSET_AAR_VERSION);
    }

    public static String getSdkVersion(@NonNull final Context context) {
        return getSdkVersion(context, "-");
    }

    public static String getSdkVersion(@NonNull final Context context, @NonNull String separator) {
        StringBuilder version = new StringBuilder(getGodelVersion(context));
        int useRcResId = context.getResources().getIdentifier("use_rc", "bool", context.getPackageName());
        boolean useRc = context.getResources().getBoolean(useRcResId);
        if (useRc) {
            version.append(separator).append(getGodelBuildVersion(context));
        }
        return version.toString();
    }

    private static boolean isSdkDebuggable(@NonNull final Context context) {
        return context.getResources().getBoolean(R.bool.godel_debuggable);
    }

    private static boolean usesLocalAssets(@NonNull final Context context) {
        return context.getResources().getBoolean(R.bool.use_local_assets);
    }
}
