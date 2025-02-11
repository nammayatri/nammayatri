package in.juspay.mobility.events.types;

import static in.juspay.mobility.BuildConfig.MERCHANT_TYPE;
import static in.juspay.mobility.BuildConfig.DEBUG;

import android.content.Context;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.util.Log;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import org.json.JSONObject;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

import in.juspay.hypersdk.data.KeyValueStore;
import in.juspay.mobility.Utils;

public class EventPayload {
    private final String level;
    private final String deviceUTCTimestamp;
    private final String service;

    @Nullable
    private final String sessionId;
    @Nullable
    private final String domainId;
    @Nullable
    private final String domainType;
    @Nullable
    private final String apiUrl;
    @Nullable
    private final String userId;
    @Nullable
    private final String stage;
    @Nullable
    private final String event;
    @Nullable
    private final String env;
    @Nullable
    private final String description;
    @Nullable
    private final String duration;
    @Nullable
    private final String apkVersion;
    @Nullable
    private final String bundleVersion;
    @Nullable
    private final String configVersion;
    @Nullable
    private final String cityId;
    @Nullable
    private final String os;
    private final static String LOG_TAG = "EventPayload";

    private static final boolean isCustomer = MERCHANT_TYPE.equals("USER");

    public EventPayload(String level, String deviceUTCTimestamp, String service,
                        @Nullable String sessionId, @Nullable String domainId, @Nullable String domainType,
                        @Nullable String apiUrl, @Nullable String userId, @Nullable String stage,
                        @Nullable String event, @Nullable String env, @Nullable String description,
                        @Nullable String duration, @Nullable String apkVersion, @Nullable String bundleVersion,
                        @Nullable String configVersion, @Nullable String cityId, @Nullable String os) {
        this.level = level;
        this.deviceUTCTimestamp = deviceUTCTimestamp;
        this.service = service;
        this.sessionId = sessionId;
        this.domainId = domainId;
        this.domainType = domainType;
        this.apiUrl = apiUrl;
        this.userId = userId;
        this.stage = stage;
        this.event = event;
        this.env = env;
        this.description = description;
        this.duration = duration;
        this.apkVersion = apkVersion;
        this.bundleVersion = bundleVersion;
        this.configVersion = configVersion;
        this.cityId = cityId;
        this.os = os;
    }
    // Getters
    public String getLevel() { return level; }
    public String getDeviceUTCTimestamp() { return deviceUTCTimestamp; }
    public String getService() { return service; }
    @Nullable public String getSessionId() { return sessionId; }
    @Nullable public String getDomainId() { return domainId; }
    @Nullable public String getDomainType() { return domainType; }
    @Nullable public String getApiUrl() { return apiUrl; }
    @Nullable public String getUserId() { return userId; }
    @Nullable public String getStage() { return stage; }
    @Nullable public String getEvent() { return event; }
    @Nullable public String getEnv() { return env; }
    @Nullable public String getDescription() { return description; }
    @Nullable public String getDuration() { return duration; }
    @Nullable public String getApkVersion() { return apkVersion; }
    @Nullable public String getBundleVersion() { return bundleVersion; }
    @Nullable public String getConfigVersion() { return configVersion; }
    @Nullable public String getCityId() { return cityId; }
    @Nullable public String getOs() { return os; }

    // Dummy Event for testing

    private static String getUTCTime() {
        long currentTimeMillis = System.currentTimeMillis();
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss", Locale.UK);
        sdf.setTimeZone(TimeZone.getTimeZone("IST"));
        return sdf.format(new Date(currentTimeMillis));
    }

    @Nullable
    private static String getValueFromStorage(String k, Context context) {
        return KeyValueStore.read(context.getApplicationContext(), context.getApplicationContext().getString(in.juspay.mobility.app.R.string.preference_file_key), k, null);
    }

    private static String getAppVersionName(Context context) {
        try {
            PackageInfo packageInfo = context.getApplicationContext().getPackageManager().getPackageInfo(context.getApplicationContext().getPackageName(), 0);
            return packageInfo.versionName;
        } catch (PackageManager.NameNotFoundException e) {
            e.printStackTrace();
            return "Unknown";
        }
    }

    private static String getUserId(Context context) {
        if (isCustomer) {
            return getValueFromStorage("CUSTOMER_ID", context.getApplicationContext());
        } else {
            return getValueFromStorage("DRIVER_ID", context.getApplicationContext());
        }
    }

    private static String getLocation(Context context) {
        if (isCustomer) {
            return getValueFromStorage("CUSTOMER_LOCATION", context.getApplicationContext());
        } else {
            return getValueFromStorage("DRIVER_LOCATION", context.getApplicationContext());
        }
    }

    private static String getEnvPayload(){
        if(DEBUG){
            return "MASTER";
        }
        return "PROD";
    }

    public static EventPayload createEventPayload(Context context, String eventName, @Nullable String description, @Nullable String duration) {
        String service = Utils.getService();
        String timeStamp = getUTCTime();
        String configVersion = getValueFromStorage("CONFIG_VERSION", context.getApplicationContext());
        String bundleVersion = getValueFromStorage("BUNDLE_VERSION", context.getApplicationContext());
        String apkVersion = getAppVersionName(context);
        String os = "Android";
        String userId = getUserId(context.getApplicationContext());
        String location = getLocation(context.getApplicationContext());
        String level = "debug";
        String env = getEnvPayload();
        Log.i(LOG_TAG, "Location is: " + location);
        return new EventPayload(
                level,
                timeStamp,
                service,
                null,
                null,
                null,
                null,
                userId,
                null,
                eventName,
                env,
                description,
                duration,
                apkVersion,
                bundleVersion,
                configVersion,
                location,
                os
        );
    }

    @NonNull
    @Override
    public String toString() {
        return "EventPayload{" +
                "level='" + level + '\'' +
                ", deviceUTCTimestamp='" + deviceUTCTimestamp + '\'' +
                ", service='" + service + '\'' +
                ", sessionId='" + sessionId + '\'' +
                ", domainId='" + domainId + '\'' +
                ", domainType='" + domainType + '\'' +
                ", apiUrl='" + apiUrl + '\'' +
                ", userId='" + userId + '\'' +
                ", stage='" + stage + '\'' +
                ", event='" + event + '\'' +
                ", env='" + env + '\'' +
                ", description='" + description + '\'' +
                ", duration='" + duration + '\'' +
                ", apkVersion='" + apkVersion + '\'' +
                ", bundleVersion='" + bundleVersion + '\'' +
                ", configVersion='" + configVersion + '\'' +
                ", cityId='" + cityId + '\'' +
                ", os='" + os + '\'' +
                '}';
    }}