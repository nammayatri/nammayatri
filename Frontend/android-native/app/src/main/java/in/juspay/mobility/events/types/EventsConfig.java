package in.juspay.mobility.events.types;

public class EventsConfig {
    private boolean enabled;
    private int pushEventChunkSize;
    private long loggingIntervalInMs;

    public boolean isEnabled() {
        return enabled;
    }

    public int getPushEventChunkSize() {
        return pushEventChunkSize;
    }

    public long getLoggingIntervalInMs() {
        return loggingIntervalInMs;
    }
}