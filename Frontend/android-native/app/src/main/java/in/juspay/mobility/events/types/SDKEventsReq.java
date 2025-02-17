package in.juspay.mobility.events.types;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import java.util.List;

// Json type of the SDKEventsReq
public class SDKEventsReq {
    @Nullable
    private String flow;
    private List<EventPayload> events;

    // Constructor
    public SDKEventsReq(@Nullable String flow, List<EventPayload> events) {
        this.flow = flow;
        this.events = events;
    }

    // Getters
    @Nullable
    public String getFlow() {
        return flow;
    }

    public List<EventPayload> getEvents() {
        return events;
    }

    // Setters
    public void setEvent(@Nullable String event) {
        this.flow = event;
    }

    public void setEvents(List<EventPayload> events) {
        this.events = events;
    }

    public String getTitle() {
        return flow;
    }

    @NonNull
    @Override
    public String toString() {
        return "SDKEventsReq{" + "flow='" + flow + '\'' + ", events=" + events + '}';
    }
}