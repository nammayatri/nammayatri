package in.juspay.mobility.events.store;

import android.content.Context;
import android.content.SharedPreferences;
import android.util.Log;

import com.google.common.reflect.TypeToken;
import com.google.gson.Gson;

import java.io.IOException;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;

import in.juspay.mobility.app.RemoteConfigs.MobilityRemoteConfigs;
import in.juspay.mobility.events.api.EventsApiClient;
import in.juspay.mobility.events.types.EventsConfig;
import in.juspay.mobility.events.types.EventPayload;
import in.juspay.mobility.events.types.EventsApiResponse;
import in.juspay.mobility.events.types.SDKEventsReq;
import retrofit2.Call;
import retrofit2.Response;

// The EventsStore structure will have a prestoLoaded boolean that will be default false for the very first time and the events will be empty
// During the course of the app the events will be appended and the prestoLoaded will be set to true
// When prestoLoaded does happen immediately clear the sharedpref
// If prestoLoaded is still false and the app crashed or closes try to send the events immediately anc clear the sharedPreference
// If not possible then during the next start up check for prestoEnabled and empty eventsList if not then send the events and continue the next cycle
// {
//  prestoLoaded: false
//  events: [..]
// }

public class EventsStore {
    private final String LOG_TAG = this.getClass().getSimpleName();
    private static final String PREF_NAME = "EventsStorePrefs";
    private static final String KEY_EVENTS = "events";
    private static final String KEY_IS_PRESTO_LOADED = "presto_loaded";
    private static volatile EventsStore instance;
    private final SharedPreferences sharedPreferences;
    private static final MobilityRemoteConfigs remoteConfigs = new MobilityRemoteConfigs(true, false);
    private final Gson gson = new Gson();
    private final Object lock = new Object();

    private EventsStore(Context c) {
        sharedPreferences = c.getSharedPreferences(PREF_NAME, Context.MODE_PRIVATE);
    }

    public static EventsStore getInstance(Context context) {
        if (instance == null) {
            synchronized (EventsStore.class) {
                if (instance == null) {
                    instance = new EventsStore(context);
                }
            }
        }
        return instance;
    }

    public void addEvent(EventPayload event) {
        synchronized (lock) {
            List<EventPayload> events = loadEvents();
            events.add(event);
            saveEvents(events);
        }
    }

    public void setPrestoLoaded(boolean value) {
        synchronized (lock) {
            sharedPreferences.edit().putBoolean(KEY_IS_PRESTO_LOADED, value).apply();
            if (value) {
                clearStore();
            }
        }
    }

    public boolean isPrestoLoaded() {
        synchronized (lock) {
            return sharedPreferences.getBoolean(KEY_IS_PRESTO_LOADED, false);
        }
    }

    private void saveEvents(List<EventPayload> events) {
        synchronized (lock) {
            sharedPreferences.edit().putString(KEY_EVENTS, gson.toJson(events)).apply();
        }
    }

    private static <T> List<List<T>> chunkList(List<T> list, int chunkSize) {
        List<List<T>> chunks = new ArrayList<>();
        int size = list.size();
        for (int i = 0; i < size; i += chunkSize) {
            chunks.add(new ArrayList<>(list.subList(i, Math.min(i + chunkSize, size))));
        }
        return chunks;
    }

    public boolean sendEvent(SDKEventsReq events) {
        Log.i(LOG_TAG, "SDKEventsReq is: " + events.getEvents().size() + " " + events);
        String events_data = remoteConfigs.getString("events_config");
        EventsConfig config = gson.fromJson(events_data, EventsConfig.class);
        int pushEventChunkSize = config.getPushEventChunkSize();
        List<List<EventPayload>> chunkedEvents = chunkList(events.getEvents(), pushEventChunkSize);
        Log.i(LOG_TAG, "chunked Events are: " + chunkedEvents.toString());
        boolean allSuccessful = true;
        for (List<EventPayload> batch : chunkedEvents) {
            SDKEventsReq batchRequest = new SDKEventsReq("NativeFlow", batch);
            Call<EventsApiResponse> call = EventsApiClient.getInstance().sendEvents(batchRequest);
            try {
                Response<EventsApiResponse> response = call.execute();
                Log.i(LOG_TAG, "Events sent successfully for batch: " + response.body() + "\n request: " + batchRequest.toString());
                if (!response.isSuccessful()) {
                    allSuccessful = false;
                }
            } catch (IOException ex) {
                Log.e(LOG_TAG, "IOException while sending batch events: ", ex);
                allSuccessful = false;
            }
        }
        return allSuccessful;
    }

    public void trySendAndClearEvents() {
        synchronized (lock) {
           if (!isPrestoLoaded()) {
            List<EventPayload> events = loadEvents();
            if (events.isEmpty()) {
                return;
            }
            SDKEventsReq payload = new SDKEventsReq("NativeFlow", events);
            Log.i(LOG_TAG, "Sent the request: " + " size: " + events.size() + " events: " + events.toString());
            boolean success = sendEvent(payload);
            if (success) {
                Log.i(LOG_TAG, "Store cleared since request was sent");
                clearStore();
            } else {
                Log.e(LOG_TAG, "Event sender failed to send the events");
            }
            Log.i(LOG_TAG, "Done with trySendAndClearEvents");
        }
       }
    }

    public void clearStore() {
        synchronized (lock) {
            sharedPreferences.edit().clear().apply();
        }
    }

    private List<EventPayload> loadEvents() {
        synchronized (lock) {
            String json = sharedPreferences.getString(KEY_EVENTS, null);
            Log.i(LOG_TAG, "Data from shared preference is: " + json);
            if (json == null) return new ArrayList<>();
            Type type = new TypeToken<List<EventPayload>>() {
            }.getType();
            return gson.fromJson(json, type);
        }
    }
}