package in.juspay.mobility.events.api;

import static in.juspay.mobility.BuildConfig.CONFIG_URL_DRIVER;
import static in.juspay.mobility.BuildConfig.CONFIG_URL_USER;
import static in.juspay.mobility.BuildConfig.MERCHANT_TYPE;

import android.util.Log;

import in.juspay.mobility.events.types.EventsApiResponse;
import in.juspay.mobility.events.types.SDKEventsReq;
import retrofit2.Call;
import retrofit2.Retrofit;
import retrofit2.converter.gson.GsonConverterFactory;
import retrofit2.http.Body;
import retrofit2.http.POST;

// API client for sending the request to the events server.
public class EventsApiClient {
    private static final String BASE_URL = MERCHANT_TYPE.equals("USER") ? CONFIG_URL_USER : CONFIG_URL_DRIVER;  // ReqRes API
    private static EventsService eventsService;

    public static EventsService getInstance() {
        if (eventsService == null) {
            Retrofit retrofit = new Retrofit.Builder().baseUrl(BASE_URL).addConverterFactory(GsonConverterFactory.create()).build();
            eventsService = retrofit.create(EventsService.class);
        }
        return eventsService;
    }

    public interface EventsService {
        @POST("sdk/events/")
            // ReqRes API for user creation
        Call<EventsApiResponse> sendEvents(@Body SDKEventsReq payload);
    }
}