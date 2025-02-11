package in.juspay.mobility.events.types;

import androidx.annotation.NonNull;

public class EventsApiResponse {

    private String result;

    public EventsApiResponse(String r) {
        result = r;
    }

    public void setResult(String result) {
        this.result = result;
    }

    public String getResult() {
        return result;
    }

    @NonNull
    @Override
    public String toString() {
        return "{" + "\n" + "result=" + result + "\n" + "}";
    }
}