package in.juspay.mobility.app.services;

public class MobilityAPIResponse {

    private int statusCode;
    private String responseBody;

    public MobilityAPIResponse() {
    }

    public MobilityAPIResponse(int statusCode, String responseBody) {
        this.statusCode = statusCode;
        this.responseBody = responseBody;
    }

    public int getStatusCode() {
        return statusCode;
    }

    public void setStatusCode(int statusCode) {
        this.statusCode = statusCode;
    }

    public String getResponseBody() {
        return responseBody;
    }

    public void setResponseBody(String responseBody) {
        this.responseBody = responseBody;
    }
}

