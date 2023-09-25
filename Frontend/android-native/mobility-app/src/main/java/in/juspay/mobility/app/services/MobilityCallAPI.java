package in.juspay.mobility.app.services;

import android.content.Context;
import android.content.SharedPreferences;

import androidx.appcompat.app.AppCompatActivity;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import javax.net.ssl.HttpsURLConnection;

import in.juspay.mobility.app.R;
import in.juspay.mobility.app.TLSSocketFactory;


public class MobilityCallAPI extends AppCompatActivity {

    private static final String DEFAULT_API_METHOD = "POST";


    public static MobilityAPIResponse callAPI(String endpoint) {
        return callAPI(endpoint, null, null, DEFAULT_API_METHOD);
    }

    public static MobilityAPIResponse callAPI(String endpoint, Map<String, String> headers) {
        return callAPI(endpoint, headers, null, DEFAULT_API_METHOD);
    }

    public static MobilityAPIResponse callAPI(String endpoint, Map<String, String> headers, String requestBody) {
        return callAPI(endpoint, headers, requestBody, DEFAULT_API_METHOD);
    }

    public static MobilityAPIResponse callAPI(String endpoint, Map<String, String> headers, String requestBody, String apiMethod) {
        MobilityAPIResponse defaultResp = new MobilityAPIResponse();
        defaultResp.setResponseBody("");
        defaultResp.setStatusCode(-1);
        try {

            URL url = new URL(endpoint);

            HttpURLConnection connection = (HttpURLConnection) (new URL(endpoint).openConnection());
            if (connection instanceof HttpsURLConnection)
                ((HttpsURLConnection) connection).setSSLSocketFactory(new TLSSocketFactory());



            connection.setRequestMethod(apiMethod);

            if (headers != null) {
                for (Map.Entry<String, String> entry : headers.entrySet()) {
                    connection.setRequestProperty(entry.getKey(), entry.getValue());
                }
            }

            connection.setDoOutput(true);

            if (requestBody != null) {

                OutputStreamWriter writer = new OutputStreamWriter(connection.getOutputStream());
                writer.write(requestBody);
                writer.flush();
                writer.close();
            }

            connection.connect();

            int responseCode = connection.getResponseCode();

            MobilityAPIResponse response = new MobilityAPIResponse();
            response.setStatusCode(responseCode);

            if (responseCode >= 200 && responseCode < 300) {
                InputStream responseStream = connection.getInputStream();
                response.setResponseBody(apiResponseBuilder(responseStream));
            } else {
                InputStream responseStream = connection.getErrorStream();
                response.setResponseBody(apiResponseBuilder(responseStream));
            }
            return response;
        }catch (Exception e){
            return defaultResp;
        }
    }

    public static Map<String, String> getBaseHeaders(Context context){

        SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        String token = sharedPref.getString("REGISTERATION_TOKEN", "null");
        String bundle_version = sharedPref.getString("BUNDLE_VERSION", "null");
        String deviceDetails = sharedPref.getString("DEVICE_DETAILS", "null");
        String version = sharedPref.getString("VERSION_NAME", "null");

        Map<String, String> resultMap = new HashMap<>();
        resultMap.put("token", token);
        resultMap.put("x-bundle-version", bundle_version);
        resultMap.put("x-device", deviceDetails);
        resultMap.put("x-client-version", version);
        resultMap.put("Content-Type", "application/json");

        return resultMap;
    }

    private static String apiResponseBuilder(InputStream responseStream) {
        try{
            BufferedReader reader = new BufferedReader(new InputStreamReader(responseStream));
            StringBuilder responseBuilder = new StringBuilder();
            String line;
            while ((line = reader.readLine()) != null) {
                responseBuilder.append(line);
            }
            reader.close();
            return responseBuilder.toString();
        }catch (Exception e){
            return "This happened - " + e;
        }

    }

}
