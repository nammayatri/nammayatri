/*
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.common.services;

import android.content.Context;
import android.content.SharedPreferences;
import android.os.Handler;
import android.os.Looper;
import android.util.Log;

import org.json.JSONException;
import org.json.JSONObject;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.HttpURLConnection;
import java.net.URL;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import javax.net.ssl.HttpsURLConnection;

import okhttp3.Call;
import okhttp3.Callback;
import okhttp3.ConnectionPool;
import okhttp3.MediaType;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.RequestBody;
import okhttp3.Response;

import in.juspay.mobility.common.R;

public class MobilityCallAPI {

    /**
     * Interface for async API call callbacks
     */
    public interface APICallback {
        void onResponse(MobilityAPIResponse response);
        void onError(Exception error);
    }

    private static final String DEFAULT_API_METHOD = "POST";
    private static final String LOG_TAG = "MobilityAPI";
    private static volatile MobilityCallAPI instance;
    private static boolean USE_OKHTTP = false;
    private static final MediaType JSON = MediaType.get("application/json; charset=utf-8");
    
    // Thread pool for async HTTP requests when not using OkHttp
    private final ExecutorService executorService;
    private final Handler mainHandler;
    
    // OkHttpClient with Keep-Alive and Connection Pooling
    private OkHttpClient client;

    // Constructor to initialize OkHttpClient with Keep-Alive and Connection Pooling
    private MobilityCallAPI(Context context) {
        // Initialize thread pool for background operations
        executorService = Executors.newFixedThreadPool(5);
        mainHandler = new Handler(Looper.getMainLooper());
        
        SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        String config = sharedPref.getString("OK_HTTP_CONFIG", "null");
        try {
            JSONObject parsedConfig = new JSONObject(config);
            USE_OKHTTP = parsedConfig.optBoolean("useOkHttp", false);
            int maxIdleConnections = parsedConfig.optInt("maxIdleConnections", 5),
                    keepAliveDuration = parsedConfig.optInt("keepAliveDuration", 5);
            client = new OkHttpClient.Builder()
                    .connectionPool(new ConnectionPool(maxIdleConnections, keepAliveDuration, TimeUnit.MINUTES)) // default maxIdleConnections = 5 and keepAliveDuration = 5 minutes
                    .build();
        } catch (JSONException e) {
            Log.e(LOG_TAG, e.getMessage() != null ? e.getMessage() : "Unknown error occurred");
            USE_OKHTTP = false; // default to HttpURLConnection in case of any error
            
            // Initialize a basic OkHttpClient even if we're not using it by default
            client = new OkHttpClient.Builder()
                    .connectionPool(new ConnectionPool(5, 5, TimeUnit.MINUTES))
                    .build();
        }
    }

    public static MobilityCallAPI getInstance(Context context) {
        if (instance == null) {
            synchronized (MobilityCallAPI.class) {
                if (instance == null) {
                    instance = new MobilityCallAPI(context);
                }
            }
        }
        return instance;
    }

    // Existing synchronous methods (maintained for backward compatibility)
    
    public MobilityAPIResponse callAPI(String endpoint) {
        return callAPI(endpoint, null, null, DEFAULT_API_METHOD, true);
    }

    public MobilityAPIResponse callAPI(String endpoint, Map<String, String> headers) {
        return callAPI(endpoint, headers, null, DEFAULT_API_METHOD, true);
    }

    public MobilityAPIResponse callAPI(String endpoint, Map<String, String> headers, String requestBody) {
        return callAPI(endpoint, headers, requestBody, DEFAULT_API_METHOD, true);
    }

    public MobilityAPIResponse callAPI(String endpoint, Map<String, String> headers, String requestBody, String apiMethod) {
        return callAPI(endpoint, headers, requestBody, apiMethod, true);
    }

    public MobilityAPIResponse callAPI(String endpoint, Map<String, String> headers, String requestBody, String apiMethod, Boolean doOutput) {
        return USE_OKHTTP
                ? callAPIWithOkHttp(endpoint, headers, requestBody, apiMethod)
                : callAPIWithHttpURLConnection(endpoint, headers, requestBody, apiMethod, doOutput);
    }
    
    // New Async API methods
    
    public void callAPI(String endpoint, APICallback callback) {
        callAPI(endpoint, null, null, DEFAULT_API_METHOD, true, callback);
    }

    public void callAPI(String endpoint, Map<String, String> headers, APICallback callback) {
        callAPI(endpoint, headers, null, DEFAULT_API_METHOD, true, callback);
    }

    public void callAPI(String endpoint, Map<String, String> headers, String requestBody, APICallback callback) {
        callAPI(endpoint, headers, requestBody, DEFAULT_API_METHOD, true, callback);
    }

    public void callAPI(String endpoint, Map<String, String> headers, String requestBody, String apiMethod, APICallback callback) {
        callAPI(endpoint, headers, requestBody, apiMethod, true, callback);
    }

    public void callAPI(String endpoint, Map<String, String> headers, String requestBody, String apiMethod, Boolean doOutput, APICallback callback) {
        if (USE_OKHTTP) {
            callAPIWithOkHttpAsync(endpoint, headers, requestBody, apiMethod, callback);
        } else {
            callAPIWithHttpURLConnectionAsync(endpoint, headers, requestBody, apiMethod, doOutput, callback);
        }
    }

    // Sync implementations
    
    private MobilityAPIResponse callAPIWithOkHttp(String endpoint, Map<String, String> headers, String requestBody, String apiMethod) {
        MobilityAPIResponse response = new MobilityAPIResponse();
        try {
            Request.Builder requestBuilder = new Request.Builder()
                    .url(endpoint)
                    .method(apiMethod, requestBody != null ? RequestBody.create(requestBody, JSON) : null);

            if (headers != null) {
                for (Map.Entry<String, String> entry : headers.entrySet()) {
                    requestBuilder.addHeader(entry.getKey(), entry.getValue());
                }
            }

            try (Response okHttpResponse = client.newCall(requestBuilder.build()).execute()) {
                response.setStatusCode(okHttpResponse.code());
                response.setResponseBody(okHttpResponse.body() != null ? okHttpResponse.body().string() : "{}");
            }
        } catch (Exception e) {
            Log.e(LOG_TAG, e.getMessage() != null ? e.getMessage() : "Unknown error occurred");
            response.setStatusCode(-1);
            response.setResponseBody(e.getMessage() != null ? e.getMessage() : "{}");
        }
        return response;
    }

    private MobilityAPIResponse callAPIWithHttpURLConnection(String endpoint, Map<String, String> headers, String requestBody, String apiMethod, Boolean doOutput) {
        MobilityAPIResponse defaultResp = new MobilityAPIResponse();
        defaultResp.setResponseBody("");
        defaultResp.setStatusCode(-1);
        try {
            HttpURLConnection connection = callAPIConnection(endpoint, headers, requestBody, apiMethod, doOutput);
            int responseCode = connection.getResponseCode();

            MobilityAPIResponse response = new MobilityAPIResponse();
            response.setStatusCode(responseCode);
            response.setResponseBody(apiResponseBuilder(getResponseStream(connection)));
            return response;
        } catch (Exception e) {
            e.printStackTrace();
            return defaultResp;
        }
    }
    
    // Async implementations
    
    private void callAPIWithOkHttpAsync(String endpoint, Map<String, String> headers, String requestBody, String apiMethod, APICallback callback) {
        try {
            Request.Builder requestBuilder = new Request.Builder()
                    .url(endpoint)
                    .method(apiMethod, requestBody != null ? RequestBody.create(requestBody, JSON) : null);

            if (headers != null) {
                for (Map.Entry<String, String> entry : headers.entrySet()) {
                    requestBuilder.addHeader(entry.getKey(), entry.getValue());
                }
            }

            client.newCall(requestBuilder.build()).enqueue(new Callback() {
                @Override
                public void onFailure(Call call, IOException e) {
                    Log.e(LOG_TAG, "API call failed: " + e.getMessage());
                    // Deliver the callback on the main thread
                    mainHandler.post(() -> callback.onError(e));
                }

                @Override
                public void onResponse(Call call, Response response) throws IOException {
                    try {
                        MobilityAPIResponse apiResponse = new MobilityAPIResponse();
                        apiResponse.setStatusCode(response.code());
                        apiResponse.setResponseBody(response.body() != null ? response.body().string() : "{}");
                        
                        // Deliver the callback on the main thread
                        mainHandler.post(() -> callback.onResponse(apiResponse));
                    } catch (Exception e) {
                        Log.e(LOG_TAG, "Error processing response: " + e.getMessage());
                        mainHandler.post(() -> callback.onError(e));
                    } finally {
                        response.close();
                    }
                }
            });
        } catch (Exception e) {
            Log.e(LOG_TAG, "Error setting up API call: " + e.getMessage());
            mainHandler.post(() -> callback.onError(e));
        }
    }

    private void callAPIWithHttpURLConnectionAsync(String endpoint, Map<String, String> headers, String requestBody, String apiMethod, Boolean doOutput, APICallback callback) {
        executorService.execute(() -> {
            MobilityAPIResponse response = new MobilityAPIResponse();
            try {
                HttpURLConnection connection = callAPIConnection(endpoint, headers, requestBody, apiMethod, doOutput);
                int responseCode = connection.getResponseCode();

                response.setStatusCode(responseCode);
                response.setResponseBody(apiResponseBuilder(getResponseStream(connection)));
                
                // Deliver callback on main thread
                mainHandler.post(() -> callback.onResponse(response));
            } catch (Exception e) {
                Log.e(LOG_TAG, "Error in async HTTP request: " + e.getMessage());
                response.setStatusCode(-1);
                response.setResponseBody(e.getMessage() != null ? e.getMessage() : "{}");
                
                // Deliver error on main thread
                mainHandler.post(() -> callback.onError(e));
            }
        });
    }
    
    // Static helper methods
    
    public static InputStream getResponseStream(HttpURLConnection connection) throws IOException {
        int responseCode = connection.getResponseCode();
        InputStream responseStream;
        if (responseCode >= 200 && responseCode < 300) {
            responseStream = connection.getInputStream();
        } else {
            responseStream = connection.getErrorStream();
        }
        return responseStream;
    }

    public static HttpURLConnection callAPIConnection(String endpoint, Map<String, String> headers, String requestBody, String apiMethod, Boolean doOutput) throws IOException, NoSuchAlgorithmException, KeyManagementException {
        HttpURLConnection connection = (HttpURLConnection) (new URL(endpoint).openConnection());
        if (connection instanceof HttpsURLConnection)
            ((HttpsURLConnection) connection).setSSLSocketFactory(new TLSSocketFactory());


        connection.setRequestMethod(apiMethod);

        if (headers != null) {
            for (Map.Entry<String, String> entry : headers.entrySet()) {
                connection.setRequestProperty(entry.getKey(), entry.getValue());
            }
        }

        connection.setDoOutput(doOutput);

        if (requestBody != null) {
            OutputStreamWriter writer = new OutputStreamWriter(connection.getOutputStream());
            writer.write(requestBody);
            writer.flush();
            writer.close();
        }
        connection.connect();
        return connection;
    }


    public static MobilityAPIResponse callMultipartAPI(
            Context context,
            String filePath,
            String fileField,
            String uploadUrl,
            String fileType,
            Map<String, String> formData,
            String httpMethod
    ) {
        try {
            String boundary = UUID.randomUUID().toString();
            URL url = new URL(uploadUrl);
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            connection.setRequestMethod(httpMethod);
            connection.setDoOutput(true);
            connection.setUseCaches(false);
            connection.setRequestProperty("Content-Type", "multipart/form-data; boundary=" + boundary);
            SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
            connection.setRequestProperty("token", sharedPref.getString("REGISTERATION_TOKEN", "null"));

            try (DataOutputStream outputStream = new DataOutputStream(connection.getOutputStream())) {
                addFileField(outputStream, fileField, filePath, fileType, boundary);

                for (Map.Entry<String, String> entry : formData.entrySet()) {
                    addFormField(outputStream, entry.getKey(), entry.getValue(), boundary);
                }

                outputStream.writeBytes("--" + boundary + "--\r\n");
            }

            int responseCode = connection.getResponseCode();
            MobilityAPIResponse response = new MobilityAPIResponse();
            response.setStatusCode(responseCode);

            InputStream responseStream;
            if (responseCode >= 200 && responseCode < 300) {
                responseStream = connection.getInputStream();
            } else {
                responseStream = connection.getErrorStream();
            }
            response.setResponseBody(apiResponseBuilder(responseStream));
            return response;
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
    
    /**
     * Async version of multipart API call
     */
    public static void callMultipartAPIAsync(
            Context context,
            String filePath,
            String fileField,
            String uploadUrl,
            String fileType,
            Map<String, String> formData,
            String httpMethod,
            APICallback callback
    ) {
        ExecutorService executor = Executors.newSingleThreadExecutor();
        Handler handler = new Handler(Looper.getMainLooper());
        
        executor.execute(() -> {
            try {
                MobilityAPIResponse response = callMultipartAPI(
                    context, filePath, fileField, uploadUrl, fileType, formData, httpMethod);
                
                handler.post(() -> callback.onResponse(response));
            } catch (Exception e) {
                Log.e(LOG_TAG, "Error in async multipart request: " + e.getMessage());
                handler.post(() -> callback.onError(e));
            }
        });
    }

    public static Map<String, String> getBaseHeaders(Context context) {
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
        try {
            BufferedReader reader = new BufferedReader(new InputStreamReader(responseStream));
            StringBuilder responseBuilder = new StringBuilder();
            String line;
            while ((line = reader.readLine()) != null) {
                responseBuilder.append(line);
            }
            reader.close();
            return responseBuilder.toString();
        } catch (Exception e) {
            return "This happened - " + e;
        }
    }

    private static void addFileField(DataOutputStream outputStream, String fileField, String filePath, String fileType, String boundary) throws IOException {
        File file = new File(filePath);
        String fileName = file.getName();

        outputStream.writeBytes("--" + boundary + "\r\n");
        outputStream.writeBytes("Content-Disposition: form-data; name=\"" + fileField + "\"; filename=\"" + fileName + "\"\r\n");

        switch (fileType) {
            case "Image":
                outputStream.writeBytes("Content-Type: image/jpeg\r\n");
                break;
            case "Audio":
                outputStream.writeBytes("Content-Type: audio/mpeg\r\n");
                break;
            case "Video":
                outputStream.writeBytes("Content-Type: video/mp4\r\n");
                break;
        }

        outputStream.writeBytes("\r\n");

        try (FileInputStream fileInputStream = new FileInputStream(file)) {
            int bytesAvailable = fileInputStream.available();
            int maxBufferSize = 1024 * 1024;
            int bufferSize = Math.min(bytesAvailable, maxBufferSize);
            byte[] buffer = new byte[bufferSize];
            int bytesRead = fileInputStream.read(buffer, 0, bufferSize);

            while (bytesRead > 0) {
                outputStream.write(buffer, 0, bufferSize);
                bytesAvailable = fileInputStream.available();
                bufferSize = Math.min(bytesAvailable, maxBufferSize);
                bytesRead = fileInputStream.read(buffer, 0, bufferSize);
            }

            outputStream.writeBytes("\r\n");
        }
    }

    private static void addFormField(DataOutputStream outputStream, String fieldName, String fieldValue, String boundary) throws IOException {
        outputStream.writeBytes("--" + boundary + "\r\n");
        outputStream.writeBytes("Content-Disposition: form-data; name=\"" + fieldName + "\"\r\n");
        outputStream.writeBytes("Content-Type: application/json\r\n");
        outputStream.writeBytes("\r\n");
        outputStream.writeBytes(fieldValue + "\r\n");
    }
}
