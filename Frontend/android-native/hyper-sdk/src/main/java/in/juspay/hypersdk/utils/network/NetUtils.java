package in.juspay.hypersdk.utils.network;

import android.content.Context;

import androidx.annotation.IntRange;
import androidx.annotation.Keep;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import org.json.JSONObject;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.net.URLEncoder;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLSocketFactory;

import in.juspay.hyper.constants.Labels;
import in.juspay.hyper.constants.LogCategory;
import in.juspay.hyper.constants.LogLevel;
import in.juspay.hyper.constants.LogSubCategory;
import in.juspay.hypersdk.R;
import in.juspay.hypersdk.core.SdkTracker;
import in.juspay.hypersdk.security.HyperSSLSocketFactory;

import okhttp3.Call;
import okhttp3.MediaType;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.RequestBody;
import okhttp3.Response;
import okio.BufferedSink;

/**
 * Utility functions to call URL's using pure URLConnections
 *
 * @author Sriduth
 */
@SuppressWarnings("JavaDoc")
public class NetUtils {
    private static final String HTTP_CACHE_NAME = "hyper-http-cache";
    private static final MediaType MEDIA_TYPE_TEXT = MediaType.parse("text/plain");

    @Nullable
    private static String packageName = null;
    private static String godelAppName = null;
    private static String USER_AGENT;

    private int readTimeout;
    public static final OkHttpClient DEFAULT_HTTP_CLIENT = buildHttpClient();
    static {
        USER_AGENT = System.getProperty("http.agent");

        if (USER_AGENT == null || USER_AGENT.length() == 0) {
            USER_AGENT = "Juspay Express Checkout Android SDK";
        }
    }

    private final boolean sslPinningRequired;
    private int connectionTimeout;

    private SSLSocketFactory sslSocketFactory;

    private static OkHttpClient buildHttpClient() {
        // File cacheDir = JuspayCoreLib
        //         .getApplicationContext()
        //         .getDir(HTTP_CACHE_NAME, Context.MODE_PRIVATE);
        // long ONE_MB = 1024L * 1024L;
        // Cache httpCache = new Cache(cacheDir, 20 * ONE_MB);
        return new OkHttpClient.Builder()
                .cache(null)
                /* This settings controls the following of redirects from HTTPS to HTTP & vice-versa.
                 * HttpUrlConnection has it disabled by default, so for the sake of parity this has
                 * also been diabled in okhttp.
                 */
                .followSslRedirects(false)
                .build();
    }

    private static void setHeaders(
            @NonNull Request.Builder requestBuilder,
            @Nullable Map<String, String> headers
    ) {
        if (headers != null) {
            for (Map.Entry<String, String> header : headers.entrySet()) {
                String name = header.getKey();
                String value = header.getValue();
                if (name == null || value == null) {
                    continue;
                }
                boolean isAcceptingGZip =
                        name.equalsIgnoreCase("accept-encoding")
                        && value.equalsIgnoreCase("gzip");
                if (isAcceptingGZip) {
                    /*
                     * This header is not needed as Okhttp will handle adding this header
                     * & decoding the body. And if you add it here, we'll have to handle
                     * gzip decoding.
                     */
                    continue;
                }
                requestBuilder.header(name, value);
            }
        }
    }

    private static RequestBody createRequestBody(
            @NonNull byte[] rawBody,
            @Nullable String contentType
    ) {
        return new RequestBody() {
            @Override
            public MediaType contentType() {
                if (contentType == null || MediaType.parse(contentType) == null) {
                    return MEDIA_TYPE_TEXT;
                } else {
                    return MediaType.parse(contentType);
                }
            }

            @Override
            public void writeTo(@NonNull BufferedSink bufferedSink) throws IOException {
                bufferedSink.write(rawBody);
            }
        };
    }

    @Nullable
    private static String getContentType(@Nullable Map<String, String> headers) {
        String contentType = null;
        if (headers != null) {
            contentType = headers.get("content-type");
            if (contentType == null) {
                contentType = headers.get("Content-Type");
            }
        }

        return contentType;
    }

    public static void setApplicationHeaders(@NonNull Context ctx) {
        packageName = ctx.getPackageName();
        godelAppName = ctx.getString(R.string.godel_app_name);
    }

    @SuppressWarnings("SameParameterValue")
    public NetUtils(@IntRange(from = 0, to = Integer.MAX_VALUE) int connectionTimeout,
                    @IntRange(from = 0, to = Integer.MAX_VALUE) int readTimeout) throws NoSuchAlgorithmException, KeyManagementException {
        this(connectionTimeout, readTimeout, false);
    }

    @SuppressWarnings("SameParameterValue")
    public NetUtils(@IntRange(from = 0, to = Integer.MAX_VALUE) int connectionTimeout,
                    @IntRange(from = 0, to = Integer.MAX_VALUE) int readTimeout,
                    boolean sslPinningRequired) throws NoSuchAlgorithmException, KeyManagementException {
        this.connectionTimeout = connectionTimeout;
        this.readTimeout = readTimeout;
        this.sslPinningRequired = sslPinningRequired;
        this.sslSocketFactory = new JuspaySSLSocketFactory();
    }

    @Keep
    public String generateQueryString(@Nullable Map<String, String> queryString)
            throws UnsupportedEncodingException {
        StringBuilder sb = new StringBuilder();
        if (queryString != null) {
            for (Map.Entry<String, String> e : queryString.entrySet()) {
                if (sb.length() > 0) {
                    sb.append('&');
                }
                sb.append(URLEncoder.encode(e.getKey(), "UTF-8")).append('=')
                        .append(URLEncoder.encode(e.getValue(), "UTF-8"));
            }
        }

        return sb.toString();
    }

    public Response doPut(@NonNull final Context context, URL url, byte[] data, @Nullable Map<String, String> headers, NetUtils netUtils, JSONObject options, @Nullable String tag) throws Exception {
        return doMethod("PUT", url.toString(), null, headers, data, null, options, tag);
    }

    public void setReadTimeout(@IntRange(from = 0, to = Integer.MAX_VALUE) int readTimeout) {
        this.readTimeout = readTimeout;
    }

    public void setConnectionTimeout(@IntRange(from = 0, to = Integer.MAX_VALUE) int connectionTimeout) {
        this.connectionTimeout = connectionTimeout;
    }

    protected Map<String, String> getDefaultSDKHeaders() {
        Map<String, String> defaultHeaders = new HashMap<>();

        defaultHeaders.put("User-Agent", USER_AGENT);
        defaultHeaders.put("Accept-Language", "en-US,en;q=0.5");
        defaultHeaders.put("X-Powered-By", "Juspay EC SDK for Android");
        defaultHeaders.put("X-App-Name", godelAppName);
        defaultHeaders.put("Referer", packageName);

        return defaultHeaders;
    }

    private void setDefaultSDKHeaders(@NonNull HttpsURLConnection conn) {
        for (Map.Entry<String, String> h : getDefaultSDKHeaders().entrySet()) {
            String name = h.getKey();
            String value = h.getValue();
            if (name != null && value != null) {
                conn.setRequestProperty(name, value);
            }
        }
    }

    /**
     * Template to be the JSON type : {@link JSONObject} or {@link org.json.JSONArray}
     *
     * @param <T>  Type of JSON: Object / Array
     * @param url  url to send the post request to
     * @param json JSON payload
     * @return String response
     * @throws IOException
     */
    public <T> Response postJson(URL url, T json, JSONObject options) throws IOException {
        return doPost(url, json.toString().getBytes(), "application/json", null, options, null);
    }

    /**
     * POSTS the given key value params as form data
     *
     * @param url         {@inheritDoc}
     * @param queryParams {@inheritDoc}
     * @return {@inheritDoc}
     * @throws IOException
     */
    public Response postForm(URL url, Map<String, String> queryParams, JSONObject options)
            throws IOException {
        return doPost(url, generateQueryString(queryParams).getBytes(), "application/x-www-form-urlencoded", null, options, null);
    }

    /**
     * Do a simple post to a url
     *
     * @param url
     * @return
     * @throws IOException
     */
    public Response postUrl(URL url, Map<String, String> urlParams, JSONObject options)
            throws IOException {
        return doPost(url, generateQueryString(urlParams).getBytes(), "application/x-www-form-urlencoded", null, options, null);
    }

    /**
     * Do a simple post to a url
     *
     * @param url
     * @return
     * @throws IOException
     */
    public Response postUrl(URL url, Map<String, String> headers, Map<String, String> urlParams, JSONObject options, @Nullable String tag)
            throws IOException {
        return doPost(url, generateQueryString(urlParams).getBytes(), "application/json", headers, options, tag);
    }

    /**
     * Do a simple post to a url
     *
     * @param url
     * @return
     * @throws IOException
     */
    public Response postUrl(URL url, Map<String, String> headers, String urlParams, JSONObject options, @Nullable String tag)
            throws IOException {
        return doPost(url, urlParams.getBytes(), "application/x-www-form-urlencoded", headers, options, tag);
    }

    /**
     * @param url URL to post to
     * @return JuspayHttpsResponse object having wrapped response contents.
     * @throws IOException
     */
    public Response postUrl(URL url, JSONObject options) throws IOException {
        return doPost(url, null, "application/x-www-form-urlencoded", null, options, null);
    }

    public Response deleteUrl(URL url, Map<String, String> urlParams, JSONObject options)
            throws IOException {
        return doDelete(url, generateQueryString(urlParams).getBytes(), "application/x-www-form-urlencoded", null, options, null);
    }

    public Response deleteUrl(URL url, Map<String, String> headers, Map<String, String> urlParams, JSONObject options, @Nullable String tag)
            throws IOException {
        return doDelete(url, generateQueryString(urlParams).getBytes(), "application/json", headers, options, tag);
    }

    public Response deleteUrl(URL url, Map<String, String> headers, String urlParams, JSONObject options, @Nullable String tag)
            throws IOException {
        return doDelete(url, urlParams.getBytes(), "application/x-www-form-urlencoded", headers, options, tag);
    }

    // ------------------

    public Response deleteUrl(URL url, JSONObject options) throws IOException {
        return doDelete(url, null, "application/x-www-form-urlencoded", null, options, null);
    }

    public Response doPost(URL url, byte[] payload, String defaultContentType, @Nullable Map<String, String> headers, JSONObject options, @Nullable String tag) throws IOException {
        return doMethod("POST", url.toString(), null, headers, payload, defaultContentType, options, tag);
    }

    public Response doGet(@NonNull String url) throws IOException {
        return doGet(url, null, null, null, null);
    }

    public Response doGet(@NonNull String url, @Nullable Map<String, String> headers, @Nullable Map<String, String> queryParams, JSONObject options, @Nullable String tag) throws IOException {
        return doMethod("GET", url, queryParams, headers, null, null, options, tag);
    }

    public Response doHead(@NonNull String url, @Nullable Map<String, String> headers, @Nullable Map<String, String> queryParams, JSONObject options, @Nullable String tag) throws IOException {
        return doMethod("HEAD", url, queryParams, headers, null, null, options, tag);
    }

    public Response doDelete(URL url, byte[] payload, String defaultContentType, @Nullable Map<String, String> headers, JSONObject options, @Nullable String tag) throws IOException {
        return doMethod("DELETE", url.toString(), null, headers, payload, defaultContentType, options, tag);
    }

    private Response doMethod(
            @NonNull String method,
            @NonNull String url,
            @Nullable Map<String, String> queryParams,
            @Nullable Map<String, String> headers,
            @Nullable byte[] payload,
            @Nullable String defaultContentType,
            @Nullable JSONObject options,
            @Nullable String tag) throws IOException {
        String query = generateQueryString(queryParams);
        StringBuilder s = new StringBuilder(url);
        if (!query.isEmpty()) {
            s.append("?").append(query);
            url = s.toString();
        }
        Request.Builder requestBuilder = new Request.Builder()
                .url(url);
        if (tag != null) {
            requestBuilder.tag(tag);
        }

        if (headers != null) {
            setHeaders(requestBuilder, headers);
        }
        setHeaders(requestBuilder, getDefaultSDKHeaders());

        if (payload != null) {
            String contentType = getContentType(headers);
            if (contentType == null) {
                contentType = defaultContentType;
            }
            RequestBody requestBody = createRequestBody(payload, contentType);
            requestBuilder.method(method, requestBody);
        } else {
            requestBuilder.method(method, null);
        }

        /*
         * Using new builder here will not use a new connection pool etc,
         * instead it will use the base client's. So connection pool, cache etc will be shared.
         * See: https://square.github.io/okhttp/5.x/okhttp/okhttp3/-ok-http-client/index.html
         */
        OkHttpClient.Builder clientBuilder = DEFAULT_HTTP_CLIENT.newBuilder();
        if (options != null) {
            setOptions(clientBuilder, options);
        }
        if (sslSocketFactory != null && HyperSSLSocketFactory.DEFAULT_TRUST_MANAGER != null) {
            clientBuilder.sslSocketFactory(
                    sslSocketFactory,
                    HyperSSLSocketFactory.DEFAULT_TRUST_MANAGER
            );
        }
        return clientBuilder.build().newCall(requestBuilder.build()).execute();
    }

    public byte[] fetchIfModified(String fileUrl, Map<String, String> connectionHeaders) throws IOException {
        Response response = doGet(fileUrl, connectionHeaders, null, new JSONObject(), null);
        int responseCode = response.code();

        if (responseCode == HttpsURLConnection.HTTP_OK) {
            return new JuspayHttpsResponse(response).responsePayload;
        }

        return null;
    }

    @SuppressWarnings("WeakerAccess")
    public SSLSocketFactory getSslSocketFactory() {
        return sslSocketFactory;
    }

    public void setSslSocketFactory(SSLSocketFactory sslSocketFactory) {
        this.sslSocketFactory = sslSocketFactory;
    }

    public static void cancelAPICall(String tag, SdkTracker sdkTracker) {
        try {
            for (Call call : DEFAULT_HTTP_CLIENT.dispatcher().runningCalls()) {
                if (tag.equals(call.request().tag())) {
                    call.cancel();
                    sdkTracker.trackAction(LogSubCategory.ApiCall.NETWORK, LogLevel.INFO, Labels.Network.CANCEL_API, "Cancelling api call from running calls", tag);
                    return;
                }
            }

            for (Call call : DEFAULT_HTTP_CLIENT.dispatcher().queuedCalls()) {
                if (tag.equals(call.request().tag())) {
                    call.cancel();
                    sdkTracker.trackAction(LogSubCategory.ApiCall.NETWORK, LogLevel.INFO, Labels.Network.CANCEL_API, "Cancelling api call from queued calls", tag);
                    return;
                }
            }
        } catch (Exception e) {
            sdkTracker.trackAndLogException("NetUtils", LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.Network.CANCEL_API, "Exception while Cancelling API with tag " + tag, e);
            return;
        }
        sdkTracker.trackAction(LogSubCategory.ApiCall.NETWORK, LogLevel.INFO, Labels.Network.CANCEL_API, "Not able to Cancel api call from queued/running calls", tag);
    }

    private void setOptions(
            OkHttpClient.Builder clientBuilder,
            JSONObject options) {
        int connectionTimeout = options.optInt(
                "connectionTimeout",
                this.connectionTimeout
        );
        int readTimeout = options.optInt(
                "readTimeout",
                this.readTimeout
        );
        int writeTimeout = options.optInt(
                "writeTimeout",
                -1);
        boolean retryOnConnectionFailure = options.optBoolean(
                "retryOnConnectionFailure",
                true
        );
        if (connectionTimeout != -1) {
            clientBuilder.connectTimeout(connectionTimeout, TimeUnit.MILLISECONDS);
        }
        if (readTimeout != -1) {
            clientBuilder.readTimeout(readTimeout, TimeUnit.MILLISECONDS);
        }
        if (writeTimeout != -1) {
            clientBuilder.writeTimeout(writeTimeout, TimeUnit.MILLISECONDS);
        }
        clientBuilder.retryOnConnectionFailure(retryOnConnectionFailure);
    }
}
