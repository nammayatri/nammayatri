package in.juspay.mobility.sdk.utils.network;

import androidx.annotation.NonNull;

import org.json.JSONObject;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.Map;
import java.util.zip.GZIPInputStream;

import javax.net.ssl.HttpsURLConnection;

import okhttp3.Response;

/**
 * Simple wrapper to hold information sent by the connection.
 * Once the response is read, the connection is closed.
 *
 * @author Sriduth
 */
public class JuspayHttpsResponse {
    public final int responseCode;
    public final Map<String, List<String>> headers;
    public final byte[] responsePayload;

    private static final String LOG_TAG = "JuspayHttpsResponse";

    public JuspayHttpsResponse(int responseCode, @NonNull byte[] responsePayload,
                               Map<String, List<String>> headers) {
        this.responseCode = responseCode;
        this.responsePayload = responsePayload;
        this.headers = headers;
    }

    private static byte[] decodeGZip(InputStream in) throws IOException {
        try (GZIPInputStream zin = new GZIPInputStream(in)) {
            try (ByteArrayOutputStream out = new ByteArrayOutputStream()) {
                byte[] bytes = new byte[1024];
                int numRead;
                while ((numRead = zin.read(bytes)) >= 0) {
                    out.write(bytes, 0, numRead);
                }
                byte[] decoded = out.toByteArray();
                out.flush();

                return decoded;
            }
        }
    }

    @SuppressWarnings("WeakerAccess")
    public JuspayHttpsResponse(HttpsURLConnection connection) throws IOException {
        this.responseCode = connection.getResponseCode();
        this.headers = connection.getHeaderFields();

        InputStream is;
        if ((responseCode >= 200 && responseCode < 300) || responseCode == 302) {
            is = connection.getInputStream();
        } else {
            is = connection.getErrorStream();
        }

        String hce = connection.getContentEncoding();
        if (hce != null && hce.equals("gzip")) {
            this.responsePayload = decodeGZip(is);
        } else {
            try (BufferedInputStream in = new BufferedInputStream(is)) {
                try (ByteArrayOutputStream out = new ByteArrayOutputStream()) {
                    byte[] buffer = new byte[1024];
                    int numRead;
                    while ((numRead = in.read(buffer, 0, 1024)) != -1) {
                        out.write(buffer, 0, numRead);
                    }
                    this.responsePayload = out.toByteArray();
                    out.flush();
                }
            }
        }
    }

    public JuspayHttpsResponse(Response response) throws IOException {
        responseCode = response.code();
        headers = response.headers().toMultimap();
        if (response.body() != null) {
            String contentEncoding = response.header("content-encoding");
            if (contentEncoding != null && contentEncoding.equalsIgnoreCase("gzip")) {
                responsePayload = decodeGZip(response.body().byteStream());
            } else {
                responsePayload = response.body().bytes();
            }
        } else {
            responsePayload = null;
        }
        if (response.body() != null) {
            response.body().close();
        }
    }

    @Override
    public String toString() {
        JSONObject object = new JSONObject();

        try {
            object.put("responseCode", responseCode);
            object.put("responsePayload", responsePayload);
            object.put("headers", headers);
        } catch (Exception ignored) {
        }

        return object.toString();
    }


}
