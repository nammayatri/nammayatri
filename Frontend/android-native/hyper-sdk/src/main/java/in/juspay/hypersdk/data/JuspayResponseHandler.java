package in.juspay.hypersdk.data;

import android.os.Bundle;

import androidx.annotation.Keep;

@Keep
public interface JuspayResponseHandler extends Runnable {
    void onResponse(String response);

    void onResponse(Bundle bundle);

    @Keep
    void onError(String error);
}
