package in.juspay.mobility.sdk.ui;

import android.content.Intent;
import android.content.IntentSender;
import android.os.Bundle;

import androidx.annotation.Nullable;

public interface IntentSenderDelegate {
    void startIntentSenderForResult(IntentSender intentSender, int requestCode, @Nullable Intent fillInIntent, int flagMask, int flagValues, int extraFlags, @Nullable Bundle options);
}
