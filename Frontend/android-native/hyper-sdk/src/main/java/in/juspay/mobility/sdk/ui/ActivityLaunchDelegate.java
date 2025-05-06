package in.juspay.mobility.sdk.ui;

import android.content.Intent;
import android.os.Bundle;

import androidx.annotation.Nullable;

public interface ActivityLaunchDelegate {

    void startActivityForResult(Intent intent, int requestCode, @Nullable Bundle bundle);
}
