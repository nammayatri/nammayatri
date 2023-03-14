package in.juspay.beckn.utils;

import androidx.annotation.Nullable;
import androidx.annotation.RequiresApi;
import androidx.appcompat.app.AppCompatActivity;

import android.content.DialogInterface;
import android.content.Intent;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.provider.Settings;
import android.util.Log;

import com.google.android.material.dialog.MaterialAlertDialogBuilder;

import in.juspay.beckn.R;


public class CheckPermissionOverlay extends AppCompatActivity {
    private int REQUEST_CODE = 1, twoChecks =2;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        checkPermission();
    }

    private void checkPermission(){

        try {
            Intent intent = new Intent(Settings.ACTION_MANAGE_OVERLAY_PERMISSION);
            Uri uri = Uri.fromParts("package", getPackageName(), null);
            intent.setData(uri);
            intent.setFlags(Intent.FLAG_ACTIVITY_NO_HISTORY);
            startActivityForResult(intent,REQUEST_CODE);

        } catch (Exception e) {
            //Log.e(LOG_TAG, "Exception in request permission", e);
        }


    }

    @RequiresApi(api = Build.VERSION_CODES.M)
    @Override
    protected void onActivityResult(int requestCode, int resultCode, @Nullable Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
        if (requestCode == REQUEST_CODE) {
            if (!Settings.canDrawOverlays(this)) {
                finish();
                // checkPermission();
            }
            else
            {
                finish();
            }

        }
    }
}