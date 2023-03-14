package in.juspay.mobility.utils;

import androidx.annotation.RequiresApi;
import androidx.appcompat.app.AppCompatActivity;

import android.content.Intent;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.provider.Settings;
import android.widget.Toast;
import android.content.Intent;
import android.net.Uri;
import android.provider.Settings;
import android.widget.Toast;
import androidx.appcompat.app.AppCompatActivity;

public class AcceptRejectOverlaySheet extends AppCompatActivity {

    @RequiresApi(api = Build.VERSION_CODES.M)

    @Override
    protected void onResume() {
        super.onResume();


        if (Settings.canDrawOverlays(this)) {
            launchMainService();
        }
        else {
            checkDrawOverlayPermission();
        }
    }

    private void launchMainService() {
        Intent data = getIntent();
        Intent svc = new Intent(this, AcceptRejectOverlaySheetService.class);
        String caseId = data.getStringExtra("caseId");
        int notificationId = data.getIntExtra("notificationId",0);
        String shared_prefs_key = data.getStringExtra("shared_prefs_key");
        String base_env = data.getStringExtra("base_env");
//        System.out.println("TESTTT :- "+ caseId + " NF: " + notificationId + " spk: "+ shared_prefs_key + " base : "+ base_env);
        svc.putExtra("caseId",caseId);
        svc.putExtra("notificationId",notificationId);
        svc.putExtra("shared_prefs_key", shared_prefs_key);
        svc.putExtra("base_env", base_env);
        svc.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        if(caseId != null && notificationId != 0 && shared_prefs_key != null && base_env != null){
            stopService(svc);
            startService(svc);
        }
        finish();
    }

    private final static int REQUEST_CODE = 10101;

    @RequiresApi(api = Build.VERSION_CODES.M)
    private void checkDrawOverlayPermission() {
        if (!Settings.canDrawOverlays(this)) {
            Intent intent = new Intent(Settings.ACTION_MANAGE_OVERLAY_PERMISSION, Uri.parse("package:" + getPackageName()));
            startActivityForResult(intent, REQUEST_CODE);
        }
    }

    @RequiresApi(api = Build.VERSION_CODES.M)
    @Override
    protected void onActivityResult(int requestCode, int resultCode,  Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
        if (requestCode == REQUEST_CODE) {
            if (Settings.canDrawOverlays(this)) {
                launchMainService();
            }
            else {
                Toast.makeText(this, "Can't draw overlays without permission.", Toast.LENGTH_SHORT).show();
            }
        }
    }
}