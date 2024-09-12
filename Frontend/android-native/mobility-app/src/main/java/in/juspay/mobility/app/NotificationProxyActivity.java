package in.juspay.mobility.app;


import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.speech.tts.Voice;

import in.juspay.mobility.app.services.IncomingCallNotificationService;

public class NotificationProxyActivity extends Activity {
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        handleIntent(getIntent());
        finish();
    }

    @Override
    protected void onNewIntent(Intent intent) {
        super.onNewIntent(intent);
        handleIntent(intent);
        finish();
    }

    private void handleIntent(Intent intent) {
        final String action = intent.getAction();
        if (action != null) {
            final Intent serviceIntent =
                    (new Intent(intent)).setClass(this, IncomingCallNotificationService.class);
            final Intent appIntent =
                    getApplicationContext().getPackageManager().getLaunchIntentForPackage(getApplicationContext().getPackageName());
            if (action.equals(String.valueOf(R.string.ACTION_INCOMING_CALL)) || action.equals(String.valueOf(R.string.ACTION_ACCEPT))) {
                launchService(serviceIntent);
                launchMainActivity(appIntent);
            } else {
                launchService(serviceIntent);
            }
        }
    }

    private void launchMainActivity(Intent intent) {
        try{
            Intent launchIntent = getApplicationContext().getPackageManager().getLaunchIntentForPackage(getApplicationContext().getPackageName());
            launchIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP | Intent.FLAG_ACTIVITY_NEW_TASK);
            startActivity(launchIntent);
        }catch (Exception e){
            e.printStackTrace();
        }
    }
    private void launchService(Intent intent) {
        Intent launchIntent = new Intent(intent);
        launchIntent.setClass(this, IncomingCallNotificationService.class);
        startService(launchIntent);
    }
}

