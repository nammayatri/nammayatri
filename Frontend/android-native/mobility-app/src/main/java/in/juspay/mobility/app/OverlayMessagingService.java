/*
 * Copyright 2022-23, Juspay India Pvt Ltd
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 * as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 * is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 * the GNU Affero General Public License along with this program. If not, see https://www.gnu.org/licenses/.
 */

/* Required Payload template
"message" : {
    "token": "<DEVICE_TOKEN>",
    "android": {
        "data": {
        "notification_type": "DRIVER_NOTIFY",
        "show_notification": "true",
        "entity_type": "Case",
        "entity_ids": "",
        "notification_json": "{
            \"title\": \"Demo message\",
            \"body\": \"Body of demo message\",
            \"icon\": \"<ICON_URL>\",
            \"tag\": \"DRIVER_NOTIFY\",
            \"sound\": \"default\",
            \"channel_id\": \"General\"
        }",
            "driver_notification_payload": "{
                \"title\" : \"This is a demo message\",
                \"description\" :\"This is the description of demo message\",
                \"imageUrl\" : \"<IMAGE_URL>\",
                \"okButtonText\" : \"Ok\",
                \"cancelButtonText\" : \"Cancel\",
                \"actions\" : [\"SET_DRIVER_ONLINE\", \"OPEN_APP\", \"OPEN_LINK\"],
                \"link\" : \"http://www.google.com\",
                \"endPoint\": \"<EP>\",
                \"method\": \"POST\",
                \"reqBody\" : {},
                \"titleVisibility\" : true,
                \"descriptionVisibility\" : true,
                \"buttonOkVisibility\" : true,
                \"buttonCancelVisibility\" : true,
                \"buttonLayoutVisibility\" : true,
                \"imageVisibility\" : true
            }"
        }
    }
    }
}
 */


package in.juspay.mobility.app;

import android.annotation.SuppressLint;
import android.app.Service;
import android.content.ActivityNotFoundException;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.graphics.PixelFormat;
import android.net.Uri;
import android.os.IBinder;
import android.provider.Settings;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.WindowManager;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.Nullable;

import com.bumptech.glide.Glide;
import com.google.android.material.button.MaterialButton;

import org.json.JSONArray;
import org.json.JSONObject;

import java.util.Objects;

public class OverlayMessagingService extends Service {
    private WindowManager windowManager;
    private View messageView;
    private String link, endPoint, method;
    private JSONArray actions;
    private JSONObject reqBody;

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    @Override
    public void onCreate() {
        if (!Settings.canDrawOverlays(this)) return;
        super.onCreate();

    }

    @SuppressLint("InflateParams")
    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        String intentMessage = intent != null && intent.hasExtra("payload") ? intent.getStringExtra("payload") : null;
        if (!Settings.canDrawOverlays(this) || intentMessage == null) return START_STICKY;
        int layoutParamsType = android.os.Build.VERSION.SDK_INT >= android.os.Build.VERSION_CODES.O ? WindowManager.LayoutParams.TYPE_APPLICATION_OVERLAY : WindowManager.LayoutParams.TYPE_PHONE;
        if (windowManager != null) {
            setDataToViews(intentMessage);
            return START_STICKY;
        }
        windowManager = (WindowManager) getSystemService(WINDOW_SERVICE);
        WindowManager.LayoutParams params = new WindowManager.LayoutParams(
                WindowManager.LayoutParams.MATCH_PARENT,
                WindowManager.LayoutParams.MATCH_PARENT,
                layoutParamsType,
                WindowManager.LayoutParams.FLAG_TURN_SCREEN_ON | WindowManager.LayoutParams.FLAG_DIM_BEHIND,
                PixelFormat.TRANSPARENT);
        params.dimAmount = 0.6f;
        params.gravity = Gravity.CENTER;
        params.screenOrientation = ActivityInfo.SCREEN_ORIENTATION_PORTRAIT;
        messageView = LayoutInflater.from(this).inflate(R.layout.message_overlay, null);
        setViewListeners(messageView);
        windowManager.addView(messageView, params);
        setDataToViews(intentMessage);
        return START_STICKY;
    }

    private void setDataToViews(String bundle) {
        try {
            JSONObject data = new JSONObject(bundle);
            TextView title = messageView.findViewById(R.id.title);
            TextView description = messageView.findViewById(R.id.description);
            ImageView imageView = messageView.findViewById(R.id.image);
            LinearLayout buttonLayout = messageView.findViewById(R.id.button_view);
            MaterialButton buttonOk = messageView.findViewById(R.id.button_ok);
            TextView buttonCancel = messageView.findViewById(R.id.button_cancel);
            title.setText(data.has("title") ? data.getString("title") : "");
            buttonOk.setText(data.has("okButtonText") ? data.getString("okButtonText") : "Cancel");
            buttonCancel.setText(data.has("cancelButtonText") ? data.getString("cancelButtonText") : "Ok");
            description.setText(data.has("description") ? data.getString("description") : "");
            link = data.has("link") ? data.getString("link") : null;
            endPoint = data.has("endPoint") ? data.getString("endPoint") : null;
            method = data.has("method") ? data.getString("method") : null;
            reqBody = data.has("reqBody") ? (JSONObject) data.get("reqBody") : null;
            actions = data.has("actions") ? data.getJSONArray("actions") : null;
            Glide.with(this).load(data.getString("imageUrl")).into(imageView);
            boolean titleVisibility = data.has("titleVisibility") && data.getBoolean("titleVisibility");
            boolean descriptionVisibility = data.has("descriptionVisibility") && data.getBoolean("descriptionVisibility");
            boolean buttonOkVisibility = data.has("buttonOkVisibility") && data.getBoolean("buttonOkVisibility");
            boolean buttonCancelVisibility = data.has("buttonCancelVisibility") && data.getBoolean("buttonCancelVisibility");
            boolean buttonLayoutVisibility = data.has("buttonLayoutVisibility") && data.getBoolean("buttonLayoutVisibility");
            boolean imageVisibility = data.has("imageVisibility") && data.getBoolean("imageVisibility");
            title.setVisibility(titleVisibility ? View.VISIBLE : View.GONE);
            description.setVisibility(descriptionVisibility ? View.VISIBLE : View.GONE);
            buttonLayout.setVisibility(buttonLayoutVisibility ? View.VISIBLE : View.GONE);
            buttonOk.setVisibility(buttonOkVisibility ? View.VISIBLE : View.GONE);
            buttonCancel.setVisibility(buttonCancelVisibility ? View.VISIBLE : View.GONE);
            imageView.setVisibility(imageVisibility ? View.VISIBLE : View.GONE);
        } catch (Exception e) {
            stopSelf();
        }
    }

    private void setViewListeners(View messageView) {
        messageView.findViewById(R.id.dismiss_message).setOnClickListener(view -> stopSelf());
        messageView.findViewById(R.id.button_ok).setOnClickListener(view -> {
            try {
                for (int i = 0; i < actions.length(); i++) {
                    String action = String.valueOf(actions.get(i));
                    switch (action) {
                        case "SET_DRIVER_ONLINE":
                            RideRequestUtils.updateDriverStatus(true, "ONLINE", this, false);
                            RideRequestUtils.restartLocationService(this);
                            break;
                        case "OPEN_LINK":
                            if (link != null) {
                                Uri uri = Uri.parse(link); // missing 'http://' will cause crash
                                Intent intent = new Intent(Intent.ACTION_VIEW, uri);
                                intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                                try {
                                    startActivity(intent);
                                } catch (ActivityNotFoundException e) {
                                    String message = this.getResources().getString(Utils.getResIdentifier(getApplicationContext(),"no_enabled_browser", "string"));
                                    Toast.makeText(this, message, Toast.LENGTH_LONG).show();
                                    RideRequestUtils.firebaseLogEventWithParams("exception", "OPEN_LINK", "ActivityNotFoundException", this);
                                } catch (Exception e) {
                                    RideRequestUtils.firebaseLogEventWithParams("exception", "OPEN_LINK", Objects.requireNonNull(e.getMessage()).substring(0, 40), this);
                                }
                            }
                            break;
                        case "CALL_API":
                            if (endPoint != null && reqBody != null && method != null) {
                                RideRequestUtils.callAPIViaFCM(endPoint, reqBody, method, this);
                            }
                            break;
                        case "OPEN_APP":
                            RideRequestUtils.openApplication(this);
                            break;
                        case "OPEN_SUBSCRIPTION" :
                            Intent intent = getApplicationContext().getPackageManager().getLaunchIntentForPackage(getApplicationContext().getPackageName());
                            intent.putExtra("notification_type", "PAYMENT_MODE_MANUAL");
                            intent.putExtra("entity_ids", "");
                            intent.putExtra("entity_type", "");
                            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                            startActivity(intent);
                            Utils.logEvent("ny_driver_overlay_join_now", getApplicationContext());
                            break;
                    }
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
            stopSelf();
        });

        messageView.findViewById(R.id.button_cancel).setOnClickListener(view -> stopSelf());
    }


    @Override
    public void onDestroy() {
        super.onDestroy();
        if (messageView != null) {
            windowManager.removeView(messageView);
            messageView = null;
        }
    }
}
