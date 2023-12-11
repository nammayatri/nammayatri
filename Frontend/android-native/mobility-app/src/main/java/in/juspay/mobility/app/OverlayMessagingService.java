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
import android.graphics.Paint;
import android.graphics.PixelFormat;
import android.graphics.Typeface;
import android.net.Uri;
import android.os.Build;
import android.os.Handler;
import android.os.IBinder;
import android.os.Looper;
import android.provider.Settings;
import android.text.Html;
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
import org.json.JSONException;
import org.json.JSONObject;

import java.util.Iterator;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import in.juspay.mobility.app.services.MobilityAPIResponse;
import in.juspay.mobility.app.services.MobilityCallAPI;

public class OverlayMessagingService extends Service {
    private WindowManager windowManager;
    private View messageView;
    private String link, endPoint, method;
    private JSONArray actions;
    private JSONArray secondaryActions;
    private String reqBody;
    private String toastMessage;
    private String supportPhoneNumber;

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
        messageView = LayoutInflater.from(getApplicationContext()).inflate(R.layout.message_overlay, null);

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
            ImageView cancelButtonImage = messageView.findViewById(R.id.cancel_button_image);
            TextView buttonCancelRight = messageView.findViewById(R.id.button_cancel_right);
            title.setText(data.has("title") ? data.getString("title") : "");
            buttonOk.setText(data.has("okButtonText") ? data.getString("okButtonText") : "Cancel");
            buttonCancel.setText(data.has("cancelButtonText") ? data.getString("cancelButtonText") : "Ok");
            description.setText(data.has("description") ? data.getString("description") : "");
            link = data.has("link") ? data.getString("link") : null;
            endPoint = data.has("endPoint") ? data.getString("endPoint") : null;
            method = data.has("method") ? data.getString("method") : null;
            reqBody = data.has("reqBody") ?  data.getString("reqBody") : null;
            actions = data.has("actions") ? data.getJSONArray("actions") : null;
            secondaryActions = data.has("secondaryActions") ? data.getJSONArray("secondaryActions") : null;
            toastMessage = data.optString("toastMessage", null);
            supportPhoneNumber = data.optString("contactSupportNumber", null);
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
            try {
                setDataToMediaView(data);
            } catch (Exception e){
                RideRequestUtils.firebaseLogEventWithParams("exception", "CONSTRUCT_MEDIA_VIEW", Objects.requireNonNull(e.getMessage()).substring(0, 40), this);
            }
            Boolean showContactSupport = supportPhoneNumber != null;
            if (showContactSupport) {
                buttonCancelRight.setVisibility(View.VISIBLE);
                cancelButtonImage.setVisibility(View.VISIBLE);
                secondaryActions = secondaryActions != null ? secondaryActions.put("CALL_SUPPORT") : new JSONArray("CALL_SUPPORT");
                buttonCancel.setText(R.string.need_help);
            }

        } catch (Exception e) {
            RideRequestUtils.firebaseLogEventWithParams("exception", "CONSTRUCT_VIEW", Objects.requireNonNull(e.getMessage()).substring(0, 40), this);
            stopSelf();
        }
    }

    public void setDataToMediaView (JSONObject data) throws JSONException {
        LinearLayout dynamicView = messageView.findViewById(R.id.dynamic_views);
        dynamicView.removeAllViews();
        JSONArray mediaViews = data.has("socialMediaLinks") ? data.getJSONArray("socialMediaLinks") : null;
        if(mediaViews != null){
            for (int i = 0; i < mediaViews.length(); i++){
                LinearLayout mediaView = new LinearLayout(this);
                JSONObject mediaViewData = mediaViews.getJSONObject(i);
                int imageHeight = mediaViewData.has("height") ? mediaViewData.getInt("height") : 0;
                int imageWidth = mediaViewData.has("width") ? mediaViewData.getInt("height") : 0;
                if(mediaViewData.has("prefixImage")) {
                    ImageView prefixImage = new ImageView(this);
                    Glide.with(this).load(mediaViewData.getString("prefixImage")).into(prefixImage);
                    prefixImage.setLayoutParams(new LinearLayout.LayoutParams(imageWidth, imageHeight));
                    boolean prefixImageVisibility = mediaViewData.has("prefixImage") && mediaViewData.getString("prefixImage").length() != 0;
                    prefixImage.setVisibility(prefixImageVisibility ? View.VISIBLE : View.GONE);
                    mediaView.addView(prefixImage);
                }

                TextView linkText = new TextView(this);
                linkText.setText(mediaViewData.has("linkText") ? mediaViewData.getString("linkText") : "");
                linkText.setTextColor(mediaViewData.has("linkTextColor") ? mediaViewData.getInt("linkTextColor") :  getResources().getColor(R.color.blue800, null)); //ask theme
                boolean isTextUnderLined = !mediaViewData.has("isTextUnderlined") || mediaViewData.getBoolean("isTextUnderlined");
                if (isTextUnderLined)
                    linkText.setPaintFlags(linkText.getPaintFlags() | Paint.UNDERLINE_TEXT_FLAG);
                boolean linkTextVisibility = mediaViewData.has("linkText") && mediaViewData.getString("linkText").length() != 0;
                linkText.setVisibility(linkTextVisibility ? View.VISIBLE : View.GONE);
                linkText.setPadding(10,0,10,0);
                mediaView.addView(linkText);

                if(mediaViewData.has("suffixImage")) {
                    ImageView suffixImage = new ImageView(this);
                    Glide.with(this).load(mediaViewData.getString("suffixImage")).into(suffixImage);
                    suffixImage.setLayoutParams(new LinearLayout.LayoutParams(imageWidth, imageHeight));
                    boolean suffixImageVisibility = mediaViewData.has("suffixImage") && mediaViewData.getString("suffixImage").length() != 0;
                    suffixImage.setVisibility(suffixImageVisibility ? View.VISIBLE : View.GONE);
                    mediaView.addView(suffixImage);
                }

                mediaView.setGravity(Gravity.CENTER);
                String mediaLink = mediaViewData.has("link") ? mediaViewData.getString("link") : null;
                if(mediaLink != null){
                    mediaView.setOnClickListener(view -> {
                        openLink(mediaLink);
                        stopSelf();
                    });
                }

                dynamicView.addView(mediaView);
            }
        }
    }

    private void setViewListeners(View messageView) {
        messageView.findViewById(R.id.dismiss_message).setOnClickListener(view -> stopSelf());
        messageView.findViewById(R.id.button_ok).setOnClickListener(view -> {
            try {
                for (int i = 0; i < actions.length(); i++) {
                    String action = String.valueOf(actions.get(i));
                    performAction(action);
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
            stopSelf();
        });

        messageView.findViewById(R.id.secondary_button).setOnClickListener(view -> {
            try {
                for (int i = 0; i < secondaryActions.length(); i++) {
                    String action = String.valueOf(secondaryActions.get(i));
                    performAction(action);
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
            stopSelf();
        });
    }

    void performAction(String action){
        switch (action) {
            case "SET_DRIVER_ONLINE":
                RideRequestUtils.updateDriverStatus(true, "ONLINE", this, false);
                RideRequestUtils.restartLocationService(this);
                break;
            case "OPEN_LINK":
                openLink(link);
                break;
            case "CALL_API":
                try{
                    if (endPoint != null && reqBody != null && method != null) {

                        new Thread(new Runnable() {
                            @Override
                            public void run() {
                                MobilityCallAPI mobilityApiHandler = new MobilityCallAPI();
                                Map<String, String> baseHeaders = mobilityApiHandler.getBaseHeaders(OverlayMessagingService.this);
                                MobilityAPIResponse apiResponse = mobilityApiHandler.callAPI(endPoint, baseHeaders, reqBody, method);
                                Handler handler = new Handler(Looper.getMainLooper());
                                handler.post(new Runnable() {
                                    @Override
                                    public void run() {
                                        if (toastMessage != null && apiResponse.getStatusCode() == 200)
                                            Toast.makeText(OverlayMessagingService.this, toastMessage, Toast.LENGTH_SHORT).show();
                                    }
                                });
                            }


                        }).start();
                    }
                } catch (Exception e){
                    System.out.println("Error : " + e);
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
            case "CALL_SUPPORT" :
                try {
                    intent = new Intent(Intent.ACTION_DIAL);
                    intent.setData(Uri.parse("tel:" + supportPhoneNumber));
                    intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                    startActivity(intent);
                } catch (Exception e){
                    System.out.println(e.toString());
                }

                break;
        }
        stopSelf();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        if (windowManager != null && messageView != null) {
            windowManager.removeView(messageView);
            messageView = null;
        }
    }

    void openLink (String link) {
        if (link != null) {
            Uri uri = Uri.parse(link); // missing 'http://' will cause crash
            Intent intent = new Intent(Intent.ACTION_VIEW, uri);
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            try {
                startActivity(intent);
            } catch (ActivityNotFoundException e) {
                String message = this.getResources().getString(Utils.getResIdentifier(getApplicationContext(), "no_enabled_browser", "string"));
                Toast.makeText(this, message, Toast.LENGTH_LONG).show();
                RideRequestUtils.firebaseLogEventWithParams("exception", "OPEN_LINK", "ActivityNotFoundException", this);
            } catch (Exception e) {
                RideRequestUtils.firebaseLogEventWithParams("exception", "OPEN_LINK", Objects.requireNonNull(e.getMessage()).substring(0, 40), this);
            }
        }
    }
}
