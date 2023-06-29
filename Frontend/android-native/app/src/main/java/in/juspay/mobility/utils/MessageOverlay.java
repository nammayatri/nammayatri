package in.juspay.mobility.utils;

import static android.content.Context.WINDOW_SERVICE;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.graphics.PixelFormat;
import android.provider.Settings;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.WindowManager;
import android.view.textclassifier.ConversationAction;
import android.widget.LinearLayout;
import android.widget.TextView;
import com.google.android.material.button.MaterialButton;

import org.json.JSONArray;
import org.json.JSONObject;
import in.juspay.mobility.MainActivity;
import in.juspay.mobility.R;

public class MessageOverlay implements View.OnClickListener {

    private View overlayView;
    private WindowManager windowManager;
    String LOG_TAG = "MessageOverlay";
    Context context;
    LinearLayout messageSheetHeader = null;
    TextView messageTextView = null;
    TextView TimestampView = null;
    MaterialButton suggestion1View = null;
    MaterialButton suggestion2View = null;
    MaterialButton suggestion3View = null;
    Suggestions suggestions = null;


    public void showMessageOverlay(String message, String timestamp, Context ctx) {
        try {
            context = ctx;
            if (!Settings.canDrawOverlays(context)) return;
            int LAYOUT_FLAG;
            if (android.os.Build.VERSION.SDK_INT >= android.os.Build.VERSION_CODES.O) {
                LAYOUT_FLAG = WindowManager.LayoutParams.TYPE_APPLICATION_OVERLAY;
            } else {
                LAYOUT_FLAG = WindowManager.LayoutParams.TYPE_PHONE;
            }
            windowManager = (WindowManager) context.getSystemService(WINDOW_SERVICE);
            if (overlayView == null) {
                overlayView = LayoutInflater.from(context).inflate(R.layout.message_sheet, null);
            }
            WindowManager.LayoutParams widgetLayoutParams = new WindowManager.LayoutParams(WindowManager.LayoutParams.MATCH_PARENT, WindowManager.LayoutParams.MATCH_PARENT, LAYOUT_FLAG, WindowManager.LayoutParams.FLAG_NOT_FOCUSABLE, PixelFormat.TRANSLUCENT);

            if (message != null && timestamp != null && windowManager != null) {
                DisplayMetrics displaymetrics = new DisplayMetrics();
                windowManager.getDefaultDisplay().getMetrics(displaymetrics);
                int width = displaymetrics.widthPixels;
                messageSheetHeader = overlayView.findViewById(R.id.message_sheet_header);
                messageTextView = overlayView.findViewById(R.id.messageView);
                TimestampView = overlayView.findViewById(R.id.messageTimeView);
                suggestion1View = overlayView.findViewById(R.id.suggestion1);
                suggestion2View = overlayView.findViewById(R.id.suggestion2);
                suggestion3View = overlayView.findViewById(R.id.suggestion3);
                messageTextView.setText(message);
                TimestampView.setText(timestamp);
                if (overlayView != null && !overlayView.isAttachedToWindow()) {
                    windowManager.addView(overlayView, widgetLayoutParams);
                }
                messageTextView.setMaxWidth((int) (width * 0.7));
                overlayView.setVisibility(View.VISIBLE);
            }

            if (messageSheetHeader != null) {
                messageSheetHeader.setOnClickListener(this);
            }

            suggestions = getDefaultSuggestions(context);

            if(suggestions != null) {
                if (suggestion1View != null) {
                    suggestion1View.setText(ChatService.getMessageFromKey(suggestions.s1));
                    suggestion1View.setVisibility(View.VISIBLE);
                    suggestion1View.setOnClickListener(this);
                }
                if (suggestion2View != null) {
                    suggestion2View.setText(ChatService.getMessageFromKey(suggestions.s2));
                    suggestion2View.setVisibility(View.VISIBLE);
                    suggestion2View.setOnClickListener(this);
                }
                if (suggestion3View != null) {
                    suggestion3View.setText(ChatService.getMessageFromKey(suggestions.s3));
                    suggestion3View.setVisibility(View.VISIBLE);
                    suggestion3View.setOnClickListener(this);
                }
            }
        } catch (Exception e) {
            Log.e(LOG_TAG, "Error in showMessageOverlay " + e);
        }
    }

    private void startMainActivity() {
        SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        if (context.getResources().getString(R.string.service).equals(context.getString(R.string.nammayatripartner)) && !sharedPref.getString(context.getResources().getString(R.string.REGISTERATION_TOKEN), "null").equals("null") && (sharedPref.getString(context.getResources().getString(R.string.ACTIVITY_STATUS), "null").equals("onPause") || sharedPref.getString(context.getResources().getString(R.string.ACTIVITY_STATUS), "null").equals("onDestroy"))) {
            Intent intent = new Intent(context, MainActivity.class);
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_REORDER_TO_FRONT);
            try {
                context.startActivity(intent);
            } catch (Exception e) {
                Log.e(LOG_TAG, "failed to start MainActivity" + e);
            }
        }
    }

    @Override
    public void onClick(View view) {
        switch (view.getId()) {
            case R.id.message_sheet_header:
                startMainActivity();
                break;
            case R.id.suggestion1:
                if(suggestions != null) ChatService.sendMessage(suggestions.s1);
                break;
            case R.id.suggestion2:
                if(suggestions != null) ChatService.sendMessage(suggestions.s2);
                break;
            case R.id.suggestion3:
                if(suggestions != null) ChatService.sendMessage(suggestions.s3);
                break;
        }
        overlayView.setVisibility(View.GONE);
    }

    public static Suggestions getDefaultSuggestions(Context context) {
        try {
            SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
            String suggestionsStr = sharedPref.getString("SUGGESTIONS", "null");
            JSONArray suggestionsArr = new JSONObject(suggestionsStr).getJSONArray("driverOverlayDefault");
            return new Suggestions(suggestionsArr.getString(0), suggestionsArr.getString(1), suggestionsArr.getString(2));
        }catch (Exception e) {
            Log.e("MessageOverlay", "Error in getDefaultSuggestions " + e);
            return null;
        }
    }
}

class Suggestions {
    String s1;
    String s2;
    String s3;
    public Suggestions(String s1, String s2, String s3) {
        this.s1 = s1;
        this.s2 = s2;
        this.s3 = s3;
    }
}
