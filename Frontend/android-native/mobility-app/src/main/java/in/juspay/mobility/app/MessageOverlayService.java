package in.juspay.mobility.app;

import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.graphics.PixelFormat;
import android.graphics.Typeface;
import android.os.IBinder;
import android.provider.Settings;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.WindowManager;
import android.widget.LinearLayout;
import android.widget.TextView;
import androidx.annotation.Nullable;
import androidx.core.content.res.ResourcesCompat;
import com.google.android.material.button.MaterialButton;

import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;

import in.juspay.mobility.app.ChatService;

public class MessageOverlayService extends Service implements View.OnClickListener {

    private View overlayView;
    private WindowManager windowManager;
    String LOG_TAG = "MessageOverlay";
    Context context;
    LinearLayout messageSheetHeader = null;
    TextView messageTextView = null;
    TextView TimestampView = null;
    TextView HeaderTextView = null;
    MaterialButton suggestion1View = null;
    MaterialButton suggestion2View = null;
    MaterialButton suggestion3View = null;
    Suggestions suggestions = null;
    private static final ArrayList<SendMessageCallBack> sendMessageCallBacks = new ArrayList<>();

    public interface SendMessageCallBack {
        void sendMessage(String message);
    }

    public static void registerSendMessageCallBack(SendMessageCallBack callBack) {
        sendMessageCallBacks.add(callBack);
    }

    public static void deRegisterSendMessageCallBack(SendMessageCallBack callBack) {
        sendMessageCallBacks.remove(callBack);
    }

    @Override
    public void onCreate() {
        context = getApplicationContext();
        super.onCreate();
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        try {
            String message = intent.getStringExtra("message");
            String timeStamp = intent.getStringExtra("timestamp");
            showMessageOverlay(message,timeStamp);
        } catch (Exception e) {
            Log.e("MessageOverlayService", "Error in onStartCommand : " + e);
        }
        return super.onStartCommand(intent, flags, startId);
    }

    private void showMessageOverlay(String message, String timestamp) {
        try {
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
                HeaderTextView = overlayView.findViewById(R.id.HeaderTextView);
                messageTextView.setText(message);
                TimestampView.setText(timestamp);
                HeaderTextView.setText(R.string.message_from_customer);
                if (overlayView != null && !overlayView.isAttachedToWindow()) {
                    windowManager.addView(overlayView, widgetLayoutParams);
                }
                messageTextView.setMaxWidth((int) (width * 0.7));
                overlayView.setVisibility(View.VISIBLE);
            }

            if (messageSheetHeader != null) {
                messageSheetHeader.setOnClickListener(this);
            }

            suggestions = getDefaultSuggestions();

            SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
            String language = sharedPref.getString("LANGUAGE_KEY", "null");

            Typeface typeface;
            if(language.equals("KN_IN")) {
                typeface = Typeface.createFromAsset(context.getAssets(), "fonts/NotoSansKannada-SemiBold.ttf");
                HeaderTextView.setTypeface(typeface);
            } else {
                typeface = ResourcesCompat.getFont(context, R.font.plus_jakartasans_semibold);
            }

            if(suggestions != null) {
                String suggestion1 = getSuggestionFromKey(suggestions.s1, language);
                String suggestion2 = getSuggestionFromKey(suggestions.s2, language);
                String suggestion3 = getSuggestionFromKey(suggestions.s3, language);
                if(suggestion1.equals("")) {
                    suggestion1 = getFallBackSuggestion(suggestions.s1, language);
                }
                if(suggestion2.equals("")) {
                    suggestion2 = getFallBackSuggestion(suggestions.s2, language);
                }
                if(suggestion3.equals("")) {
                    suggestion3 = getFallBackSuggestion(suggestions.s3, language);
                }
                if (suggestion1View != null && !suggestion1.equals("")) {
                    suggestion1View.setText(suggestion1);
                    suggestion1View.setTypeface(typeface);
                    suggestion1View.setVisibility(View.VISIBLE);
                    suggestion1View.setOnClickListener(this);
                }
                if (suggestion2View != null && !suggestion2.equals("")) {
                    suggestion2View.setText(suggestion2);
                    suggestion2View.setTypeface(typeface);
                    suggestion2View.setVisibility(View.VISIBLE);
                    suggestion2View.setOnClickListener(this);
                }
                if (suggestion3View != null && !suggestion3.equals("")) {
                    suggestion3View.setText(suggestion3);
                    suggestion3View.setTypeface(typeface);
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
        if ( !sharedPref.getString(context.getResources().getString(R.string.REGISTERATION_TOKEN), "null").equals("null") && (sharedPref.getString(context.getResources().getString(R.string.ACTIVITY_STATUS), "null").equals("onPause") || sharedPref.getString(context.getResources().getString(R.string.ACTIVITY_STATUS), "null").equals("onDestroy"))) {
            Intent intent = context.getPackageManager().getLaunchIntentForPackage(context.getPackageName());
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
        int id = view.getId();
        if (id == R.id.message_sheet_header) {
            startMainActivity();
        } else if (id == R.id.suggestion1) {
            if (suggestions != null) for (SendMessageCallBack cb : sendMessageCallBacks)
                sendMessage(cb, suggestions.s1);
        } else if (id == R.id.suggestion2) {
            if (suggestions != null) for (SendMessageCallBack cb : sendMessageCallBacks)
                sendMessage(cb, suggestions.s2);
        } else if (id == R.id.suggestion3) {
            if (suggestions != null) for (SendMessageCallBack cb : sendMessageCallBacks)
                sendMessage(cb,suggestions.s3);
        }
        overlayView.setVisibility(View.GONE);
    }

    private void sendMessage(SendMessageCallBack cb, String message){
        Thread thread = new Thread(() -> {
            try {
                cb.sendMessage(message);
            } catch (Exception e) {
                Log.e(LOG_TAG, "Error in sending a message" + e);
            }
        });
        thread.start();
    }

    public Suggestions getDefaultSuggestions() {
        try {
            SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
            String suggestionsStr = sharedPref.getString("SUGGESTIONS", "null");
            String isDriverAtPickup = sharedPref.getString("IS_DRIVER_AT_PICKUP", "null");
            JSONArray suggestionsArr = new JSONArray();
            if(isDriverAtPickup.equals("true")) {
                suggestionsArr = new JSONObject(suggestionsStr).getJSONArray("driverOverlayDefaultAP");
            } else {
                suggestionsArr = new JSONObject(suggestionsStr).getJSONArray("driverOverlayDefaultBP");
            }
            return new Suggestions(suggestionsArr.getString(0) , suggestionsArr.getString(1), suggestionsArr.getString(2));
        }catch (Exception e) {
            Log.e("MessageOverlay", "Error in getDefaultSuggestions " + e);
            return getFallBackSuggestions(context);
        }
    }

    private String getSuggestionFromKey (String key, String language) {
        try {
            SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
            String suggestionsStr = sharedPref.getString("SUGGESTIONS_DEFINITIONS", "null");
            JSONObject suggestions = new JSONObject(suggestionsStr);
            if(suggestions.has(key)) {
                JSONObject message = suggestions.getJSONObject(key);
                if(message.has(language.toLowerCase())) return message.getString(language.toLowerCase());
                else return message.getString("en_us");
            } else {
                return key;
            }
        } catch (Exception e) {
            Log.e("MessageOverlay","Error in getMessageFromKey : " + e);
            return "";
        }
    }

    private static Suggestions getFallBackSuggestions (Context context) {
        try {
            SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
            String isDriverAtPickup = sharedPref.getString("IS_DRIVER_AT_PICKUP", "null");
            String language = sharedPref.getString("LANGUAGE_KEY", "null");
            if(isDriverAtPickup.equals("true")) {
                return new Suggestions(getFallBackSuggestion("dols1AP", language), getFallBackSuggestion("dols2AP", language), getFallBackSuggestion("dols3AP", language));
            } else {
                return new Suggestions(getFallBackSuggestion("dols1BP", language), getFallBackSuggestion("dols2BP", language), getFallBackSuggestion("dols3BP", language));
            }
        } catch (Exception e) {
            Log.e("MessageOverlay", "Error in getFallBackSuggestions " + e);
            return null;
        }
    }

    private static String getFallBackSuggestion (String key, String language) {
        try {
            JSONObject suggestions = defaultSuggestions();
            JSONObject suggestion = suggestions.getJSONObject(key);
            if(suggestion.has(language.toLowerCase())) return suggestion.getString(language.toLowerCase());
            else return suggestion.getString("en_us");
        } catch (Exception e) {
            Log.e("MessageOverlay", "Error in getFallBackSuggestions " + e);
            return "";
        }
    }

    private static JSONObject defaultSuggestions () {
        try {
            JSONObject dols1BP = new JSONObject("{\"ta_in\":\"சரி\",\"ml_in\":\"ഓക്കേ\",\"kn_in\":\"ಸರಿ\",\"hi_in\":\"ठीक है\",\"en_us\":\"Ok\",\"bn_in\":\"ঠিক আছে\"}");
            JSONObject dols2BP = new JSONObject("{\"en_us\":\"Yes\",\"ta_in\":\"ஆம்\",\"kn_in\":\"ಹೌದು\",\"hi_in\":\"हाँ\",\"ml_in\":\"അതെ\",\"bn_in\":\"হ্যাঁ\"}");
            JSONObject dols3BP = new JSONObject("{\"en_us\":\"On my way\",\"ta_in\":\"வந்துகொண்டிருக்கிறேன்\",\"kn_in\":\"ಬರುತ್ತಿದ್ದೇನೆ\",\"hi_in\":\"मैं आ रहा हूँ \",\"ml_in\":\"ഞാൻ വന്നുകൊണ്ടിരിക്കുകയാണ്\",\"bn_in\":\"আমি আসছি\"}");
            JSONObject dols1AP = new JSONObject("{\"en_us\":\"Yes\",\"ta_in\":\"ஆம்\",\"kn_in\":\"ಹೌದು\",\"hi_in\":\"हाँ\",\"ml_in\":\"അതെ\",\"bn_in\":\"হ্যাঁ\"}");
            JSONObject dols2AP = new JSONObject("{\"ta_in\":\"சரி\",\"ml_in\":\"ഓക്കേ\",\"kn_in\":\"ಸರಿ\",\"hi_in\":\"ठीक है\",\"en_us\":\"Ok\",\"bn_in\":\"ঠিক আছে\"}");
            JSONObject dols3AP = new JSONObject("{\"en_us\" : \"At pick-up\", \"ta_in\" : \"பிக்-அப்பில் உள்ளேன்\", \"kn_in\" : \"ಪಿಕ್ ಅಪ್ ನಲ್ಲಿ \", \"hi_in\" : \"मैं लोकेशन पे हूँ\", \"ml_in\" : \"ഞാൻ പിക്ക്-അപ്പിൽ ആണ്\", \"bn_in\" : \"আমি পিক-আপ স্থানে আছি\" }");
            JSONObject suggestions = new JSONObject();
            suggestions.put("dols1BP", dols1BP);
            suggestions.put("dols2BP", dols2BP);
            suggestions.put("dols3BP", dols3BP);
            suggestions.put("dols1AP", dols1AP);
            suggestions.put("dols2AP", dols2AP);
            suggestions.put("dols3AP", dols3AP);
            return suggestions;
        } catch (Exception e){
            Log.e("MessageOverlay", "Error in defaultSuggestions " + e);
            return null;
        }
    }

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    @Override
    public void onDestroy() {
        if(overlayView != null) {
            overlayView.setVisibility(View.GONE);
        }
        super.onDestroy();
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
