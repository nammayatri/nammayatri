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
import android.view.ViewGroup;
import android.view.WindowManager;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;
import androidx.annotation.Nullable;
import androidx.core.content.res.ResourcesCompat;
import com.google.android.material.button.MaterialButton;

import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;

import in.juspay.mobility.app.ChatService;
import okhttp3.internal.http2.Header;

public class MessageOverlayService extends Service implements View.OnClickListener {

    private View overlayView;
    private View stopDetectedOverlayView;
    private WindowManager windowManager;
    String LOG_TAG = "MessageOverlay";
    Context context;
    LinearLayout messageSheetHeader = null;
    TextView messageTextView = null;
    TextView TimestampView = null;
    TextView headerTextView = null;
    TextView bodyTextView = null;
    LinearLayout stopDetectedSuggestion1View = null;
    LinearLayout stopDetectedSuggestion2View = null;
    LinearLayout stopDetectedSuggestion3View = null;
    TextView stopSuggestion1TextView = null;
    TextView stopSuggestion2TextView = null;
    TextView stopSuggestion3TextView = null;
    TextView quickChatsTextView = null;
    ImageView stopSuggestion1ImageView = null;
    ImageView stopSuggestion2ImageView = null;
    ImageView stopSuggestion3ImageView = null;
    ImageView headerSuffixImage = null;
    TextView suggestion1SentTextView = null;
    TextView suggestion2SentTextView = null;
    TextView suggestion3SentTextView = null;
    MaterialButton gotItButtonView = null;
    MaterialButton suggestion1View = null;
    MaterialButton suggestion2View = null;
    MaterialButton suggestion3View = null;
    Suggestions suggestions = null;
    boolean stopDetectedOverlay = false;
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
            stopDetectedOverlay = intent.getBooleanExtra("isStopDetected", false);
            if (stopDetectedOverlay) {
                showStopDetectedOverlay (timeStamp);
            } else {
                showMessageOverlay(message,timeStamp);
            }
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
                headerTextView = overlayView.findViewById(R.id.HeaderTextView);
                messageTextView.setText(message);
                TimestampView.setText(timestamp);
                headerTextView.setText(R.string.message_from_customer);
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
                headerTextView.setTypeface(typeface);
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
    private void showStopDetectedOverlay(String timestamp) {
        try {
            if (!Settings.canDrawOverlays(context)) return;
            int LAYOUT_FLAG;
            if (android.os.Build.VERSION.SDK_INT >= android.os.Build.VERSION_CODES.O) {
                LAYOUT_FLAG = WindowManager.LayoutParams.TYPE_APPLICATION_OVERLAY;
            } else {
                LAYOUT_FLAG = WindowManager.LayoutParams.TYPE_PHONE;
            }
            windowManager = (WindowManager) context.getSystemService(WINDOW_SERVICE);
            if (stopDetectedOverlayView == null) {
                stopDetectedOverlayView = LayoutInflater.from(context).inflate(R.layout.stop_detected_message_sheet, null);
            }
            WindowManager.LayoutParams widgetLayoutParams = new WindowManager.LayoutParams(WindowManager.LayoutParams.MATCH_PARENT, WindowManager.LayoutParams.MATCH_PARENT, LAYOUT_FLAG, WindowManager.LayoutParams.FLAG_NOT_FOCUSABLE, PixelFormat.TRANSLUCENT);
            if (timestamp != null && timestamp != null && windowManager != null) {
                DisplayMetrics displaymetrics = new DisplayMetrics();
                windowManager.getDefaultDisplay().getMetrics(displaymetrics);
                int width = displaymetrics.widthPixels;
                messageSheetHeader = stopDetectedOverlayView.findViewById(R.id.stop_detected_message_sheet_header);
                stopDetectedSuggestion1View = stopDetectedOverlayView.findViewById(R.id.suggestion1);
                stopDetectedSuggestion2View = stopDetectedOverlayView.findViewById(R.id.suggestion2);
                stopDetectedSuggestion3View = stopDetectedOverlayView.findViewById(R.id.suggestion3);
                stopSuggestion1TextView = stopDetectedOverlayView.findViewById(R.id.suggestion1_text);
                stopSuggestion2TextView = stopDetectedOverlayView.findViewById(R.id.suggestion2_text);
                stopSuggestion3TextView = stopDetectedOverlayView.findViewById(R.id.suggestion3_text);
                stopSuggestion1ImageView = stopDetectedOverlayView.findViewById(R.id.suggestion1_image);
                stopSuggestion2ImageView = stopDetectedOverlayView.findViewById(R.id.suggestion2_image);
                stopSuggestion3ImageView = stopDetectedOverlayView.findViewById(R.id.suggestion3_image);
                quickChatsTextView = stopDetectedOverlayView.findViewById(R.id.quick_chats);
                headerTextView = stopDetectedOverlayView.findViewById(R.id.HeaderTextView);
                bodyTextView = stopDetectedOverlayView.findViewById(R.id.BodyTextView);
                gotItButtonView = stopDetectedOverlayView.findViewById(R.id.got_it_button);
                suggestion1SentTextView = stopDetectedOverlayView.findViewById(R.id.suggestion1_sent_text);
                suggestion2SentTextView = stopDetectedOverlayView.findViewById(R.id.suggestion2_sent_text);
                suggestion3SentTextView = stopDetectedOverlayView.findViewById(R.id.suggestion3_sent_text);
                headerSuffixImage = stopDetectedOverlayView.findViewById(R.id.header_suffix_image);
                if (headerSuffixImage != null) headerSuffixImage.setOnClickListener(this);
                headerTextView.setText(R.string.customer_is_waiting);
                bodyTextView.setText(R.string.please_start_moving_towards_the_pickup_location);

                if(gotItButtonView != null) gotItButtonView.setVisibility(View.GONE);
                if (stopDetectedOverlayView != null && !stopDetectedOverlayView.isAttachedToWindow()) {
                    windowManager.addView(stopDetectedOverlayView, widgetLayoutParams);
                }
                if (quickChatsTextView != null) {
                    quickChatsTextView.setText(R.string.quick_chats);
                }
                stopDetectedOverlayView.setVisibility(View.VISIBLE);
            }

            suggestions = getDefaultSuggestions();
            if (suggestions == null) {
                suggestions = new Suggestions("driverStopDetectedOverlay1BP", "driverStopDetectedOverlay2BP", "driverStopDetectedOverlay3BP");
            }

            SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
            String language = sharedPref.getString("LANGUAGE_KEY", "null");

            Typeface typeface;
            if(language.equals("KN_IN")) {
                typeface = Typeface.createFromAsset(context.getAssets(), "fonts/NotoSansKannada-SemiBold.ttf");
                headerTextView.setTypeface(typeface);
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
                if (stopDetectedSuggestion1View != null && stopSuggestion1TextView != null && stopSuggestion1ImageView != null && suggestion1SentTextView != null && !suggestion1.equals("")) {
                    setStopSuggestionImageAndText(stopDetectedSuggestion1View, stopSuggestion1TextView, stopSuggestion1ImageView, suggestion1, typeface, suggestion1SentTextView);
                    setImageDimensions(stopSuggestion1ImageView, 64, 64);
                }
                if (stopDetectedSuggestion2View != null && stopSuggestion2TextView != null && stopSuggestion2ImageView != null && suggestion2SentTextView != null && !suggestion2.equals("")) {
                    setStopSuggestionImageAndText(stopDetectedSuggestion2View, stopSuggestion2TextView, stopSuggestion2ImageView, suggestion2, typeface, suggestion2SentTextView);
                    setImageDimensions(stopSuggestion2ImageView, 64, 64);
                }
                if (stopDetectedSuggestion3View != null && stopSuggestion3TextView != null && stopSuggestion3ImageView != null && suggestion3SentTextView != null && !suggestion3.equals("")) {
                    setStopSuggestionImageAndText(stopDetectedSuggestion3View, stopSuggestion3TextView, stopSuggestion3ImageView, suggestion3, typeface, suggestion3SentTextView);
                    setImageDimensions(stopSuggestion3ImageView, 64, 64);
                }
            }
        } catch (Exception e) {
            Log.e(LOG_TAG, "Error in showMessageOverlay " + e);
        }
    }

    private void setStopSuggestionImageAndText(LinearLayout parentView, TextView textView, ImageView imageView, String text, Typeface typeface, TextView sentTextView) {
        textView.setText(text);
        imageView.setImageDrawable(getDrawable(R.drawable.ic_send_blue));
        sentTextView.setVisibility(View.GONE);
        textView.setTypeface(typeface);
        parentView.setVisibility(View.VISIBLE);
        parentView.setOnClickListener(this);
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
        } else if (id == R.id.header_suffix_image) stopDetectedOverlayView.setVisibility((View.GONE));
        if (stopDetectedOverlay) {
            stopDetectedOverlay = false;
            sendMessageViewForStopsOverlay(id);
        } else {
            if (overlayView != null) overlayView.setVisibility(View.GONE);
            if (stopDetectedOverlayView != null) stopDetectedOverlayView.setVisibility((View.GONE));
        }
    }

    private void sendMessageViewForStopsOverlay(int id) {
        stopDetectedSuggestion1View.setVisibility(View.GONE);
        stopDetectedSuggestion2View.setVisibility(View.GONE);
        stopDetectedSuggestion3View.setVisibility(View.GONE);
        stopDetectedSuggestion1View.setClickable(false);
        stopDetectedSuggestion2View.setClickable(false);
        stopDetectedSuggestion3View.setClickable(false);
        if (gotItButtonView != null) {
            gotItButtonView.setText(R.string.okay_got_it_smallcase);
            gotItButtonView.setVisibility(View.VISIBLE);
            gotItButtonView.setOnClickListener(this);
        }
        if (id == R.id.suggestion1 && stopSuggestion1ImageView != null && suggestion1SentTextView != null) {
            setSentSuggestionImageAndText(stopDetectedSuggestion1View, R.drawable.ny_ic_check_green , stopSuggestion1ImageView, suggestion1SentTextView);
        } else if (id == R.id.suggestion2 && stopSuggestion2ImageView != null && suggestion2SentTextView != null) {
            setSentSuggestionImageAndText(stopDetectedSuggestion2View, R.drawable.ny_ic_check_green , stopSuggestion2ImageView, suggestion2SentTextView);
        } else if (id == R.id.suggestion3 && stopSuggestion3ImageView != null && suggestion3SentTextView != null) {
            setSentSuggestionImageAndText(stopDetectedSuggestion3View, R.drawable.ny_ic_check_green , stopSuggestion3ImageView, suggestion3SentTextView);
        }
    }
    private void setSentSuggestionImageAndText(LinearLayout parent, int drawableImage, ImageView imageView, TextView sentTextView) {
        parent.setVisibility(View.VISIBLE);
        sentTextView.setVisibility(View.VISIBLE);
        sentTextView.setText(R.string.sent);
        imageView.setImageDrawable(getDrawable(drawableImage));
        setImageDimensions(imageView, 50, 50);
    }
    private void setImageDimensions(ImageView imageView, int height, int width) {
        ViewGroup.LayoutParams params = imageView.getLayoutParams();
        params.width = width;
        params.height = height;
        imageView.setLayoutParams(params);
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
            if (stopDetectedOverlay) {
                suggestionsArr = new JSONObject(suggestionsStr).getJSONArray("driverStopDetectedOverlayDefault");
            } else if(isDriverAtPickup.equals("true")) {
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

    private Suggestions getFallBackSuggestions (Context context) {
        try {
            SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
            String isDriverAtPickup = sharedPref.getString("IS_DRIVER_AT_PICKUP", "null");
            String language = sharedPref.getString("LANGUAGE_KEY", "null");
            if (stopDetectedOverlay) {
                return new Suggestions(getFallBackSuggestion("driverStopDetectedOverlay1BP", language), getFallBackSuggestion("driverStopDetectedOverlay2BP", language), getFallBackSuggestion("driverStopDetectedOverlay3BP", language));
            } else if(isDriverAtPickup.equals("true")) {
                return new Suggestions(getFallBackSuggestion("dols1AP", language), getFallBackSuggestion("dols2AP", language), getFallBackSuggestion("dols3AP", language));
            } else {
                return new Suggestions(getFallBackSuggestion("dols1BP", language), getFallBackSuggestion("dols2BP", language), getFallBackSuggestion("dols3BP", language));
            }
        } catch (Exception e) {
            Log.e("MessageOverlay", "Error in getFallBackSuggestions " + e);
            return null;
        }
    }

    private String getFallBackSuggestion (String key, String language) {
        try {
            JSONObject suggestions = new JSONObject();
            if (stopDetectedOverlay) {
                suggestions = defaultStopSuggestions();
            } else {
                suggestions = defaultSuggestions();
            }
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
            JSONObject dols1BP = new JSONObject("{\"ta_in\":\"சரி\",\"ml_in\":\"ഓക്കേ\",\"kn_in\":\"ಸರಿ\",\"hi_in\":\"ठीक है\",\"en_us\":\"Ok\",\"bn_in\":\"ঠিক আছে\",\"te_in\":\"సరే\"}");
            JSONObject dols2BP = new JSONObject("{\"en_us\":\"Yes\",\"ta_in\":\"ஆம்\",\"kn_in\":\"ಹೌದು\",\"hi_in\":\"हाँ\",\"ml_in\":\"അതെ\",\"bn_in\":\"হ্যাঁ\",\"te_in\":\"అవును\"}");
            JSONObject dols3BP = new JSONObject("{\"en_us\":\"5 mins\",\"ta_in\":\"5 mins\",\"kn_in\":\"5 mins\",\"hi_in\":\"5 mins\",\"ml_in\":\"5 mins\",\"bn_in\":\"5 mins\",\"te_in\":\"5 mins\"}");
            JSONObject dols1AP = new JSONObject("{\"en_us\":\"Yes\",\"ta_in\":\"ஆம்\",\"kn_in\":\"ಹೌದು\",\"hi_in\":\"हाँ\",\"ml_in\":\"അതെ\",\"bn_in\":\"হ্যাঁ\",\"te_in\":\"అవును\"}");
            JSONObject dols2AP = new JSONObject("{\"ta_in\":\"சரி\",\"ml_in\":\"ഓക്കേ\",\"kn_in\":\"ಸರಿ\",\"hi_in\":\"ठीक है\",\"en_us\":\"Ok\",\"bn_in\":\"ঠিক আছে\",\"te_in\":\"సరే\"}");
            JSONObject dols3AP = new JSONObject("{\"en_us\":\"5 mins\",\"ta_in\":\"5 mins\",\"kn_in\":\"5 mins\",\"hi_in\":\"5 mins\",\"ml_in\":\"5 mins\",\"bn_in\":\"5 mins\",\"te_in\":\"5 mins\"}");
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

    private static JSONObject defaultStopSuggestions () {
        try {
            JSONObject suggestions = new JSONObject();
            suggestions.put("driverStopDetectedOverlay1BP", new JSONObject()
                    .put("en_us", "On my way, hold tight!")
                    .put("ta_in", "வந்துகொண்டு இருக்கிறேன், சற்று காத்திருங்கள்!")
                    .put("kn_in", "ಸರಿ ಬರುತ್ತಿದ್ದೇನೆ, ಸ್ವಲ್ಪ ಕಾಯಿರಿ")
                    .put("hi_in", "रास्ते में हूँ, थोड़ा इंतजार करें!")
                    .put("ml_in", "ഞാൻ വരുന്നു, കുറച്ച് സമയം കാത്തിരിക്കൂ!")
                    .put("bn_in", "রাস্তায় আছি, একটু অপেক্ষা করুন!")
                    .put("te_in", "మార్గ మధ్యంలో ఉన్నాను,కాసేపు ఆగండి")
            );
            suggestions.put("driverStopDetectedOverlay2BP", new JSONObject()
                    .put("en_us", "Traffic ahead, few mins delay")
                    .put("ta_in", "போக்குவரத்து நெரிசலாக உள்ளது, சில நிமிடங்கள் தாமதமாகும்")
                    .put("kn_in", "ಟ್ರಾಫಿಕ್ ಇದೆ, ಸ್ವಲ್ಪ ಸಮಯ ತಡೆಯಿರಿ")
                    .put("hi_in", "आगे ट्रैफिक है, कुछ मिनट देरी होगी")
                    .put("ml_in", "ട്രാഫിക് ഉണ്ട്, കുറച്ച് നേരം വൈകും")
                    .put("bn_in", "সামনের দিকে ট্রাফিক আছে, কয়েক মিনিট দেরি")
                    .put("te_in", "ముందు ట్రాఫిక్ ఉంది, కొద్దిసేపు ఆలస్యం అవుతుంది.")
            );
            suggestions.put("driverStopDetectedOverlay3BP", new JSONObject()
                    .put("en_us", "Struggling with location, one sec!")
                    .put("ta_in", "இடத்தை கண்டுபிடிக்க சிரமமாக உள்ளது, ஒரு நொடி!")
                    .put("kn_in", "ಸ್ಥಳ ಸಿಗುವುದರಲ್ಲಿ ತೊಂದರೆ, ಒಂದು ಕ್ಷಣ!")
                    .put("hi_in", "स्थान ढूंढने में मुश्किल हो रही है, एक सेकंड!")
                    .put("ml_in", "ലൊക്കേഷൻ കണ്ടെത്താൻ ബുദ്ധിമുട്ടുന്നു, ഒരു സെക്കന്റ്!")
                    .put("bn_in", "লোকেশন খুঁজতে অসুবিধা হচ্ছে, এক সেকেন্ড!")
                    .put("te_in", "లొకేషన్ కనుగొనుటలో ఇబ్బందిఉంది, ఒక సెకండ్ ఆగండి!")
            );
            return suggestions;
        } catch (Exception e){
            Log.e("MessageOverlay", "Error in defaultStopSuggestions " + e);
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
        if(stopDetectedOverlayView != null) {
            stopDetectedOverlayView.setVisibility(View.GONE);
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
