package in.juspay.mobility.app.overlayMessage;

import static android.view.View.GONE;
import static android.view.View.VISIBLE;
import static in.juspay.mobility.app.Utils.dpToPx;

import android.content.Context;
import android.graphics.Typeface;
import android.media.Image;
import android.os.Build;
import android.util.Log;
import android.view.ContextThemeWrapper;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import com.google.android.flexbox.FlexboxLayout;
import com.google.android.material.button.MaterialButton;
import com.google.android.material.card.MaterialCardView;
import com.google.gson.Gson;

import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Objects;

import in.juspay.mobility.app.R;
import in.juspay.mobility.common.services.MobilityAPIResponse;
import in.juspay.mobility.common.services.MobilityCallAPI;

public class CallFeedbackView implements ViewInterface {
    private static final String TAG = "CallFeedbackView";
    private View view;
    private ServiceInterface serviceInterface;
    private Context context;
    private CreateViewBundle createViewBundle;

    private HashSet<String> selectedOptionsId;

    private static class Option {
        public String message;
        public String messageKey;
    };

    private static class CallFeedbackFCMData{
        public String callId;
        public ArrayList<Option> options;
    }

    private static class CreateViewBundle {
        public CallFeedbackFCMData entity_data;
    }


    @Override
    public View createView(String bundle, Context context, ServiceInterface serviceInterface) {
        this.serviceInterface = serviceInterface;
        this.selectedOptionsId = new HashSet<String>();
        this.context = context;
        try{
            view = LayoutInflater.from(context.getApplicationContext()).inflate(R.layout.extra_fare_demanded_overlay, null);
            Gson gson = new Gson();
            createViewBundle = gson.fromJson(bundle, CreateViewBundle.class);

            FlexboxLayout optionsListView = view.findViewById(R.id.optionsList);

            for(Option option : createViewBundle.entity_data.options){
                MaterialCardView cardView = createOptionView(option.message, option.messageKey);
                optionsListView.addView(cardView);
            }

            MaterialButton okButton = view.findViewById(R.id.button_ok);
            okButton.setOnClickListener(view -> {
                callFeedbackApi(true);
                serviceInterface.killService();
            });

            LinearLayout secondaryBtn = view.findViewById(R.id.secondary_button);
            secondaryBtn.setOnClickListener(view -> {
                callFeedbackApi(false);
                serviceInterface.killService();
            });

            return view;
        }catch (Exception e){
            Log.e(TAG, e.toString());
            return null;
        }
    }


    @Override
    public void destroyView() {

    }

    @Override
    public View getView() {
        return view;
    }


    private MaterialCardView createOptionView(String text, String tag){
        // MaterialCardView
        Context materialContext = new ContextThemeWrapper(context, R.style.MaterialTheme);
        MaterialCardView cardView = new MaterialCardView(materialContext);
        cardView.setTag(tag);
        LinearLayout.LayoutParams params = new LinearLayout.LayoutParams(
                LinearLayout.LayoutParams.WRAP_CONTENT,
                LinearLayout.LayoutParams.WRAP_CONTENT
        );
        params.setMargins(
                dpToPx(context, 12),
                dpToPx(context, 12),
                dpToPx(context, 12),
                dpToPx(context, 12)
        );
        cardView.setLayoutParams(params);
        cardView.setCardBackgroundColor(context.getResources().getColor(R.color.blue600));
        cardView.setRadius(dpToPx(context, 20));
        cardView.setCardElevation(0f);
        cardView.setContentPadding(
                dpToPx(context, 16), // left
                dpToPx(context, 12), // top
                dpToPx(context, 16), // right
                dpToPx(context, 12)  // bottom
        );

        cardView.setOnClickListener(view -> {
            String optionId = (String) view.getTag();
            ImageView tickView = cardView.findViewWithTag(optionId + "Tick");
            TextView textView = cardView.findViewWithTag(optionId + "Text");
            Log.i(TAG, "OptionId " + optionId + " button clicked");

            if(selectedOptionsId.contains(optionId)){
                Log.i(TAG, "OptionId " + optionId + " removed from set");
                selectedOptionsId.remove(optionId);

                cardView.setCardBackgroundColor(context.getResources().getColor(R.color.blue600));
                tickView.setVisibility(GONE);
                textView.setTextColor(context.getResources().getColor(R.color.dark_grayish_blue));
            }else{
                Log.i(TAG, "OptionId " + optionId + " added to set");
                selectedOptionsId.add(optionId);

                cardView.setCardBackgroundColor(context.getResources().getColor(R.color.blue800));
                tickView.setVisibility(VISIBLE);
                textView.setTextColor(context.getResources().getColor(R.color.colorWhite));
            }
        });


        // Inner LinearLayout (Horizontal)
        LinearLayout linearLayout = new LinearLayout(context);
        linearLayout.setOrientation(LinearLayout.HORIZONTAL);
        linearLayout.setLayoutParams(new LinearLayout.LayoutParams(
                LinearLayout.LayoutParams.WRAP_CONTENT,
                LinearLayout.LayoutParams.WRAP_CONTENT
        ));
        linearLayout.setGravity(Gravity.CENTER);

        // ImageView
        ImageView imageView = new ImageView(context);
        LinearLayout.LayoutParams imageParams = new LinearLayout.LayoutParams(
                dpToPx(context, 16),
                dpToPx(context, 16)
        );
        imageView.setLayoutParams(imageParams);
        imageView.setImageResource(R.drawable.ny_ic_tick_white);
        imageView.setVisibility(GONE);
        imageParams.gravity = Gravity.CENTER;
        imageView.setTag(tag + "Tick");

        // TextView
        TextView textView = new TextView(context);
        LinearLayout.LayoutParams textParams = new LinearLayout.LayoutParams(
                LinearLayout.LayoutParams.MATCH_PARENT,
                LinearLayout.LayoutParams.WRAP_CONTENT
        );
        textParams.setMargins(4, 0, 0, 0);
        textView.setLayoutParams(textParams);
        textView.setText(text);
        textView.setTextSize(14); // 7pt ≈ 10sp
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            textView.setTypeface(context.getResources().getFont(R.font.plus_jakartasans_regular),  Typeface.BOLD);
        }
        textView.setTextColor(context.getResources().getColor(R.color.dark_grayish_blue));
        textView.setTypeface(textView.getTypeface(), android.graphics.Typeface.BOLD); // simulate textFontWeight="700"
        textView.setTag(tag + "Text");

        // Add views to layout
        linearLayout.addView(imageView);
        linearLayout.addView(textView);
        cardView.addView(linearLayout);

        return cardView;
    }

    private void callFeedbackApi(boolean askedExtra){
        MobilityCallAPI mobilityCallAPI = MobilityCallAPI.getInstance(context);

        JSONObject payload = new JSONObject();
        try {
            payload.put("callId", createViewBundle.entity_data.callId);

            JSONArray optionIdsJsonArray = new JSONArray();
            if (askedExtra && selectedOptionsId != null) {
                for (Object id : selectedOptionsId) {
                    optionIdsJsonArray.put(id);
                }
            }
            payload.put("optionIds", optionIdsJsonArray);

            String baseUrl = context.getApplicationContext()
                    .getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE)
                    .getString("BASE_URL", "");

            String url = baseUrl + "driver/call/feedback";

            Log.i(TAG, "headers: " + MobilityCallAPI.getBaseHeaders(context).toString());
            Log.i(TAG, "payload: " + payload.toString());
            Log.i(TAG, "url: " + url);

            MobilityAPIResponse resp = mobilityCallAPI.callAPI(url, MobilityCallAPI.getBaseHeaders(context), payload.toString());
            Log.i(TAG, "response: " + resp.getStatusCode() + " " + resp.getResponseBody());
        } catch (Exception e) {
            Log.e(TAG, e.toString());
        }
    }

}
