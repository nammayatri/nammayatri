package in.juspay.mobility.common;

import android.app.Activity;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.LinearLayout;

import java.io.IOException;

public class SpeechRecognitionView extends LinearLayout {

    protected String LOG_TAG = SpeechRecognitionView.class.getSimpleName();
    protected int layout = R.layout.speech_recognition_view;
    private final Activity activity;

    public SpeechRecognitionView(Context context, Activity activity) {
        super(context);
        this.activity = activity;
        LayoutInflater.from(context).inflate(layout, this);
    }

    public void inflateView(int id) throws IOException {
        if (activity != null) {
            LinearLayout linearLayout = activity.findViewById(id);
            if (linearLayout != null) {
                linearLayout.addView(this);
            }
        }
    }
}
