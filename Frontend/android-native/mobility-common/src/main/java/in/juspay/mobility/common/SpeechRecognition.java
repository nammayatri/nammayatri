package in.juspay.mobility.common;

import android.Manifest;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.speech.RecognitionListener;
import android.speech.RecognizerIntent;
import android.speech.SpeechRecognizer;
import android.util.Log;
import android.view.View;
import android.widget.ImageView;

import androidx.core.app.ActivityCompat;
import androidx.core.content.ContextCompat;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Locale;

import in.juspay.hyper.core.BridgeComponents;

public class SpeechRecognition {
    private static final int REQUEST_MIC_PERMISSION = 1;
    protected String LOG_TAG = SpeechRecognition.class.getSimpleName();
    private View haloView = null;
    private BridgeComponents bridgeComponents = null;
    private SpeechRecognizer speechRecognizer = null;

    private boolean isListening;

    public SpeechRecognition(BridgeComponents bridgeComponents) {
        this.bridgeComponents = bridgeComponents;
        this.speechRecognizer = SpeechRecognizer.createSpeechRecognizer(bridgeComponents.getContext());
        if (bridgeComponents.getActivity() != null) {
            haloView = bridgeComponents.getActivity().findViewById(R.id.halo_view);
        }
        isListening = false;
    }


    public Intent setIntent() {
        Intent intent = new Intent(RecognizerIntent.ACTION_RECOGNIZE_SPEECH);
        intent.putExtra(RecognizerIntent.EXTRA_LANGUAGE_MODEL, RecognizerIntent.LANGUAGE_MODEL_FREE_FORM);
        intent.putExtra(RecognizerIntent.EXTRA_PARTIAL_RESULTS, true);
        intent.putExtra(RecognizerIntent.EXTRA_MAX_RESULTS, 101);
        intent.putExtra(RecognizerIntent.EXTRA_LANGUAGE, Locale.getDefault());
        return intent;
    }


    public void setupSpeechRecognitionView(String viewId) {
        SpeechRecognitionView speechRecognitionView = new SpeechRecognitionView(bridgeComponents.getContext(), bridgeComponents.getActivity());
        try {
            speechRecognitionView.inflateView(Integer.parseInt(viewId));
            if (bridgeComponents.getActivity() != null) {
                haloView = bridgeComponents.getActivity().findViewById(R.id.halo_view);
                ImageView imageIcon = bridgeComponents.getActivity().findViewById(R.id.mic_icon_button);
                imageIcon.setOnClickListener(new View.OnClickListener() {
                    @Override
                    public void onClick(View v) {
                        new Handler(Looper.getMainLooper()).post(() -> {
                            if (speechRecognizer != null) {
                                startListening(setIntent());
                            }
                        });
                    }
                });
            }
        } catch (IOException e) {
            e.printStackTrace();
            ;
        }
    }

    public void stopListening() {
        Log.i(LOG_TAG, "Speech recognition stopped");
        isListening = false;
        if (speechRecognizer != null) {
            speechRecognizer.stopListening();
            speechRecognizer.cancel();
            speechRecognizer.destroy();
            speechRecognizer = null;
        }
    }

    public void setupSpeechRecognition(String callback) {
        this.speechRecognizer.setRecognitionListener(new RecognitionListener() {
            @Override
            public void onReadyForSpeech(Bundle params) {
                Log.i(LOG_TAG, "onReadyForSpeech triggered");
                ImageView micIcon = bridgeComponents.getActivity().findViewById(R.id.mic_icon_button);
                if(micIcon != null) {
                    micIcon.setImageResource(R.drawable.ny_ic_mic_icon_red);
                }
                String js = String.format(Locale.ENGLISH, "window.callUICallback('%s', '%s', '%s');", callback, "INIT", "");
                bridgeComponents.getJsCallback().addJsToWebView(js);
            }

            @Override
            public void onBeginningOfSpeech() {
                Log.i(LOG_TAG, "onBeginningOfSpeech triggered");
                haloView.setVisibility(View.VISIBLE);
            }

            @Override
            public void onRmsChanged(float rmsdB) {
                if (haloView.getVisibility() != View.VISIBLE) {
                    haloView.setVisibility(View.VISIBLE);
                }

                float minScale = 0.7f;
                float maxScale = 8.0f;

                float scale = minScale + (rmsdB / 6f);
                scale = Math.max(minScale, Math.min(scale, maxScale));

                haloView.animate()
                        .scaleX(scale)
                        .scaleY(scale)
                        .setDuration(20)
                        .start();
            }

            @Override
            public void onBufferReceived(byte[] buffer) {
            }

            @Override
            public void onEndOfSpeech() {
                Log.i(LOG_TAG, "onEndOfSpeech triggered, stopping recognition");
                if (haloView != null) {
                    haloView.animate()
                            .scaleX(1f)
                            .scaleY(1f)
                            .setDuration(300)
                            .start();
                    haloView.setVisibility(View.INVISIBLE);
                }
            }

            @Override
            public void onError(int error) {
                isListening = false;
                String errorMessage;
                switch (error) {
                    case SpeechRecognizer.ERROR_AUDIO:
                        errorMessage = "Audio recording error";
                        break;
                    case SpeechRecognizer.ERROR_CLIENT:
                        errorMessage = "Client-side error";
                        break;
                    case SpeechRecognizer.ERROR_INSUFFICIENT_PERMISSIONS:
                        errorMessage = "Insufficient permissions";
                        break;
                    case SpeechRecognizer.ERROR_NETWORK:
                        errorMessage = "Network error";
                        break;
                    case SpeechRecognizer.ERROR_NETWORK_TIMEOUT:
                        errorMessage = "Network timeout";
                        break;
                    case SpeechRecognizer.ERROR_NO_MATCH:
                        errorMessage = "No match found";
                        break;
                    case SpeechRecognizer.ERROR_RECOGNIZER_BUSY:
                        errorMessage = "RecognitionService is busy";
                        break;
                    case SpeechRecognizer.ERROR_SERVER:
                        errorMessage = "Server error";
                        break;
                    case SpeechRecognizer.ERROR_SPEECH_TIMEOUT:
                        errorMessage = "No speech input";
                        break;
                    default:
                        errorMessage = "Unknown error";
                        break;
                }
                Log.e(LOG_TAG, "Speech recognition error: " + errorMessage);
                ImageView micIcon = bridgeComponents.getActivity().findViewById(R.id.mic_icon_button);
                if(micIcon != null) {
                    micIcon.setImageResource(R.drawable.ny_ic_mic_icon_blue);
                }
                haloView.setVisibility(View.INVISIBLE);
                speechRecognizer.cancel();
                String js = String.format(Locale.ENGLISH, "window.callUICallback('%s', '%s', '%s');", callback, "FAILED", "");
                bridgeComponents.getJsCallback().addJsToWebView(js);
            }

            @Override
            public void onResults(Bundle results) {
                ArrayList<String> matches = results.getStringArrayList(SpeechRecognizer.RESULTS_RECOGNITION);
                isListening = false;
                if (matches != null && !matches.isEmpty()) {
                    String match = matches.get(0);
                    Log.i(LOG_TAG, "Voice Results: " + match);
                    String js = String.format(Locale.ENGLISH, "window.callUICallback('%s', '%s', '%s');", callback, "SUCCESS", match);
                    bridgeComponents.getJsCallback().addJsToWebView(js);
                }
            }

            @Override
            public void onPartialResults(Bundle partialResults) {
                ArrayList<String> matches = partialResults.getStringArrayList(SpeechRecognizer.RESULTS_RECOGNITION);
                if (matches != null && !matches.isEmpty()) {
                    String match = matches.get(0);
                    Log.i(LOG_TAG, "partialResults" + matches.toString());
                    String js = String.format(Locale.ENGLISH, "window.callUICallback('%s', '%s', '%s');", callback, "SUCCESS", match);
                    bridgeComponents.getJsCallback().addJsToWebView(js);
                }
            }

            @Override
            public void onEvent(int eventType, Bundle params) {
            }
        });

    }

    public void startListening(Intent intent) {
        if(this.isListening) return;
        this.isListening = true;
        this.speechRecognizer.startListening(intent);
    }

    public void requestPermissions() {
        Context context = bridgeComponents.getContext();
        if (ContextCompat.checkSelfPermission(context, Manifest.permission.RECORD_AUDIO) != PackageManager.PERMISSION_GRANTED) {
            if (bridgeComponents.getActivity() != null) {
                ActivityCompat.requestPermissions(bridgeComponents.getActivity(), new String[]{Manifest.permission.RECORD_AUDIO}, REQUEST_MIC_PERMISSION);
            }
        }
    }

}
