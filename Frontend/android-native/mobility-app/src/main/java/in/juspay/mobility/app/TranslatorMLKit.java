package in.juspay.mobility.app;

import static android.content.Context.MODE_PRIVATE;
import static java.util.concurrent.TimeUnit.MILLISECONDS;
import static java.util.concurrent.TimeUnit.SECONDS;

import static in.juspay.hyper.core.JuspayCoreLib.getApplicationContext;
import static in.juspay.mobility.app.RideRequestUtils.firebaseLogEventWithParams;

import android.app.Activity;
import android.content.Context;
import android.os.Bundle;
import android.util.Log;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import  androidx.appcompat.app.AppCompatActivity;
import com.google.android.gms.tasks.OnFailureListener;
import com.google.android.gms.tasks.OnSuccessListener;
import com.google.android.gms.tasks.Task;
import com.google.android.gms.tasks.Tasks;
import com.google.mlkit.common.MlKitException;
import com.google.mlkit.common.model.DownloadConditions;
import com.google.mlkit.common.model.RemoteModelManager;
import com.google.mlkit.nl.translate.TranslateLanguage;
import com.google.mlkit.nl.translate.TranslateRemoteModel;
import com.google.mlkit.nl.translate.Translation;
import com.google.mlkit.nl.translate.Translator;
import com.google.mlkit.nl.translate.TranslatorOptions;



import java.util.Locale;
import java.util.concurrent.CountDownLatch;

import in.juspay.hyper.core.BridgeComponents;
import java.util.*;

import kotlin.jvm.Synchronized;


public class TranslatorMLKit {



    private static Translator translator;

    private Context context;

    final RemoteModelManager remoteModelManager = RemoteModelManager.getInstance();

    public TranslatorMLKit(String initialLanguage, String finalLanguage, Context context, String lastLang){

        Log.d(LOG_TAG, "old");                      // remove
//        downloadedModels(remoteModelManager);            // remove
        Log.d(LOG_TAG, "lastLang = " + lastLang);   // remove
        removeModel(remoteModelManager, lastLang);
        translator = downloadModel(initialLanguage, finalLanguage);
        this.context = context;
    }


    public static String result, LOG_TAG = "TranslatorMLKit";
    public Translator downloadModel(String initialLanguage, String finalLanguage) {

        TranslatorOptions options =
                new TranslatorOptions.Builder()
                        .setSourceLanguage(initialLanguage)
                        .setTargetLanguage(finalLanguage.substring(0, 2).toLowerCase())
                        .build();
        final Translator translator =
                Translation.getClient(options);

        translator.downloadModelIfNeeded()
                .addOnSuccessListener(
                        v -> {
                            Log.d(LOG_TAG, "download successful " + finalLanguage);
                            firebaseLogEventWithParams("download_translation_model", "download_successful", "successful_translation", context);
                            downloadedModels(remoteModelManager);       // remove
                        })
                .addOnFailureListener(
                        e -> {
                            Log.d(LOG_TAG, "download failed");
                            firebaseLogEventWithParams("download_translation_model", "download_failed", e.toString(), context);
                        });

        return translator;

    }


    public void removeModel(RemoteModelManager modelManager, String language) {

        TranslateRemoteModel previousModel = new TranslateRemoteModel.Builder(language.substring(0, 2).toLowerCase()).build();
        modelManager.deleteDownloadedModel(previousModel)
                .addOnSuccessListener((OnSuccessListener) v -> {
                    Log.d(LOG_TAG, "model deleted" + language);
                })
                .addOnFailureListener(e -> {
                    Log.d(LOG_TAG, "model deletion failed " + e);
                });

    }

    public void downloadedModels(RemoteModelManager modelManager){
        modelManager.getDownloadedModels(TranslateRemoteModel.class)
                .addOnSuccessListener(new OnSuccessListener() {
                    @Override
                    public void onSuccess(Object models) {

                        Log.d(LOG_TAG, "models = " + models.toString());
                    }
                })
                .addOnFailureListener(new OnFailureListener() {
                        @Override
                        public void onFailure(@NonNull Exception e) {
                            // Error.
                        }
                    });
    }


    public static void translateStringInTextView(String stringToTranslate, TextView textView){

            translator.translate(stringToTranslate)
                    .addOnSuccessListener(
                            translatedText -> {

                                Animation fadeInAnimation = AnimationUtils.loadAnimation(textView.getContext(), R.anim.fadein);

                                textView.startAnimation(fadeInAnimation);
                                textView.setHeight(textView.getHeight());
                                textView.setText(translatedText);

                            })
                    .addOnFailureListener(
                            e -> {
                                Log.d(LOG_TAG, "translation failed");
                            });

    }

    public static void translateStringWithCallback(String initialAddress, String callback, BridgeComponents bridgeComponents){

        translator.translate(initialAddress)
                .addOnSuccessListener(
                        translatedText -> {

                            if (callback != null) {
                                String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                                        callback, translatedText);
                                bridgeComponents.getJsCallback().addJsToWebView(javascript);
                            }
                        })
                .addOnFailureListener(
                        e -> {
                            if (callback != null) {
                                String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                                        callback, initialAddress);
                                bridgeComponents.getJsCallback().addJsToWebView(javascript);
                            }
                        });
    }

}
