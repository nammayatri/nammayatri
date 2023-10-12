/*
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.app;


import static in.juspay.mobility.app.RideRequestUtils.firebaseLogEventWithParams;

import android.content.Context;
import android.util.Log;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;
import android.widget.TextView;
import com.google.android.gms.tasks.OnSuccessListener;
import com.google.mlkit.common.model.RemoteModelManager;
import com.google.mlkit.nl.translate.TranslateRemoteModel;
import com.google.mlkit.nl.translate.Translation;
import com.google.mlkit.nl.translate.Translator;
import com.google.mlkit.nl.translate.TranslatorOptions;
import java.util.Locale;
import in.juspay.hyper.core.BridgeComponents;


public class TranslatorMLKit {
    private static Translator translator;

    private Context context;

    final RemoteModelManager remoteModelManager = RemoteModelManager.getInstance();

    public TranslatorMLKit(String initialLanguage, String finalLanguage, Context context){
        translator = downloadModel(initialLanguage, finalLanguage);
        this.context = context;
    }


    public static String result, LOG_TAG = "Mobility Translator";
    public Translator downloadModel(String initialLanguage, String finalLanguage) {
        if (finalLanguage == null || finalLanguage.equals("null")) {
            finalLanguage = "en";
        }
        TranslatorOptions options =
                new TranslatorOptions.Builder()
                        .setSourceLanguage(initialLanguage)
                        .setTargetLanguage(finalLanguage.substring(0, 2).toLowerCase())
                        .build();
        final Translator scopedTranslator =
                Translation.getClient(options);
        try{
            translator.downloadModelIfNeeded()
                    .addOnSuccessListener(
                            v -> {
                                firebaseLogEventWithParams("download_translation_model", "download_successful", "successful_translation", context);
                            })
                    .addOnFailureListener(
                            e -> {
                                Log.d(LOG_TAG, "download failed");
                                firebaseLogEventWithParams("download_translation_model", "download_failed", e.toString(), context);
                            });
        }catch (Exception exception){
            exception.printStackTrace();
            firebaseLogEventWithParams("download_translation_model_ml", "download_failed", exception.toString(), context);
        }
        return scopedTranslator;
    }


//    public void removeModel(RemoteModelManager modelManager, String language) { //TODO:: Manage this properly in next update
//
//        TranslateRemoteModel previousModel = new TranslateRemoteModel.Builder(language.substring(0, 2).toLowerCase()).build();
//        if (modelManager == null) {
//            modelManager = RemoteModelManager.getInstance();
//        }
//        modelManager.deleteDownloadedModel(previousModel)
//                .addOnSuccessListener((OnSuccessListener) v -> {
//                    Log.d(LOG_TAG, "model deleted" + language);
//                })
//                .addOnFailureListener(e -> {
//                    Log.d(LOG_TAG, "model deletion failed " + e);
//                });
//
//    }

    public void downloadedModels(RemoteModelManager modelManager){
        if(modelManager!=null){
            modelManager
                    .getDownloadedModels(TranslateRemoteModel.class)
                    .addOnSuccessListener(
                            remoteModels -> {
    //                            List<String> modelCodes = new ArrayList<>(remoteModels.size()); // TODO:: To be used in future
                                for (TranslateRemoteModel model : remoteModels) {
    //                                modelCodes.add(model.getLanguage());
                                    Log.d(LOG_TAG, "models names - " + model.getLanguage() + "   " + model);
                                }
                            });
        }
    }


    public static void translateStringInTextView(String stringToTranslate, TextView textView){
            if (translator!=null) {
                translator.translate(stringToTranslate)
                        .addOnSuccessListener(
                                translatedText -> {
                                    if (textView!=null) {
                                        Animation fadeInAnimation = AnimationUtils.loadAnimation(textView.getContext(), R.anim.fadein);
                                        textView.startAnimation(fadeInAnimation);
                                        textView.setHeight(textView.getHeight());
                                        textView.setText(translatedText);
                                    }
                                })
                        .addOnFailureListener(
                                e -> {
                                    Log.d(LOG_TAG, "translation failed");
                                });
            }
    }

    public static void translateStringWithCallback(String initialAddress, String callback, BridgeComponents bridgeComponents) {
        if (translator != null) {
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
        }else{
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                    callback, initialAddress);
            bridgeComponents.getJsCallback().addJsToWebView(javascript);
        }

    }
}
