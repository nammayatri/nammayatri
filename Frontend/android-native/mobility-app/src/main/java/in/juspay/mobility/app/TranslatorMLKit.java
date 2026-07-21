/*
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.app;


import static java.util.Map.entry;
import static in.juspay.mobility.app.RideRequestUtils.firebaseLogEventWithParams;

import android.content.Context;
import android.util.Log;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;
import android.widget.TextView;

import com.google.mlkit.common.model.RemoteModelManager;
import com.google.mlkit.nl.translate.TranslateRemoteModel;
import com.google.mlkit.nl.translate.Translation;
import com.google.mlkit.nl.translate.Translator;
import com.google.mlkit.nl.translate.TranslatorOptions;

import org.json.JSONArray;
import org.json.JSONObject;

import java.util.Collections;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import in.juspay.hyper.core.BridgeComponents;


public class TranslatorMLKit {
    private static final Map<String, Translator> translatorCache = new ConcurrentHashMap<>();
    private Translator translator;

    private final Context context;
    private String sourceLanguage;
    private String targetLanguage;

    static final RemoteModelManager remoteModelManager = RemoteModelManager.getInstance();

    public TranslatorMLKit(Context context) {
        this.context = context;
    }

    public TranslatorMLKit(String sourceLanguage, String destinationLanguage, Context context) {
        this.context = context;
        this.sourceLanguage = getSupportedLanguageCode(sourceLanguage, "en");
        this.targetLanguage = getSupportedLanguageCode(destinationLanguage, "en");
        if (!this.sourceLanguage.equals(this.targetLanguage)) {
            translator = downloadModel(this.sourceLanguage, this.targetLanguage);
        }
    }

    public static String result, LOG_TAG = "Mobility Translator";

    public void triggerDownloadForLang(String language) {
        try {
            String targetLanguage = getSupportedLanguageCode(language, "en");
            TranslatorOptions options = buildTranslatorOptions("en", targetLanguage);
            initializeTranslator("en-" + targetLanguage, options, context);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public Translator downloadModel(String sourceLanguage, String finalLanguage) {
        try {
            TranslatorOptions options = buildTranslatorOptions(sourceLanguage, finalLanguage);
            return initializeTranslator(sourceLanguage + "-" + finalLanguage, options, context);
        } catch (Exception exception) {
            exception.printStackTrace();
            firebaseLogEventWithParams("download_translation_model_ml", "download_failed", exception.toString(), context);
            return null;
        }
    }

    private TranslatorOptions buildTranslatorOptions(String sourceLanguage, String finalLanguage) {
        return new TranslatorOptions.Builder()
                .setSourceLanguage(sourceLanguage)
                .setTargetLanguage(finalLanguage)
                .build();
    }

    private Translator initializeTranslator(String cacheKey, TranslatorOptions options, Context context) {
        return translatorCache.computeIfAbsent(cacheKey, ignored -> Translation.getClient(options));
    }

    public void deleteDownloadedModel(String language) {
        TranslateRemoteModel deleteModel =
                new TranslateRemoteModel.Builder(language).build();

        remoteModelManager.isModelDownloaded(deleteModel)
                .addOnSuccessListener(
                        isDownloaded -> {
                            if (isDownloaded) {
                                remoteModelManager.deleteDownloadedModel(deleteModel)
                                        .addOnSuccessListener(unused -> {

                                        })
                                        .addOnFailureListener(e -> {

                                        });
                            }
                        })
                .addOnFailureListener(
                        e -> Log.d(LOG_TAG, "list downloaded models failed"));
    }

    public void translateStringInTextView(String stringToTranslate, TextView textView) {
        if (translator != null) {
            translator.downloadModelIfNeeded()
                    .addOnSuccessListener(
                            v -> translator.translate(stringToTranslate)
                                    .addOnSuccessListener(
                                            translatedText -> {
                                                if (textView != null) {
                                                    Animation fadeInAnimation = AnimationUtils.loadAnimation(textView.getContext(), R.anim.fadein);
                                                    textView.startAnimation(fadeInAnimation);
                                                    textView.setText(translatedText);
                                                }
                                            })
                                    .addOnFailureListener(
                                            e -> Log.d(LOG_TAG, "translation failed")))
                    .addOnFailureListener(
                            e -> {
                                Log.d(LOG_TAG, "download failed");
                                firebaseLogEventWithParams("download_translation_model", "download_failed", e.toString(), context);
                            });
        }
    }

    public void translateStringWithCallback(String initialAddress, String callback, BridgeComponents bridgeComponents) {
        if (sourceLanguage != null && sourceLanguage.equals(targetLanguage)) {
            sendCallback(callback, initialAddress, bridgeComponents);
        } else if (translator != null) {
            translator.downloadModelIfNeeded()
                    .addOnSuccessListener(
                            v -> translator.translate(initialAddress)
                                    .addOnSuccessListener(
                                            translatedText -> sendCallback(callback, translatedText, bridgeComponents))
                                    .addOnFailureListener(
                                            e -> sendCallback(callback, initialAddress, bridgeComponents)))
                    .addOnFailureListener(
                            e -> {
                                Log.d(LOG_TAG, "download failed");
                                firebaseLogEventWithParams("download_translation_model", "download_failed", e.toString(), context);
                                sendCallback(callback, initialAddress, bridgeComponents);
                            });
        } else {
            sendCallback(callback, initialAddress, bridgeComponents);
        }

    }

    private void sendCallback(String callback, String value, BridgeComponents bridgeComponents) {
        if (callback != null && bridgeComponents != null) {
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s',%s);",
                    callback, JSONObject.quote(value));
            bridgeComponents.getJsCallback().addJsToWebView(javascript);
        }
    }

    public void listDownloadedModels(String callback, BridgeComponents bridgeComponents) {
        remoteModelManager.getDownloadedModels(TranslateRemoteModel.class)
                .addOnSuccessListener(
                        remoteModels -> {
                            JSONArray models = new JSONArray();
                            for (TranslateRemoteModel model : remoteModels) {
                                models.put(model.getLanguage());
                            }
                            if (callback != null && bridgeComponents != null) {
                                String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                                        callback, models);
                                bridgeComponents.getJsCallback().addJsToWebView(javascript);
                            }
                            Log.d(LOG_TAG, "list of downloaded models - " + models);
                        })
                .addOnFailureListener(
                        e -> {
                            if (callback != null && bridgeComponents != null) {
                                JSONArray models = new JSONArray();
                                String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                                        callback, models);
                                bridgeComponents.getJsCallback().addJsToWebView(javascript);
                            }
                            Log.d(LOG_TAG, "list downloaded models failed");
                        });
    }

    public static String getSupportedLanguageCode(String language, String fallbackLanguage) {
        if (language == null || language.equals("null") || language.length() < 2) {
            return fallbackLanguage;
        }
        String key = language.split("[-_]")[0].toLowerCase(Locale.ROOT);
        return languageMap.containsKey(key) ? key : fallbackLanguage;
    }

    public static final Map<String, String> languageMap = Collections.unmodifiableMap(new HashMap<>(Map.<String, String>ofEntries(
            entry("af", "Afrikaans"),
            entry("ar", "Arabic"),
            entry("be", "Belarusian"),
            entry("bg", "Bulgarian"),
            entry("bn", "Bengali"),
            entry("ca", "Catalan"),
            entry("cs", "Czech"),
            entry("cy", "Welsh"),
            entry("da", "Danish"),
            entry("de", "German"),
            entry("el", "Greek"),
            entry("en", "English"),
            entry("eo", "Esperanto"),
            entry("es", "Spanish"),
            entry("et", "Estonian"),
            entry("fa", "Persian"),
            entry("fi", "Finnish"),
            entry("fr", "French"),
            entry("ga", "Irish"),
            entry("gl", "Galician"),
            entry("gu", "Gujarati"),
            entry("he", "Hebrew"),
            entry("hi", "Hindi"),
            entry("hr", "Croatian"),
            entry("ht", "Haitian"),
            entry("hu", "Hungarian"),
            entry("id", "Indonesian"),
            entry("is", "Icelandic"),
            entry("it", "Italian"),
            entry("ja", "Japanese"),
            entry("ka", "Georgian"),
            entry("kn", "Kannada"),
            entry("ko", "Korean"),
            entry("lt", "Lithuanian"),
            entry("lv", "Latvian"),
            entry("mk", "Macedonian"),
            entry("mr", "Marathi"),
            entry("ms", "Malay"),
            entry("mt", "Maltese"),
            entry("nl", "Dutch"),
            entry("no", "Norwegian"),
            entry("pl", "Polish"),
            entry("pt", "Portuguese"),
            entry("ro", "Romanian"),
            entry("ru", "Russian"),
            entry("sk", "Slovak"),
            entry("sl", "Slovenian"),
            entry("sq", "Albanian"),
            entry("sv", "Swedish"),
            entry("sw", "Swahili"),
            entry("ta", "Tamil"),
            entry("te", "Telugu"),
            entry("th", "Thai"),
            entry("tl", "Tagalog"),
            entry("tr", "Turkish"),
            entry("uk", "Ukrainian"),
            entry("ur", "Urdu"),
            entry("vi", "Vietnamese"),
            entry("zh", "Chinese")
    )));
}
