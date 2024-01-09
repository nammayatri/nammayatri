package in.juspay.mobility.app;

import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.util.Base64;
import com.google.android.gms.tasks.Task;
import com.google.mlkit.vision.common.InputImage;
import com.google.mlkit.vision.text.Text;
import com.google.mlkit.vision.text.TextRecognition;
import com.google.mlkit.vision.text.TextRecognizer;
import com.google.mlkit.vision.text.latin.TextRecognizerOptions;
import java.util.Locale;
import in.juspay.hyper.core.BridgeComponents;

public class TextRecognizerKit {

    TextRecognizer recognizer;

    public TextRecognizerKit() {
        recognizer = TextRecognition.getClient(TextRecognizerOptions.DEFAULT_OPTIONS);
    }

    public void decodeImageInText(String callback, String base64, BridgeComponents bridgeComponents){
        try {
            byte[] imageBytes = Base64.decode(base64, Base64.DEFAULT); // Decode the base64 string into a byte array
            Bitmap bitmap = BitmapFactory.decodeByteArray(imageBytes, 0, imageBytes.length); // Convert the byte array to a Bitmap
            InputImage image = InputImage.fromBitmap(bitmap, 0); // Create InputImage from the Bitmap // You may need to provide the rotation degree
            if (recognizer == null) recognizer = TextRecognition.getClient(TextRecognizerOptions.DEFAULT_OPTIONS);
            Task<Text> result = recognizer.process(image)
                    .addOnSuccessListener(visionText -> {
                        String resultText = visionText.getText();
                        resultText = resultText.replace("'","");
                        resultText = resultText.replace("\n", " ").trim();
                        String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                                callback, resultText);
                        bridgeComponents.getJsCallback().addJsToWebView(javascript);
                        // Task completed successfully
                    })
                    .addOnFailureListener(e -> {
                        String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                                callback, ""); //ERROR
                        bridgeComponents.getJsCallback().addJsToWebView(javascript);
                        // Task failed with an exception
                    });
        }catch (Exception e){
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                    callback, ""); //ERROR
            bridgeComponents.getJsCallback().addJsToWebView(javascript);
            //Failure
        }
    }
}