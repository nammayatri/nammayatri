package in.juspay.mobility.app;

import android.app.Activity;
import android.app.PictureInPictureParams;
import android.graphics.Rect;
import android.os.Build;
import android.util.Rational;

import org.json.JSONObject;

import in.juspay.hyper.core.BridgeComponents;

public class PIPMode {
    public static void enterPipMode(Activity activity, JSONObject jsonObject) {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O && activity != null) {
            int numerator = jsonObject.optInt("numerator", 3);
            int denominator = jsonObject.optInt("denominator", 4);


            Rational aspectRatio = new Rational(numerator, denominator);
            PictureInPictureParams.Builder params = new PictureInPictureParams.Builder()
                    .setAspectRatio(aspectRatio)
                    .setSourceRectHint(new Rect());

            JSONObject rectObj = jsonObject.optJSONObject("rect");
            if(rectObj!= null){
                int left = rectObj.optInt("left", 0);
                int right = rectObj.optInt("right", 0);
                int top = rectObj.optInt("top", 0);
                int bottom = rectObj.optInt("bottom", 0);

                Rect rect = new Rect(left, top, right, bottom);
                params.setSourceRectHint(rect);
            }

            activity.enterPictureInPictureMode( params.build());
        }
    }

}
