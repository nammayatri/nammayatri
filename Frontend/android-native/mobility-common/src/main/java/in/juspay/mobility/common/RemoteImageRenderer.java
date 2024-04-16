package in.juspay.mobility.common;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.PorterDuff;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;
import android.graphics.drawable.GradientDrawable;
import android.os.AsyncTask;
import android.os.Build;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import in.juspay.hyper.core.BridgeComponents;

public class RemoteImageRenderer extends AsyncTask<String, Void, Bitmap> {
    private ImageView imageView;
    private final BridgeComponents bridgeComponents;
    private String pointerImage;
    private boolean isInvisiblePointer;
    private String actionImage;
    private MobilityCommonBridge.MarkerType markerType;
    private MobilityCommonBridge.MarkerConfig markerConfig;

    public RemoteImageRenderer(ImageView imageView, BridgeComponents bridgeComponents, String pointerImage, boolean isInvisiblePointer, String actionImage, MobilityCommonBridge.MarkerType markerType, MobilityCommonBridge.MarkerConfig markerConfig) {
        this.imageView = imageView;
        this.bridgeComponents = bridgeComponents;
        this.pointerImage = pointerImage;
        this.isInvisiblePointer = isInvisiblePointer;
        this.actionImage = actionImage;
        this.markerType = markerType;
        this.markerConfig = markerConfig;
    }

    @Override
    protected Bitmap doInBackground(String... urls) {
        String urlDisplay = urls[0];
        Bitmap bitmap = null;
        try {
            URL url = new URL(urlDisplay);
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            connection.setDoInput(true);
            connection.connect();
            InputStream input = connection.getInputStream();
            bitmap = BitmapFactory.decodeStream(input);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return bitmap;
    }

    @Override
    protected void onPostExecute(Bitmap bitmap) {
//        imageView.setImageBitmap(result);
        Context context = this.bridgeComponents.getContext();
        @SuppressLint("InflateParams")

        String textColor = markerConfig.theme.equals(MobilityCommonBridge.Theme.DARK) ? "#FFFFFF" : "#454545";
        String backgroundColor = markerConfig.theme.equals(MobilityCommonBridge.Theme.DARK) ? "#454545" : "#FFFFFF";

        View customMarkerView = ((LayoutInflater) context.getSystemService(Context.LAYOUT_INFLATER_SERVICE)).inflate(context.getResources().getLayout(context.getResources().getIdentifier("marker_label_layout", "layout", context.getPackageName())), null);
        try {
            setMarkerBackground(backgroundColor, customMarkerView);
            setMarkerText(textColor, customMarkerView, markerConfig);
            setMarkerlabelImage(markerConfig.labelImage, customMarkerView, bitmap);
            setMarkerPointerImage(pointerImage, isInvisiblePointer, markerType, customMarkerView);
            setMarkerActionImage(actionImage, markerConfig.primaryText, customMarkerView);
            setLabelImageAction(customMarkerView, markerConfig.labelActionImage);
        } catch (Exception e) {
            Log.e("getMarkerBitmapFromView", "Exception in rendering Image" + e);
        }
        customMarkerView.measure(View.MeasureSpec.UNSPECIFIED, View.MeasureSpec.UNSPECIFIED);
        customMarkerView.layout(0, 0, customMarkerView.getMeasuredWidth(), customMarkerView.getMeasuredHeight());
        customMarkerView.buildDrawingCache();
        Bitmap returnedBitmap = Bitmap.createBitmap(customMarkerView.getMeasuredWidth(), customMarkerView.getMeasuredHeight(), Bitmap.Config.ARGB_8888);
        Canvas canvas = new Canvas(returnedBitmap);
        canvas.drawColor(Color.WHITE, PorterDuff.Mode.SRC_IN);
        Drawable drawable = customMarkerView.getBackground();
        if (drawable != null)
            drawable.draw(canvas);
        customMarkerView.draw(canvas);
        this.imageView.setImageBitmap(returnedBitmap);
//        return returnedBitmap;
    }

    private void setLabelImageAction(View customMarkerView, String labelActionImage) {
        if (!labelActionImage.equals("")) {
            Context context = this.bridgeComponents.getContext();
            ImageView imageView = customMarkerView.findViewById(R.id.label_image_action);
            imageView.setVisibility(View.VISIBLE);
            imageView.setImageDrawable(context.getResources().getDrawable(context.getResources().getIdentifier(labelActionImage, "drawable", context.getPackageName())));
        }
    }

    private void setMarkerActionImage(String actionImage, String primaryText, View customMarkerView) {
        Context context = this.bridgeComponents.getContext();
        if (actionImage != null) {
            ImageView markerActionImage = customMarkerView.findViewById(R.id.marker_action_image);
            markerActionImage.setVisibility(View.VISIBLE);
            markerActionImage.setImageDrawable(context.getResources().getDrawable(context.getResources().getIdentifier(actionImage, "drawable", context.getPackageName())));
        }
        if (actionImage == null && primaryText.equals("")){
            View mainLableLayout = customMarkerView.findViewById(R.id.main_label_layout);
            mainLableLayout.setVisibility(View.GONE);
        }
    }

    private void setMarkerPointerImage(String pointerImage, Boolean isInvisiblePointer, MobilityCommonBridge.MarkerType markerType, View customMarkerView) {
        Context context = this.bridgeComponents.getContext();
        ImageView pointer = customMarkerView.findViewById(R.id.pointer_img);
        if (pointerImage != null)
            pointer.setImageDrawable(context.getResources().getDrawable(context.getResources().getIdentifier(pointerImage, "drawable", context.getPackageName())));
        if (isInvisiblePointer)
            pointer.setVisibility(View.INVISIBLE);
        if (markerType.equals(MobilityCommonBridge.MarkerType.SPECIAL_ZONE_MARKER)) {
            ViewGroup.LayoutParams layoutParams = pointer.getLayoutParams();
            layoutParams.height = 55;
            layoutParams.width = 55;
            pointer.setLayoutParams(layoutParams);
        } else {
            if (pointerImage != null && pointerImage.equals("ny_ic_customer_current_location")) {
                ViewGroup.LayoutParams layoutParams = pointer.getLayoutParams();
                layoutParams.height = 160;
                layoutParams.width = 160;
                pointer.setImportantForAccessibility(2);
                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.P)
                    pointer.setAccessibilityHeading(false);
                pointer.setLayoutParams(layoutParams);
            }
        }
    }

    private void setMarkerlabelImage(String labelImageName, View customMarkerView, Bitmap bitmap) {
        try {
            Context context = this.bridgeComponents.getContext();
//            if (labelImageName != null && !labelImageName.equals("")) {
            ImageView labelImage = customMarkerView.findViewById(R.id.zone_image);
            labelImage.setVisibility(View.VISIBLE);
//            int imageID = context.getResources().getIdentifier(labelImageName, "drawable", bridgeComponents.getContext().getPackageName());
//            BitmapDrawable bitmap = (BitmapDrawable) context.getResources().getDrawable(imageID);
            labelImage.setImageBitmap(bitmap);
//            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void setMarkerText(String textColor, View customMarkerView, MobilityCommonBridge.MarkerConfig markerConfig) {
        try {
            Integer labelTextSize = 30;
            View ImageAndTextView = customMarkerView.findViewById(R.id.zone_image_and_text);
            View labelTextViews = ImageAndTextView.findViewById(R.id.label_texts);
            String primaryText = markerConfig.primaryText;
            String secondaryText = markerConfig.secondaryText;
            TextView primaryTextView = labelTextViews.findViewById(R.id.primary_marker_text);
            TextView secondaryTextView = labelTextViews.findViewById(R.id.secondary_marker_text);
            if (primaryText.equals("")) {
                ImageAndTextView.setVisibility(View.GONE);
            } else {
                String labelText = primaryText.length() > labelTextSize ? primaryText.substring(0, labelTextSize - 3) + "..." : primaryText;
                primaryTextView.setText(labelText);
                primaryTextView.setImportantForAccessibility(TextView.IMPORTANT_FOR_ACCESSIBILITY_YES);
                primaryTextView.setContentDescription(labelText);
                primaryTextView.setTextColor(Color.parseColor(textColor));
            }
            if (!secondaryText.equals("")) {
                String secondaryLabelText = secondaryText.length() > labelTextSize ? secondaryText.substring(0, labelTextSize - 3) + "..." : secondaryText;
                secondaryTextView.setText(secondaryLabelText);
                secondaryTextView.setImportantForAccessibility(TextView.IMPORTANT_FOR_ACCESSIBILITY_YES);
                secondaryTextView.setContentDescription(secondaryLabelText);
                secondaryTextView.setVisibility(View.VISIBLE);
                secondaryTextView.setTextColor(Color.parseColor(textColor));
            } else {
                ViewGroup.LayoutParams labelTextsViewLayoutParams = labelTextViews.getLayoutParams();
                labelTextsViewLayoutParams.height = ViewGroup.LayoutParams.MATCH_PARENT;
                labelTextViews.setLayoutParams(labelTextsViewLayoutParams);
                ViewGroup.LayoutParams primaryTextViewLayoutParams = primaryTextView.getLayoutParams();
                primaryTextViewLayoutParams.height = ViewGroup.LayoutParams.MATCH_PARENT;
                primaryTextView.setLayoutParams(primaryTextViewLayoutParams);
            }
        }catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void setMarkerBackground(String backgroundColor, View customMarkerView) {
        Context context = this.bridgeComponents.getContext();
        View background = customMarkerView.findViewById(R.id.main_label_layout);
        GradientDrawable drawable = (GradientDrawable) context.getResources().getDrawable(context.getResources().getIdentifier("ic_grey_border", "drawable", context.getPackageName()));
        drawable.setColor(Color.parseColor(backgroundColor));
        background.setBackground(drawable);
    }

}