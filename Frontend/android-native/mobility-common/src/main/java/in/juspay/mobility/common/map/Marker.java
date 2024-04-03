package in.juspay.mobility.common.map;

import static in.juspay.mobility.common.Utils.drawableToBitmap;

import android.animation.Animator;
import android.animation.AnimatorListenerAdapter;
import android.animation.ValueAnimator;
import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.PorterDuff;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;
import android.graphics.drawable.GradientDrawable;
import android.os.Build;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.LinearInterpolator;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.google.android.gms.maps.GoogleMap;
import com.google.android.gms.maps.model.BitmapDescriptorFactory;
import com.google.android.gms.maps.model.LatLng;
import com.google.android.gms.maps.model.MarkerOptions;
import com.google.maps.android.SphericalUtil;

import org.json.JSONObject;

import in.juspay.hyper.core.BridgeComponents;
import in.juspay.hyper.core.ExecutorManager;
import in.juspay.mobility.common.R;

public class Marker {

    @Nullable private com.google.android.gms.maps.model.Marker marker;
    private MarkerOptions markerOptions;
    private final BridgeComponents bridgeComponents;
    private String onClickCallback;
    private final String id;

    public enum Theme {
        DARK, LIGHT
    }

    public Marker(final String id, final BridgeComponents bridgeComponents){
        markerOptions = new MarkerOptions();
        this.bridgeComponents = bridgeComponents;
        this.id = id;
    }

    /*
    * @Desc: Insert the marker based on the configObj
    * */
    @SuppressLint("PotentialBehaviorOverride")
    public void addMarker(GoogleMap googleMap, JSONObject configObj){
        ExecutorManager.runOnMainThread(() -> {
            try {

                if (!configObj.optString("title", "").equals("")) {
                    markerOptions = markerOptions.title(configObj.getString("title"));
                }

                if (configObj.has("lat") && configObj.has("lon") && configObj.getDouble("lat") != 0.0 && configObj.getDouble("lon") != 0.0) {
                    markerOptions = markerOptions.position(new LatLng(configObj.getDouble("lat"), configObj.getDouble("lon")));
                }

                if (configObj.has("visible")) {
                    markerOptions = markerOptions.visible(configObj.getBoolean("visible"));
                }

                if (configObj.has("flat")) {
                    markerOptions = markerOptions.flat(configObj.getBoolean("flat"));
                }


                if (configObj.has("bitmapMarker") && configObj.getJSONObject("bitmapMarker").optBoolean("visible", false)) {
                    markerOptions = markerOptions.icon(BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(configObj.getJSONObject("bitmapMarker"))));
                }

                if (configObj.optDouble("anchorU", 0.0) != 0.0 && configObj.optDouble("anchorV", 0.0) != 0.0) {
                    markerOptions = markerOptions.anchor((float) configObj.getDouble("anchorU"), (float) configObj.getDouble("anchorV"));
                }


                marker = googleMap.addMarker(markerOptions);
                if (marker != null) {
                    marker.setTag(id);
                }

                if (configObj.has("hideInfoWindow")) {
                    marker.hideInfoWindow();
                }

                if (configObj.has("onClick") && !configObj.getString("onClick").equals("")) {
                    this.onClickCallback = configObj.getString("onClick");
                }

            } catch (Exception e) {
                e.printStackTrace();
            }
        });
    }


    public String getOnClickCallback(){
        return onClickCallback;
    }

    /*
    * @Desc: Move the marker to new position with animation
    * */
    public void moveMarker(LatLng newPos, boolean movingAnimation){
        ExecutorManager.runOnMainThread(() -> {
            if (marker == null) {
                return ;
            }

            LatLng oldPos = marker.getPosition();
            if (movingAnimation) {
                moveWithAnimation(newPos);
            } else if (!oldPos.equals(newPos)) {
                marker.setPosition(newPos);
            }
        });
    }

    private void moveWithAnimation(LatLng destination) {
        if (marker != null) {
            LatLng startPosition = marker.getPosition();

            ValueAnimator valueAnimator = ValueAnimator.ofFloat(0, 1);
            valueAnimator.setDuration(2000);
            valueAnimator.setInterpolator(new LinearInterpolator());

            valueAnimator.addUpdateListener(animation -> {
                try {
                    float v = animation.getAnimatedFraction();
                    LatLng newPosition = SphericalUtil.interpolate(startPosition, destination, v);
                    float rotation = (float) SphericalUtil.computeHeading(startPosition, destination);
                    marker.setRotation(rotation);
                    marker.setPosition(newPosition);
                } catch (Exception e) {
                    e.printStackTrace();
                }
            });
            valueAnimator.addListener(new AnimatorListenerAdapter() {
                @Override
                public void onAnimationEnd(Animator animation) {
                    super.onAnimationEnd(animation);
                }
            });

            valueAnimator.start();
        }
    }

    /*
     * @Desc: Returns the current marker position.
     * */
    @Nullable public LatLng getCurrentPosition(){
        return marker == null ? null : marker.getPosition();
    }

    /*
     * @Desc: remove the marker from map
     * */
    public void removeMarker(){
        ExecutorManager.runOnMainThread(() -> {
            if(marker != null) {
                marker.remove();
            }
        });
    }

    /*
    * @Desc: Create Bitmap marker based on Config
    * */
    protected Bitmap getMarkerBitmapFromView(JSONObject configObj) {
        Context context = bridgeComponents.getContext();

        boolean visible = configObj.optBoolean("visible", false);
        String pointerImage = configObj.optString("pointerImage", "");
        int ptrImgHeight = configObj.optInt("ptrImgHeight", 0);
        int ptrImgWidth = configObj.optInt("ptrImgWidth", 0);
        String actionImage = configObj.optString("actionImage", "");
        String textColor = configObj.optString("textColor", "#FFFFFF");
        String backgroundColor = configObj.optString("backgroundColor", "#454545");
        String primaryText = configObj.optString("primaryText", "");
        String secondaryText = configObj.optString("secondaryText", "");
        int textMaxSize = configObj.optInt("textMaxSize", 30);
        String labelImage = configObj.optString("labelImage", "");
        String labelActionImage = configObj.optString("labelActionImage", "");
        double ptrImgMagnifier = configObj.optDouble("ptrImgMagnifier", 1.0);
        boolean ptrImgVis = configObj.optBoolean("ptrImgVis", false);


        View customMarkerView = ((LayoutInflater) context.getSystemService(Context.LAYOUT_INFLATER_SERVICE)).inflate(context.getResources().getLayout(context.getResources().getIdentifier("marker_label_layout", "layout", context.getPackageName())), null);

        try {
            setMarkerBackground(backgroundColor, customMarkerView);
            setMarkerText(textColor, primaryText, secondaryText, textMaxSize, customMarkerView);
            setMarkerlabelImage(labelImage, customMarkerView);
            setMarkerPointerImage(pointerImage, ptrImgVis, ptrImgHeight, ptrImgWidth, ptrImgMagnifier, customMarkerView);
            setMarkerActionImage(actionImage, primaryText, customMarkerView);
            setLabelImageAction(customMarkerView, labelActionImage);
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
        return returnedBitmap;
    }

    private void setMarkerBackground(String backgroundColor, View customMarkerView) {
        Context context = bridgeComponents.getContext();
        View background = customMarkerView.findViewById(R.id.main_label_layout);
        @SuppressLint({"UseCompatLoadingForDrawables", "DiscouragedApi"}) GradientDrawable drawable = (GradientDrawable) context.getResources().getDrawable(context.getResources().getIdentifier("ic_grey_border", "drawable", context.getPackageName()));
        drawable.setColor(Color.parseColor(backgroundColor));
        background.setBackground(drawable);
    }

    private void setMarkerText(String textColor, final String primaryText, final String secondaryText, final int textMaxSize, View customMarkerView) {
        try {
            View ImageAndTextView = customMarkerView.findViewById(R.id.zone_image_and_text);

            View labelTextViews = ImageAndTextView.findViewById(R.id.label_texts);
            TextView primaryTextView = labelTextViews.findViewById(R.id.primary_marker_text);
            TextView secondaryTextView = labelTextViews.findViewById(R.id.secondary_marker_text);


            if (primaryText.equals("")) {
                ImageAndTextView.setVisibility(View.GONE);
            } else {
                String labelText = primaryText.length() > textMaxSize ? primaryText.substring(0, textMaxSize - 3) + "..." : primaryText;
                primaryTextView.setText(labelText);
                primaryTextView.setImportantForAccessibility(TextView.IMPORTANT_FOR_ACCESSIBILITY_YES);
                primaryTextView.setContentDescription(labelText);
                primaryTextView.setTextColor(Color.parseColor(textColor));
            }
            if (!secondaryText.equals("")) {
                String secondaryLabelText = secondaryText.length() > textMaxSize ? secondaryText.substring(0, textMaxSize - 3) + "..." : secondaryText;
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

    private void setMarkerlabelImage(String labelImageName, View customMarkerView) {
        try {
            Context context = bridgeComponents.getContext();
            if (labelImageName != null && !labelImageName.equals("")) {
                ImageView labelImage = customMarkerView.findViewById(R.id.zone_image);
                labelImage.setVisibility(View.VISIBLE);
                int imageID = context.getResources().getIdentifier(labelImageName, "drawable", bridgeComponents.getContext().getPackageName());
                BitmapDrawable bitmap = (BitmapDrawable) context.getResources().getDrawable(imageID);
                labelImage.setImageDrawable(bitmap);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void setMarkerPointerImage(final String pointerImage, final Boolean visible, final int height, final int width, final double magnifier, final View customMarkerView) {
        Context context = bridgeComponents.getContext();
        ImageView pointer = customMarkerView.findViewById(R.id.pointer_img);

        if (!pointerImage.equals("")) {
            Bitmap bitmap = drawableToBitmap(context.getResources().getDrawable(context.getResources().getIdentifier(pointerImage, "drawable", context.getPackageName())));
            int btWidth = bitmap.getWidth();
            int btHeight = bitmap.getHeight();

            pointer.setImageBitmap(bitmap);

            ViewGroup.LayoutParams layoutParams = pointer.getLayoutParams();

            if(height != 0 && width != 0) {
                layoutParams.height = height;
                layoutParams.width = width;
            }else if(magnifier != 0){
                layoutParams.height = (int) (btWidth * magnifier);
                layoutParams.width = (int) (btHeight * magnifier);
            }
        }

        if (!visible) {
            pointer.setVisibility(View.INVISIBLE);
        }

        pointer.setImportantForAccessibility(2);
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.P)
            pointer.setAccessibilityHeading(false);

    }

    private void setMarkerActionImage(@NonNull String actionImage, String primaryText, View customMarkerView) {
        Context context = bridgeComponents.getContext();
        if (!actionImage.equals("")) {
            ImageView markerActionImage = customMarkerView.findViewById(R.id.marker_action_image);
            markerActionImage.setVisibility(View.VISIBLE);
            markerActionImage.setImageDrawable(context.getResources().getDrawable(context.getResources().getIdentifier(actionImage, "drawable", context.getPackageName())));
        }
        if (actionImage.equals("") && primaryText.equals("")){
            View mainLableLayout = customMarkerView.findViewById(R.id.main_label_layout);
            mainLableLayout.setVisibility(View.GONE);
        }
    }

    private void setLabelImageAction(View customMarkerView, String labelActionImage) {
        if (!labelActionImage.equals("")) {
            Context context = bridgeComponents.getContext();
            ImageView imageView = customMarkerView.findViewById(R.id.label_image_action);
            imageView.setVisibility(View.VISIBLE);
            imageView.setImageDrawable(context.getResources().getDrawable(context.getResources().getIdentifier(labelActionImage, "drawable", context.getPackageName())));
        }
    }
}



