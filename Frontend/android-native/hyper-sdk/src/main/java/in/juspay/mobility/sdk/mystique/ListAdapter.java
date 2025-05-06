package in.juspay.mobility.sdk.mystique;

import android.content.Context;
import android.content.pm.PackageManager;
import android.graphics.Bitmap;
import android.graphics.Color;
import android.graphics.Typeface;
import android.graphics.drawable.ColorDrawable;
import android.graphics.drawable.Drawable;
import android.graphics.drawable.GradientDrawable;
import android.os.Build;
import android.text.TextUtils;
import android.util.LruCache;
import android.util.TypedValue;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.core.content.res.ResourcesCompat;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.InputStream;
import java.util.Arrays;
import java.util.Hashtable;
import java.util.Iterator;

import in.juspay.mobility.sdk.core.DuiCallback;
import in.juspay.mobility.sdk.core.Renderer;

public class ListAdapter extends BaseAdapter implements DownloadImageTask.Adapter {

    private final DuiCallback duiCallback;
    private final float density;
    private final Context context;
    private final Renderer renderer;
    private final JSONObject itemView;
    private JSONArray rowData;
    private final JSONArray holderData;

    private final LruCache<String, Integer> colorCache;
    private final LruCache<String, Drawable> drawableCache;
    private final LruCache<String, Typeface> typefaceCache;
    private final LruCache<String, Integer> typefaceWeightCache;
    private final BitmapCache bitmapCache;
    private final Hashtable<Integer, Integer> imageDownloadMap;
    private final Hashtable<Integer, Integer> imageRetryCountMap;
    private int maxRetryImageTaskCount;

    public ListAdapter(Context context, Renderer renderer, JSONObject itemView, JSONArray holderData,
                JSONArray rowData, DuiCallback duiCallback) {
        this.renderer = renderer;
        this.rowData = rowData;
        this.itemView = itemView;
        this.holderData = holderData;
        this.duiCallback = duiCallback;
        this.bitmapCache = BitmapCache.getInstance();
        this.colorCache = new LruCache<>(20);
        this.drawableCache = new LruCache<>(50);
        this.typefaceCache = new LruCache<>(20);
        this.typefaceWeightCache = new LruCache<>(20);
        this.context = context;
        this.imageRetryCountMap = new Hashtable<>();
        this.imageDownloadMap = new Hashtable<>();
        this.maxRetryImageTaskCount = 3;
        this.density = context.getResources().getDisplayMetrics().density;
    }

    private View createView() {
        try {
            return renderer.createView(itemView);
        } catch (Exception e) {
            return null;
        }
    }

    private String getString(JSONObject object, String key, String def) {
        try {
            return object.getString(key);
        } catch (Exception e) {
            return def;
        }
    }

    private void setBackground(View view, String value) {
        if (value == null) {
            if (view.getBackground() instanceof GradientDrawable) {
                ((GradientDrawable) view.getBackground()).setColor(Color.TRANSPARENT);
            } else {
                view.setBackgroundDrawable(null);
            }
            view.setBackgroundDrawable(null);
        } else {
            Integer color = colorCache.get(value);
            if (color == null) {
                color = Color.parseColor(value);
                colorCache.put(value, color);
            }
            Drawable drawable = view.getBackground();
            if (drawable == null || (drawable instanceof ColorDrawable
                    && ((ColorDrawable) drawable).getColor() != color)) {
                view.setBackgroundColor(color);
            } else if (drawable instanceof GradientDrawable) {
                ((GradientDrawable) drawable).setColor(color);
            }
        }
    }

    private void setText(View view, String text) {
        if (view instanceof TextView && !((TextView) view).getText().equals(text)) {
            ((TextView) view).setText(text);
        }
    }

    private void setTextColor(View view, String color) {
        if (!(view instanceof TextView)) {
            return;
        }
        if (color == null) {
            ((TextView) view).setTextColor(Color.BLACK);
        } else {
            Integer colorInt = colorCache.get(color);
            if (colorInt == null) {
                colorInt = Color.parseColor(color);
                colorCache.put(color, colorInt);
            }
            ((TextView) view).setTextColor(colorInt);
        }
    }

    private void setImage(View view, String imageLocation, int index) {
        /*  imageSources                           placholder: -> resName -> applicable for urls only

         *  resId->number
         *  imgName                 -> considered present in res/drawable
         *  path->assets/img/../imgName.png
         *  path->res/drawable/../imgNAme.png
         *  url->url,resName
         * */
        if (!(view instanceof ImageView)) {
            return;
        }
        final ImageView imageView = (ImageView) view ;
        try{
            String[] imgArr = imageLocation.split(",");
            Integer placeholder = null;   //  placholder is expected to be a resource id
            if(imgArr.length > 1 && !imgArr[1].isEmpty()){
                placeholder = context.getResources().getIdentifier(imgArr[1], "drawable", context.getPackageName());
            }
            String[] imgAttr = imgArr[0].split("->");   // type->src
            Drawable drawable = null ;
            String cacheKey = "";
            if(imgAttr.length == 1){    // matching when only image name is passed
                String imgName = imgAttr[0];
                int resId = context.getResources().getIdentifier(imgName, "drawable", context.getPackageName());
                drawable = drawableCache.get(imgName);
                if(drawable == null){
                    drawable = context.getResources().getDrawable(resId);
                    cacheKey=imgName;
                }
            }
            else {
                switch(imgAttr[0]){
                    case "path" :
                        if(imgAttr[1].contains("assets/")){
                            String imgPath = imgAttr[1].replace("assets/", "");
                            drawable = drawableCache.get(imgPath);
                            if(drawable == null) {
                                try{
                                    InputStream inputStream = context.getApplicationContext().getAssets().open(imgPath) ;
                                    drawable = Drawable.createFromStream(inputStream, null);
                                    cacheKey = imgPath;
                                    inputStream.close();
                                }
                                catch(Exception err) {
                                    duiCallback.getLogger().e("IMG_ERR", "Couldn't read from assets");
                                }
                            }
                        }
                        else if(imgAttr[1].contains("res/")){   // res/drawable/abc.png
                            String[] pathArr = imgAttr[1].split("/");
                            String imgName = pathArr[pathArr.length-1].split("\\.")[0];
                            String imgPath = null;
                            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                                imgPath = TextUtils.join("/" , Arrays.copyOfRange(pathArr, 1, pathArr.length-1));
                            }
                            else{
                                imgPath = TextUtils.join("/" ,Arrays.copyOfRange(pathArr, 1, pathArr.length-1));
                            }
                            Integer resId = context.getResources().getIdentifier(imgName, imgPath, context.getPackageName());
                            drawable = drawableCache.get(imgName);
                            if(drawable == null){
                                drawable = context.getResources().getDrawable(resId);
                                cacheKey=imgName;
                            }
                        }
                        break ;
                    case "resId" :
                        drawable = drawableCache.get(imgAttr[1]);
                        if(drawable == null) {
                            Integer resId = Integer.parseInt(imgAttr[1]);
                            drawable = context.getResources().getDrawable(resId);
                            cacheKey=imgAttr[1];
                        }
                        break ;
                    case "url" :    //the image will be downloaded and cached if not present in bitmapcache
                        String imageUrl = imgAttr[1] ;
                        Bitmap bitmap = bitmapCache.get(imageUrl) ;
                        if(bitmap != null){
                            imageView.setImageBitmap(bitmap);
                        }
                        else {
                            if(placeholder !=null){
                                Drawable tempDrawable = context.getResources().getDrawable(placeholder);
                                imageView.setImageDrawable(tempDrawable);
                            }
                            if(!imageRetryCountMap.contains(index)){
                                imageRetryCountMap.put(index, maxRetryImageTaskCount);
                            }
                            imageDownloadMap.put(index, imageDownloadMap.containsKey(index) ? imageDownloadMap.get(index) + 1 : 1);
                            new DownloadImageTask(this, placeholder, context.getApplicationContext(), bitmapCache , duiCallback, index, imageDownloadMap, imageRetryCountMap).execute(imageUrl) ;
                        }
                        break ;
                }
            }
            if(drawable != null){
                imageView.setImageDrawable(drawable);
                if(!cacheKey.isEmpty()){
                    drawableCache.put(cacheKey, drawable);
                }
            }
        } catch (Exception err) {
            duiCallback.getLogger().e("IMG_ERR", "Unable to set drawable, input error");
        }
    }

    private void setFontStyle(View view, String value){
        if(!(view instanceof TextView)) {
            return;
        }
        try{
            final TextView textView = (TextView) view ;
            boolean isValidFontString = value.contains(",");
            Integer weight = null ;
            weight = typefaceWeightCache.get(value);
            Typeface typeface = null ;
            typeface = typefaceCache.get(value);
            if (typeface != null) {
                if (textView.getTypeface() != typeface) {
                    if(weight != null) textView.setTypeface(typeface, weight);
                    else textView.setTypeface(typeface);
                }
                return;
            }
            else if(isValidFontString){
                // Font stringified object
                String[] arr = value.split(",");
                if(arr.length != 2){
                    duiCallback.getLogger().e("FONT_ERROR","incorrect font format recieved");
                    return;
                }
                String type = arr[0], fontValue=arr[1] ;
                switch(type){
                    case "path" :
                        if(fontValue.contains("assets/")){
                            String fontPath = fontValue.replace("assets/", "");
                            typeface = Typeface.createFromAsset(context.getAssets(), fontPath);
                        }
                        else if(fontValue.contains("res/")){
                            String[] fontTempArr = fontValue.split("/");
                            String fontResName = fontTempArr[fontTempArr.length - 1].split("\\.")[0];
                            Integer resID = context.getResources().getIdentifier(fontResName, "font", context.getPackageName());
                            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                                typeface = context.getResources().getFont(resID);
                            }
                            else{
                                Context _context = context.getApplicationContext();
                                typeface = ResourcesCompat.getFont(_context, resID);
                            }
                        }
                        break;
                    case "resId" :
                        Integer resID = Integer.parseInt(fontValue);
                        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                            typeface = context.getResources().getFont(resID);
                        }
                        else{
                            Context _context = context.getApplicationContext();
                            typeface = ResourcesCompat.getFont(_context, resID);
                        }
                        break;
                    case "default" :
                        switch(fontValue){
                            case "regular" :
                                weight = Typeface.NORMAL;
                                typeface = Typeface.create("sans-serif", weight);
                                break;
                            case "bold" :
                                weight = Typeface.BOLD;
                                typeface = Typeface.create("sans-serif", weight);
                                break;
                            case "semiBold" :
                                weight = Typeface.NORMAL;
                                typeface = Typeface.create("sans-serif-medium", weight);
                                break;
                        }
                        break;
                }
            }
            else{
                // for fontStyle prop -> fontName only
                value = value == null ? "" : value;
                typeface = Typeface.createFromAsset(context.getAssets(), "fonts/" + value + ".ttf");
            }
            typefaceCache.put(value, typeface);
            if(weight != null){
                typefaceWeightCache.put(value, weight);
                textView.setTypeface(typeface, weight);
            }
            else textView.setTypeface(typeface);
        }
        catch(Exception e){
            duiCallback.getLogger().e("FONT_ERROR",e.toString());
        }
    }

    private void setVisibility(View view, String value) {
        view.setVisibility(value.equalsIgnoreCase("gone")
                ? View.GONE
                : value.equalsIgnoreCase("invisible")
                ? View.INVISIBLE : View.VISIBLE);
    }

    private void setTextSize(View view, String value) {
        if (!(view instanceof TextView)) {
            return;
        }
        float size = Integer.parseInt(value) * density;
        float setSize = ((TextView) view).getTextSize();
        if (setSize != size) {
            ((TextView) view).setTextSize(TypedValue.COMPLEX_UNIT_PX, size);
        }
    }

    private void setPackageIcon(View view, String packageName) throws PackageManager.NameNotFoundException {
        ImageView img = (ImageView) view;
        PackageManager pm = this.context.getPackageManager();
        Drawable drawable = pm.getApplicationInfo(packageName, 0).loadIcon(pm);
        img.setImageDrawable(drawable);
    }

    private void setAlpha(View view, String value) {
        view.setAlpha(Float.parseFloat(value));
    }

    private void setClickListener(View view, final String value, final int index){
        view.setOnClickListener( new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                duiCallback.addJsToWebView("window.callUICallback('" + value + "',"+index+");");
            }
        });
    }

     private void setCornerRadius(View view, String corners) {
        if (corners != null) {
            String[] values = corners.split(",");
            float[] cornerRadius = new float[8];
            GradientDrawable gd = null;
            if (values.length > 0) {
                try {
                    if (values.length == 1) {
                        cornerRadius[0] = Float.parseFloat(values[0]);
                    } else {
                        cornerRadius = getCorners(values);
                    }
                } catch (Exception ignored) {
                    return;
                }
                Drawable background = view.getBackground();
                if (background instanceof ColorDrawable) {
                    gd = new GradientDrawable();
                    gd.setColor(((ColorDrawable) background).getColor());
                    view.setBackground(gd);
                } else if (background instanceof GradientDrawable) {
                    gd = (GradientDrawable) background;
                }
                if (gd != null) {
                    if (values.length == 1) {
                        gd.setCornerRadius(cornerRadius[0]);
                    } else {
                        gd.setCornerRadii(cornerRadius);
                    }
                }
            }
        }
    }

    private float[] getCorners (String[] values) {
        float[] corners = new float[8];
        float radius = Float.parseFloat(values[0]);
        int index = 0;
        for (int i =1;i< values.length ;i++){
            if (Boolean.parseBoolean(values[i])) {
                corners[index++] = radius;
                corners[index++] = radius;
            } else {
                index+=2;
            }
        }
        return corners;
    }

    private String getDefault(String key, String value){
        if(key.equals("onClick")){
            return value;
        }
        return null;
    }

    private void applyUpdate(View view, JSONObject holderProperties, JSONObject data, int index) {
        Iterator<String> keys = holderProperties.keys();
        while (keys.hasNext()) {
            String key = keys.next();
            String value = getString(data, getString(holderProperties, key, ""), getDefault(key, getString(holderProperties, key, "")));
            try {
                switch (key) {
                    case "background":
                        setBackground(view, value);
                        break;
                    case "text":
                        setText(view, value);
                        break;
                    case "color":
                        setTextColor(view, value);
                        break;
                    case "imageUrl":
                        setImage(view, value, index);
                        break;
                    case "visibility":
                        setVisibility(view, value);
                        break;
                    case "fontStyle":
                        setFontStyle(view, value);
                        break;
                    case "textSize":
                        setTextSize(view, value);
                        break;
                    case "packageIcon":
                        setPackageIcon(view, value);
                        break;
                    case "alpha":
                        setAlpha(view, value);
                        break;
                    case "cornerRadius":
                        setCornerRadius(view, value);
                        break;
                    case "onClick":
                        setClickListener(view, value, index);
                        break;
                    default:
                        try {
                            JSONObject props = new JSONObject();
                            props.put(key, value);
                            if(duiCallback.getInflateView() != null && value != null) {
                                duiCallback.getInflateView().putInState("view", view);
                                duiCallback.getInflateView().parseKeys(key, props, view, false);
                            }
                        } catch (Exception e) {
                            duiCallback.getLogger().e("Error while adding properties to list item", e.toString());
                        }
                }
            } catch (Exception e) {
                // IGNORED
            }
        }
    }

    private void updateView(View view, int pos) throws Exception {
        if (view.getTag() == null) {
            return;
        }
        Holder holder = (Holder) view.getTag();
        for (int i = 0; i < holder.views.length; i++) {
            if (holder.views[i] == null) {
                continue;
            }
            View child = holder.views[i];
            JSONObject data = rowData.getJSONObject(pos);
            JSONObject holderObj = holderData.getJSONObject(i);
            applyUpdate(child, holderObj, data, pos);
        }
    }

    public void updateRowData(JSONArray rowData) {
        imageRetryCountMap.clear();
        this.rowData = rowData;
    }
    public void setMaxRetryImageTaskCount(int count){
        this.maxRetryImageTaskCount = count;
    }

    @Override
    public View getView(int pos, View view, ViewGroup viewGroup) {
        if (view == null) {
            view = createView();
            if (view == null) {
                return new View(context);
            }
            view.setTag(new Holder(view));
        }
        try {
            updateView(view, pos);
        } catch (Exception e) {
            // IGNORED
        }
        return view;
    }

    @Override
    public int getCount() {
        return rowData.length();
    }

    @Override
    public Object getItem(int i) {
        return null;
    }

    @Override
    public long getItemId(int i) {
        return 0;
    }

    @Override
    public void notifyItemChanged(int position) {
        super.notifyDataSetChanged();
    }


    class Holder {
        View[] views;

        Holder(View view) {
            views = new View[holderData.length()];
            for (int i = 0; i < holderData.length(); i++) {
                try {
                    JSONObject object = holderData.getJSONObject(i);
                    views[i] = view.findViewById(object.getInt("id"));
                } catch (JSONException ignored) {
                }
            }
        }
    }
}
