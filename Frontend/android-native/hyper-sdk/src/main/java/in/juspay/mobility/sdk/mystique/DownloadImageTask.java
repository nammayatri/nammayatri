package in.juspay.mobility.sdk.mystique;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.os.AsyncTask;
import android.text.Html;

import java.lang.ref.WeakReference;
import java.net.URL;
import java.util.Hashtable;

import javax.net.ssl.HttpsURLConnection;

import in.juspay.mobility.sdk.core.DuiCallback;

public class DownloadImageTask extends AsyncTask<String, Void, Bitmap> {
    private final DuiCallback duiCallback;
    private String imageUrl;
    private final BitmapCache bitmapCache;
    private final Integer placeHolder;
    private final WeakReference<Context> contextWeakReference;
    private final Adapter adapter;
    Hashtable<Integer, Integer> lastImageMap;

    private final int viewIndex;
    private final Hashtable <Integer,Integer> imageRetryCountMap;
    public DownloadImageTask(Adapter adapter, Integer placeHolder, Context context, BitmapCache bitmapCache, DuiCallback duiCallback, int viewIndex, Hashtable<Integer, Integer> lastImageMap, Hashtable<Integer, Integer> imageRetryCountMap) {
        this.adapter = adapter;
        this.placeHolder = placeHolder ;
        this.contextWeakReference = new WeakReference<>(context);
        this.bitmapCache = bitmapCache ;
        this.duiCallback = duiCallback ;
        this.viewIndex = viewIndex;
        this.lastImageMap = lastImageMap;
        this.imageRetryCountMap = imageRetryCountMap;
    }

    @Override
    protected Bitmap doInBackground(String... params) {
        imageUrl = params[0];
        return getImage(imageUrl);
    }

    @Override
    protected void onPostExecute(Bitmap result) {
        super.onPostExecute(result);
        if (result != null) {
            bitmapCache.put(imageUrl, result);
        }
        checkAndNotify();
    }

    private Bitmap getImage(String imageUrl) {

        if(bitmapCache.get(imageUrl) == null) {
            HttpsURLConnection connection = null ;
            try {
                URL url = new URL(Html.fromHtml(imageUrl).toString());
                return BitmapFactory.decodeStream(url.openConnection().getInputStream());
            } catch (Exception error) {
                try {
                    Context context = contextWeakReference.get();
                    if (context != null) {
                        Bitmap icon = BitmapFactory.decodeResource(context.getResources(), placeHolder);
                        return icon;
                    }
                } catch (Exception err) {
                    duiCallback.getLogger().e("IMG_ERR", "Not able to apply placeholder");
                }
            } finally {
                if (connection != null) {
                    connection.disconnect();
                }
            }
        }
        return null;
    }

    private synchronized void checkAndNotify(){
        lastImageMap.put(viewIndex, lastImageMap.containsKey(viewIndex) ? lastImageMap.get(viewIndex) - 1 :  -1);

        if (adapter != null && lastImageMap.get(viewIndex) == 0 && imageRetryCountMap.containsKey(viewIndex) && imageRetryCountMap.get(viewIndex) > 0){
            imageRetryCountMap.put(viewIndex, imageRetryCountMap.containsKey(viewIndex) ? imageRetryCountMap.get(viewIndex)-1 : 0);
            adapter.notifyItemChanged(viewIndex);
        }
        else{
            duiCallback.getLogger().e("IMG_ERR", "Fetching image from url failed. Null adapter passed");
        }
    }



    public interface Adapter {
        void notifyItemChanged(int position);
    }
}
