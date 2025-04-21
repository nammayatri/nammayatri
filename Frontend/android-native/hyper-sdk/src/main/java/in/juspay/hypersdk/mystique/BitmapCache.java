package in.juspay.hypersdk.mystique;

import android.graphics.Bitmap;
import android.util.LruCache;

public class BitmapCache {
    private LruCache<String, Bitmap> bitmapStore ;
    private static BitmapCache bitmapCache ;
    private BitmapCache(){
        initializeCache(50);
    }

    public void initializeCache(int size){
        bitmapStore = new LruCache<String, Bitmap>(size) ;
    }
    public static BitmapCache getInstance(){
        if(bitmapCache == null){
            bitmapCache = new BitmapCache();
        }
        return bitmapCache ;
    }
    public void put(String url, Bitmap bitmap){
        this.bitmapStore.put(url, bitmap);
    }
    public Bitmap get(String url){
        return this.bitmapStore.get(url);
    }
    public void clear(){
        this.bitmapStore.evictAll();
    }
    public void getSize(){
        this.bitmapStore.size();
    }
}
