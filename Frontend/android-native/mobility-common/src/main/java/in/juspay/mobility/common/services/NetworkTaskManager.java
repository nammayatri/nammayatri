package in.juspay.mobility.common.services;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.drawable.BitmapDrawable;
import android.os.Handler;
import android.os.Looper;
import android.webkit.URLUtil;
import android.widget.ImageView;

import androidx.annotation.NonNull;

import com.google.android.gms.maps.model.BitmapDescriptorFactory;
import com.google.android.gms.maps.model.Marker;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.ArrayList;

import in.juspay.hyper.core.BridgeComponents;

public class NetworkTaskManager {

    public ArrayList<String> urls = new ArrayList<>();
    private Runnable postRunnable;
    private Handler mainHandler;
    private BridgeComponents bridgeComponents;

    private NetworkTaskManager(BridgeComponents bridgeComponents) {
        this.bridgeComponents = bridgeComponents;
        this.mainHandler = new Handler(Looper.getMainLooper());
    }

    public static NetworkTaskManager with(BridgeComponents bridgeComponents) {
        return new NetworkTaskManager(bridgeComponents);
    }

    public NetworkTaskManager load(ArrayList<String> urls) {
        this.urls = urls;
        return this;
    }

    public NetworkTaskManager post(Runnable runnable) {
        this.postRunnable = runnable;
        return this;
    }

    public void execute() {
        try {
            ArrayList<String> images = this.urls;
            new Thread(() -> {
                loadAndStoreImageFromUrl(images);
                if (postRunnable != null) {
                    mainHandler = new Handler(Looper.getMainLooper());
                    mainHandler.post(postRunnable);
                }
            }).start();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void loadAndStoreImageFromUrl(ArrayList<String> images) {
        try {
            for (String image : images) {
                String[] splitImage = image.split(",");
                String url = splitImage.length > 1 ? splitImage[1] : "";
                if (URLUtil.isHttpsUrl(url)) {
                    int lastIndex = url.lastIndexOf('/') + 1;
                    String imageName = url.substring(lastIndex);
                    if (!isImagePresent(imageName)) {
                        URL imageUrl = new URL(url);
                        HttpURLConnection connection = (HttpURLConnection) imageUrl.openConnection();
                        connection.setDoInput(true);
                        connection.setConnectTimeout(500);
                        connection.setReadTimeout(500);
                        connection.connect();
                        InputStream input = connection.getInputStream();
                        Bitmap bitmap = BitmapFactory.decodeStream(input);
                        writeImageToInternalStorage(bitmap, imageName);
                    }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public Bitmap fetchImageFromInternalStorage(String image) {
            try {
                File directory = bridgeComponents.getContext().getDir("nammayatri", Context.MODE_PRIVATE);
                File file = new File(directory, image);
                Bitmap bitmap = BitmapFactory.decodeStream(new FileInputStream(file));
                return bitmap;
            } catch (Exception e) {
                e.printStackTrace();
            }
        return null;
    }

    public Bitmap fetchImageWithFallback(String image) {
            try {
                Context context = bridgeComponents.getContext();
                String[] splitImage = image.split(",");
                String url = splitImage.length > 1 ? splitImage[1] : "";
                int lastIndex = url.lastIndexOf('/') + 1;
                String urlImageName = url.length() > lastIndex + 1 ? url.substring(lastIndex) : "";
                if (!urlImageName.equals("") && isImagePresent(urlImageName)) {
                    return fetchImageFromInternalStorage(urlImageName);
                } else {
                    String fallbackImageName = splitImage[0];
                    int imageID = context.getResources().getIdentifier(fallbackImageName, "drawable", context.getPackageName());
                    BitmapDrawable bitmapDrawable = (BitmapDrawable) context.getResources().getDrawable(imageID);
                    return bitmapDrawable.getBitmap();
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        return null;
    }

    public boolean isImagePresent(String imageName) {
            Context context = bridgeComponents.getContext();
            File directory = context.getDir("nammayatri", Context.MODE_PRIVATE);
            File file = new File(directory, imageName);
            return file.exists();
    }

    public void writeImageToInternalStorage(Bitmap bitmap, String imageName) {
        Context cxt = bridgeComponents.getContext();
        File directory = cxt.getDir("nammayatri", Context.MODE_PRIVATE);
        File file = new File(directory, imageName);

        FileOutputStream fos = null;
        try {
            fos = new FileOutputStream(file);
            bitmap.compress(Bitmap.CompressFormat.PNG, 100, fos);
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            try {
                fos.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
}