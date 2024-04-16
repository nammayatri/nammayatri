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

    private ArrayList<String> urls = new ArrayList<>();
    private String url = null;
    private Runnable postRunnable;
    private Handler mainHandler;
//    private Context context;
    private BridgeComponents bridgeComponents;

    private NetworkTaskManager(ArrayList<String> url, BridgeComponents bridgeComponents) {
        this.urls = url;
        this.mainHandler = new Handler(Looper.getMainLooper());
        this.bridgeComponents = bridgeComponents;
    }

    private NetworkTaskManager(String url) {
        this.url = url;
        this.mainHandler = new Handler(Looper.getMainLooper());;
    }

//    private NetworkTaskManager(Context context) {
//        this.context = context;
////        this.mainHandler = new Handler(Looper.getMainLooper());;
//    }

    public static NetworkTaskManager load(ArrayList<String> url, BridgeComponents bridgeComponents) {
        return new NetworkTaskManager(url, bridgeComponents);
    }

    public static NetworkTaskManager load(String url) {
        return new NetworkTaskManager(url);
    }

    public NetworkTaskManager post(Runnable runnable) {
        this.postRunnable = runnable;
        return this;
    }

//    public static NetworkTaskManager with(Context context) {
//        return new NetworkTaskManager(context);
//        this.context = context;
//        return this;
//    }

    public void into(ImageView imageView) {
        if (url != null) {
            new Thread(() -> {
                loadAndStoreImageFromUrl(this.urls);
                if (postRunnable != null) {
                    mainHandler.post(() -> {
                        imageView.setImageBitmap(fetchImageFromInternalStorage(this.url));
                    });
                }
            }).start();
        }
    }

    public void into(Marker marker) {
        if (url != null) {
            new Thread(() -> {
                loadAndStoreImageFromUrl(this.urls);
                if (postRunnable != null) {
                    mainHandler.post(() -> {
                        marker.setIcon(BitmapDescriptorFactory.fromBitmap(fetchImageFromInternalStorage(this.url)));
//                        imageView.setImageBitmap(fetchImageFromInternalStorage(this.url));
                    });
                }
            }).start();
        }
    }

    public void execute() {
        try {
            new Thread(() -> {
                loadAndStoreImageFromUrl(this.urls);
                if (postRunnable != null) {
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
//        if (context != null) {
            try {
//            Context context = bridgeComponents.getContext();
                File directory = bridgeComponents.getContext().getDir("nammayatri", Context.MODE_PRIVATE);
                File file = new File(directory, image);
                Bitmap bitmap = BitmapFactory.decodeStream(new FileInputStream(file));
                return bitmap;
            } catch (Exception e) {
                e.printStackTrace();
            }
//        }
        return null;
    }

    public static Bitmap fetchImageFromInternalStorage(String image, BridgeComponents bridgeComponents) {
//        if (context != null) {
        try {
//            Context context = bridgeComponents.getContext();
            File directory = bridgeComponents.getContext().getDir("nammayatri", Context.MODE_PRIVATE);
            File file = new File(directory, image);
            Bitmap bitmap = BitmapFactory.decodeStream(new FileInputStream(file));
            return bitmap;
        } catch (Exception e) {
            e.printStackTrace();
        }
//        }
        return null;
    }

    public Bitmap fetchImageWithFallback(String image) {
//        if (context != null) {
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
//        }
        return null;
    }

    public static Bitmap fetchImageWithFallback(String image, BridgeComponents bridgeComponents) {
//        if (context != null) {
        try {
            Context context = bridgeComponents.getContext();
            String[] splitImage = image.split(",");
            String url = splitImage.length > 1 ? splitImage[1] : "";
            int lastIndex = url.lastIndexOf('/') + 1;
            String urlImageName = url.length() > lastIndex + 1 ? url.substring(lastIndex) : "";
            if (!urlImageName.equals("") && isImagePresent(urlImageName, bridgeComponents)) {
                return fetchImageFromInternalStorage(urlImageName, bridgeComponents);
            } else {
                String fallbackImageName = splitImage[0];
                int imageID = context.getResources().getIdentifier(fallbackImageName, "drawable", context.getPackageName());
                BitmapDrawable bitmapDrawable = (BitmapDrawable) context.getResources().getDrawable(imageID);
                return bitmapDrawable.getBitmap();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
//        }
        return null;
    }

    public boolean isImagePresent(String imageName) {
//        if (context != null) {
        Context context = bridgeComponents.getContext();
            File directory = context.getDir("nammayatri", Context.MODE_PRIVATE);
            File file = new File(directory, imageName);
            return file.exists();
//        }
//        return false;
    }

    public static boolean isImagePresent(String imageName, BridgeComponents bridgeComponents) {
//        if (context != null) {
        Context context = bridgeComponents.getContext();
        File directory = context.getDir("nammayatri", Context.MODE_PRIVATE);
        File file = new File(directory, imageName);
        return file.exists();
//        }
//        return false;
    }

    public void writeImageToInternalStorage(Bitmap bitmap, String imageName) {
        Context cxt = bridgeComponents.getContext();
        //        Context context = bridgeComponents.getContext();
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