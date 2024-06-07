package in.juspay.mobility.app;

import android.Manifest;
import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.ContentValues;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.database.Cursor;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Matrix;
import android.net.Uri;
import android.os.Environment;
import android.provider.MediaStore;
import android.util.Base64;
import android.util.Log;
import android.util.Size;
import android.view.ViewGroup;
import android.webkit.JavascriptInterface;
import android.widget.LinearLayout;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.camera.core.CameraSelector;
import androidx.camera.core.Preview;
import androidx.camera.core.VideoCapture;
import androidx.camera.core.ImageCapture;
import androidx.camera.core.ImageCaptureException;
import androidx.camera.lifecycle.ProcessCameraProvider;
import androidx.camera.view.PreviewView;
import androidx.core.app.ActivityCompat;
import androidx.core.content.ContextCompat;
import androidx.core.content.FileProvider;
import androidx.exifinterface.media.ExifInterface;
import androidx.lifecycle.LifecycleOwner;

import com.google.common.util.concurrent.ListenableFuture;

import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.io.File;
import java.util.Date;
import java.util.Locale;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executor;
import in.juspay.hypersdk.data.KeyValueStore;

import in.juspay.hyper.core.BridgeComponents;
import in.juspay.hyper.core.ExecutorManager;

public class CameraUtils {

    private static final String CAMERA_UTILS = "CAMERA_UTILS";
    PreviewView previewView;
    private VideoCapture videoCapture;
    private ImageCapture imageCapture;

    private Executor getExecutor(Activity activity) {
        return ContextCompat.getMainExecutor(activity);
    }

    @SuppressLint("RestrictedApi")
    private void startCameraX(Activity activity, ProcessCameraProvider cameraProvider, CameraSelector cameraSelector) {
        cameraProvider.unbindAll();
        if (activity == null) return;

        Preview preview = new Preview.Builder().build();

        preview.setSurfaceProvider(previewView.getSurfaceProvider());

        imageCapture = new ImageCapture.Builder()
                .setTargetResolution(new Size(480, 640))
                .build();

        videoCapture = new VideoCapture.Builder()
                .setTargetResolution(new Size(480, 640))
                .setBitRate(500000)
                .setVideoFrameRate(15)
                .setMaxResolution(new Size(480, 640))
                .build();

        if (activity != null)
            cameraProvider.bindToLifecycle((LifecycleOwner) activity, cameraSelector, preview, videoCapture, imageCapture);

    }

    public void setupCamera(Activity activity, Context context, String previewViewId, boolean isBackCamera) {
        ExecutorManager.runOnMainThread(new Runnable() {
            @Override
            public void run() {
                PermissionUtils.askRequestedPermissions(activity, context, new String[]{Manifest.permission.CAMERA, Manifest.permission.RECORD_AUDIO}, null);

                if(activity != null) {
                    int viewId = Integer.parseInt(previewViewId);
                    LinearLayout parentView = activity.findViewById(viewId);
                    if (parentView == null) return;
                    parentView.removeAllViews();
                    previewView = null;

                    if (previewView == null) {
                        previewView = new PreviewView(context.getApplicationContext());
                        previewView.setImplementationMode(PreviewView.ImplementationMode.COMPATIBLE);
                        previewView.setLayoutParams(new ViewGroup.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.MATCH_PARENT));

                        parentView.addView(previewView);
                        parentView.bringChildToFront(previewView);
                    }
                    if (previewView == null) return;

                    ListenableFuture<ProcessCameraProvider> cameraProviderFuture = ProcessCameraProvider.getInstance(context);
                    cameraProviderFuture.addListener(() -> {
                        try {
                            ProcessCameraProvider cameraProvider = cameraProviderFuture.get();
                            CameraSelector cameraSelector = isBackCamera ? CameraSelector.DEFAULT_BACK_CAMERA : CameraSelector.DEFAULT_FRONT_CAMERA;
                            startCameraX(activity, cameraProvider, cameraSelector);
                        } catch (ExecutionException | InterruptedException e) {
                            e.printStackTrace();
                            return;
                        }
                    }, ContextCompat.getMainExecutor(activity));
                }

            }
        });
    }

    @SuppressLint("RestrictedApi")
    public void recordVideo(Activity activity, Context context, final String callback, BridgeComponents bridgeComponents) {
        if (videoCapture != null) {
            long timeStamp = System.currentTimeMillis();
            ContentValues contentValues = new ContentValues();
            contentValues.put(MediaStore.MediaColumns.DISPLAY_NAME, timeStamp);
            contentValues.put(MediaStore.MediaColumns.MIME_TYPE, "video/mp4");

            PermissionUtils.askRequestedPermissions(activity, context, new String[]{Manifest.permission.CAMERA, Manifest.permission.RECORD_AUDIO}, null);

            try {
                if (ActivityCompat.checkSelfPermission(context, Manifest.permission.RECORD_AUDIO) != PackageManager.PERMISSION_GRANTED) {
                    return;
                }
                videoCapture.startRecording(
                        new VideoCapture.OutputFileOptions.Builder(
                                activity.getContentResolver(),
                                MediaStore.Video.Media.EXTERNAL_CONTENT_URI,
                                contentValues
                        ).build(),
                        getExecutor(activity),
                        new VideoCapture.OnVideoSavedCallback() {
                            @Override
                            public void onVideoSaved(@NonNull VideoCapture.OutputFileResults outputFileResults) {
                                Uri videoUri = outputFileResults.getSavedUri();
                                Log.d(CAMERA_UTILS, "videoUri -> " + videoUri);
                                if (videoUri != null) {
                                    Cursor cursor = context.getContentResolver().query(videoUri, new String[]{MediaStore.Video.VideoColumns.DATA}, null, null, null);
                                    cursor.moveToFirst();
                                    String filePath = cursor.getString(0);
                                    cursor.close();
                                    String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s','%s');", callback, "VIDEO_RECORDED", filePath);
                                    bridgeComponents.getJsCallback().addJsToWebView(javascript);
                                }
                            }

                            @Override
                            public void onError(int videoCaptureError, @NonNull String message, @Nullable Throwable cause) {
                                Toast.makeText(activity, "Error: " + message, Toast.LENGTH_SHORT).show();
                            }
                        }
                );
            } catch (Exception e){
                Log.e(CAMERA_UTILS, "Error in starting the recording :" + e);
            }
        }
    }

    @SuppressLint("RestrictedApi")
    public void stopRecord() {
        if(videoCapture!=null)
            videoCapture.stopRecording();
    }

    @SuppressLint("RestrictedApi")
    public void takePhoto(Activity activity, Context context, final String callback, BridgeComponents bridgeComponents) {
        if (imageCapture != null) {
            String timeStamp = new SimpleDateFormat("yyyyMMdd_HHmmss", Locale.getDefault()).format(new Date());
            KeyValueStore.write(context, bridgeComponents.getSdkName(), context.getResources().getString(R.string.TIME_STAMP_FILE_UPLOAD), timeStamp);
            File photoFile = new File(context.getFilesDir(), "IMG_" + timeStamp + ".jpg");
            Uri photoUri = FileProvider.getUriForFile(context, context.getPackageName() + ".provider", photoFile);

            ImageCapture.OutputFileOptions outputFileOptions = new ImageCapture.OutputFileOptions.Builder(photoFile).build();

            imageCapture.takePicture(
                    outputFileOptions,
                    getExecutor(activity),
                    new ImageCapture.OnImageSavedCallback() {
                        @Override
                        public void onImageSaved(@NonNull ImageCapture.OutputFileResults outputFileResults) {
                            try {
                                String base64Image = encodeFileToBase64(photoFile);
                                String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s','%s');", callback, photoUri, base64Image );
                                bridgeComponents.getJsCallback().addJsToWebView(javascript);
                            } catch (IOException e) {
                                Log.e(CAMERA_UTILS, "Failed to encode image to base64: " + e.getMessage(), e);
                                Toast.makeText(activity, "Error encoding image to base64", Toast.LENGTH_SHORT).show();
                            }
                        }

                        @Override
                        public void onError(@NonNull ImageCaptureException exception) {
                            Log.e(CAMERA_UTILS, "Photo capture failed: " + exception.getMessage(), exception);
                            Toast.makeText(activity, "Error: " + exception.getMessage(), Toast.LENGTH_SHORT).show();
                        }
                    }
            );
        } else {
            Log.e(CAMERA_UTILS, "ImageCapture is not initialized.");
            Toast.makeText(activity, "Camera is not ready. Please try again.", Toast.LENGTH_SHORT).show();
        }
    }

    private String encodeFileToBase64(File file) throws IOException {
        Bitmap bitmap = BitmapFactory.decodeFile(file.getAbsolutePath());
        bitmap = handleImageRotation(file.getAbsolutePath(), bitmap);

        ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
        bitmap.compress(Bitmap.CompressFormat.JPEG, 100, byteArrayOutputStream);
        byte[] byteArray = byteArrayOutputStream.toByteArray();

        return Base64.encodeToString(byteArray, Base64.NO_WRAP);
    }

    private Bitmap handleImageRotation(String imagePath, Bitmap bitmap) throws IOException {
        ExifInterface exif = new ExifInterface(imagePath);
        int orientation = exif.getAttributeInt(ExifInterface.TAG_ORIENTATION, ExifInterface.ORIENTATION_UNDEFINED);

        int rotation = 0;
        switch (orientation) {
            case ExifInterface.ORIENTATION_ROTATE_90:
                rotation = 90;
                break;
            case ExifInterface.ORIENTATION_ROTATE_180:
                rotation = 180;
                break;
            case ExifInterface.ORIENTATION_ROTATE_270:
                rotation = 270;
                break;
            default:
                break;
        }

        Matrix matrix = new Matrix();
        if (rotation != 0) {
            matrix.postRotate(rotation);
        }
        matrix.postScale(-1, 1); // Horizontal flip
        return Bitmap.createBitmap(bitmap, 0, 0, bitmap.getWidth(), bitmap.getHeight(), matrix, true);
    }
    @SuppressLint("RestrictedApi")
    public void stopCamera(Activity activity) {
        if (activity == null) return;

        ListenableFuture<ProcessCameraProvider> cameraProviderFuture = ProcessCameraProvider.getInstance(activity);
        cameraProviderFuture.addListener(() -> {
            try {
                ProcessCameraProvider cameraProvider = cameraProviderFuture.get();
                cameraProvider.unbindAll();
            } catch (ExecutionException | InterruptedException e) {
                e.printStackTrace();
            }
        }, ContextCompat.getMainExecutor(activity));
    }
}

