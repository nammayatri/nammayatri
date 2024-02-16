package in.juspay.mobility.app;

import android.Manifest;
import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.ContentValues;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.database.Cursor;
import android.net.Uri;
import android.provider.MediaStore;
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
import androidx.camera.lifecycle.ProcessCameraProvider;
import androidx.camera.view.PreviewView;
import androidx.core.app.ActivityCompat;
import androidx.core.content.ContextCompat;
import androidx.core.content.FileProvider;
import androidx.lifecycle.LifecycleOwner;

import com.google.common.util.concurrent.ListenableFuture;

import java.io.File;
import java.util.Locale;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executor;

import in.juspay.hyper.core.BridgeComponents;
import in.juspay.hyper.core.ExecutorManager;

public class CameraUtils {

    private static final String CAMERA_UTILS = "CAMERA_UTILS";
    PreviewView previewView;
    private VideoCapture videoCapture;

    private Executor getExecutor(Activity activity) {
        return ContextCompat.getMainExecutor(activity);
    }

    @SuppressLint("RestrictedApi")
    private void startCameraX(Activity activity, ProcessCameraProvider cameraProvider, CameraSelector cameraSelector) {
        cameraProvider.unbindAll();
        if (activity == null) return;

        Preview preview = new Preview.Builder().build();

        preview.setSurfaceProvider(previewView.getSurfaceProvider());

        videoCapture = new VideoCapture.Builder()
                .setTargetResolution(new Size(480, 640))
                .setBitRate(500000)
                .setVideoFrameRate(15)
                .setMaxResolution(new Size(480, 640))
                .build();

        if (activity != null)
            cameraProvider.bindToLifecycle((LifecycleOwner) activity, cameraSelector, preview, videoCapture);

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


}
