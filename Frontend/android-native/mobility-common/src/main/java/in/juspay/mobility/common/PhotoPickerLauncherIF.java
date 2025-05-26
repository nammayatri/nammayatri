package in.juspay.mobility.common;

import android.net.Uri;

import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.PickVisualMediaRequest;

public abstract class PhotoPickerLauncherIF {
    public Uri cameraImageUri;
    public abstract ActivityResultLauncher<PickVisualMediaRequest>  getPhotoPickerLauncher();
    public abstract ActivityResultLauncher<Uri>  getCameraLauncher();
}
