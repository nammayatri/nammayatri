package `in`.juspay.mobility.common

import android.Manifest
import android.app.AlertDialog
import android.content.DialogInterface
import android.content.pm.PackageManager
import android.net.Uri
import androidx.activity.result.PickVisualMediaRequest
import androidx.activity.result.contract.ActivityResultContracts.PickVisualMedia.ImageOnly
import androidx.core.app.ActivityCompat
import androidx.core.content.ContextCompat
import androidx.core.content.FileProvider
import `in`.juspay.hyper.core.BridgeComponents
import `in`.juspay.hyper.core.ExecutorManager
import java.io.File


open class BaseMediaPlayer(var bridgeComponents: BridgeComponents) {
    val REQUEST_CAMERA_PERMISSION_CODE = 1001;
    fun uploadFile() {
        ExecutorManager.runOnMainThread {
            val builder: AlertDialog.Builder = AlertDialog.Builder(bridgeComponents.activity)
            builder.setTitle("Select Image")
                .setItems(
                    arrayOf<CharSequence>("Take Photo", "Choose from Gallery")
                ) { dialog: DialogInterface?, which: Int ->

                    if (which == 0) {
                        if (ContextCompat.checkSelfPermission(bridgeComponents.context, Manifest.permission.CAMERA)
                            != PackageManager.PERMISSION_GRANTED) {
                            if (bridgeComponents.activity != null) {
                                ActivityCompat.requestPermissions(
                                    bridgeComponents.activity!!,
                                    arrayOf(Manifest.permission.CAMERA),
                                    REQUEST_CAMERA_PERMISSION_CODE
                                );
                            }
                        }else{
                            // Take Photo
                            MobilityCommonBridge.photoPickerLauncherIF.cameraImageUri =
                                FileProvider.getUriForFile(
                                    bridgeComponents.context,
                                    bridgeComponents.context.getPackageName() + ".provider",
                                    File(
                                        bridgeComponents.context.getFilesDir(),
                                        "IMG_" + System.currentTimeMillis() + ".jpg"
                                    )
                                )
                            MobilityCommonBridge.photoPickerLauncherIF.cameraLauncher.launch(
                                MobilityCommonBridge.photoPickerLauncherIF.cameraImageUri
                            )
                        }
                    } else {
                        // Choose from Gallery
                        MobilityCommonBridge.photoPickerLauncherIF.photoPickerLauncher.launch(
                            PickVisualMediaRequest.Builder()
                                .setMediaType(ImageOnly)
                                .build()
                        )
                    }
                }.show()

        }
    }
}