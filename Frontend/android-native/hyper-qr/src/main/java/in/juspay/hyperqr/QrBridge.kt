package `in`.juspay.hyperqr

import android.Manifest
import android.content.Context
import android.content.Intent
import android.content.pm.PackageManager
import android.graphics.Bitmap
import android.graphics.BitmapFactory
import android.graphics.Canvas
import android.net.Uri
import android.os.Handler
import android.os.Message
import android.provider.MediaStore
import android.text.TextUtils
import android.util.Base64
import android.view.View
import android.view.ViewGroup
import android.webkit.JavascriptInterface
import android.widget.ImageView
import androidx.core.content.ContextCompat
import androidx.lifecycle.LifecycleOwner
import com.google.mlkit.vision.common.InputImage
import `in`.juspay.hyper.bridge.HyperBridge
import `in`.juspay.hyper.constants.Labels
import `in`.juspay.hyper.constants.LogCategory
import `in`.juspay.hyper.constants.LogLevel
import `in`.juspay.hyper.constants.LogSubCategory
import `in`.juspay.hyper.core.BridgeComponents
import `in`.juspay.hyper.core.ExecutorManager
import `in`.juspay.hyper.core.JuspayLogger
import `in`.juspay.hyper.core.ResultAwaitingDuiHook
import `in`.juspay.widget.qrscanner.com.google.zxing.BarcodeFormat
import `in`.juspay.widget.qrscanner.com.google.zxing.BinaryBitmap
import `in`.juspay.widget.qrscanner.com.google.zxing.DecodeHintType
import `in`.juspay.widget.qrscanner.com.google.zxing.EncodeHintType
import `in`.juspay.widget.qrscanner.com.google.zxing.LuminanceSource
import `in`.juspay.widget.qrscanner.com.google.zxing.MultiFormatReader
import `in`.juspay.widget.qrscanner.com.google.zxing.RGBLuminanceSource
import `in`.juspay.widget.qrscanner.com.google.zxing.Reader
import `in`.juspay.widget.qrscanner.com.google.zxing.ResultPoint
import `in`.juspay.widget.qrscanner.com.google.zxing.common.BitMatrix
import `in`.juspay.widget.qrscanner.com.google.zxing.common.HybridBinarizer
import `in`.juspay.widget.qrscanner.com.google.zxing.qrcode.QRCodeWriter
import `in`.juspay.widget.qrscanner.com.journeyapps.barcodescanner.BarcodeCallback
import `in`.juspay.widget.qrscanner.com.journeyapps.barcodescanner.BarcodeResult
import `in`.juspay.widget.qrscanner.com.journeyapps.barcodescanner.CaptureManager
import `in`.juspay.widget.qrscanner.com.journeyapps.barcodescanner.DecoratedBarcodeView
import org.json.JSONObject
import java.io.BufferedInputStream
import java.io.FileNotFoundException
import java.io.IOException
import java.io.InputStream
import java.net.URLEncoder

class QrBridge(bridgeComponents: BridgeComponents) : HyperBridge(bridgeComponents) {
    private var captureManager: CaptureManager? = null
    private val listenerMap: MutableMap<String, Any> = hashMapOf()
    private var firebaseScanner: CameraStreamScanner? = null
    private var imageScanner: InputImageScanner? = null

//    @Suppress("unused")
//    private val imageFromGallery: Unit
//        get() {
//            val photoPickerIntent = Intent(Intent.ACTION_PICK)
//            photoPickerIntent.setType("image/*")
//            bridgeComponents.fragmentHooks
//                .startActivityForResult(photoPickerIntent, SELECT_PHOTO, null)
//        }

    @JavascriptInterface
    fun scanQRFromGallery(callback: String?) {
        listenerMap[GALLERY] =
            ResultAwaitingDuiHook { requestCode: Int, _: Int, data: Intent? ->
                try {
                    if (data == null) {
                        bridgeComponents.callbackInvoker.invokeCallbackInDUIWebview(
                            callback,
                            String.format(
                                "{\"error\":\"true\",\"data\":\"%s\"}",
                                Base64.encodeToString(
                                    "NO IMAGE SELECTED".toByteArray(),
                                    Base64.NO_WRAP
                                )
                            )
                        )
                        return@ResultAwaitingDuiHook false
                    } else if (GALLERY_KITKAT_INTENT_CALLED != requestCode && SELECT_PHOTO != requestCode) {
                        return@ResultAwaitingDuiHook false
                    }
                    if (checkFirebaseScanner()) {
                        try {
                            val uri = data.data
                            if (uri != null) {
                                val image = InputImage.fromFilePath(bridgeComponents.context, uri)
                                if (image != null) {
                                    imageScanner = InputImageScanner(successCallback = {
                                        bridgeComponents.callbackInvoker.invokeCallbackInDUIWebview(callback, makeSuccessData(it))
                                    }, failureCallback = {
                                        bridgeComponents.callbackInvoker.invokeCallbackInDUIWebview(callback, makeFailureData(it))
                                    })
                                    imageScanner?.scanImage(image)
                                } else {
                                    bridgeComponents.callbackInvoker.invokeCallbackInDUIWebview(callback, makeFailureData("can't find image at uri: $uri"))
                                }
                            } else {
                                bridgeComponents.callbackInvoker.invokeCallbackInDUIWebview(callback, makeFailureData("Selected image uri is null"))
                            }
                        } catch (e: IOException) {
                            e.printStackTrace()
                            return@ResultAwaitingDuiHook false
                        }
                        return@ResultAwaitingDuiHook true
                    }
                    val inputStream: InputStream? = try {
                        data.data?.let {
                            bridgeComponents.context.contentResolver
                                .openInputStream(it)
                        }
                    } catch (e: FileNotFoundException) {
                        JuspayLogger.d(LOG_TAG, "Exception in scanning the QR code :$e")
                        null
                    }
                    val bufferedInputStream = BufferedInputStream(inputStream)
                    val bMap = BitmapFactory.decodeStream(bufferedInputStream)
                    readQRFromBitmap(bMap, callback)
                    listenerMap.remove(GALLERY)
                    return@ResultAwaitingDuiHook true
                } catch (e: Exception) {
                    JuspayLogger.d(LOG_TAG, "Exception in scanning the QR code :$e")
                    return@ResultAwaitingDuiHook false
                }
            }
        val intent = Intent(Intent.ACTION_OPEN_DOCUMENT)
        intent.addCategory(Intent.CATEGORY_OPENABLE)
        intent.setType("image/*")
        bridgeComponents.fragmentHooks
            .startActivityForResult(intent, GALLERY_KITKAT_INTENT_CALLED, null)
    }

    private fun readQRFromBitmap(bMap: Bitmap, callback: String?) {
        try {
            val intArray = IntArray(bMap.width * bMap.height)
            bMap.getPixels(intArray, 0, bMap.width, 0, 0, bMap.width, bMap.height)
            val source: LuminanceSource = RGBLuminanceSource(bMap.width, bMap.height, intArray)
            val bitmap = BinaryBitmap(HybridBinarizer(source))
            val reader: Reader = MultiFormatReader()
            val decodeHints = mapOf(
                DecodeHintType.TRY_HARDER to true,
                DecodeHintType.ALSO_INVERTED to true
            )
            val result = reader.decode(bitmap, decodeHints)
            val barcode = result.text
            JuspayLogger.d(LOG_TAG, "Scanned QR Result: $barcode")
            bridgeComponents.callbackInvoker
                .invokeCallbackInDUIWebview(callback, makeSuccessData(URLEncoder.encode(barcode, "UTF-8").replace("+", "%20")))
        } catch (e: Exception) {
            bridgeComponents.callbackInvoker.invokeCallbackInDUIWebview(
                callback,
                String.format(
                    "{\"error\":\"true\",\"data\":\"%s\"}",
                    Base64.encodeToString(
                        "unknown_error::$e".toByteArray(),
                        Base64.NO_WRAP
                    )
                )
            )
        }
    }

    @JavascriptInterface
    fun shareImage(viewId: Int, subject: String?, text: String?, intentHeading: String?) {
        val activity = bridgeComponents.activity
        if (activity != null) {
            try {
                val bitmap = screenShot(activity.findViewById(viewId))
                val pathOfBmp = MediaStore.Images.Media.insertImage(
                    bridgeComponents.context.contentResolver,
                    bitmap,
                    text,
                    subject
                )
                val uri = Uri.parse(pathOfBmp)
                val shareIntent = Intent(Intent.ACTION_SEND)
                shareIntent.setType("image/*")
                shareIntent.putExtra(Intent.EXTRA_SUBJECT, subject)
                shareIntent.putExtra(Intent.EXTRA_TEXT, text)
                shareIntent.putExtra(Intent.EXTRA_STREAM, uri)
                bridgeComponents.fragmentHooks.startActivityForResult(
                    Intent.createChooser(shareIntent, intentHeading),
                    -1,
                    null
                )
            } catch (e: Exception) {
                JuspayLogger.d(LOG_TAG, "Exception in share qr :$e")
            }
        }
    }

    @JavascriptInterface
    fun saveImage(viewId: Int, imageName: String?, imageDescription: String?, callback: String?) {
        val activity = bridgeComponents.activity
        if (activity != null) {
            try {
                val bitmap = screenShot(activity.findViewById(viewId))
                val ctx: Context = bridgeComponents.context
                val path = MediaStore.Images.Media.insertImage(
                    ctx.contentResolver,
                    bitmap,
                    imageName,
                    imageDescription
                )
                val respData = JSONObject()
                respData.put("error", "false")
                respData.put("data", path)
                bridgeComponents.callbackInvoker
                    .invokeCallbackInDUIWebview(callback, respData.toString())
            } catch (e: Exception) {
                JuspayLogger.d(LOG_TAG, "Exception in download qr :$e")
                bridgeComponents.callbackInvoker.invokeCallbackInDUIWebview(
                    callback,
                    String.format(
                        "{\"error\":\"true\",\"data\":\"%s\"}",
                        Base64.encodeToString(
                            "unknown_error::$e".toByteArray(),
                            Base64.NO_WRAP
                        )
                    )
                )
            }
        }
    }

    private fun screenShot(view: View): Bitmap {
        val bitmap = Bitmap.createBitmap(view.width, view.height, Bitmap.Config.ARGB_8888)
        val canvas = Canvas(bitmap)
        view.draw(canvas)
        return bitmap
    }

    private fun makeSuccessData(text: String): String {
        val data = JSONObject()
        data.put("error", "false")
        data.put(
            "data",
            text
        )
        return data.toString()
    }

    private fun makeFailureData(text: String): String {
        val data = JSONObject()
        data.put("error", "true")
        data.put(
            "data",
            text
        )
        return data.toString()
    }

    private fun openQRScanner(viewGrp: String, callback: String?) {
        if (bridgeComponents.activity != null && !TextUtils.isEmpty(viewGrp)) {
            val frameID = viewGrp.toInt()
            JuspayLogger.d(LOG_TAG, "Opening QR Scanner inside Frame with ID :$frameID")
            ExecutorManager.runOnMainThread {
                val activity = bridgeComponents.activity ?: return@runOnMainThread
                val barcodeFrame: ViewGroup =
                    activity.findViewById(frameID)
                if (checkFirebaseScanner()) {
                    JuspayLogger.d(LOG_TAG, "Using Firebase Scanner")
                    firebaseScanner =
                        CameraStreamScanner(container = barcodeFrame, context = bridgeComponents.context, owner = bridgeComponents.activity as LifecycleOwner, callback = {
                            bridgeComponents.callbackInvoker.invokeCallbackInDUIWebview(callback, makeSuccessData(it))
                        })
                    firebaseScanner?.scanWithCamera()
                    return@runOnMainThread
                }
                JuspayLogger.d(LOG_TAG, "Using Zxing Scanner")
                val barcodeView = DecoratedBarcodeView(bridgeComponents.activity)
                val layoutParams = ViewGroup.LayoutParams(
                    ViewGroup.LayoutParams.MATCH_PARENT,
                    ViewGroup.LayoutParams.MATCH_PARENT
                )
                barcodeView.layoutParams = layoutParams
                barcodeFrame.addView(barcodeView)
                captureManager = CaptureManager(activity, barcodeView)
                captureManager?.setBarcodeCallBack(object : BarcodeCallback {
                    override fun barcodeResult(barcodeResult: BarcodeResult) {
                        try {
                            JuspayLogger.d(
                                LOG_TAG,
                                "Scanned QR Result: $barcodeResult"
                            )
                            bridgeComponents.callbackInvoker
                                .invokeCallbackInDUIWebview(
                                    callback,
                                    makeSuccessData(
                                        Base64.encodeToString(
                                            barcodeResult.toString().toByteArray(),
                                            Base64.NO_WRAP
                                        )
                                    )
                                )
                        } catch (e: Exception) {
//                                JuspayLogger.trackAndLogException(LOG_TAG, "", e);
                            bridgeComponents.callbackInvoker.invokeCallbackInDUIWebview(
                                callback,
                                String.format(
                                    "{\"error\":\"true\",\"data\":\"%s\"}",
                                    Base64.encodeToString(
                                        "unknown_error".toByteArray(),
                                        Base64.NO_WRAP
                                    )
                                )
                            )
                        }
                    }

                    override fun possibleResultPoints(list: List<ResultPoint>) {}
                })
                captureManager?.onResume()
                captureManager?.decode()
            }
        } else {
            JuspayLogger.e(LOG_TAG, "ERROR: Frame ID null!")
        }
    }

    private val isPermissionGranted: Boolean
        get() = ContextCompat.checkSelfPermission(
            bridgeComponents.context,
            Manifest.permission.CAMERA
        ) == PackageManager.PERMISSION_GRANTED

    @JavascriptInterface
    fun startQRScanner(parentId: String?, callback: String?) {
        if (parentId == null) {
            onNullParam("parentId", callback)
            return
        }
        if (isPermissionGranted) {
            openQRScanner(parentId, callback)
        } else {
            bridgeComponents.fragmentHooks
                .requestPermission(arrayOf(Manifest.permission.CAMERA), 101)
            listenerMap[REQUEST_PERMISSION_PREFIX + "101"] = Handler.Callback {
                if (isPermissionGranted) {
                    openQRScanner(parentId, callback)
                } else {
                    bridgeComponents.callbackInvoker.invokeCallbackInDUIWebview(
                        callback,
                        String.format(
                            "{\"error\":\"true\",\"data\":\"%s\"}",
                            Base64.encodeToString("permission_denied".toByteArray(), Base64.NO_WRAP)
                        )
                    )
                }
                false
            }
        }
    }

    @JavascriptInterface
    fun stopScanning() {
        firebaseScanner?.releaseCameraResources()
        imageScanner?.releaseResources()
        ExecutorManager.runOnMainThread {
            captureManager?.let {
                it.onPause()
                it.onDestroy()
                captureManager = null
            }
        }
    }

    @JavascriptInterface
    fun generateQRCode(data: String?, parent: String?, qrSize: Int, margin: Int, callback: String?) {
        if (data == null) {
            onNullParam("data", callback)
            return
        } else if (parent == null) {
            onNullParam("parent", callback)
            return
        }
        try {
            val id = parent.toInt()
            val activity = bridgeComponents.activity ?: return
            val view: ImageView = activity.findViewById(id)
            // add more hints if required
            val hintMap = mapOf(EncodeHintType.MARGIN to margin)
            val writer = QRCodeWriter()
            val bitMatrix: BitMatrix =
                writer.encode(data, BarcodeFormat.QR_CODE, qrSize, qrSize, hintMap)
            val width: Int = bitMatrix.width
            val height: Int = bitMatrix.height
            ExecutorManager.runOnMainThread {
                view.setImageBitmap(QrHelper.getBitMapFromBitMatrix(width, height, bitMatrix))
                bridgeComponents.callbackInvoker
                    .invokeCallbackInDUIWebview(callback, "SUCCESS")
            }
        } catch (e: Exception) {
            bridgeComponents.trackerInterface.trackAndLogException(
                LOG_TAG,
                LogCategory.ACTION,
                LogSubCategory.Action.SYSTEM,
                Labels.System.JBRIDGE,
                "Exception while generating QR Code",
                e
            )
            bridgeComponents.callbackInvoker.invokeCallbackInDUIWebview(callback, "FAILURE")
        }
    }

    @JavascriptInterface
    fun checkQRScannerLibrary(): Boolean {
        return try {
            Class.forName("in.juspay.widget.qrscanner.com.google.zxing.BarcodeFormat")
            Class.forName("in.juspay.widget.qrscanner.com.google.zxing.EncodeHintType")
            Class.forName("in.juspay.widget.qrscanner.com.google.zxing.ResultPoint")
            Class.forName("in.juspay.widget.qrscanner.com.google.zxing.common.BitMatrix")
            Class.forName("in.juspay.widget.qrscanner.com.google.zxing.qrcode.QRCodeWriter")
            Class.forName("in.juspay.widget.qrscanner.com.journeyapps.barcodescanner.BarcodeCallback")
            Class.forName("in.juspay.widget.qrscanner.com.journeyapps.barcodescanner.BarcodeResult")
            Class.forName("in.juspay.widget.qrscanner.com.journeyapps.barcodescanner.CaptureManager")
            Class.forName("in.juspay.widget.qrscanner.com.journeyapps.barcodescanner.DecoratedBarcodeView")
            true
        } catch (ignored: Exception) {
            false
        }
    }

    private fun checkFirebaseScanner(): Boolean {
        return try {
            Class.forName("androidx.camera.core.CameraSelector")
            Class.forName("androidx.camera.core.ImageAnalysis")
            Class.forName("androidx.camera.core.ImageCapture")
            Class.forName("androidx.camera.core.ImageProxy")
            Class.forName("androidx.camera.core.Preview")
            Class.forName("androidx.camera.lifecycle.ProcessCameraProvider")
            Class.forName("androidx.camera.view.PreviewView")
            Class.forName("androidx.core.content.ContextCompat")
            Class.forName("androidx.lifecycle.LifecycleOwner")
            Class.forName("com.google.mlkit.vision.barcode.BarcodeScannerOptions")
            Class.forName("com.google.mlkit.vision.barcode.BarcodeScanning")
            Class.forName("com.google.mlkit.vision.barcode.common.Barcode")
            Class.forName("com.google.mlkit.vision.common.InputImage")
            true
        } catch (ignored: Exception) {
            false
        }
    }

    override fun reset() {
        listenerMap.clear()
    }

    override fun onActivityResult(requestCode: Int, resultCode: Int, data: Intent?): Boolean {
        for (listener in listenerMap.values) {
            if (listener is ResultAwaitingDuiHook) {
                if (listener.onActivityResult(
                        requestCode,
                        resultCode,
                        data
                    )
                ) {
                    // Hook consumed the result. Do not pass it to other hooks or the micro-app.
                    bridgeComponents.trackerInterface.trackAction(
                        LogSubCategory.Action.SYSTEM,
                        LogLevel.INFO,
                        Labels.System.JBRIDGE,
                        "on_activity_result",
                        "Result consumed by ResultAwaitingDuiHook " + listener.javaClass.name
                    )
                    return true
                }
            }
        }
        return false
    }

    private fun onNullParam(paramName: String, callback: String?) {
        JuspayLogger.d(LOG_TAG, "'$paramName' is null, returning.")
        bridgeComponents.callbackInvoker.invokeCallbackInDUIWebview(
            callback,
            String.format(
                "{\"error\":\"true\",\"data\":\"%s\"}",
                Base64.encodeToString(
                    "'$paramName' NOT PROVIDED".toByteArray(),
                    Base64.NO_WRAP
                )
            )
        )
    }

    override fun onRequestPermissionResult(
        requestCode: Int,
        permissions: Array<String>,
        grantResults: IntArray
    ): Boolean {
        val subscriber = listenerMap[REQUEST_PERMISSION_PREFIX + requestCode]
        if (subscriber is Handler.Callback) {
            val msg = Message.obtain()
            msg.obj = grantResults
            subscriber.handleMessage(msg)
            return true
        }
        return false
    }

    companion object {
        private const val LOG_TAG = "QRBridge"
        const val GALLERY = "GALLERY"
        const val REQUEST_PERMISSION_PREFIX = "ReqPermi"
        private const val GALLERY_KITKAT_INTENT_CALLED = 118
        private const val SELECT_PHOTO = 117
    }
}
