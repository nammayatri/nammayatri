package `in`.juspay.mobility.sdk.hyperqr

import android.content.Context
import android.util.Base64
import android.util.Size
import android.view.View
import android.view.ViewGroup
import android.widget.RelativeLayout
import androidx.camera.core.CameraSelector
import androidx.camera.core.ImageAnalysis
import androidx.camera.core.ImageProxy
import androidx.camera.core.Preview
import androidx.camera.lifecycle.ProcessCameraProvider
import androidx.camera.view.PreviewView
import androidx.core.content.ContextCompat
import androidx.lifecycle.LifecycleOwner
import com.google.mlkit.vision.barcode.BarcodeScanner
import com.google.mlkit.vision.barcode.BarcodeScannerOptions
import com.google.mlkit.vision.barcode.BarcodeScanning
import com.google.mlkit.vision.barcode.common.Barcode
import com.google.mlkit.vision.common.InputImage
import `in`.juspay.mobility.sdk.hyper.core.JuspayLogger
import java.net.URLEncoder
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

class CameraStreamScanner(private val container: ViewGroup, private val owner: LifecycleOwner, private val context: Context, private val callback: (String) -> Unit) {
    private lateinit var previewView: PreviewView
    private lateinit var relativeLayout: RelativeLayout
    private lateinit var cameraExecutor: ExecutorService
    private lateinit var cameraProvider: ProcessCameraProvider

    fun scanWithCamera() {
        try {
            createUI()
            startCamera()
        } catch (e: Exception) {
            JuspayLogger.e("FirebaseQrScanner", "setting ui and starting camera failed", e)
            releaseCameraResources()
        }
    }

    private fun createUI() {
        cameraExecutor = Executors.newSingleThreadExecutor()

        // creating previewView
        relativeLayout = RelativeLayout(context)
        relativeLayout.layoutParams = ViewGroup.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.MATCH_PARENT)
        previewView = PreviewView(context).apply {
            layoutParams = ViewGroup.LayoutParams(
                ViewGroup.LayoutParams.MATCH_PARENT,
                ViewGroup.LayoutParams.MATCH_PARENT
            )
            visibility = View.VISIBLE // Ensure visibility
        }
        previewView.implementationMode = PreviewView.ImplementationMode.COMPATIBLE
        container.addView(relativeLayout)
    }

    private fun startCamera() {
        val cameraProviderFuture = ProcessCameraProvider.getInstance(context)
        cameraProviderFuture.addListener(
            Runnable {
                cameraProvider = cameraProviderFuture.get()

                val preview = Preview.Builder().build().also {
                    it.setSurfaceProvider(previewView.surfaceProvider)
                }

//            cameraProvider.availableCameraInfos.get(0).get

                val imageAnalyzer = ImageAnalysis.Builder().setTargetResolution(Size(2560, 1440)).setBackpressureStrategy(ImageAnalysis.STRATEGY_KEEP_ONLY_LATEST).build().also {
                    val analyzer = QRCodeAnalyzer { qrCode ->
                        callback(qrCode)
                        releaseCameraResources()
                    }
                    it.setAnalyzer(cameraExecutor, analyzer)
                }

                val cameraSelector = CameraSelector.DEFAULT_BACK_CAMERA

                try {
                    cameraProvider.unbindAll()
                    cameraProvider.bindToLifecycle(
                        owner,
                        cameraSelector,
                        preview,
                        imageAnalyzer
                    )
                    relativeLayout.addView(previewView, 0)
                } catch (exc: Exception) {
                    JuspayLogger.e("FirebaseQrScanner", "camera binding failed", exc)
                }
            },
            ContextCompat.getMainExecutor(context)
        )
    }

    fun releaseCameraResources() {
        try {
            cameraProvider.unbindAll()
            cameraExecutor.shutdown()
        } catch (ignored: Exception) {
            JuspayLogger.e("FirebaseQrScanner", "camera resource release failed", ignored)
        }
    }
}

class QRCodeAnalyzer(private val onQrCodeDetected: (String) -> Unit) : ImageAnalysis.Analyzer {
    val options = BarcodeScannerOptions.Builder().setBarcodeFormats(
        Barcode.FORMAT_QR_CODE
    ).build()
    private val scanner = BarcodeScanning.getClient(options)

    @androidx.annotation.OptIn(androidx.camera.core.ExperimentalGetImage::class)
    override fun analyze(imageProxy: ImageProxy) {
        val mediaImage = imageProxy.image
        if (mediaImage != null) {
            val rotationDegrees = imageProxy.imageInfo.rotationDegrees
            val imageRotation = degreesToFirebaseRotation(rotationDegrees)
            val image = InputImage.fromMediaImage(mediaImage, imageRotation)

            scanner.process(image).addOnSuccessListener { barcodes ->
                for (barcode in barcodes) {
                    when (barcode.valueType) {
                        Barcode.TYPE_TEXT -> {
                            if (barcode.rawValue != null) {
                                onQrCodeDetected(
                                    Base64.encodeToString(
                                        barcode.rawValue!!.toByteArray(),
                                        Base64.NO_WRAP
                                    )
                                )
                                break
                            }
                        }
                    }
                }
            }.addOnFailureListener {
                JuspayLogger.e("FirebaseQrScanner", "QR Code detection failed", it)
            }.addOnCompleteListener {
                imageProxy.close()
            }
        } else {
            imageProxy.close()
        }
    }

    private fun degreesToFirebaseRotation(degrees: Int): Int {
        return when (degrees) {
            0 -> 0
            90 -> 90
            180 -> 180
            270 -> 270
            else -> throw IllegalArgumentException("Unsupported rotation degrees: $degrees")
        }
    }
}

class InputImageScanner(private val successCallback: (String) -> Unit, private val failureCallback: (String) -> Unit) {
    private lateinit var scanner: BarcodeScanner
    fun scanImage(inputImage: InputImage) {
        scanner = BarcodeScanning.getClient(
            BarcodeScannerOptions.Builder().setBarcodeFormats(
                Barcode.FORMAT_QR_CODE
            ).build()
        )
        scanner.process(inputImage).addOnSuccessListener { barcodes ->
            for (barcode in barcodes) {
                when (barcode.valueType) {
                    Barcode.TYPE_TEXT -> {
                        if (barcode.rawValue != null) {
                            successCallback(URLEncoder.encode(barcode.rawValue, "UTF-8").replace("+", "%20"))
                            return@addOnSuccessListener
                        }
                    }
                }
            }
            failureCallback("Could not able to find any qr in image")
        }.addOnFailureListener {
            failureCallback("exception on scanning image $it")
            JuspayLogger.e("FirebaseQrScanner", "exception on scanning gallery image", it)
        }
    }

    fun releaseResources() {
        scanner?.close()
    }
}
