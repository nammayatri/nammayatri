package in.juspay.hyperqr;

import android.graphics.Bitmap;
import android.graphics.Color;

import androidx.annotation.Keep;

import java.util.HashMap;
import java.util.Map;

import in.juspay.widget.qrscanner.com.google.zxing.BarcodeFormat;
import in.juspay.widget.qrscanner.com.google.zxing.EncodeHintType;
import in.juspay.widget.qrscanner.com.google.zxing.common.BitMatrix;
import in.juspay.widget.qrscanner.com.google.zxing.qrcode.QRCodeWriter;

/**
 * This is accessed via reflection by presto list and also a prop in presto dom
 */
public class QrHelper {

    @Keep
    public static Bitmap getBitMapFromBitMatrix(int width, int height, BitMatrix bitMatrix) {
        Bitmap bitmap = Bitmap.createBitmap(width, height, Bitmap.Config.RGB_565);
        for (int x = 0; x < width; x++) {
            for (int y = 0; y < height; y++) {
                bitmap.setPixel(x, y, bitMatrix.get(x, y) ? Color.BLACK : Color.WHITE);
            }
        }
        return bitmap;
    }

    @Keep
    public static Bitmap getQrBitMap(String data, int qrSize, int margin) {
        Map<EncodeHintType, Object> hintMap = new HashMap<>();
        hintMap.put(EncodeHintType.MARGIN, margin);
        QRCodeWriter writer = new QRCodeWriter();
        final BitMatrix bitMatrix = writer.encode(data, BarcodeFormat.QR_CODE, qrSize, qrSize, hintMap);
        final int width = bitMatrix.getWidth();
        final int height = bitMatrix.getHeight();

        return getBitMapFromBitMatrix(width, height, bitMatrix);
    }
}
