package in.juspay.hypersdk.security;

import androidx.annotation.Keep;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.security.DigestInputStream;
import java.security.Key;
import java.security.KeyPair;
import java.security.KeyStore;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.SecureRandom;
import java.security.interfaces.RSAPublicKey;
import java.util.HashMap;
import java.util.Map;
import java.util.zip.GZIPInputStream;

import javax.crypto.spec.SecretKeySpec;

import in.juspay.hyper.constants.Labels;
import in.juspay.hyper.constants.LogCategory;
import in.juspay.hyper.constants.LogSubCategory;
import in.juspay.hyper.core.JuspayLogger;
import in.juspay.hypersdk.core.PaymentConstants;
import in.juspay.hypersdk.core.SdkTracker;
import in.juspay.hypersdk.utils.Utils;


/**
 * Created by kushagra on 15/03/17.
 */

@Keep
public class EncryptionHelper {
    private final static String LOG_TAG = EncryptionHelper.class.getSimpleName();

    private static final String algorithm = "AES";
    private static final byte[] logsEntryRequirement = new byte[]{
            (byte) 204, 51, (byte) 188, (byte) 135, (byte) 212, (byte) 142,
            (byte) 197, (byte) 236, (byte) 177, 22, 34, (byte) 179, (byte) 208, (byte) 181, 45, 93
    };

    @Keep
    public static String getSHA256Hash(@Nullable String data) {
        if (data == null) return null;
        // generate a hash
        MessageDigest digest = null;
        String hash;
        try {
            digest = MessageDigest.getInstance("SHA-256");
            digest.update(data.getBytes());
            hash = bytesToHexString(digest.digest());

            JuspayLogger.d(LOG_TAG, "result is " + hash);
            return hash;
        } catch (NoSuchAlgorithmException e) {
            SdkTracker.trackAndLogBootException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.HELPER, "Exception caught trying to SHA-256 hash", e);
        }
        return null;
    }

    @SuppressWarnings("unused")
    public static String md5(String s) {
        return md5(s.getBytes());
    }

    public static String md5(final byte[] bytes) {
        final String MD5 = "MD5";
        try {
            // Create MD5 Hash
            MessageDigest digest = MessageDigest
                    .getInstance(MD5);
            digest.update(bytes);
            byte[] messageDigest = digest.digest();

            // Create Hex String
            StringBuilder hexString = new StringBuilder();
            for (byte aMessageDigest : messageDigest) {
                StringBuilder h = new StringBuilder(Integer.toHexString(0xFF & aMessageDigest));
                while (h.length() < 2) {
                    h.insert(0, "0");
                }
                hexString.append(h);
            }
            return hexString.toString();

        } catch (NoSuchAlgorithmException e) {
            SdkTracker.trackAndLogBootException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.HELPER, "Exception trying to calculate md5sum from given string", e);
        }
        return null;
    }

    @SuppressWarnings("WeakerAccess")
    public static String md5(InputStream inputStream) {
        try {
            MessageDigest md = MessageDigest.getInstance("MD5");
            byte[] digest;
            try (DigestInputStream digestInputStream = new DigestInputStream(inputStream, md)) {
                //noinspection StatementWithEmptyBody
                while (digestInputStream.read() != -1) {
                    // updates message digest inside DigestInputStream
                }
                digest = digestInputStream.getMessageDigest().digest();
            }
            // Create Hex String
            StringBuilder hexString = new StringBuilder();
            for (byte aMessageDigest : digest) {
                StringBuilder h = new StringBuilder(Integer.toHexString(0xFF & aMessageDigest));
                while (h.length() < 2) {
                    h.insert(0, "0");
                }
                hexString.append(h);
            }
            return hexString.toString();
        } catch (NoSuchAlgorithmException | IOException e) {
            SdkTracker.trackAndLogBootException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.HELPER, "Exception trying to get md5sum from input stream", e);
        }
        return null;
    }

    public static byte[] decryptThenGunzip(byte[] encrypted, String ignoredLogic) {
        try {
//            if (logic.equals("v1"))
            return gunzipContent(v1Decrypt(encrypted));
//            else
//                return gunzipContent(decrypt(encrypted));
        } catch (Exception e) {
            SdkTracker.trackAndLogBootException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.HELPER, "Exception in decrypting", e);
            throw new RuntimeException(e);
        }
    }

    public static byte[] gunzipContent(byte[] compressed) {
        final int BUFFER_SIZE = 1024;
        byte[] data = new byte[BUFFER_SIZE];

        try {
            byte[] output;
            try (ByteArrayInputStream is = new ByteArrayInputStream(compressed)) {
                try (ByteArrayOutputStream os = new ByteArrayOutputStream()) {
                    try (GZIPInputStream gis = new GZIPInputStream(is, BUFFER_SIZE)) {
                        int bytesRead;
                        while ((bytesRead = gis.read(data)) != -1) {
                            os.write(data, 0, bytesRead);
                        }
                    }
                    output = os.toByteArray();
                }
            }
            return output;
        } catch (IOException e) {
            SdkTracker.trackAndLogBootException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.HELPER, "Error while gunzipping", e);
            throw new RuntimeException(e);
        }
    }

    @SuppressWarnings("WeakerAccess")
    public static byte[] v1Decrypt(byte[] encrypted) {
        int FRAME_SIZE = 8;
        int KEY_SIZE = 8;
        byte[] keyValue = new byte[KEY_SIZE];
        byte[] encryptedWithoutKey = new byte[encrypted.length - KEY_SIZE];
        int keyCounter = 0;
        int encodedCounter = 0;
        int totalBytes = encrypted.length;
        keyValue[0] = encrypted[9];
        keyValue[1] = encrypted[19];
        keyValue[2] = encrypted[29];
        keyValue[3] = encrypted[39];
        keyValue[4] = encrypted[49];
        keyValue[5] = encrypted[59];
        keyValue[6] = encrypted[69];
        keyValue[7] = encrypted[79];
        for (int encryptedCounter = 0; encryptedCounter < totalBytes; encryptedCounter++) {
            if (encryptedCounter % 10 == 9 && keyCounter < KEY_SIZE) {
                //Skip this byte
                keyCounter++;
            } else {
                //XOR Byte with KEY Byte
                encryptedWithoutKey[encodedCounter] = (byte) ((int) encrypted[encryptedCounter] ^ (int) keyValue[encodedCounter % FRAME_SIZE]);
                encodedCounter++;
            }
        }
        return encryptedWithoutKey;
    }

    private static Key generateKey() {
        try (InputStream is = new ByteArrayInputStream(logsEntryRequirement)) {
            byte[] content = new byte[is.available()];
            int readBytes = 0;

            do {
                readBytes += is.read(content);
            } while (readBytes < content.length);

            return new SecretKeySpec(content, algorithm);
        } catch (IOException e) {
            SdkTracker.trackAndLogBootException(LOG_TAG, PaymentConstants.Category.SDK, LogSubCategory.Action.SYSTEM, "generate_key", null, e);
        }

        return null;
    }

    @SuppressWarnings({"WeakerAccess", "RedundantThrows"})
    public static byte[] v1Encrypt(byte[] input) throws Exception {
        byte[] gzippedContent = Utils.gzipContent(input);

        int FRAME_SIZE = 8;
        int KEY_SIZE = 8;
        //Generate 8 byte Random Key
        SecureRandom random = new SecureRandom();
        byte[] randomKey = new byte[8];
        random.nextBytes(randomKey);

        //Generate Obfuscated JSA File
        int totalBytes = gzippedContent.length;
        byte[] encodedBytes = new byte[totalBytes + KEY_SIZE];
        int hiddenCounter = 0;
        int encodedCounter = 0;
        for (int compressedCounter = 0; compressedCounter < totalBytes && encodedCounter < totalBytes + KEY_SIZE; encodedCounter++) {
            if (encodedCounter % 10 == 9 && hiddenCounter < KEY_SIZE) {
                //Append Key
                encodedBytes[encodedCounter] = randomKey[hiddenCounter];
                hiddenCounter++;
            } else {
                //XOR Byte with KEY Byte
                encodedBytes[encodedCounter] = (byte) ((int) gzippedContent[compressedCounter] ^ (int) randomKey[compressedCounter % FRAME_SIZE]);
                compressedCounter++;
            }
        }
        return encodedBytes;
    }


    public static byte[] gzipThenEncrypt(byte[] content, RSAPublicKey publicKey) {
        try {
            byte[] gzippedContent = Utils.gzipContent(content);
            String headers = "{\"alg\":\"RSA-OAEP-256\",\"enc\":\"A256GCM\"}";

            HashMap<String, Object> map = JOSEUtils.jweEncrypt(gzippedContent, headers, publicKey);
            String encryptedPayload = JOSEUtils.constructPayload(map);
            if (encryptedPayload != null) {
                return encryptedPayload.getBytes(StandardCharsets.UTF_8);
            }
        } catch (Exception e) {
            SdkTracker.trackAndLogBootException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.HELPER, "Exception while GZipping and encrypting", e);
        }
        return null;
    }

    public static byte[] gzipThenEncryptExp(byte[] content, RSAPublicKey publicKey, Map<String, String> header) {
        try {
            byte[] gzippedContent = Utils.gzipContent(content);
            String headers = "{\"alg\":\"RSA-OAEP-256\",\"enc\":\"A256GCM\"}";

            HashMap<String, Object> properties = JOSEUtils.jweEncrypt(gzippedContent, headers, publicKey);
            if (properties.containsKey("headers") && properties.containsKey("encryptedKey") && properties.containsKey("iv") && properties.containsKey("cipherText") && properties.containsKey("authTag")) {
                headers = (String) properties.get("headers");
                String encryptedKey = (String) properties.get("encryptedKey");
                String iv = (String) properties.get("iv");
                byte[] cipherText = (byte[]) properties.get("cipherText");
                String authTag = (String) properties.get("authTag");
                if (headers != null && encryptedKey != null && iv != null && cipherText != null && authTag != null) {
                    header.put("protectedHeaders", headers);
                    header.put("encryptedKey", encryptedKey);
                    header.put("iv", iv);
                    header.put("authTag", authTag);
                    return cipherText;
                }
            }
            return null;

        } catch (Exception e) {
            SdkTracker.trackAndLogBootException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.HELPER, "Exception while GZipping and encrypting", e);
        }
        return null;
    }

    @NonNull
    public static String bytesToHexString(@NonNull byte[] bytes) {
        // utility function
        StringBuilder stringBuffer = new StringBuilder();
        for (byte aByte : bytes) {
            String hex = Integer.toHexString(0xFF & aByte);
            if (hex.length() == 1) {
                stringBuffer.append('0');
            }
            stringBuffer.append(hex);
        }
        return stringBuffer.toString();
    }

    @NonNull
    private static KeyStore getAndroidKeyStore() throws Exception {
        KeyStore keyStore = KeyStore.getInstance("AndroidKeyStore");
        keyStore.load(null);

        return keyStore;
    }

    @NonNull
    public static KeyPair getKeyPair(String alias) throws Exception {
        KeyStore keyStore = getAndroidKeyStore();

        PrivateKey privateKey = (PrivateKey) keyStore.getKey(alias, null);
        PublicKey publicKey = keyStore.getCertificate(alias).getPublicKey();

        return new KeyPair(publicKey, privateKey);
    }
}
