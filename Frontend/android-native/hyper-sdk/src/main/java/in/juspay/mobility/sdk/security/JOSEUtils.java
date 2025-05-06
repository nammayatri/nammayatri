package in.juspay.mobility.sdk.security;


import static in.juspay.mobility.sdk.security.Base64Codec.decode;
import static in.juspay.mobility.sdk.security.Base64Codec.encodeToString;

import android.os.Build;

import androidx.annotation.Nullable;

import org.json.JSONObject;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.security.KeyFactory;
import java.security.PrivateKey;
import java.security.SecureRandom;
import java.security.Signature;
import java.security.interfaces.RSAPublicKey;
import java.security.spec.AlgorithmParameterSpec;
import java.security.spec.RSAPublicKeySpec;
import java.security.spec.X509EncodedKeySpec;
import java.util.HashMap;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.spec.GCMParameterSpec;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;


public class JOSEUtils {

    public static String jwsSign(String data, String headers, PrivateKey key) throws Exception {
        String msg = encodeToString(headers.getBytes(StandardCharsets.UTF_8), true) + "." + encodeToString(data.getBytes(StandardCharsets.UTF_8), true);
        String alg = extractKey("alg", headers);

        assertIfSupportedSigningAlg(alg);

        // Sign
        Signature privateSignature = Signature.getInstance(getJavaAlg(alg));
        privateSignature.initSign(key);
        privateSignature.update(msg.getBytes(StandardCharsets.UTF_8));
        byte[] signature = privateSignature.sign();

        return msg + "." + encodeToString(signature, true);
    }


    public static boolean jwsVerify(String data, byte[] wrappingKey) throws Exception {
        RSAPublicKey key = (RSAPublicKey) KeyFactory.getInstance("RSA")
                .generatePublic(new X509EncodedKeySpec(wrappingKey));
        String[] x = data.split("\\.");
        if (x.length == 3) {
            String msg = x[0] + "." + x[1];
            String alg = extractKey("alg", new String(decode(x[0])));

            assertIfSupportedSigningAlg(alg);

            // Verify
            Signature sign = Signature.getInstance(getJavaAlg(alg));
            sign.initVerify(key);
            sign.update(msg.getBytes(StandardCharsets.UTF_8));

            return sign.verify(decode(x[2]));
        } else {
            throw new Exception("JWS Verify - mandatory params missing " + x.length);
        }
    }

    public static String jweEncrypt(String data, String headers, byte[] wrappingKey) throws Exception {

        RSAPublicKey key = (RSAPublicKey) KeyFactory.getInstance("RSA")
                .generatePublic(new X509EncodedKeySpec(wrappingKey));

        byte[] bytes = data.getBytes(StandardCharsets.UTF_8);

        return constructPayload(jweEncrypt(bytes, headers, key));
    }

    public static String constructPayload(HashMap<String, Object> properties) throws Exception {
        if (properties.containsKey("headers") && properties.containsKey("encryptedKey") && properties.containsKey("iv") && properties.containsKey("cipherText") && properties.containsKey("authTag")) {
            String headers = (String) properties.get("headers");
            String encryptedKey = (String) properties.get("encryptedKey");
            String iv = (String) properties.get("iv");
            byte[] cipherText = (byte[]) properties.get("cipherText");
            String authTag = (String) properties.get("authTag");
            if (headers != null && encryptedKey != null && iv != null && cipherText != null && authTag != null) {
                return headers
                        + "."
                        + encryptedKey
                        + "."
                        + iv
                        + "."
                        + encodeToString(cipherText, true)
                        + "."
                        + authTag;
            }
        }
        return null;
    }


    /**
     * 1. Generate CEK
     * 2. Encrypt CEK with RSA Key
     * 3. AES Encrypt Payload with CEK
     * 4. Construct payload
     * Note = It only supports, enc=A256GCM,alg=RSA1_5
     */
    public static HashMap<String, Object> jweEncrypt(byte[] data, String headers, RSAPublicKey key) throws Exception {

        String alg = extractKey("alg", headers);

        assertIfSupportedEncAlg(alg);
        assertIfMatches(extractKey("enc", headers), "A256GCM");

        // Generate CEK, enc="A256GCM"
        final byte[] cekMaterial = new byte[256 / 8]; // IV Size is dependent on encryption algorithm
        final SecureRandom randomGen = new SecureRandom();
        randomGen.nextBytes(cekMaterial);
        SecretKeySpec cek = new SecretKeySpec(cekMaterial, "AES");

        // Encrypt CEK
        Cipher ciph = Cipher.getInstance(getJavaAlg(alg));
        ciph.init(Cipher.ENCRYPT_MODE, key);
        String encryptedKey = encodeToString(ciph.doFinal(cek.getEncoded()), true);

        // Calculate AAD
        byte[] aad = encodeToString(headers.getBytes(StandardCharsets.UTF_8), true).getBytes(StandardCharsets.US_ASCII);

        // Generate IV
        byte[] iv = new byte[96 / 8]; // IV Size is dependent on encryption algorithm
        final SecureRandom rGen = new SecureRandom();
        rGen.nextBytes(iv);

        //AES Encrypt, this works only on Android Api Version >=19
        Cipher cipher = Cipher.getInstance("AES/GCM/NoPadding");
        AlgorithmParameterSpec spec;
        try {
            if (Build.VERSION.SDK_INT <= Build.VERSION_CODES.M) {
                spec = new IvParameterSpec(iv);
            } else {
                spec = new GCMParameterSpec(128, iv);
            }
        } catch (Exception e) {
            spec = new GCMParameterSpec(128, iv);
        }


        cipher.init(Cipher.ENCRYPT_MODE, cek, spec);
        cipher.updateAAD(aad);
        byte[] cipherOutput = cipher.doFinal(data);
        final int tagPos = cipherOutput.length - (128 / 8);
        byte[] cipherText = subArray(cipherOutput, 0, tagPos);
        byte[] authTag = subArray(cipherOutput, tagPos, 128 / 8);
        try {
            if (Build.VERSION.SDK_INT <= Build.VERSION_CODES.M) {
                IvParameterSpec actualParams = cipher.getParameters().getParameterSpec(IvParameterSpec.class);
                iv = actualParams.getIV();
            } else {
                GCMParameterSpec actualParams = cipher.getParameters().getParameterSpec(GCMParameterSpec.class);
                iv = actualParams.getIV();
            }
        } catch (Exception e) {
            GCMParameterSpec actualParams = cipher.getParameters().getParameterSpec(GCMParameterSpec.class);
            iv = actualParams.getIV();
        }

        HashMap<String, Object> map = new HashMap<>();
        map.put("encryptedKey", encryptedKey);
        map.put("iv", encodeToString(iv, true));
        map.put("cipherText", cipherText);
        map.put("authTag", encodeToString(authTag, true));
        map.put("headers", encodeToString(headers.getBytes(StandardCharsets.UTF_8), true));

        return map;
    }

    public static JSONObject jweDecrypt(String cipher, PrivateKey key) throws Exception {
        String[] cipherArr = cipher.split("\\.");
        String headers = new String(decode(cipherArr[0]));
        String alg = extractKey("alg", headers);

        assertIfSupportedEncAlg(alg);
        assertIfMatches(extractKey("enc", headers), "A256GCM");

        final byte[] cekMaterial = new byte[256 * 8];
        new SecureRandom().nextBytes(cekMaterial);
        SecretKey cek;
        String encryptedCEK = cipherArr[1];
        Cipher cipher2 = Cipher.getInstance(getJavaAlg(alg));
        cipher2.init(Cipher.DECRYPT_MODE, key);
        byte[] secretKeyBytes = cipher2.doFinal(decode(encryptedCEK));

        //TODO :: check safe length
        cek = new SecretKeySpec(secretKeyBytes, "AES");
        final byte[] aad = encodeToString(headers.getBytes(StandardCharsets.UTF_8), true).getBytes(StandardCharsets.US_ASCII);
        final SecretKey aesKey = new SecretKeySpec(cek.getEncoded(), "AES");
        Cipher aesCipher = Cipher.getInstance("AES/GCM/NoPadding");
        AlgorithmParameterSpec spec;
        try {
            if (Build.VERSION.SDK_INT <= Build.VERSION_CODES.M) {
                spec = new IvParameterSpec(decode(cipherArr[2]));
            } else {
                spec = new GCMParameterSpec(128, decode(cipherArr[2]));
            }
        } catch (Exception e) {
            spec = new GCMParameterSpec(128, decode(cipherArr[2]));
        }
        aesCipher.init(Cipher.DECRYPT_MODE, aesKey, spec);
        aesCipher.updateAAD(aad);
        byte[] plainText = aesCipher.doFinal(concat(decode(cipherArr[3]), decode(cipherArr[4])));
        JSONObject result = new JSONObject();
        result.put("headers", headers);
        result.put("payload", new String(plainText));
        return result;
    }


    public static String extractKey(String key, String headers) throws Exception {
        JSONObject jHeaders = new JSONObject(headers);
        if (jHeaders.has(key)) {
            return jHeaders.getString(key);
        } else {
            throw new Exception("JWS Sign - header missing " + key);
        }
    }

    public static void assertIfMatches(String org, String expected) throws Exception {
        if (!org.equals(expected)) {
            throw new Exception("Assert failed, org=" + org + ", expected=" + expected);
        }
    }

    public static byte[] subArray(byte[] byteArray, int beginIndex, int length) {
        byte[] subArray = new byte[length];
        System.arraycopy(byteArray, beginIndex, subArray, 0, subArray.length);
        return subArray;
    }

    public static void assertIfSupportedSigningAlg(String alg) throws Exception {
        switch (alg) {
            case "RS256":
            case "RS512":
                return;
            default:
                throw new Exception("Not supported signing alg " + alg);
        }
    }

    public static void assertIfSupportedEncAlg(String alg) throws Exception {
        switch (alg) {
            case "RSA1_5":
            case "RSA-OAEP":
            case "RSA-OAEP-256":
                return;
            default:
                throw new Exception("Not supported signing alg " + alg);
        }
    }

    public static String getJavaAlg(String alg) throws Exception {
        switch (alg) {
            case "RS256":
                return "SHA256withRSA";
            case "RS512":
                return "SHA512withRSA";
            case "RSA1_5":
                return "RSA/ECB/PKCS1Padding";
            case "RSA-OAEP":
                return "RSA/ECB/OAEPWithSHA-1AndMGF1Padding";
            case "RSA-OAEP-256":
                return "RSA/ECB/OAEPWithSHA-256AndMGF1Padding";
            default:
                throw new Exception("Not supported signing alg " + alg);
        }
    }

    public static byte[] concat(byte[]... byteArrays) {
        try {
            try (ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
                for (byte[] bytes : byteArrays) {
                    if (bytes == null) {
                        continue; // skip
                    }
                    baos.write(bytes);
                }
                return baos.toByteArray();
            }
        } catch (IOException e) {
            // Should never happen
            throw new IllegalStateException(e.getMessage(), e);
        }
    }

    @Nullable
    public static RSAPublicKey JWKtoRSAPublicKey(JSONObject jwk) throws Exception {
        String n = jwk.getString("n");
        String e = jwk.getString("e");

        byte[] modulusBytes = decode(n);
        BigInteger modulus = new BigInteger(1, modulusBytes);
        byte[] exponentBytes = decode(e);
        BigInteger exponent  = new BigInteger(1, exponentBytes);

        RSAPublicKeySpec spec = new RSAPublicKeySpec(modulus, exponent);
        KeyFactory factory = KeyFactory.getInstance("RSA");

        return (RSAPublicKey) factory.generatePublic(spec);
    }
}
