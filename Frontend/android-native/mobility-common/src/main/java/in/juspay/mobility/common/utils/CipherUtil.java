

package in.juspay.mobility.common.utils;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.SecretKeySpec;

import java.nio.charset.StandardCharsets;
import java.security.spec.KeySpec;
import android.util.Base64;

public class CipherUtil {
    private String ALGO;
    private String ALGO_PADDING;
    private String INSTANCE_TYPE;
    private String encKey;

    private static final byte[] salt = {(byte) 0xA8, (byte) 0x9B, (byte) 0xC8, (byte) 0x32, (byte) 0x56, (byte) 0x34,
            (byte) 0xE3, (byte) 0x03};

    private static CipherUtil cipherUtil = null;


    public void setParams(final String _algo, final String _algo_padding, final String _instanceType, final String _encKey){
        this.ALGO = _algo;
        this.ALGO_PADDING = _algo_padding;
        this.INSTANCE_TYPE = _instanceType;
        this.encKey = _encKey;
    }

    public static CipherUtil getInstance(){
        if(cipherUtil == null){
            cipherUtil = new CipherUtil();
        }
        return cipherUtil;
    }


    public String decryptData(String str) {
        if (str.isEmpty()) {
            return "";
        }
        int iterationCount = 20;
        try {
            KeySpec keySpec = new PBEKeySpec(encKey.toCharArray(), salt, iterationCount, 128);
            SecretKeyFactory keyFactory = SecretKeyFactory.getInstance(INSTANCE_TYPE);
            byte[] keyBytes = keyFactory.generateSecret(keySpec).getEncoded();
            SecretKey key = new SecretKeySpec(keyBytes, ALGO);
            Cipher dcipher = Cipher.getInstance(getAesPadding());
            dcipher.init(Cipher.DECRYPT_MODE, key);
            byte[] dec = Base64.decode(str, Base64.DEFAULT);
            byte[] utf8 = dcipher.doFinal(dec);
            return new String(utf8, StandardCharsets.UTF_8);
        } catch (Exception e) {
//            logger.error("Exception while getting decryptData");
        }
        return str;

    }

    public String encryptData(String str) {
        if (str.isEmpty()) {
            return "";
        }
        int iterationCount = 20;
        try {
            KeySpec keySpec = new PBEKeySpec(encKey.toCharArray(), salt, iterationCount, 128);
            SecretKeyFactory keyFactory = SecretKeyFactory.getInstance(INSTANCE_TYPE);
            byte[] keyBytes = keyFactory.generateSecret(keySpec).getEncoded();
            SecretKey key = new SecretKeySpec(keyBytes, ALGO);
            Cipher ecipher = Cipher.getInstance(getAesPadding());
            ecipher.init(Cipher.ENCRYPT_MODE, key);
            byte[] utf8 = str.getBytes(StandardCharsets.UTF_8);
            byte[] enc = ecipher.doFinal(utf8);
            return Base64.encodeToString(enc, Base64.DEFAULT);
        } catch (Exception e) {
//            logger.error("Exception while getting encryptData");
        }
        return str;
    }

    private String getAesPadding() {
        return this.ALGO_PADDING;
    }
}
