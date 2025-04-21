package in.juspay.hypersdk.security;

import android.annotation.SuppressLint;
import android.net.http.X509TrustManagerExtensions;

import androidx.annotation.Keep;

import java.security.KeyStore;
import java.security.SecureRandom;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.Set;

import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;
import javax.net.ssl.X509TrustManager;

import in.juspay.hypersdk.BuildConfig;
import in.juspay.hypersdk.core.PaymentUtils;
import in.juspay.hypersdk.utils.network.JuspaySSLSocketFactory;

public class HyperSSLSocketFactory extends JuspaySSLSocketFactory {
    private final SSLSocketFactory sslSocketFactory;
    public static final X509TrustManager DEFAULT_TRUST_MANAGER = getDefaultTrustManager();

    public HyperSSLSocketFactory(final Set<String> acceptedCerts) throws Exception {
        SSLContext ssl = SSLContext.getInstance("SSL");
        final X509TrustManagerExtensions defaultTrustExtension = new X509TrustManagerExtensions(DEFAULT_TRUST_MANAGER);
        TrustManager[] managers = new TrustManager[]{
                new CustomX509TrustManager(DEFAULT_TRUST_MANAGER, defaultTrustExtension, acceptedCerts)
        };
        ssl.init(null, managers, new SecureRandom());
        sslSocketFactory = ssl.getSocketFactory();
    }

    private static X509TrustManager getDefaultTrustManager() {
        try {
            TrustManagerFactory tmf = TrustManagerFactory
                    .getInstance(TrustManagerFactory.getDefaultAlgorithm());
            tmf.init((KeyStore) null);
            return (X509TrustManager) tmf.getTrustManagers()[0];
        } catch (Exception ignored) {
            return null;
        }
    }

    @SuppressLint("CustomX509TrustManager")
    static class CustomX509TrustManager implements X509TrustManager {
        private final X509TrustManager defaultTrust;
        private final X509TrustManagerExtensions defaultTrustExtension;
        private final Set<String> acceptedCerts;

        CustomX509TrustManager(X509TrustManager defaultTrust, X509TrustManagerExtensions defaultTrustExtension , Set<String> acceptedCerts){
            this.defaultTrust = defaultTrust;
            this.acceptedCerts = acceptedCerts;
            this.defaultTrustExtension = defaultTrustExtension;
        }

        public X509Certificate[] getAcceptedIssuers() {
            if(BuildConfig.BUILD_TYPE.equals("debug") || BuildConfig.BUILD_TYPE.equals("qa")) {
                return null;
            } else {
                return defaultTrust.getAcceptedIssuers();
            }
        }

        @SuppressLint("TrustAllX509TrustManager")
        public void checkClientTrusted(X509Certificate[] certs, String authType) throws CertificateException {
            if (!BuildConfig.BUILD_TYPE.equals("debug") && !BuildConfig.BUILD_TYPE.equals("qa")) {
                defaultTrust.checkClientTrusted(certs, authType);
            }
        }

        @SuppressLint("TrustAllX509TrustManager")
        public void checkServerTrusted(X509Certificate[] certs, String authType) throws CertificateException {
            defaultTrust.checkServerTrusted(certs, authType);
            if(PaymentUtils.validatePinning(certs, acceptedCerts)) {
                throw new CertificateException("SSL Pinning failed");
            }
        }

        @Keep
        public void checkServerTrusted(X509Certificate[] certs, String authType, String hostName)
                throws CertificateException {
            defaultTrustExtension.checkServerTrusted(certs, authType,hostName);
            if(PaymentUtils.validatePinning(certs, acceptedCerts)) {
                throw new CertificateException("SSL Pinning failed");
            }
        }
    }

    public SSLSocketFactory getSslSocketFactory() {
        return sslSocketFactory;
    }
}
