package in.juspay.hypersdk.core;

import android.util.Log;

import com.caoccao.javet.interfaces.IJavetLogger;


public class JavetLogger implements IJavetLogger {

    public JavetLogger(String name) {
//        logger = LoggerFactory.getLogger(name);
    }

    @Override
    public void debug(String message) {
        Log.d("Javet", message);
//        if (logger.isDebugEnabled()) {
//            logger.debug(message);
//        }
    }

    @Override
    public void error(String message) {
        Log.e("Javet", message);
//        if (logger.isDebugEnabled()) {
//            logger.error(message);
//        }
    }

    @Override
    public void error(String message, Throwable throwable) {
        Log.e("Javet", message);
//        if (logger.isDebugEnabled()) {
//            logger.error(message, throwable);
//        }
    }

    @Override
    public void info(String message) {
        Log.i("Javet", message);
//        if (logger.isInfoEnabled()) {
//            logger.info(message);
//        }
    }

    @Override
    public void warn(String message) {
        Log.w("Javet", message);
//        if (logger.isWarnEnabled()) {
//            logger.warn(message);
//        }
    }
}