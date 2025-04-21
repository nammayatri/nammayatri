package `in`.juspay.hyper.core

import android.util.Log
import androidx.annotation.Keep
import `in`.juspay.hyper.constants.LogLevel

/**
 * A central logging system for all the Juspay SDKs. This class does all the checks, and only logs the messages if the
 * app is in debug configuration. Otherwise, the messages are just ignored.
 *
 * @author Kushagra Singh [kushagra.singh@juspay.in]
 * @author Sri Harsha Chilakapati [sri.harsha@juspay.in]
 * @since 15/03/2017
 */
@Keep
object JuspayLogger {
    /**
     * Sends a `DEBUG` message if the application is debuggable.
     *
     * @param tag         Used to identify the source of a log message. It usually identifies the class or activity
     * where the log call occurs.
     * @param description The message you would like logged.
     */
    @JvmStatic
    fun d(tag: String, description: String) {
        if (JuspayCoreLib.isAppDebuggable) {
            Log.d(tag, description)
        }
    }

    /**
     * Sends an `ERROR` message if the application is debuggable.
     *
     * @param tag         Used to identify the source of a log message. It usually identifies the class or activity
     * where the log call occurs.
     * @param description The message you would like logged.
     */
    @JvmStatic
    fun e(tag: String, description: String) {
        if (JuspayCoreLib.isAppDebuggable) {
            Log.e(tag, description)
        }
    }

    /**
     * Sends a `ERROR` message if the application is debuggable.
     *
     * @param tag         Used to identify the source of a log message. It usually identifies the class or activity
     * where the log call occurs.
     * @param description The message you would like logged.
     * @param throwable   The exception to be logged.
     */
    @JvmStatic
    fun e(tag: String, description: String, throwable: Throwable) {
        if (JuspayCoreLib.isAppDebuggable) {
            Log.e(tag, description, throwable)
        }
    }

    /**
     * Sends a `INFO` message if the application is debuggable.
     *
     * @param tag         Used to identify the source of a log message. It usually identifies the class or activity
     * where the log call occurs.
     * @param description The message you would like logged.
     */
    @JvmStatic
    fun i(tag: String, description: String) {
        if (JuspayCoreLib.isAppDebuggable) {
            Log.i(tag, description)
        }
    }

    /**
     * Sends a `WARN` message if the application is debuggable.
     *
     * @param tag         Used to identify the source of a log message. It usually identifies the class or activity
     * where the log call occurs.
     * @param description The message you would like logged.
     */
    @JvmStatic
    fun w(tag: String, description: String) {
        if (JuspayCoreLib.isAppDebuggable) {
            Log.w(tag, description)
        }
    }

    /**
     * Sends a message in the given log level if the application is debuggable.
     *
     * @param tag         Used to identify the source of a log message. It usually identifies the class or activity
     * where the log call occurs.
     * @param level       The level of this log.
     * @param description The message you would like logged.
     */
    @JvmStatic
    fun log(tag: String, level: String, description: String) {
        when (level) {
            LogLevel.CRITICAL, LogLevel.ERROR -> e(tag, description)
            LogLevel.WARNING -> w(tag, description)
            LogLevel.INFO -> i(tag, description)
            LogLevel.DEBUG -> d(tag, description)
        }
    }
}
