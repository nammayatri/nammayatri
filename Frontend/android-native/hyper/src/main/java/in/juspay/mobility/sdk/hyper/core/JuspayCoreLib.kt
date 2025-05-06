package `in`.juspay.mobility.sdk.hyper.core

import android.annotation.SuppressLint
import android.content.Context
import android.content.pm.ApplicationInfo
import androidx.annotation.Discouraged
import androidx.annotation.Keep

/**
 * A fake library initializer that is used to grab the application context. This will not cause any memory leak since
 * the context we get here, that is, the application context, is a long living context. Setting the application context
 * here will be taken care by `HyperServices` class.
 *
 * @author Sri Harsha Chilakapati [sri.harsha@juspay.in]
 * @since 26/11/2019
 */
@Keep
object JuspayCoreLib {
    /**
     * @return The Application context.
     */
    @JvmStatic
    @get:Discouraged(
        message = "Getting applicationContext from JuspayCoreLib is discouraged. " +
            "There is probably a better way to get the context, " +
            "unless you are in some static context"
    )
    var applicationContext: Context? = null
        private set

    @JvmStatic
    fun setApplicationContext(applicationContext: Context) {
        JuspayCoreLib.applicationContext = applicationContext
    }

    @JvmStatic
    val isAppDebuggable: Boolean
        /**
         * @return Whether the application is in debug variant
         */
        get() {
            @SuppressLint("DiscouragedApi")
            val context = applicationContext ?: return false
            return context.applicationInfo.flags and ApplicationInfo.FLAG_DEBUGGABLE != 0
        }
}
