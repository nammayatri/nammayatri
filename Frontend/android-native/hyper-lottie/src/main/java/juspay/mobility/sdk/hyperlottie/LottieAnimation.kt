package `in`.juspay.hyperlottie

import android.content.Context
import android.view.View
import com.airbnb.lottie.LottieComposition
import com.airbnb.lottie.LottieCompositionFactory
import com.airbnb.lottie.LottieDrawable
import `in`.juspay.mobility.sdk.hyper.core.FileProviderInterface
import `in`.juspay.mobility.sdk.hyper.core.JsCallback
import org.json.JSONArray
import java.util.WeakHashMap

class LottieAnimation(
    private val context: Context,
    private val dynamicUI: JsCallback,
    private val fileProviderInterface: FileProviderInterface
) {
    private val jsonStringCache: WeakHashMap<String, String?> = WeakHashMap()

    fun applyAnimation(view: Any?, animations: JSONArray) {
        try {
            if (view !is View) return
            if (animations.length() == 0) {
                if (view.background is LottieDrawable) view.background = null
                return
            }
            if (animations.length() > 1) {
                dynamicUI.addJsToWebView("console.log(\"LottieAnimations Array is > 1\");")
            }
            val animation = animations.getJSONObject(animations.length() - 1) ?: return
            val lottieDrawable: LottieDrawable
            val startLottie = animation.optBoolean(START_LOTTIE, true)
            if (animation.has(LOTTIE_URL)) {
                val url = animation.getString(LOTTIE_URL)
                val jsonString: String?
                if (jsonStringCache.containsKey(url)) {
                    jsonString = jsonStringCache[url]
                } else {
                    jsonString = fileProviderInterface.readFromFile(context, url)
                    if (jsonString.isNullOrEmpty()) return
                    jsonStringCache[url] = jsonString
                }
                lottieDrawable = LottieDrawable()
                LottieCompositionFactory.fromJsonString(jsonString, url)
                    .addListener { result: LottieComposition? ->
                        lottieDrawable.setComposition(result)
                        view.background = lottieDrawable
                        if (startLottie) lottieDrawable.start() else lottieDrawable.stop()
                    }
            } else if (view.background is LottieDrawable) {
                lottieDrawable = view.background as LottieDrawable
            } else {
                return
            }
            if (animation.has(REPEAT_MODE)) {
                val isReverse = "reverse" == animation.optString(REPEAT_MODE, "")
                lottieDrawable.repeatMode =
                    if (isReverse) LottieDrawable.REVERSE else LottieDrawable.RESTART
            }
            if (animation.has(REPEAT_COUNT)) {
                val count = animation.optInt(REPEAT_COUNT, 0)
                if (count < 0) {
                    lottieDrawable.repeatCount =
                        LottieDrawable.INFINITE
                } else {
                    lottieDrawable.repeatCount = count
                }
            }
            if (animation.has(SPEED)) {
                val speed = animation.optDouble(SPEED, 1.0).toFloat()
                lottieDrawable.speed = speed
            }
            if (animation.has(MIN_FRAME)) {
                val frame = animation.optInt(MIN_FRAME, 0)
                if (frame >= 0) lottieDrawable.setMinFrame(frame)
            }
            if (animation.has(MAX_FRAME)) {
                val frame = animation.optInt(MAX_FRAME, 0)
                if (frame >= 0) lottieDrawable.setMaxFrame(frame)
            }
            if (animation.has(MIN_PROGRESS)) {
                val progress = animation.optDouble(MIN_PROGRESS, 0.0).toFloat()
                if (progress in 0f..1f) lottieDrawable.setMinProgress(progress)
            }
            if (animation.has(MAX_PROGRESS)) {
                val progress = animation.optDouble(MAX_PROGRESS, 0.0).toFloat()
                if (progress in 0f..1f) lottieDrawable.setMaxProgress(progress)
            }
            if (animation.has(SAFE_MODE)) {
                val safe = animation.optBoolean(SAFE_MODE, false)
                lottieDrawable.setSafeMode(safe)
            }
            if (animation.has(ALPHA)) {
                val alpha = animation.optInt(ALPHA, 255)
                if (alpha in 0..255) lottieDrawable.alpha = alpha
            }
            if (startLottie) lottieDrawable.start() else lottieDrawable.stop()
        } catch (ignored: Exception) {
        }
    }

    companion object {
        private const val LOTTIE_URL = "lottieUrl"
        private const val REPEAT_MODE = "repeatMode"
        private const val REPEAT_COUNT = "repeatCount"
        private const val SPEED = "speed"
        private const val MIN_FRAME = "minFrame"
        private const val MAX_FRAME = "maxFrame"
        private const val MIN_PROGRESS = "minProgress"
        private const val MAX_PROGRESS = "maxProgress"
        private const val SAFE_MODE = "safeMode"
        private const val ALPHA = "lottieAlpha"
        private const val START_LOTTIE = "startLottie"
    }
}
