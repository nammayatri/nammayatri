package `in`.juspay.hyper.core

import android.content.Context

interface FileProviderInterface {
    fun readFromFile(context: Context, fileName: String): String
    fun renewFile(endPoint: String, fileName: String, startTime: Long)
}
