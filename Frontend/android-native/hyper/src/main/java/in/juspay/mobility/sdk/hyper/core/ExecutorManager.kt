package `in`.juspay.mobility.sdk.hyper.core

import android.os.Handler
import android.os.Looper
import java.util.concurrent.Executors

object ExecutorManager {
    private val logsPool = Executors.newSingleThreadExecutor()
    private val remoteAssetsPool = Executors.newSingleThreadExecutor()
    private val jsThread = Executors.newSingleThreadExecutor();
    private val sharedPool = Executors.newFixedThreadPool(4) // Should this be configurable
    private val sharedApiPool = Executors.newFixedThreadPool(10) // Should this be configurable
    private val sdkTrackerPool = Executors.newSingleThreadExecutor()
    private val logSessioniserPool = Executors.newSingleThreadExecutor()
    private val logPusherPool = Executors.newSingleThreadExecutor()
    private var logsThreadId: String? = null
    private var trackerThreadId: String? = null

    @JvmStatic
    fun setLogsThreadId(threadId: Long) {
        logsThreadId = threadId.toString()
    }

    @JvmStatic
    fun setTrackerThreadId(threadId: Long) {
        trackerThreadId = threadId.toString()
    }

    @JvmStatic
    fun runOnApiThread(task: Runnable?) {
        sharedApiPool.execute(task)
    }

    @JvmStatic
    fun postOnMainThread(milliSecond: Long, task: Runnable) {
        val handler = Handler(Looper.getMainLooper())
        handler.postDelayed(task, milliSecond)
    }

    @JvmStatic
    fun runOnMainThread(task: Runnable) {
        if (Looper.myLooper() != Looper.getMainLooper()) {
            val handler = Handler(Looper.getMainLooper())
            handler.post(task)
        } else {
            task.run()
        }
    }

    @JvmStatic
    fun runOnJSThread(task: Runnable) {
        jsThread.execute(task);
    }

    @JvmStatic
    fun runOnBackgroundThread(task: Runnable?) {
        sharedPool.execute(task)
    }

    @JvmStatic
    fun runOnLogsPool(task: Runnable) {
        if (logsThreadId == Thread.currentThread().id.toString()) {
            task.run()
        } else {
            logsPool.execute(task)
        }
    }

    @JvmStatic
    fun runOnSdkTrackerPool(task: Runnable) {
        if (trackerThreadId == Thread.currentThread().id.toString()) {
            task.run()
        } else {
            sdkTrackerPool.execute(task)
        }
    }

    @JvmStatic
    fun runOnLogSessioniserThread(task: Runnable?) {
        logSessioniserPool.execute(task)
    }

    @JvmStatic
    fun runOnLogPusherThread(task: Runnable?) {
        logPusherPool.execute(task)
    }

    @Suppress("unused")
    fun runOnRemoteAssetsPool(task: Runnable?) {
        remoteAssetsPool.execute(task)
    }
}
