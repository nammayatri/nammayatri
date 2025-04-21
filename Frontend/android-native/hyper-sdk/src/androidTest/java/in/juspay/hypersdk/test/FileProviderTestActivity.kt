@file:Suppress("ktlint:standard:filename")

package `in`.juspay.hypersdk.test

import android.content.Context
import androidx.test.ext.junit.runners.AndroidJUnit4
import androidx.test.platform.app.InstrumentationRegistry
import `in`.juspay.hyper.core.JuspayCoreLib
import `in`.juspay.hypersdk.core.JuspayServices
import `in`.juspay.hypersdk.utils.IntegrationUtils
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotEquals
import org.junit.Assert.assertNotNull
import org.junit.Before
import org.junit.Test
import org.junit.runner.RunWith
import java.io.File

/**
 * Instrumented test, which will execute on an Android device.
 *
 * See [testing documentation](http://d.android.com/tools/testing).
 */
@RunWith(AndroidJUnit4::class)
class FileProviderTestActivity {
    companion object {
        val CTX: Context = InstrumentationRegistry
            .getInstrumentation()
            .targetContext

        // File names contain the non-rc SDK version, trimming rc version here.
        val SDK_VERSION: String = IntegrationUtils
            .getSdkVersion(CTX)
            .split("-")[0]
    }

    init {
        JuspayCoreLib.setApplicationContext(CTX)
    }

    val fileProviderService = JuspayServices(CTX, null).fileProviderService

    private fun assertNotEmpty(text: String?) {
        assertNotNull(text)
        text?.let { assert(it.isNotEmpty()) }
    }

    @Before
    fun cleanUp() {
        // Running clean up before test otherwise, previous tests can
        // break the assumptions for a new one.
        CTX.getDir("juspay", Context.MODE_PRIVATE).deleteRecursively()
    }

    @Test
    fun readPackagedFile() {
        val packaged = fileProviderService
            .readFromFile(CTX, "packaged.txt")
        val expected = CTX.assets.open("juspay/packaged.txt")
            .bufferedReader()
            .readText()
        assertNotEmpty(expected)
        assertEquals(expected, packaged)
    }

    @Test
    fun readPackagedJSAFile() {
        val conifg = fileProviderService
            .readFromFile(CTX, "v1-configuration.jsa")
        assertNotEmpty(conifg)
    }

    @Test
    fun cacheWhitelistIsRespected() {
        val fileName = "v1-configuration.jsa"
        fileProviderService.addToFileCacheWhiteList(fileName)
        val text = fileProviderService.readFromFile(CTX, fileName)
        assertNotEmpty(text)
        assertEquals(text, fileProviderService.readFromCache(fileName))
    }

    @Test
    fun newFileGetsCreatedOnDisk() {
        val text = "HELLO WORLD!"
        fileProviderService.updateFile(CTX, "hello.txt", text.toByteArray())
        val dir = CTX.getDir("juspay", Context.MODE_PRIVATE)
        val newFile = File(dir, "hello_godel_$SDK_VERSION.txt")
        assertEquals(newFile.readText(), text)
    }

    @Test
    fun newerFileOverridesPackagedVersion() {
        val filePath = "packaged.txt"
        val newText = "New file!"
        val oldText = fileProviderService.readFromFile(CTX, filePath)
        assertNotEmpty(oldText)
        assertNotEquals(oldText, newText)
        fileProviderService.updateFile(CTX, filePath, newText.toByteArray())
        assertEquals(fileProviderService.readFromFile(CTX, filePath), newText)
    }
}
