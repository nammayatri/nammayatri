# Add project specific ProGuard rules here.
# You can control the set of applied configuration files using the
# proguardFiles setting in build.gradle.
#
# For more details, see
#   http://developer.android.com/guide/developing/tools/proguard.html

# If your project uses WebView with JS, uncomment the following
# and specify the fully qualified class name to the JavaScript interface
# class:
#-keepclassmembers class fqcn.of.javascript.interface.for.webview {
#   public *;
#}

# Uncomment this to preserve the line number information for
# debugging stack traces.
#-keepattributes SourceFile,LineNumberTable

# If you keep the line number information, uncomment this to
# hide the original source file name.
#-renamesourcefileattribute SourceFile

-keep class com.google.android.gms.auth.api.phone.SmsRetriever { *; }
-keep class com.google.android.gms.common.api.Status { *; }
-keep class com.facebook.shimmer.* { *;}
-keep class com.facebook.shimmer.Shimmer.* { *;}
-keep class androidx.coordinatorlayout.widget.* { *;}
-keep class androidx.coordinatorlayout.widget.CoordinatorLayout.* { *;}
-keep class com.airbnb.lottie.* { *;}
-keep class androidx.profileinstaller.** { *; }

-assumenosideeffects class android.util.Log {
    public static boolean isLoggable(java.lang.String, int);
    public static *** d(...);
    public static *** w(...);
    public static *** v(...);
    public static *** i(...);
}
-assumenosideeffects class android.util.Log { public * ; }

-assumenosideeffects class java.io.PrintStream {
     public void println(%);
     public void println(**);
 }

-keep public class * implements com.bumptech.glide.module.GlideModule
-keep public class * extends com.bumptech.glide.module.AppGlideModule
-keep public enum com.bumptech.glide.load.ImageHeaderParser$** {
  **[] $VALUES;
  public *;
}

 -dontwarn in.juspay.hypertrident.TridentBridge
 -dontwarn dalvik.system.ZipPathValidator
 -dontwarn co.hyperverge.**
 -dontwarn org.openjsse.net.ssl.OpenJSSE
 -dontwarn org.openjsse.javax.net.ssl.SSLParameters
 -dontwarn org.openjsse.javax.net.ssl.SSLSocket

 -dontwarn com.finternet.sdk.R$anim
 -dontwarn com.finternet.sdk.R$styleable

 -dontwarn com.aayushatharva.brotli4j.Brotli4jLoader
 -dontwarn com.aayushatharva.brotli4j.decoder.DecoderJNI$Status
 -dontwarn com.aayushatharva.brotli4j.decoder.DecoderJNI$Wrapper
 -dontwarn com.aayushatharva.brotli4j.encoder.BrotliEncoderChannel
 -dontwarn com.aayushatharva.brotli4j.encoder.Encoder$Mode
 -dontwarn com.aayushatharva.brotli4j.encoder.Encoder$Parameters
 -dontwarn com.clevertap.android.sdk.pushnotification.PushType
 -dontwarn com.facebook.proguard.annotations.DoNotStrip
 -dontwarn com.fasterxml.jackson.core.JsonFactory
 -dontwarn com.fasterxml.jackson.core.JsonGenerator
 -dontwarn com.fasterxml.jackson.core.JsonParser$Feature
 -dontwarn com.fasterxml.jackson.core.JsonParser
 -dontwarn com.fasterxml.jackson.core.JsonToken
 -dontwarn com.fasterxml.jackson.databind.JsonDeserializer
 -dontwarn com.fasterxml.jackson.databind.JsonSerializer
 -dontwarn com.fasterxml.jackson.databind.Module
 -dontwarn com.fasterxml.jackson.databind.ObjectMapper
 -dontwarn com.fasterxml.jackson.databind.SerializationFeature
 -dontwarn com.fasterxml.jackson.databind.module.SimpleModule
 -dontwarn com.github.luben.zstd.Zstd
 -dontwarn com.jcraft.jzlib.Deflater
 -dontwarn com.jcraft.jzlib.Inflater
 -dontwarn com.jcraft.jzlib.JZlib$WrapperType
 -dontwarn com.jcraft.jzlib.JZlib
 -dontwarn edu.umd.cs.findbugs.annotations.SuppressFBWarnings
 -dontwarn io.netty.channel.epoll.Epoll
 -dontwarn io.netty.channel.epoll.EpollChannelOption
 -dontwarn io.netty.channel.epoll.EpollDatagramChannel
 -dontwarn io.netty.channel.epoll.EpollDomainSocketChannel
 -dontwarn io.netty.channel.epoll.EpollEventLoopGroup
 -dontwarn io.netty.channel.epoll.EpollServerDomainSocketChannel
 -dontwarn io.netty.channel.epoll.EpollServerSocketChannel
 -dontwarn io.netty.channel.epoll.EpollSocketChannel
 -dontwarn io.netty.channel.kqueue.KQueue
 -dontwarn io.netty.channel.kqueue.KQueueChannelOption
 -dontwarn io.netty.channel.kqueue.KQueueDatagramChannel
 -dontwarn io.netty.channel.kqueue.KQueueDomainSocketChannel
 -dontwarn io.netty.channel.kqueue.KQueueEventLoopGroup
 -dontwarn io.netty.channel.kqueue.KQueueServerDomainSocketChannel
 -dontwarn io.netty.channel.kqueue.KQueueServerSocketChannel
 -dontwarn io.netty.channel.kqueue.KQueueSocketChannel
 -dontwarn io.netty.handler.codec.haproxy.HAProxyMessage
 -dontwarn io.netty.handler.codec.haproxy.HAProxyMessageDecoder
 -dontwarn io.netty.handler.codec.haproxy.HAProxyProxiedProtocol$TransportProtocol
 -dontwarn io.netty.handler.codec.haproxy.HAProxyProxiedProtocol
 -dontwarn io.netty.internal.tcnative.AsyncSSLPrivateKeyMethod
 -dontwarn io.netty.internal.tcnative.AsyncTask
 -dontwarn io.netty.internal.tcnative.Buffer
 -dontwarn io.netty.internal.tcnative.CertificateCallback
 -dontwarn io.netty.internal.tcnative.CertificateCompressionAlgo
 -dontwarn io.netty.internal.tcnative.CertificateVerifier
 -dontwarn io.netty.internal.tcnative.Library
 -dontwarn io.netty.internal.tcnative.SSL
 -dontwarn io.netty.internal.tcnative.SSLContext
 -dontwarn io.netty.internal.tcnative.SSLPrivateKeyMethod
 -dontwarn io.netty.internal.tcnative.SSLSession
 -dontwarn io.netty.internal.tcnative.SSLSessionCache
 -dontwarn io.netty.internal.tcnative.SessionTicketKey
 -dontwarn io.netty.internal.tcnative.SniHostNameMatcher
 -dontwarn io.vertx.codegen.annotations.CacheReturn
 -dontwarn io.vertx.codegen.annotations.DataObject
 -dontwarn io.vertx.codegen.annotations.Fluent
 -dontwarn io.vertx.codegen.annotations.GenIgnore
 -dontwarn io.vertx.codegen.annotations.VertxGen
 -dontwarn org.eclipse.jetty.alpn.ALPN$ClientProvider
 -dontwarn org.eclipse.jetty.alpn.ALPN$Provider
 -dontwarn org.eclipse.jetty.alpn.ALPN$ServerProvider
 -dontwarn org.eclipse.jetty.alpn.ALPN
 -dontwarn org.eclipse.jetty.npn.NextProtoNego$ClientProvider
 -dontwarn org.eclipse.jetty.npn.NextProtoNego$Provider
 -dontwarn org.eclipse.jetty.npn.NextProtoNego$ServerProvider
 -dontwarn org.eclipse.jetty.npn.NextProtoNego
 -dontwarn reactor.blockhound.integration.BlockHoundIntegration