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
 -dontwarn com.clevertap.android.sdk.pushnotification.PushType