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

-keep class in.juspay.mobility.app.XiaomiMessageReceiver{*;}

#SDK has been obfuscated and compressed to avoid class not found error due to re-obfuscation.
-keep class com.xiaomi.**

#If the compiling Android version you are using is 23, you can prevent getting a false warning which makes it impossible to compile.
-dontwarn com.xiaomi.push.**

-keep class com.xiaomi.mipush.sdk.MiPushMessage {*;}
-keep class com.xiaomi.mipush.sdk.MiPushCommandMessage {*;}
-keep class com.xiaomi.mipush.sdk.PushMessageReceiver {*;}
-keep class com.xiaomi.mipush.sdk.MessageHandleService {*;}
-keep class com.xiaomi.push.service.XMJobService {*;}
-keep class com.xiaomi.push.service.XMPushService {*;}
-keep class com.xiaomi.mipush.sdk.PushMessageHandler {*;}
-keep class com.xiaomi.push.service.receivers.NetworkStatusReceiver {*;}
-keep class com.xiaomi.push.service.receivers.PingReceiver {*;}
-keep class com.xiaomi.mipush.sdk.NotificationClickedActivity {*;}