package in.juspay.mobility;

import android.app.PendingIntent;
import android.appwidget.AppWidgetManager;
import android.appwidget.AppWidgetProvider;
import android.appwidget.AppWidgetProviderInfo;
import android.content.Context;
import android.content.Intent;
import android.util.DisplayMetrics;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.RemoteViews;

import com.google.firebase.firestore.core.View;

public class HomeWidgetProvider extends AppWidgetProvider {
    public void onUpdate(Context context, AppWidgetManager appWidgetManager, int[] appWidgetIds) {
        // Perform this loop procedure for each widget that belongs to this
        // provider.
        for (int i=0; i < appWidgetIds.length; i++) {
            int appWidgetId = appWidgetIds[i];
//            AppWidgetProviderInfo
            //appWidgetManager.getAppWidgetInfo(appWidgetId).minHeight = pxToDp(180, context);
            //appWidgetManager.getAppWidgetInfo(appWidgetId).minWidth = pxToDp(328, context);
            Intent homeIntent = new Intent(context, MainActivity.class);
            homeIntent.putExtra("urlData", "Home");
            homeIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);

            Intent workIntent = new Intent(context, MainActivity.class);
            workIntent.putExtra("urlData", "Work");
            workIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);

            Intent whereToIntent = new Intent(context, MainActivity.class);
            whereToIntent.putExtra("urlData", "WhereTo");
            whereToIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);

            Intent favouriteIntent = new Intent(context, MainActivity.class);
            favouriteIntent.putExtra("urlData", "FAVOURITES");
            favouriteIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);

            PendingIntent homePendingIntent = PendingIntent.getActivity(context, 0, homeIntent, PendingIntent.FLAG_UPDATE_CURRENT | PendingIntent.FLAG_IMMUTABLE);
            PendingIntent workPendingIntent = PendingIntent.getActivity(context, 1, workIntent, PendingIntent.FLAG_UPDATE_CURRENT | PendingIntent.FLAG_IMMUTABLE);
            PendingIntent wheretoPendingIntent = PendingIntent.getActivity(context, 2, whereToIntent, PendingIntent.FLAG_UPDATE_CURRENT | PendingIntent.FLAG_IMMUTABLE);
            PendingIntent favouritePendingIntent = PendingIntent.getActivity(context, 3, favouriteIntent, PendingIntent.FLAG_UPDATE_CURRENT | PendingIntent.FLAG_IMMUTABLE);

            RemoteViews views = new RemoteViews(context.getPackageName(), R.layout.home_appwidget_layout);
            views.setOnClickPendingIntent(R.id.button2, homePendingIntent);
            views.setOnClickPendingIntent(R.id.button3, workPendingIntent);
            views.setOnClickPendingIntent(R.id.button4, wheretoPendingIntent);
            views.setOnClickPendingIntent(R.id.button5, favouritePendingIntent);

            appWidgetManager.updateAppWidget(appWidgetId, views);
        }
    }

    private int pxToDp(int px, Context context) {
        DisplayMetrics displayMetrics = context.getResources().getDisplayMetrics();
        return Math.round(px / (displayMetrics.xdpi / DisplayMetrics.DENSITY_DEFAULT));
    }
}
