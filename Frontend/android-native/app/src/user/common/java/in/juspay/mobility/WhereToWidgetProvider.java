package in.juspay.mobility;

import android.app.PendingIntent;
import android.appwidget.AppWidgetManager;
import android.appwidget.AppWidgetProvider;
import android.content.Context;
import android.content.Intent;
import android.util.DisplayMetrics;
import android.widget.RemoteViews;

public class WhereToWidgetProvider extends AppWidgetProvider {
    public void onUpdate(Context context, AppWidgetManager appWidgetManager, int[] appWidgetIds) {
        for (int i=0; i < appWidgetIds.length; i++) {
            int appWidgetId = appWidgetIds[i];

            Intent whereToIntent = new Intent(context, MainActivity.class);
            whereToIntent.putExtra("urlData", "WhereTo");
            whereToIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);

            PendingIntent wheretoPendingIntent = PendingIntent.getActivity(context, 2, whereToIntent, PendingIntent.FLAG_UPDATE_CURRENT | PendingIntent.FLAG_IMMUTABLE);

            RemoteViews views = new RemoteViews(context.getPackageName(), R.layout.whereto_appwidget_layout);
            views.setOnClickPendingIntent(R.id.button6, wheretoPendingIntent);

            appWidgetManager.updateAppWidget(appWidgetId, views);
        }
    }
}
