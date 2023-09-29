/*
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.app;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.AsyncTask.Status;
import android.os.Build;
import android.util.AttributeSet;
import android.view.View;

import androidx.annotation.ColorRes;
import androidx.annotation.Nullable;
import androidx.annotation.RequiresApi;
import androidx.core.content.ContextCompat;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;

public class SoundVisualizerBarView extends View {

    /**
     * constant value for Height of the bar
     */
    public static final int VISUALIZER_HEIGHT = 20;

    /**
     * bytes array converted from file.
     */
    private byte[] bytes;

    public AsyncTask task;
    OnTaskCompleteListener onTaskCompleteListener;

    public OnTaskCompleteListener getOnTaskCompleteListener() {
        return onTaskCompleteListener;
    }

    public void setOnTaskCompleteListener(OnTaskCompleteListener onTaskCompleteListener) {
        this.onTaskCompleteListener = onTaskCompleteListener;
    }

    /**
     * Percentage of audio sample scale
     * Should updated dynamically while audioPlayer is played
     */
    private float denseness;

    /**
     * Canvas painting for sample scale, filling played part of audio sample
     */
    private Paint playedStatePainting = new Paint();
    /**
     * Canvas painting for sample scale, filling not played part of audio sample
     */
    private Paint notPlayedStatePainting = new Paint();

    private int width;
    private int height;

    private int playedStateColor;
    private int nonPlayedStateColor;

    private final Context context;

    public SoundVisualizerBarView(Context context) {
        super(context);
        this.context = context;
        init();
    }

    public SoundVisualizerBarView(Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        this.context = context;

        TypedArray a = context.obtainStyledAttributes(attrs,
                R.styleable.SoundVisualizerBarView, 0, 0);
        playedStateColor = a.getColor(R.styleable.SoundVisualizerBarView_statePlayingColor,
                ContextCompat.getColor(context, R.color.Black500));
        nonPlayedStateColor = a.getColor(R.styleable.SoundVisualizerBarView_stateNonPlayingColor,
                ContextCompat.getColor(context, R.color.blue700));

        a.recycle();

        init();
    }

    public void setPlayedStateColor(@ColorRes int playedStateColor) {
        this.playedStateColor = playedStateColor;
        init();
    }

    public void setNonPlayedStateColor(@ColorRes int nonPlayedStateColor) {
        this.nonPlayedStateColor = nonPlayedStateColor;
        init();
    }

    private void init() {
        bytes = null;

        playedStatePainting.setStrokeWidth(1f);
        playedStatePainting.setAntiAlias(true);
        playedStatePainting.setColor(playedStateColor);
        notPlayedStatePainting.setStrokeWidth(1f);
        notPlayedStatePainting.setAntiAlias(true);
        notPlayedStatePainting.setColor(nonPlayedStateColor);
    }

    /**
     * update and redraw Visualizer view
     */
    public void updateVisualizer(Uri uri) {
        try {
            InputStream inputStream = context.getContentResolver().openInputStream(uri);
            updateVisualizer(inputStream);
            onTaskCompleteListener.onTaskComplete();
        } catch (Exception e) {

        }
    }

    /**
     * update and redraw Visualizer view
     */
    public void updateVisualizer(String url) throws IOException {
        cancelTask();
        task = new AsyncTask() {
            @Override
            protected Object doInBackground(Object[] objects) {
                try {
                    URLConnection connection = new URL(url).openConnection();
                    connection.connect();
                    updateVisualizer(connection.getInputStream());
                } catch (Exception e) {

                }
                return null;
            }

            @Override
            protected void onPostExecute(Object o) {
                super.onPostExecute(o);
                onTaskCompleteListener.onTaskComplete();
            }
        };
        task.execute();
    }

    /**
     * update and redraw Visualizer view
     */
    public void updateVisualizer(InputStream inputStream) {
        this.bytes = readInputStream(inputStream);
        invalidate();
    }

    /**
     * update and redraw Visualizer view
     */
    public void updateVisualizer(byte[] bytes) {
        this.bytes = bytes;
        invalidate();
    }

    /**
     * Update player percent. 0 - file not played, 1 - full played
     *
     * @param percent
     */
    public void updatePlayerPercent(float percent) {
        denseness = (int) Math.ceil(width * percent);
        if (denseness < 0) {
            denseness = 0;
        } else if (denseness > width) {
            denseness = width;
        }
        invalidate();
    }

    public void cancelTask() {
        if (task != null && task.getStatus() != Status.FINISHED) {
            task.cancel(true);
        }
    }

    @Override
    protected void onLayout(boolean changed, int left, int top, int right, int bottom) {
        super.onLayout(changed, left, top, right, bottom);
        width = getMeasuredWidth();
        height = getMeasuredHeight();
    }

    @RequiresApi(api = Build.VERSION_CODES.LOLLIPOP)
    @Override
    protected void onDraw(Canvas canvas) {
        super.onDraw(canvas);
        if (bytes == null || width == 0) {
            return;
        }
        float totalBarsCount = width / dp(3);
        if (totalBarsCount <= 0.1f) {
            return;
        }
        byte value;
        int samplesCount = (bytes.length * 8 / 5);
        float samplesPerBar = samplesCount / totalBarsCount;
        float barCounter = 0;
        int nextBarNum = 0;

        int y = (height - dp(VISUALIZER_HEIGHT)) / 2;
        int barNum = 0;
        int lastBarNum;
        int drawBarCount;

        for (int a = 0; a < samplesCount; a++) {
            if (a != nextBarNum) {
                continue;
            }
            drawBarCount = 0;
            lastBarNum = nextBarNum;
            while (lastBarNum == nextBarNum) {
                barCounter += samplesPerBar;
                nextBarNum = (int) barCounter;
                drawBarCount++;
            }

            int bitPointer = a * 5;
            int byteNum = bitPointer / Byte.SIZE;
            int byteBitOffset = bitPointer - byteNum * Byte.SIZE;
            int currentByteCount = Byte.SIZE - byteBitOffset;
            int nextByteRest = 5 - currentByteCount;
            value = (byte) ((bytes[byteNum] >> byteBitOffset) & ((2 << (Math.min(5, currentByteCount) - 1)) - 1));
            if (nextByteRest > 0) {
                value <<= nextByteRest;
                value |= bytes[byteNum + 1] & ((2 << (nextByteRest - 1)) - 1);
            }

            for (int b = 0; b < drawBarCount; b++) {
                float left = barNum * dp(4);
                float top = (float) (y + dp(VISUALIZER_HEIGHT - Math.max(1, VISUALIZER_HEIGHT * value / 30.0f)));
                float right = left + dp(3);
                float bottom = (float) (y / 1.1 + dp(Math.max(1, VISUALIZER_HEIGHT * value / 30.0f)));
                if (left < denseness && left + dp(2) < denseness) {
                    canvas.drawRoundRect(left, top, right, bottom, 10.5f, 10.5f, notPlayedStatePainting);
                } else {
                    canvas.drawRoundRect(left, top, right, bottom, 10.5f, 10.5f, playedStatePainting);
                    if (left < denseness) {
                        canvas.drawRoundRect(left, top, right, bottom, 10.5f, 10.5f, notPlayedStatePainting);
                    }
                }
                barNum++;
            }
        }
    }

    public int dp(float value) {
        if (value == 0) {
            return 0;
        }
        return (int) Math.ceil(getContext().getResources().getDisplayMetrics().density * value);
    }

    private byte[] readInputStream(InputStream inputStream) {
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();

        byte buf[] = new byte[1024];
        int len;
        try {
            while ((len = inputStream.read(buf)) != -1) {
                outputStream.write(buf, 0, len);
            }
            outputStream.close();
            inputStream.close();
        } catch (IOException e) {

        }
        return outputStream.toByteArray();
    }
}
