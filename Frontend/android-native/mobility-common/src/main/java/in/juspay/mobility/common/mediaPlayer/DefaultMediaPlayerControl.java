/*
 * Copyright 2022-23, Juspay
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 * as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 * is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 * the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */
package in.juspay.mobility.common.mediaPlayer;

import android.content.Context;
import android.media.MediaPlayer;
import android.net.Uri;
import android.os.Handler;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.concurrent.atomic.AtomicLong;

public class DefaultMediaPlayerControl implements MediaPlayerControl {

    public static MediaPlayer mediaPlayer = new MediaPlayer();
    private Handler handler = new Handler();
    private Runnable runnable;
    private AtomicLong durationCounter = new AtomicLong();

    private MediaPlayerOnPreparedListener onPrepariedListener;
    private MediaPlayerOnPauseListener onPauseListener;
    private MediaPlayerOnPlayListener onPlayListener;
    private MediaPlayerOnDurationListener onDurationListener;
    private MediaPlayerOnCompleteListener onCompleteListener;

    private final long INTERVAL = 1000;

    @Override
    public void preparePlayer() {
        mediaPlayer.prepareAsync();
        mediaPlayer.setVolume(1.0f, 1.0f);
        mediaPlayer.setOnPreparedListener(new MediaPlayer.OnPreparedListener() {
            @Override
            public void onPrepared(MediaPlayer mp) {
                if (onPrepariedListener != null) {
                    onPrepariedListener.onPrepared(DefaultMediaPlayerControl.this);
                }
            }
        });

        runnable = new Runnable() {
            @Override
            public void run() {
                if (onDurationListener != null)
                    onDurationListener.onDurationProgress(DefaultMediaPlayerControl.this, getDuration(), durationCounter.addAndGet(INTERVAL));
                if (mediaPlayer.isPlaying()) {
                    handler.postDelayed(this, INTERVAL);
                }
            }
        };
        mediaPlayer.setOnCompletionListener(new MediaPlayer.OnCompletionListener() {
            @Override
            public void onCompletion(MediaPlayer mp) {
                if (onCompleteListener != null)
                    onCompleteListener.onComplete(DefaultMediaPlayerControl.this);

                durationCounter.set(0);
                handler.removeCallbacks(runnable);
            }
        });
    }

    @Override
    public MediaPlayerControl setOnPrepariedListener(MediaPlayerOnPreparedListener onPrepariedListener) {
        this.onPrepariedListener = onPrepariedListener;

        return this;
    }

    @Override
    public MediaPlayerControl setOnPauseListener(MediaPlayerOnPauseListener onPauseListener) {
        this.onPauseListener = onPauseListener;

        return this;
    }

    @Override
    public MediaPlayerControl setOnPlayListener(MediaPlayerOnPlayListener onPlayListener) {
        this.onPlayListener = onPlayListener;

        return this;
    }

    @Override
    public MediaPlayerControl setOnDurationListener(MediaPlayerOnDurationListener onDurationListener) {
        this.onDurationListener = onDurationListener;

        return this;
    }

    @Override
    public MediaPlayerControl setOnCompleteListener(MediaPlayerOnCompleteListener onCompleteListener) {
        this.onCompleteListener = onCompleteListener;

        return this;
    }

    @Override
    public void setAudioSource(Context context, Uri uri) throws IOException {
        mediaPlayer.setDataSource(context, uri);

        preparePlayer();
    }

    @Override
    public void setAudioSource(String url) throws IOException {
        mediaPlayer.setDataSource(url);

        preparePlayer();
    }

    @Override
    public void setAudioSource(FileInputStream file) throws IOException {
        mediaPlayer.reset();
        mediaPlayer.setDataSource(file.getFD());
        preparePlayer();
    }

    @Override
    public void play() {
        mediaPlayer.start();
        if (onPlayListener != null)
            onPlayListener.onPlay(this);
        handler.postDelayed(runnable, INTERVAL);
    }

    @Override
    public void pause() {
        mediaPlayer.pause();
        if (onPauseListener != null)
            onPauseListener.onPause(this);
    }

    @Override
    public void stop() {
        if (handler != null)
            handler.removeCallbacks(runnable);
        mediaPlayer.stop();
        mediaPlayer.reset();
        if (durationCounter != null)
            durationCounter.set(0);
    }

    @Override
    public void toggle() {
        if (mediaPlayer.isPlaying()) {
            pause();
        } else {
            play();
        }
    }

    @Override
    public boolean isPlaying() {
        return mediaPlayer.isPlaying();
    }

    @Override
    public long getDuration() {
        try{
            return mediaPlayer.getDuration();
        }catch (Exception e){
            return 0;
        }
    }
}
