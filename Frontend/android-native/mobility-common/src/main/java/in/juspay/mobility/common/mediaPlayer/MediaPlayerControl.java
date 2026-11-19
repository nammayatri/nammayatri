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
import android.net.Uri;

import java.io.FileInputStream;
import java.io.IOException;

public interface MediaPlayerControl {

    public void preparePlayer();

    public MediaPlayerControl setOnPrepariedListener(MediaPlayerOnPreparedListener onPrepariedListener);

    public MediaPlayerControl setOnPauseListener(MediaPlayerOnPauseListener onPauseListener);

    public MediaPlayerControl setOnPlayListener(MediaPlayerOnPlayListener onPlayListener);

    public MediaPlayerControl setOnDurationListener(MediaPlayerOnDurationListener onDurationListener);

    public MediaPlayerControl setOnCompleteListener(MediaPlayerOnCompleteListener onCompleteListener);

    public void setAudioSource(Context context, Uri uri) throws IOException;

    public void setAudioSource(String url) throws IOException;

    public void setAudioSource(FileInputStream file) throws IOException;

    public void play();

    public void pause();

    public void stop();

    public void toggle();

    public boolean isPlaying();

    public long getDuration();

}
