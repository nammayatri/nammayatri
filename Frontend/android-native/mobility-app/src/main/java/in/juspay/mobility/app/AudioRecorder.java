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
import android.media.MediaRecorder;
import android.util.Log;

public class AudioRecorder {
    private static final String LOG_TAG = "AudioRecordTest";
    public static final int REQUEST_RECORD_AUDIO_PERMISSION = 200;
    private static String fileName = null;
    private MediaRecorder recorder = null;

    public void startRecording(Context context) {
        Log.d(LOG_TAG, "Recording in audio recorder");
        fileName = context.getFilesDir().getAbsolutePath() + "/namma_yatri_audio_record.mp3";
        recorder = new MediaRecorder();
        recorder.setAudioSource(MediaRecorder.AudioSource.MIC);
        recorder.setOutputFormat(MediaRecorder.OutputFormat.MPEG_4);
        recorder.setOutputFile(fileName);
        recorder.setAudioEncoder(MediaRecorder.AudioEncoder.AAC);
        try {
            recorder.prepare();
        } catch (Exception e) {
            e.printStackTrace();
        }
        recorder.start();
    }

    public String stopRecording() {
        if (recorder != null) {
            recorder.stop();
            recorder.release();
            recorder = null;
            return fileName;
        }
        return null;
    }
}
