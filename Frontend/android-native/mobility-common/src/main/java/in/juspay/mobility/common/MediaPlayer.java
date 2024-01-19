/*
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.common;

import static android.Manifest.permission.CAMERA;
import static android.Manifest.permission.READ_EXTERNAL_STORAGE;
import static android.Manifest.permission.RECORD_AUDIO;
import static android.Manifest.permission.WRITE_EXTERNAL_STORAGE;
import static android.content.Context.MODE_PRIVATE;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.graphics.Color;
import android.net.Uri;
import android.provider.MediaStore;
import android.util.Base64;
import android.util.Log;

import androidx.core.app.ActivityCompat;
import androidx.core.content.FileProvider;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Locale;

import in.juspay.hyper.core.BridgeComponents;
import in.juspay.hyper.core.ExecutorManager;
import in.juspay.hypersdk.data.KeyValueStore;
import in.juspay.mobility.app.services.MobilityCallAPI;
import in.juspay.mobility.common.mediaPlayer.DefaultMediaPlayerControl;

public class MediaPlayer {
    public boolean isUploadPopupOpen = false;
    public ArrayList<MediaPlayerView> audioPlayers = new ArrayList<>();
    public MediaPlayerView.AudioRecorder audioRecorder = null;
    private static final int IMAGE_CAPTURE_REQ_CODE = 101;
    private static final int IMAGE_PERMISSION_REQ_CODE = 4997;
    private BridgeComponents bridgeComponents = null;

    public MediaPlayer (BridgeComponents bridgeComponents){
        this.bridgeComponents = bridgeComponents;
    }

    public String saveAudioFile(String source) throws IOException {
        File sourceFile = new File(source);
        FileInputStream fis = new FileInputStream(sourceFile);
        File destFile = new File(bridgeComponents.getContext().getFilesDir().getAbsolutePath() + "/final_audio_record.mp3");
        FileOutputStream fos = new FileOutputStream(destFile);
        int n;
        while ((n = fis.read()) != -1) {
            fos.write(n);
        }
        fis.close();
        fos.close();
        return destFile.getAbsolutePath();
    }

    public String stopAudioRecording() {
        if (audioRecorder != null) {
            String res = audioRecorder.stopRecording();
            Log.d("AUDIO_RECORDING", "stopAudioRecording: " + res);
            audioRecorder = null;
            return res;
        }
        return null;
    }

    public void uploadFile() { 
        if (!isUploadPopupOpen) {
            ExecutorManager.runOnMainThread(() -> {
                Context context = bridgeComponents.getContext();
                if ((ActivityCompat.checkSelfPermission(context.getApplicationContext(), CAMERA) == PackageManager.PERMISSION_GRANTED)) {
                    if (bridgeComponents.getActivity() != null) {
                        Intent takePicture = new Intent(MediaStore.ACTION_IMAGE_CAPTURE);
                        String timeStamp = new SimpleDateFormat("yyyyMMdd_HHmmss", Locale.getDefault()).format(new Date());
                        KeyValueStore.write(context, bridgeComponents.getSdkName(), context.getResources().getString(R.string.TIME_STAMP_FILE_UPLOAD), timeStamp);
                        Uri photoFile = FileProvider.getUriForFile(context, context.getPackageName() + ".provider", new File(context.getFilesDir(), "IMG_" + timeStamp + ".jpg"));
                        takePicture.putExtra(MediaStore.EXTRA_OUTPUT, photoFile);
                        Intent chooseFromFile = new Intent(Intent.ACTION_GET_CONTENT);
                        chooseFromFile.setType("image/*");
                        Intent chooser = Intent.createChooser(takePicture, context.getString(R.string.upload_image));
                        chooser.putExtra(Intent.EXTRA_INITIAL_INTENTS, new Intent[]{chooseFromFile});
                        isUploadPopupOpen = true;
                        bridgeComponents.getActivity().startActivityForResult(chooser, IMAGE_CAPTURE_REQ_CODE, null);
                    }
                } else {
                    if (bridgeComponents.getActivity() != null) {
                        ActivityCompat.requestPermissions(bridgeComponents.getActivity(), new String[]{CAMERA, READ_EXTERNAL_STORAGE, WRITE_EXTERNAL_STORAGE}, IMAGE_PERMISSION_REQ_CODE);
                    }
                }
            });
        }
    }

    public boolean isMicrophonePermissionEnabled() {
        return ActivityCompat.checkSelfPermission(bridgeComponents.getContext(), RECORD_AUDIO) == PackageManager.PERMISSION_GRANTED;
    }

    public boolean startAudioRecording() {
        if (isMicrophonePermissionEnabled()) {
            audioRecorder = new MediaPlayerView.AudioRecorder();
            audioRecorder.startRecording(bridgeComponents.getContext());
            System.out.println("Started recording");
            return true;
        } else {
            if (bridgeComponents.getActivity() != null) {
                ActivityCompat.requestPermissions(bridgeComponents.getActivity(), new String[]{RECORD_AUDIO}, MediaPlayerView.AudioRecorder.REQUEST_RECORD_AUDIO_PERMISSION);
            }
            return false;
        }
    }


    public void removeMediaPlayer() {
        try {
            if (audioPlayers != null) {
                for (MediaPlayerView audioPlayer : audioPlayers) {
                    audioPlayer.resetListeners();
                }
                bridgeComponents.getContext().getCacheDir().delete();
                audioPlayers.clear();
                DefaultMediaPlayerControl.mediaPlayer.reset();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void pauseMediaPlayer() {
        ExecutorManager.runOnBackgroundThread(() -> {
        if (DefaultMediaPlayerControl.mediaPlayer.isPlaying()) {
            DefaultMediaPlayerControl.mediaPlayer.pause();
        }
        for (MediaPlayerView audioPlayer : audioPlayers) {
            audioPlayer.onPause(audioPlayer.getPlayer());
        }
        });
    }

    public void addMediaPlayer(String viewID, String source) {
        ExecutorManager.runOnMainThread(() -> {
            MediaPlayerView audioPlayer = new MediaPlayerView(bridgeComponents.getContext(), bridgeComponents.getActivity());
            try {
                audioPlayer.inflateView(Integer.parseInt(viewID));
                if (source.contains(".mp3")) {
                    audioPlayer.addAudioFileUrl(source);
                } else {
                    Thread thread = new Thread(() -> {
                        try {
                            String base64 = MobilityCallAPI.callAPI(source, MobilityCallAPI.getBaseHeaders(bridgeComponents.getContext()), null, "GET", false).getResponseBody();
                            byte[] decodedAudio = Base64.decode(base64, Base64.DEFAULT);
                            File tempMp3 = File.createTempFile("audio_cache", "mp3", bridgeComponents.getContext().getCacheDir());
                            tempMp3.deleteOnExit();
                            FileOutputStream fos = new FileOutputStream(tempMp3);
                            fos.write(decodedAudio);
                            fos.close();
                            FileInputStream fis = new FileInputStream(tempMp3);
                            audioPlayer.addAudioFileInput(fis);
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                    });
                    thread.start();
                }
                audioPlayers.add(audioPlayer);
            } catch (IOException e) {
                e.printStackTrace();
            }
        });
    }

    public void addMediaFile(String viewID, String source, String actionPlayerID, String playIcon, String pauseIcon, String timerID) {
        Log.d("ADD_MEDIA_FILE", "addMediaFile: " + source);
        Context context = bridgeComponents.getContext();
        Activity activity = bridgeComponents.getActivity();
        ExecutorManager.runOnMainThread(() -> {
            MediaPlayerView audioPlayer;
            if (Integer.parseInt(actionPlayerID) != -1) {
                if (Integer.parseInt(timerID) != -1) {
                    audioPlayer = new MediaPlayerView(context, activity, Integer.parseInt(actionPlayerID), playIcon, pauseIcon, Integer.parseInt(timerID));
                    audioPlayer.setTimerColorAndSize(Color.WHITE, 14);
                    audioPlayer.setVisualizerBarPlayedColor(Color.WHITE);
                } else {
                    audioPlayer = new MediaPlayerView(context, activity, Integer.parseInt(actionPlayerID), playIcon, pauseIcon);
                    audioPlayer.setTimerColorAndSize(Color.GRAY, 14);
                }
            } else {
                audioPlayer = new MediaPlayerView(context, activity);
            }
            try {
                audioPlayer.inflateView(Integer.parseInt(viewID));
                if (source.startsWith("http")) {
                    Thread thread = new Thread(() -> {
                        try {
                            String base64 = MobilityCallAPI.callAPI(source, MobilityCallAPI.getBaseHeaders(bridgeComponents.getContext()), null, "GET", false).getResponseBody();
                            byte[] decodedAudio = Base64.decode(base64, Base64.DEFAULT);
                            File tempMp3 = File.createTempFile("audio_cache", source.substring(source.length()-3), context.getCacheDir());
                            tempMp3.deleteOnExit();
                            FileOutputStream fos = new FileOutputStream(tempMp3);
                            fos.write(decodedAudio);
                            fos.close();
                            FileInputStream fis = new FileInputStream(tempMp3);
                            audioPlayer.addAudioFileInput(fis);
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                    });
                    thread.start();
                } else {
                    File file = new File(source);
                    FileInputStream fis = new FileInputStream(file);
                    audioPlayer.addAudioFileInput(fis);
                }
                audioPlayers.add(audioPlayer);
            } catch (IOException e) {
                e.printStackTrace();
            }
        });
    }
}
