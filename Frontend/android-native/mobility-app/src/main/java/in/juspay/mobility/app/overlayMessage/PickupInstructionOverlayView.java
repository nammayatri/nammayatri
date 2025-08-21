package in.juspay.mobility.app.overlayMessage;

import android.content.Context;
import android.content.Intent;
import android.media.MediaPlayer;
import android.os.Handler;
import android.os.Looper;
import android.util.Base64;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.ImageButton;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.appcompat.view.ContextThemeWrapper;

import java.io.BufferedInputStream;
// Removed unused ByteArray* imports
import java.io.FileInputStream;
// Removed unused InputStream import
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.UnknownHostException;
import java.net.SocketTimeoutException;
import java.net.SocketException;
// Removed unused URLConnection and certificate imports; HttpsURLConnection not used
import javax.net.ssl.SSLException;

import com.airbnb.lottie.LottieAnimationView;
// Removed unused ShapeableImageView import
import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;

import org.json.JSONException;
import org.json.JSONObject;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.List;
import java.util.Map;

import in.juspay.mobility.app.BuildConfig;
import in.juspay.mobility.app.R;
import in.juspay.mobility.app.RideRequestUtils;

public class PickupInstructionOverlayView implements ViewInterface {
    private static final String TAG = "PICKUP_FCM_InstructionView";
    
    private View view;
    private Context context;
    private ServiceInterface serviceInterface;
    private MediaPlayer mediaPlayer;
    private Button mainActionButton;
    private TextView pickupInstructionContent;
    private TextView textNoteLabel;
    private TextView audioNoteLabel;
    private View audioSection;
    private View textNoteSection;
    private ImageButton audioControlButton;
    private LottieAnimationView audioVisualizer; // Keep as View to handle both types safely
    private TextView replayButton;
    private ImageButton arrowButton;
    private TextView audioTitle;
    
    private enum AudioState {
        NOT_STARTED,
        PLAYING,
        COMPLETED
    }
    
    private AudioState currentAudioState = AudioState.NOT_STARTED;
    
    // Data model to match backend payload
    private static class PickupInstructionData {
        public String instruction;
        public String audioUrl;  // Changed from audioBase64 to audioUrl for new payload format
        public String senderName;
    }
    
    @Override
    public View createView(String bundle, Context context, ServiceInterface serviceInterface) {
        this.context = context;
        this.serviceInterface = serviceInterface;
        
        try {
            // Received raw bundle
            
            // Parse the JSON data
            PickupInstructionData data = parseData(bundle);
            
            // Parsed
            
            // Using AppCompatContext for Material components compatibility
            Context wrappedContext = new ContextThemeWrapper(context, 
                    androidx.appcompat.R.style.Theme_AppCompat_Light_DarkActionBar);
            
            // Create a fallback view in case of inflation failure
            TextView fallbackView = new TextView(context);
            fallbackView.setText(context.getString(R.string.pickup_instruction_fallback));
            fallbackView.setPadding(20, 20, 20, 20);
            
            try {
                // Inflate the layout with the wrapped context
                view = LayoutInflater.from(wrappedContext).inflate(
                        R.layout.pickup_instruction_overlay, null);
                
                if (view == null) {
                    Log.e(TAG, "Failed to inflate view");
                    return fallbackView;
                }
                
                // Initialize views
                initializeViews();
                
                // Set data to views
                if (data != null) {
                    populateViews(data);
                    setupButtonListeners(data);
                } else {
                    // Handle error state - empty or invalid data
                    if (pickupInstructionContent != null) {
                        pickupInstructionContent.setText("No pickup instructions available");
                    }
                    if (mainActionButton != null) {
                        mainActionButton.setText("Close");
                        mainActionButton.setOnClickListener(v -> serviceInterface.killService());
                    }
                }
            } catch (Exception e) {
                Log.e(TAG, "Error inflating view: " + e.getMessage());
                return fallbackView;
            }
        } catch (Exception e) {
            Log.e(TAG, "Error creating view: " + e.getMessage());
            // Create a simple fallback view in case of failure
            TextView textView = new TextView(context);
            textView.setText(context.getString(R.string.pickup_instruction_error));
            textView.setPadding(20, 20, 20, 20);
            return textView;
        }
        
        return view;
    }
    
    @Override
    public void destroyView() {
        if (mediaPlayer != null) {
            try {
                if (mediaPlayer.isPlaying()) {
                    mediaPlayer.stop();
                }
                mediaPlayer.release();
            } catch (Exception e) {
                Log.e(TAG, "Error releasing MediaPlayer: " + e.getMessage());
            }
            mediaPlayer = null;
        }
        view = null;
    }
    
    @Override
    public View getView() {
        return view;
    }
    
    private PickupInstructionData parseData(String bundle) {
        try {
            // Parse pickup instruction payload
            
            // First try to parse as direct object
            Gson gson = new Gson();
            try {
                PickupInstructionData data = gson.fromJson(bundle, PickupInstructionData.class);
                // Parsed via Gson
                
                // If data parsed successfully but audioUrl is null, let's check if maybe audioUrl was nested
                if (data != null && (data.audioUrl == null || data.audioUrl.isEmpty())) {
                    // Check nested entity_data
                    JSONObject jsonObject = new JSONObject(bundle);
                    
                    // If the bundle has entity_data, it might be a wrapped object from FCM
                    if (jsonObject.has("entity_data") && !jsonObject.isNull("entity_data")) {
                        String entityData = jsonObject.getString("entity_data");
                        // Found embedded entity_data
                        
                        try {
                            JSONObject entityJson = new JSONObject(entityData);
                            if (entityJson.has("audioUrl") && !entityJson.isNull("audioUrl")) {
                                data.audioUrl = entityJson.getString("audioUrl");
                                // audioUrl from entity_data
                            }
                            
                            // Also check for instruction in entity_data if not already set
                            if ((data.instruction == null || data.instruction.isEmpty()) && 
                                entityJson.has("instruction") && !entityJson.isNull("instruction")) {
                                data.instruction = entityJson.getString("instruction");
                                // instruction from entity_data
                            }
                        } catch (Exception nestedEx) {
                            Log.e(TAG, "Error parsing entity_data: " + nestedEx.getMessage());
                        }
                    }
                }
                
                return data;
            } catch (JsonSyntaxException e) {
                Log.d(TAG, "Gson parsing failed, trying JSONObject");
                
                // If fails, try to parse as JSON object and extract fields
                JSONObject jsonObject = new JSONObject(bundle);
                // JSONObject path
                
                PickupInstructionData data = new PickupInstructionData();
                
                // Check if this is entity_data from FCM
                if (jsonObject.has("entity_data") && !jsonObject.isNull("entity_data")) {
                    try {
                        // Try to parse entity_data as a JSON string
                        String entityData = jsonObject.getString("entity_data");
                        // Found embedded entity_data
                        JSONObject entityJson = new JSONObject(entityData);
                        
                        if (entityJson.has("instruction") && !entityJson.isNull("instruction")) {
                            data.instruction = entityJson.getString("instruction");
                            // instruction from entity_data
                        }
                        
                        if (entityJson.has("audioUrl") && !entityJson.isNull("audioUrl")) {
                            data.audioUrl = entityJson.getString("audioUrl");
                            // audioUrl from entity_data
                        }
                    } catch (Exception innerEx) {
                        Log.e(TAG, "Error parsing entity_data: " + innerEx.getMessage());
                    }
                }
                
                // Direct fields check (non-nested)
                if ((data.instruction == null || data.instruction.isEmpty()) && 
                    jsonObject.has("instruction") && !jsonObject.isNull("instruction")) {
                    data.instruction = jsonObject.getString("instruction");
                    // instruction from root
                } else {
                    // no instruction in root
                }
                
                if ((data.audioUrl == null || data.audioUrl.isEmpty()) && 
                    jsonObject.has("audioUrl") && !jsonObject.isNull("audioUrl")) {
                    data.audioUrl = jsonObject.getString("audioUrl");
                    // audioUrl from root
                } else if (data.audioUrl == null || data.audioUrl.isEmpty()) {
                    // no audioUrl in root
                }
                
                if (jsonObject.has("senderName") && !jsonObject.isNull("senderName")) {
                    data.senderName = jsonObject.getString("senderName");
                    // senderName from root
                } else {
                    data.senderName = "Customer"; // Changed from "Driver" to "Customer"
                    // default senderName
                }
                
                // Parsed via JSONObject
                return data;
            }
        } catch (Exception e) {
            Log.e(TAG, "PICKUP DEBUG: Error parsing pickup instruction data: " + e.getMessage());
            e.printStackTrace();
            return null;
        }
    }
    
    private void initializeViews() {
        try {
            if (view == null) {
                Log.e(TAG, "View is null in initializeViews");
                return;
            }
            
            // Get references to views from layout with null checks
            // Get arrow button
            arrowButton = view.findViewById(R.id.overlay_open_chat_button);
            if (arrowButton == null) {
                Log.e(TAG, "arrowButton view not found");
            }
            
            pickupInstructionContent = view.findViewById(R.id.pickup_instruction_content);
            if (pickupInstructionContent == null) {
                Log.e(TAG, "pickupInstructionContent view not found");
            }
            
            mainActionButton = view.findViewById(R.id.got_it_button);
            if (mainActionButton == null) {
                Log.e(TAG, "mainActionButton view not found");
            } else {
                // Set a failsafe click listener right away that just closes the overlay
                mainActionButton.setOnClickListener(v -> {
                    try {
                        if (serviceInterface != null) {
                            serviceInterface.killService();
                        }
                    } catch (Exception e) {
                        Log.e(TAG, "Error in failsafe button click: " + e.getMessage());
                    }
                });
            }
            
            textNoteLabel = view.findViewById(R.id.text_note_label);
            if (textNoteLabel == null) {
                Log.e(TAG, "textNoteLabel view not found");
            }
            
            // Get reference to audio section views
            audioSection = view.findViewById(R.id.audio_section);
            if (audioSection == null) {
                Log.e(TAG, "audioSection view not found");
            }
            
            textNoteSection = view.findViewById(R.id.text_note_section);
            if (textNoteSection == null) {
                Log.e(TAG, "textNoteSection view not found");
            }
            
            audioNoteLabel = view.findViewById(R.id.audio_note_label);
            if (audioNoteLabel == null) {
                Log.e(TAG, "audioNoteLabel view not found");
            }
            
            audioControlButton = view.findViewById(R.id.audio_control_button);
            if (audioControlButton == null) {
                Log.e(TAG, "audioControlButton view not found");
            }
            
            audioVisualizer = view.findViewById(R.id.audio_visualizer);
            if (audioVisualizer == null) {
                Log.e(TAG, "audioVisualizer view not found");
            }
            
            replayButton = view.findViewById(R.id.replay_button);
            if (replayButton == null) {
                Log.e(TAG, "replayButton view not found");
            }
            
            audioTitle = view.findViewById(R.id.audio_title);
            if (audioTitle == null) {
                Log.e(TAG, "audioTitle view not found");
            }
            
            // Background is now set in XML, no need to set it programmatically
        } catch (Exception e) {
            Log.e(TAG, "Error initializing views: " + e.getMessage());
        }
    }
    
    private void populateViews(PickupInstructionData data) {
        try {
            // Get references to views that might not have been initialized in initializeViews()
            if (view == null) {
                Log.e(TAG, "View is null in populateViews");
                return;
            }
            
            TextView overlaySenderName = view.findViewById(R.id.overlay_sender_name);
            
            // Set sender name if available
            if (overlaySenderName != null && data != null && data.senderName != null && !data.senderName.isEmpty()) {
                overlaySenderName.setText(data.senderName);
            }
            
            // Re-initialize views if they're null
            if (pickupInstructionContent == null) {
                pickupInstructionContent = view.findViewById(R.id.pickup_instruction_content);
            }
            
            if (textNoteLabel == null) {
                textNoteLabel = view.findViewById(R.id.text_note_label);
            }
            
            if (audioSection == null) {
                audioSection = view.findViewById(R.id.audio_section);
            }
            
            if (textNoteSection == null) {
                textNoteSection = view.findViewById(R.id.text_note_section);
            }
            
            if (audioTitle == null) {
                audioTitle = view.findViewById(R.id.audio_title);
            }
            
            boolean hasTextInstruction = data != null && data.instruction != null && !data.instruction.isEmpty();
            boolean hasAudioInstruction = data != null && data.audioUrl != null && !data.audioUrl.isEmpty();
            
            // Add debug logging for the boolean flags
            // Flags for instruction and audio presence
            
            String instructionText = hasTextInstruction ? data.instruction : context.getString(R.string.pickup_instruction_default_text);
            
            // Determine whether to show text-only or audio mode
            if (hasAudioInstruction) {
                // Audio mode
                // Audio available - show audio section, hide text section
                if (textNoteSection != null) {
                    textNoteSection.setVisibility(View.GONE);
                }
                if (audioSection != null) {
                    audioSection.setVisibility(View.VISIBLE);
                    // Initialize audio UI state
                    updateUIForAudioState(AudioState.NOT_STARTED);
                }
                
                // Set instruction text in audio title
                if (audioTitle != null) {
                    audioTitle.setText(instructionText);
                }
                
                // Initialize MediaPlayer for audio
                initializeMediaPlayerWithErrorHandling(data.audioUrl);
                
                // Set main button to "Play voice instructions"
                if (mainActionButton != null) {
                    mainActionButton.setText(context.getString(R.string.pickup_instruction_play_voice));
                }
            } else {
                // Text-only mode
                // Text-only mode - show text section, hide audio section
                if (textNoteSection != null) {
                    textNoteSection.setVisibility(View.VISIBLE);
                }
                if (audioSection != null) {
                    audioSection.setVisibility(View.GONE);
                }
                
                // Set instruction text in text content area
                if (pickupInstructionContent != null) {
                    pickupInstructionContent.setText(instructionText);
                }
                
                // Set main button to "Close"
                if (mainActionButton != null) {
                    mainActionButton.setText("Close");
                }
            }
            
        } catch (Exception e) {
            Log.e(TAG, "Error populating views: " + e.getMessage());
        }
    }
    
    private void setupButtonListeners(PickupInstructionData data) {
        try {
            if (view == null || data == null) {
                Log.e(TAG, "View or data is null in setupButtonListeners");
                return;
            }
            
            // Re-initialize buttons if they're null
            if (mainActionButton == null) {
                mainActionButton = view.findViewById(R.id.got_it_button);
            }
            
            if (audioControlButton == null) {
                audioControlButton = view.findViewById(R.id.audio_control_button);
            }
            
            if (replayButton == null) {
                replayButton = view.findViewById(R.id.replay_button);
            }
            
            // Determine if we have audio data
            boolean hasAudioInstruction = data != null && data.audioUrl != null && !data.audioUrl.isEmpty();
            Log.d(TAG, "PICKUP DEBUG: setupButtonListeners hasAudioInstruction=" + hasAudioInstruction);
            
            // Setup main action button based on whether we have audio or not
            if (mainActionButton != null) {
                if (hasAudioInstruction) {
                    // Audio mode - Play audio, then close when completed
                    mainActionButton.setText(context.getString(R.string.pickup_instruction_play_voice));
                    mainActionButton.setOnClickListener(v -> {
                        try {
                            Log.d(TAG, "PICKUP: Main button clicked, audio state: " + currentAudioState);
                            if (currentAudioState == AudioState.COMPLETED) {
                                // If audio completed, close overlay
                                Log.d(TAG, "PICKUP: Audio completed, closing overlay");
                                serviceInterface.killService();
                            } else if (currentAudioState == AudioState.PLAYING) {
                                // If audio is playing, stop it and return to NOT_STARTED state
                                Log.d(TAG, "PICKUP: Stopping audio playback");
                                pauseAudio();
                                updateUIForAudioState(AudioState.NOT_STARTED);
                            } else {
                                // Otherwise play the audio
                                Log.d(TAG, "PICKUP: Attempting to play audio");
                                playAudio();
                            }
                        } catch (Exception e) {
                            Log.e(TAG, "Error in mainActionButton click: " + e.getMessage());
                            // If error, just close the overlay
                            serviceInterface.killService();
                        }
                    });
                } else {
                    // Text-only mode - Simple close button
                    mainActionButton.setText("Close");
                    mainActionButton.setOnClickListener(v -> {
                        try {
                            Log.d(TAG, "PICKUP: Close button clicked in text-only mode");
                            serviceInterface.killService();
                        } catch (Exception e) {
                            Log.e(TAG, "Error in Close button click: " + e.getMessage());
                            // If error, still try to close the overlay
                            serviceInterface.killService();
                        }
                    });
                }
            }
            
            // Audio control button - Play/pause
            if (audioControlButton != null) {
                audioControlButton.setOnClickListener(v -> {
                    try {
                        if (currentAudioState == AudioState.PLAYING) {
                            pauseAudio();
                        } else {
                            playAudio();
                        }
                    } catch (Exception e) {
                        Log.e(TAG, "Error in audioControlButton click: " + e.getMessage());
                        // If error, just close the overlay
                        serviceInterface.killService();
                    }
                });
            }
            
            // Replay button - replay the audio
            if (replayButton != null) {
                replayButton.setOnClickListener(v -> {
                    try {
                        playAudio();
                    } catch (Exception e) {
                        Log.e(TAG, "Error in replayButton click: " + e.getMessage());
                        // If error, just close the overlay
                        serviceInterface.killService();
                    }
                });
            }
            
            // Setup arrow button to open the app
            if (arrowButton != null) {
                arrowButton.setOnClickListener(v -> {
                    try {
                        Log.d(TAG, "PICKUP: Arrow button clicked - attempting to open app");
                        openApplication(); // Call existing method to open the app
                        Log.d(TAG, "PICKUP: App opening attempt completed, closing overlay");
                        serviceInterface.killService();
                    } catch (Exception e) {
                        Log.e(TAG, "PICKUP: Error opening app from arrow button: " + e.getMessage());
                        e.printStackTrace();
                        // Still close overlay even if app opening fails
                        serviceInterface.killService();
                    }
                });
            }
        } catch (Exception e) {
            Log.e(TAG, "Error setting up button listeners: " + e.getMessage());
        }
    }
    
    /**
     * Initialize MediaPlayer with enhanced error handling
     * This method attempts to initialize the MediaPlayer with the provided audio URL
     * and handles any errors that may occur during initialization with specific error messages
     */
    /**
     * Initialize MediaPlayer with enhanced error handling
     * This method attempts to initialize the MediaPlayer with the provided audio URL
     * and handles any errors that may occur during initialization with specific error messages
     */
    private void initializeMediaPlayerWithErrorHandling(String audioUrl) {
        try {
            Log.d(TAG, "PICKUP: Starting MediaPlayer initialization with audio URL: " + audioUrl);
            
            // Release any existing MediaPlayer
            if (mediaPlayer != null) {
                try {
                    if (mediaPlayer.isPlaying()) {
                        mediaPlayer.stop();
                    }
                    mediaPlayer.release();
                    mediaPlayer = null;
                } catch (Exception e) {
                    Log.e(TAG, "PICKUP: Error releasing MediaPlayer: " + e.getMessage());
                }
            }
            
            // Check if we have a valid audio URL
            if (audioUrl == null || audioUrl.isEmpty()) {
                Log.e(TAG, "PICKUP: No audio URL provided");
                // Use simulated playback as fallback
                // Using simulated playback as fallback
                return;
            }
            
            // Use a background thread for network operations
            new Thread(() -> {
                try {
                    // Create a temporary file for the downloaded audio
                    File cacheDir = context.getCacheDir();
                    File audioFile = new File(cacheDir, "pickup_instruction_" + System.currentTimeMillis() + ".mp3");
                    
                    // Download the audio file from the URL
                    boolean downloadSuccess = downloadAudioFile(audioUrl, audioFile);
                    
                    if (!downloadSuccess || !audioFile.exists() || audioFile.length() == 0) {
                        Log.e(TAG, "PICKUP: Failed to download audio file or file is empty");
                        // Fall back to simulated playback
                        new Handler(Looper.getMainLooper()).post(() -> {
                            simulateAudioPlayback(7000);
                        });
                        return;
                    }
                    
                    // Verify the audio file is valid
                    if (!verifyAudioFile(audioFile)) {
                        Log.e(TAG, "PICKUP: Downloaded file is not a valid audio file");
                        // Fall back to simulated playback
                        new Handler(Looper.getMainLooper()).post(() -> {
                            simulateAudioPlayback(7000);
                        });
                        return;
                    }
                    
                    // Downloaded audio file
                    
                    // Log audio file metadata before initializing MediaPlayer
                    // Audio file metadata
                    
                    // Initialize MediaPlayer on the main thread
                    final File finalAudioFile = audioFile;
                    new Handler(Looper.getMainLooper()).post(() -> {
                        FileInputStream fis = null;
                        try {
                            // Initialize MediaPlayer with the downloaded file
                            mediaPlayer = new MediaPlayer();
                            
                            // Log MediaPlayer state before setup
                            // Initialize MediaPlayer instance
                            
                            // Set error listener with enhanced error logging before preparing
                            mediaPlayer.setOnErrorListener((mp, what, extra) -> {
                                String errorType = "Unknown";
                                if (what == MediaPlayer.MEDIA_ERROR_UNKNOWN) {
                                    errorType = "MEDIA_ERROR_UNKNOWN";
                                } else if (what == MediaPlayer.MEDIA_ERROR_SERVER_DIED) {
                                    errorType = "MEDIA_ERROR_SERVER_DIED";
                                }
                                
                                String extraInfo = "Unknown";
                                if (extra == MediaPlayer.MEDIA_ERROR_IO) {
                                    extraInfo = "MEDIA_ERROR_IO";
                                } else if (extra == MediaPlayer.MEDIA_ERROR_MALFORMED) {
                                    extraInfo = "MEDIA_ERROR_MALFORMED";
                                } else if (extra == MediaPlayer.MEDIA_ERROR_UNSUPPORTED) {
                                    extraInfo = "MEDIA_ERROR_UNSUPPORTED";
                                } else if (extra == MediaPlayer.MEDIA_ERROR_TIMED_OUT) {
                                    extraInfo = "MEDIA_ERROR_TIMED_OUT";
                                }
                                
                                Log.e(TAG, "PICKUP: MediaPlayer detailed error: " + errorType + ", " + extraInfo);
                                
                                // Fall back to simulation
                                simulateAudioPlayback(7000);
                                return true;
                            });
                            
                            // Try to load and prepare the audio file
                            fis = new FileInputStream(finalAudioFile);
                            mediaPlayer.setDataSource(fis.getFD());
                            
                            Log.d(TAG, "PICKUP: Preparing MediaPlayer...");
                            mediaPlayer.prepare(); // This may fail
                            
                            Log.d(TAG, "PICKUP: MediaPlayer initialized with downloaded audio");
                            
                            // Set completion listener
                            mediaPlayer.setOnCompletionListener(mp -> {
                                Log.d(TAG, "PICKUP: Audio playback completed");
                                updateUIForAudioState(AudioState.COMPLETED);
                            });
                            
                            // Everything successful
                            Log.d(TAG, "PICKUP: MediaPlayer initialization successful");
                            
                        } catch (Exception e) {
                            Log.e(TAG, "PICKUP: Error initializing MediaPlayer: " + e.getMessage(), e);
                            // Log stack trace for more details
                            e.printStackTrace();
                            simulateAudioPlayback(7000);
                        } finally {
                            // Clean up resources
                            try {
                                if (fis != null) fis.close();
                            } catch (Exception e) {
                                Log.e(TAG, "Error closing file input stream: " + e.getMessage());
                            }
                        }
                    });
                    
                } catch (Exception e) {
                    Log.e(TAG, "PICKUP: Error in background thread: " + e.getMessage(), e);
                    // Fall back to simulated playback on the main thread
                    new Handler(Looper.getMainLooper()).post(() -> {
                        simulateAudioPlayback(7000);
                    });
                }
            }).start();
            
        } catch (Exception e) {
            Log.e(TAG, "PICKUP: Fatal error in initializeMediaPlayerWithErrorHandling: " + e.getMessage(), e);
            // Fall back to simulated playback as a last resort
            simulateAudioPlayback(7000);
        }
    }
    
    /**
     * Downloads an audio file from a URL and saves it to the specified file
     * Enhanced with more detailed error handling and base64 detection
     * @param audioUrl The URL to download from
     * @param audioFile The file to save to
     * @return true if download was successful, false otherwise
     */
    private boolean downloadAudioFile(String audioUrl, File audioFile) {
        HttpURLConnection connection = null;
        BufferedInputStream inputStream = null;
        FileOutputStream outputStream = null;

        try {
            Log.d(TAG, "PICKUP: Attempting to download from URL: " + audioUrl);

            // Create URL object
            URL url = new URL(audioUrl);

            // Open connection
            connection = (HttpURLConnection) url.openConnection();

            // Check if this is an AWS S3 URL with security token
            if (audioUrl.contains("X-Amz-Security-Token=")) {
                Log.d(TAG, "PICKUP: URL contains security token, adding special headers");
                // Adding these headers can help with AWS S3 signed URLs
                connection.setRequestProperty("Accept", "*/*");
                connection.setRequestProperty("Connection", "keep-alive");
            }

            Log.d(TAG, "PICKUP: Opening connection to " + url.getHost());

            // Set timeout
            connection.setConnectTimeout(15000);
            connection.setReadTimeout(30000);

            // Get response code
            int responseCode = connection.getResponseCode();
            Log.d(TAG, "PICKUP: Server returned HTTP " + responseCode);

            if (responseCode != HttpURLConnection.HTTP_OK) {
                Log.e(TAG, "PICKUP: Server returned error: " + responseCode);
                return false;
            }

            // Get content length and type
            int contentLength = connection.getContentLength();
            String contentType = connection.getContentType();
            Log.d(TAG, "PICKUP: Content length: " + contentLength + " bytes");
            Log.d(TAG, "PICKUP: Content type: " + contentType);

            // Create file
            Log.d(TAG, "PICKUP: Target file: " + audioFile.getAbsolutePath());

            // Log response headers for debugging
            Log.d(TAG, "PICKUP: Response headers:");
            for (Map.Entry<String, List<String>> header : connection.getHeaderFields().entrySet()) {
                if (header.getKey() != null) {
                    for (String value : header.getValue()) {
                        Log.d(TAG, "PICKUP: " + header.getKey() + ": " + value);
                    }
                }
            }

            // Get input stream
            inputStream = new BufferedInputStream(connection.getInputStream());

            // Create output stream
            outputStream = new FileOutputStream(audioFile);

            // Create buffer
            byte[] buffer = new byte[4096];
            int bytesRead;
            int totalBytesRead = 0;

            // Read data and write to file
            while ((bytesRead = inputStream.read(buffer)) != -1) {
                outputStream.write(buffer, 0, bytesRead);
                totalBytesRead += bytesRead;
            }

            Log.d(TAG, "PICKUP: Audio file downloaded successfully: " + totalBytesRead + " bytes to " + audioFile.getAbsolutePath());
            Log.d(TAG, "PICKUP: Downloaded file details - Size: " + audioFile.length() + " bytes, Last modified: " + new java.util.Date(audioFile.lastModified()));

            return true;

        } catch (UnknownHostException e) {
            Log.e(TAG, "PICKUP: Unknown host: " + e.getMessage());
            return false;
        } catch (SocketTimeoutException e) {
            Log.e(TAG, "PICKUP: Connection timed out: " + e.getMessage());
            return false;
        } catch (SocketException e) {
            Log.e(TAG, "PICKUP: Socket error: " + e.getMessage());
            return false;
        } catch (SSLException e) {
            Log.e(TAG, "PICKUP: SSL/TLS error: " + e.getMessage());
            return false;
        } catch (Exception e) {
            Log.e(TAG, "PICKUP: Error downloading audio file: " + e.getMessage());
            e.printStackTrace();
            return false;
        } finally {
            // Close streams
            try {
                if (outputStream != null) {
                    outputStream.close();
                }
                if (inputStream != null) {
                    inputStream.close();
                }
                if (connection != null) {
                    connection.disconnect();
                }
            } catch (Exception e) {
                Log.e(TAG, "PICKUP: Error closing streams: " + e.getMessage());
            }
        }
    }

    /**
     * Verifies that the downloaded file is a valid audio file
     * Enhanced with more detailed checks and error reporting
     * @param file The file to verify
     * @return true if the file is a valid audio file, false otherwise
     */
    private boolean verifyAudioFile(File file) {
        if (file == null || !file.exists()) {
            Log.e(TAG, "PICKUP: File is null or does not exist");
            return false;
        }

        Log.d(TAG, "PICKUP: Verifying audio file: size = " + file.length() + " bytes");
        Log.d(TAG, "PICKUP: File path: " + file.getAbsolutePath());
        Log.d(TAG, "PICKUP: File permissions: " + (file.canRead() ? "r" : "-") + (file.canWrite() ? "w" : "-"));

        // Check if file is large enough to be valid audio
        if (file.length() < 8) {
            Log.e(TAG, "PICKUP: File too small to be valid audio: " + file.length() + " bytes");
            return false;
        }

        try {
            // Read the first few bytes to check file header
            byte[] header = new byte[16];
            FileInputStream fis = new FileInputStream(file);
            int bytesRead = fis.read(header);
            fis.close();

            if (bytesRead < 8) {
                Log.e(TAG, "PICKUP: Couldn't read enough bytes for header check");
                return false;
            }

            // Convert header to hex string for debugging
            StringBuilder hexHeader = new StringBuilder();
            for (int i = 0; i < bytesRead; i++) {
                hexHeader.append(String.format("%02X ", header[i]));
            }
            Log.d(TAG, "PICKUP: Audio file header (hex): " + hexHeader.toString());

            // Convert header to ASCII for debugging
            StringBuilder asciiHeader = new StringBuilder();
            for (int i = 0; i < bytesRead; i++) {
                if (header[i] >= 32 && header[i] < 127) {
                    asciiHeader.append((char) header[i]);
                } else {
                    asciiHeader.append('.');
                }
            }
            Log.d(TAG, "PICKUP: Audio file header (ascii): " + asciiHeader.toString());

            Log.d(TAG, "PICKUP: Downloaded data analysis:");

            // Check if it's base64 encoded data
            String fileStart = asciiHeader.toString().trim();
            if (fileStart.startsWith("AAA") || isBase64Header(header)) {
                Log.d(TAG, "PICKUP: File appears to be base64 encoded - attempting decode");

                // Decode base64 content to a new file
                File decodedFile = decodeBase64File(file);
                if (decodedFile != null && decodedFile.exists() && decodedFile.length() > 0) {
                    Log.d(TAG, "PICKUP: Successfully decoded base64 file to: " + decodedFile.getAbsolutePath());

                    // Use the decoded file as our audio file by renaming it to the original file name
                    if (decodedFile.renameTo(file)) {
                        Log.d(TAG, "PICKUP: Replaced original file with decoded version");
                        return true;
                    } else {
                        Log.e(TAG, "PICKUP: Failed to rename decoded file");
                        return false;
                    }
                } else {
                    Log.e(TAG, "PICKUP: Failed to decode base64 file");
                    return false;
                }
            }

            // Check for common audio file signatures
            // MP3 file signature check
            if ((header[0] & 0xFF) == 0xFF && (header[1] & 0xE0) == 0xE0) {
                Log.d(TAG, "PICKUP: File appears to be valid MP3 (ID3v2)");
                return true;
            }

            // ID3 tag check
            if (header[0] == 'I' && header[1] == 'D' && header[2] == '3') {
                Log.d(TAG, "PICKUP: File appears to be valid MP3 (ID3)");
                return true;
            }

            // MP4/M4A file signature check
            if (asciiHeader.toString().contains("ftyp")) {
                Log.d(TAG, "PICKUP: File appears to be valid MP4/M4A");
                return true;
            }

            // OGG file signature check
            if (header[0] == 'O' && header[1] == 'g' && header[2] == 'g' && header[3] == 'S') {
                Log.d(TAG, "PICKUP: File appears to be valid OGG");
                return true;
            }

            // WAV file signature check
            if (header[0] == 'R' && header[1] == 'I' && header[2] == 'F' && header[3] == 'F' &&
                header[8] == 'W' && header[9] == 'A' && header[10] == 'V' && header[11] == 'E') {
                Log.d(TAG, "PICKUP: File appears to be valid WAV");
                return true;
            }

            // If no known format detected, but file is not empty, let MediaPlayer try anyway
            if (file.length() > 1000) { // Arbitrary threshold
                Log.w(TAG, "PICKUP: File format not recognized, but will attempt to play");
                return true;
            }

            Log.e(TAG, "PICKUP: File does not appear to be a valid audio file");
            return false;

        } catch (Exception e) {
            Log.e(TAG, "PICKUP: Error verifying audio file: " + e.getMessage());
            return false;
        }
    }

    /**
     * Checks if the byte array appears to be a base64 header
     */
    private boolean isBase64Header(byte[] header) {
        // Check if header contains only base64 valid characters
        for (int i = 0; i < Math.min(header.length, 8); i++) {
            char c = (char)header[i];
            if (!(Character.isLetterOrDigit(c) || c == '+' || c == '/' || c == '=')) {
                return false;
            }
        }
        return true;
    }

    /**
     * Decodes a base64 encoded file to a new file
     * @param encodedFile The base64 encoded file
     * @return The decoded file, or null if decoding failed
     */
    private File decodeBase64File(File encodedFile) {
        try {
            // Create a temporary file for the decoded audio
            File cacheDir = context.getCacheDir();
            File decodedFile = new File(cacheDir, "decoded_audio_" + System.currentTimeMillis() + ".mp3");

            // Read the entire file
            FileInputStream fis = new FileInputStream(encodedFile);
            byte[] encodedBytes = new byte[(int)encodedFile.length()];
            fis.read(encodedBytes);
            fis.close();

            // Convert to string for base64 decoding
            String encodedString = new String(encodedBytes, "UTF-8").trim();

            // Try to decode the base64 data
            byte[] decodedBytes = null;
            try {
                decodedBytes = Base64.decode(encodedString, Base64.DEFAULT);
                Log.d(TAG, "PICKUP: Base64 decoded with DEFAULT flags");
            } catch (IllegalArgumentException e) {
                // Try with different flags if default fails
                try {
                    decodedBytes = Base64.decode(encodedString, Base64.NO_WRAP);
                    Log.d(TAG, "PICKUP: Base64 decoded with NO_WRAP flags");
                } catch (IllegalArgumentException e2) {
                    // Clean the string and try again (some URLs may have line breaks or extra chars)
                    String cleanedString = encodedString.replaceAll("[^A-Za-z0-9+/=]", "");
                    try {
                        decodedBytes = Base64.decode(cleanedString, Base64.DEFAULT);
                        Log.d(TAG, "PICKUP: Base64 decoded with cleaned string");
                    } catch (IllegalArgumentException e3) {
                        Log.e(TAG, "PICKUP: Failed to decode base64 data: " + e3.getMessage());
                        return null;
                    }
                }
            }

            if (decodedBytes == null) {
                Log.e(TAG, "PICKUP: Decoded bytes are null");
                return null;
            }

            // Write the decoded data to file
            FileOutputStream fos = new FileOutputStream(decodedFile);
            fos.write(decodedBytes);
            fos.close();

            Log.d(TAG, "PICKUP: Decoded " + encodedBytes.length + " bytes to " + decodedBytes.length + " bytes");

            return decodedFile;
        } catch (Exception e) {
            Log.e(TAG, "PICKUP: Error decoding base64 file: " + e.getMessage());
            return null;
        }
    }
    
    /**
     * Handle errors that occur during audio initialization
     * This method disables audio playback controls and updates the UI accordingly
     * IMPORTANT: This does NOT change section visibility to avoid UI jumps
     * @param errorReason Optional error reason to display for debugging
     */
    private void handleAudioInitializationError(String errorReason) {
        try {
            Log.d(TAG, "PICKUP: Handling audio initialization error: " + (errorReason != null ? errorReason : "unknown error"));
            
            // Clean up any MediaPlayer resources
            if (mediaPlayer != null) {
                try {
                    if (mediaPlayer.isPlaying()) {
                        mediaPlayer.stop();
                    }
                    mediaPlayer.release();
                    mediaPlayer = null;
                    Log.d(TAG, "PICKUP: MediaPlayer resources cleaned up");
                } catch (Exception e) {
                    Log.e(TAG, "PICKUP: Error cleaning up MediaPlayer: " + e.getMessage());
                }
            }
            
            // Call the shared method to disable controls
            disableAudioControls(errorReason);
            
            // In debug builds, add more info
            if (BuildConfig.DEBUG && errorReason != null && !errorReason.isEmpty()) {
                Log.d(TAG, "PICKUP: Audio error reason: " + errorReason);
            }
        } catch (Exception e) {
            Log.e(TAG, "PICKUP: Error handling audio initialization error: " + e.getMessage());
        }
    }
    
    /**
     * Handle errors that occur during audio initialization (no reason provided)
     */
    private void handleAudioInitializationError() {
        handleAudioInitializationError(null);
    }
    
    /**
     * Original initializeMediaPlayer method - replaced with enhanced error handling version
     */
    private void initializeMediaPlayer(String audioUrl) {
        initializeMediaPlayerWithErrorHandling(audioUrl);
    }
    
    private void playAudio() {
        try {
            Log.d(TAG, "PICKUP: Attempting to play audio");
            
            try {
                // First update UI state
                updateUIForAudioState(AudioState.PLAYING);
                
                if (mediaPlayer != null) {
                    try {
                        // Reset to start if needed
                        if (!mediaPlayer.isPlaying()) {
                            mediaPlayer.seekTo(0);
                        }
                        
                        // Start playback
                        mediaPlayer.start();
                        Log.d(TAG, "PICKUP: MediaPlayer started playback");
                        return;
                    } catch (Exception e) {
                        Log.e(TAG, "PICKUP: Error starting MediaPlayer: " + e.getMessage());
                        // Continue to fallback if MediaPlayer fails
                    }
                }
                
                // Fallback: Simulate playback with a delay if MediaPlayer fails
                Log.d(TAG, "PICKUP: Using simulated playback as fallback");
                simulateAudioPlayback(7000); // Use longer duration for simulated playback
                
            } catch (Exception e) {
                Log.e(TAG, "PICKUP: Error in audio playback: " + e.getMessage());
                // If anything fails, try to kill the service to avoid crashes
                if (serviceInterface != null) {
                    serviceInterface.killService();
                }
            }
        } catch (Exception e) {
            Log.e(TAG, "PICKUP: Fatal error in playAudio: " + e.getMessage());
            // Last resort - kill the service
            if (serviceInterface != null) {
                serviceInterface.killService();
            }
        }
    }
    
    /**
     * Disable audio controls without changing section visibility
     * This prevents the UI from showing a completely different overlay
     * This is only used for initialization errors, not playback errors
     */
    private void disableAudioControls(String reason) {
        try {
            Log.d(TAG, "PICKUP: Disabling audio controls: " + reason);
            
            // Disable play button
            if (audioControlButton != null) {
                audioControlButton.setEnabled(false);
                audioControlButton.setAlpha(0.5f);
            }
            
            // Show error message in audio title
            if (audioTitle != null) {
                String currentText = audioTitle.getText().toString();
                String suffix = " " + context.getString(R.string.pickup_instruction_audio_unavailable_suffix);
                if (!currentText.contains(suffix)) {
                    audioTitle.setText(currentText + suffix);
                }
            }
            
            // Note: We no longer change the main button behavior or hide visualization/replay
            // This allows the proper state transitions to still work
        } catch (Exception e) {
            Log.e(TAG, "PICKUP: Error disabling audio controls: " + e.getMessage());
        }
    }
    
    /**
     * Simulates audio playback when actual playback fails
     * This ensures the user still experiences the proper UI flow
     */
    /**
     * Estimates audio duration based on file size
     * This is used for simulated playback when actual playback fails
     */
    private int estimateAudioDuration(int byteCount) {
        // Very rough estimation: typical MP3 is ~16KB per second at 128kbps
        // We're using a simplified calculation that ensures a reasonable duration
        if (byteCount <= 0) {
            return 7000; // Default 7 seconds if we don't have size info
        }
        
        int sizeKB = byteCount / 1024;
        // Convert to seconds based on approximate bitrate (MP3 ~16KB per second)
        int estimatedSeconds = Math.max(3, Math.min(sizeKB / 16, 15));
        return estimatedSeconds * 1000; // Convert to milliseconds
    }
    
    private void simulateAudioPlayback(int durationMs) {
        try {
            Log.d(TAG, "PICKUP: Simulating audio playback for " + durationMs + "ms");
            
            // Use Handler to transition to COMPLETED state after the duration
            new Handler(Looper.getMainLooper()).postDelayed(() -> {
                try {
                    Log.d(TAG, "PICKUP: Simulated playback completed");
                    // If the app crashes here, we'll know it's in the state transition
                    updateUIForAudioState(AudioState.COMPLETED);
                } catch (Exception e) {
                    Log.e(TAG, "PICKUP: Error in simulated playback completion: " + e.getMessage());
                    // If state transition fails, just close the overlay
                    try {
                        if (serviceInterface != null) {
                            serviceInterface.killService();
                        }
                    } catch (Exception ex) {
                        Log.e(TAG, "PICKUP: Could not kill service: " + ex.getMessage());
                    }
                }
            }, durationMs);
            
        } catch (Exception e) {
            Log.e(TAG, "PICKUP: Error simulating audio playback: " + e.getMessage());
            // Fallback to killing the service if simulation fails
            try {
                if (serviceInterface != null) {
                    serviceInterface.killService();
                }
            } catch (Exception finalEx) {
                Log.e(TAG, "PICKUP: Could not kill service: " + finalEx.getMessage());
            }
        }
    }
    
    private void pauseAudio() {
        try {
            if (mediaPlayer != null && mediaPlayer.isPlaying()) {
                mediaPlayer.pause();
                updateUIForAudioState(AudioState.NOT_STARTED);
            }
        } catch (Exception e) {
            Log.e(TAG, "Error pausing audio: " + e.getMessage());
        }
    }
    
    private void updateUIForAudioState(AudioState state) {
        try {
            // Safety check
            if (view == null) {
                Log.e(TAG, "View is null in updateUIForAudioState");
                return;
            }
            
            // Set the state immediately to avoid any state confusion
            currentAudioState = state;
            
            // Re-initialize views if they're null - but don't try to use them if they're still null
            if (audioControlButton == null) {
                audioControlButton = view.findViewById(R.id.audio_control_button);
                if (audioControlButton == null) {
                    Log.e(TAG, "audioControlButton still null after findViewById");
                }
            }
            
            if (mainActionButton == null) {
                mainActionButton = view.findViewById(R.id.got_it_button);
                if (mainActionButton == null) {
                    Log.e(TAG, "mainActionButton still null after findViewById");
                }
            }
            
            if (audioVisualizer == null) {
                audioVisualizer = view.findViewById(R.id.audio_visualizer);
                if (audioVisualizer == null) {
                    Log.e(TAG, "audioVisualizer still null after findViewById");
                }
            }
            
            if (replayButton == null) {
                replayButton = view.findViewById(R.id.replay_button);
                if (replayButton == null) {
                    Log.e(TAG, "replayButton still null after findViewById");
                }
            }
            
            if (audioTitle == null) {
                audioTitle = view.findViewById(R.id.audio_title);
                if (audioTitle == null) {
                    Log.e(TAG, "audioTitle still null after findViewById");
                }
            }
            
            if (audioSection == null) {
                audioSection = view.findViewById(R.id.audio_section);
                if (audioSection == null) {
                    Log.e(TAG, "audioSection still null after findViewById");
                }
            }
            
            // Super simplified state transitions to avoid any crashes
            try {
                switch (state) {
                    case NOT_STARTED:
                        // Initial state with play button - minimal operations
                        if (audioControlButton != null) {
                            audioControlButton.setImageResource(R.drawable.play_circle);
                        }
                        
                        if (mainActionButton != null) {
                            mainActionButton.setText("Play voice instructions");
                            mainActionButton.setEnabled(true);
                        }
                        
                        if (audioVisualizer != null) {
                            audioVisualizer.setVisibility(View.GONE);
                        }
                        
                        if (replayButton != null) {
                            replayButton.setVisibility(View.GONE);
                        }
                        break;
                        
                        case PLAYING:
                        // Playing state - show pause icon but hide audio title
                        if (audioControlButton != null) {
                            audioControlButton.setImageResource(R.drawable.pause_circle);
                            audioControlButton.setVisibility(View.VISIBLE);
                        }
                        
                        // Hide audio title in Stage 2 to allow visualizer to take full width
                        if (audioTitle != null) {
                            audioTitle.setVisibility(View.GONE);
                            audioTitle.setLayoutParams(new LinearLayout.LayoutParams(0, LinearLayout.LayoutParams.WRAP_CONTENT, 0)); // Remove weight
                        }
                        
                        if (mainActionButton != null) {
                            mainActionButton.setText("Play voice instructions");
                            mainActionButton.setEnabled(false);
                            mainActionButton.setAlpha(0.5f); // Make disabled state more visible
                        }
                        
                        // Show the visualizer animation with proper initialization zxc
                        if (audioVisualizer != null) {
                            audioVisualizer.setVisibility(View.VISIBLE);
                        }
                        
                        if (replayButton != null) {
                            replayButton.setVisibility(View.GONE);
                        }
                        break;
                        
                    case COMPLETED:
                        // Completed state - minimal operations
                        if (audioControlButton != null) {
                            audioControlButton.setImageResource(R.drawable.play_circle);
                            audioControlButton.setVisibility(View.VISIBLE);
                        }
                        
                        // Restore audio title in Stage 3
                        if (audioTitle != null) {
                            audioTitle.setVisibility(View.VISIBLE);
                            audioTitle.setLayoutParams(new LinearLayout.LayoutParams(0, LinearLayout.LayoutParams.WRAP_CONTENT, 1)); // Restore weight
                        }
                        
                        if (mainActionButton != null) {
                            mainActionButton.setText("Close");
                            mainActionButton.setEnabled(true);
                            mainActionButton.setAlpha(1.0f); // Reset to full opacity like stage 1
                        }
                        
                        if (audioVisualizer != null) {
                            audioVisualizer.setVisibility(View.GONE);
                        }
                        
                        if (replayButton != null) {
                            replayButton.setVisibility(View.VISIBLE);
                        }
                        break;
                }
            } catch (Exception e) {
                // If any error happens during state transitions, log it but don't crash
                Log.e(TAG, "Error during state transition: " + e.getMessage());
            }
        } catch (Exception e) {
            Log.e(TAG, "Error updating UI for audio state: " + e.getMessage());
        }
    }
    
    /**
     * Convert dp to pixels
     */
    private int dpToPx(int dp) {
        float density = context.getResources().getDisplayMetrics().density;
        return Math.round(dp * density);
    }
    
    /**
     * Open the main application
     * Enhanced with comprehensive logging for debugging app opening issues
     */
    private void openApplication() {
        try {
            Log.d(TAG, "PICKUP: openApplication() method called - preparing to open main app");
            
            // Log package details for debugging
            String packageName = context.getPackageName();
            Log.d(TAG, "PICKUP: Package name to open: " + packageName);
            
            // Check if the package exists before trying to open it
            try {
                context.getPackageManager().getPackageInfo(packageName, 0);
                Log.d(TAG, "PICKUP: Package is installed and accessible");
            } catch (Exception e) {
                Log.e(TAG, "PICKUP: Package not found or accessible: " + e.getMessage());
            }
            
            // Get the launch intent manually for logging purposes
            Intent intent = context.getPackageManager().getLaunchIntentForPackage(packageName);
            if (intent != null) {
                Log.d(TAG, "PICKUP: Launch intent obtained successfully: " + intent.toString());
                Log.d(TAG, "PICKUP: Intent flags: " + Integer.toHexString(intent.getFlags()));
            } else {
                Log.e(TAG, "PICKUP: Failed to get launch intent for package: " + packageName);
            }
            
            // Use the existing utility method to open the application
            Log.d(TAG, "PICKUP: Calling RideRequestUtils.openApplication()");
            RideRequestUtils.openApplication(context);
            Log.d(TAG, "PICKUP: RideRequestUtils.openApplication() completed without exceptions");
            
            // No need to close the overlay here as it will be handled by the caller
        } catch (Exception e) {
            Log.e(TAG, "PICKUP: Error opening application: " + e.getMessage());
            e.printStackTrace();
        }
    }
}
