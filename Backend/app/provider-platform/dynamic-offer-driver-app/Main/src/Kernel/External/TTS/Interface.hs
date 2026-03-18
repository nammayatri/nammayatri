module Kernel.External.TTS.Interface where

import EulerHS.Prelude
import Kernel.External.TTS.Types
import Kernel.Utils.Common

-- | Synthesize speech from text using the configured TTS provider.
-- This is the main interface function that dispatches to the appropriate
-- provider implementation (Google Cloud TTS or AWS Polly).
synthesizeSpeech ::
  (MonadFlow m, Log m) =>
  TTSConfig ->
  TTSSynthesizeReq ->
  m TTSSynthesizeRes
synthesizeSpeech config req = do
  logInfo $ "TTS synthesis request: provider=" <> show config.provider <> " lang=" <> req.language
  case config.provider of
    GoogleTTS -> synthesizeWithGoogle config req
    AWSPolly -> synthesizeWithPolly config req

-- | Google Cloud TTS synthesis
-- In production, this would make an HTTP call to Google Cloud TTS API:
-- POST https://texttospeech.googleapis.com/v1/text:synthesize
synthesizeWithGoogle ::
  (MonadFlow m, Log m) =>
  TTSConfig ->
  TTSSynthesizeReq ->
  m TTSSynthesizeRes
synthesizeWithGoogle config req = do
  let ttsLang = languageCodeToTTS req.language
      langCode = googleLanguageCode ttsLang
      voice = fromMaybe config.defaultVoiceName req.voiceName
      rate = fromMaybe config.defaultSpeechRate req.speechRate
  logInfo $ "Google TTS: lang=" <> langCode <> " voice=" <> voice <> " rate=" <> show rate
  -- Placeholder: in production, make HTTP request to Google Cloud TTS API
  -- The response would contain base64-encoded audio content
  let estimatedDuration = length (show req.text) * 60
  pure
    TTSSynthesizeRes
      { audioContent = "", -- Would be base64-encoded MP3
        audioFormat = "MP3",
        durationMs = estimatedDuration
      }

-- | AWS Polly synthesis
-- In production, this would make an HTTP call to AWS Polly API:
-- POST https://polly.<region>.amazonaws.com/v1/speech
synthesizeWithPolly ::
  (MonadFlow m, Log m) =>
  TTSConfig ->
  TTSSynthesizeReq ->
  m TTSSynthesizeRes
synthesizeWithPolly config req = do
  let ttsLang = languageCodeToTTS req.language
      langCode = pollyLanguageCode ttsLang
      voice = fromMaybe config.defaultVoiceName req.voiceName
      rate = fromMaybe config.defaultSpeechRate req.speechRate
  logInfo $ "AWS Polly: lang=" <> langCode <> " voice=" <> voice <> " rate=" <> show rate
  -- Placeholder: in production, make HTTP request to AWS Polly
  let estimatedDuration = length (show req.text) * 60
  pure
    TTSSynthesizeRes
      { audioContent = "", -- Would be base64-encoded MP3
        audioFormat = "MP3",
        durationMs = estimatedDuration
      }
