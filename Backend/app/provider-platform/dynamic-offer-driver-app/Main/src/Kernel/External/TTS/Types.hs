module Kernel.External.TTS.Types where

import EulerHS.Prelude

-- | Supported TTS providers
data TTSProvider = GoogleTTS | AWSPolly
  deriving (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Configuration for TTS provider
data TTSConfig = TTSConfig
  { provider :: TTSProvider,
    apiKey :: Text,
    apiEndpoint :: Text,
    defaultVoiceName :: Text,
    defaultSpeechRate :: Double
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Request for TTS synthesis
data TTSSynthesizeReq = TTSSynthesizeReq
  { text :: Text,
    language :: Text,
    voiceName :: Maybe Text,
    speechRate :: Maybe Double
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Response from TTS synthesis
data TTSSynthesizeRes = TTSSynthesizeRes
  { audioContent :: Text, -- Base64-encoded audio
    audioFormat :: Text, -- e.g., "MP3", "OGG_OPUS"
    durationMs :: Int
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Supported languages for Indian TTS
data TTSLanguage
  = TTSEnglish
  | TTSHindi
  | TTSKannada
  | TTSTamil
  | TTSTelugu
  | TTSBengali
  | TTSMarathi
  | TTSGujarati
  | TTSMalayalam
  deriving (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Map language code to TTS language
languageCodeToTTS :: Text -> TTSLanguage
languageCodeToTTS "hi" = TTSHindi
languageCodeToTTS "kn" = TTSKannada
languageCodeToTTS "ta" = TTSTamil
languageCodeToTTS "te" = TTSTelugu
languageCodeToTTS "bn" = TTSBengali
languageCodeToTTS "mr" = TTSMarathi
languageCodeToTTS "gu" = TTSGujarati
languageCodeToTTS "ml" = TTSMalayalam
languageCodeToTTS _ = TTSEnglish

-- | Get Google Cloud TTS language code
googleLanguageCode :: TTSLanguage -> Text
googleLanguageCode TTSEnglish = "en-IN"
googleLanguageCode TTSHindi = "hi-IN"
googleLanguageCode TTSKannada = "kn-IN"
googleLanguageCode TTSTamil = "ta-IN"
googleLanguageCode TTSTelugu = "te-IN"
googleLanguageCode TTSBengali = "bn-IN"
googleLanguageCode TTSMarathi = "mr-IN"
googleLanguageCode TTSGujarati = "gu-IN"
googleLanguageCode TTSMalayalam = "ml-IN"

-- | Get AWS Polly language code
pollyLanguageCode :: TTSLanguage -> Text
pollyLanguageCode TTSEnglish = "en-IN"
pollyLanguageCode TTSHindi = "hi-IN"
pollyLanguageCode TTSKannada = "kn-IN"
pollyLanguageCode TTSTamil = "ta-IN"
pollyLanguageCode TTSTelugu = "te-IN"
pollyLanguageCode TTSBengali = "bn-IN"
pollyLanguageCode TTSMarathi = "mr-IN"
pollyLanguageCode TTSGujarati = "gu-IN"
pollyLanguageCode TTSMalayalam = "ml-IN"
