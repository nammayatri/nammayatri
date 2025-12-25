{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ChatCompletion.Gemini.Types where

import Data.Text as T
import Kernel.Prelude
import Kernel.Utils.JSON

newtype ContentsReq = ContentsReq
  { contents :: [Content]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data Content = Content
  { role :: Text,
    parts :: [Part]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

newtype Part = Part
  { text :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data ContentsResp = ContentsResp
  { candidates :: [Candidate],
    usageMetadata :: UsageMetadata,
    modelVersion :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data Candidate = Candidate
  { content :: Content,
    finishReason :: Text,
    _index :: Int,
    safetyRatings :: [SafetyRating]
  }
  deriving (Show, Generic)

instance FromJSON Candidate where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Candidate where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data UsageMetadata = UsageMetadata
  { promptTokenCount :: Int,
    candidatesTokenCount :: Int,
    totalTokenCount :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data SafetyRating = SafetyRating
  { category :: Text,
    probability :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)
