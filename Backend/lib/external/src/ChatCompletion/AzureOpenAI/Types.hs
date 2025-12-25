{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ChatCompletion.AzureOpenAI.Types where

import Data.Text as T
import Kernel.Prelude
import Kernel.Utils.JSON

newtype ChatCompletionReq = ChatCompletionReq
  { messages :: [Message]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data Message = Message
  { role :: Text,
    content :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data ContentFilterResults = ContentFilterResults
  { hate :: FilterResult,
    self_harm :: FilterResult,
    sexual :: FilterResult,
    violence :: FilterResult
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data FilterResult = FilterResult
  { filtered :: Bool,
    severity :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data Choice = Choice
  { content_filter_results :: ContentFilterResults,
    finish_reason :: Text,
    _index :: Int,
    message :: Message
  }
  deriving (Show, Generic)

instance FromJSON Choice where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Choice where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data ChatCompletionResponse = ChatCompletionResponse
  { choices :: [Choice],
    created :: Int,
    id :: Text,
    model :: Text,
    _object :: Text,
    prompt_filter_results :: [PromptFilterResult],
    system_fingerprint :: Text,
    usage :: Usage
  }
  deriving (Show, Generic)

instance FromJSON ChatCompletionResponse where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON ChatCompletionResponse where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data PromptFilterResult = PromptFilterResult
  { prompt_index :: Int,
    content_filter_results :: ContentFilterResults
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data Usage = Usage
  { completion_tokens :: Int,
    prompt_tokens :: Int,
    total_tokens :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON)
