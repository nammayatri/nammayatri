{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Lib.GoogleTranslate.Types where

import Kernel.Prelude
import Kernel.Utils.JSON

type HasGoogleTranslate m r = (MonadReader r m, HasField "googleTranslateUrl" r BaseUrl, HasField "googleTranslateKey" r Text)

data TranslateResp = TranslateResp
  { _data :: Translations,
    _error :: Maybe TranslateError
  }
  deriving (Generic, Show, ToSchema)

instance FromJSON TranslateResp where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON TranslateResp where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

newtype TranslateError = TranslateError
  { code :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

newtype Translations = Translation
  { translations :: [TranslatedText]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

newtype TranslatedText = TranslatedText
  { translatedText :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)
