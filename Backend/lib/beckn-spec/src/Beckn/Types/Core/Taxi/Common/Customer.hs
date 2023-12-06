{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.Common.Customer where

import Beckn.Types.Core.Taxi.Common.Tags
import Data.Aeson
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Utils.JSON
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)
import qualified Text.Show

data Customer = Customer
  { contact :: Contact,
    person :: Maybe OrderPerson
  }
  deriving (Generic, Show)

instance ToSchema Customer where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance FromJSON Customer where
  parseJSON = genericParseJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}

instance ToJSON Customer where
  toJSON = genericToJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}

newtype Contact = Contact
  { phone :: Phone
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data Phone = Phone
  { phoneCountryCode :: Text,
    phoneNumber :: Text
  }
  deriving (Generic)

instance Show Phone where
  show (Phone phoneCountryCode phoneNumber) = T.unpack phoneCountryCode <> "-" <> T.unpack phoneNumber

instance Read Phone where
  readsPrec _ str =
    case T.splitOn "-" $ T.pack str of
      phoneCountryCode : phoneNumber : _ -> [(Phone phoneCountryCode phoneNumber, "")]
      _ -> []

instance ToJSON Phone where
  toJSON = String . T.pack . show

instance FromJSON Phone where
  parseJSON = withText "Phone" $ \s -> do
    case readMaybe $ T.unpack s of
      Nothing -> fail "Unable to parse Phone"
      Just ic -> return ic

instance ToSchema Phone where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data OrderPerson = OrderPerson
  { name :: Text,
    tags :: Maybe TagGroups
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema OrderPerson where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
