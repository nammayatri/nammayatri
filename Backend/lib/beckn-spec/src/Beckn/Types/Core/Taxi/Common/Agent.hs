{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Beckn.Types.Core.Taxi.Common.Agent where

import Beckn.Types.Core.Taxi.Common.DecimalValue as Reexport
import Data.Aeson as A
import Data.OpenApi hiding (Example, example, name, tags)
import Kernel.Prelude
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data Agent = Agent
  { name :: Text,
    phone :: Text,
    tags :: AgentTags
  }
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

instance ToSchema Agent where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data AgentTags = AgentTags
  { registered_at :: UTCTime,
    rating :: Maybe DecimalValue
  }
  deriving (Eq, Generic, Show)

instance ToSchema AgentTags where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions agentTagsJSONOptions

instance FromJSON AgentTags where
  parseJSON = withObject "AgentTags" $ \obj -> do
    tagGroupCode <- (obj .: "groups/1/descriptor/code")
    tag1Code <- (obj .: "groups/1/list/1/descriptor/code")
    tag2Code :: Maybe Text <- (obj .:? "groups/1/list/2/descriptor/code")
    unless (tagGroupCode == "driver_details") $ fail ("Wrong tag group code: " <> tagGroupCode)
    unless (tag1Code == "registered_at") $ fail ("Wrong tag code registered_at: " <> tag1Code)
    unless (tag2Code == Just "rating") $ fail ("Wrong tag code for rating: " <> show tag2Code)
    AgentTags
      <$> obj .: "groups/1/list/1/value"
      <*> (fmap read <$> obj .:? "groups/1/list/2/value")

instance ToJSON AgentTags where
  toJSON AgentTags {..} = do
    A.Object $
      "groups/1/descriptor/name" .= A.String "Driver Details"
        <> "groups/1/descriptor/code" .= A.String "driver_details"
        <> "groups/1/list/1/descriptor/name" .= A.String "Registered At"
        <> "groups/1/list/1/descriptor/code" .= A.String "registered_at"
        <> "groups/1/list/1/value" .= registered_at
        <> ( case rating of
               Just ratingVal ->
                 "groups/1/list/2/descriptor/name" .= A.String "Rating"
                   <> "groups/1/list/2/descriptor/code" .= A.String "rating"
                   <> "groups/1/list/2/value" .= A.String (show ratingVal)
               Nothing -> mempty
           )

agentTagsJSONOptions :: A.Options
agentTagsJSONOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "registered_at" -> "registered_at"
        a -> a
    }
