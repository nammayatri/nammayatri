module Core.Fulfillment where

import Beckn.Prelude
import Beckn.Utils.Example
import Beckn.Utils.JSON
import Core.Location (LocationDetails)
import Core.Time
import Data.Aeson

data Fulfillment = Fulfillment
  { id :: Maybe Text,
    -- _type :: Maybe Text,
    -- provider_id :: Maybe Text,
    -- state :: Maybe State,
    -- tracking :: Bool,
    -- customer :: Maybe FulfillmentParticipant,
    -- agent :: Maybe Agent,
    -- vehicle :: Maybe Vehicle,
    start :: Maybe FulfillmentDetails,
    end :: Maybe FulfillmentDetails
    -- tags :: Maybe Tags
  }
  deriving (Generic, Show)

emptyFulfillment :: Fulfillment
emptyFulfillment =
  Fulfillment
    { id = Nothing,
      --   _type = Nothing,
      --   provider_id = Nothing,
      --   state = Nothing,
      --   tracking = False,
      --   customer = Nothing,
      --   agent = Nothing,
      --   vehicle = Nothing,
      start = Nothing,
      end = Nothing
      --   tags = Nothing
    }

instance FromJSON Fulfillment where
  parseJSON = withObject "Fulfillment" $ \o ->
    Fulfillment
      <$> o .:? "id"
      --   <*> o .:? "type"
      --   <*> o .:? "provider_id"
      --   <*> o .:? "state"
      --   <*> o .:? "tracking" .!= False
      --   <*> o .:? "customer"
      --   <*> o .:? "agent"
      --   <*> o .:? "vehicle"
      <*> o .:? "start"
      <*> o .:? "end"

--   <*> o .:? "tags"

instance ToJSON Fulfillment where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny {omitNothingFields = True}

instance Example Fulfillment where
  example =
    Fulfillment
      { id = Nothing,
        -- _type = Nothing,
        -- provider_id = Nothing,
        -- state = Nothing,
        -- tracking = False,
        -- customer = Nothing,
        -- agent = Nothing,
        -- vehicle = Nothing,
        start = Nothing,
        end = Nothing
        -- tags = Nothing
      }

-- data FulfillmentParticipant = FulfillmentParticipant
--   { person :: Maybe Person,
-- contact :: Maybe Contact
--   }
--   deriving (Generic, FromJSON, ToJSON, Show)
--
-- data FulfillmentDetails = FulfillmentDetails
--   { location :: Maybe Location,
-- time :: Maybe Time,
-- instructions :: Maybe Descriptor,
-- contact :: Maybe Contact,
-- person :: Maybe Person
--   }
--   deriving (Generic, FromJSON, ToJSON, Show)

data FulfillmentDetails = FulfillmentDetails
  { location :: Maybe LocationDetails,
    time :: Maybe Time
    -- instructions :: Maybe Descriptor,
    -- contact :: Maybe Contact,
    -- person :: Maybe Person
  }
  deriving (Generic, FromJSON, Show)

instance ToJSON FulfillmentDetails where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

emptyFulfillmentDetails :: FulfillmentDetails
emptyFulfillmentDetails =
  FulfillmentDetails
    { location = Nothing,
      time = Nothing
      --   instructions = Nothing,
      --   contact = Nothing,
      --   person = Nothing
    }
