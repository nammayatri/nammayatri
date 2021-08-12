module Beckn.Types.Mobility.Traveller where

import Beckn.Types.Core.Person
import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import Data.Text
import EulerHS.Prelude

data Traveller = Traveller
  -- Core Person type
  { name :: Name,
    image :: Maybe Image,
    dob :: Maybe Text,
    organization_name :: Maybe Text,
    gender :: Text, -- male, female
    email :: Maybe Text,
    phones :: [Text], -- Phone numer in E.164 format (ITUT recommendation,
    origin_stop_id :: Text,
    destination_stop_id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

instance Example Traveller where
  example =
    Traveller
      { name = example,
        image = example,
        dob = Just "28-11-1990",
        organization_name = Nothing,
        gender = "male",
        email = Just "john.smith@email.com",
        phones = ["+919999999999"],
        origin_stop_id = idExample,
        destination_stop_id = idExample
      }
