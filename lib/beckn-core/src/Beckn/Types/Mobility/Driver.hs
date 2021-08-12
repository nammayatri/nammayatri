{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Mobility.Driver where

import Beckn.Types.Core.Duration
import Beckn.Types.Core.Person
import Beckn.Types.Core.Rating
import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import Data.Text
import Data.Time (Day (ModifiedJulianDay), UTCTime (UTCTime))
import EulerHS.Prelude

data Driver = Driver
  -- Core Person type
  { name :: Name,
    image :: Maybe Image,
    dob :: Maybe Text,
    organization_name :: Maybe Text,
    gender :: Text, -- male, female
    email :: Maybe Text,
    phones :: [Text], -- Phone numer in E.164 format (ITUT recommendation
    experience :: Maybe Duration,
    rating :: Maybe Rating,
    registeredAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

instance Example Driver where
  example =
    Driver
      { name = example,
        image = example,
        dob = Just "28-11-1990",
        organization_name = Nothing,
        gender = "male",
        email = Just "john.smith@email.com",
        phones = ["+919999999999"],
        experience = Nothing,
        rating = Nothing,
        registeredAt = UTCTime (ModifiedJulianDay 0) 0
      }
