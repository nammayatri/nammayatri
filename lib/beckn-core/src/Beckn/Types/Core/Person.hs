{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Person where

import Beckn.Utils.Example
import Beckn.Utils.JSON
import Data.Text
import EulerHS.Prelude

data Person = Person
  { name :: Name,
    image :: Maybe Image,
    dob :: Maybe Text,
    organization_name :: Maybe Text,
    gender :: Maybe Text, -- male, female
    email :: Maybe Text,
    phones :: [Text] -- Phone numer in E.164 format (ITUT recommendation
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance Example Person where
  example =
    Person
      { name = example,
        image = example,
        dob = Just "28-11-1990",
        organization_name = Nothing,
        gender = Nothing,
        email = Just "john.smith@email.com",
        phones = ["+919999999999"]
      }

data Image = Image
  { _type :: Text, -- "url", "data"
    label :: Text,
    url :: Maybe Text,
    _data :: Maybe Text
  }
  deriving (Generic, Show, Eq)

instance FromJSON Image where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Image where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Image where
  example =
    Image
      { _type = "data",
        label = "logo",
        url = Nothing,
        _data = Just "https://i.imgur.com/MjeqeUP.gif"
      }

data Name = Name
  { additional_name :: Maybe Text, -- An additional name for a Person, can be used for a middle name
    family_name :: Maybe Text, -- In India, it is the last name of an Person. This can be used along with givenName instead of the name property
    given_name :: Text, -- In India, it is the first name of a Person. This can be used along with familyName instead of the name property
    call_sign :: Maybe Text, -- A callsign, as used in broadcasting and radio communications to identify people
    honorific_prefix :: Maybe Text, -- An honorific prefix preceding a Person's name such as Dr/Mrs/Mr.
    honorific_suffix :: Maybe Text -- An honorific suffix preceding a Person's name such as M.D. /PhD/MSCSW.
  }
  deriving (Generic, Show)

instance FromJSON Name where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Name where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Name where
  example =
    Name
      { additional_name = Just "Smith",
        family_name = Nothing,
        given_name = "John",
        call_sign = Nothing,
        honorific_prefix = Just "Mr",
        honorific_suffix = Nothing
      }

withGivenName :: Text -> Name
withGivenName givenName =
  Name
    { additional_name = Nothing,
      family_name = Nothing,
      given_name = givenName,
      call_sign = Nothing,
      honorific_prefix = Nothing,
      honorific_suffix = Nothing
    }
