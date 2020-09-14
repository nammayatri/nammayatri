{-# LANGUAGE DerivingStrategies #-}

module External.Delhivery.Types where

import Data.Aeson
import EulerHS.Prelude
import Types.Common
import Web.FormUrlEncoded

data TokenReq = TokenReq
  { client_id :: ClientId,
    client_secret :: ClientSecret,
    grant_type :: Text
  }
  deriving (Generic, Show, ToForm)

data TokenRes = TokenRes
  { access_token :: Token,
    expires_in :: Integer,
    refresh_expires_in :: Integer,
    refresh_token :: Token,
    token_type :: Text,
    not_before_policy :: Maybe Integer,
    session_state :: Text,
    scope :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)
