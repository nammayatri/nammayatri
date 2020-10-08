{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types.Common where

import EulerHS.Prelude
import Servant

newtype ClientId = ClientId {getClientId :: Text}
  deriving (Generic, Show)

deriving newtype instance ToJSON ClientId

deriving newtype instance FromJSON ClientId

deriving newtype instance ToHttpApiData ClientId

deriving newtype instance FromHttpApiData ClientId

newtype ClientSecret = ClientSecret {getClientSecret :: Text}
  deriving (Generic, Show)

deriving newtype instance ToJSON ClientSecret

deriving newtype instance FromJSON ClientSecret

deriving newtype instance ToHttpApiData ClientSecret

deriving newtype instance FromHttpApiData ClientSecret

newtype Token = Token {getToken :: Text}
  deriving (Generic, Show)

deriving newtype instance ToJSON Token

deriving newtype instance FromJSON Token

deriving newtype instance ToHttpApiData Token

deriving newtype instance FromHttpApiData Token
