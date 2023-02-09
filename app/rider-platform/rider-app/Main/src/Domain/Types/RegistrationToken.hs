{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.RegistrationToken where

import Kernel.Prelude
import Kernel.Types.Id

data Medium
  = SMS
  | EMAIL
  deriving (Generic, FromJSON, ToJSON, Eq, Show, Read, ToSchema)

data RTEntityType
  = CUSTOMER
  | USER
  deriving (Generic, FromJSON, ToJSON, Eq, Show, Read)

data LoginType
  = OTP
  | PASSWORD
  deriving (Generic, FromJSON, ToJSON, Eq, Show, Read, ToSchema)

data RegistrationToken = RegistrationToken
  { id :: Id RegistrationToken,
    token :: Text,
    attempts :: Int,
    authMedium :: Medium,
    authType :: LoginType,
    authValueHash :: Text,
    verified :: Bool,
    authExpiry :: Int,
    tokenExpiry :: Int,
    entityId :: Text,
    entityType :: RTEntityType,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    info :: Maybe Text
  }
  deriving (Generic, Show)
