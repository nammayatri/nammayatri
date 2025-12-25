{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Utils.Common.JWT.Config where

import Data.Aeson.Types
import EulerHS.Prelude
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import Kernel.External.Encryption
import Kernel.Storage.Esqueleto (derivePersistField)

data WalletService = GoogleWallet
  deriving (Show, Read, Eq, Ord, Generic)

$(mkBeamInstancesForEnumAndList ''WalletService)
derivePersistField "WalletService"

instance FromJSON WalletService where
  parseJSON (String "GoogleWallet") = pure GoogleWallet
  parseJSON (String _) = parseFail "Expected \"GoogleWallet\""
  parseJSON e = typeMismatch "String" e

instance ToJSON WalletService where
  toJSON = String . show

data GoogleWalletCfg = GoogleWalletCfg
  { privateKeyId :: EncryptedField 'AsEncrypted Text,
    clientEmail :: Text,
    tokenUri :: Text,
    issuerId :: EncryptedField 'AsEncrypted Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype WalletServiceConfig = GoogleWalletConfig GoogleWalletCfg
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
