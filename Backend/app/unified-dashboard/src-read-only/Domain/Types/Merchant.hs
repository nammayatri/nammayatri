{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Merchant where

import Data.Aeson
import qualified Data.Text
import qualified Domain.Types.AccessMatrix
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Dhall
import qualified Tools.Beam.UtilsTH

data MerchantE e = Merchant
  { authToken :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Data.Text.Text),
    createdAt :: Kernel.Prelude.UTCTime,
    defaultOperatingCity :: Kernel.Types.Beckn.Context.City,
    domain :: Kernel.Prelude.Maybe Data.Text.Text,
    enabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    hasFleetMemberHierarchy :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    id :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    is2faMandatory :: Kernel.Prelude.Bool,
    isStrongNameCheckRequired :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    requireAdminApprovalForFleetOnboarding :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    serverNames :: [Domain.Types.AccessMatrix.ServerName],
    shortId :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant,
    singleActiveSessionOnly :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    supportedOperatingCities :: [Kernel.Types.Beckn.Context.City],
    verifyFleetWhileLogin :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    website :: Kernel.Prelude.Maybe Data.Text.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type Merchant = MerchantE ('AsEncrypted)

type DecryptedMerchant = MerchantE ('AsUnencrypted)

instance EncryptedItem Merchant where
  type Unencrypted Merchant = (DecryptedMerchant, HashSalt)
  encryptItem (entity, salt) = do
    authToken_ <- encryptItem $ (,salt) <$> authToken entity
    pure
      Merchant
        { authToken = authToken_,
          createdAt = createdAt entity,
          defaultOperatingCity = defaultOperatingCity entity,
          domain = domain entity,
          enabled = enabled entity,
          hasFleetMemberHierarchy = hasFleetMemberHierarchy entity,
          id = id entity,
          is2faMandatory = is2faMandatory entity,
          isStrongNameCheckRequired = isStrongNameCheckRequired entity,
          requireAdminApprovalForFleetOnboarding = requireAdminApprovalForFleetOnboarding entity,
          serverNames = serverNames entity,
          shortId = shortId entity,
          singleActiveSessionOnly = singleActiveSessionOnly entity,
          supportedOperatingCities = supportedOperatingCities entity,
          verifyFleetWhileLogin = verifyFleetWhileLogin entity,
          website = website entity,
          updatedAt = updatedAt entity
        }
  decryptItem entity = do
    authToken_ <- fmap fst <$> decryptItem (authToken entity)
    pure
      ( Merchant
          { authToken = authToken_,
            createdAt = createdAt entity,
            defaultOperatingCity = defaultOperatingCity entity,
            domain = domain entity,
            enabled = enabled entity,
            hasFleetMemberHierarchy = hasFleetMemberHierarchy entity,
            id = id entity,
            is2faMandatory = is2faMandatory entity,
            isStrongNameCheckRequired = isStrongNameCheckRequired entity,
            requireAdminApprovalForFleetOnboarding = requireAdminApprovalForFleetOnboarding entity,
            serverNames = serverNames entity,
            shortId = shortId entity,
            singleActiveSessionOnly = singleActiveSessionOnly entity,
            supportedOperatingCities = supportedOperatingCities entity,
            verifyFleetWhileLogin = verifyFleetWhileLogin entity,
            website = website entity,
            updatedAt = updatedAt entity
          },
        ""
      )

instance EncryptedItem' Merchant where
  type UnencryptedItem Merchant = DecryptedMerchant
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst

data AvailableCitiesForMerchant = AvailableCitiesForMerchant {merchantShortId :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant, operatingCity :: [Kernel.Types.Beckn.Context.City]}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
