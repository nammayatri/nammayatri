{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.RiderDriverCorrelation where
import Kernel.Prelude
import Kernel.External.Encryption
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.RiderDetails
import qualified Tools.Beam.UtilsTH



data RiderDriverCorrelationE e
    = RiderDriverCorrelation {createdAt :: Kernel.Prelude.UTCTime,
                              driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
                              favourite :: Kernel.Prelude.Bool,
                              merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                              merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                              mobileNumber :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
                              riderDetailId :: Kernel.Types.Id.Id Domain.Types.RiderDetails.RiderDetails,
                              updatedAt :: Kernel.Prelude.UTCTime}
    deriving Generic
type RiderDriverCorrelation = RiderDriverCorrelationE ('AsEncrypted)
type DecryptedRiderDriverCorrelation = RiderDriverCorrelationE ('AsUnencrypted)
instance EncryptedItem RiderDriverCorrelation
    where type Unencrypted RiderDriverCorrelation = (DecryptedRiderDriverCorrelation, HashSalt)
          encryptItem (entity, salt) = do {mobileNumber_ <- encryptItem (mobileNumber entity, salt);
                                           pure RiderDriverCorrelation{createdAt = createdAt entity,
                                                                       driverId = driverId entity,
                                                                       favourite = favourite entity,
                                                                       merchantId = merchantId entity,
                                                                       merchantOperatingCityId = merchantOperatingCityId entity,
                                                                       mobileNumber = mobileNumber_,
                                                                       riderDetailId = riderDetailId entity,
                                                                       updatedAt = updatedAt entity}}
          decryptItem entity = do {mobileNumber_ <- fst <$> decryptItem (mobileNumber entity);
                                   pure (RiderDriverCorrelation{createdAt = createdAt entity,
                                                                driverId = driverId entity,
                                                                favourite = favourite entity,
                                                                merchantId = merchantId entity,
                                                                merchantOperatingCityId = merchantOperatingCityId entity,
                                                                mobileNumber = mobileNumber_,
                                                                riderDetailId = riderDetailId entity,
                                                                updatedAt = updatedAt entity},
                                         "")}
instance EncryptedItem' RiderDriverCorrelation
    where type UnencryptedItem RiderDriverCorrelation = DecryptedRiderDriverCorrelation
          toUnencrypted a salt = (a, salt)
          fromUnencrypted = fst



