{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Safety.Domain.Types.PersonDefaultEmergencyNumber where
import Kernel.Prelude
import Kernel.External.Encryption
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Safety.Domain.Types.Common
import qualified Kernel.Beam.Lib.UtilsTH
import qualified Tools.Beam.UtilsTH



data PersonDefaultEmergencyNumberE e
    = PersonDefaultEmergencyNumber {contactPersonId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Safety.Domain.Types.Common.Person),
                                    createdAt :: Kernel.Prelude.UTCTime,
                                    enableForFollowing :: Kernel.Prelude.Bool,
                                    enableForShareRide :: Kernel.Prelude.Bool,
                                    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Safety.Domain.Types.Common.Merchant),
                                    mobileCountryCode :: Kernel.Prelude.Text,
                                    mobileNumber :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
                                    name :: Kernel.Prelude.Text,
                                    personId :: Kernel.Types.Id.Id Safety.Domain.Types.Common.Person,
                                    priority :: Kernel.Prelude.Int,
                                    shareTripWithEmergencyContactOption :: Kernel.Prelude.Maybe Safety.Domain.Types.Common.RideShareOptions}
    deriving Generic
type PersonDefaultEmergencyNumber = PersonDefaultEmergencyNumberE ('AsEncrypted)
type DecryptedPersonDefaultEmergencyNumber = PersonDefaultEmergencyNumberE ('AsUnencrypted)
instance EncryptedItem PersonDefaultEmergencyNumber
    where type Unencrypted PersonDefaultEmergencyNumber = (DecryptedPersonDefaultEmergencyNumber, HashSalt)
          encryptItem (entity, salt) = do {mobileNumber_ <- encryptItem (mobileNumber entity, salt);
                                           pure PersonDefaultEmergencyNumber{contactPersonId = contactPersonId entity,
                                                                             createdAt = createdAt entity,
                                                                             enableForFollowing = enableForFollowing entity,
                                                                             enableForShareRide = enableForShareRide entity,
                                                                             merchantId = merchantId entity,
                                                                             mobileCountryCode = mobileCountryCode entity,
                                                                             mobileNumber = mobileNumber_,
                                                                             name = name entity,
                                                                             personId = personId entity,
                                                                             priority = priority entity,
                                                                             shareTripWithEmergencyContactOption = shareTripWithEmergencyContactOption entity}}
          decryptItem entity = do {mobileNumber_ <- fst <$> decryptItem (mobileNumber entity);
                                   pure (PersonDefaultEmergencyNumber{contactPersonId = contactPersonId entity,
                                                                      createdAt = createdAt entity,
                                                                      enableForFollowing = enableForFollowing entity,
                                                                      enableForShareRide = enableForShareRide entity,
                                                                      merchantId = merchantId entity,
                                                                      mobileCountryCode = mobileCountryCode entity,
                                                                      mobileNumber = mobileNumber_,
                                                                      name = name entity,
                                                                      personId = personId entity,
                                                                      priority = priority entity,
                                                                      shareTripWithEmergencyContactOption = shareTripWithEmergencyContactOption entity},
                                         "")}
instance EncryptedItem' PersonDefaultEmergencyNumber
    where type UnencryptedItem PersonDefaultEmergencyNumber = DecryptedPersonDefaultEmergencyNumber
          toUnencrypted a salt = (a, salt)
          fromUnencrypted = fst



