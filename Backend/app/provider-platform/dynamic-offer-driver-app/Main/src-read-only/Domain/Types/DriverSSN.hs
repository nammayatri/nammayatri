{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.DriverSSN where
import Kernel.Prelude
import Kernel.External.Encryption
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Kernel.Types.Documents
import qualified Tools.Beam.UtilsTH



data DriverSSNE e
    = DriverSSN {driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
                 id :: Kernel.Types.Id.Id Domain.Types.DriverSSN.DriverSSN,
                 rejectReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                 ssn :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
                 verificationStatus :: Kernel.Types.Documents.VerificationStatus}
    deriving Generic
type DriverSSN = DriverSSNE ('AsEncrypted)
type DecryptedDriverSSN = DriverSSNE ('AsUnencrypted)
instance EncryptedItem DriverSSN
    where type Unencrypted DriverSSN = (DecryptedDriverSSN, HashSalt)
          encryptItem (entity, salt) = do {ssn_ <- encryptItem (ssn entity, salt);
                                           pure DriverSSN{driverId = driverId entity, id = id entity, rejectReason = rejectReason entity, ssn = ssn_, verificationStatus = verificationStatus entity}}
          decryptItem entity = do {ssn_ <- fst <$> decryptItem (ssn entity);
                                   pure (DriverSSN{driverId = driverId entity, id = id entity, rejectReason = rejectReason entity, ssn = ssn_, verificationStatus = verificationStatus entity}, "")}
instance EncryptedItem' DriverSSN
    where type UnencryptedItem DriverSSN = DecryptedDriverSSN
          toUnencrypted a salt = (a, salt)
          fromUnencrypted = fst



