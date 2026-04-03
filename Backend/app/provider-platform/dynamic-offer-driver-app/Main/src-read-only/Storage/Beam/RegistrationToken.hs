{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.RegistrationToken where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Domain.Types.RegistrationToken
import qualified Database.Beam as B



data RegistrationTokenT f
    = RegistrationTokenT {alternateNumberAttempts :: (B.C f Kernel.Prelude.Int),
                          attempts :: (B.C f Kernel.Prelude.Int),
                          authExpiry :: (B.C f Kernel.Prelude.Int),
                          authMedium :: (B.C f Domain.Types.RegistrationToken.Medium),
                          authType :: (B.C f Domain.Types.RegistrationToken.LoginType),
                          authValueHash :: (B.C f Kernel.Prelude.Text),
                          createdAt :: (B.C f Kernel.Prelude.UTCTime),
                          entityId :: (B.C f Kernel.Prelude.Text),
                          entityType :: (B.C f Domain.Types.RegistrationToken.RTEntityType),
                          id :: (B.C f Kernel.Prelude.Text),
                          info :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                          merchantId :: (B.C f Kernel.Prelude.Text),
                          merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                          token :: (B.C f Kernel.Prelude.Text),
                          tokenExpiry :: (B.C f Kernel.Prelude.Int),
                          updatedAt :: (B.C f Kernel.Prelude.UTCTime),
                          verified :: (B.C f Kernel.Prelude.Bool)}
    deriving (Generic, B.Beamable)
instance B.Table RegistrationTokenT
    where data PrimaryKey RegistrationTokenT f = RegistrationTokenId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = RegistrationTokenId . id
type RegistrationToken = RegistrationTokenT Identity

$(enableKVPG (''RegistrationTokenT) [('id)] [[('entityId)], [('token)]])

$(mkTableInstances (''RegistrationTokenT) "registration_token")

