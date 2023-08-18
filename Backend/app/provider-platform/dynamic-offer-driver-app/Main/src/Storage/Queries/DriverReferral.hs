{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.DriverReferral where

import Domain.Types.DriverReferral as DDR
import qualified Domain.Types.Person as SP
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Logging
import qualified Sequelize as Se
import qualified Storage.Beam.DriverReferral as BeamDR

create :: (L.MonadFlow m, Log m) => DDR.DriverReferral -> m ()
create = createWithKV

findByRefferalCode ::
  (L.MonadFlow m, Log m) =>
  Id DriverReferral ->
  m (Maybe DriverReferral)
findByRefferalCode (Id referralId) = findOneWithKV [Se.Is BeamDR.referralCode $ Se.Eq referralId]

findById ::
  (L.MonadFlow m, Log m) =>
  Id SP.Person ->
  m (Maybe DriverReferral)
findById (Id driverId) = findOneWithKV [Se.Is BeamDR.driverId $ Se.Eq driverId]

instance FromTType' BeamDR.DriverReferral DriverReferral where
  fromTType' BeamDR.DriverReferralT {..} = do
    pure $
      Just
        DriverReferral
          { referralCode = Id referralCode,
            driverId = Id driverId,
            linkedAt = linkedAt
          }

instance ToTType' BeamDR.DriverReferral DriverReferral where
  toTType' DriverReferral {..} = do
    BeamDR.DriverReferralT
      { BeamDR.referralCode = getId referralCode,
        BeamDR.driverId = getId driverId,
        BeamDR.linkedAt = linkedAt
      }
