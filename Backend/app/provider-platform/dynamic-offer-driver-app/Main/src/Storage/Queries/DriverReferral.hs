module Storage.Queries.DriverReferral where

import Domain.Types.DriverReferral
import qualified Domain.Types.Person as SP
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.DriverReferral as BeamDR
import Storage.Tabular.DriverReferral

create :: DriverReferral -> SqlDB ()
create = Esq.create

findByRefferalCode :: Transactionable m => Id DriverReferral -> m (Maybe DriverReferral)
findByRefferalCode = Esq.findById

findById ::
  Transactionable m =>
  Id SP.Person ->
  m (Maybe DriverReferral)
findById driverId = do
  findOne $ do
    driverReferral <- from $ table @DriverReferralT
    where_ $ driverReferral ^. DriverReferralDriverId ==. val (toKey driverId)
    return driverReferral

transformBeamDriverReferralToDomain :: BeamDR.DriverReferral -> DriverReferral
transformBeamDriverReferralToDomain BeamDR.DriverReferralT {..} = do
  DriverReferral
    { referralCode = Id referralCode,
      driverId = Id driverId,
      linkedAt = linkedAt
    }

transformDomainDriverReferralToBeam :: DriverReferral -> BeamDR.DriverReferral
transformDomainDriverReferralToBeam DriverReferral {..} =
  BeamDR.DriverReferralT
    { BeamDR.referralCode = getId referralCode,
      BeamDR.driverId = getId driverId,
      BeamDR.linkedAt = linkedAt
    }
