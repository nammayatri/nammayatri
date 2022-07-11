module Storage.Queries.Driveronboarding.VehicleRegistrationCert where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Storage.Tabular.Driveronboarding.VehicleRegistrationCert
import Domain.Types.Driveronboarding.VehicleRegistrationCert
import Domain.Types.Person (Person)
import Storage.Tabular.Person ()

create :: VehicleRegistrationCert -> SqlDB ()
create = Esq.create

findById ::
  EsqDBFlow m r =>
  Id VehicleRegistrationCert ->
  m (Maybe VehicleRegistrationCert)
findById = Esq.findById

 
findByPId ::
  Transactionable m =>
  Id Person ->
  m (Maybe VehicleRegistrationCert)
findByPId personid = do
  findOne $ do
    vechileRegCert <- from $ table @VehicleRegistrationCertT
    where_ $ vechileRegCert ^. VehicleRegistrationCertDriverId ==. val (toKey personid)
    return vechileRegCert
    
-- data to be update in DB:
--         rc status
--         vehicle class
--         rc start date
--         rc exp. date
--         permit expiry
--         insurance expiry

updateRCDetails :: Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe UTCTime -> Maybe UTCTime -> IdfyStatus -> Maybe COV -> SqlDB ()
updateRCDetails requestId permitStart permitValidity fitnessExpiry insuranceValidity status cov = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ VehicleRegistrationCertVehicleClass =. val cov,
        VehicleRegistrationCertPermitStart =. val permitStart,
        VehicleRegistrationCertPermitExpiry =. val permitValidity,
        VehicleRegistrationCertFitnessCertExpiry =. val fitnessExpiry,
        VehicleRegistrationCertInsuranceValidity =. val insuranceValidity,
        VehicleRegistrationCertRcStatus =. val status
      ]
    where_ $ tbl ^. VehicleRegistrationCertRequest_id  ==. val requestId
