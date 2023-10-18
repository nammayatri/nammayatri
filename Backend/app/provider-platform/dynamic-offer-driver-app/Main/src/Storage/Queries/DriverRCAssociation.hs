{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.DriverRCAssociation where

import qualified Database.Beam as B
import Domain.Types.DriverOnboarding.DriverRCAssociation as DriverRCAssociation
import Domain.Types.DriverOnboarding.VehicleRegistrationCertificate as VehicleRegistrationCertificate
import Domain.Types.FleetDriverAssociation as FleetDriverAssociation
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Storage.Beam.Common as SBC
import qualified Storage.Beam.DriverOnboarding.DriverRCAssociation as BeamDRCA
import qualified Storage.Beam.DriverOnboarding.VehicleRegistrationCertificate as BeamVRC
import qualified Storage.Beam.FleetDriverAssociation as BeamFDA
import Storage.Queries.FleetDriverAssociation ()
import Storage.Queries.VehicleRegistrationCertificate ()

instance FromTType' BeamDRCA.DriverRCAssociation DriverRCAssociation where
  fromTType' BeamDRCA.DriverRCAssociationT {..} = do
    pure $
      Just
        DriverRCAssociation
          { id = Id id,
            driverId = Id driverId,
            rcId = Id rcId,
            associatedOn = associatedOn,
            associatedTill = associatedTill,
            consent = consent,
            consentTimestamp = consentTimestamp,
            isRcActive = isRcActive
          }

instance ToTType' BeamDRCA.DriverRCAssociation DriverRCAssociation where
  toTType' DriverRCAssociation {..} = do
    BeamDRCA.DriverRCAssociationT
      { BeamDRCA.id = getId id,
        BeamDRCA.driverId = getId id,
        BeamDRCA.rcId = getId id,
        BeamDRCA.associatedOn = associatedOn,
        BeamDRCA.associatedTill = associatedTill,
        BeamDRCA.consent = consent,
        BeamDRCA.consentTimestamp = consentTimestamp,
        BeamDRCA.isRcActive = isRcActive
      }

mapping :: MonadFlow m => Maybe Text -> Maybe Int -> Maybe Int -> m [(VehicleRegistrationCertificate, FleetDriverAssociation, DriverRCAssociation)]
mapping fleetIdWanted mbLimit mbOffset = do
  dbConf <- getMasterBeamConfig
  res <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.limit_ (fromIntegral $ fromMaybe 100 mbLimit) $
          B.offset_ (fromIntegral $ fromMaybe 0 mbOffset) $
            do
              vehicleRC' <- B.filter_' (\vehicleRC'' -> BeamVRC.fleetOwnerId vehicleRC'' B.==?. B.val_ fleetIdWanted) (B.all_ (SBC.vehicleRegistrationCertificate SBC.atlasDB))
              fleetDriverAssn' <- B.join_' (SBC.fleetDriverAssociation SBC.atlasDB) (\fleetDriverAssn'' -> (B.just_ $ fleetDriverAssn''.fleetOwnerId) B.==?. vehicleRC'.fleetOwnerId)
              driverRCAssn' <- B.join_' (SBC.driverRCAssociation SBC.atlasDB) (\driverRCAssn'' -> BeamDRCA.driverId driverRCAssn'' B.==?. BeamFDA.driverId fleetDriverAssn')
              pure (vehicleRC', fleetDriverAssn', driverRCAssn')
  case res of
    Right res' -> do
      let vrc' = fmap fst' res'
          fda' = fmap snd' res'
          drca' = fmap thd' res'
      vrc <- catMaybes <$> mapM fromTType' vrc'
      fda <- catMaybes <$> mapM fromTType' fda'
      drca <- catMaybes <$> mapM fromTType' drca'
      pure $ zip3 vrc fda drca
    Left _ -> pure []
  where
    fst' (x, _, _) = x
    snd' (_, y, _) = y
    thd' (_, _, z) = z
