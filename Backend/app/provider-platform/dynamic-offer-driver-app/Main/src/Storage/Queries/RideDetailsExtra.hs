{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RideDetailsExtra where

import qualified Database.Beam as B
import qualified Domain.Types.Ride
import qualified Domain.Types.RideDetails
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.RideDetails as Beam
import Storage.Queries.OrphanInstances.RideDetails

-- Extra code goes here --

findByCreatedAtAndVehicleNumber :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (UTCTime -> Text -> m (Maybe Domain.Types.RideDetails.RideDetails))
findByCreatedAtAndVehicleNumber fromDate vehicleNumber = do
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ 1 $
            B.filter_'
              ( \rd ->
                  rd.vehicleNumber B.==?. B.val_ vehicleNumber
                    B.&&?. B.sqlBool_ (rd.createdAt B./=. B.val_ (Nothing :: Maybe UTCTime))
                    B.&&?. B.sqlBool_ (rd.createdAt B.>. B.val_ (Just fromDate))
              )
              do
                rd <- B.all_ (BeamCommon.rideDetails BeamCommon.atlasDB)
                pure rd
  case res of
    Right res' -> do
      rides <- catMaybes <$> mapM fromTType' res'
      pure $ listToMaybe rides
    Left _ -> pure Nothing

findByCreatedAtAndVehicleNumber' :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (UTCTime -> Text -> m [Domain.Types.RideDetails.RideDetails])
findByCreatedAtAndVehicleNumber' fromDate vehicleNumber = do
  findAllWithOptionsKV'
    [ Se.And $
        [ Se.Is Beam.vehicleNumber $ Se.Eq vehicleNumber,
          Se.Is Beam.createdAt $ Se.Not Se.Null
        ]
          <> [Se.Is Beam.createdAt $ Se.GreaterThan (Just fromDate)]
    ]
    (Just 1)
    Nothing
