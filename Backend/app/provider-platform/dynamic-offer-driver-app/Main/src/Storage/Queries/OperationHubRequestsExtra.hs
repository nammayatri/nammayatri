{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OperationHubRequestsExtra where

import qualified Database.Beam as B
import qualified Database.Beam.Query ()
import qualified Domain.Types.OperationHub as DOH
import Domain.Types.OperationHubRequests
import Domain.Types.Person
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime, logError)
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.OperationHubRequests as BeamOHR
import qualified Storage.Beam.Person as BeamP
import Storage.Queries.OrphanInstances.OperationHubRequests ()
import Storage.Queries.OrphanInstances.Person ()

-- Extra code goes here --
findAllRequestsInRange ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  UTCTime ->
  UTCTime ->
  Int ->
  Int ->
  Maybe DbHash ->
  Maybe RequestStatus ->
  Maybe RequestType ->
  Maybe Text ->
  Maybe (Id DOH.OperationHub) ->
  Maybe Text ->
  m [(OperationHubRequests, Person)]
findAllRequestsInRange from to limit offset mbMobileNumberHash mbReqStatus mbReqType mbDriverId mbOperationHubId mbRegistrationNo = do
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ (toInteger limit) $
            B.offset_ (toInteger offset) $
              B.filter_'
                ( \(operationHubRequests, driver) ->
                    B.sqlBool_ (operationHubRequests.createdAt B.>=. B.val_ from)
                      B.&&?. B.sqlBool_ (operationHubRequests.createdAt B.<=. B.val_ to)
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\driverId -> operationHubRequests.driverId B.==?. B.val_ driverId) mbDriverId
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\reqType -> operationHubRequests.requestType B.==?. B.val_ reqType) mbReqType
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\reqStatus -> operationHubRequests.requestStatus B.==?. B.val_ reqStatus) mbReqStatus
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\mobileNumberSearchStringDB -> driver.mobileNumberHash B.==?. B.val_ (Just mobileNumberSearchStringDB)) mbMobileNumberHash
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\operationHubId -> operationHubRequests.operationHubId B.==?. B.val_ operationHubId.getId) mbOperationHubId
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\registrationNo -> operationHubRequests.registrationNo B.==?. B.val_ registrationNo) mbRegistrationNo
                )
                do
                  operationHubRequests <- B.all_ (BeamCommon.operationHubRequests BeamCommon.atlasDB)
                  driver <- B.join_ (BeamCommon.person BeamCommon.atlasDB) (\driver -> BeamOHR.driverId operationHubRequests B.==. BeamP.id driver)
                  pure (operationHubRequests, driver)
  case res of
    Right res' -> do
      finalRes <- forM res' $ \(operationHubRequests, person) -> runMaybeT $ do
        o <- MaybeT $ fromTType' operationHubRequests
        p <- MaybeT $ fromTType' person
        pure (o, p)
      pure $ catMaybes finalRes
    Left err -> do
      logError $ "Error in findAllRequestsInRange " <> show err
      pure []
