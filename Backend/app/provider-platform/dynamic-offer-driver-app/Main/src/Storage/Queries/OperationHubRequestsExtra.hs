{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OperationHubRequestsExtra where

import qualified Data.Text as T
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
import qualified Storage.Beam.OperationHub as BeamOH
import qualified Storage.Beam.OperationHubRequests as BeamOHR
import qualified Storage.Beam.Person as BeamP
import Storage.Queries.OrphanInstances.OperationHub ()
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
  Maybe Text ->
  Maybe (Id Person) ->
  m [(OperationHubRequests, Person, DOH.OperationHub)]
findAllRequestsInRange from to limit offset mbMobileNumberHash mbReqStatus mbReqType mbCreatorId mbOperationHubId mbOperationHubName mbRegistrationNo mbDriverId = do
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ (toInteger limit) $
            B.offset_ (toInteger offset) $
              B.filter_'
                ( \(operationHubRequests, driver, operationHub) ->
                    B.sqlBool_ (operationHubRequests.createdAt B.>=. B.val_ from)
                      B.&&?. B.sqlBool_ (operationHubRequests.createdAt B.<=. B.val_ to)
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\creatorId -> operationHubRequests.creatorId B.==?. B.val_ creatorId) mbCreatorId
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\reqType -> operationHubRequests.requestType B.==?. B.val_ reqType) mbReqType
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\reqStatus -> operationHubRequests.requestStatus B.==?. B.val_ reqStatus) mbReqStatus
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\mobileNumberSearchStringDB -> driver.mobileNumberHash B.==?. B.val_ (Just mobileNumberSearchStringDB)) mbMobileNumberHash
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\operationHubId -> operationHubRequests.operationHubId B.==?. B.val_ operationHubId.getId) mbOperationHubId
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\registrationNo -> B.maybe_ (B.sqlBool_ $ B.val_ False) (\rcNo -> B.sqlBool_ (B.lower_ rcNo `B.like_` (B.val_ ("%" <> T.toLower registrationNo <> "%")))) operationHubRequests.registrationNo) mbRegistrationNo
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\operationHubName -> B.sqlBool_ (B.lower_ operationHub.name `B.like_` (B.val_ ("%" <> T.toLower operationHubName <> "%")))) mbOperationHubName
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\driverId -> operationHubRequests.driverId B.==?. B.val_ (Just driverId.getId)) mbDriverId
                )
                do
                  operationHubRequests <- B.all_ (BeamCommon.operationHubRequests BeamCommon.atlasDB)
                  operationHub <- B.join_ (BeamCommon.operationHub BeamCommon.atlasDB) (\operationHub -> BeamOHR.operationHubId operationHubRequests B.==. BeamOH.id operationHub)
                  driver <- B.join_ (BeamCommon.person BeamCommon.atlasDB) (\driver -> BeamOHR.creatorId operationHubRequests B.==. BeamP.id driver)
                  pure (operationHubRequests, driver, operationHub)
  case res of
    Right res' -> do
      finalRes <- forM res' $ \(operationHubRequests, person, operationHub) -> runMaybeT $ do
        ohr <- MaybeT $ fromTType' operationHubRequests
        p <- MaybeT $ fromTType' person
        oh <- MaybeT $ fromTType' operationHub
        pure (ohr, p, oh)
      pure $ catMaybes finalRes
    Left err -> do
      logError $ "Error in findAllRequestsInRange " <> show err
      pure []

deleteByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id Person -> m ()
deleteByDriverId driverId = do
  deleteWithKV [Se.Is BeamOHR.creatorId $ Se.Eq (getId driverId)]

findPendingRequestByOperatorId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id Person -> m (Maybe OperationHubRequests)
findPendingRequestByOperatorId operatorId = do
  findOneWithKV
    [ Se.And
        [ Se.Is BeamOHR.operatorId $ Se.Eq (Just $ getId operatorId),
          Se.Is BeamOHR.requestStatus $ Se.Eq PENDING
        ]
    ]

deleteByOperatorId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id Person -> m ()
deleteByOperatorId operatorId = do
  deleteWithKV [Se.Is BeamOHR.operatorId $ Se.Eq (Just $ getId operatorId)]

findLatestByDriverIdAndRequestType :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id Person -> RequestType -> m (Maybe OperationHubRequests)
findLatestByDriverIdAndRequestType driverId requestType = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamOHR.driverId $ Se.Eq (Just $ getId driverId),
          Se.Is BeamOHR.requestType $ Se.Eq requestType
        ]
    ]
    (Se.Desc BeamOHR.createdAt)
    (Just 1)
    Nothing
    <&> listToMaybe

findLatestByRegistrationNoAndRequestType :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Text -> RequestType -> m (Maybe OperationHubRequests)
findLatestByRegistrationNoAndRequestType registrationNo requestType = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamOHR.registrationNo $ Se.Eq (Just registrationNo),
          Se.Is BeamOHR.requestType $ Se.Eq requestType
        ]
    ]
    (Se.Desc BeamOHR.createdAt)
    (Just 1)
    Nothing
    <&> listToMaybe

-- Duplicate check: any PENDING request for this driver (any creator)
findPendingByDriverIdAndRequestType :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id Person -> RequestType -> m (Maybe OperationHubRequests)
findPendingByDriverIdAndRequestType driverId requestType =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamOHR.driverId $ Se.Eq (Just $ getId driverId),
          Se.Is BeamOHR.requestType $ Se.Eq requestType,
          Se.Is BeamOHR.requestStatus $ Se.Eq PENDING
        ]
    ]

-- Duplicate check: any PENDING request for this RC (any creator)
findPendingByRegistrationNoAndRequestType :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Text -> RequestType -> m (Maybe OperationHubRequests)
findPendingByRegistrationNoAndRequestType registrationNo requestType =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamOHR.registrationNo $ Se.Eq (Just registrationNo),
          Se.Is BeamOHR.requestType $ Se.Eq requestType,
          Se.Is BeamOHR.requestStatus $ Se.Eq PENDING
        ]
    ]
