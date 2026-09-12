{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Queries over the dashboard audit trail, parameterised by the action type of
-- whichever package owns the endpoint.
--
-- The @endpoint@ column is the one place the typed action is rendered: it has
-- always stored @show@ output (that is what @mkBeamInstancesForEnum@ did), so
-- the conversion lives here and nowhere else. Everything above this module
-- keeps the enum.
module Storage.Queries.Transaction where

import qualified Dashboard.Common.Driver as Common
import qualified Data.Text as T
import qualified Database.Beam as B
import qualified Domain.Types.DashboardActionType as DashAuth
import Domain.Types.Person as DP
import Domain.Types.ServerName as DSN
import Domain.Types.Transaction as DT
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.BeamFlow
import qualified Storage.Beam.Common as SBC
import qualified Storage.Beam.DashboardTransaction as BeamDT
import qualified Storage.Beam.Person as BeamP
import Storage.Queries.Person ()

-- | Plain conversions rather than @ToTType'@/@FromTType'@ instances: those
-- classes carry a functional dependency @t -> a@, which would pin this one Beam
-- table to a single action type. Ordinary functions have no such restriction, so
-- one table serves every owning package.
toBeamTransaction :: Show uat => DT.Transaction uat -> BeamDT.DashboardTransaction
toBeamTransaction DT.Transaction {..} =
  BeamDT.DashboardTransactionT
    { id = getId id,
      requestorId = getId <$> requestorId,
      merchantId = getId <$> merchantId,
      commonDriverId = getId <$> commonDriverId,
      commonRideId = getId <$> commonRideId,
      endpoint = T.pack $ show endpoint,
      ..
    }

fromBeamTransaction :: Read uat => BeamDT.DashboardTransaction -> DT.Transaction uat
fromBeamTransaction BeamDT.DashboardTransactionT {..} =
  DT.Transaction
    { id = Id id,
      requestorId = Id <$> requestorId,
      merchantId = Id <$> merchantId,
      commonDriverId = Id <$> commonDriverId,
      commonRideId = Id <$> commonRideId,
      -- unparseable rows must not take the whole listing down
      endpoint = fromMaybe DT.UnknownEndpoint (readMaybe (T.unpack endpoint)),
      ..
    }

-- | KV-backed write for lib-dashboard's own audit rows. Concrete in the action
-- type, which is what @ToTType'@'s functional dependency @t -> a@ requires --
-- one Beam type determines one domain type. Owning packages that write their own
-- action types declare their own Beam type and instance the same way.
instance ToTType' BeamDT.DashboardTransaction (DT.Transaction DashAuth.DashboardActionType) where
  toTType' = toBeamTransaction

createDashboardTransaction :: BeamFlow m r => DT.Transaction DashAuth.DashboardActionType -> m ()
createDashboardTransaction = createWithKV

-- | Raw insert for callers whose action type is not statically known (the
-- dashboards, which proxy both platforms). Bypasses KV: correct, but the row is
-- only visible to readers that hit Postgres.
create :: (BeamFlow m r, Show uat) => DT.Transaction uat -> m ()
create txn = do
  dbConf <- getMasterBeamConfig
  void $
    L.runDB dbConf $
      L.insertRows $
        B.insert (SBC.dashboardTransaction SBC.atlasDB) $
          B.insertValues [toBeamTransaction txn]

fetchLastTransaction :: (BeamFlow m r, Show uat, Read uat) => DT.Endpoint uat -> DSN.ServerName -> m (Maybe (DT.Transaction uat))
fetchLastTransaction endpoint serverName = do
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ 1 $
            B.orderBy_ (\t -> B.desc_ t.createdAt) $
              B.filter_'
                ( \t ->
                    t.endpoint B.==?. B.val_ (T.pack $ show endpoint)
                      B.&&?. t.serverName B.==?. B.val_ (Just serverName)
                )
                $ B.all_ (SBC.dashboardTransaction SBC.atlasDB)
  pure $ either (const Nothing) (fmap fromBeamTransaction . listToMaybe) res

findAllTransactionsByLimitOffset ::
  (BeamFlow m r, Show uat, Read uat) =>
  Maybe Text ->
  Maybe DbHash ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe (Id DP.Person) ->
  Maybe (Id Common.Driver) ->
  Maybe (Id Common.Ride) ->
  Maybe (DT.Endpoint uat) ->
  Maybe DSN.ServerName ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  m [(DT.Transaction uat, DP.Person)]
findAllTransactionsByLimitOffset mbSearchString mbSearchStrDBHash mbLimit mbOffset mbRequestorId mbDriverId mbRideId mbEndpoint mbServerName mbFrom mbTo = do
  let limitVal = fromMaybe 5 mbLimit
      offsetVal = fromMaybe 0 mbOffset
  dbConf <- getReplicaBeamConfig
  res <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.limit_ limitVal $
          B.offset_ offsetVal $
            B.orderBy_ (\(transaction, _) -> B.desc_ transaction.createdAt) $
              B.filter_'
                ( \(transaction, person) ->
                    ( maybe (B.sqlBool_ $ B.val_ True) (\searchString -> B.sqlBool_ (B.concat_ [person.firstName, person.lastName] `B.like_` B.val_ ("%" <> searchString <> "%"))) mbSearchString
                        B.||?. maybe (B.sqlBool_ $ B.val_ True) (\searchStrDBHash -> person.mobileNumberHash B.==?. B.val_ searchStrDBHash) mbSearchStrDBHash
                    )
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\requestorId -> person.id B.==?. B.val_ requestorId.getId) mbRequestorId
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\requestorId -> transaction.requestorId B.==?. B.val_ (Just requestorId.getId)) mbRequestorId
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\rideId -> transaction.commonRideId B.==?. B.val_ (Just rideId.getId)) mbRideId
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\driverId -> transaction.commonDriverId B.==?. B.val_ (Just driverId.getId)) mbDriverId
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\endpoint -> transaction.endpoint B.==?. B.val_ (T.pack $ show endpoint)) mbEndpoint
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\serverName -> transaction.serverName B.==?. B.val_ (Just serverName)) mbServerName
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\fromTime -> B.sqlBool_ (transaction.createdAt B.>=. B.val_ fromTime)) mbFrom
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\toTime -> B.sqlBool_ (transaction.createdAt B.<=. B.val_ toTime)) mbTo
                )
                $ do
                  transaction <- B.all_ (SBC.dashboardTransaction SBC.atlasDB)
                  person <- B.join_' (SBC.person SBC.atlasDB) (\person -> BeamDT.requestorId transaction B.==?. B.just_ (BeamP.id person))
                  pure (transaction, person)
  case res of
    Right res' -> do
      finalRes <- forM res' $ \(transaction, person) -> runMaybeT $ do
        p <- MaybeT $ fromTType' person
        pure (fromBeamTransaction transaction, p)
      pure $ catMaybes finalRes
    Left err -> do
      logError $ "findAllTransactionsByLimitOffset failed: " <> T.pack (show err)
      throwError (InternalError $ "Failed to fetch transactions: " <> T.pack (show err))
