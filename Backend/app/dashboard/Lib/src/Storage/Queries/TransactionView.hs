{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The audit-trail listing query, with the endpoint column left as text.
--
-- Same query as 'Storage.Queries.Transaction' in lib-dashboard-api -- the join,
-- the filters and the ordering are identical. See 'Domain.Types.TransactionView'
-- for why the endpoint is not decoded.
module Storage.Queries.TransactionView
  ( findAllTransactionsByLimitOffset,
    fetchLastTransaction,
  )
where

import qualified Data.Text as T
import qualified Database.Beam as B
import Domain.Types.Person as DP
import qualified Domain.Types.ServerName as DSN
import Domain.Types.TransactionView (TransactionRow (..))
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Sequelize as Se
import Storage.Beam.BeamFlow
import qualified Storage.Beam.Common as SBC
import qualified Storage.Beam.Person as BeamP
import qualified Storage.Beam.Transaction as BeamT
import Storage.Queries.Person ()

findAllTransactionsByLimitOffset ::
  BeamFlow m r =>
  Maybe Text ->
  Maybe DbHash ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe (Id DP.Person) ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  m [(TransactionRow, DP.Person)]
findAllTransactionsByLimitOffset mbSearchString mbSearchStrDBHash mbLimit mbOffset mbRequestorId mbDriverId mbRideId mbEndpoint mbFrom mbTo = do
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
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\rideId -> transaction.commonRideId B.==?. B.val_ (Just rideId)) mbRideId
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\driverId -> transaction.commonDriverId B.==?. B.val_ (Just driverId)) mbDriverId
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\endpoint -> transaction.endpoint B.==?. B.val_ endpoint) mbEndpoint
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\fromTime -> B.sqlBool_ (transaction.createdAt B.>=. B.val_ fromTime)) mbFrom
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\toTime -> B.sqlBool_ (transaction.createdAt B.<=. B.val_ toTime)) mbTo
                )
                $ do
                  transaction <- B.all_ (SBC.transaction SBC.atlasDB)
                  person <- B.join_' (SBC.person SBC.atlasDB) (\person -> BeamT.requestorId transaction B.==?. B.just_ (BeamP.id person))
                  pure (transaction, person)
  case res of
    Right res' -> do
      finalRes <- forM res' $ \(transaction, person) -> runMaybeT $ do
        t <- MaybeT $ fromTType' transaction
        p <- MaybeT $ fromTType' person
        pure (t, p)
      pure $ catMaybes finalRes
    Left err -> do
      logError $ "findAllTransactionsByLimitOffset failed: " <> T.pack (show err)
      throwError (InternalError $ "Failed to fetch transactions: " <> T.pack (show err))

-- | Most recent row for one endpoint on one server. The endpoint is the text the
-- column holds -- @Domain.Types.Transaction.Endpoint@'s Show form, e.g.
-- @"ExotelAPI ExotelHeartbeatEndpoint"@.
fetchLastTransaction :: BeamFlow m r => Text -> DSN.ServerName -> m (Maybe TransactionRow)
fetchLastTransaction endpoint serverName =
  findAllWithOptionsKV
    [ Se.Is BeamT.endpoint $ Se.Eq endpoint,
      Se.Is BeamT.serverName $ Se.Eq (Just serverName)
    ]
    (Se.Desc BeamT.createdAt)
    (Just 1)
    Nothing
    <&> listToMaybe

instance FromTType' BeamT.Transaction TransactionRow where
  fromTType' BeamT.TransactionT {..} = pure $ Just TransactionRow {..}
