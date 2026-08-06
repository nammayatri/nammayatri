{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TransactionExtra where

import qualified Data.Text
import qualified Database.Beam as B
import qualified Domain.Types.AccessMatrix
import qualified Domain.Types.Person
import qualified Domain.Types.Transaction
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.BeamFlow
import qualified Storage.Beam.Common as SBC
import qualified Storage.Beam.Person as BeamP
import qualified Storage.Beam.Transaction as BeamT
import Storage.Queries.OrphanInstances.Person
import Storage.Queries.OrphanInstances.Transaction

-- Beam query for listing transactions with Person join and filters
findAllTransactionsByLimitOffset ::
  (BeamFlow m r) =>
  Kernel.Prelude.Maybe Data.Text.Text ->
  Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash ->
  Kernel.Prelude.Maybe Kernel.Prelude.Integer ->
  Kernel.Prelude.Maybe Kernel.Prelude.Integer ->
  Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) ->
  Kernel.Prelude.Maybe Data.Text.Text ->
  Kernel.Prelude.Maybe Data.Text.Text ->
  Kernel.Prelude.Maybe Domain.Types.AccessMatrix.UserActionType ->
  m [(Domain.Types.Transaction.Transaction, Domain.Types.Person.Person)]
findAllTransactionsByLimitOffset mbSearchString mbSearchStrDBHash mbLimit mbOffset mbRequestorId mbDriverId mbRideId mbEndpoint = do
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
                        B.||?. maybe (B.sqlBool_ $ B.val_ True) (\searchStrDBHash -> person.mobileNumberHash B.==?. B.just_ (B.val_ searchStrDBHash)) mbSearchStrDBHash
                    )
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\requestorId -> person.id B.==?. B.val_ requestorId.getId) mbRequestorId
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\requestorId -> transaction.requestorId B.==?. B.just_ (B.val_ requestorId.getId)) mbRequestorId
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\rideId -> transaction.commonRideId B.==?. B.just_ (B.val_ rideId)) mbRideId
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\driverId -> transaction.commonDriverId B.==?. B.just_ (B.val_ driverId)) mbDriverId
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\endpoint -> transaction.endpoint B.==?. B.val_ endpoint) mbEndpoint
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
    Left _ -> pure []
