module Storage.Queries.SapJournalExtra where

import qualified Database.Beam as B
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Lib.Finance.Domain.Types.SapJournalEntry as Domain
import Lib.Finance.Storage.Queries.OrphanInstances.SapJournalEntry ()
import qualified Storage.Beam.Common as BeamCommon

findByMerchantIdWithFilters ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  Text ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Domain.TransactionType ->
  Maybe Domain.JournalEntryStatus ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  m [Domain.SapJournalEntry]
findByMerchantIdWithFilters merchantId merchantOperatingCityId mbDateFrom mbDateTo mbTransactionType mbStatus mbBatchId mbBelnr mbGlNumber mbLimit mbOffset = do
  let limitVal = fromIntegral $ min 100 $ fromMaybe 20 mbLimit
      offsetVal = fromIntegral $ fromMaybe 0 mbOffset
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ limitVal $
            B.offset_ offsetVal $
              B.orderBy_ (\entry -> B.desc_ entry.createdAt) $
                B.filter_'
                  ( \entry ->
                      entry.merchantId B.==?. B.val_ merchantId
                        B.&&?. entry.merchantOperatingCityId B.==?. B.val_ merchantOperatingCityId
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\fromDate -> B.sqlBool_ (entry.createdAt B.>=. B.val_ fromDate)) mbDateFrom
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\toDate -> B.sqlBool_ (entry.createdAt B.<=. B.val_ toDate)) mbDateTo
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\txnType -> entry.transactionType B.==?. B.val_ txnType) mbTransactionType
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\st -> entry.status B.==?. B.val_ st) mbStatus
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\bid -> entry.batchId B.==?. B.val_ bid) mbBatchId
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\belnr -> entry.belnr B.==?. B.val_ (Just belnr)) mbBelnr
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\gl -> B.sqlBool_ (B.customExpr_ ("gl_number @> ARRAY['" <> fromString (toString gl) <> "']::text[]"))) mbGlNumber
                  )
                  (B.all_ (BeamCommon.sapJournalEntry BeamCommon.atlasDB))
  case res of
    Right entries -> catMaybes <$> mapM fromTType' entries
    Left _ -> pure []
