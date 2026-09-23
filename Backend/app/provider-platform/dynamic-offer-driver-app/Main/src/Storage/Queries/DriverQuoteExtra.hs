module Storage.Queries.DriverQuoteExtra where

import qualified Data.Time as T
import Domain.Types.DriverQuote
import qualified Domain.Types.DriverQuote as Domain
import qualified Domain.Types.Estimate as DEstimate
import Domain.Types.Person
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchTry as DST
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.DriverQuote as BeamDQ
import qualified Storage.Queries.FareParameters as SQFP
import Storage.Queries.OrphanInstances.DriverQuote ()

-- Extra code goes here --

findAllBySTId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DST.SearchTry -> m [Domain.DriverQuote]
findAllBySTId (Id searchTryId) =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDQ.searchTryId $ Se.Eq searchTryId,
          Se.Is BeamDQ.status $ Se.Eq Domain.Active
        ]
    ]

findAllBySTIdIgnoringStatus :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DST.SearchTry -> m [Domain.DriverQuote]
findAllBySTIdIgnoringStatus (Id searchTryId) =
  findAllWithKVAndConditionalDB
    [Se.Is BeamDQ.searchTryId $ Se.Eq searchTryId]
    Nothing

countAllBySTId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DST.SearchTry -> m Int
countAllBySTId searchTId =
  findAllWithKVAndConditionalDB
    [ Se.And
        [ Se.Is BeamDQ.searchTryId $ Se.Eq (getId searchTId),
          Se.Is BeamDQ.status $ Se.Eq Domain.Active
        ]
    ]
    Nothing
    <&> length

setInactiveAllDQByEstId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DEstimate.Estimate -> UTCTime -> m () -- push
setInactiveAllDQByEstId (Id estimateId) now = updateWithKV [Se.Set BeamDQ.status Domain.Inactive, Se.Set BeamDQ.updatedAt (T.utcToLocalTime T.utc now)] [Se.And [Se.Is BeamDQ.estimateId $ Se.Eq estimateId, Se.Is BeamDQ.status $ Se.Eq Domain.Active, Se.Is BeamDQ.validTill $ Se.GreaterThan (T.utcToLocalTime T.utc now)]]

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverQuote.DriverQuote -> m ())
create tbl = do SQFP.create tbl.fareParams; createWithKV tbl

setInactiveBySTId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DST.SearchTry -> m ()
setInactiveBySTId (Id searchTryId) = updateWithKV [Se.Set BeamDQ.status Domain.Inactive] [Se.Is BeamDQ.searchTryId $ Se.Eq searchTryId]

setInactiveBySRId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DSR.SearchRequest -> m ()
setInactiveBySRId (Id searchReqId) = updateWithKV [Se.Set BeamDQ.status Domain.Inactive] [Se.Is BeamDQ.requestId $ Se.Eq searchReqId]

-- | /init's fulfillment id for the estimate-based flow is the estimate id for an ONDC
-- (non-value-add) BAP -- that is what Beckn.ACL.OnSelect.mkFulfillmentV2 announced -- not the
-- DriverQuote's own id. This resolves it back to the DriverQuote /init actually needs; see
-- Domain.Action.Beckn.Init.validateRequest. At most one Active DriverQuote exists per estimate
-- at a time -- setInactiveAllDQByEstId invalidates any prior one before a new one is created at
-- select-time -- so no tie-break is needed here.
findActiveByEstimateId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DEstimate.Estimate -> m (Maybe Domain.DriverQuote)
findActiveByEstimateId (Id estimateId) =
  listToMaybe
    <$> findAllWithKVAndConditionalDB
      [ Se.And
          [ Se.Is BeamDQ.estimateId $ Se.Eq estimateId,
            Se.Is BeamDQ.status $ Se.Eq Domain.Active
          ]
      ]
      Nothing

findActiveQuotesByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Seconds -> m [Domain.DriverQuote]
findActiveQuotesByDriverId (Id driverId) driverUnlockDelay = do
  now <- getCurrentTime
  let delayToAvoidRaces = secondsToNominalDiffTime . negate $ driverUnlockDelay
  findAllFromKvRedis
    [ Se.And
        [ Se.Is BeamDQ.status $ Se.Eq Domain.Active,
          Se.Is BeamDQ.driverId $ Se.Eq driverId,
          Se.Is BeamDQ.validTill $ Se.GreaterThan (T.utcToLocalTime T.utc $ addUTCTime delayToAvoidRaces now)
        ]
    ]
    Nothing
