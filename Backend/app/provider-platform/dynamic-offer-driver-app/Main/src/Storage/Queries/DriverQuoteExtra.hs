{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverQuoteExtra where

import qualified Data.Text as T
import qualified Data.Time as T
import qualified Domain.Types.Common as DTC
import Domain.Types.DriverQuote
import qualified Domain.Types.DriverQuote as Domain
import qualified Domain.Types.Estimate as DEstimate
import Domain.Types.Person
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchTry as DST
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Version
import qualified Sequelize as Se
import SharedLogic.DriverPool.Types
import qualified Storage.Beam.DriverQuote as BeamDQ
import Storage.Queries.FareParameters as BeamQFP
import qualified Storage.Queries.FareParameters as SQFP
import Storage.Queries.OrphanInstances.DriverQuote

-- Extra code goes here --

findAllBySTId :: KvDbFlow m r => Id DST.SearchTry -> m [Domain.DriverQuote]
findAllBySTId (Id searchTryId) =
  findAllWithKVAndConditionalDB
    [ Se.And
        [ Se.Is BeamDQ.searchTryId $ Se.Eq searchTryId,
          Se.Is BeamDQ.status $ Se.Eq Domain.Active
        ]
    ]
    Nothing

countAllBySTId :: KvDbFlow m r => Id DST.SearchTry -> m Int
countAllBySTId searchTId =
  findAllWithKVAndConditionalDB
    [ Se.And
        [ Se.Is BeamDQ.searchTryId $ Se.Eq (getId searchTId),
          Se.Is BeamDQ.status $ Se.Eq Domain.Active
        ]
    ]
    Nothing
    <&> length

setInactiveAllDQByEstId :: KvDbFlow m r => Id DEstimate.Estimate -> UTCTime -> m () -- push
setInactiveAllDQByEstId (Id estimateId) now = updateWithKV [Se.Set BeamDQ.status Domain.Inactive, Se.Set BeamDQ.updatedAt (T.utcToLocalTime T.utc now)] [Se.And [Se.Is BeamDQ.estimateId $ Se.Eq estimateId, Se.Is BeamDQ.status $ Se.Eq Domain.Active, Se.Is BeamDQ.validTill $ Se.GreaterThan (T.utcToLocalTime T.utc now)]]

create :: KvDbFlow m r => (Domain.Types.DriverQuote.DriverQuote -> m ())
create tbl = do SQFP.create tbl.fareParams; createWithKV tbl

setInactiveBySTId :: KvDbFlow m r => Id DST.SearchTry -> m ()
setInactiveBySTId (Id searchTryId) = updateWithKV [Se.Set BeamDQ.status Domain.Inactive] [Se.Is BeamDQ.searchTryId $ Se.Eq searchTryId]

setInactiveBySRId :: KvDbFlow m r => Id DSR.SearchRequest -> m ()
setInactiveBySRId (Id searchReqId) = updateWithKV [Se.Set BeamDQ.status Domain.Inactive] [Se.Is BeamDQ.requestId $ Se.Eq searchReqId]

findActiveQuotesByDriverId :: KvDbFlow m r => Id Person -> Seconds -> m [Domain.DriverQuote]
findActiveQuotesByDriverId (Id driverId) driverUnlockDelay = do
  now <- getCurrentTime
  let delayToAvoidRaces = secondsToNominalDiffTime . negate $ driverUnlockDelay
  findAllWithKVAndConditionalDB
    [ Se.And
        [ Se.Is BeamDQ.status $ Se.Eq Domain.Active,
          Se.Is BeamDQ.driverId $ Se.Eq driverId,
          Se.Is BeamDQ.validTill $ Se.GreaterThan (T.utcToLocalTime T.utc $ addUTCTime delayToAvoidRaces now)
        ]
    ]
    Nothing
