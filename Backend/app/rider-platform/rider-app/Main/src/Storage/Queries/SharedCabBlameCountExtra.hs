module Storage.Queries.SharedCabBlameCountExtra where

import qualified Database.Beam as B
import qualified Database.Beam.Postgres.Full as BF
import qualified Domain.Types.SharedCabBlameCount as Domain
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Utils.Common (EsqDBFlow, MonadFlow)
import qualified Storage.Beam.SharedCabBlameCount as Beam
import Storage.Queries.OrphanInstances.SharedCabBlameCount ()

newtype BlameDB f = BlameDB {blame :: f (B.TableEntity Beam.SharedCabBlameCountT)}
  deriving (Generic, B.Database be)

blameDB :: B.DatabaseSettings be BlameDB
blameDB = B.defaultDbSettings `B.withDbModification` B.dbModification {blame = Beam.sharedCabBlameCountTable}

-- | One statement, so concurrent bumps outside any lock never lose an increment: insert the row at
-- count 1, or on the (subject_type, subject_id, merchant_operating_city_id) unique key (ddl 1582)
-- add 1 to the existing row.
bump :: (MonadFlow m, EsqDBFlow m r) => Domain.SharedCabBlameCount -> m ()
bump domainRow = do
  let row = toTType' domainRow
  dbConf <- getMasterBeamConfig
  void . L.runDB dbConf . L.insertRows $
    BF.insert
      (blame blameDB)
      (B.insertValues [row])
      ( BF.onConflict
          (BF.conflictingFields $ \r -> (Beam.subjectType r, Beam.subjectId r, Beam.merchantOperatingCityId r))
          ( BF.onConflictUpdateSet $ \fields _excluded ->
              (Beam.count fields B.<-. B.current_ (Beam.count fields) + 1)
                <> (Beam.lastAt fields B.<-. B.val_ (Beam.lastAt row))
                <> (Beam.lastBookingId fields B.<-. B.val_ (Beam.lastBookingId row))
                <> (Beam.updatedAt fields B.<-. B.val_ (Beam.updatedAt row))
          )
      )
