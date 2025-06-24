module Storage.Queries.NyRegularSubscriptionExtra where

-- import qualified Data.Text as T
-- import Database.Beam (desc_) -- For orderBy_
import Domain.Types.NyRegularSubscription
  ( NyRegularSubscription, -- Domain type
    NyRegularSubscriptionStatus,
  )
-- import qualified Domain.Types.NyRegularSubscription as NyRegularSubscriptionDomain -- For toDomain if needed, though KV functions often handle it
import Domain.Types.Person (Person)
import Environment (Flow)
-- import qualified Environment

import EulerHS.Prelude ((<|>))
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Id as Id
import qualified Sequelize as Se
import qualified Storage.Beam.NyRegularSubscription as BeamNyReg
import Storage.Queries.OrphanInstances.NyRegularSubscription ()

listSubscriptionsByFilters ::
  Id.Id Person ->
  Maybe NyRegularSubscriptionStatus ->
  Maybe Int -> -- Limit
  Maybe Integer -> -- Offset
  Flow [NyRegularSubscription]
listSubscriptionsByFilters personId mStatus mLimit mOffset = do
  let limit' = mLimit <|> Just 100
      offset' = fmap fromIntegral $ mOffset <|> Just 0
  -- findAllWithOptionsKV [Se.Is BeamB.riderId $ Se.Eq personId] (Se.Desc BeamB.createdAt) limit' offset'
  let conditions =
        [ Se.Is BeamNyReg.userId $ Se.Eq (Id.getId personId)
        ]
          <> case mStatus of
            Just statusVal -> [Se.Is BeamNyReg.status $ Se.Eq statusVal]
            Nothing -> []

      orderByClause = Se.Desc BeamNyReg.createdAt

  -- limitAsInteger = fmap fromIntegral mLimit

  -- findAllWithOptionsKV handles Maybe for limit/offset and uses FromTType' for conversion
  findAllWithOptionsKV conditions orderByClause limit' offset'
