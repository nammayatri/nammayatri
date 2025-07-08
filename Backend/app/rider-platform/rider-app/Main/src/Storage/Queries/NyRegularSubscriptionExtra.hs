{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.NyRegularSubscriptionExtra where

-- import qualified Data.Text as T
-- import Database.Beam (desc_) -- For orderBy_

-- Domain type

-- import qualified Domain.Types.NyRegularSubscription as NyRegularSubscriptionDomain -- For toDomain if needed, though KV functions often handle it

-- import qualified Environment

import qualified Data.Time as Time
import qualified Domain.Types.MerchantOperatingCity as MerchantOperatingCity
import Domain.Types.NyRegularSubscription
  ( NyRegularSubscription,
    NyRegularSubscriptionStatus,
  )
import qualified Domain.Types.NyRegularSubscription as NySub
import Domain.Types.Person (Person)
import Environment (Flow)
import EulerHS.Prelude (whenNothingM_, (<|>))
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import Storage.Beam.NyRegularSubscription as Beam
import qualified Storage.Beam.NyRegularSubscription as BeamNyReg
import qualified Storage.Queries.Location as QL
import Storage.Queries.OrphanInstances.NyRegularSubscription ()

listSubscriptionsByFilters ::
  Id.Id Person ->
  Maybe NySub.NyRegularSubscriptionStatus ->
  Maybe Int -> -- Limit
  Maybe Integer -> -- Offset
  Flow [NySub.NyRegularSubscription]
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

findAllActiveAt ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id.Id MerchantOperatingCity.MerchantOperatingCity ->
  Time.Day ->
  Maybe Int ->
  m [NySub.NyRegularSubscription]
findAllActiveAt cityId today limit =
  findAllWithOptionsKV'
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Just cityId.getId),
          Se.Is Beam.status $ Se.Eq NySub.ACTIVE,
          Se.Or -- lastProcessedAt is null OR lastProcessedAt is before the start of today
            [ Se.Is Beam.lastProcessedAt $ Se.Eq Nothing,
              Se.Is Beam.lastProcessedAt $ Se.LessThan (Just $ Time.UTCTime today 0)
            ],
          Se.Or -- recurrenceEndDate is null OR recurrenceEndDate is after or on today
            [ Se.Is Beam.recurrenceEndDate $ Se.Eq Nothing,
              Se.Is Beam.recurrenceEndDate $ Se.GreaterThanOrEq (Just today)
            ]
        ]
    ]
    limit
    Nothing

createWithLocation :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.NyRegularSubscription.NyRegularSubscription -> m ())
createWithLocation subscription = do
  whenNothingM_ (QL.findById subscription.pickupLocation.id) $ QL.create subscription.pickupLocation
  whenNothingM_ (QL.findById subscription.dropoffLocation.id) $ QL.create subscription.dropoffLocation
  createWithKV subscription
