{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Payment.Storage.Queries.PayoutOrderExtra where

import Data.Time (UTCTime (UTCTime, utctDay), secondsToDiffTime)
import Kernel.Beam.Functions
import Kernel.External.Encryption (DbHash)
import qualified Kernel.External.Payout.Juspay.Types.Payout as Payout
import Kernel.Prelude
import Kernel.Types.Beckn.Context (City)
import Kernel.Utils.Common
import Lib.Payment.Domain.Types.Common
import Lib.Payment.Domain.Types.PayoutOrder
import Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Beam.PayoutOrder as Beam
import Lib.Payment.Storage.Queries.OrphanInstances.PayoutOrder ()
import qualified Sequelize as Se

findAllByEntityNameAndEntityIds ::
  (BeamFlow m r) =>
  (Maybe Int -> Maybe Int -> Maybe EntityName -> [Maybe Text] -> m [PayoutOrder])
findAllByEntityNameAndEntityIds limit offset entityName entityIds = do
  let mbIds = Just $ catMaybes entityIds
  findAllWithOptionsKV
    [ Se.And
        [Se.Is Beam.entityName $ Se.Eq entityName, Se.Is Beam.entityIds $ Se.Eq mbIds]
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

updateLastCheckedOn :: BeamFlow m r => [Text] -> m ()
updateLastCheckedOn payoutOrderIds = do
  now <- getCurrentTime
  let lastCheckedAt = UTCTime (utctDay now) (secondsToDiffTime 0)
  updateWithKV
    [ Se.Set Beam.lastStatusCheckedAt (Just lastCheckedAt),
      Se.Set Beam.updatedAt now
    ]
    [Se.Is Beam.orderId (Se.In payoutOrderIds)]

findAllWithOptions :: BeamFlow m r => Int -> Int -> Maybe Text -> Maybe DbHash -> Maybe UTCTime -> Maybe UTCTime -> Bool -> City -> m [PayoutOrder]
findAllWithOptions limit offset mbDriverId mbMobileNumberHash mbFrom mbTo isFailedOnly city = do
  findAllWithOptionsKV
    [ Se.And
        ( foldMap (\v -> [Se.Is Beam.createdAt $ Se.GreaterThanOrEq v]) mbFrom
            <> foldMap (\v -> [Se.Is Beam.createdAt $ Se.LessThanOrEq v]) mbTo
            <> foldMap (\v -> [Se.Is Beam.mobileNoHash $ Se.Eq v]) mbMobileNumberHash
            <> foldMap (\v -> [Se.Is Beam.customerId $ Se.Eq v]) mbDriverId
            <> [Se.Is Beam.status $ Se.Eq Payout.FULFILLMENTS_FAILURE | isFailedOnly]
            <> [Se.Is Beam.city $ Se.Eq (show city)]
        )
    ]
    (Se.Desc Beam.createdAt)
    (Just limit)
    (Just offset)

findAllWithStatusAndEntity :: BeamFlow m r => Int -> Int -> Payout.PayoutOrderStatus -> [Maybe EntityName] -> m [PayoutOrder]
findAllWithStatusAndEntity limit offset status entityNames = do
  findAllWithOptionsKV
    [ Se.And
        ( [Se.Is Beam.entityName (Se.In entityNames)]
            <> [Se.Is Beam.status $ Se.Eq status]
        )
    ]
    (Se.Desc Beam.createdAt)
    (Just limit)
    (Just offset)

findLatestPaidPayoutByCustomerId :: BeamFlow m r => Text -> m (Maybe PayoutOrder)
findLatestPaidPayoutByCustomerId customerId = do
  findAllWithOptionsKV
    [ Se.And
        ( [Se.Is Beam.customerId $ Se.Eq customerId]
            <> [Se.Is Beam.status $ Se.Eq Payout.FULFILLMENTS_SUCCESSFUL]
        )
    ]
    (Se.Desc Beam.createdAt)
    (Just 1)
    Nothing
    <&> listToMaybe
