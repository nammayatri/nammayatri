{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Coins.CoinHistory where

import Data.Time (UTCTime (UTCTime, utctDay), addDays)
import Domain.Types.Coins.CoinHistory
import qualified Domain.Types.Person as SP
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Coins.CoinHistory as BeamDC

updateCoinEvent :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => CoinHistory -> m ()
updateCoinEvent = createWithKV

getCoinEventSummary :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id SP.Person -> UTCTime -> NominalDiffTime -> m [CoinHistory]
getCoinEventSummary (Id driverId) time timeDiffFromUtc = do
  let todayStart = UTCTime (utctDay time) 0
      tomorrowStart = addUTCTime (24 * 60 * 60) todayStart
      istOffset = - timeDiffFromUtc
      todayStart' = addUTCTime istOffset todayStart
      tomorrowStart' = addUTCTime istOffset tomorrowStart
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamDC.createdAt $ Se.GreaterThanOrEq todayStart',
          Se.Is BeamDC.createdAt $ Se.LessThan tomorrowStart',
          Se.Is BeamDC.driverId $ Se.Eq driverId
        ]
    ]
    (Se.Desc BeamDC.createdAt)
    Nothing
    Nothing

getTotalCoins :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id SP.Person -> NominalDiffTime -> m [CoinHistory]
getTotalCoins (Id driverId) timeDiffFromUtc = do
  istTime <- addUTCTime timeDiffFromUtc <$> getCurrentTime
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDC.driverId $ Se.Eq driverId,
          Se.Or
            [ Se.Is BeamDC.expirationAt $ Se.GreaterThanOrEq (Just istTime),
              Se.Is BeamDC.expirationAt $ Se.Eq Nothing
            ]
        ]
    ]

getExpiringCoinsInXDay :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id SP.Person -> NominalDiffTime -> NominalDiffTime -> m [CoinHistory]
getExpiringCoinsInXDay (Id driverId) configTime timeDiffFromUtc = do
  istTime <- addUTCTime timeDiffFromUtc <$> getCurrentTime
  let futureTime = addUTCTime configTime istTime
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDC.driverId $ Se.Eq driverId,
          Se.Is BeamDC.status $ Se.Eq Remaining,
          Se.Is BeamDC.expirationAt $ Se.GreaterThanOrEq (Just istTime),
          Se.Is BeamDC.expirationAt $ Se.LessThanOrEq (Just futureTime)
        ]
    ]

getCoinsEarnedLastDay :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id SP.Person -> UTCTime -> NominalDiffTime -> m [CoinHistory]
getCoinsEarnedLastDay (Id driverId) now timeDiffFromUtc = do
  let todayStart = UTCTime (utctDay now) 0
      yesterdayStart = UTCTime (addDays (-1) (utctDay now)) 0
      todayStart' = addUTCTime (- timeDiffFromUtc) todayStart
      yesterdayStart' = addUTCTime (- timeDiffFromUtc) yesterdayStart
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDC.driverId $ Se.Eq driverId,
          Se.Is BeamDC.createdAt $ Se.GreaterThanOrEq yesterdayStart',
          Se.Is BeamDC.createdAt $ Se.LessThanOrEq todayStart'
        ]
    ]

getDriverCoinInfo :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id SP.Person -> NominalDiffTime -> m [CoinHistory]
getDriverCoinInfo (Id driverId) timeDiffFromUtc = do
  istTime <- addUTCTime timeDiffFromUtc <$> getCurrentTime
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamDC.driverId $ Se.Eq driverId,
          Se.Is BeamDC.status $ Se.Eq Remaining,
          Se.Is BeamDC.expirationAt $ Se.GreaterThanOrEq (Just istTime)
        ]
    ]
    (Se.Asc BeamDC.createdAt)
    Nothing
    Nothing

updateStatusOfCoins :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> Int -> CoinStatus -> m ()
updateStatusOfCoins id coinsRemainingValue newStatus = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamDC.status newStatus,
      Se.Set BeamDC.coinsUsed coinsRemainingValue,
      Se.Set BeamDC.updatedAt now
    ]
    [Se.Is BeamDC.id $ Se.Eq id]

totalCoinEarnHistory :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id SP.Person -> Maybe Integer -> Maybe Integer -> m [CoinHistory]
totalCoinEarnHistory (Id driverId) mbLimit mbOffset = do
  let limitVal = maybe 10 fromInteger mbLimit
      offsetVal = maybe 0 fromInteger mbOffset
  findAllWithOptionsKV
    [Se.Is BeamDC.driverId $ Se.Eq driverId]
    (Se.Desc BeamDC.createdAt)
    (Just limitVal)
    (Just offsetVal)

instance FromTType' BeamDC.CoinHistory CoinHistory where
  fromTType' BeamDC.CoinHistoryT {..} = do
    pure $
      Just
        CoinHistory
          { id = Id id,
            driverId = driverId,
            eventFunction = eventFunction,
            merchantId = merchantId,
            merchantOptCityId = merchantOptCityId,
            coins = coins,
            createdAt = createdAt,
            updatedAt = updatedAt,
            expirationAt = expirationAt,
            status = status,
            coinsUsed = coinsUsed,
            bulkUploadTitle = bulkUploadTitle
          }

instance ToTType' BeamDC.CoinHistory CoinHistory where
  toTType' CoinHistory {..} = do
    BeamDC.CoinHistoryT
      { BeamDC.id = getId id,
        BeamDC.driverId = driverId,
        BeamDC.createdAt = createdAt,
        BeamDC.updatedAt = updatedAt,
        BeamDC.eventFunction = eventFunction,
        BeamDC.merchantId = merchantId,
        BeamDC.merchantOptCityId = merchantOptCityId,
        BeamDC.coins = coins,
        BeamDC.expirationAt = expirationAt,
        BeamDC.status = status,
        BeamDC.coinsUsed = coinsUsed,
        BeamDC.bulkUploadTitle = bulkUploadTitle
      }
