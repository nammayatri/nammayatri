{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.PPF
  ( buildMobilityPPFReconEntry,
    isPPFEnabled,
  )
where

import qualified Data.Text as T
import qualified Domain.Types.BecknConfig as DBC
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude
import Kernel.Types.Common
import Lib.Finance.Domain.Types.PPFRecon

isPPFEnabled :: DBC.BecknConfig -> Bool
isPPFEnabled bapConfig = fromMaybe False bapConfig.ppfEnabled

-- | Build a PPFRecon entry for a completed ride (Mobility domain).
-- The settled amount is the totalFare; expected is left empty (BAP side doesn't validate).
buildMobilityPPFReconEntry ::
  (MonadFlow m) =>
  DBC.BecknConfig ->
  DRB.Booking ->
  DRide.Ride ->
  Price ->
  m PPFRecon
buildMobilityPPFReconEntry bapConfig booking ride totalFare = do
  reconId <- generateGUID
  now <- getCurrentTime
  let buyerFinderFee = fromMaybe 0 $ (readMaybe . T.unpack) =<< bapConfig.buyerFinderFee
      bppOrderId = ride.bppRideId.getId
  pure
    PPFRecon
      { id = reconId,
        domain = MOBILITY,
        networkOrderId = bppOrderId,
        currency = show totalFare.currency,
        transactionId = booking.transactionId,
        collectorSubscriberId = bapConfig.subscriberId,
        receiverSubscriberId = booking.providerId,
        orderAmount =
          ReconSettlementAmount
            { settled = totalFare.amount,
              expected = Nothing
            },
        buyerAppCommission =
          ReconSettlementAmount
            { settled = buyerFinderFee,
              expected = Nothing
            },
        networkFee = Nothing,
        gstAmount = Nothing,
        settlementStatus = PENDING,
        utr = Nothing,
        settlementDate = Nothing,
        settlementId = "",
        merchantId = booking.merchantId.getId,
        merchantOperatingCityId = booking.merchantOperatingCityId.getId,
        createdAt = now,
        updatedAt = now
      }
