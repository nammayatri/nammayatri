{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.OndcCancellationReason
  ( parseOndcCancellationReasonId,
    ondcReasonInternalCode,
    resolveCancellationReasonCode,
    ondcUnspecifiedCode,
    ondcUnknownCodePrefix,
  )
where

import qualified BecknV2.OnDemand.Enums as Enums
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Domain.Types.TransporterConfig as DTC
import EulerHS.Prelude
import Kernel.Utils.Common
import qualified SharedLogic.CancellationConfig as SCC

parseOndcCancellationReasonId :: Text -> Maybe Enums.CancellationReasonId
parseOndcCancellationReasonId raw =
  let normalised = T.strip raw
   in find (\reasonId -> T.pack (show reasonId) == normalised) [minBound .. maxBound]

ondcReasonInternalCode :: Enums.CancellationReasonId -> Text
ondcReasonInternalCode = \case
  Enums.TECHNICAL_CANCELLATION -> "ONDC_TECHNICAL_CANCELLATION"
  Enums.DRIVER_NOT_MOVING -> "ONDC_DRIVER_NOT_MOVING"
  Enums.DRIVER_NOT_REACHABLE -> "ONDC_DRIVER_NOT_REACHABLE"
  Enums.DRIVER_ASKED_TO_CANCEL -> "ONDC_DRIVER_ASKED_TO_CANCEL"
  Enums.INCORRECT_PICKUP_LOCATION -> "ONDC_INCORRECT_PICKUP_LOCATION"
  Enums.BOOKED_BY_MISTAKE -> "ONDC_BOOKED_BY_MISTAKE"
  Enums.SAFETY_CONCERN_WITH_DRIVER_OR_RIDE -> "ONDC_SAFETY_CONCERN_WITH_DRIVER_OR_RIDE"
  Enums.VEHICLE_UNSAFE_OR_NON_COMPLIANT -> "ONDC_VEHICLE_UNSAFE_OR_NON_COMPLIANT"

ondcUnknownCodePrefix :: Text
ondcUnknownCodePrefix = "ONDC_UNKNOWN_"

ondcUnspecifiedCode :: Text
ondcUnspecifiedCode = "ONDC_UNSPECIFIED"

resolveCancellationReasonCode ::
  (Monad m, Log m) =>
  Maybe DTC.TransporterConfig ->
  Maybe Text ->
  Maybe Text ->
  m (Maybe Text)
resolveCancellationReasonCode mbTransporterConfig mbOndcReasonId mbShortDesc
  | not preferOndcReasonId = pure mbShortDesc
  | otherwise = case mbOndcReasonId of
    Just rawId -> case parseOndcCancellationReasonId rawId of
      Just reasonId -> pure . Just $ ondcReasonInternalCode reasonId
      Nothing -> do
        logError $ "Unmapped ONDC cancellation_reason_id received: " <> show rawId
        pure . Just $ ondcUnknownCodePrefix <> sanitiseUnknownCode rawId
    Nothing -> case mbShortDesc of
      Just shortDesc -> pure $ Just shortDesc
      Nothing -> do
        logError "Buyer cancellation carried neither cancellation_reason_id nor short_desc"
        pure $ Just ondcUnspecifiedCode
  where
    preferOndcReasonId = SCC.preferOndcCancellationReasonId mbTransporterConfig

sanitiseUnknownCode :: Text -> Text
sanitiseUnknownCode raw =
  let kept = T.filter (\c -> C.isAsciiUpper c || C.isAsciiLower c || C.isDigit c || c == '_') raw
   in if T.null kept then "EMPTY" else T.toUpper (T.take 32 kept)
