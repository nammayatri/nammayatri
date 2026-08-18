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

-- | Parse @message.cancellation_reason_id@ into the buyer-side enum.
--
-- Built from @[minBound .. maxBound]@ against each constructor's 'Show' — the wire values
-- live only in that instance, so adding a constructor extends the parser with no second
-- list to keep in step.
parseOndcCancellationReasonId :: Text -> Maybe Enums.CancellationReasonId
parseOndcCancellationReasonId raw =
  let normalised = T.strip raw
   in find (\reasonId -> T.pack (show reasonId) == normalised) [minBound .. maxBound]

-- | The internal reason code a mapped ONDC id resolves to.
--
-- Total by construction: with @-Werror@ a new 'Enums.CancellationReasonId' constructor becomes
-- a build failure here, which is the reason this is a @case@ rather than a seeded table.
-- Namespaced @ONDC_@ because this value shares a field with the driver vocabulary
-- (@DRIVER_CANCEL_*@) and NY's own reasons, disambiguated downstream only by @cancelledBy@;
-- the prefix makes a collision impossible rather than unlikely.
ondcReasonInternalCode :: Enums.CancellationReasonId -> Text
ondcReasonInternalCode = \case
  Enums.TECHNICAL_CANCELLATION -> "ONDC_TECHNICAL_CANCELLATION"
  Enums.DRIVER_NOT_MOVING -> "ONDC_DRIVER_NOT_MOVING"
  Enums.DRIVER_NOT_REACHABLE -> "ONDC_DRIVER_NOT_REACHABLE"
  Enums.DRIVER_ASKED_TO_CANCEL -> "ONDC_DRIVER_ASKED_TO_CANCEL"
  Enums.INCORRECT_PICKUP_LOCATION -> "ONDC_INCORRECT_PICKUP_LOCATION"
  Enums.BOOKED_BY_MISTAKE -> "ONDC_BOOKED_BY_MISTAKE"

ondcUnknownCodePrefix :: Text
ondcUnknownCodePrefix = "ONDC_UNKNOWN_"

ondcUnspecifiedCode :: Text
ondcUnspecifiedCode = "ONDC_UNSPECIFIED"

-- | Resolve the internal reason code for a buyer-initiated cancellation.
--
-- @
-- flag off  ->  short_desc                                   -- today's behaviour, unchanged
-- flag on   ->  reason_id present, mapped    -> ONDC_\<CODE\>
--               reason_id present, unmapped  -> ONDC_UNKNOWN_\<code\>, alarm
--               reason_id absent             -> short_desc
--               neither                      -> ONDC_UNSPECIFIED, alarm
-- @
--
-- With the flag on the ladder never yields 'Nothing', so the fee rules always have something
-- to branch on. The raw @reason_id@ is preserved on @additionalInfo@ by the caller either way,
-- so an unmapped code loses no information.
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

-- | An unmapped id is third-party input that becomes a rule-engine key, so it is reduced to a
-- bounded alphanumeric token. The unsanitised value survives on @additionalInfo@.
sanitiseUnknownCode :: Text -> Text
sanitiseUnknownCode raw =
  let kept = T.filter (\c -> C.isAsciiUpper c || C.isAsciiLower c || C.isDigit c || c == '_') raw
   in if T.null kept then "EMPTY" else T.toUpper (T.take 32 kept)
