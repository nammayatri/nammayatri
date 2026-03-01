{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module BecknV2.Utils where

import BecknV2.OnDemand.Tags
import qualified BecknV2.OnDemand.Types as Spec
import qualified Data.Aeson as A
import qualified Data.Text as T
import Data.Time
import Data.Time.Format.ISO8601
import EulerHS.Prelude
import Text.Regex.Posix ((=~))

getTagV2 :: BecknTagGroup -> BecknTag -> Maybe [Spec.TagGroup] -> Maybe Text
getTagV2 tagGroupCode tagCode mbTagGroups = do
  case mbTagGroups of
    Nothing -> Nothing
    Just tagGroups -> do
      tagGroup <- find (\tagGroup -> descriptorCode tagGroup.tagGroupDescriptor == Just (show tagGroupCode)) tagGroups
      case tagGroup.tagGroupList of
        Nothing -> Nothing
        Just tagGroupList -> do
          tag <- find (\tag -> descriptorCode tag.tagDescriptor == Just (show tagCode)) tagGroupList
          tag.tagValue
  where
    descriptorCode :: Maybe Spec.Descriptor -> Maybe Text
    descriptorCode (Just desc) = desc.descriptorCode
    descriptorCode Nothing = Nothing

-- | Version-aware tag parsing with fallback for v2.0.0 -> v2.1.0 transition.
-- Tries the primary tag group first, then falls back to legacy tag group names.
getTagV2Compat :: BecknTagGroup -> BecknTag -> Maybe [Spec.TagGroup] -> Maybe Text
getTagV2Compat tagGroupCode tagCode mbTagGroups =
  getTagV2 tagGroupCode tagCode mbTagGroups
    <|> case tagGroupCode of
      BAP_TERMS -> getTagV2 BUYER_FINDER_FEES tagCode mbTagGroups
      BPP_TERMS -> getTagV2 SETTLEMENT_TERMS tagCode mbTagGroups
      _ -> Nothing

parseISO8601Duration :: Text -> Maybe NominalDiffTime
parseISO8601Duration durationStr = do
  (calenderDiffernceTime :: CalendarDiffTime) <- iso8601ParseM $ T.unpack durationStr
  Just $ ctTime calenderDiffernceTime

formatTimeDifference :: NominalDiffTime -> Text
formatTimeDifference duration = T.pack $ iso8601Show $ calendarTimeTime duration

addDurationToUTCTime :: UTCTime -> NominalDiffTime -> UTCTime
addDurationToUTCTime time duration = addUTCTime duration time

-- | Build a Descriptor for stop instructions (ONDC v2.1.0)
mkStopInstructions :: Maybe Text -> Maybe Spec.Descriptor
mkStopInstructions mInstructions =
  mInstructions <&> \inst ->
    Spec.Descriptor
      { descriptorCode = Nothing,
        descriptorName = Just inst,
        descriptorShortDesc = Nothing
      }

-- | Compute an ISO 8601 duration string for the scheduled pickup window.
--   Returns Nothing for immediate rides.
mkScheduledPickupDuration :: Bool -> Maybe Text
mkScheduledPickupDuration isScheduled =
  if isScheduled
    then Just (formatTimeDifference 600) -- 10-minute pickup window
    else Nothing

maskNumber :: Text -> Text
maskNumber billingNumber = do
  let startingDigitLen = 2
  let trailingDigitLen = 2
  let totalDigitLen = startingDigitLen + trailingDigitLen
  if T.length billingNumber <= totalDigitLen
    then billingNumber
    else
      T.take startingDigitLen billingNumber
        <> T.replicate (T.length billingNumber - totalDigitLen) "*"
        <> T.drop (T.length billingNumber - trailingDigitLen) billingNumber

maskSensitiveData :: A.Value -> A.Value
maskSensitiveData (A.String t) = A.String $ maskSensitiveText t
maskSensitiveData (A.Array arr) = A.Array $ fmap maskSensitiveData arr
maskSensitiveData (A.Object obj) = A.Object $ fmap maskSensitiveData obj
maskSensitiveData v = v

maskSensitiveText :: Text -> Text
maskSensitiveText text = do
  let str = T.unpack text
      regexPattern = "^[0-9]{10}$" :: String
      hit = str =~ regexPattern :: (String, String, String)
   in case hit of
        (before, match, after) ->
          if null match
            then text
            else T.concat [T.pack before, maskNumber (T.pack match), T.pack after]
