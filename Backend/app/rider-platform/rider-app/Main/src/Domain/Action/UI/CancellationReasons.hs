{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.CancellationReasons (getRideBookingCancellationReasons) where

import qualified API.Types.UI.CancellationReasons as API
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Domain.Action.UI.CancelLogic as CancelLogic
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Person as Person
import Environment
import Kernel.Beam.Functions as B
import qualified Kernel.External.Types as Lang
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Translations as CQTranslations
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QRide
import Tools.Error

getRideBookingCancellationReasons ::
  ( Maybe (Id Person.Person),
    Id merchant
  ) ->
  Id SRB.Booking ->
  Maybe Lang.Language ->
  Flow [API.CancellationReasonEntity]
getRideBookingCancellationReasons _authInfo bookingId mbLang = do
  booking <- B.runInReplica $ QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  mbActiveRide <- B.runInReplica $ QRide.findActiveByRBId bookingId
  let merchantOperatingCityId = booking.merchantOperatingCityId
      hasRideAssigned = isJust mbActiveRide
      isAC = fromMaybe False booking.isAirConditioned
      language = fromMaybe Lang.ENGLISH mbLang
  configs <- CancelLogic.computeCancellationReasons (cast merchantOperatingCityId) hasRideAssigned isAC
  forM configs $ \CancelLogic.CancellationReasonConfig {code, iconUrl} -> do
    mbTranslation <- CQTranslations.findByMerchantOpCityIdMessageKeyLanguageWithInMemcache (cast merchantOperatingCityId) code language
    let resolvedText = maybe (mkDefaultText code) (.message) mbTranslation
    pure $ API.CancellationReasonEntity {code = code, iconUrl = iconUrl, text = resolvedText}
  where
    mkDefaultText :: Text -> Text
    mkDefaultText code' =
      let words' = Text.words (Text.replace "_" " " (Text.toLower code'))
          cap w = case Text.uncons w of
            Nothing -> w
            Just (c, rest) -> Text.cons (Char.toUpper c) rest
       in Text.unwords (map cap words')
