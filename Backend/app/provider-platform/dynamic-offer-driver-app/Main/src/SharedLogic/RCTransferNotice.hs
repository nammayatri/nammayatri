{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | "Your vehicle is now linked to another account" push for a driver who lost an RC to another driver.
--   The overlay copy lives in merchant_overlay (per city and language). It is a one-time push: nothing is
--   stored, so a push that is not delivered is not retried. Only the button label depends on the previous
--   holder's other vehicles: a driver with another valid RC still linked gets the udf1 = HAS_OTHER_VEHICLES
--   row ("Switch vehicle"); otherwise the base row ("Add vehicle").
module SharedLogic.RCTransferNotice
  ( notifyRCTransfer,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Text as T
import qualified Domain.Types.Person as DP
import qualified Domain.Types.VehicleRegistrationCertificate as DVRC
import Kernel.External.Encryption
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.External.Types (Language (..))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.Overlay as CMP
import qualified Storage.Queries.DriverRCAssociation as QDRCA
import qualified Storage.Queries.Person as QPerson
import qualified Tools.Notifications as Notify

overlayKey :: Text
overlayKey = "RC_TRANSFERRED_TO_ANOTHER_ACCOUNT"

hasOtherVehiclesUdf :: Text
hasOtherVehiclesUdf = "HAS_OTHER_VEHICLES"

-- | Push the overlay to the previous holder. Call only after the new driver's activation has succeeded.
--   The new driver's id is only logged: the push never identifies them.
notifyRCTransfer ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r, Redis.HedisLTSFlowEnv r) =>
  Id DP.Person ->
  Id DP.Person ->
  DVRC.VehicleRegistrationCertificate ->
  m ()
notifyRCTransfer previousDriverId newDriverId rc = do
  previousDriver <- QPerson.findById previousDriverId >>= fromMaybeM (PersonNotFound previousDriverId.getId)
  let cityId = previousDriver.merchantOperatingCityId
  rcNo <- decrypt rc.certificateNumber
  -- The transferred RC stays linked to the previous holder (inactive), so it is excluded here.
  otherRCs <- filter (\(_, otherRc) -> otherRc.id /= rc.id && otherRc.verificationStatus == Documents.VALID) <$> QDRCA.findAllActiveAndInactiveAssociationsByDriverId previousDriverId
  logTagInfo "RC_TRANSFER" $
    "rcId: " <> rc.id.getId <> ", rcNumber: " <> maskText rcNo <> ", fromDriverId: " <> previousDriverId.getId
      <> ", toDriverId: "
      <> newDriverId.getId
      <> ", merchantOperatingCityId: "
      <> cityId.getId
      <> ", hasOtherVehicles: "
      <> show (not $ null otherRCs)
  let lookupIn udf1 lang = CMP.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory cityId overlayKey lang udf1 Nothing Nothing
      language = fromMaybe ENGLISH previousDriver.language
      -- The driver's language first, then the ENGLISH row, so a language without a seeded row still gets the push.
      lookupWithFallback udf1 = lookupIn udf1 language >>= maybe (if language == ENGLISH then pure Nothing else lookupIn udf1 ENGLISH) (pure . Just)
  -- A city without the HAS_OTHER_VEHICLES rows still gets the base row (only the button label differs).
  mbOverlay <-
    if null otherRCs
      then lookupWithFallback Nothing
      else lookupWithFallback (Just hasOtherVehiclesUdf) >>= maybe (lookupWithFallback Nothing) (pure . Just)
  case mbOverlay of
    Nothing -> logWarning $ "merchant_overlay " <> overlayKey <> " not configured for city " <> cityId.getId <> ", skipping RC transfer push for driver " <> previousDriverId.getId
    Just overlay -> do
      -- The overlay row's number wins; otherwise the city's support number, so the app's "Need help" shows.
      mbCitySupportNumber <- (>>= (.supportNumber)) <$> CQMOC.findById cityId
      -- The full number: it is the previous holder's own vehicle. Logs use the masked form.
      let fillRcNo = T.replace "{#rcNumber#}" rcNo
          nonEmpty = mfilter (not . T.null . T.strip)
          overlayReq = Notify.mkOverlayReq overlay
          supportNumber = nonEmpty overlayReq.contactSupportNumber <|> nonEmpty mbCitySupportNumber
      Notify.sendOverlay cityId previousDriver $
        overlayReq
          { FCM.title = fillRcNo <$> overlayReq.title,
            FCM.description = fillRcNo <$> overlayReq.description,
            FCM.toastMessage = fillRcNo <$> overlayReq.toastMessage,
            FCM.contactSupportNumber = supportNumber,
            -- The Call Support label only with a number to call: older APKs show any cancel button
            -- (buttonCancelVisibility = isJust cancelButtonText), which would do nothing without one.
            FCM.cancelButtonText = overlayReq.cancelButtonText <* supportNumber
          }
