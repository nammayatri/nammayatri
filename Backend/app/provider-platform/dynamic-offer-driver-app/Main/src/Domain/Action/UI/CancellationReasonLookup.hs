module Domain.Action.UI.CancellationReasonLookup (getRideGetCancellationReasons) where

import qualified API.Types.UI.CancellationReasonLookup
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MessageDictionary as DMD
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Types as Language
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.MessageDictionary as QMD
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.TranslationsExtra as QTranslation
import Tools.Error

getRideGetCancellationReasons ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
    Environment.Flow [API.Types.UI.CancellationReasonLookup.CancellationReasonResp]
  )
getRideGetCancellationReasons (mbDriverId, _merchantId, _merchantOpCityId) rideId = do
  ride <- QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  let merchantOpCityId = ride.merchantOperatingCityId
      merchantId = ride.merchantId

  -- Get driver's preferred language (default to ENGLISH)
  driverLanguage <- case mbDriverId of
    Just driverId -> do
      person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
      pure $ fromMaybe Language.ENGLISH person.language
    Nothing -> pure Language.ENGLISH

  -- Query all cancellation reason message keys for this merchant/city
  messageEntries <-
    QMD.findAllByMessageTypeAndMerchantOpCityIdAndMerchantId
      DMD.CancellationReason
      merchantOpCityId
      (fromMaybe _merchantId merchantId)

  -- For each message key, look up the translation in driver's language (falls back to ENGLISH)
  forM messageEntries $ \entry -> do
    mbTranslation <- QTranslation.findByErrorAndLanguage entry.messageKey driverLanguage
    let reasonMessage = maybe entry.messageKey (.message) mbTranslation
    pure $
      API.Types.UI.CancellationReasonLookup.CancellationReasonResp
        { reasonCode = entry.messageKey,
          reasonMessage = reasonMessage
        }
