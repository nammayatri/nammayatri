{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Checks a driver's live vehicle photo against the RC number they entered, using the Juspay Grid
-- decisions API. The question set is fixed here on purpose: question text must never come from the
-- client, otherwise a caller could steer the model.
module SharedLogic.VehiclePhotoVerification
  ( VehiclePhotoCheck (..),
    verifyVehiclePhoto,
    isVehiclePhotoAcceptable,
  )
where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DOSC
import Kernel.External.Encryption (EncFlow)
import qualified Kernel.External.Grid as Grid
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error (GenericError (..))
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QOMSC

-- | Raw model output. A field is 'Nothing' when Grid returned no valid answer for that question.
data VehiclePhotoCheck = VehiclePhotoCheck
  { -- | One of "auto", "cab", "bike", "other", "no_vehicle".
    vehicleType :: Maybe Text,
    -- | P(yes) in [0, 1].
    plateVisible :: Maybe Double,
    -- | P(yes) in [0, 1].
    plateMatchesRc :: Maybe Double,
    -- | P(yes) in [0, 1].
    isRealPhoto :: Maybe Double,
    -- | Expected score in [0, 3]: 0 unusable .. 3 good.
    photoQuality :: Maybe Double,
    -- | Resolved Grid model, kept for auditing.
    model :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Returns 'Nothing' when Grid is not configured for the city. Throws 'InvalidRequest' for a bad RC
-- number or image, and 'Grid.GridError' when the call fails (see 'Grid.isGridRetryable').
verifyVehiclePhoto ::
  ( HasCallStack,
    MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    EncFlow m r,
    HasRequestId r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  -- | RC number as entered by the driver
  Text ->
  -- | Photo as base64, with or without a data-URI prefix
  Text ->
  m (Maybe VehiclePhotoCheck)
verifyVehiclePhoto merchantOpCityId rawRcNumber imageBase64 = do
  mbCfg <- getGridCfg merchantOpCityId
  forM mbCfg $ \cfg -> do
    rcNumber <- Grid.normalizeIdentifier (6, 12) rawRcNumber & fromMaybeM (InvalidRequest "Invalid RC number")
    image <- either (throwError . InvalidRequest) pure $ Grid.mkGridImageFromBase64 cfg.maxImageBytes imageBase64
    let questions = vehiclePhotoQuestions rcNumber
        req =
          Grid.GridDecisionReq
            { model = cfg.model,
              state = "Live vehicle photo (attached image) uploaded by a driver in a ride-hailing app in India. The driver entered RC number " <> rcNumber <> ".",
              images = [image],
              questions = questions
            }
    resp <- Grid.decide cfg req
    let answers = Grid.decodeAnswers questions resp
    pure
      VehiclePhotoCheck
        { vehicleType = (.choice) <$> Grid.getChoice vehicleTypeKey answers,
          plateVisible = (.pYes) <$> Grid.getNoul plateVisibleKey answers,
          plateMatchesRc = (.pYes) <$> Grid.getNoul plateMatchesRcKey answers,
          isRealPhoto = (.pYes) <$> Grid.getNoul isRealPhotoKey answers,
          photoQuality = (.score) <$> Grid.getScore Grid.photoQualityKey answers,
          model = resp.model
        }

-- | Default pass rule: plate readable and matching the RC, a real photo, and at least "ok" quality.
-- A missing answer fails the check. The vehicle type is not checked here because only the caller
-- knows which category the RC is registered for.
isVehiclePhotoAcceptable :: VehiclePhotoCheck -> Bool
isVehiclePhotoAcceptable c =
  atLeast 0.9 c.plateVisible
    && atLeast 0.9 c.plateMatchesRc
    && atLeast 0.9 c.isRealPhoto
    && atLeast 1.5 c.photoQuality
    && c.vehicleType /= Just "no_vehicle"
  where
    atLeast threshold = maybe False (>= threshold)

getGridCfg ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  m (Maybe Grid.GridCfg)
getGridCfg merchantOpCityId = do
  mbServiceConfig <- QOMSC.findByServiceAndCity (DOSC.GridService DOSC.JuspayGrid) merchantOpCityId
  pure $ case (.serviceConfig) <$> mbServiceConfig of
    Just (DOSC.GridServiceConfig cfg) -> Just cfg
    _ -> Nothing

vehicleTypeKey, plateVisibleKey, plateMatchesRcKey, isRealPhotoKey :: Grid.QuestionKey
vehicleTypeKey = "vehicle_type"
plateVisibleKey = "plate_visible"
plateMatchesRcKey = "plate_matches_rc"
isRealPhotoKey = "is_real_photo"

-- | @rcNumber@ must already be normalized: it is spliced into the prompt.
vehiclePhotoQuestions :: Text -> Grid.GridQuestions
vehiclePhotoQuestions rcNumber =
  Grid.mkGridQuestions
    [ ( vehicleTypeKey,
        Grid.ChoiceQ
          "What kind of vehicle is the main subject of the photo?"
          ( ("auto", "Three-wheeler auto rickshaw")
              :| [ ("cab", "Four-wheeler car or taxi"),
                   ("bike", "Two-wheeler"),
                   ("other", "Some other vehicle"),
                   ("no_vehicle", "No vehicle in the photo")
                 ]
          )
      ),
      (plateVisibleKey, Grid.NoulQ "Is the vehicle number plate clearly visible and readable?"),
      (plateMatchesRcKey, Grid.NoulQ $ "Does the number plate in the photo read exactly " <> rcNumber <> " (ignore spaces and line breaks)?"),
      (isRealPhotoKey, Grid.NoulQ "Is this a real photo of a physical vehicle, not a photo of a screen, a printout or another photo?"),
      ( Grid.photoQualityKey,
        Grid.ScoreQ
          "How usable is this photo for verifying the vehicle?"
          ( "unusable: blurry, dark or plate cut off"
              :| ["poor", "ok", "good: sharp, well lit, full vehicle and plate visible"]
          )
      )
    ]
