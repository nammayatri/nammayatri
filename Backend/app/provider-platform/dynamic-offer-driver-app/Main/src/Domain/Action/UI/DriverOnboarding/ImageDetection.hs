{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.DriverOnboarding.ImageDetection where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import Kernel.External.Types (ServiceFlow)
import qualified Kernel.External.Verification.Interface.Types as VIT
import Kernel.Prelude
import Kernel.Types.Id
import qualified Tools.Verification as Verification

data DetectImageType = Face | Vehicle
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data DetectImageReq = DetectImageReq
  { image :: Text,
    imageType :: DetectImageType
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data DetectImageStatus = FaceDetected | NoFaces
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data DetectImageRecommendation = FullFace | PartialFace | RejectedFace
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data DetectImageResp = DetectImageResp
  { status :: DetectImageStatus,
    fullFaces :: Maybe Int,
    partialFaces :: Maybe Int,
    rejectedFaces :: Maybe Int,
    total :: Maybe Int,
    recommendation :: Maybe DetectImageRecommendation,
    message :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

detectImage ::
  ServiceFlow m r =>
  (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  DetectImageReq ->
  m DetectImageResp
detectImage (personId, merchantId, merchantOpCityId) req = do
  let ocrReq =
        VIT.OCRRequest
          { image = req.image,
            imageType = toImgType req.imageType,
            driverId = personId.getId,
            prompt = Nothing
          }
  summary <- Verification.detectImage merchantId merchantOpCityId ocrReq
  pure $ toResp summary
  where
    toImgType Face = VIT.Face
    toImgType Vehicle = VIT.VehicleRegistrationCertificate

    toResp VIT.FaceDetectionSummary {..} =
      DetectImageResp
        { status = toStatus status,
          fullFaces = fullFaces,
          partialFaces = partialFaces,
          rejectedFaces = rejectedFaces,
          total = total,
          recommendation = toRec <$> recommendation,
          message = message
        }

    toStatus VIT.FaceDetected = FaceDetected
    toStatus VIT.NoFaces = NoFaces

    toRec VIT.FullFace = FullFace
    toRec VIT.PartialFace = PartialFace
    toRec VIT.RejectedFace = RejectedFace
