module Safety.API.UI.Sos
  ( SosAPI,
    SOSVideoUploadReq (..),
    AddSosVideoRes (..),
  )
where

import qualified AWS.S3 as S3
import Data.Text (Text)
import qualified Data.Text
import EulerHS.Prelude hiding (id)
import Kernel.Prelude (ToSchema)
import Kernel.ServantMultipart
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import qualified Safety.API.Types.UI.Sos as API
import qualified Safety.Domain.Types.Common
import qualified Safety.Domain.Types.Sos
import Servant

-- | Upload request type (shared across rider and driver).
data SOSVideoUploadReq = SOSVideoUploadReq
  { payload :: FilePath,
    fileType :: S3.FileType,
    fileExtension :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance FromMultipart Tmp SOSVideoUploadReq where
  fromMultipart form = do
    fileData <- lookupFile "payload" form
    let mimeType = fdFileCType fileData
    let file = fdPayload fileData
    let fileExtension = Data.Text.takeWhileEnd (/= '/') mimeType
    fileType <- validateContentType mimeType
    return $ SOSVideoUploadReq file fileType fileExtension
    where
      validateContentType = \case
        "video/mp4" -> Right S3.Video
        "audio/wave" -> Right S3.Audio
        "audio/mpeg" -> Right S3.Audio
        "audio/mp4" -> Right S3.Audio
        _ -> Left "Unsupported file format"

instance ToMultipart Tmp SOSVideoUploadReq where
  toMultipart sosVideoUploadReq =
    MultipartData
      []
      [FileData (show sosVideoUploadReq.fileType) "" (show sosVideoUploadReq.fileType) (sosVideoUploadReq.payload)]

newtype AddSosVideoRes = AddSosVideoRes
  { fileUrl :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

-- | Shared route type for both rider and driver SOS endpoints.
-- Each app instantiates with its own TokenAuth:
--   type API = SosAPI TokenAuth
-- The /sos prefix and /v2 or /ui prefix are added by the app's top-level API type.
type SosAPI authToken =
  "sos"
    :> ( authToken
           :> "getDetails"
           :> Capture "rideId" (Kernel.Types.Id.Id Safety.Domain.Types.Common.Ride)
           :> Get '[JSON] API.SosDetailsRes
           :<|> "IvrOutcome"
             :> QueryParam "CallFrom" Data.Text.Text
             :> QueryParam "CallSid" Data.Text.Text
             :> QueryParam "CallStatus" Data.Text.Text
             :> QueryParam "digits" Data.Text.Text
             :> Get '[JSON] Kernel.Types.APISuccess.APISuccess
           :<|> authToken
             :> "create"
             :> ReqBody '[JSON] API.SosReq
             :> Post '[JSON] API.SosRes
           :<|> authToken
             :> Capture "sosId" (Kernel.Types.Id.Id Safety.Domain.Types.Sos.Sos)
             :> "status"
             :> ReqBody '[JSON] API.SosUpdateReq
             :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
           :<|> authToken
             :> "markRideAsSafe"
             :> Capture "sosId" (Kernel.Types.Id.Id Safety.Domain.Types.Sos.Sos)
             :> ReqBody '[JSON] API.MarkAsSafeReq
             :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
           :<|> authToken
             :> "createMockSos"
             :> ReqBody '[JSON] API.MockSosReq
             :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
           :<|> authToken
             :> "callPolice"
             :> ReqBody '[JSON] API.CallPoliceAPI
             :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
           :<|> authToken
             :> Capture "sosId" (Kernel.Types.Id.Id Safety.Domain.Types.Sos.Sos)
             :> "updateLocation"
             :> ReqBody '[JSON] API.SosLocationUpdateReq
             :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
           :<|> Capture "sosId" (Kernel.Types.Id.Id Safety.Domain.Types.Sos.Sos)
             :> "tracking"
             :> Get '[JSON] API.SosTrackingRes
           :<|> authToken
             :> "startTracking"
             :> ReqBody '[JSON] API.StartTrackingReq
             :> Post '[JSON] API.StartTrackingRes
           :<|> authToken
             :> "updateState"
             :> Capture "sosId" (Kernel.Types.Id.Id Safety.Domain.Types.Sos.Sos)
             :> ReqBody '[JSON] API.UpdateStateReq
             :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
           :<|> authToken
             :> "trackingDetails"
             :> Capture "sosId" (Kernel.Types.Id.Id Safety.Domain.Types.Sos.Sos)
             :> Get '[JSON] API.SosTrackingDetailsRes
           :<|> authToken
             :> Capture "sosId" (Kernel.Types.Id.Id Safety.Domain.Types.Sos.Sos)
             :> "updateToRide"
             :> ReqBody '[JSON] API.UpdateToRideReq
             :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
           :<|> authToken
             :> "getDetailsByPerson"
             :> Capture "sosStatus" Safety.Domain.Types.Sos.SosStatus
             :> Get '[JSON] API.SosDetailsRes
           :<|> "erss"
             :> "statusUpdate"
             :> ReqBody '[JSON] API.ErssStatusUpdateReq
             :> Post '[JSON] API.ErssStatusUpdateRes
           :<|> "rideDetails"
             :> Capture "rideShortId" (Kernel.Types.Id.ShortId Safety.Domain.Types.Common.Ride)
             :> Get '[JSON] API.RideDetailsForDriverRes
           :<|> authToken
             :> Capture "sosId" (Kernel.Types.Id.Id Safety.Domain.Types.Sos.Sos)
             :> "upload"
             :> MultipartForm Tmp SOSVideoUploadReq
             :> Post '[JSON] AddSosVideoRes
       )
