module Domain.Action.Internal.ProdLoopStatus where

-- import Domain.Types.External.LiveEKD

import EulerHS.Language ()
import Kernel.Beam.Functions (runInReplica)
import Kernel.External.Types (Language (ENGLISH))
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, fromMaybeM)
import Kernel.Utils.Error.Throwing hiding (fromMaybeM)
import qualified Storage.Queries.CallStatus as CallStatus
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QR
import qualified Tools.Notifications as TN

data ProdLoopStatusReq = ProdLoopStatusReq
  { callId :: Text,
    userType :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

prodLoopStatus :: (CacheFlow m r, EsqDBFlow m r) => ProdLoopStatusReq -> m APISuccess
prodLoopStatus (ProdLoopStatusReq {..}) = do
  callStatus <- CallStatus.findByCallId callId >>= fromMaybeM (CallStatusDoesNotExist) -- @TODO: Change to CallIDDoesNotExist
  case callStatus.merchantOperatingCityId of
    Just operatingCityId -> case callStatus.entityId of
      Just rideId -> do
        ride <- runInReplica $ QR.findById (Id rideId) >>= fromMaybeM (RideDoesNotExist rideId) -- @TODO: Change to RideIDDoesNotExist
        let personId = ride.driverId.getId
        person <- runInReplica $ QP.findById (Id personId) >>= fromMaybeM (PersonDoesNotExist personId) -- @TODO: Change to PersonIDDoesNotExist
        let deviceToken = person.deviceToken
        let language = fromMaybe ENGLISH person.language
        void $ TN.sendDriverEKDLiveFCM operatingCityId (Id personId) deviceToken language
        pure Success
      Nothing -> throwError (InvalidRequest "Ride does not exist")
    Nothing -> throwError (InvalidRequest "Merchant operating city does not exist")
