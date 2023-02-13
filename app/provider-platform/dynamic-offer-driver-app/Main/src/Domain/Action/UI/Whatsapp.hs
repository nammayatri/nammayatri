module Domain.Action.UI.Whatsapp (module Domain.Action.UI.Whatsapp, module Reexport) where

import qualified Domain.Types.Person as DP
import Kernel.External.Encryption (decrypt)
import Kernel.External.Whatsapp.Interface.Types as Reexport
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as DB
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.Queries.Person as QP
import Tools.Metrics (CoreMetrics)
import Tools.Whatsapp as Whatsapp

newtype OptAPIRequest = OptAPIRequest
  { status :: OptApiMethods
  }
  deriving (Show, Eq, Generic, ToSchema, FromJSON, ToJSON)

whatsAppOptAPI :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r, CoreMetrics m) => Id DP.Person -> OptAPIRequest -> m APISuccess
whatsAppOptAPI personId OptAPIRequest {..} = do
  DP.Person {..} <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  mobileNo <- mapM decrypt mobileNumber >>= fromMaybeM (InvalidRequest "Person is not linked with any mobile number")
  unless (whatsappNotificationEnrollStatus == Just status) $
    void $ Whatsapp.whatsAppOptAPI merchantId $ OptApiReq {phoneNumber = mobileNo, method = status}
  DB.runTransaction $
    void $ QP.updateWhatsappNotificationEnrollStatus personId $ Just status
  return Success
