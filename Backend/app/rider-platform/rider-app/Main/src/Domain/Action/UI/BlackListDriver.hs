module Domain.Action.UI.BlackListDriver where

import qualified API.Types.UI.BlackListDriver
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.Prelude
import Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Error (GenericError (InternalError))
import qualified Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM)
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Person as QP

postDriverBlackList ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Text ->
    API.Types.UI.BlackListDriver.BlackListDriverReq ->
    Environment.Flow APISuccess
  )
postDriverBlackList (mbPersonId, merchantId) driverId API.Types.UI.BlackListDriver.BlackListDriverReq {..} = do
  personId <- mbPersonId & fromMaybeM (InternalError "No person found")
  person <- QP.findById personId >>= fromMaybeM (InternalError "No person found") >>= decrypt
  merchant <- CQM.findById merchantId >>= fromMaybeM (InternalError "Merchant not found")
  case (person.mobileNumber, person.mobileCountryCode) of
    (Just mobileNumber, Just countryCode) ->
      CallBPPInternal.blackListDriver
        merchant.driverOfferApiKey
        merchant.driverOfferBaseUrl
        merchant.driverOfferMerchantId
        mobileNumber
        countryCode
        driverId
        blackListed
        >> pure APISuccess.Success
    _ -> pure APISuccess.Success
