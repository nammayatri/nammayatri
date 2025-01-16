module Domain.Action.UI.FavouriteDriver where

import qualified API.Types.UI.FavouriteDriver
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

getDriverFavorites ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.Flow [API.Types.UI.FavouriteDriver.FavouriteDriverResp]
  )
getDriverFavorites (mbPersonId, merchantId) = do
  personId <- mbPersonId & fromMaybeM (InternalError "No person found")
  person <- QP.findById personId >>= fromMaybeM (InternalError "No person found") >>= decrypt
  merchant <- CQM.findById merchantId >>= fromMaybeM (InternalError "Merchant not found found")
  case (person.mobileNumber, person.mobileCountryCode) of
    (Just mobileNumber, Just countryCode) ->
      do
        CallBPPInternal.getFavouriteDriverList
          (merchant.driverOfferApiKey)
          (merchant.driverOfferBaseUrl)
          (merchant.driverOfferMerchantId)
          mobileNumber
          countryCode
        <&> map
          ( \r ->
              API.Types.UI.FavouriteDriver.FavouriteDriverResp
                { driverName = r.driverName,
                  id = r.id,
                  favCount = r.favCount,
                  driverRating = r.driverRating,
                  driverPhone = r.driverPhone
                }
          )
    _ -> pure []

postFavoritesRemove ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Text ->
    Environment.Flow APISuccess
  )
postFavoritesRemove (mbPersonId, merchantId) driverId = do
  personId <- mbPersonId & fromMaybeM (InternalError "No person found")
  person <- QP.findById personId >>= fromMaybeM (InternalError "No person found") >>= decrypt
  merchant <- CQM.findById merchantId >>= fromMaybeM (InternalError "Merchant not found found")
  case (person.mobileNumber, person.mobileCountryCode) of
    (Just mobileNumber, Just countryCode) ->
      do
        CallBPPInternal.removeFavouriteDriver merchant.driverOfferApiKey merchant.driverOfferBaseUrl merchant.driverOfferMerchantId mobileNumber countryCode driverId
        >> pure APISuccess.Success
    _ -> pure APISuccess.Success
