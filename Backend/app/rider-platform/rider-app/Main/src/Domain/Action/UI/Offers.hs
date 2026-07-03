module Domain.Action.UI.Offers (getOffersList) where

import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM)
import qualified SharedLogic.Offer as SOffer
import qualified Storage.Queries.Person as QPerson
import Tools.Error

getOffersList ::
  ( Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Maybe Kernel.Types.Common.HighPrecMoney ->
  Environment.Flow [SOffer.OfferRespAPIEntity]
getOffersList (mbPersonId, merchantId) mbAmount = do
  personId <- mbPersonId & fromMaybeM (PersonDoesNotExist "personId missing from token")
  person <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  SOffer.listOffersForPerson merchantId person mbAmount Nothing
