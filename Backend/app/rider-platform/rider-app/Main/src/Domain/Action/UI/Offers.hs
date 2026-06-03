module Domain.Action.UI.Offers (getOffersList) where

import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, mkPrice)
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
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
  let price = mkPrice (Just Kernel.Types.Common.INR) (fromMaybe 1 mbAmount)
  productOffers <-
    SOffer.offerListWithBasket merchantId personId person.merchantOperatingCityId DOrder.RideHailing [("offers-list", price)] Nothing Nothing Nothing
  let offerResps = concatMap (\(_, resp) -> resp.offerResp) productOffers
  mapM (SOffer.mkOfferRespAPIEntity Nothing) offerResps
