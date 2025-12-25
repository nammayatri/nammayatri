module Domain.Action.Dashboard.RideBooking.Maps
  ( postMapsAutoComplete,
    postMapsGetPlaceDetails,
    postMapsGetPlaceName,
  )
where

import qualified API.UI.Maps
import qualified "this" Domain.Action.UI.Maps
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import SharedLogic.Merchant (findMerchantByShortId)

postMapsAutoComplete ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Domain.Action.UI.Maps.AutoCompleteReq ->
  Environment.Flow Domain.Action.UI.Maps.AutoCompleteResp
postMapsAutoComplete merchantShortId _opCity personId req = do
  m <- findMerchantByShortId merchantShortId
  API.UI.Maps.autoComplete' (personId, m.id) req

postMapsGetPlaceDetails ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Domain.Action.UI.Maps.GetPlaceDetailsReq ->
  Environment.Flow Domain.Action.UI.Maps.GetPlaceDetailsResp
postMapsGetPlaceDetails merchantShortId _opCity personId req = do
  m <- findMerchantByShortId merchantShortId
  API.UI.Maps.getPlaceDetails' (personId, m.id) req

postMapsGetPlaceName ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Domain.Action.UI.Maps.GetPlaceNameReq ->
  Environment.Flow Domain.Action.UI.Maps.GetPlaceNameResp
postMapsGetPlaceName merchantShortId _opCity personId req = do
  m <- findMerchantByShortId merchantShortId
  API.UI.Maps.getPlaceName' (personId, m.id) req
