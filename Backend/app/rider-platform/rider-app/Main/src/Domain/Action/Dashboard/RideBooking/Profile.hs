module Domain.Action.Dashboard.RideBooking.Profile
  ( getProfileDetail,
    postProfileUpdate,
  )
where

import qualified API.UI.Profile
import qualified "this" Domain.Action.UI.Profile
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import SharedLogic.Merchant (findMerchantByShortId)

getProfileDetail ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Environment.Flow Domain.Action.UI.Profile.ProfileRes
getProfileDetail merchantShortId _opCity personId = do
  m <- findMerchantByShortId merchantShortId
  API.UI.Profile.getPersonDetails' (personId, m.id) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing --CACTODO: this is a temporary implementation need to discuss and implement toss part if required or change response type all together for this api to stop sending cfg version forever.

postProfileUpdate ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Domain.Action.UI.Profile.UpdateProfileReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postProfileUpdate merchantShortId _opCity personId req = do
  m <- findMerchantByShortId merchantShortId
  API.UI.Profile.updatePerson' (personId, m.id) req Nothing Nothing Nothing Nothing Nothing
