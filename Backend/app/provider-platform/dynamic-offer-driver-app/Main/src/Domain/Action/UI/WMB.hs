{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.WMB
  ( getUiWmbAvailableRoutes,
    postUiWmbTripLink,
    getUiWmbTripActive,
    postUiWmbTripStart,
    postUiWmbTripEnd,
  )
where

import qualified API.Types.UI.WMB
import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

getUiWmbAvailableRoutes ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Data.Text.Text ->
    Environment.Flow [API.Types.UI.WMB.AvailableRoutesList]
  )
getUiWmbAvailableRoutes = do error "Logic yet to be decided"

postUiWmbTripLink ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.WMB.TripLinkReq ->
    Environment.Flow API.Types.UI.WMB.TripLinkResp
  )
postUiWmbTripLink = do error "Logic yet to be decided"

getUiWmbTripActive ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow API.Types.UI.WMB.TripLinkResp
  )
getUiWmbTripActive = do error "Logic yet to be decided"

postUiWmbTripStart ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Data.Text.Text ->
    API.Types.UI.WMB.TripStartReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postUiWmbTripStart = do error "Logic yet to be decided"

postUiWmbTripEnd ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Data.Text.Text ->
    API.Types.UI.WMB.TripEndReq ->
    Environment.Flow API.Types.UI.WMB.TripEndResp
  )
postUiWmbTripEnd = do error "Logic yet to be decided"
