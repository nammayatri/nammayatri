{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.FleetOwnerList
  ( API,
    handler,
  )
where

import qualified API.Types.UI.FleetOwnerList
import qualified Control.Lens
import qualified Domain.Action.UI.FleetOwnerList
import qualified Domain.Types.FleetOwnerInformation
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "fleetOwner" :> "list" :> QueryParam "blocked" Kernel.Prelude.Bool :> QueryParam "fleetType" Domain.Types.FleetOwnerInformation.FleetType
      :> QueryParam
           "fromDate"
           Kernel.Prelude.UTCTime
      :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "mbSearchString"
           Kernel.Prelude.Text
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "onlyEnabled"
           Kernel.Prelude.Bool
      :> QueryParam
           "toDate"
           Kernel.Prelude.UTCTime
      :> Get
           '[JSON]
           [API.Types.UI.FleetOwnerList.FleetOwnerListItem]
  )

handler :: Environment.FlowServer API
handler = getFleetOwnerList

getFleetOwnerList ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    Kernel.Prelude.Maybe Domain.Types.FleetOwnerInformation.FleetType ->
    Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Text ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
    Environment.FlowHandler [API.Types.UI.FleetOwnerList.FleetOwnerListItem]
  )
getFleetOwnerList a9 a8 a7 a6 a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FleetOwnerList.getFleetOwnerList (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a9) a8 a7 a6 a5 a4 a3 a2 a1
