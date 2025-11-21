{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.DriverOnboardingV2
  ( API,
    handler,
  )
where

import qualified API.Types.UI.DriverOnboardingV2
import qualified Control.Lens
import qualified Domain.Action.UI.DriverOnboardingV2
import qualified Domain.Types.Common
import qualified Domain.Types.Image
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "onboarding" :> "configs" :> QueryParam "makeSelfieAadhaarPanMandatory" Kernel.Prelude.Bool :> QueryParam "onlyVehicle" Kernel.Prelude.Bool
      :> Get
           '[JSON]
           API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigList
      :<|> TokenAuth
      :> "driver"
      :> "rateCard"
      :> QueryParam
           "distance"
           Kernel.Types.Common.Meters
      :> QueryParam
           "durationInMin"
           Kernel.Types.Common.Minutes
      :> QueryParam
           "tripCategory"
           Domain.Types.Common.TripCategory
      :> QueryParam
           "vehicleServiceTier"
           Domain.Types.Common.ServiceTierType
      :> Get
           '[JSON]
           [API.Types.UI.DriverOnboardingV2.RateCardResp]
      :<|> TokenAuth
      :> "driver"
      :> "vehiclePhotos"
      :> MandatoryQueryParam
           "rcNo"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           API.Types.UI.DriverOnboardingV2.VehiclePhotosResp
      :<|> TokenAuth
      :> "driver"
      :> "vehiclePhotosB64"
      :> QueryParam
           "back"
           Kernel.Prelude.Bool
      :> QueryParam
           "backInterior"
           Kernel.Prelude.Bool
      :> QueryParam
           "front"
           Kernel.Prelude.Bool
      :> QueryParam
           "frontInterior"
           Kernel.Prelude.Bool
      :> QueryParam
           "left"
           Kernel.Prelude.Bool
      :> QueryParam
           "odometer"
           Kernel.Prelude.Bool
      :> QueryParam
           "onlyLatest"
           Kernel.Prelude.Bool
      :> QueryParam
           "right"
           Kernel.Prelude.Bool
      :> MandatoryQueryParam
           "rcNo"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           API.Types.UI.DriverOnboardingV2.VehiclePhotosResp
      :<|> TokenAuth
      :> "driver"
      :> "updateAirCondition"
      :> ReqBody
           '[JSON]
           API.Types.UI.DriverOnboardingV2.UpdateAirConditionUpdateRequest
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "driver"
      :> "vehicleServiceTiers"
      :> Get
           '[JSON]
           API.Types.UI.DriverOnboardingV2.DriverVehicleServiceTiers
      :<|> TokenAuth
      :> "driver"
      :> "updateServiceTiers"
      :> ReqBody
           '[JSON]
           API.Types.UI.DriverOnboardingV2.DriverVehicleServiceTiers
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "driver"
      :> "register"
      :> "ssn"
      :> ReqBody
           '[JSON]
           API.Types.UI.DriverOnboardingV2.SSNReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "driver"
      :> "backgroundVerification"
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "driver"
      :> "register"
      :> "pancard"
      :> ReqBody
           '[JSON]
           API.Types.UI.DriverOnboardingV2.DriverPanReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "driver"
      :> "register"
      :> "bankAccount"
      :> "link"
      :> Get
           '[JSON]
           API.Types.UI.DriverOnboardingV2.BankAccountLinkResp
      :<|> TokenAuth
      :> "driver"
      :> "register"
      :> "bankAccount"
      :> "status"
      :> Get
           '[JSON]
           API.Types.UI.DriverOnboardingV2.BankAccountResp
      :<|> TokenAuth
      :> "driver"
      :> "register"
      :> "getLiveSelfie"
      :> MandatoryQueryParam
           "status"
           Domain.Types.Image.SelfieFetchStatus
      :> Get
           '[JSON]
           API.Types.UI.DriverOnboardingV2.GetLiveSelfieResp
      :<|> TokenAuth
      :> "driver"
      :> "register"
      :> "aadhaarCard"
      :> ReqBody
           '[JSON]
           API.Types.UI.DriverOnboardingV2.AadhaarCardReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "driver"
      :> "register"
      :> "logHvSdkCall"
      :> ReqBody
           '[JSON]
           API.Types.UI.DriverOnboardingV2.HVSdkCallLogReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "driver"
      :> "register"
      :> "commonDocument"
      :> ReqBody
           '[JSON]
           API.Types.UI.DriverOnboardingV2.CommonDocumentReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "driver"
      :> "fleet"
      :> "rcs"
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> Get
           '[JSON]
           API.Types.UI.DriverOnboardingV2.FleetRCListRes
      :<|> TokenAuth
      :> "driver"
      :> "digilocker"
      :> "initiate"
      :> ReqBody
           '[JSON]
           API.Types.UI.DriverOnboardingV2.DigiLockerInitiateReq
      :> Post
           '[JSON]
           API.Types.UI.DriverOnboardingV2.DigiLockerInitiateResp
      :<|> TokenAuth
      :> "driver"
      :> "digilocker"
      :> "pullDocuments"
      :> ReqBody
           '[JSON]
           API.Types.UI.DriverOnboardingV2.PullDocumentReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = getOnboardingConfigs :<|> getDriverRateCard :<|> getDriverVehiclePhotos :<|> getDriverVehiclePhotosB64 :<|> postDriverUpdateAirCondition :<|> getDriverVehicleServiceTiers :<|> postDriverUpdateServiceTiers :<|> postDriverRegisterSsn :<|> postDriverBackgroundVerification :<|> postDriverRegisterPancard :<|> getDriverRegisterBankAccountLink :<|> getDriverRegisterBankAccountStatus :<|> getDriverRegisterGetLiveSelfie :<|> postDriverRegisterAadhaarCard :<|> postDriverRegisterLogHvSdkCall :<|> postDriverRegisterCommonDocument :<|> getDriverFleetRcs :<|> postDriverDigilockerInitiate :<|> postDriverDigilockerPullDocuments

getOnboardingConfigs ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    Environment.FlowHandler API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigList
  )
getOnboardingConfigs a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverOnboardingV2.getOnboardingConfigs (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

getDriverRateCard ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Kernel.Types.Common.Meters ->
    Kernel.Prelude.Maybe Kernel.Types.Common.Minutes ->
    Kernel.Prelude.Maybe Domain.Types.Common.TripCategory ->
    Kernel.Prelude.Maybe Domain.Types.Common.ServiceTierType ->
    Environment.FlowHandler [API.Types.UI.DriverOnboardingV2.RateCardResp]
  )
getDriverRateCard a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverOnboardingV2.getDriverRateCard (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a5) a4 a3 a2 a1

getDriverVehiclePhotos ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Text ->
    Environment.FlowHandler API.Types.UI.DriverOnboardingV2.VehiclePhotosResp
  )
getDriverVehiclePhotos a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverOnboardingV2.getDriverVehiclePhotos (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getDriverVehiclePhotosB64 ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    Kernel.Prelude.Text ->
    Environment.FlowHandler API.Types.UI.DriverOnboardingV2.VehiclePhotosResp
  )
getDriverVehiclePhotosB64 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverOnboardingV2.getDriverVehiclePhotosB64 (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a10) a9 a8 a7 a6 a5 a4 a3 a2 a1

postDriverUpdateAirCondition ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverOnboardingV2.UpdateAirConditionUpdateRequest ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postDriverUpdateAirCondition a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverOnboardingV2.postDriverUpdateAirCondition (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getDriverVehicleServiceTiers ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler API.Types.UI.DriverOnboardingV2.DriverVehicleServiceTiers
  )
getDriverVehicleServiceTiers a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverOnboardingV2.getDriverVehicleServiceTiers (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

postDriverUpdateServiceTiers ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverOnboardingV2.DriverVehicleServiceTiers ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postDriverUpdateServiceTiers a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverOnboardingV2.postDriverUpdateServiceTiers (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postDriverRegisterSsn ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverOnboardingV2.SSNReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postDriverRegisterSsn a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverOnboardingV2.postDriverRegisterSsn (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postDriverBackgroundVerification ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postDriverBackgroundVerification a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverOnboardingV2.postDriverBackgroundVerification (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

postDriverRegisterPancard ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverOnboardingV2.DriverPanReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postDriverRegisterPancard a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverOnboardingV2.postDriverRegisterPancard (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getDriverRegisterBankAccountLink ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler API.Types.UI.DriverOnboardingV2.BankAccountLinkResp
  )
getDriverRegisterBankAccountLink a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverOnboardingV2.getDriverRegisterBankAccountLink (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

getDriverRegisterBankAccountStatus ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler API.Types.UI.DriverOnboardingV2.BankAccountResp
  )
getDriverRegisterBankAccountStatus a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverOnboardingV2.getDriverRegisterBankAccountStatus (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

getDriverRegisterGetLiveSelfie ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Domain.Types.Image.SelfieFetchStatus ->
    Environment.FlowHandler API.Types.UI.DriverOnboardingV2.GetLiveSelfieResp
  )
getDriverRegisterGetLiveSelfie a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverOnboardingV2.getDriverRegisterGetLiveSelfie (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postDriverRegisterAadhaarCard ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverOnboardingV2.AadhaarCardReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postDriverRegisterAadhaarCard a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverOnboardingV2.postDriverRegisterAadhaarCard (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postDriverRegisterLogHvSdkCall ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverOnboardingV2.HVSdkCallLogReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postDriverRegisterLogHvSdkCall a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverOnboardingV2.postDriverRegisterLogHvSdkCall (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postDriverRegisterCommonDocument ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverOnboardingV2.CommonDocumentReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postDriverRegisterCommonDocument a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverOnboardingV2.postDriverRegisterCommonDocument (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getDriverFleetRcs ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Environment.FlowHandler API.Types.UI.DriverOnboardingV2.FleetRCListRes
  )
getDriverFleetRcs a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverOnboardingV2.getDriverFleetRcs (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

postDriverDigilockerInitiate ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverOnboardingV2.DigiLockerInitiateReq ->
    Environment.FlowHandler API.Types.UI.DriverOnboardingV2.DigiLockerInitiateResp
  )
postDriverDigilockerInitiate a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverOnboardingV2.postDriverDigilockerInitiate (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postDriverDigilockerPullDocuments ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverOnboardingV2.PullDocumentReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postDriverDigilockerPullDocuments a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverOnboardingV2.postDriverDigilockerPullDocuments (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
