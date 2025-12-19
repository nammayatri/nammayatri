{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.Tickets where

import qualified "this" API.Types.UI.TicketService
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Time.Calendar
import qualified "this" Domain.Types.MerchantOnboarding
import qualified "this" Domain.Types.TicketBooking
import qualified "this" Domain.Types.TicketBookingService
import qualified "this" Domain.Types.TicketDashboard
import qualified "this" Domain.Types.TicketMerchantDetails
import qualified "this" Domain.Types.TicketPlace
import qualified "this" Domain.Types.TicketService
import qualified Domain.Types.TicketSubPlace
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.External.Payment.Interface.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data SendVerifyOtpReq = SendVerifyOtpReq {mobileNumber :: Kernel.Prelude.Text, mobileCountryCode :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TicketDashboardAgreementTemplateResp = TicketDashboardAgreementTemplateResp {template :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TicketDashboardLoginReq = TicketDashboardLoginReq {mobileNumber :: Kernel.Prelude.Text, mobileCountryCode :: Kernel.Prelude.Text, otp :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TicketDashboardLoginResp = TicketDashboardLoginResp {authToken :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TicketDashboardRegisterReq = TicketDashboardRegisterReq
  { firstName :: Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Text,
    mobileCountryCode :: Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Text,
    city :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    email :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    otp :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TicketDashboardRegisterResp = TicketDashboardRegisterResp {success :: Kernel.Prelude.Bool, message :: Kernel.Prelude.Maybe Kernel.Prelude.Text, id :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TicketDashboardUserInfo = TicketDashboardUserInfo
  { firstName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    role :: Domain.Types.MerchantOnboarding.RequestorRole,
    registeredNumber :: Kernel.Prelude.Text,
    agreementLetter :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    bankAccountNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    bankAccountType :: Kernel.Prelude.Maybe Domain.Types.TicketMerchantDetails.BankAccountType,
    bankBeneficiaryName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    bankIfsc :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    contactDetails :: Kernel.Prelude.Maybe Domain.Types.TicketMerchantDetails.ContactDetails,
    docCancelledCheque :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    docPan :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    gstin :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    orgAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    orgName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pan :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    state :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = (PostTicketsVerify :<|> PostTicketsServices :<|> GetTicketsPlaces :<|> PostTicketsUpdate :<|> PostTicketsBookingsCancel :<|> PostTicketsServiceCancel :<|> GetTicketsBookingDetails :<|> PostTicketsTicketdashboardRegister :<|> PostTicketsTicketdashboardLoginAuth :<|> PostTicketsTicketdashboardLoginVerify :<|> GetTicketsTicketdashboardAgreement :<|> GetTicketsTicketdashboardUserInfo :<|> GetTicketsTicketdashboardFile :<|> PostTicketsTicketdashboardSendverifyotp :<|> GetTicketsTicketdashboardTicketplaceInfo :<|> PostTicketsTicketdashboardTicketplaceUpdate :<|> GetTicketsTicketdashboardTicketplaces :<|> GetTicketsTicketdashboardTicketplaceSubPlaces :<|> PostTicketsTicketdashboardTicketplaceUpdateSubPlaces :<|> GetTicketFleetVehicles :<|> GetTicketFleetVehiclesV2 :<|> PostTicketBookingsVerifyV2 :<|> PostTicketPlacesBook :<|> GetTicketPlaces :<|> GetTicketPlaceServices :<|> GetTicketBookingDetails :<|> GetTicketsDashboardBookingStatus :<|> GetAllTicketBookings :<|> PostTicketBookingCashCollect :<|> PostTicketPlacesDirectBook :<|> GetTicketPlaceBookings)

type PostTicketsVerify =
  ( "tickets" :> Capture "personServiceId" (Kernel.Types.Id.Id Domain.Types.TicketService.TicketService)
      :> Capture
           "ticketBookingShortId"
           (Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService)
      :> "verify"
      :> QueryParam
           "fleetOwnerId"
           Kernel.Prelude.Text
      :> QueryParam
           "vehicleId"
           Kernel.Prelude.Text
      :> Post
           ('[JSON])
           API.Types.UI.TicketService.TicketServiceVerificationResp
  )

type PostTicketsServices =
  ( Capture "ticketPlaceId" (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace) :> "services" :> QueryParam "date" Data.Time.Calendar.Day
      :> QueryParam
           "subPlaceId"
           (Kernel.Types.Id.Id Domain.Types.TicketSubPlace.TicketSubPlace)
      :> Post
           ('[JSON])
           [API.Types.UI.TicketService.TicketServiceResp]
  )

type GetTicketsPlaces = ("places" :> Get ('[JSON]) [Domain.Types.TicketPlace.TicketPlace])

type PostTicketsUpdate = ("update" :> ReqBody ('[JSON]) API.Types.UI.TicketService.TicketBookingUpdateSeatsReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type PostTicketsBookingsCancel = ("bookings" :> "cancel" :> ReqBody ('[JSON]) API.Types.UI.TicketService.TicketBookingCancelReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type PostTicketsServiceCancel = ("service" :> "cancel" :> ReqBody ('[JSON]) API.Types.UI.TicketService.TicketServiceCancelReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type GetTicketsBookingDetails =
  ( "booking" :> Capture "ticketBookingShortId" (Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking) :> "details"
      :> Get
           ('[JSON])
           API.Types.UI.TicketService.TicketBookingDetails
  )

type PostTicketsTicketdashboardRegister = ("ticketdashboard" :> "register" :> ReqBody ('[JSON]) TicketDashboardRegisterReq :> Post ('[JSON]) TicketDashboardRegisterResp)

type PostTicketsTicketdashboardLoginAuth = ("ticketdashboard" :> "login" :> "auth" :> ReqBody ('[JSON]) TicketDashboardLoginReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type PostTicketsTicketdashboardLoginVerify = ("ticketdashboard" :> "login" :> "verify" :> ReqBody ('[JSON]) TicketDashboardLoginReq :> Post ('[JSON]) TicketDashboardLoginResp)

type GetTicketsTicketdashboardAgreement = ("ticketdashboard" :> "agreement" :> Capture "templateName" Kernel.Prelude.Text :> Get ('[JSON]) TicketDashboardAgreementTemplateResp)

type GetTicketsTicketdashboardUserInfo =
  ( "ticketdashboard" :> "user" :> "info" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> QueryParam
           "userRole"
           Domain.Types.MerchantOnboarding.RequestorRole
      :> QueryParam "requestorRole" Domain.Types.MerchantOnboarding.RequestorRole
      :> Get
           ('[JSON])
           TicketDashboardUserInfo
  )

type GetTicketsTicketdashboardFile =
  ( "ticketdashboard" :> "file" :> Capture "fileId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorRole"
           Domain.Types.MerchantOnboarding.RequestorRole
      :> Get ('[JSON]) Domain.Types.MerchantOnboarding.GetFileResponse
  )

type PostTicketsTicketdashboardSendverifyotp = ("ticketdashboard" :> "sendverifyotp" :> ReqBody ('[JSON]) SendVerifyOtpReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type GetTicketsTicketdashboardTicketplaceInfo =
  ( "ticketdashboard" :> "ticketplace"
      :> Capture
           "ticketPlaceId"
           (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace)
      :> "info"
      :> QueryParam "requestorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorRole"
           Domain.Types.MerchantOnboarding.RequestorRole
      :> Get
           ('[JSON])
           Domain.Types.TicketDashboard.TicketPlaceDashboardDetails
  )

type PostTicketsTicketdashboardTicketplaceUpdate =
  ( "ticketdashboard" :> "ticketplace" :> "update" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorRole"
           Domain.Types.MerchantOnboarding.RequestorRole
      :> ReqBody
           ('[JSON])
           Domain.Types.TicketDashboard.TicketPlaceDashboardDetails
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type GetTicketsTicketdashboardTicketplaces =
  ( "ticketdashboard" :> "ticketplaces" :> QueryParam "status" Kernel.Prelude.Text
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> QueryParam "requestorRole" Domain.Types.MerchantOnboarding.RequestorRole
      :> Get ('[JSON]) [Domain.Types.TicketPlace.TicketPlace]
  )

type GetTicketsTicketdashboardTicketplaceSubPlaces =
  ( "ticketdashboard" :> "ticketplace"
      :> Capture
           "ticketPlaceId"
           (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace)
      :> "subPlaces"
      :> Get ('[JSON]) [Domain.Types.TicketSubPlace.TicketSubPlace]
  )

type PostTicketsTicketdashboardTicketplaceUpdateSubPlaces =
  ( "ticketdashboard" :> "ticketplace"
      :> Capture
           "ticketPlaceId"
           (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace)
      :> "updateSubPlaces"
      :> ReqBody ('[JSON]) [Domain.Types.TicketSubPlace.TicketSubPlace]
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type GetTicketFleetVehicles =
  ( "ticket" :> "places" :> Capture "placeId" (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace) :> "fleet" :> "VehicleAssociation" :> "list"
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "searchString"
           Kernel.Prelude.Text
      :> Get
           ('[JSON])
           [API.Types.UI.TicketService.TicketFleetVehicleResp]
  )

type GetTicketFleetVehiclesV2 =
  ( "ticket" :> "places"
      :> Capture
           "placeId"
           (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace)
      :> "fleet"
      :> "VehicleAssociation"
      :> "list"
      :> "v2"
      :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "searchString"
           Kernel.Prelude.Text
      :> Get
           ('[JSON])
           [API.Types.UI.TicketService.TicketFleetVehicleResp]
  )

type PostTicketBookingsVerifyV2 =
  ( "tickets" :> Capture "personServiceId" (Kernel.Types.Id.Id Domain.Types.TicketService.TicketService)
      :> Capture
           "ticketServiceShortId"
           (Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService)
      :> "verify"
      :> "v2"
      :> ReqBody
           ('[JSON])
           API.Types.UI.TicketService.TicketServiceVerificationReq
      :> Post
           ('[JSON])
           API.Types.UI.TicketService.TicketServiceVerificationResp
  )

type PostTicketPlacesBook =
  ( "ticket" :> "places" :> Capture "placeId" (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace) :> "book"
      :> ReqBody
           ('[JSON])
           API.Types.UI.TicketService.TicketBookingReq
      :> Post ('[JSON]) Kernel.External.Payment.Interface.Types.CreateOrderResp
  )

type GetTicketPlaces = ("ticket" :> "places" :> Get ('[JSON]) [Domain.Types.TicketPlace.TicketPlace])

type GetTicketPlaceServices =
  ( "ticket" :> "places" :> Capture "placeId" (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace) :> "services"
      :> QueryParam
           "date"
           Data.Time.Calendar.Day
      :> QueryParam "subPlaceId" (Kernel.Types.Id.Id Domain.Types.TicketSubPlace.TicketSubPlace)
      :> Get
           ('[JSON])
           [API.Types.UI.TicketService.TicketServiceResp]
  )

type GetTicketBookingDetails =
  ( "ticket" :> "booking" :> Capture "bookingShortId" (Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking)
      :> Get
           ('[JSON])
           API.Types.UI.TicketService.TicketBookingDetails
  )

type GetTicketsDashboardBookingStatus =
  ( "dashboard" :> "booking" :> Capture "userPhoneNumber" Kernel.Prelude.Text
      :> Capture
           "bookingShortId"
           (Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking)
      :> "status"
      :> Get ('[JSON]) Domain.Types.TicketBooking.BookingStatus
  )

type GetAllTicketBookings =
  ( "ticket" :> "bookings" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "status"
           Domain.Types.TicketBooking.BookingStatus
      :> Get ('[JSON]) [API.Types.UI.TicketService.TicketBookingAPIEntityV2]
  )

type PostTicketBookingCashCollect =
  ( "ticket" :> "booking" :> Capture "bookingShortId" (Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking) :> "cashCollect"
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type PostTicketPlacesDirectBook =
  ( "ticket" :> "places" :> Capture "placeId" (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace) :> "directBook"
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> ReqBody ('[JSON]) API.Types.UI.TicketService.DirectTicketBookingReq
      :> Post
           ('[JSON])
           API.Types.UI.TicketService.DirectTicketBookingResp
  )

type GetTicketPlaceBookings =
  ( "ticket" :> "places" :> Capture "placeId" (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace) :> "bookings" :> "list"
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> MandatoryQueryParam
           "status"
           Domain.Types.TicketBooking.BookingStatus
      :> Get
           ('[JSON])
           API.Types.UI.TicketService.TicketPlaceBookingList
  )

data TicketsAPIs = TicketsAPIs
  { postTicketsVerify :: (Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> EulerHS.Types.EulerClient API.Types.UI.TicketService.TicketServiceVerificationResp),
    postTicketsServices :: (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Data.Time.Calendar.Day) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.TicketSubPlace.TicketSubPlace) -> EulerHS.Types.EulerClient [API.Types.UI.TicketService.TicketServiceResp]),
    getTicketsPlaces :: (EulerHS.Types.EulerClient [Domain.Types.TicketPlace.TicketPlace]),
    postTicketsUpdate :: (API.Types.UI.TicketService.TicketBookingUpdateSeatsReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postTicketsBookingsCancel :: (API.Types.UI.TicketService.TicketBookingCancelReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postTicketsServiceCancel :: (API.Types.UI.TicketService.TicketServiceCancelReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    getTicketsBookingDetails :: (Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> EulerHS.Types.EulerClient API.Types.UI.TicketService.TicketBookingDetails),
    postTicketsTicketdashboardRegister :: (TicketDashboardRegisterReq -> EulerHS.Types.EulerClient TicketDashboardRegisterResp),
    postTicketsTicketdashboardLoginAuth :: (TicketDashboardLoginReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postTicketsTicketdashboardLoginVerify :: (TicketDashboardLoginReq -> EulerHS.Types.EulerClient TicketDashboardLoginResp),
    getTicketsTicketdashboardAgreement :: (Kernel.Prelude.Text -> EulerHS.Types.EulerClient TicketDashboardAgreementTemplateResp),
    getTicketsTicketdashboardUserInfo :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> EulerHS.Types.EulerClient TicketDashboardUserInfo),
    getTicketsTicketdashboardFile :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> EulerHS.Types.EulerClient Domain.Types.MerchantOnboarding.GetFileResponse),
    postTicketsTicketdashboardSendverifyotp :: (SendVerifyOtpReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    getTicketsTicketdashboardTicketplaceInfo :: (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> EulerHS.Types.EulerClient Domain.Types.TicketDashboard.TicketPlaceDashboardDetails),
    postTicketsTicketdashboardTicketplaceUpdate :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Domain.Types.TicketDashboard.TicketPlaceDashboardDetails -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    getTicketsTicketdashboardTicketplaces :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> EulerHS.Types.EulerClient [Domain.Types.TicketPlace.TicketPlace]),
    getTicketsTicketdashboardTicketplaceSubPlaces :: (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> EulerHS.Types.EulerClient [Domain.Types.TicketSubPlace.TicketSubPlace]),
    postTicketsTicketdashboardTicketplaceUpdateSubPlaces :: (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> [Domain.Types.TicketSubPlace.TicketSubPlace] -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    getTicketFleetVehicles :: (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> EulerHS.Types.EulerClient [API.Types.UI.TicketService.TicketFleetVehicleResp]),
    getTicketFleetVehiclesV2 :: (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> EulerHS.Types.EulerClient [API.Types.UI.TicketService.TicketFleetVehicleResp]),
    postTicketBookingsVerifyV2 :: (Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService -> API.Types.UI.TicketService.TicketServiceVerificationReq -> EulerHS.Types.EulerClient API.Types.UI.TicketService.TicketServiceVerificationResp),
    postTicketPlacesBook :: (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> API.Types.UI.TicketService.TicketBookingReq -> EulerHS.Types.EulerClient Kernel.External.Payment.Interface.Types.CreateOrderResp),
    getTicketPlaces :: (EulerHS.Types.EulerClient [Domain.Types.TicketPlace.TicketPlace]),
    getTicketPlaceServices :: (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Data.Time.Calendar.Day) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.TicketSubPlace.TicketSubPlace) -> EulerHS.Types.EulerClient [API.Types.UI.TicketService.TicketServiceResp]),
    getTicketBookingDetails :: (Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> EulerHS.Types.EulerClient API.Types.UI.TicketService.TicketBookingDetails),
    getTicketsDashboardBookingStatus :: (Kernel.Prelude.Text -> Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> EulerHS.Types.EulerClient Domain.Types.TicketBooking.BookingStatus),
    getAllTicketBookings :: (Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Domain.Types.TicketBooking.BookingStatus) -> EulerHS.Types.EulerClient [API.Types.UI.TicketService.TicketBookingAPIEntityV2]),
    postTicketBookingCashCollect :: (Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postTicketPlacesDirectBook :: (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.UI.TicketService.DirectTicketBookingReq -> EulerHS.Types.EulerClient API.Types.UI.TicketService.DirectTicketBookingResp),
    getTicketPlaceBookings :: (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Domain.Types.TicketBooking.BookingStatus -> EulerHS.Types.EulerClient API.Types.UI.TicketService.TicketPlaceBookingList)
  }

mkTicketsAPIs :: (Client EulerHS.Types.EulerClient API -> TicketsAPIs)
mkTicketsAPIs ticketsClient = (TicketsAPIs {..})
  where
    postTicketsVerify :<|> postTicketsServices :<|> getTicketsPlaces :<|> postTicketsUpdate :<|> postTicketsBookingsCancel :<|> postTicketsServiceCancel :<|> getTicketsBookingDetails :<|> postTicketsTicketdashboardRegister :<|> postTicketsTicketdashboardLoginAuth :<|> postTicketsTicketdashboardLoginVerify :<|> getTicketsTicketdashboardAgreement :<|> getTicketsTicketdashboardUserInfo :<|> getTicketsTicketdashboardFile :<|> postTicketsTicketdashboardSendverifyotp :<|> getTicketsTicketdashboardTicketplaceInfo :<|> postTicketsTicketdashboardTicketplaceUpdate :<|> getTicketsTicketdashboardTicketplaces :<|> getTicketsTicketdashboardTicketplaceSubPlaces :<|> postTicketsTicketdashboardTicketplaceUpdateSubPlaces :<|> getTicketFleetVehicles :<|> getTicketFleetVehiclesV2 :<|> postTicketBookingsVerifyV2 :<|> postTicketPlacesBook :<|> getTicketPlaces :<|> getTicketPlaceServices :<|> getTicketBookingDetails :<|> getTicketsDashboardBookingStatus :<|> getAllTicketBookings :<|> postTicketBookingCashCollect :<|> postTicketPlacesDirectBook :<|> getTicketPlaceBookings = ticketsClient

data TicketsUserActionType
  = POST_TICKETS_VERIFY
  | POST_TICKETS_SERVICES
  | GET_TICKETS_PLACES
  | POST_TICKETS_UPDATE
  | POST_TICKETS_BOOKINGS_CANCEL
  | POST_TICKETS_SERVICE_CANCEL
  | GET_TICKETS_BOOKING_DETAILS
  | POST_TICKETS_TICKETDASHBOARD_REGISTER
  | POST_TICKETS_TICKETDASHBOARD_LOGIN_AUTH
  | POST_TICKETS_TICKETDASHBOARD_LOGIN_VERIFY
  | GET_TICKETS_TICKETDASHBOARD_AGREEMENT
  | GET_TICKETS_TICKETDASHBOARD_USER_INFO
  | GET_TICKETS_TICKETDASHBOARD_FILE
  | POST_TICKETS_TICKETDASHBOARD_SENDVERIFYOTP
  | GET_TICKETS_TICKETDASHBOARD_TICKETPLACE_INFO
  | POST_TICKETS_TICKETDASHBOARD_TICKETPLACE_UPDATE
  | GET_TICKETS_TICKETDASHBOARD_TICKETPLACES
  | GET_TICKETS_TICKETDASHBOARD_TICKETPLACE_SUB_PLACES
  | POST_TICKETS_TICKETDASHBOARD_TICKETPLACE_UPDATE_SUB_PLACES
  | GET_TICKET_FLEET_VEHICLES
  | GET_TICKET_FLEET_VEHICLES_V2
  | POST_TICKET_BOOKINGS_VERIFY_V2
  | POST_TICKET_PLACES_BOOK
  | GET_TICKET_PLACES
  | GET_TICKET_PLACE_SERVICES
  | GET_TICKET_BOOKING_DETAILS
  | GET_TICKETS_DASHBOARD_BOOKING_STATUS
  | GET_ALL_TICKET_BOOKINGS
  | POST_TICKET_BOOKING_CASH_COLLECT
  | POST_TICKET_PLACES_DIRECT_BOOK
  | GET_TICKET_PLACE_BOOKINGS
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''TicketsUserActionType)])
