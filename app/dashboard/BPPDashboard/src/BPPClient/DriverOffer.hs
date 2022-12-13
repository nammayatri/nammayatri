{-# LANGUAGE AllowAmbiguousTypes #-}

module BPPClient.DriverOffer
  ( callDriverOfferBPP,
    DriversAPIs (..),
    DriverOfferAPIs (..),
  )
where

import "driver-offer-bpp" API.Dashboard as BPP
import Beckn.Prelude
import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Dashboard.Common.Driver as Common
import qualified Dashboard.Common.Driver.Registration as Common
import qualified Dashboard.Common.Ride as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import Domain.Types.ServerName
import qualified EulerHS.Types as Euler
import Servant
import Tools.Auth.Merchant (CheckedShortId)
import Tools.Client
import "lib-dashboard" Tools.Metrics

data DriverOfferAPIs = DriverOfferAPIs
  { drivers :: DriversAPIs,
    rides :: RidesAPIs
  }

data DriversAPIs = DriversAPIs
  { driverDocumentsInfo :: Euler.EulerClient Common.DriverDocumentsInfoRes,
    listDrivers :: Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> Euler.EulerClient Common.DriverListRes,
    driverActivity :: Euler.EulerClient Common.DriverActivityRes,
    enableDriver :: Id Common.Driver -> Euler.EulerClient APISuccess,
    disableDriver :: Id Common.Driver -> Euler.EulerClient APISuccess,
    driverLocation :: Maybe Int -> Maybe Int -> Common.DriverIds -> Euler.EulerClient Common.DriverLocationRes,
    driverInfo :: Maybe Text -> Maybe Text -> Euler.EulerClient Common.DriverInfoRes,
    deleteDriver :: Id Common.Driver -> Euler.EulerClient APISuccess,
    documentsList :: Id Common.Driver -> Euler.EulerClient Common.DocumentsListResponse,
    getDocument :: Id Common.Image -> Euler.EulerClient Common.GetDocumentResponse,
    uploadDocument :: Id Common.Driver -> Common.UploadDocumentReq -> Euler.EulerClient Common.UploadDocumentResp,
    registerDL :: Id Common.Driver -> Common.RegisterDLReq -> Euler.EulerClient APISuccess,
    registerRC :: Id Common.Driver -> Common.RegisterRCReq -> Euler.EulerClient APISuccess,
    unlinkVehicle :: Id Common.Driver -> Euler.EulerClient APISuccess,
    updatePhoneNumber :: Id Common.Driver -> Common.UpdatePhoneNumberReq -> Euler.EulerClient APISuccess,
    addVehicle :: Id Common.Driver -> Common.AddVehicleReq -> Euler.EulerClient APISuccess
  }

data RidesAPIs = RidesAPIs
  { rideList :: Maybe Int -> Maybe Int -> Maybe Common.BookingStatus -> Maybe (Id Common.Ride) -> Maybe Text -> Maybe Text -> Euler.EulerClient Common.RideListRes,
    rideStart :: Id Common.Ride -> Common.StartRideReq -> Euler.EulerClient APISuccess,
    rideEnd :: Id Common.Ride -> Common.EndRideReq -> Euler.EulerClient APISuccess,
    rideCancel :: Id Common.Ride -> Common.CancelRideReq -> Euler.EulerClient APISuccess,
    rideInfo :: Id Common.Ride -> Euler.EulerClient Common.RideInfoRes
  }

mkDriverOfferAPIs :: CheckedShortId DM.Merchant -> Text -> DriverOfferAPIs
mkDriverOfferAPIs merchantId token = do
  let drivers = DriversAPIs {..}
  let rides = RidesAPIs {..}
  DriverOfferAPIs {..}
  where
    driversClient
      :<|> ridesClient = clientWithMerchant (Proxy :: Proxy BPP.API') merchantId token

    driverDocumentsInfo
      :<|> listDrivers
      :<|> driverActivity
      :<|> enableDriver
      :<|> disableDriver
      :<|> driverLocation
      :<|> driverInfo
      :<|> deleteDriver
      :<|> unlinkVehicle
      :<|> updatePhoneNumber
      :<|> addVehicle
      :<|> ( documentsList
               :<|> getDocument
               :<|> uploadDocument
               :<|> registerDL
               :<|> registerRC
             ) =
        driversClient

    rideList
      :<|> rideStart
      :<|> rideEnd
      :<|> rideCancel
      :<|> rideInfo = ridesClient

callDriverOfferBPP ::
  forall m r b c.
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DataServer]],
    CallServerAPI DriverOfferAPIs m r b c
  ) =>
  CheckedShortId DM.Merchant ->
  (DriverOfferAPIs -> b) ->
  c
callDriverOfferBPP merchantId = callServerAPI @_ @m @r DRIVER_OFFER_BPP (mkDriverOfferAPIs merchantId) "callDriverOfferBPP"
