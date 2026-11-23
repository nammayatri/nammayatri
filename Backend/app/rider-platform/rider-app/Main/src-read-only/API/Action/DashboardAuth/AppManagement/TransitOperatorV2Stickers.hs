{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.AppManagement.TransitOperatorV2Stickers
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.TransitOperatorV2Stickers
import qualified "beckn-spec" BecknV2.OnDemand.Enums
import qualified Control.Lens
import qualified Domain.Action.Dashboard.AppManagement.TransitOperatorV2Stickers
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified "bms-backend" Lib.BmsBackend.Transit.Stickers.Types
import Servant
import qualified Tools.ActorInfo
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("transitOperatorV2" :> (TransitOperatorV2ListStickerDesigns :<|> TransitOperatorV2CreateStickerDesign :<|> TransitOperatorV2PreviewSticker :<|> TransitOperatorV2GetStickerSigning :<|> TransitOperatorV2ReadStickerFleetFile :<|> TransitOperatorV2GetStickerDesign :<|> TransitOperatorV2UpdateStickerDesign :<|> TransitOperatorV2DeleteStickerDesign :<|> TransitOperatorV2GetStickerBackground :<|> TransitOperatorV2DuplicateStickerDesign :<|> TransitOperatorV2GenerateStickers :<|> TransitOperatorV2ListStickerBatches :<|> TransitOperatorV2ListStickerFonts :<|> TransitOperatorV2UploadStickerFont :<|> TransitOperatorV2DeleteStickerFont :<|> TransitOperatorV2GetStickerFontFile))

type TransitOperatorV2ListStickerDesigns =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_LIST_STICKER_DESIGNS"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Stickers.TransitOperatorV2ListStickerDesigns
  )

type TransitOperatorV2CreateStickerDesign =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_CREATE_STICKER_DESIGN"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Stickers.TransitOperatorV2CreateStickerDesign
  )

type TransitOperatorV2PreviewSticker =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_PREVIEW_STICKER"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Stickers.TransitOperatorV2PreviewSticker
  )

type TransitOperatorV2GetStickerSigning =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_GET_STICKER_SIGNING"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Stickers.TransitOperatorV2GetStickerSigning
  )

type TransitOperatorV2ReadStickerFleetFile =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_READ_STICKER_FLEET_FILE"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Stickers.TransitOperatorV2ReadStickerFleetFile
  )

type TransitOperatorV2GetStickerDesign =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_GET_STICKER_DESIGN"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Stickers.TransitOperatorV2GetStickerDesign
  )

type TransitOperatorV2UpdateStickerDesign =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_UPDATE_STICKER_DESIGN"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Stickers.TransitOperatorV2UpdateStickerDesign
  )

type TransitOperatorV2DeleteStickerDesign =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_DELETE_STICKER_DESIGN"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Stickers.TransitOperatorV2DeleteStickerDesign
  )

type TransitOperatorV2GetStickerBackground =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_GET_STICKER_BACKGROUND"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Stickers.TransitOperatorV2GetStickerBackground
  )

type TransitOperatorV2DuplicateStickerDesign =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_DUPLICATE_STICKER_DESIGN"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Stickers.TransitOperatorV2DuplicateStickerDesign
  )

type TransitOperatorV2GenerateStickers =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_GENERATE_STICKERS"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Stickers.TransitOperatorV2GenerateStickers
  )

type TransitOperatorV2ListStickerBatches =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_LIST_STICKER_BATCHES"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Stickers.TransitOperatorV2ListStickerBatches
  )

type TransitOperatorV2ListStickerFonts =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_LIST_STICKER_FONTS"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Stickers.TransitOperatorV2ListStickerFonts
  )

type TransitOperatorV2UploadStickerFont =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_UPLOAD_STICKER_FONT"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Stickers.TransitOperatorV2UploadStickerFont
  )

type TransitOperatorV2DeleteStickerFont =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_DELETE_STICKER_FONT"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Stickers.TransitOperatorV2DeleteStickerFont
  )

type TransitOperatorV2GetStickerFontFile =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_GET_STICKER_FONT_FILE"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Stickers.TransitOperatorV2GetStickerFontFile
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = transitOperatorV2ListStickerDesigns merchantId city :<|> transitOperatorV2CreateStickerDesign merchantId city :<|> transitOperatorV2PreviewSticker merchantId city :<|> transitOperatorV2GetStickerSigning merchantId city :<|> transitOperatorV2ReadStickerFleetFile merchantId city :<|> transitOperatorV2GetStickerDesign merchantId city :<|> transitOperatorV2UpdateStickerDesign merchantId city :<|> transitOperatorV2DeleteStickerDesign merchantId city :<|> transitOperatorV2GetStickerBackground merchantId city :<|> transitOperatorV2DuplicateStickerDesign merchantId city :<|> transitOperatorV2GenerateStickers merchantId city :<|> transitOperatorV2ListStickerBatches merchantId city :<|> transitOperatorV2ListStickerFonts merchantId city :<|> transitOperatorV2UploadStickerFont merchantId city :<|> transitOperatorV2DeleteStickerFont merchantId city :<|> transitOperatorV2GetStickerFontFile merchantId city

transitOperatorV2ListStickerDesigns :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Lib.BmsBackend.Transit.Stickers.Types.StickerDesignSummaryList)
transitOperatorV2ListStickerDesigns a8 a7 a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a6 a8 a7
        Tools.ActorInfo.withDashboardUserActorInfo a6 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Stickers.transitOperatorV2ListStickerDesigns a8 a7 a5 a4 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a6)) a1
    )

transitOperatorV2CreateStickerDesign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Stickers.Types.StickerDesignCreate -> Environment.FlowHandler Lib.BmsBackend.Transit.Stickers.Types.StickerDesign)
transitOperatorV2CreateStickerDesign a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_CREATE_STICKER_DESIGN" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Stickers.transitOperatorV2CreateStickerDesign a5 a4 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a2 a1
    )

transitOperatorV2PreviewSticker :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Stickers.Types.StickerPreviewReq -> Environment.FlowHandler Lib.BmsBackend.Transit.Stickers.Types.StickerFile)
transitOperatorV2PreviewSticker a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_PREVIEW_STICKER" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Stickers.transitOperatorV2PreviewSticker a5 a4 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a2 a1
    )

transitOperatorV2GetStickerSigning :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Lib.BmsBackend.Transit.Stickers.Types.StickerSigning)
transitOperatorV2GetStickerSigning a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a2 a4 a3
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Stickers.transitOperatorV2GetStickerSigning a4 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2)) a1
    )

transitOperatorV2ReadStickerFleetFile :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Stickers.Types.FleetFileReq -> Environment.FlowHandler Lib.BmsBackend.Transit.Stickers.Types.FleetFileRead)
transitOperatorV2ReadStickerFleetFile a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_READ_STICKER_FLEET_FILE" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Stickers.transitOperatorV2ReadStickerFleetFile a5 a4 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a2 a1
    )

transitOperatorV2GetStickerDesign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Lib.BmsBackend.Transit.Stickers.Types.StickerDesign)
transitOperatorV2GetStickerDesign a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Stickers.transitOperatorV2GetStickerDesign a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )

transitOperatorV2UpdateStickerDesign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Stickers.Types.StickerDesignUpdate -> Environment.FlowHandler Lib.BmsBackend.Transit.Stickers.Types.StickerDesign)
transitOperatorV2UpdateStickerDesign a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a4 a6 a5
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_UPDATE_STICKER_DESIGN" a4 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Stickers.transitOperatorV2UpdateStickerDesign a6 a5 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a2 a1
    )

transitOperatorV2DeleteStickerDesign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
transitOperatorV2DeleteStickerDesign a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_DELETE_STICKER_DESIGN" a3 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Stickers.transitOperatorV2DeleteStickerDesign a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )

transitOperatorV2GetStickerBackground :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Lib.BmsBackend.Transit.Stickers.Types.StickerFile)
transitOperatorV2GetStickerBackground a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Stickers.transitOperatorV2GetStickerBackground a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )

transitOperatorV2DuplicateStickerDesign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Lib.BmsBackend.Transit.Stickers.Types.StickerDesign)
transitOperatorV2DuplicateStickerDesign a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_DUPLICATE_STICKER_DESIGN" a3 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Stickers.transitOperatorV2DuplicateStickerDesign a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )

transitOperatorV2GenerateStickers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Stickers.Types.StickerGenerateReq -> Environment.FlowHandler Lib.BmsBackend.Transit.Stickers.Types.StickerFile)
transitOperatorV2GenerateStickers a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a4 a6 a5
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_GENERATE_STICKERS" a4 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Stickers.transitOperatorV2GenerateStickers a6 a5 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a2 a1
    )

transitOperatorV2ListStickerBatches :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [Lib.BmsBackend.Transit.Stickers.Types.StickerBatch])
transitOperatorV2ListStickerBatches a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Stickers.transitOperatorV2ListStickerBatches a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )

transitOperatorV2ListStickerFonts :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [Lib.BmsBackend.Transit.Stickers.Types.StickerFont])
transitOperatorV2ListStickerFonts a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Stickers.transitOperatorV2ListStickerFonts a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )

transitOperatorV2UploadStickerFont :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Stickers.Types.StickerFontUpload -> Environment.FlowHandler Lib.BmsBackend.Transit.Stickers.Types.StickerFont)
transitOperatorV2UploadStickerFont a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_UPLOAD_STICKER_FONT" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Stickers.transitOperatorV2UploadStickerFont a5 a4 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a2 a1
    )

transitOperatorV2DeleteStickerFont :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
transitOperatorV2DeleteStickerFont a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_STICKERS/TRANSIT_OPERATOR_V2_DELETE_STICKER_FONT" a3 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Stickers.transitOperatorV2DeleteStickerFont a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )

transitOperatorV2GetStickerFontFile :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Lib.BmsBackend.Transit.Stickers.Types.StickerFile)
transitOperatorV2GetStickerFontFile a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Stickers.transitOperatorV2GetStickerFontFile a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )
