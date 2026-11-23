{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.TransitOperatorV2Stickers where

import qualified "beckn-spec" BecknV2.OnDemand.Enums
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified "bms-backend" Lib.BmsBackend.Transit.Stickers.Types
import Servant
import Servant.Client

type API = ("transitOperatorV2" :> (TransitOperatorV2ListStickerDesignsHelper :<|> TransitOperatorV2CreateStickerDesignHelper :<|> TransitOperatorV2PreviewStickerHelper :<|> TransitOperatorV2GetStickerSigningHelper :<|> TransitOperatorV2ReadStickerFleetFileHelper :<|> TransitOperatorV2GetStickerDesignHelper :<|> TransitOperatorV2UpdateStickerDesignHelper :<|> TransitOperatorV2DeleteStickerDesignHelper :<|> TransitOperatorV2GetStickerBackgroundHelper :<|> TransitOperatorV2DuplicateStickerDesignHelper :<|> TransitOperatorV2GenerateStickersHelper :<|> TransitOperatorV2ListStickerBatchesHelper :<|> TransitOperatorV2ListStickerFontsHelper :<|> TransitOperatorV2UploadStickerFontHelper :<|> TransitOperatorV2DeleteStickerFontHelper :<|> TransitOperatorV2GetStickerFontFileHelper))

type TransitOperatorV2ListStickerDesigns =
  ( "stickerDesigns" :> QueryParam "search" Kernel.Prelude.Text :> QueryParam "organizationId" Kernel.Prelude.Text
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.Stickers.Types.StickerDesignSummaryList
  )

type TransitOperatorV2ListStickerDesignsHelper =
  ( "stickerDesigns" :> QueryParam "search" Kernel.Prelude.Text :> QueryParam "organizationId" Kernel.Prelude.Text
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.Stickers.Types.StickerDesignSummaryList
  )

type TransitOperatorV2CreateStickerDesign =
  ( "stickerDesigns" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.Stickers.Types.StickerDesignCreate
      :> Post ('[JSON]) Lib.BmsBackend.Transit.Stickers.Types.StickerDesign
  )

type TransitOperatorV2CreateStickerDesignHelper =
  ( "stickerDesigns" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.Stickers.Types.StickerDesignCreate
      :> Post
           ('[JSON])
           Lib.BmsBackend.Transit.Stickers.Types.StickerDesign
  )

type TransitOperatorV2PreviewSticker =
  ( "stickerDesigns" :> "preview" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.Stickers.Types.StickerPreviewReq
      :> Post ('[JSON]) Lib.BmsBackend.Transit.Stickers.Types.StickerFile
  )

type TransitOperatorV2PreviewStickerHelper =
  ( "stickerDesigns" :> "preview" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.Stickers.Types.StickerPreviewReq
      :> Post
           ('[JSON])
           Lib.BmsBackend.Transit.Stickers.Types.StickerFile
  )

type TransitOperatorV2GetStickerSigning =
  ( "stickerDesigns" :> "signing" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.Stickers.Types.StickerSigning
  )

type TransitOperatorV2GetStickerSigningHelper =
  ( "stickerDesigns" :> "signing" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) Lib.BmsBackend.Transit.Stickers.Types.StickerSigning
  )

type TransitOperatorV2ReadStickerFleetFile =
  ( "stickerDesigns" :> "fleetFile" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.Stickers.Types.FleetFileReq
      :> Post ('[JSON]) Lib.BmsBackend.Transit.Stickers.Types.FleetFileRead
  )

type TransitOperatorV2ReadStickerFleetFileHelper =
  ( "stickerDesigns" :> "fleetFile" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.Stickers.Types.FleetFileReq
      :> Post
           ('[JSON])
           Lib.BmsBackend.Transit.Stickers.Types.FleetFileRead
  )

type TransitOperatorV2GetStickerDesign =
  ( "stickerDesigns" :> Capture "designId" Kernel.Prelude.Text :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.Stickers.Types.StickerDesign
  )

type TransitOperatorV2GetStickerDesignHelper =
  ( "stickerDesigns" :> Capture "designId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) Lib.BmsBackend.Transit.Stickers.Types.StickerDesign
  )

type TransitOperatorV2UpdateStickerDesign =
  ( "stickerDesigns" :> Capture "designId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.Stickers.Types.StickerDesignUpdate
      :> Put
           ('[JSON])
           Lib.BmsBackend.Transit.Stickers.Types.StickerDesign
  )

type TransitOperatorV2UpdateStickerDesignHelper =
  ( "stickerDesigns" :> Capture "designId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.Stickers.Types.StickerDesignUpdate
      :> Put
           ('[JSON])
           Lib.BmsBackend.Transit.Stickers.Types.StickerDesign
  )

type TransitOperatorV2DeleteStickerDesign =
  ( "stickerDesigns" :> Capture "designId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Delete ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type TransitOperatorV2DeleteStickerDesignHelper =
  ( "stickerDesigns" :> Capture "designId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Delete ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type TransitOperatorV2GetStickerBackground =
  ( "stickerDesigns" :> Capture "designId" Kernel.Prelude.Text :> "background"
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) Lib.BmsBackend.Transit.Stickers.Types.StickerFile
  )

type TransitOperatorV2GetStickerBackgroundHelper =
  ( "stickerDesigns" :> Capture "designId" Kernel.Prelude.Text :> "background"
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.Stickers.Types.StickerFile
  )

type TransitOperatorV2DuplicateStickerDesign =
  ( "stickerDesigns" :> Capture "designId" Kernel.Prelude.Text :> "duplicate"
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Post ('[JSON]) Lib.BmsBackend.Transit.Stickers.Types.StickerDesign
  )

type TransitOperatorV2DuplicateStickerDesignHelper =
  ( "stickerDesigns" :> Capture "designId" Kernel.Prelude.Text :> "duplicate"
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Post
           ('[JSON])
           Lib.BmsBackend.Transit.Stickers.Types.StickerDesign
  )

type TransitOperatorV2GenerateStickers =
  ( "stickerDesigns" :> Capture "designId" Kernel.Prelude.Text :> "generate"
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.Stickers.Types.StickerGenerateReq
      :> Post
           ('[JSON])
           Lib.BmsBackend.Transit.Stickers.Types.StickerFile
  )

type TransitOperatorV2GenerateStickersHelper =
  ( "stickerDesigns" :> Capture "designId" Kernel.Prelude.Text :> "generate"
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.Stickers.Types.StickerGenerateReq
      :> Post
           ('[JSON])
           Lib.BmsBackend.Transit.Stickers.Types.StickerFile
  )

type TransitOperatorV2ListStickerBatches =
  ( "stickerDesigns" :> Capture "designId" Kernel.Prelude.Text :> "batches"
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) [Lib.BmsBackend.Transit.Stickers.Types.StickerBatch]
  )

type TransitOperatorV2ListStickerBatchesHelper =
  ( "stickerDesigns" :> Capture "designId" Kernel.Prelude.Text :> "batches"
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           [Lib.BmsBackend.Transit.Stickers.Types.StickerBatch]
  )

type TransitOperatorV2ListStickerFonts =
  ( "stickerFonts" :> QueryParam "organizationId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) [Lib.BmsBackend.Transit.Stickers.Types.StickerFont]
  )

type TransitOperatorV2ListStickerFontsHelper =
  ( "stickerFonts" :> QueryParam "organizationId" Kernel.Prelude.Text
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) [Lib.BmsBackend.Transit.Stickers.Types.StickerFont]
  )

type TransitOperatorV2UploadStickerFont =
  ( "stickerFonts" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.Stickers.Types.StickerFontUpload
      :> Post ('[JSON]) Lib.BmsBackend.Transit.Stickers.Types.StickerFont
  )

type TransitOperatorV2UploadStickerFontHelper =
  ( "stickerFonts" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.Stickers.Types.StickerFontUpload
      :> Post
           ('[JSON])
           Lib.BmsBackend.Transit.Stickers.Types.StickerFont
  )

type TransitOperatorV2DeleteStickerFont =
  ( "stickerFonts" :> Capture "fontId" Kernel.Prelude.Text :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Delete
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type TransitOperatorV2DeleteStickerFontHelper =
  ( "stickerFonts" :> Capture "fontId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Delete ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type TransitOperatorV2GetStickerFontFile =
  ( "stickerFonts" :> Capture "fontId" Kernel.Prelude.Text :> "file"
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) Lib.BmsBackend.Transit.Stickers.Types.StickerFile
  )

type TransitOperatorV2GetStickerFontFileHelper =
  ( "stickerFonts" :> Capture "fontId" Kernel.Prelude.Text :> "file"
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) Lib.BmsBackend.Transit.Stickers.Types.StickerFile
  )

data TransitOperatorV2StickersAPIs = TransitOperatorV2StickersAPIs
  { transitOperatorV2ListStickerDesigns :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Stickers.Types.StickerDesignSummaryList),
    transitOperatorV2CreateStickerDesign :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Stickers.Types.StickerDesignCreate -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Stickers.Types.StickerDesign),
    transitOperatorV2PreviewSticker :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Stickers.Types.StickerPreviewReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Stickers.Types.StickerFile),
    transitOperatorV2GetStickerSigning :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Stickers.Types.StickerSigning),
    transitOperatorV2ReadStickerFleetFile :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Stickers.Types.FleetFileReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Stickers.Types.FleetFileRead),
    transitOperatorV2GetStickerDesign :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Stickers.Types.StickerDesign),
    transitOperatorV2UpdateStickerDesign :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Stickers.Types.StickerDesignUpdate -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Stickers.Types.StickerDesign),
    transitOperatorV2DeleteStickerDesign :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    transitOperatorV2GetStickerBackground :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Stickers.Types.StickerFile),
    transitOperatorV2DuplicateStickerDesign :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Stickers.Types.StickerDesign),
    transitOperatorV2GenerateStickers :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Stickers.Types.StickerGenerateReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Stickers.Types.StickerFile),
    transitOperatorV2ListStickerBatches :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [Lib.BmsBackend.Transit.Stickers.Types.StickerBatch]),
    transitOperatorV2ListStickerFonts :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [Lib.BmsBackend.Transit.Stickers.Types.StickerFont]),
    transitOperatorV2UploadStickerFont :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Stickers.Types.StickerFontUpload -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Stickers.Types.StickerFont),
    transitOperatorV2DeleteStickerFont :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    transitOperatorV2GetStickerFontFile :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Stickers.Types.StickerFile)
  }

mkTransitOperatorV2StickersAPIs :: (Client EulerHS.Types.EulerClient API -> TransitOperatorV2StickersAPIs)
mkTransitOperatorV2StickersAPIs transitOperatorV2StickersClient = (TransitOperatorV2StickersAPIs {..})
  where
    transitOperatorV2ListStickerDesigns :<|> transitOperatorV2CreateStickerDesign :<|> transitOperatorV2PreviewSticker :<|> transitOperatorV2GetStickerSigning :<|> transitOperatorV2ReadStickerFleetFile :<|> transitOperatorV2GetStickerDesign :<|> transitOperatorV2UpdateStickerDesign :<|> transitOperatorV2DeleteStickerDesign :<|> transitOperatorV2GetStickerBackground :<|> transitOperatorV2DuplicateStickerDesign :<|> transitOperatorV2GenerateStickers :<|> transitOperatorV2ListStickerBatches :<|> transitOperatorV2ListStickerFonts :<|> transitOperatorV2UploadStickerFont :<|> transitOperatorV2DeleteStickerFont :<|> transitOperatorV2GetStickerFontFile = transitOperatorV2StickersClient

data TransitOperatorV2StickersUserActionType
  = TRANSIT_OPERATOR_V2_LIST_STICKER_DESIGNS
  | TRANSIT_OPERATOR_V2_CREATE_STICKER_DESIGN
  | TRANSIT_OPERATOR_V2_PREVIEW_STICKER
  | TRANSIT_OPERATOR_V2_GET_STICKER_SIGNING
  | TRANSIT_OPERATOR_V2_READ_STICKER_FLEET_FILE
  | TRANSIT_OPERATOR_V2_GET_STICKER_DESIGN
  | TRANSIT_OPERATOR_V2_UPDATE_STICKER_DESIGN
  | TRANSIT_OPERATOR_V2_DELETE_STICKER_DESIGN
  | TRANSIT_OPERATOR_V2_GET_STICKER_BACKGROUND
  | TRANSIT_OPERATOR_V2_DUPLICATE_STICKER_DESIGN
  | TRANSIT_OPERATOR_V2_GENERATE_STICKERS
  | TRANSIT_OPERATOR_V2_LIST_STICKER_BATCHES
  | TRANSIT_OPERATOR_V2_LIST_STICKER_FONTS
  | TRANSIT_OPERATOR_V2_UPLOAD_STICKER_FONT
  | TRANSIT_OPERATOR_V2_DELETE_STICKER_FONT
  | TRANSIT_OPERATOR_V2_GET_STICKER_FONT_FILE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''TransitOperatorV2StickersUserActionType)])
