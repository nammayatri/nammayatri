{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.AppManagement.TransitOperatorV2Stickers
  ( transitOperatorV2ListStickerDesigns,
    transitOperatorV2CreateStickerDesign,
    transitOperatorV2PreviewSticker,
    transitOperatorV2GetStickerSigning,
    transitOperatorV2ReadStickerFleetFile,
    transitOperatorV2GetStickerDesign,
    transitOperatorV2UpdateStickerDesign,
    transitOperatorV2DeleteStickerDesign,
    transitOperatorV2GetStickerBackground,
    transitOperatorV2DuplicateStickerDesign,
    transitOperatorV2GenerateStickers,
    transitOperatorV2ListStickerBatches,
    transitOperatorV2ListStickerFonts,
    transitOperatorV2UploadStickerFont,
    transitOperatorV2DeleteStickerFont,
    transitOperatorV2GetStickerFontFile,
  )
where

import qualified "beckn-spec" BecknV2.OnDemand.Enums
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified "bms-backend" Lib.BmsBackend.Transit.Stickers.API as API
import qualified "bms-backend" Lib.BmsBackend.Transit.Stickers.Types
import Servant
import SharedLogic.TransitOperatorV2 (tms, tmsUnit, transitCtx)
import qualified SharedLogic.TransitOperatorV2 as Fields
import Tools.Auth

transitOperatorV2ListStickerDesigns :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Lib.BmsBackend.Transit.Stickers.Types.StickerDesignSummaryList)
transitOperatorV2ListStickerDesigns merchantShortId opCity search organizationId limit offset requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "listStickerDesigns" API.listStickerDesignsAPI (API.listStickerDesigns ctx.gtfsId search organizationId limit offset)

transitOperatorV2CreateStickerDesign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Stickers.Types.StickerDesignCreate -> Environment.Flow Lib.BmsBackend.Transit.Stickers.Types.StickerDesign)
transitOperatorV2CreateStickerDesign merchantShortId opCity requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "createStickerDesign" API.createStickerDesignAPI (API.createStickerDesign ctx.gtfsId req)

transitOperatorV2PreviewSticker :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Stickers.Types.StickerPreviewReq -> Environment.Flow Lib.BmsBackend.Transit.Stickers.Types.StickerFile)
transitOperatorV2PreviewSticker merchantShortId opCity requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "previewSticker" API.previewStickerAPI (API.previewSticker ctx.gtfsId req)

transitOperatorV2GetStickerSigning :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Lib.BmsBackend.Transit.Stickers.Types.StickerSigning)
transitOperatorV2GetStickerSigning merchantShortId opCity requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "getStickerSigning" API.getStickerSigningAPI (API.getStickerSigning ctx.gtfsId)

transitOperatorV2ReadStickerFleetFile :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Stickers.Types.FleetFileReq -> Environment.Flow Lib.BmsBackend.Transit.Stickers.Types.FleetFileRead)
transitOperatorV2ReadStickerFleetFile merchantShortId opCity requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "readFleetFile" API.readFleetFileAPI (API.readFleetFile ctx.gtfsId req)

transitOperatorV2GetStickerDesign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Lib.BmsBackend.Transit.Stickers.Types.StickerDesign)
transitOperatorV2GetStickerDesign merchantShortId opCity designId requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "getStickerDesign" API.getStickerDesignAPI (API.getStickerDesign ctx.gtfsId designId)

transitOperatorV2UpdateStickerDesign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Stickers.Types.StickerDesignUpdate -> Environment.Flow Lib.BmsBackend.Transit.Stickers.Types.StickerDesign)
transitOperatorV2UpdateStickerDesign merchantShortId opCity designId requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "updateStickerDesign" API.updateStickerDesignAPI (API.updateStickerDesign ctx.gtfsId designId req)

transitOperatorV2DeleteStickerDesign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
transitOperatorV2DeleteStickerDesign merchantShortId opCity designId requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tmsUnit ctx "deleteStickerDesign" API.deleteStickerDesignAPI (API.deleteStickerDesign ctx.gtfsId designId)

transitOperatorV2GetStickerBackground :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Lib.BmsBackend.Transit.Stickers.Types.StickerFile)
transitOperatorV2GetStickerBackground merchantShortId opCity designId requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "getStickerBackground" API.getStickerBackgroundAPI (API.getStickerBackground ctx.gtfsId designId)

transitOperatorV2DuplicateStickerDesign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Lib.BmsBackend.Transit.Stickers.Types.StickerDesign)
transitOperatorV2DuplicateStickerDesign merchantShortId opCity designId requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "duplicateStickerDesign" API.duplicateStickerDesignAPI (API.duplicateStickerDesign ctx.gtfsId designId)

transitOperatorV2GenerateStickers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Stickers.Types.StickerGenerateReq -> Environment.Flow Lib.BmsBackend.Transit.Stickers.Types.StickerFile)
transitOperatorV2GenerateStickers merchantShortId opCity designId requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "generateStickers" API.generateStickersAPI (API.generateStickers ctx.gtfsId designId req)

transitOperatorV2ListStickerBatches :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [Lib.BmsBackend.Transit.Stickers.Types.StickerBatch])
transitOperatorV2ListStickerBatches merchantShortId opCity designId requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "listStickerBatches" API.listStickerBatchesAPI (API.listStickerBatches ctx.gtfsId designId)

transitOperatorV2ListStickerFonts :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [Lib.BmsBackend.Transit.Stickers.Types.StickerFont])
transitOperatorV2ListStickerFonts merchantShortId opCity organizationId requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "listStickerFonts" API.listStickerFontsAPI (API.listStickerFonts ctx.gtfsId organizationId)

transitOperatorV2UploadStickerFont :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Stickers.Types.StickerFontUpload -> Environment.Flow Lib.BmsBackend.Transit.Stickers.Types.StickerFont)
transitOperatorV2UploadStickerFont merchantShortId opCity requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "uploadStickerFont" API.uploadStickerFontAPI (API.uploadStickerFont ctx.gtfsId req)

transitOperatorV2DeleteStickerFont :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
transitOperatorV2DeleteStickerFont merchantShortId opCity fontId requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tmsUnit ctx "deleteStickerFont" API.deleteStickerFontAPI (API.deleteStickerFont ctx.gtfsId fontId)

transitOperatorV2GetStickerFontFile :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Lib.BmsBackend.Transit.Stickers.Types.StickerFile)
transitOperatorV2GetStickerFontFile merchantShortId opCity fontId requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "getStickerFontFile" API.getStickerFontFileAPI (API.getStickerFontFile ctx.gtfsId fontId)
