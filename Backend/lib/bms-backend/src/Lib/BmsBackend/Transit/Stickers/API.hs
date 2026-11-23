module Lib.BmsBackend.Transit.Stickers.API where

import qualified EulerHS.Types as ET
import Kernel.Prelude
import Lib.BmsBackend.Transit.Common
import Lib.BmsBackend.Transit.Stickers.Types
import Servant

type ListStickerDesignsAPI =
  Transit ("sticker-designs" :> QueryParam "search" Text :> QueryParam "organizationId" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> Scoped (Get '[JSON] (ListResponse StickerDesignSummary)))

listStickerDesignsAPI :: Proxy ListStickerDesignsAPI
listStickerDesignsAPI = Proxy

listStickerDesigns :: Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> ET.EulerClient (ListResponse StickerDesignSummary)
listStickerDesigns = ET.client listStickerDesignsAPI

type CreateStickerDesignAPI =
  Transit ("sticker-designs" :> ReqBody '[JSON] StickerDesignCreate :> Scoped (Post '[JSON] StickerDesign))

createStickerDesignAPI :: Proxy CreateStickerDesignAPI
createStickerDesignAPI = Proxy

createStickerDesign :: Text -> StickerDesignCreate -> Maybe Text -> Maybe Text -> ET.EulerClient StickerDesign
createStickerDesign = ET.client createStickerDesignAPI

type PreviewStickerAPI =
  Transit ("sticker-designs" :> "preview" :> ReqBody '[JSON] StickerPreviewReq :> Scoped (Post '[JSON] StickerFile))

previewStickerAPI :: Proxy PreviewStickerAPI
previewStickerAPI = Proxy

previewSticker :: Text -> StickerPreviewReq -> Maybe Text -> Maybe Text -> ET.EulerClient StickerFile
previewSticker = ET.client previewStickerAPI

type GetStickerSigningAPI =
  Transit ("sticker-designs" :> "signing" :> Scoped (Get '[JSON] StickerSigning))

getStickerSigningAPI :: Proxy GetStickerSigningAPI
getStickerSigningAPI = Proxy

getStickerSigning :: Text -> Maybe Text -> Maybe Text -> ET.EulerClient StickerSigning
getStickerSigning = ET.client getStickerSigningAPI

type ReadFleetFileAPI =
  Transit ("sticker-designs" :> "fleet-file" :> ReqBody '[JSON] FleetFileReq :> Scoped (Post '[JSON] FleetFileRead))

readFleetFileAPI :: Proxy ReadFleetFileAPI
readFleetFileAPI = Proxy

readFleetFile :: Text -> FleetFileReq -> Maybe Text -> Maybe Text -> ET.EulerClient FleetFileRead
readFleetFile = ET.client readFleetFileAPI

type GetStickerDesignAPI =
  Transit ("sticker-designs" :> Capture "id" Text :> Scoped (Get '[JSON] StickerDesign))

getStickerDesignAPI :: Proxy GetStickerDesignAPI
getStickerDesignAPI = Proxy

getStickerDesign :: Text -> Text -> Maybe Text -> Maybe Text -> ET.EulerClient StickerDesign
getStickerDesign = ET.client getStickerDesignAPI

type UpdateStickerDesignAPI =
  Transit ("sticker-designs" :> Capture "id" Text :> ReqBody '[JSON] StickerDesignUpdate :> Scoped (Put '[JSON] StickerDesign))

updateStickerDesignAPI :: Proxy UpdateStickerDesignAPI
updateStickerDesignAPI = Proxy

updateStickerDesign :: Text -> Text -> StickerDesignUpdate -> Maybe Text -> Maybe Text -> ET.EulerClient StickerDesign
updateStickerDesign = ET.client updateStickerDesignAPI

type DeleteStickerDesignAPI =
  Transit ("sticker-designs" :> Capture "id" Text :> Scoped (Verb 'DELETE 204 '[JSON] NoContent))

deleteStickerDesignAPI :: Proxy DeleteStickerDesignAPI
deleteStickerDesignAPI = Proxy

deleteStickerDesign :: Text -> Text -> Maybe Text -> Maybe Text -> ET.EulerClient NoContent
deleteStickerDesign = ET.client deleteStickerDesignAPI

type GetStickerBackgroundAPI =
  Transit ("sticker-designs" :> Capture "id" Text :> "background" :> Scoped (Get '[JSON] StickerFile))

getStickerBackgroundAPI :: Proxy GetStickerBackgroundAPI
getStickerBackgroundAPI = Proxy

getStickerBackground :: Text -> Text -> Maybe Text -> Maybe Text -> ET.EulerClient StickerFile
getStickerBackground = ET.client getStickerBackgroundAPI

type DuplicateStickerDesignAPI =
  Transit ("sticker-designs" :> Capture "id" Text :> "duplicate" :> Scoped (Post '[JSON] StickerDesign))

duplicateStickerDesignAPI :: Proxy DuplicateStickerDesignAPI
duplicateStickerDesignAPI = Proxy

duplicateStickerDesign :: Text -> Text -> Maybe Text -> Maybe Text -> ET.EulerClient StickerDesign
duplicateStickerDesign = ET.client duplicateStickerDesignAPI

type GenerateStickersAPI =
  Transit ("sticker-designs" :> Capture "id" Text :> "generate" :> ReqBody '[JSON] StickerGenerateReq :> Scoped (Post '[JSON] StickerFile))

generateStickersAPI :: Proxy GenerateStickersAPI
generateStickersAPI = Proxy

generateStickers :: Text -> Text -> StickerGenerateReq -> Maybe Text -> Maybe Text -> ET.EulerClient StickerFile
generateStickers = ET.client generateStickersAPI

type ListStickerBatchesAPI =
  Transit ("sticker-designs" :> Capture "id" Text :> "batches" :> Scoped (Get '[JSON] [StickerBatch]))

listStickerBatchesAPI :: Proxy ListStickerBatchesAPI
listStickerBatchesAPI = Proxy

listStickerBatches :: Text -> Text -> Maybe Text -> Maybe Text -> ET.EulerClient [StickerBatch]
listStickerBatches = ET.client listStickerBatchesAPI

type ListStickerFontsAPI =
  Transit ("sticker-fonts" :> QueryParam "organizationId" Text :> Scoped (Get '[JSON] [StickerFont]))

listStickerFontsAPI :: Proxy ListStickerFontsAPI
listStickerFontsAPI = Proxy

listStickerFonts :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> ET.EulerClient [StickerFont]
listStickerFonts = ET.client listStickerFontsAPI

type UploadStickerFontAPI =
  Transit ("sticker-fonts" :> ReqBody '[JSON] StickerFontUpload :> Scoped (Post '[JSON] StickerFont))

uploadStickerFontAPI :: Proxy UploadStickerFontAPI
uploadStickerFontAPI = Proxy

uploadStickerFont :: Text -> StickerFontUpload -> Maybe Text -> Maybe Text -> ET.EulerClient StickerFont
uploadStickerFont = ET.client uploadStickerFontAPI

type DeleteStickerFontAPI =
  Transit ("sticker-fonts" :> Capture "id" Text :> Scoped (Verb 'DELETE 204 '[JSON] NoContent))

deleteStickerFontAPI :: Proxy DeleteStickerFontAPI
deleteStickerFontAPI = Proxy

deleteStickerFont :: Text -> Text -> Maybe Text -> Maybe Text -> ET.EulerClient NoContent
deleteStickerFont = ET.client deleteStickerFontAPI

type GetStickerFontFileAPI =
  Transit ("sticker-fonts" :> Capture "id" Text :> "file" :> Scoped (Get '[JSON] StickerFile))

getStickerFontFileAPI :: Proxy GetStickerFontFileAPI
getStickerFontFileAPI = Proxy

getStickerFontFile :: Text -> Text -> Maybe Text -> Maybe Text -> ET.EulerClient StickerFile
getStickerFontFile = ET.client getStickerFontFileAPI
