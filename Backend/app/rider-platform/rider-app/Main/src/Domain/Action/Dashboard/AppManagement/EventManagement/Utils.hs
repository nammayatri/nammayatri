module Domain.Action.Dashboard.AppManagement.EventManagement.Utils where

import qualified API.Types.Dashboard.AppManagement.TicketDashboard
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.TicketPlace
import qualified Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.Flow as Storage
import qualified Storage.Queries.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.RiderConfig as QRC
import Storage.Types (FileType (..))
import System.IO (hFileSize)
import Tools.Error

uploadAssetHelper :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> FilePath -> Text -> Environment.Flow API.Types.Dashboard.AppManagement.TicketDashboard.UploadPublicFileResponse)
uploadAssetHelper _merchantShortId _opCity ticketPlaceId file reqContentType = do
  fileSize <- L.runIO $ withFile file ReadMode hFileSize
  fileExtension <- getExtension reqContentType
  when (fileSize > 1000000) $
    throwError $ FileSizeExceededError ("File size " <> show fileSize <> " exceeds the limit of 1 MB")
  documentFile <- L.runIO $ BS.readFile file
  uuid <- generateGUID
  m <- findMerchantByShortId _merchantShortId
  moCity <- CQMOC.findByMerchantIdAndCity m.id m.defaultCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> _merchantShortId.getShortId <> " ,city: " <> show m.defaultCity)
  riderConfig <- QRC.findByMerchantOperatingCityId moCity.id >>= fromMaybeM (RiderConfigNotFound $ "merchantShortId: " <> _merchantShortId.getShortId <> " ,city: " <> show m.defaultCity)
  filePath <- Storage.createFilePublicPath "ticket-assets" ("ticket-place/" <> ticketPlaceId.getId) uuid fileExtension
  logDebug $ "filePath: " <> show filePath
  _ <- fork "Storage put file" $ Storage.putRaw (T.unpack filePath) documentFile (T.unpack reqContentType)
  let url = fromMaybe mempty (riderConfig.ticketAssetDomain) <> filePath
  return $ API.Types.Dashboard.AppManagement.TicketDashboard.UploadPublicFileResponse {publicUrl = url}

deleteAssetHelper :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.TicketDashboard.DeletePublicFileRequest -> Environment.Flow ()
deleteAssetHelper _merchantShortId _opCity req = do
  m <- findMerchantByShortId _merchantShortId
  moCity <- CQMOC.findByMerchantIdAndCity m.id m.defaultCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> _merchantShortId.getShortId <> " ,city: " <> show m.defaultCity)
  deleteAsset moCity.id req

deleteAsset :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> API.Types.Dashboard.AppManagement.TicketDashboard.DeletePublicFileRequest -> Environment.Flow ()
deleteAsset moCityId req = do
  riderConfig <- QRC.findByMerchantOperatingCityId moCityId >>= fromMaybeM (RiderConfigNotFound $ "merchantOperatingCityId: " <> show moCityId)
  let filePath = fromMaybe req.assetId $ T.stripPrefix (fromMaybe mempty (riderConfig.ticketAssetDomain)) req.assetId
  void $ fork "Storage delete file" $ Storage.delete (T.unpack filePath)

getExtension :: Text -> Environment.Flow Text
getExtension = \case
  "image/jpeg" -> return ".jpg"
  "image/png" -> return ".png"
  _ -> throwError $ InvalidRequest "Invalid file type"
