module SharedLogic.DriverOnboarding.DocumentFileValidation
  ( validateFileExtension,
    sanitizeExtension,
    extensionToText,
    parseFileExtension,
  )
where

import qualified Data.Text as T
import Domain.Types.DocumentVerificationConfig (SupportedFileExtension (..))
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common

extensionToText :: SupportedFileExtension -> Text
extensionToText JPG = "jpg"
extensionToText JPEG = "jpeg"
extensionToText PNG = "png"
extensionToText PDF = "pdf"
extensionToText WEBP = "webp"

parseFileExtension :: Text -> Maybe SupportedFileExtension
parseFileExtension raw = case T.toLower (T.strip (T.dropWhile (== '.') raw)) of
  "jpg" -> Just JPG
  "jpeg" -> Just JPEG
  "png" -> Just PNG
  "pdf" -> Just PDF
  "webp" -> Just WEBP
  _ -> Nothing

sanitizeExtension :: Maybe Text -> Text
sanitizeExtension = \case
  Nothing -> "png"
  Just ext ->
    let t = T.toLower $ T.strip $ T.dropWhile (== '.') ext
     in if T.null t then "png" else t

validateFileExtension ::
  (MonadThrow m, Log m) =>
  Maybe [SupportedFileExtension] ->
  Maybe Text ->
  m SupportedFileExtension
validateFileExtension mbAllowedExts mbRawExt = do
  let sanitized = sanitizeExtension mbRawExt
  ext <- fromMaybeM (InvalidRequest $ "Unsupported file extension: ." <> sanitized) $ parseFileExtension sanitized
  case mbAllowedExts of
    Nothing -> pure ext -- Backward Compatibility: If no config is set, allow all extensions
    Just allowed ->
      if ext `elem` allowed
        then pure ext
        else
          throwError $
            InvalidRequest $
              "File extension ."
                <> extensionToText ext
                <> " not allowed for this document. Supported: "
                <> T.intercalate ", " (map (("." <>) . extensionToText) allowed)
