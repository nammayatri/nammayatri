module Domain.Action.UI.CallFeedbackFCM where

import Data.Text hiding (map)
import qualified Domain.Types.CallStatus as CallStatus
import qualified Domain.Types.MerchantOperatingCity as MerchantOperatingCity
import EulerHS.Prelude hiding (id, map, state)
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error (GenericError (InternalError))
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common (CacheFlow, throwError)
import qualified Storage.Queries.CallFeedbackOptions as CallFeedbackOptions
import qualified Storage.Queries.Translations as Translations

data Options = Options
  { messageKey :: Text,
    message :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data CallFeedbackEntity = CallFeedbackEntity
  { callId :: Text,
    options :: [Options]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

makeCallFeedbackEntity :: (CacheFlow m r, EsqDBFlow m r) => Id.Id CallStatus.CallStatus -> Text -> Id.Id MerchantOperatingCity.MerchantOperatingCity -> Language -> m CallFeedbackEntity
makeCallFeedbackEntity callId category merchantOperatingCityId language = do
  options <- CallFeedbackOptions.findByCategoryAndMerchantOperatingCityId category (Just merchantOperatingCityId)
  optionsText <-
    mapM
      ( \option -> do
          mbTranslation <- Translations.findByErrorAndLanguage (option.messageKey) language
          case mbTranslation of
            Just translation -> return $ Options {messageKey = option.messageKey, message = translation.message}
            Nothing -> throwError $ InternalError ("Translation not found for message key: " <> option.messageKey)
      )
      options
  pure CallFeedbackEntity {callId = callId.getId, options = optionsText}
