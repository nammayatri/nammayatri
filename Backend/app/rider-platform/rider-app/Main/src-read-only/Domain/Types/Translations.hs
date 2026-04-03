{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.Translations where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Kernel.External.Types
import qualified Domain.Types.MerchantOperatingCity
import qualified Data.Text
import qualified Tools.Beam.UtilsTH



data Translations
    = Translations {createdAt :: Kernel.Prelude.UTCTime,
                    id :: Kernel.Types.Id.Id Domain.Types.Translations.Translations,
                    language :: Kernel.External.Types.Language,
                    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                    message :: Data.Text.Text,
                    messageKey :: Data.Text.Text,
                    updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



