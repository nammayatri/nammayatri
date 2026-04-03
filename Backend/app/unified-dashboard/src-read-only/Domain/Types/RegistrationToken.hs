{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.RegistrationToken where
import Kernel.Prelude
import Data.Aeson
import Kernel.Utils.Dhall
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Kernel.Types.Beckn.Context
import qualified Domain.Types.Person
import qualified Data.Text
import qualified Tools.Beam.UtilsTH



data RegistrationToken
    = RegistrationToken {createdAt :: Kernel.Prelude.UTCTime,
                         enabled :: Kernel.Prelude.Bool,
                         id :: Kernel.Types.Id.Id Domain.Types.RegistrationToken.RegistrationToken,
                         merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                         operatingCity :: Kernel.Types.Beckn.Context.City,
                         personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
                         token :: Data.Text.Text,
                         updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



