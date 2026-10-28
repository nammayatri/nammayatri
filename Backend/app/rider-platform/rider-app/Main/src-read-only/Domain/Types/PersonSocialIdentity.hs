{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PersonSocialIdentity where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data PersonSocialIdentity = PersonSocialIdentity
  { appleRefreshToken :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedField 'Kernel.External.Encryption.AsEncrypted Kernel.Prelude.Text),
    clientId :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    email :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedField 'Kernel.External.Encryption.AsEncrypted Kernel.Prelude.Text),
    id :: Kernel.Types.Id.Id Domain.Types.PersonSocialIdentity.PersonSocialIdentity,
    isPrivateEmail :: Kernel.Prelude.Bool,
    issuer :: Kernel.Prelude.Text,
    lastLoginAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    provider :: Domain.Types.PersonSocialIdentity.SocialLoginProvider,
    providerSubject :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    validTill :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, (Show))

data SocialLoginProvider = GOOGLE | APPLE deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''SocialLoginProvider))
