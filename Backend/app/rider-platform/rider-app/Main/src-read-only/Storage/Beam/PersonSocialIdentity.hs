{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PersonSocialIdentity where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.PersonSocialIdentity
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data PersonSocialIdentityT f = PersonSocialIdentityT
  { appleRefreshToken :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    clientId :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    email :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    id :: (B.C f Kernel.Prelude.Text),
    isPrivateEmail :: (B.C f Kernel.Prelude.Bool),
    issuer :: (B.C f Kernel.Prelude.Text),
    lastLoginAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    merchantId :: (B.C f Kernel.Prelude.Text),
    personId :: (B.C f Kernel.Prelude.Text),
    provider :: (B.C f Domain.Types.PersonSocialIdentity.SocialLoginProvider),
    providerSubject :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    validTill :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table PersonSocialIdentityT where
  data PrimaryKey PersonSocialIdentityT f = PersonSocialIdentityId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PersonSocialIdentityId . id

type PersonSocialIdentity = PersonSocialIdentityT Identity

$(enableKVPG (''PersonSocialIdentityT) [('id)] [[('personId)], [('providerSubject)]])

$(mkTableInstances (''PersonSocialIdentityT) "person_social_identity")
