{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Provider where

import qualified API.Types.Provider.Person
import qualified Data.List
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude
import qualified Text.Read
import qualified Text.Show

newtype ProviderUserActionType
  = PERSON API.Types.Provider.Person.PersonUserActionType
  deriving stock (Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Text.Show.Show ProviderUserActionType where
  show = \case
    PERSON e -> "PERSON/" <> show e

instance Text.Read.Read ProviderUserActionType where
  readsPrec d' = Text.Read.readParen (d' > app_prec) (\r -> [(PERSON v1, r2) | r1 <- stripPrefix "PERSON/" r, (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1])
    where
      app_prec = 10
      stripPrefix pref r = bool [] [Data.List.drop (length pref) r] $ Data.List.isPrefixOf pref r

$(Data.Singletons.TH.genSingletons [(''ProviderUserActionType)])
