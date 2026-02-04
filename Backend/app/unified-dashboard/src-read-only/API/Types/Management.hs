{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Management where

import qualified API.Types.Management.AccessMatrix
import qualified API.Types.Management.Merchant
import qualified API.Types.Management.Person
import qualified API.Types.Management.Registration
import qualified API.Types.Management.Role
import qualified API.Types.Management.Transaction
import qualified Data.List
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude
import qualified Text.Read
import qualified Text.Show

data ManagementUserActionType
  = ACCESS_MATRIX API.Types.Management.AccessMatrix.AccessMatrixUserActionType
  | MERCHANT API.Types.Management.Merchant.MerchantUserActionType
  | PERSON API.Types.Management.Person.PersonUserActionType
  | REGISTRATION API.Types.Management.Registration.RegistrationUserActionType
  | ROLE API.Types.Management.Role.RoleUserActionType
  | TRANSACTION API.Types.Management.Transaction.TransactionUserActionType
  deriving stock (Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Text.Show.Show ManagementUserActionType where
  show = \case
    ACCESS_MATRIX e -> "ACCESS_MATRIX/" <> show e
    MERCHANT e -> "MERCHANT/" <> show e
    PERSON e -> "PERSON/" <> show e
    REGISTRATION e -> "REGISTRATION/" <> show e
    ROLE e -> "ROLE/" <> show e
    TRANSACTION e -> "TRANSACTION/" <> show e

instance Text.Read.Read ManagementUserActionType where
  readsPrec d' =
    Text.Read.readParen
      (d' > app_prec)
      ( \r ->
          [(ACCESS_MATRIX v1, r2) | r1 <- stripPrefix "ACCESS_MATRIX/" r, (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1]
            ++ [ ( MERCHANT v1,
                   r2
                 )
                 | r1 <- stripPrefix "MERCHANT/" r,
                   (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( PERSON v1,
                   r2
                 )
                 | r1 <- stripPrefix "PERSON/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( REGISTRATION v1,
                   r2
                 )
                 | r1 <- stripPrefix "REGISTRATION/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( ROLE v1,
                   r2
                 )
                 | r1 <- stripPrefix "ROLE/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( TRANSACTION v1,
                   r2
                 )
                 | r1 <- stripPrefix "TRANSACTION/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
      )
    where
      app_prec = 10
      stripPrefix pref r = bool [] [Data.List.drop (length pref) r] $ Data.List.isPrefixOf pref r

$(Data.Singletons.TH.genSingletons [''ManagementUserActionType])
