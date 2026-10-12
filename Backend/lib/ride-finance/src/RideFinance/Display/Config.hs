module RideFinance.Display.Config
  ( VatClubRule (..),
    allComponents,
    clubPolicy,
    invoiceClubPolicy,
  )
where

import Data.List (find)
import qualified Data.Set as Set
import Kernel.Prelude
import RideFinance.Core.Components (allComponents)
import RideFinance.Core.Types

data VatClubRule = VatClubRule
  { issuedToType :: Maybe Text,
    invoiceType :: Maybe Text,
    components :: [FareComponent]
  }
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON, ToSchema)

clubPolicy :: Maybe [FareComponent] -> ClubPolicy
clubPolicy = Set.fromList . fromMaybe []

invoiceClubPolicy :: [VatClubRule] -> Text -> Text -> ClubPolicy
invoiceClubPolicy rules issuedTo invoiceKind =
  clubPolicy $ (.components) <$> find matches rules
  where
    matches rule = maybe True (== issuedTo) rule.issuedToType && maybe True (== invoiceKind) rule.invoiceType
