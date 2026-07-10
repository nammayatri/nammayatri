-- Single source of truth for the dashboard-common <-> domain AddressDocumentType cast.
-- Kept in a leaf module (only type imports, no action-module deps) so every caller can share it
-- without import cycles — Fleet.RegistrationV2 cannot import the Management.Driver action module.
module Domain.Action.Dashboard.Common.AddressDocumentType
  ( castToCommon,
    castFromCommon,
  )
where

import qualified Dashboard.Common.Driver as Common
import qualified Domain.Types.DriverInformation as DrInfo

castToCommon :: DrInfo.AddressDocumentType -> Common.AddressDocumentType
castToCommon = \case
  DrInfo.RationCard -> Common.RationCard
  DrInfo.UtilityBill -> Common.UtilityBill
  DrInfo.Passport -> Common.Passport
  DrInfo.VoterId -> Common.VoterId
  DrInfo.LifeInsurancePolicy -> Common.LifeInsurancePolicy
  DrInfo.Others -> Common.Others

castFromCommon :: Common.AddressDocumentType -> DrInfo.AddressDocumentType
castFromCommon = \case
  Common.RationCard -> DrInfo.RationCard
  Common.UtilityBill -> DrInfo.UtilityBill
  Common.Passport -> DrInfo.Passport
  Common.VoterId -> DrInfo.VoterId
  Common.LifeInsurancePolicy -> DrInfo.LifeInsurancePolicy
  Common.Others -> DrInfo.Others
