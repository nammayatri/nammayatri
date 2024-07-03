{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.OnDemand.Utils.Search where

import BecknV2.OnDemand.Tags as Tag
import qualified BecknV2.OnDemand.Types as Spec
import qualified Data.Aeson as A
import qualified Data.List as List
import EulerHS.Prelude hiding (id)
import qualified Tools.Maps as Maps

mkCustomerInfoTags :: Maybe Maps.Language -> Maybe Text -> Maybe Text -> Bool -> Maybe [Spec.TagGroup]
mkCustomerInfoTags Nothing Nothing _ False = Nothing
mkCustomerInfoTags customerLanguage disabilityTag _ isDashboard =
  Just
    [ Spec.TagGroup
        { tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just $ show Tag.CUSTOMER_INFO,
                  descriptorName = Just "Customer Information",
                  descriptorShortDesc = Nothing
                },
          tagGroupDisplay = Just False,
          tagGroupList = customerLanguageSingleton customerLanguage <> disabilityTagSingleton disabilityTag <> dashboardUserSingleton
        }
    ]
  where
    customerLanguageSingleton Nothing = Nothing
    customerLanguageSingleton (Just language) =
      Just $
        List.singleton $
          Spec.Tag
            { tagDescriptor =
                Just $
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tag.CUSTOMER_LANGUAGE,
                      descriptorName = Just "Customer Language",
                      descriptorShortDesc = Nothing
                    },
              tagDisplay = Just False,
              tagValue = (Just . show) language
            }
    dashboardUserSingleton =
      Just $
        List.singleton $
          Spec.Tag
            { tagDescriptor =
                Just $
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tag.DASHBOARD_USER,
                      descriptorName = Just "Dashboard User",
                      descriptorShortDesc = Nothing
                    },
              tagDisplay = Just False,
              tagValue = (Just . show) isDashboard
            }
    disabilityTagSingleton Nothing = Nothing
    disabilityTagSingleton (Just disability) =
      Just $
        List.singleton $
          Spec.Tag
            { tagDescriptor =
                Just $
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tag.CUSTOMER_DISABILITY,
                      descriptorName = Just "Customer Disability",
                      descriptorShortDesc = Nothing
                    },
              tagDisplay = Just False,
              tagValue = (A.decode . A.encode) disability
            }

mkReallocationInfoTags :: Maybe Bool -> Maybe [Spec.TagGroup]
mkReallocationInfoTags Nothing = Nothing
mkReallocationInfoTags (Just isReallocationEnabled) =
  Just
    [ Spec.TagGroup
        { tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just $ show Tag.REALLOCATION_INFO,
                  descriptorName = Just "Reallocation Information",
                  descriptorShortDesc = Nothing
                },
          tagGroupDisplay = Just False,
          tagGroupList =
            Just
              [ Spec.Tag
                  { tagDescriptor =
                      Just $
                        Spec.Descriptor
                          { descriptorCode = Just $ show Tag.IS_REALLOCATION_ENABLED,
                            descriptorName = Just "Is Reallocation Enabled",
                            descriptorShortDesc = Nothing
                          },
                    tagDisplay = Just False,
                    tagValue = Just $ show isReallocationEnabled
                  }
              ]
        }
    ]
