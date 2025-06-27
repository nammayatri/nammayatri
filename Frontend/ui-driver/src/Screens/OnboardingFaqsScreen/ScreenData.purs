{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.OnboardingFaqsScreen.ScreenData where

import Data.Maybe
import Prelude
import Screens.Types as ST
import ConfigProvider
import Common.Types.App (LazyCheck(..))
import Resource.Localizable.TypesV2 as LT2
import Resource.Localizable.StringsV2 (getStringV2)

initData :: ST.OnboardingFaqsScreenState
initData = {
  data: {
    categoryToQuestionAnsMap : dummyQuestions,
    selectedSectionQnAList : []
  },
  props: {
    selectedCategory : Nothing,
    selectedCategoryIndex : Nothing,
    showAns : false,
    selectedQnA : {question : "", answer : ""}
  }
} 

dummyQuestions :: Array ST.CategoryToQuestionAnsMap
dummyQuestions = [ 
    { category : getStringV2 LT2.onboarding_faq_setting_up_account
    , questionAnsMap : [
        { question : getStringV2 LT2.onboarding_faq_register_fleet_operator_q
        , answer : getStringV2 LT2.onboarding_faq_register_fleet_operator_a
        },
        { question : getStringV2 LT2.onboarding_faq_choose_language_q
        , answer : getStringV2 LT2.onboarding_faq_choose_language_a
        },
        { question : getStringV2 LT2.onboarding_faq_signup_email_q
        , answer : getStringV2 LT2.onboarding_faq_signup_email_a
        },
        { question : getStringV2 LT2.onboarding_faq_otp_issue_q
        , answer : getStringV2 LT2.onboarding_faq_otp_issue_a
        },
        { question : getStringV2 LT2.onboarding_faq_individual_business_fleet_q
        , answer : getStringV2 LT2.onboarding_faq_individual_business_fleet_a
        },
        { question : getStringV2 LT2.onboarding_faq_registration_details_q
        , answer : getStringV2 LT2.onboarding_faq_registration_details_a
        },
        { question : getStringV2 LT2.onboarding_faq_operating_cities_q
        , answer : getStringV2 LT2.onboarding_faq_operating_cities_a
        },
        { question : getStringV2 LT2.onboarding_faq_operator_referral_code_q
        , answer : getStringV2 LT2.onboarding_faq_operator_referral_code_a
        },
        { question : getStringV2 LT2.onboarding_faq_required_documents_q
        , answer : getStringV2 LT2.onboarding_faq_required_documents_a
        },
        { question : getStringV2 LT2.onboarding_faq_registration_assistance_q
        , answer : getStringV2 LT2.onboarding_faq_registration_assistance_a
        }
      ]
    },
    { category : getStringV2 LT2.onboarding_faq_managing_account
    , questionAnsMap : [
        { question : getStringV2 LT2.onboarding_faq_update_documents_q
        , answer : getStringV2 LT2.onboarding_faq_update_documents_a
        },
        { question : getStringV2 LT2.onboarding_faq_view_profile_q
        , answer : getStringV2 LT2.onboarding_faq_view_profile_a
        },
        { question : getStringV2 LT2.onboarding_faq_logout_q
        , answer : getStringV2 LT2.onboarding_faq_logout_a
        },
        { question : getStringV2 LT2.onboarding_faq_profile_picture_q
        , answer : getStringV2 LT2.onboarding_faq_profile_picture_a
        }
      ]
    },
    { category : getStringV2 LT2.onboarding_faq_adding_managing_drivers
    , questionAnsMap : [
        { question : getStringV2 LT2.onboarding_faq_msil_provide_drivers_q
        , answer : getStringV2 LT2.onboarding_faq_msil_provide_drivers_a
        },
        { question : getStringV2 LT2.onboarding_faq_add_multiple_drivers_q
        , answer : getStringV2 LT2.onboarding_faq_add_multiple_drivers_a
        },
        { question : getStringV2 LT2.onboarding_faq_onboard_driver_q
        , answer : getStringV2 LT2.onboarding_faq_onboard_driver_a
        },
        { question : getStringV2 LT2.onboarding_faq_add_driver_manually_q
        , answer : getStringV2 LT2.onboarding_faq_add_driver_manually_a
        },
        { question : getStringV2 LT2.onboarding_faq_driver_bulk_upload_q
        , answer : getStringV2 LT2.onboarding_faq_driver_bulk_upload_a
        },
        { question : getStringV2 LT2.onboarding_faq_driver_documents_q
        , answer : getStringV2 LT2.onboarding_faq_driver_documents_a
        },
        { question : getStringV2 LT2.onboarding_faq_driver_tab_q
        , answer : getStringV2 LT2.onboarding_faq_driver_tab_a
        },
        { question : getStringV2 LT2.onboarding_faq_track_driver_docs_q
        , answer : getStringV2 LT2.onboarding_faq_track_driver_docs_a
        },
        { question : getStringV2 LT2.onboarding_faq_add_remove_drivers_q
        , answer : getStringV2 LT2.onboarding_faq_add_remove_drivers_a
        },
        { question : getStringV2 LT2.onboarding_faq_manage_driver_docs_q
        , answer : getStringV2 LT2.onboarding_faq_manage_driver_docs_a
        }
      ]
    },
    { category : getStringV2 LT2.onboarding_faq_adding_managing_vehicles
    , questionAnsMap : [
        { question : getStringV2 LT2.onboarding_faq_onboard_vehicle_q
        , answer : getStringV2 LT2.onboarding_faq_onboard_vehicle_a
        },
        { question : getStringV2 LT2.onboarding_faq_add_multiple_vehicles_q
        , answer : getStringV2 LT2.onboarding_faq_add_multiple_vehicles_a
        },
        { question : getStringV2 LT2.onboarding_faq_vehicle_documents_q
        , answer : getStringV2 LT2.onboarding_faq_vehicle_documents_a
        },
        { question : getStringV2 LT2.onboarding_faq_readd_vehicle_q
        , answer : getStringV2 LT2.onboarding_faq_readd_vehicle_a
        },
        { question : getStringV2 LT2.onboarding_faq_vehicle_inspection_q
        , answer : getStringV2 LT2.onboarding_faq_vehicle_inspection_a
        },
        { question : getStringV2 LT2.onboarding_faq_inspection_cost_q
        , answer : getStringV2 LT2.onboarding_faq_inspection_cost_a
        },
        { question : getStringV2 LT2.onboarding_faq_manage_vehicle_docs_q
        , answer : getStringV2 LT2.onboarding_faq_manage_vehicle_docs_a
        },
        { question : getStringV2 LT2.onboarding_faq_track_vehicle_docs_q
        , answer : getStringV2 LT2.onboarding_faq_track_vehicle_docs_a
        }
      ]
    },
    { category : getStringV2 LT2.onboarding_faq_mapping_drivers_vehicles
    , questionAnsMap : [
        { question : getStringV2 LT2.onboarding_faq_assign_driver_vehicle_q
        , answer : getStringV2 LT2.onboarding_faq_assign_driver_vehicle_a
        },
        { question : getStringV2 LT2.onboarding_faq_remove_vehicle_q
        , answer : getStringV2 LT2.onboarding_faq_remove_vehicle_a
        },
        { question : getStringV2 LT2.onboarding_faq_edit_vehicle_q
        , answer : getStringV2 LT2.onboarding_faq_edit_vehicle_a
        },
        { question : getStringV2 LT2.onboarding_faq_update_driver_status_q
        , answer : getStringV2 LT2.onboarding_faq_update_driver_status_a
        }
      ]
    }
  ]