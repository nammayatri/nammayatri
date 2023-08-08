module Screens.SubscriptionScreen.Transformer where

import Prelude

import Screens.Types (PlanCardConfig, PromoConfig)
import Services.API (GetCurrentPlanResp(..), OfferEntity(..), PlanEntity(..), UiPlansResp(..))

planListTransformer :: UiPlansResp -> Array PlanCardConfig
planListTransformer (UiPlansResp planResp) = do
    let planEntityArray = planResp.list
    map (\ (PlanEntity planEntity) -> {
    id : planEntity.id ,
    title : planEntity.name ,
    description : planEntity.description ,
    isSelected : false ,
    offers : getPromoConfig planEntity.offerList ,
    planPrice : planEntity.amount
    }) planEntityArray

getPromoConfig :: Array OfferEntity -> Array PromoConfig
getPromoConfig offerEntityArr = (map (\ (OfferEntity item) ->  {     
    title : item.title ,
    isGradient : false ,
    gradient : [] ,
    hasImage : false ,
    imageURL : "" ,
    offerDescription : item.description
    }) offerEntityArr)

myPlanListTransformer :: GetCurrentPlanResp -> PlanCardConfig
myPlanListTransformer (GetCurrentPlanResp getCurrentPlanResp) = do 
    let (PlanEntity planEntity) = getCurrentPlanResp.planEntity
    {   id : planEntity.id ,
        title : planEntity.name ,
        description : planEntity.description ,
        isSelected : false ,
        offers : getPromoConfig planEntity.offerList ,
        planPrice : planEntity.amount
    }