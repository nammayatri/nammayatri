module Screens.ExtraChargeInfoScreen.ComponentConfig where

import Prelude
import Effect
import PrestoDOM

import Components.PrimaryButton as PrimaryButton

import Screens.ExtraChargeInfoScreen.Controller
import Screens.ExtraChargeInfoScreen.ScreenData

import Resource.Localizable.StringsV2
import Resource.Localizable.TypesV2

import Data.Maybe
import RemoteConfig
import Services.API

readMoreQA :: String ->  Array ({question :: String, answer :: String})
readMoreQA lazy = [
    {
        question: getStringV2 extra_charge_q1,
        answer: getStringV2 extra_charge_a1
    },
    {
        question: getStringV2 extra_charge_q2,
        answer: getStringV2 extra_charge_a2
    },
    {
        question: getStringV2 extra_charge_q3,
        answer: getStringV2 extra_charge_a3
    },
    {
        question: getStringV2 extra_charge_q4,
        answer: getStringV2 extra_charge_a4
    }
]

gotItBtnConfig :: ExtraChargeInfoScreenState ->  PrimaryButton.Config
gotItBtnConfig state = PrimaryButton.config {
    textConfig {
        text = getStringV2 ok_got_it
    }
,   margin = Margin 16 24 16 0
}


getTagBasedVideo :: Maybe ExtraChargeVideoConfig -> Maybe GetDriverInfoResp ->  String
getTagBasedVideo mbConfig mbDriverInfoResp =
    case mbConfig, mbDriverInfoResp of
        Just config, Just (GetDriverInfoResp driverInfoResp) ->
            case driverInfoResp.driverTags >>= \(DriverTags tags) -> tags."DriverChargingBehaviour" of
                Just SuperOverCharging -> config.blocked
                Just HighOverCharging -> config.blocked
                Just MediumOverCharging -> config.suspended
                Just ModerateOverCharging -> config.high
                Just LowOverCharging -> config.low
                Just VeryLowOverCharging -> config.low
                Just NoOverCharging -> config.zero
                Nothing -> ""
        _,_ -> ""
