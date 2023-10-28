{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Styles.Colors where

import Styles.Types

import MerchantConfig.Utils (getValueFromConfig)

-- Text Color

textListItemTitle :: Color
textListItemTitle = "#666666"

textListItemDescription :: Color
textListItemDescription = "#999999"

textDanger :: Color
textDanger = "#fa6259"

greyBackDarkColor :: Color
greyBackDarkColor = "#D3D3D3"

blueTextColor :: Color
blueTextColor = "#2172FF"

greyTextColor :: Color
greyTextColor = "#222222"

textWarning :: Color
textWarning = "#FEA01E"

textSuccess :: Color
textSuccess = "#33bf65"

lightBlue :: Color
lightBlue = "#E0EAFD"

lightBlue1 :: Color
lightBlue1 = "#EDF8F9"

darkBlue1 :: Color
darkBlue1 = "#1D629B"

lightOrange2 :: Color
lightOrange2 = "#F76F6F"

selectLightBlue :: Color
selectLightBlue = "#80E0EAFD"

hintColor :: Color
hintColor = "#3E2D2D2D"

darkGreen :: Color
darkGreen = "#1F9649"

darkBlue :: Color
darkBlue = "#2172FF"

whiteBlue :: Color
whiteBlue = "#3f2172FF"

blackBlurColor :: Color
blackBlurColor = "#8B8B8B"
-- Component Background
landingDisableBG :: Color
landingDisableBG = "#1A2d2d2d"

greyBG :: Color
greyBG = "#F8F8F8"

primaryBG :: Color
primaryBG = "#204480"

primaryBGDisable :: Color
primaryBGDisable = "#BAC5D8"


black :: Color
black = "#000000"

greyLight :: Color
greyLight = "#ECECEC"

orange :: Color
orange = "#FF9F07"

lightOrange :: Color
lightOrange = "#16FFBD3E"

redDark :: Color
redDark = "#AE285D"

lightRed :: Color
lightRed = "#16D76893"

blueBack :: Color
blueBack = "#F6FAFF"

greyDark :: Color
greyDark = "#D8D8D8"

greyDarker :: Color
greyDarker = "#D6D6D6"

borderListItem :: Color
borderListItem = "#33484848"

borderColorLight :: Color
borderColorLight = "#e4e4e4"

notificationColor :: Color
notificationColor = "#F3454545"

paymentPageBack :: Color
paymentPageBack = "#f8f9f9"

loanOffersBack :: Color
loanOffersBack  = "#7FBCFF"

activeLoansBack :: Color
activeLoansBack = "#89EBA5"

pendingLoansBack :: Color
pendingLoansBack = "#FFD07F"

navMenuBodyText :: Color
navMenuBodyText = "#02346B"

invoiceStatusA :: Color
invoiceStatusA = "#27BC5C"

invoiceStatusBackA :: Color
invoiceStatusBackA = "#1A27BC5C"

editTextBorder :: Color
editTextBorder = "#DEDEDE"

backSuccess :: Color
backSuccess = "#dff5e7"

backSuccessLight :: Color
backSuccessLight = "#E9F8EE"

backDanger :: Color
backDanger = "#FCEDED"

selectedItemBG :: Color
selectedItemBG = "#F4F6F9"

selectedItemDarkBG :: Color
selectedItemDarkBG = "#4DDADADA"

darkDescriptionText :: Color
darkDescriptionText = "#333333"

lightGreen :: Color
lightGreen = "#68D08D"

incorrectAlert :: Color
incorrectAlert  = "#F76F6F"

darkMint :: Color
darkMint = "#53bb6f"

charcoalGrey :: Color
charcoalGrey = "#323643"

yellowText :: Color
yellowText = "#FDD836"

tripDetailTime :: Color
tripDetailTime = "#313644"

primaryButtonColor :: Color
primaryButtonColor = "#FDD836"

paymentDescriptionColor :: Color
paymentDescriptionColor = "#363636"

blackBlue1 :: Color
blackBlue1 = "#2b1e94"

primaryBlackColor :: Color
primaryBlackColor = "#373e4c"

destinationBackColor :: Color
destinationBackColor = "#f5f6f7"

primaryBlue :: Color
primaryBlue = "#3d64f4"

primaryBlueInactive :: Color
primaryBlueInactive = "#4d3d64f4"

blueViolet :: Color
blueViolet = "#6200ee"

blueViolet38 :: Color
blueViolet38 = "#C39EF9"

blueGrey :: Color
blueGrey = "#9899a1"

filterBorder :: Color
filterBorder = "#cbcbcf"

salmon :: Color
salmon = "#f88080"

brownishGrey :: Color
brownishGrey = "#606060"

destinationTextColor :: Color
destinationTextColor = "#3D64f4"

tripTitleSecondColor :: Color
tripTitleSecondColor = "#606060"

dropDownListTitleColor :: Color
dropDownListTitleColor = "#989aa1"

filterButtonBGColor :: Color
filterButtonBGColor = "#0a0a548c"

filterDisableButtonColor :: Color
filterDisableButtonColor = "#707070"

tripDetailsgrey :: Color
tripDetailsgrey = "#989aa1"

greyWhiteColor :: Color
greyWhiteColor = "#33ffffff"

lightGrey :: Color
lightGrey = "#E1E1E1"

charcoalReyBorder :: Color
charcoalReyBorder = "#1e323643"

warningRed :: Color
warningRed  = "#f04c31"

leftSizeBlack :: Color
leftSizeBlack = "#212121"

profileNameYellow :: Color
profileNameYellow = "#fdd836"

profilePhoneNumber :: Color
profilePhoneNumber = "#c8ced2"

accountName :: Color
accountName = "#3a3e4d"

navBarActive :: Color
navBarActive = "#323643"

helpButtonBorder :: Color
helpButtonBorder = "#2172FF"

navBarActiveAlpha :: Color
navBarActiveAlpha = "#33323643"

greyAlpha :: Color
greyAlpha = "#0A222222"

border_color :: Color
border_color = "#CED0DA"

borderGreyColor :: Color
borderGreyColor = "#D9D9D9"

borderMainColor :: Color
borderMainColor = "#979797"

bigTextColor :: Color
bigTextColor = "#565656"

smallTextColor :: Color
smallTextColor = "#5A645D"

greenCircleColor :: Color
greenCircleColor = "#2CBC1D"

redCircleColor :: Color
redCircleColor = "#E94040"

black800 :: Color
black800 = getValueFromConfig "black800"

black900 :: Color
black900 = getValueFromConfig "black900"

blackLightGrey :: Color
blackLightGrey = "#949494"

catskillWhite :: Color
catskillWhite = "#F7F9FB"

yellowRadler :: Color
yellowRadler = "#FFD058"

black700 :: Color
black700 = "#6D7280"

blue900 :: Color
blue900 = "#0066FF"

lightGreyShade :: Color
lightGreyShade = "#DDDDDD"

greyShade :: Color
greyShade =  "#777777"

greyishBlue :: Color
greyishBlue = "#738299"

whiteFloral :: Color
whiteFloral = "#F8F5EE"

grey900 :: Color
grey900 = "#E5E7EB"

blackLessTrans :: Color
blackLessTrans = "#99000000"

black9000 :: Color
black9000 = "#CC2C2F3A"

yellow900 :: Color
yellow900 = "#ffffff"

blue600 :: Color
blue600 = "#F4F7FF"

purplePantone :: Color
purplePantone = "#5F259F"

whiteSmoke :: Color
whiteSmoke = "#F6F6F6"

greySmoke :: Color
greySmoke = "#E6E6E6"

grey800 :: Color
grey800 = "#F1F1F1"

purpleSirocco :: Color
purpleSirocco = "#788889"

greyDavy :: Color
greyDavy = "#555555"

pinkPantone :: Color
pinkPantone = "#f5c6c6"

red :: Color
red = getValueFromConfig "red"

yellow800 :: Color
yellow800 = "#FEEBB9"

black600 :: Color
black600 = "#A7A7A7"

lightWhite :: Color
lightWhite = "#f7f5f5"

grey92 :: Color
grey92 = "#EBEBEB"
-- ================================================================================

mainPrimary :: Color
mainPrimary = "#3D64F4"

textPrimary :: Color
textPrimary = "#323643"

buttonTextPrimary :: Color
buttonTextPrimary = "#FDD836"

textSecondary :: Color
textSecondary = "#5B5E69"

primaryButtonBg :: Color
primaryButtonBg = "#323643"

mainBlack :: Color
mainBlack = "#323643"

white900 :: Color
white900 = "#FFFFFF"

warning :: Color
warning = "#F2A673"

positive :: Color
positive = "#53BB6F"

negative :: Color
negative = "#F88080"

inactive :: Color
inactive = "#989BA1"

inactiveButtonTextPrimary :: Color
inactiveButtonTextPrimary = "#FEF2BD"

hintText :: Color
hintText =  "#E5E7E8"

transparent :: Color
transparent = "#00FFFFFF"

transparentGrey :: Color
transparentGrey = "#66888888"

blue800 :: Color
blue800 = "#2194FF"

golden :: Color
golden = "#E9BE4D"

cancelledBg :: Color
cancelledBg = "#17E55454"

lightBlack900 :: Color
lightBlack900 = "#992C2F3A"

black500 :: Color
black500 = "#B9BABE"

mint :: Color
mint = "#1A53BB6F"

lightMaroon :: Color
lightMaroon = "#E45454"
blue700' :: Color
blue700' = "#80B2FF"

blue600' :: Color
blue600' = "#F4F7FF"

grey :: Color
grey = "#D0D5DD"

orange800 :: Color
orange800 = "#FFA666"
grey700 :: Color
grey700 = "#F4F4F6"

black650 :: Color
black650 = "#868B98"

green200 :: Color
green200 = "#2053BB6F"

linearGradient :: Array (Array Color)
linearGradient = [
  ["#26FE2D" , "#79F400", "#A6E900", "#C8DD00", "#E6CF00", "#FFBF00", "#FFAE00", "#FF9C05", "#FF8A37", "#FF7755", "#FF6670", "#FF598B", "#FF51A5", "#FF51BE", "#FF57D6", "#FF62EC", "#F66EFF"]
  , ["#26FE2D" , "#2AFE58", "#2FFE81", "#33FEA8", "#38FECE", "#3CFEF3", "#41E7FE", "#45C6FF", "#4AA7FF", "#4E89FF", "#536DFF", "#5C57FF", "#7E5CFF",          "#9F60FF", "#BD65FF", "#DB69FF", "#F66EFF"]
  , ["#26FE2D", "#33F53A", "#40EC47" , "#4DE354", "#5ADA62", "#67D16F", "#74C87C", "#81BF89", "#8EB696", "#9BADA3", "#A8A4B0", "#B59BBD", "#C292CB", "#CF89D8", "#DC80E5", "#E977F2", "#F66EFF"]
  , ["#FE4F95", "#F2529C", "#E654A2", "#DA57A9", "#CE59B0", "#C25CB6", "#B65EBD", "#AA61C3", "#9E63CA", "#9166D1", "#8568D7", "#796BDE", "#6D6DE5", "#6170EB" , "#5572F2" , "#4975F8", "#3D77FF"]
  , ["#D075FE", "#D17EF5", "#D286EB", "#D38FE2", "#D498D8", "#D5A0CF", "#D6A9C5", "#D7B1BC", "#D8BAB2", "#D9C3A9", "#DACB9F", "#DBD496", "#DCDD8C", "#DDE583" , "#DEEE79", "#DFF670", "#E0FF66"]
  , ["#ADB2FE", "#9CBAFF", "#87C3FF", "#6DCCFF", "#4DD4FF", "#18DCFF", "#00E3FF" , "#00EAFF", "#00EFF4", "#00F5E4", "#00F9D2", "#33FDBF", "#5FFFAB", "#82FF97", "#A2FF85", "#C1FF74", "#E0FF66"]
]
green900 :: Color
green900 = "#53BB6F"

lightMintGreen :: Color
lightMintGreen = "#F0FAF0"

elfGreen :: Color
elfGreen = "#269574"

transparentBlue :: Color
transparentBlue = "#100066FF"

pink :: Color
pink = "#FEF1E5"

pickledBlue :: Color 
pickledBlue = "#29334A"

aliceBlue :: Color 
aliceBlue = "#F1F3F4"
