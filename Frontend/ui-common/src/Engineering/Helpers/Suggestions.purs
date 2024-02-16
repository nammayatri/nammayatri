{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Engineering.Helpers.Suggestions where

import Data.Array (filter, head, concatMap)
import Prelude ((==))
import Data.Maybe (Maybe(..), fromMaybe)
import JBridge (getSuggestionfromKey, getSuggestionsfromLocal)

suggestionsDefinitions ∷ String -> SuggestionDefinitions
suggestionsDefinitions dummy =
  [ { key: "cis1AP", value: { en_us: "At pick-up, Where are you ?", ta_in: "பிக்-அப்பில் உள்ளேன், நீங்க?", kn_in: "ಪಿಕ್-ಅಪ್‌ನಲ್ಲಿದ್ದೇನೆ  ನೀವು ಎಲ್ಲಿದ್ದೀರಿ?", hi_in: "मैं लोकेशन पे हूं , आप कहा हो ?", ml_in: "ഞാൻ പിക്ക്-അപ്പിൽ ആണ്, താങ്കൾ എവിടെ?", bn_in: "আমি পিক-আপ স্থানে আছি, আপনি কোথায়?" } }
  , { key: "cis2AP", value: { en_us: "Reaching shortly", ta_in: "விரைவில் வருகிறேன்", kn_in: "ಶೀಘ್ರದಲ್ಲೇ ತಲುಪುತ್ತೇನೆ", hi_in: "मैं थोड़ी देर में पहुँच रहा हूँ ", ml_in: "ഉടൻ എത്തും", bn_in: "আমি শীঘ্রই পৌঁছাচ্ছি" } }
  , { key: "cis3AP", value: { en_us: "Call me, You're unreachable", ta_in: "அணுக முடியவில்லை. கால் செய்யுங்கள்", kn_in: "ಕರೆ ಮಾಡಿ, ನೀವು ತಲುಪಲು ಸಾಧ್ಯವಿಲ್ಲ ", hi_in: "आपका फ़ोन नहीं लग रहा, कॉल कीजिये", ml_in: "എന്നെ വിളിക്കൂ, താങ്കൾ പരിധിക്കു പുറത്താണ്", bn_in: "আমাকে কল করুন, আপনার ফোনে যোগাযোগ করা যাচ্ছে না" } }
  , { key: "cis1BP", value: { en_us: "Are you starting ?", ta_in: "நீங்கள் தொடங்குகிறீர்களா?", kn_in: "ನೀವು ಪ್ರಾರಂಭಿಸುತ್ತಿದ್ದೀರಾ?", hi_in: "आप आ रहे है क्या? ", ml_in: "താങ്കൾ പുറപ്പെടുകയാണോ?", bn_in: "আপনি কি আসছেন ?" } }
  , { key: "cis2BP", value: { en_us: "Urgent, come soon", ta_in: "அவசரம், சீக்கிரம் வாருங்கள்", kn_in: "ದಯವಿಟ್ಟು ಬೇಗ ಬನ್ನಿ ", hi_in: "अर्जेंट है प्लीज जल्दी आइये", ml_in: "അത്യാവശ്യമാണ്, വേഗം വരൂ", bn_in: "জরুরী আছে, দয়া করে তাড়াতাড়ি আসুন" } }
  , { key: "cis3BP", value: { en_us: "Waiting at Pick-up", ta_in: "பிக்-அப்பில்  காத்திருக்கிறேன்", kn_in: "ಪಿಕ್-ಅಪ್‌ನಲ್ಲಿ ಕಾಯುತ್ತಿದ್ದೇನೆ", hi_in: "मैं लोकेशन पे इंतजार कर रहा हूँ ", ml_in: "ഞാൻ പിക്ക്-അപ്പിൽ കാത്തിരിക്കുകയാണ്", bn_in: "আমি পিক-আপ স্থানে অপেক্ষা করছি" } }
  , { key: "cis4BP", value: { en_us: "Call me once you reach", ta_in: "சேர்ந்தவுடன் கால் செய்யவும்", kn_in: "ನೀವು ತಲುಪಿದ ನಂತರ ಕರೆ ಮಾಡಿ", hi_in: "पहुँच जाओगे तब मुझे एक बार कॉल कर देना", ml_in: "താങ്കൾ എത്തിക്കഴിയുമ്പോൾ എന്നെ വിളിക്കുക", bn_in: "পৌঁছে গেলে আমাকে ফোন করুন" } }
  , { key: "cis1dr1AP", value: { en_us: "Waiting for you", ta_in: "உங்களுக்காக காத்திருக்கிறேன்", kn_in: "ನಿಮಗಾಗಿ ಕಾಯುತ್ತಿದ್ದೇನೆ", hi_in: "मैं आपका इंतजार कर रहा हूँ ", ml_in: "താങ്കൾക്കായി കാത്തിരിക്കുകയാണ്", bn_in: "আপনার জন্য অপেক্ষা করছি" } }
  , { key: "cis1dr2AP", value: { en_us: "Call me, You're unreachable", ta_in: "அணுக முடியவில்லை. கால் செய்யுங்கள்", kn_in: "ಕರೆ ಮಾಡಿ, ನೀವು ತಲುಪಲು ಸಾಧ್ಯವಿಲ್ಲ ", hi_in: "आपका फ़ोन नहीं लग रहा, कॉल कीजिये", ml_in: "എന്നെ വിളിക്കൂ, താങ്കൾ പരിധിക്കു പുറത്താണ്", bn_in: "আমাকে কল করুন, আপনার ফোনে যোগাযোগ করা যাচ্ছে না" } }
  , { key: "cis2dr1AP", value: { en_us: "Waiting for you", ta_in: "உங்களுக்காக காத்திருக்கிறேன்", kn_in: "ನಿಮಗಾಗಿ ಕಾಯುತ್ತಿದ್ದೇನೆ", hi_in: "मैं आपका इंतजार कर रहा हूँ ", ml_in: "താങ്കൾക്കായി കാത്തിരിക്കുകയാണ്", bn_in: "আপনার জন্য অপেক্ষা করছি" } }
  , { key: "cis2dr2AP", value: { en_us: "Call me, You're unreachable", ta_in: "அணுக முடியவில்லை. கால் செய்யுங்கள்", kn_in: "ಕರೆ ಮಾಡಿ, ನೀವು ತಲುಪಲು ಸಾಧ್ಯವಿಲ್ಲ ", hi_in: "आपका फ़ोन नहीं लग रहा, कॉल कीजिये", ml_in: "എന്നെ വിളിക്കൂ, താങ്കൾ പരിധിക്കു പുറത്താണ്", bn_in: "আমাকে কল করুন, আপনার ফোনে যোগাযোগ করা যাচ্ছে না" } }
  , { key: "cis3dr1AP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "cis3dr2AP", value: { en_us: "Please wait", ta_in: "தயவுசெய்து காத்திருக்கவும்", kn_in: "ದಯವಿಟ್ಟು ನಿರೀಕ್ಷಿಸಿ", hi_in: "प्लीज थोड़ी देर रुकिए", ml_in: "ദയവായി കാത്തിരിക്കുക", bn_in: "দয়া করে অপেক্ষা করুন" } }
  , { key: "cis1dr1BP", value: { en_us: "On my way", ta_in: "வந்துகொண்டிருக்கிறேன்", kn_in: "ಬರುತ್ತಿದ್ದೇನೆ", hi_in: "मैं आ रहा हूँ ", ml_in: "ഞാൻ വന്നുകൊണ്ടിരിക്കുകയാണ്", bn_in: "আমি আসছি" } }
  , { key: "cis1dr2BP", value: { en_us: "Starting shortly, Please wait", ta_in: "விரைவில் துவங்குவேன். வருகிறேன்", kn_in: "ಪ್ರಾರಂಭಿಸಲಾಗುತ್ತಿದೆ,ದಯವಿಟ್ಟು ನಿರೀಕ್ಷಿಸಿ", hi_in: "थोड़ी देर में आ रहा हूँ ", ml_in: "ഉടനെ പുറപ്പെടും, ദയവായി കാത്തിരിക്കുക", bn_in: "আমি শীঘ্রই আসছি, দয়া করে অপেক্ষা করুন" } }
  , { key: "cis1dr3BP", value: { en_us: "Road block/Traffic, Please wait", ta_in: "சாலை தடுப்பு/டிராபிக். வருகிறேன்", kn_in: "ರಸ್ತೆ ತಡೆ, ದಯವಿಟ್ಟು ನಿರೀಕ್ಷಿಸಿ", hi_in: "रास्ता बंद है / ट्रैफिक में फसा हूँ, प्लीज रुकिए", ml_in: "റോഡ് ബ്ലോക്ക്/ട്രാഫിക്, ദയവായി കാത്തിരിക്കുക", bn_in: "রাস্তা ব্লক/ট্রাফিক, দয়া করে অপেক্ষা করুন" } }
  , { key: "cis1dr4BP", value: { en_us: "Finishing a trip, coming soon", ta_in: "மற்றொரு சவாரி முடிக்கிறேன். வருகிறேன்", kn_in: "ಟ್ರಿಪ್ ಮುಗಿಸಲಾಗುತ್ತಿದೆ, ಶೀಘ್ರದಲ್ಲೇ ಬರುತ್ತೇನೆ", hi_in: "एक ट्रिप ख़त्म कर के आ रहा हूँ ", ml_in: "ഒരു ട്രിപ്പ് പൂർത്തിയാക്കുന്നു, ഉടൻ വരുന്നു", bn_in: "একটা ট্রিপ শেষ করে শীঘ্রই আসছি" } }
  , { key: "cis2dr1BP", value: { en_us: "On my way", ta_in: "வந்துகொண்டிருக்கிறேன்", kn_in: "ಬರುತ್ತಿದ್ದೇನೆ", hi_in: "मैं आ रहा हूँ ", ml_in: "ഞാൻ വന്നുകൊണ്ടിരിക്കുകയാണ്", bn_in: "আমি আসছি" } }
  , { key: "cis2dr2BP", value: { en_us: "Starting shortly, Please wait", ta_in: "விரைவில் துவங்குவேன். வருகிறேன்", kn_in: "ಪ್ರಾರಂಭಿಸಲಾಗುತ್ತಿದೆ,ದಯವಿಟ್ಟು ನಿರೀಕ್ಷಿಸಿ", hi_in: "थोड़ी देर में आ रहा हूँ ", ml_in: "ഉടനെ പുറപ്പെടും, ദയവായി കാത്തിരിക്കുക", bn_in: "আমি শীঘ্রই আসছি, দয়া করে অপেক্ষা করুন" } }
  , { key: "cis2dr3BP", value: { en_us: "Road block/Traffic, Please wait", ta_in: "சாலை தடுப்பு/டிராபிக். வருகிறேன்", kn_in: "ರಸ್ತೆ ತಡೆ, ದಯವಿಟ್ಟು ನಿರೀಕ್ಷಿಸಿ", hi_in: "रास्ता बंद है / ट्रैफिक में फसा हूँ, प्लीज रुकिए", ml_in: "റോഡ് ബ്ലോക്ക്/ട്രാഫിക്, ദയവായി കാത്തിരിക്കുക", bn_in: "রাস্তা ব্লক/ট্রাফিক, দয়া করে অপেক্ষা করুন" } }
  , { key: "cis2dr4BP", value: { en_us: "Finishing a trip, coming soon", ta_in: "மற்றொரு சவாரி முடிக்கிறேன். வருகிறேன்", kn_in: "ಟ್ರಿಪ್ ಮುಗಿಸಲಾಗುತ್ತಿದೆ, ಶೀಘ್ರದಲ್ಲೇ ಬರುತ್ತೇನೆ", hi_in: "एक ट्रिप ख़त्म कर के आ रहा हूँ ", ml_in: "ഒരു ട്രിപ്പ് പൂർത്തിയാക്കുന്നു, ഉടൻ വരുന്നു", bn_in: "একটা ট্রিপ শেষ করে শীঘ্রই আসছি" } }
  , { key: "cis3dr1BP", value: { en_us: "On my way", ta_in: "வந்துகொண்டிருக்கிறேன்", kn_in: "ಬರುತ್ತಿದ್ದೇನೆ", hi_in: "मैं आ रहा हूँ ", ml_in: "ഞാൻ വന്നുകൊണ്ടിരിക്കുകയാണ്", bn_in: "আমি আসছি" } }
  , { key: "cis3dr2BP", value: { en_us: "Starting shortly, Please wait", ta_in: "விரைவில் துவங்குவேன். வருகிறேன்", kn_in: "ಪ್ರಾರಂಭಿಸಲಾಗುತ್ತಿದೆ,ದಯವಿಟ್ಟು ನಿರೀಕ್ಷಿಸಿ", hi_in: "थोड़ी देर में आ रहा हूँ ", ml_in: "ഉടനെ പുറപ്പെടും, ദയവായി കാത്തിരിക്കുക", bn_in: "আমি শীঘ্রই আসছি, দয়া করে অপেক্ষা করুন" } }
  , { key: "cis3dr3BP", value: { en_us: "Road block/Traffic, Please wait", ta_in: "சாலை தடுப்பு/டிராபிக். வருகிறேன்", kn_in: "ರಸ್ತೆ ತಡೆ, ದಯವಿಟ್ಟು ನಿರೀಕ್ಷಿಸಿ", hi_in: "रास्ता बंद है / ट्रैफिक में फसा हूँ, प्लीज रुकिए", ml_in: "റോഡ് ബ്ലോക്ക്/ട്രാഫിക്, ദയവായി കാത്തിരിക്കുക", bn_in: "রাস্তা ব্লক/ট্রাফিক, দয়া করে অপেক্ষা করুন" } }
  , { key: "cis3dr4BP", value: { en_us: "Finishing a trip, coming soon", ta_in: "மற்றொரு சவாரி முடிக்கிறேன். வருகிறேன்", kn_in: "ಟ್ರಿಪ್ ಮುಗಿಸಲಾಗುತ್ತಿದೆ, ಶೀಘ್ರದಲ್ಲೇ ಬರುತ್ತೇನೆ", hi_in: "एक ट्रिप ख़त्म कर के आ रहा हूँ ", ml_in: "ഒരു ട്രിപ്പ് പൂർത്തിയാക്കുന്നു, ഉടൻ വരുന്നു", bn_in: "একটা ট্রিপ শেষ করে শীঘ্রই আসছি" } }
  , { key: "cis4dr1BP", value: { en_us: "On my way", ta_in: "வந்துகொண்டிருக்கிறேன்", kn_in: "ಬರುತ್ತಿದ್ದೇನೆ", hi_in: "मैं आ रहा हूँ ", ml_in: "ഞാൻ വന്നുകൊണ്ടിരിക്കുകയാണ്", bn_in: "আমি আসছি" } }
  , { key: "cis4dr2BP", value: { en_us: "Starting shortly, Please wait", ta_in: "விரைவில் துவங்குவேன். வருகிறேன்", kn_in: "ಪ್ರಾರಂಭಿಸಲಾಗುತ್ತಿದೆ,ದಯವಿಟ್ಟು ನಿರೀಕ್ಷಿಸಿ", hi_in: "थोड़ी देर में आ रहा हूँ ", ml_in: "ഉടനെ പുറപ്പെടും, ദയവായി കാത്തിരിക്കുക", bn_in: "আমি শীঘ্রই আসছি, দয়া করে অপেক্ষা করুন" } }
  , { key: "cis4dr3BP", value: { en_us: "Road block/Traffic, Please wait", ta_in: "சாலை தடுப்பு/டிராபிக். வருகிறேன்", kn_in: "ರಸ್ತೆ ತಡೆ, ದಯವಿಟ್ಟು ನಿರೀಕ್ಷಿಸಿ", hi_in: "रास्ता बंद है / ट्रैफिक में फसा हूँ, प्लीज रुकिए", ml_in: "റോഡ് ബ്ലോക്ക്/ട്രാഫിക്, ദയവായി കാത്തിരിക്കുക", bn_in: "রাস্তা ব্লক/ট্রাফিক, দয়া করে অপেক্ষা করুন" } }
  , { key: "cis4dr4BP", value: { en_us: "Finishing a trip, coming soon", ta_in: "மற்றொரு சவாரி முடிக்கிறேன். வருகிறேன்", kn_in: "ಟ್ರಿಪ್ ಮುಗಿಸಲಾಗುತ್ತಿದೆ, ಶೀಘ್ರದಲ್ಲೇ ಬರುತ್ತೇನೆ", hi_in: "एक ट्रिप ख़त्म कर के आ रहा हूँ ", ml_in: "ഒരു ട്രിപ്പ് പൂർത്തിയാക്കുന്നു, ഉടൻ വരുന്നു", bn_in: "একটা ট্রিপ শেষ করে শীঘ্রই আসছি" } }
  , { key: "cis1dr1cs1AP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "cis1dr1cs2AP", value: { en_us: "Please wait", ta_in: "தயவுசெய்து காத்திருக்கவும்", kn_in: "ದಯವಿಟ್ಟು ನಿರೀಕ್ಷಿಸಿ", hi_in: "प्लीज थोड़ी देर रुकिए", ml_in: "ദയവായി കാത്തിരിക്കുക", bn_in: "দয়া করে অপেক্ষা করুন" } }
  , { key: "cis1dr2cs1AP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "cis1dr2cs2AP", value: { en_us: "Please wait", ta_in: "தயவுசெய்து காத்திருக்கவும்", kn_in: "ದಯವಿಟ್ಟು ನಿರೀಕ್ಷಿಸಿ", hi_in: "प्लीज थोड़ी देर रुकिए", ml_in: "ദയവായി കാത്തിരിക്കുക", bn_in: "দয়া করে অপেক্ষা করুন" } }
  , { key: "cis2dr1cs1AP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "cis2dr1cs2AP", value: { en_us: "Please wait", ta_in: "தயவுசெய்து காத்திருக்கவும்", kn_in: "ದಯವಿಟ್ಟು ನಿರೀಕ್ಷಿಸಿ", hi_in: "प्लीज थोड़ी देर रुकिए", ml_in: "ദയവായി കാത്തിരിക്കുക", bn_in: "দয়া করে অপেক্ষা করুন" } }
  , { key: "cis2dr2cs1AP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "cis2dr2cs2AP", value: { en_us: "Please wait", ta_in: "தயவுசெய்து காத்திருக்கவும்", kn_in: "ದಯವಿಟ್ಟು ನಿರೀಕ್ಷಿಸಿ", hi_in: "प्लीज थोड़ी देर रुकिए", ml_in: "ദയവായി കാത്തിരിക്കുക", bn_in: "দয়া করে অপেক্ষা করুন" } }
  , { key: "cis1dr1cs1BP", value: { en_us: "Waiting, Don't cancel", ta_in: "காத்திருக்கிறேன், ரத்து செய்யாதீர்", kn_in: "ನಿರೀಕ್ಷಿಸಲಾಗುತ್ತಿದೆ, ರದ್ದು ಮಾಡಬೇಡಿ", hi_in: "मैं रुक गया हूँ , प्लीज आप कैंसिल मत करिये", ml_in: "ഞാൻ കാത്തിരിക്കുകയാണ്, ക്യാൻസൽ ചെയ്യരുത്", bn_in: "আমি অপেক্ষা করছি, দয়া করে বাতিল করবেন না" } }
  , { key: "cis1dr1cs2BP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "cis1dr2cs1BP", value: { en_us: "Waiting, Don't cancel", ta_in: "காத்திருக்கிறேன், ரத்து செய்யாதீர்", kn_in: "ನಿರೀಕ್ಷಿಸಲಾಗುತ್ತಿದೆ, ರದ್ದು ಮಾಡಬೇಡಿ", hi_in: "मैं रुक गया हूँ , प्लीज आप कैंसिल मत करिये", ml_in: "ഞാൻ കാത്തിരിക്കുകയാണ്, ക്യാൻസൽ ചെയ്യരുത്", bn_in: "আমি অপেক্ষা করছি, দয়া করে বাতিল করবেন না" } }
  , { key: "cis1dr2cs2BP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "cis1dr3cs1BP", value: { en_us: "Waiting, Don't cancel", ta_in: "காத்திருக்கிறேன், ரத்து செய்யாதீர்", kn_in: "ನಿರೀಕ್ಷಿಸಲಾಗುತ್ತಿದೆ, ರದ್ದು ಮಾಡಬೇಡಿ", hi_in: "मैं रुक गया हूँ , प्लीज आप कैंसिल मत करिये", ml_in: "ഞാൻ കാത്തിരിക്കുകയാണ്, ക്യാൻസൽ ചെയ്യരുത്", bn_in: "আমি অপেক্ষা করছি, দয়া করে বাতিল করবেন না" } }
  , { key: "cis1dr3cs2BP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "cis1dr4cs1BP", value: { en_us: "Waiting, Don't cancel", ta_in: "காத்திருக்கிறேன், ரத்து செய்யாதீர்", kn_in: "ನಿರೀಕ್ಷಿಸಲಾಗುತ್ತಿದೆ, ರದ್ದು ಮಾಡಬೇಡಿ", hi_in: "मैं रुक गया हूँ , प्लीज आप कैंसिल मत करिये", ml_in: "ഞാൻ കാത്തിരിക്കുകയാണ്, ക്യാൻസൽ ചെയ്യരുത്", bn_in: "আমি অপেক্ষা করছি, দয়া করে বাতিল করবেন না" } }
  , { key: "cis1dr4cs2BP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "cis2dr1cs1BP", value: { en_us: "Waiting, Don't cancel", ta_in: "காத்திருக்கிறேன், ரத்து செய்யாதீர்", kn_in: "ನಿರೀಕ್ಷಿಸಲಾಗುತ್ತಿದೆ, ರದ್ದು ಮಾಡಬೇಡಿ", hi_in: "मैं रुक गया हूँ , प्लीज आप कैंसिल मत करिये", ml_in: "ഞാൻ കാത്തിരിക്കുകയാണ്, ക്യാൻസൽ ചെയ്യരുത്", bn_in: "আমি অপেক্ষা করছি, দয়া করে বাতিল করবেন না" } }
  , { key: "cis2dr1cs2BP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "cis2dr2cs1BP", value: { en_us: "Waiting, Don't cancel", ta_in: "காத்திருக்கிறேன், ரத்து செய்யாதீர்", kn_in: "ನಿರೀಕ್ಷಿಸಲಾಗುತ್ತಿದೆ, ರದ್ದು ಮಾಡಬೇಡಿ", hi_in: "मैं रुक गया हूँ , प्लीज आप कैंसिल मत करिये", ml_in: "ഞാൻ കാത്തിരിക്കുകയാണ്, ക്യാൻസൽ ചെയ്യരുത്", bn_in: "আমি অপেক্ষা করছি, দয়া করে বাতিল করবেন না" } }
  , { key: "cis2dr2cs2BP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "cis2dr3cs1BP", value: { en_us: "Waiting, Don't cancel", ta_in: "காத்திருக்கிறேன், ரத்து செய்யாதீர்", kn_in: "ನಿರೀಕ್ಷಿಸಲಾಗುತ್ತಿದೆ, ರದ್ದು ಮಾಡಬೇಡಿ", hi_in: "मैं रुक गया हूँ , प्लीज आप कैंसिल मत करिये", ml_in: "ഞാൻ കാത്തിരിക്കുകയാണ്, ക്യാൻസൽ ചെയ്യരുത്", bn_in: "আমি অপেক্ষা করছি, দয়া করে বাতিল করবেন না" } }
  , { key: "cis2dr3cs2BP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "cis2dr4cs1BP", value: { en_us: "Waiting, Don't cancel", ta_in: "காத்திருக்கிறேன், ரத்து செய்யாதீர்", kn_in: "ನಿರೀಕ್ಷಿಸಲಾಗುತ್ತಿದೆ, ರದ್ದು ಮಾಡಬೇಡಿ", hi_in: "मैं रुक गया हूँ , प्लीज आप कैंसिल मत करिये", ml_in: "ഞാൻ കാത്തിരിക്കുകയാണ്, ക്യാൻസൽ ചെയ്യരുത്", bn_in: "আমি অপেক্ষা করছি, দয়া করে বাতিল করবেন না" } }
  , { key: "cis2dr4cs2BP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "cis3dr1cs1BP", value: { en_us: "Waiting, Don't cancel", ta_in: "காத்திருக்கிறேன், ரத்து செய்யாதீர்", kn_in: "ನಿರೀಕ್ಷಿಸಲಾಗುತ್ತಿದೆ, ರದ್ದು ಮಾಡಬೇಡಿ", hi_in: "मैं रुक गया हूँ , प्लीज आप कैंसिल मत करिये", ml_in: "ഞാൻ കാത്തിരിക്കുകയാണ്, ക്യാൻസൽ ചെയ്യരുത്", bn_in: "আমি অপেক্ষা করছি, দয়া করে বাতিল করবেন না" } }
  , { key: "cis3dr1cs2BP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "cis3dr2cs1BP", value: { en_us: "Waiting, Don't cancel", ta_in: "காத்திருக்கிறேன், ரத்து செய்யாதீர்", kn_in: "ನಿರೀಕ್ಷಿಸಲಾಗುತ್ತಿದೆ, ರದ್ದು ಮಾಡಬೇಡಿ", hi_in: "मैं रुक गया हूँ , प्लीज आप कैंसिल मत करिये", ml_in: "ഞാൻ കാത്തിരിക്കുകയാണ്, ക്യാൻസൽ ചെയ്യരുത്", bn_in: "আমি অপেক্ষা করছি, দয়া করে বাতিল করবেন না" } }
  , { key: "cis3dr2cs2BP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "cis3dr3cs1BP", value: { en_us: "Waiting, Don't cancel", ta_in: "காத்திருக்கிறேன், ரத்து செய்யாதீர்", kn_in: "ನಿರೀಕ್ಷಿಸಲಾಗುತ್ತಿದೆ, ರದ್ದು ಮಾಡಬೇಡಿ", hi_in: "मैं रुक गया हूँ , प्लीज आप कैंसिल मत करिये", ml_in: "ഞാൻ കാത്തിരിക്കുകയാണ്, ക്യാൻസൽ ചെയ്യരുത്", bn_in: "আমি অপেক্ষা করছি, দয়া করে বাতিল করবেন না" } }
  , { key: "cis3dr3cs2BP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "cis3dr4cs1BP", value: { en_us: "Waiting, Don't cancel", ta_in: "காத்திருக்கிறேன், ரத்து செய்யாதீர்", kn_in: "ನಿರೀಕ್ಷಿಸಲಾಗುತ್ತಿದೆ, ರದ್ದು ಮಾಡಬೇಡಿ", hi_in: "मैं रुक गया हूँ , प्लीज आप कैंसिल मत करिये", ml_in: "ഞാൻ കാത്തിരിക്കുകയാണ്, ക്യാൻസൽ ചെയ്യരുത്", bn_in: "আমি অপেক্ষা করছি, দয়া করে বাতিল করবেন না" } }
  , { key: "cis3dr4cs2BP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "cis4dr1cs1BP", value: { en_us: "Waiting, Don't cancel", ta_in: "காத்திருக்கிறேன், ரத்து செய்யாதீர்", kn_in: "ನಿರೀಕ್ಷಿಸಲಾಗುತ್ತಿದೆ, ರದ್ದು ಮಾಡಬೇಡಿ", hi_in: "मैं रुक गया हूँ , प्लीज आप कैंसिल मत करिये", ml_in: "ഞാൻ കാത്തിരിക്കുകയാണ്, ക്യാൻസൽ ചെയ്യരുത്", bn_in: "আমি অপেক্ষা করছি, দয়া করে বাতিল করবেন না" } }
  , { key: "cis4dr1cs2BP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "cis4dr2cs1BP", value: { en_us: "Waiting, Don't cancel", ta_in: "காத்திருக்கிறேன், ரத்து செய்யாதீர்", kn_in: "ನಿರೀಕ್ಷಿಸಲಾಗುತ್ತಿದೆ, ರದ್ದು ಮಾಡಬೇಡಿ", hi_in: "मैं रुक गया हूँ , प्लीज आप कैंसिल मत करिये", ml_in: "ഞാൻ കാത്തിരിക്കുകയാണ്, ക്യാൻസൽ ചെയ്യരുത്", bn_in: "আমি অপেক্ষা করছি, দয়া করে বাতিল করবেন না" } }
  , { key: "cis4dr2cs2BP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "cis4dr3cs1BP", value: { en_us: "Waiting, Don't cancel", ta_in: "காத்திருக்கிறேன், ரத்து செய்யாதீர்", kn_in: "ನಿರೀಕ್ಷಿಸಲಾಗುತ್ತಿದೆ, ರದ್ದು ಮಾಡಬೇಡಿ", hi_in: "मैं रुक गया हूँ , प्लीज आप कैंसिल मत करिये", ml_in: "ഞാൻ കാത്തിരിക്കുകയാണ്, ക്യാൻസൽ ചെയ്യരുത്", bn_in: "আমি অপেক্ষা করছি, দয়া করে বাতিল করবেন না" } }
  , { key: "cis4dr3cs2BP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "cis4dr4cs1BP", value: { en_us: "Waiting, Don't cancel", ta_in: "காத்திருக்கிறேன், ரத்து செய்யாதீர்", kn_in: "ನಿರೀಕ್ಷಿಸಲಾಗುತ್ತಿದೆ, ರದ್ದು ಮಾಡಬೇಡಿ", hi_in: "मैं रुक गया हूँ , प्लीज आप कैंसिल मत करिये", ml_in: "ഞാൻ കാത്തിരിക്കുകയാണ്, ക്യാൻസൽ ചെയ്യരുത്", bn_in: "আমি অপেক্ষা করছি, দয়া করে বাতিল করবেন না" } }
  , { key: "cis4dr4cs2BP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "cds1AP", value: { en_us: "Please wait", ta_in: "தயவுசெய்து காத்திருக்கவும்", kn_in: "ದಯವಿಟ್ಟು ನಿರೀಕ್ಷಿಸಿ", hi_in: "प्लीज थोड़ी देर रुकिए", ml_in: "ദയവായി കാത്തിരിക്കുക", bn_in: "দয়া করে অপেক্ষা করুন" } }
  , { key: "cds2AP", value: { en_us: "Call me, You're unreachable", ta_in: "அணுக முடியவில்லை. கால் செய்யுங்கள்", kn_in: "ಕರೆ ಮಾಡಿ, ನೀವು ತಲುಪಲು ಸಾಧ್ಯವಿಲ್ಲ ", hi_in: "आपका फ़ोन नहीं लग रहा, कॉल कीजिये", ml_in: "എന്നെ വിളിക്കൂ, താങ്കൾ പരിധിക്കു പുറത്താണ്", bn_in: "আমাকে কল করুন, আপনার ফোনে যোগাযোগ করা যাচ্ছে না" } }
  , { key: "cds1BP", value: { en_us: "Are you starting?", ta_in: "நீங்கள் தொடங்குகிறீர்களா?", kn_in: "ನೀವು ಪ್ರಾರಂಭಿಸುತ್ತಿದ್ದೀರಾ?", hi_in: "आप आ रहे है क्या? ", ml_in: "താങ്കൾ പുറപ്പെടുകയാണോ?", bn_in: "আপনি কি আসছেন ?" } }
  , { key: "cds2BP", value: { en_us: "Please come soon", ta_in: "விரைவாக வாருங்கள்", kn_in: "ದಯವಿಟ್ಟು ಬೇಗ ಬನ್ನಿ ", hi_in: "प्लीज थोड़ा जल्दी आइये", ml_in: "ദയവായി വേഗം വരൂ", bn_in: "দয়া করে তাড়াতাড়ি আসুন" } }
  , { key: "dis1AP", value: { en_us: "I've arrived", ta_in: "நான் வந்துவிட்டேன்", kn_in: "ನಾನು ತಲುಪಿದೆ", hi_in: "मैं लोकेशन पे आ गया हूँ ", ml_in: "ഞാൻ എത്തി", bn_in: "আমি পৌঁছে গেছি" } }
  , { key: "dis2AP", value: { en_us: "Please come fast, I am waiting", ta_in: "சீக்கிரம் வாருங்கள், காத்திருக்கிறேன்", kn_in: "ಬೇಗ ಬನ್ನಿ, ನಾನು ಕಾಯುತ್ತಿದ್ದೇನೆ.", hi_in: "प्लीज जल्दी आइये मैं लोकेशन पे हूँ", ml_in: "വേഗം വരൂ, ഞാൻ കാത്തിരിക്കുകയാണ്", bn_in: "দয়া করে তাড়াতাড়ি আসুন, আমি অপেক্ষা করছি" } }
  , { key: "dis3AP", value: { en_us: "Call me, You're unreachable", ta_in: "அணுக முடியவில்லை. கால் செய்யுங்கள்", kn_in: "ಕರೆ ಮಾಡಿ, ನೀವು ತಲುಪಲು ಸಾಧ್ಯವಿಲ್ಲ ", hi_in: "आपका फ़ोन नहीं लग रहा, कॉल कीजिये", ml_in: "എന്നെ വിളിക്കൂ, താങ്കൾ പരിധിക്കു പുറത്താണ്", bn_in: "আমাকে কল করুন, আপনার ফোনে যোগাযোগ করা যাচ্ছে না" } }
  , { key: "dis1BP", value: { en_us: "On my way", ta_in: "வந்துகொண்டிருக்கிறேன்", kn_in: "ಬರುತ್ತಿದ್ದೇನೆ", hi_in: "मैं आ रहा हूँ ", ml_in: "ഞാൻ വന്നുകൊണ്ടിരിക്കുകയാണ്", bn_in: "আমি আসছি" } }
  , { key: "dis2BP", value: { en_us: "Starting shortly, Please wait", ta_in: "விரைவில் துவங்குவேன். வருகிறேன்", kn_in: "ಪ್ರಾರಂಭಿಸಲಾಗುತ್ತಿದೆ,ದಯವಿಟ್ಟು ನಿರೀಕ್ಷಿಸಿ", hi_in: "थोड़ी देर में आ रहा हूँ ", ml_in: "ഉടനെ പുറപ്പെടും, ദയവായി കാത്തിരിക്കുക", bn_in: "আমি শীঘ্রই আসছি, দয়া করে অপেক্ষা করুন" } }
  , { key: "dis3BP", value: { en_us: "Road block/Traffic, Please wait", ta_in: "சாலை தடுப்பு/டிராபிக். வருகிறேன்", kn_in: "ರಸ್ತೆ ತಡೆ, ದಯವಿಟ್ಟು ನಿರೀಕ್ಷಿಸಿ", hi_in: "रास्ता बंद है / ट्रैफिक में फसा हूँ, प्लीज रुकिए", ml_in: "റോഡ് ബ്ലോക്ക്/ട്രാഫിക്, ദയവായി കാത്തിരിക്കുക", bn_in: "রাস্তা ব্লক/ট্রাফিক, দয়া করে অপেক্ষা করুন" } }
  , { key: "dis4BP", value: { en_us: "Finishing a trip, coming soon", ta_in: "மற்றொரு சவாரி முடிக்கிறேன். வருகிறேன்", kn_in: "ಟ್ರಿಪ್ ಮುಗಿಸಲಾಗುತ್ತಿದೆ, ಶೀಘ್ರದಲ್ಲೇ ಬರುತ್ತೇನೆ", hi_in: "एक ट्रिप ख़त्म कर के आ रहा हूँ ", ml_in: "ഒരു ട്രിപ്പ് പൂർത്തിയാക്കുന്നു, ഉടൻ വരുന്നു", bn_in: "একটা ট্রিপ শেষ করে শীঘ্রই আসছি" } }
  , { key: "dis1cr1AP", value: { en_us: "At pick-up", ta_in: "பிக்-அப்பில் உள்ளேன்", kn_in: "ಪಿಕ್ ಅಪ್ ನಲ್ಲಿ ", hi_in: "मैं लोकेशन पे आ गया हूँ ", ml_in: "ഞാൻ പിക്ക്-അപ്പിൽ ആണ്", bn_in: "আমি পিক-আপ স্থানে আছি" } }
  , { key: "dis1cr2AP", value: { en_us: "Ok, Coming shortly", ta_in: "சரி, விரைவில் வருகிறேன்", kn_in: "ಸರಿ, ಶೀಘ್ರದಲ್ಲೇ ಬರುತ್ತೇನೆ", hi_in: "ठीक है मैं थोड़ी देर मैं आ रहा हूँ ", ml_in: "ഓക്കേ, ഉടൻ വരുന്നു", bn_in: "ঠিক আছে, আমি শীঘ্রই আসছি" } }
  , { key: "dis1cr3AP", value: { en_us: "Call me, You're unreachable", ta_in: "அணுக முடியவில்லை. கால் செய்யுங்கள்", kn_in: "ಕರೆ ಮಾಡಿ, ನೀವು ತಲುಪಲು ಸಾಧ್ಯವಿಲ್ಲ ", hi_in: "आपका फ़ोन नहीं लग रहा, कॉल कीजिये", ml_in: "എന്നെ വിളിക്കൂ, താങ്കൾ പരിധിക്കു പുറത്താണ്", bn_in: "আমাকে কল করুন, আপনার ফোনে যোগাযোগ করা যাচ্ছে না" } }
  , { key: "dis2cr1AP", value: { en_us: "At pick-up", ta_in: "பிக்-அப்பில் உள்ளேன்", kn_in: "ಪಿಕ್ ಅಪ್ ನಲ್ಲಿ ", hi_in: "मैं लोकेशन पे आ गया हूँ ", ml_in: "ഞാൻ പിക്ക്-അപ്പിൽ ആണ്", bn_in: "আমি পিক-আপ স্থানে আছি" } }
  , { key: "dis2cr2AP", value: { en_us: "Ok, Coming shortly", ta_in: "சரி, விரைவில் வருகிறேன்", kn_in: "ಸರಿ, ಶೀಘ್ರದಲ್ಲೇ ಬರುತ್ತೇನೆ", hi_in: "ठीक है मैं थोड़ी देर मैं आ रहा हूँ ", ml_in: "ഓക്കേ, ഉടൻ വരുന്നു", bn_in: "ঠিক আছে, আমি শীঘ্রই আসছি" } }
  , { key: "dis2cr3AP", value: { en_us: "Call me, You're unreachable", ta_in: "அணுக முடியவில்லை. கால் செய்யுங்கள்", kn_in: "ಕರೆ ಮಾಡಿ, ನೀವು ತಲುಪಲು ಸಾಧ್ಯವಿಲ್ಲ ", hi_in: "आपका फ़ोन नहीं लग रहा, कॉल कीजिये", ml_in: "എന്നെ വിളിക്കൂ, താങ്കൾ പരിധിക്കു പുറത്താണ്", bn_in: "আমাকে কল করুন, আপনার ফোনে যোগাযোগ করা যাচ্ছে না" } }
  , { key: "dis3cr1AP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "dis3cr2AP", value: { en_us: "Please wait", ta_in: "தயவுசெய்து காத்திருக்கவும்", kn_in: "ದಯವಿಟ್ಟು ನಿರೀಕ್ಷಿಸಿ", hi_in: "प्लीज थोड़ी देर रुकिए", ml_in: "ദയവായി കാത്തിരിക്കുക", bn_in: "দয়া করে অপেক্ষা করুন" } }
  , { key: "dis1cr1BP", value: { en_us: "Call me once you reach", ta_in: "சேர்ந்தவுடன் கால் செய்யவும்", kn_in: "ನೀವು ತಲುಪಿದ ನಂತರ ಕರೆ ಮಾಡಿ", hi_in: "पहुँच जाओगे तब मुझे एक बार कॉल कर देना", ml_in: "താങ്കൾ എത്തിക്കഴിയുമ്പോൾ എന്നെ വിളിക്കുക", bn_in: "পৌঁছে গেলে আমাকে ফোন করুন" } }
  , { key: "dis1cr2BP", value: { en_us: "Please come soon", ta_in: "விரைவாக வாருங்கள்", kn_in: "ದಯವಿಟ್ಟು ಬೇಗ ಬನ್ನಿ ", hi_in: "प्लीज थोड़ा जल्दी आइये", ml_in: "ദയവായി വേഗം വരൂ", bn_in: "দয়া করে তাড়াতাড়ি আসুন" } }
  , { key: "dis1cr3BP", value: { en_us: "Ok, I am waiting", ta_in: "சரி, காத்திருக்கிறேன்", kn_in: "ಸರಿ, ನಾನು ಕಾಯುತ್ತಿದ್ದೇನೆ", hi_in: "ठीक है, मैं इंतजार कर रहा हूँ ", ml_in: "ഓക്കേ, ഞാൻ കാത്തിരിക്കുകയാണ്", bn_in: "ঠিক আছে, আমি অপেক্ষা করছি" } }
  , { key: "dis2cr1BP", value: { en_us: "Call me once you reach", ta_in: "சேர்ந்தவுடன் கால் செய்யவும்", kn_in: "ನೀವು ತಲುಪಿದ ನಂತರ ಕರೆ ಮಾಡಿ", hi_in: "पहुँच जाओगे तब मुझे एक बार कॉल कर देना", ml_in: "താങ്കൾ എത്തിക്കഴിയുമ്പോൾ എന്നെ വിളിക്കുക", bn_in: "পৌঁছে গেলে আমাকে ফোন করুন" } }
  , { key: "dis2cr2BP", value: { en_us: "Please come soon", ta_in: "விரைவாக வாருங்கள்", kn_in: "ದಯವಿಟ್ಟು ಬೇಗ ಬನ್ನಿ ", hi_in: "प्लीज थोड़ा जल्दी आइये", ml_in: "ദയവായി വേഗം വരൂ", bn_in: "দয়া করে তাড়াতাড়ি আসুন" } }
  , { key: "dis2cr3BP", value: { en_us: "Ok, I am waiting", ta_in: "சரி, காத்திருக்கிறேன்", kn_in: "ಸರಿ, ನಾನು ಕಾಯುತ್ತಿದ್ದೇನೆ", hi_in: "ठीक है, मैं इंतजार कर रहा हूँ ", ml_in: "ഓക്കേ, ഞാൻ കാത്തിരിക്കുകയാണ്", bn_in: "ঠিক আছে, আমি অপেক্ষা করছি" } }
  , { key: "dis3cr1BP", value: { en_us: "Call me once you reach", ta_in: "சேர்ந்தவுடன் கால் செய்யவும்", kn_in: "ನೀವು ತಲುಪಿದ ನಂತರ ಕರೆ ಮಾಡಿ", hi_in: "पहुँच जाओगे तब मुझे एक बार कॉल कर देना", ml_in: "താങ്കൾ എത്തിക്കഴിയുമ്പോൾ എന്നെ വിളിക്കുക", bn_in: "পৌঁছে গেলে আমাকে ফোন করুন" } }
  , { key: "dis3cr2BP", value: { en_us: "Please come soon", ta_in: "விரைவாக வாருங்கள்", kn_in: "ದಯವಿಟ್ಟು ಬೇಗ ಬನ್ನಿ ", hi_in: "प्लीज थोड़ा जल्दी आइये", ml_in: "ദയവായി വേഗം വരൂ", bn_in: "দয়া করে তাড়াতাড়ি আসুন" } }
  , { key: "dis3cr3BP", value: { en_us: "Ok, I am waiting", ta_in: "சரி, காத்திருக்கிறேன்", kn_in: "ಸರಿ, ನಾನು ಕಾಯುತ್ತಿದ್ದೇನೆ", hi_in: "ठीक है, मैं इंतजार कर रहा हूँ ", ml_in: "ഓക്കേ, ഞാൻ കാത്തിരിക്കുകയാണ്", bn_in: "ঠিক আছে, আমি অপেক্ষা করছি" } }
  , { key: "dis4cr1BP", value: { en_us: "Call me once you reach", ta_in: "சேர்ந்தவுடன் கால் செய்யவும்", kn_in: "ನೀವು ತಲುಪಿದ ನಂತರ ಕರೆ ಮಾಡಿ", hi_in: "पहुँच जाओगे तब मुझे एक बार कॉल कर देना", ml_in: "താങ്കൾ എത്തിക്കഴിയുമ്പോൾ എന്നെ വിളിക്കുക", bn_in: "পৌঁছে গেলে আমাকে ফোন করুন" } }
  , { key: "dis4cr2BP", value: { en_us: "Please come soon", ta_in: "விரைவாக வாருங்கள்", kn_in: "ದಯವಿಟ್ಟು ಬೇಗ ಬನ್ನಿ ", hi_in: "प्लीज थोड़ा जल्दी आइये", ml_in: "ദയവായി വേഗം വരൂ", bn_in: "দয়া করে তাড়াতাড়ি আসুন" } }
  , { key: "dis4cr3BP", value: { en_us: "Ok, I am waiting", ta_in: "சரி, காத்திருக்கிறேன்", kn_in: "ಸರಿ, ನಾನು ಕಾಯುತ್ತಿದ್ದೇನೆ", hi_in: "ठीक है, मैं इंतजार कर रहा हूँ ", ml_in: "ഓക്കേ, ഞാൻ കാത്തിരിക്കുകയാണ്", bn_in: "ঠিক আছে, আমি অপেক্ষা করছি" } }
  , { key: "dis1cr1ds1AP", value: { en_us: "Looking for you", ta_in: "உங்களை தேடுகிறேன்", kn_in: "ನಿಮ್ಮನ್ನೆ  ಹುಡುಕುತ್ತಿರುವೆ", hi_in: "मैं आपको ढूंड रहा हूँ, आप कहा हो ?", ml_in: "താങ്കളെ തിരയുകയാണ്", bn_in: "আপনাকে খুঁজছি, আপনি কোথায় ?" } }
  , { key: "dis1cr1ds2AP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "dis1cr2ds1AP", value: { en_us: "Ok, will wait", ta_in: "சரி, காத்திருக்கிறேன்", kn_in: "ಸರಿ, ಕಾಯುತ್ತೇನೆ", hi_in: "ठीक है, मैं रुकता हूँ ", ml_in: "ഓക്കേ, ഞാൻ കാത്തിരിക്കാം", bn_in: "ঠিক আছে, আমি অপেক্ষা করবো" } }
  , { key: "dis1cr3ds1AP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "dis1cr3ds2AP", value: { en_us: "Please wait", ta_in: "தயவுசெய்து காத்திருக்கவும்", kn_in: "ದಯವಿಟ್ಟು ನಿರೀಕ್ಷಿಸಿ", hi_in: "प्लीज थोड़ी देर रुकिए", ml_in: "ദയവായി കാത്തിരിക്കുക", bn_in: "দয়া করে অপেক্ষা করুন" } }
  , { key: "dis2cr1ds1AP", value: { en_us: "Looking for you", ta_in: "உங்களை தேடுகிறேன்", kn_in: "ನಿಮ್ಮನ್ನೆ  ಹುಡುಕುತ್ತಿರುವೆ", hi_in: "मैं आपको ढूंड रहा हूँ, आप कहा हो ?", ml_in: "താങ്കളെ തിരയുകയാണ്", bn_in: "আপনাকে খুঁজছি, আপনি কোথায় ?" } }
  , { key: "dis2cr1ds2AP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "dis2cr2ds1AP", value: { en_us: "Ok, will wait", ta_in: "சரி, காத்திருக்கிறேன்", kn_in: "ಸರಿ, ಕಾಯುತ್ತೇನೆ", hi_in: "ठीक है, मैं रुकता हूँ ", ml_in: "ഓക്കേ, ഞാൻ കാത്തിരിക്കാം", bn_in: "ঠিক আছে, আমি অপেক্ষা করবো" } }
  , { key: "dis2cr3ds1AP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "dis2cr3ds2AP", value: { en_us: "Please wait", ta_in: "தயவுசெய்து காத்திருக்கவும்", kn_in: "ದಯವಿಟ್ಟು ನಿರೀಕ್ಷಿಸಿ", hi_in: "प्लीज थोड़ी देर रुकिए", ml_in: "ദയവായി കാത്തിരിക്കുക", bn_in: "দয়া করে অপেক্ষা করুন" } }
  , { key: "dis1cr1ds1BP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "dis1cr1ds2BP", value: { en_us: "Please don't cancel", ta_in: "தயவுற்று ரத்து செய்யாதீர்கள்", kn_in: "ದಯವಿಟ್ಟು ರದ್ದು ಮಾಡಬೇಡಿ", hi_in: "प्लीज कैंसिल मत करिये", ml_in: "ദയവായി ക്യാൻസൽ ചെയ്യരുത്", bn_in: "দয়া করে বাতিল করবেন না" } }
  , { key: "dis1cr2ds1BP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "dis1cr2ds2BP", value: { en_us: "Please don't cancel", ta_in: "தயவுற்று ரத்து செய்யாதீர்கள்", kn_in: "ದಯವಿಟ್ಟು ರದ್ದು ಮಾಡಬೇಡಿ", hi_in: "प्लीज कैंसिल मत करिये", ml_in: "ദയവായി ക്യാൻസൽ ചെയ്യരുത്", bn_in: "দয়া করে বাতিল করবেন না" } }
  , { key: "dis1cr3ds1BP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "dis1cr3ds2BP", value: { en_us: "Please don't cancel", ta_in: "தயவுற்று ரத்து செய்யாதீர்கள்", kn_in: "ದಯವಿಟ್ಟು ರದ್ದು ಮಾಡಬೇಡಿ", hi_in: "प्लीज कैंसिल मत करिये", ml_in: "ദയവായി ക്യാൻസൽ ചെയ്യരുത്", bn_in: "দয়া করে বাতিল করবেন না" } }
  , { key: "dis2cr1ds1BP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "dis2cr1ds2BP", value: { en_us: "Please don't cancel", ta_in: "தயவுற்று ரத்து செய்யாதீர்கள்", kn_in: "ದಯವಿಟ್ಟು ರದ್ದು ಮಾಡಬೇಡಿ", hi_in: "प्लीज कैंसिल मत करिये", ml_in: "ദയവായി ക്യാൻസൽ ചെയ്യരുത്", bn_in: "দয়া করে বাতিল করবেন না" } }
  , { key: "dis2cr2ds1BP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "dis2cr2ds2BP", value: { en_us: "Please don't cancel", ta_in: "தயவுற்று ரத்து செய்யாதீர்கள்", kn_in: "ದಯವಿಟ್ಟು ರದ್ದು ಮಾಡಬೇಡಿ", hi_in: "प्लीज कैंसिल मत करिये", ml_in: "ദയവായി ക്യാൻസൽ ചെയ്യരുത്", bn_in: "দয়া করে বাতিল করবেন না" } }
  , { key: "dis2cr3ds1BP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "dis2cr3ds2BP", value: { en_us: "Please don't cancel", ta_in: "தயவுற்று ரத்து செய்யாதீர்கள்", kn_in: "ದಯವಿಟ್ಟು ರದ್ದು ಮಾಡಬೇಡಿ", hi_in: "प्लीज कैंसिल मत करिये", ml_in: "ദയവായി ക്യാൻസൽ ചെയ്യരുത്", bn_in: "দয়া করে বাতিল করবেন না" } }
  , { key: "dis3cr1ds1BP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "dis3cr1ds2BP", value: { en_us: "Please don't cancel", ta_in: "தயவுற்று ரத்து செய்யாதீர்கள்", kn_in: "ದಯವಿಟ್ಟು ರದ್ದು ಮಾಡಬೇಡಿ", hi_in: "प्लीज कैंसिल मत करिये", ml_in: "ദയവായി ക്യാൻസൽ ചെയ്യരുത്", bn_in: "দয়া করে বাতিল করবেন না" } }
  , { key: "dis3cr2ds1BP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "dis3cr2ds2BP", value: { en_us: "Please don't cancel", ta_in: "தயவுற்று ரத்து செய்யாதீர்கள்", kn_in: "ದಯವಿಟ್ಟು ರದ್ದು ಮಾಡಬೇಡಿ", hi_in: "प्लीज कैंसिल मत करिये", ml_in: "ദയവായി ക്യാൻസൽ ചെയ്യരുത്", bn_in: "দয়া করে বাতিল করবেন না" } }
  , { key: "dis3cr3ds1BP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "dis3cr3ds2BP", value: { en_us: "Please don't cancel", ta_in: "தயவுற்று ரத்து செய்யாதீர்கள்", kn_in: "ದಯವಿಟ್ಟು ರದ್ದು ಮಾಡಬೇಡಿ", hi_in: "प्लीज कैंसिल मत करिये", ml_in: "ദയവായി ക്യാൻസൽ ചെയ്യരുത്", bn_in: "দয়া করে বাতিল করবেন না" } }
  , { key: "dis4cr1ds1BP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "dis4cr1ds2BP", value: { en_us: "Please don't cancel", ta_in: "தயவுற்று ரத்து செய்யாதீர்கள்", kn_in: "ದಯವಿಟ್ಟು ರದ್ದು ಮಾಡಬೇಡಿ", hi_in: "प्लीज कैंसिल मत करिये", ml_in: "ദയവായി ക്യാൻസൽ ചെയ്യരുത്", bn_in: "দয়া করে বাতিল করবেন না" } }
  , { key: "dis4cr2ds1BP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "dis4cr2ds2BP", value: { en_us: "Please don't cancel", ta_in: "தயவுற்று ரத்து செய்யாதீர்கள்", kn_in: "ದಯವಿಟ್ಟು ರದ್ದು ಮಾಡಬೇಡಿ", hi_in: "प्लीज कैंसिल मत करिये", ml_in: "ദയവായി ക്യാൻസൽ ചെയ്യരുത്", bn_in: "দয়া করে বাতিল করবেন না" } }
  , { key: "dis4cr3ds1BP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "dis4cr3ds2BP", value: { en_us: "Please don't cancel", ta_in: "தயவுற்று ரத்து செய்யாதீர்கள்", kn_in: "ದಯವಿಟ್ಟು ರದ್ದು ಮಾಡಬೇಡಿ", hi_in: "प्लीज कैंसिल मत करिये", ml_in: "ദയവായി ക്യാൻസൽ ചെയ്യരുത്", bn_in: "দয়া করে বাতিল করবেন না" } }
  , { key: "dds1AP", value: { en_us: "I've arrived", ta_in: "நான் வந்துவிட்டேன்", kn_in: "ನಾನು ತಲುಪಿದೆ", hi_in: "मैं लोकेशन पे आ गया हूँ ", ml_in: "ഞാൻ എത്തി", bn_in: "আমি পৌঁছে গেছি" } }
  , { key: "dds2AP", value: { en_us: "Please come fast, I am waiting", ta_in: "சீக்கிரம் வாருங்கள், காத்திருக்கிறேன்", kn_in: "ಬೇಗ ಬನ್ನಿ, ನಾನು ಕಾಯುತ್ತಿದ್ದೇನೆ.", hi_in: "प्लीज जल्दी आइये मैं लोकेशन पे हूँ", ml_in: "വേഗം വരൂ, ഞാൻ കാത്തിരിക്കുകയാണ്", bn_in: "দয়া করে তাড়াতাড়ি আসুন, আমি অপেক্ষা করছি" } }
  , { key: "dds3AP", value: { en_us: "Call me, You're unreachable", ta_in: "அணுக முடியவில்லை. கால் செய்யுங்கள்", kn_in: "ಕರೆ ಮಾಡಿ, ನೀವು ತಲುಪಲು ಸಾಧ್ಯವಿಲ್ಲ ", hi_in: "आपका फ़ोन नहीं लग रहा, कॉल कीजिये", ml_in: "എന്നെ വിളിക്കൂ, താങ്കൾ പരിധിക്കു പുറത്താണ്", bn_in: "আমাকে কল করুন, আপনার ফোনে যোগাযোগ করা যাচ্ছে না" } }
  , { key: "dds1BP", value: { en_us: "On my way", ta_in: "வந்துகொண்டிருக்கிறேன்", kn_in: "ಬರುತ್ತಿದ್ದೇನೆ", hi_in: "मैं आ रहा हूँ ", ml_in: "ഞാൻ വന്നുകൊണ്ടിരിക്കുകയാണ്", bn_in: "আমি আসছি" } }
  , { key: "dds2BP", value: { en_us: "Road block/Traffic, Please wait", ta_in: "சாலை தடுப்பு/டிராபிக். வருகிறேன்", kn_in: "ರಸ್ತೆ ತಡೆ, ದಯವಿಟ್ಟು ನಿರೀಕ್ಷಿಸಿ", hi_in: "रास्ता बंद है / ट्रैफिक में फसा हूँ, प्लीज रुकिए", ml_in: "റോഡ് ബ്ലോക്ക്/ട്രാഫിക്, ദയവായി കാത്തിരിക്കുക", bn_in: "রাস্তা ব্লক/ট্রাফিক, দয়া করে অপেক্ষা করুন" } }
  , { key: "dds3BP", value: { en_us: "Call me, You're unreachable", ta_in: "அணுக முடியவில்லை. கால் செய்யுங்கள்", kn_in: "ಕರೆ ಮಾಡಿ, ನೀವು ತಲುಪಲು ಸಾಧ್ಯವಿಲ್ಲ ", hi_in: "आपका फ़ोन नहीं लग रहा, कॉल कीजिये", ml_in: "എന്നെ വിളിക്കൂ, താങ്കൾ പരിധിക്കു പുറത്താണ്", bn_in: "আমাকে কল করুন, আপনার ফোনে যোগাযোগ করা যাচ্ছে না" } }
  , { key: "dols1BP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "dols2BP", value: { en_us: "Yes", ta_in: "ஆம்", kn_in: "ಹೌದು", hi_in: "हाँ", ml_in: "അതെ", bn_in: "হ্যাঁ" } }
  , { key: "dols3BP", value: { en_us: "On my way", ta_in: "வந்துகொண்டிருக்கிறேன்", kn_in: "ಬರುತ್ತಿದ್ದೇನೆ", hi_in: "मैं आ रहा हूँ ", ml_in: "ഞാൻ വന്നുകൊണ്ടിരിക്കുകയാണ്", bn_in: "আমি আসছি" } }
  , { key: "dols1AP", value: { en_us: "Yes", ta_in: "ஆம்", kn_in: "ಹೌದು", hi_in: "हाँ", ml_in: "അതെ", bn_in: "হ্যাঁ" } }
  , { key: "dols2AP", value: { en_us: "Ok", ta_in: "சரி", kn_in: "ಸರಿ", hi_in: "ठीक है", ml_in: "ഓക്കേ", bn_in: "ঠিক আছে" } }
  , { key: "dols3AP", value: { en_us: "At pick-up", ta_in: "பிக்-அப்பில் உள்ளேன்", kn_in: "ಪಿಕ್ ಅಪ್ ನಲ್ಲಿ ", hi_in: "मैं लोकेशन पे आ गया हूँ ", ml_in: "ഞാൻ പിക്ക്-അപ്പിൽ ആണ്", bn_in: "আমি পিক-আপ স্থানে আছি" } }
  ]

getSuggestions ∷ String -> Suggestions
getSuggestions dummy =
  [ { key: "customerInitialAP", value: [ "cis1AP", "cis2AP", "cis3AP" ] }
  , { key: "cis1AP", value: [ "cis1dr1AP", "cis1dr2AP" ] }
  , { key: "cis2AP", value: [ "cis2dr1AP", "cis2dr2AP" ] }
  , { key: "cis3AP", value: [ "cis3dr1AP", "cis3dr2AP" ] }
  , { key: "cis1dr1AP", value: [ "cis1dr1cs1AP", "cis1dr1cs2AP" ] }
  , { key: "cis1dr2AP", value: [ "cis1dr2cs1AP", "cis1dr2cs2AP" ] }
  , { key: "cis2dr1AP", value: [ "cis2dr1cs1AP", "cis2dr1cs2AP" ] }
  , { key: "cis2dr2AP", value: [ "cis2dr2cs1AP", "cis2dr2cs2AP" ] }
  , { key: "customerInitialBP", value: [ "cis1BP", "cis2BP", "cis3BP", "cis4BP" ] }
  , { key: "cis1BP", value: [ "cis1dr1BP", "cis1dr2BP", "cis1dr3BP", "cis1dr4BP" ] }
  , { key: "cis2BP", value: [ "cis2dr1BP", "cis2dr2BP", "cis2dr3BP", "cis2dr4BP" ] }
  , { key: "cis3BP", value: [ "cis3dr1BP", "cis3dr2BP", "cis3dr3BP", "cis3dr4BP" ] }
  , { key: "cis4BP", value: [ "cis4dr1BP", "cis4dr2BP", "cis4dr3BP", "cis4dr4BP" ] }
  , { key: "cis1dr1BP", value: [ "cis1dr1cs1BP", "cis1dr1cs2BP" ] }
  , { key: "cis1dr2BP", value: [ "cis1dr2cs1BP", "cis1dr2cs2BP" ] }
  , { key: "cis1dr3BP", value: [ "cis1dr3cs1BP", "cis1dr3cs2BP" ] }
  , { key: "cis1dr4BP", value: [ "cis1dr4cs1BP", "cis1dr4cs2BP" ] }
  , { key: "cis2dr1BP", value: [ "cis2dr1cs1BP", "cis2dr1cs2BP" ] }
  , { key: "cis2dr2BP", value: [ "cis2dr2cs1BP", "cis2dr2cs2BP" ] }
  , { key: "cis2dr3BP", value: [ "cis2dr3cs1BP", "cis2dr3cs2BP" ] }
  , { key: "cis2dr4BP", value: [ "cis2dr4cs1BP", "cis2dr4cs2BP" ] }
  , { key: "cis3dr1BP", value: [ "cis3dr1cs1BP", "cis3dr1cs2BP" ] }
  , { key: "cis3dr2BP", value: [ "cis3dr2cs1BP", "cis3dr2cs2BP" ] }
  , { key: "cis3dr3BP", value: [ "cis3dr3cs1BP", "cis3dr3cs2BP" ] }
  , { key: "cis3dr4BP", value: [ "cis3dr4cs1BP", "cis3dr4cs2BP" ] }
  , { key: "cis4dr1BP", value: [ "cis4dr1cs1BP", "cis4dr1cs2BP" ] }
  , { key: "cis4dr2BP", value: [ "cis4dr2cs1BP", "cis4dr2cs2BP" ] }
  , { key: "cis4dr3BP", value: [ "cis4dr3cs1BP", "cis4dr3cs2BP" ] }
  , { key: "cis4dr4BP", value: [ "cis4dr4cs1BP", "cis4dr4cs2BP" ] }
  , { key: "customerDefaultAP", value: [ "cds1AP", "cds2AP" ] }
  , { key: "customerDefaultBP", value: [ "cds1BP", "cds2BP" ] }
  , { key: "driverInitialAP", value: [ "dis1AP", "dis2AP", "dis3AP" ] }
  , { key: "dis1AP", value: [ "dis1cr1AP", "dis1cr2AP", "dis1cr3AP" ] }
  , { key: "dis2AP", value: [ "dis2cr1AP", "dis2cr2AP", "dis2cr3AP" ] }
  , { key: "dis3AP", value: [ "dis3cr1AP", "dis3cr2AP" ] }
  , { key: "dis1cr1AP", value: [ "dis1cr1ds1AP", "dis1cr1ds2AP" ] }
  , { key: "dis1cr2AP", value: [ "dis1cr2ds1AP" ] }
  , { key: "dis1cr3AP", value: [ "dis1cr3ds1AP", "dis1cr3ds2AP" ] }
  , { key: "dis2cr1AP", value: [ "dis2cr1ds1AP", "dis2cr1ds2AP" ] }
  , { key: "dis2cr2AP", value: [ "dis2cr2ds1AP" ] }
  , { key: "dis2cr3AP", value: [ "dis2cr3ds1AP", "dis2cr3ds2AP" ] }
  , { key: "driverInitialBP", value: [ "dis1BP", "dis2BP", "dis3BP", "dis4BP" ] }
  , { key: "dis1BP", value: [ "dis1cr1BP", "dis1cr2BP", "dis1cr3BP" ] }
  , { key: "dis2BP", value: [ "dis2cr1BP", "dis2cr2BP", "dis2cr3BP" ] }
  , { key: "dis3BP", value: [ "dis3cr1BP", "dis3cr2BP", "dis3cr3BP" ] }
  , { key: "dis4BP", value: [ "dis4cr1BP", "dis4cr2BP", "dis4cr3BP" ] }
  , { key: "dis1cr1BP", value: [ "dis1cr1ds1BP", "dis1cr1ds2BP" ] }
  , { key: "dis1cr2BP", value: [ "dis1cr2ds1BP", "dis1cr2ds2BP" ] }
  , { key: "dis1cr3BP", value: [ "dis1cr3ds1BP", "dis1cr3ds2BP" ] }
  , { key: "dis2cr1BP", value: [ "dis2cr1ds1BP", "dis2cr1ds2BP" ] }
  , { key: "dis2cr2BP", value: [ "dis2cr2ds1BP", "dis2cr2ds2BP" ] }
  , { key: "dis2cr3BP", value: [ "dis2cr3ds1BP", "dis2cr3ds2BP" ] }
  , { key: "dis3cr1BP", value: [ "dis3cr1ds1BP", "dis3cr1ds2BP" ] }
  , { key: "dis3cr2BP", value: [ "dis3cr2ds1BP", "dis3cr2ds2BP" ] }
  , { key: "dis3cr3BP", value: [ "dis3cr3ds1BP", "dis3cr3ds2BP" ] }
  , { key: "dis4cr1BP", value: [ "dis4cr1ds1BP", "dis4cr1ds2BP" ] }
  , { key: "dis4cr2BP", value: [ "dis4cr2ds1BP", "dis4cr2ds2BP" ] }
  , { key: "dis4cr3BP", value: [ "dis4cr3ds1BP", "dis4cr3ds2BP" ] }
  , { key: "driverOverlayDefaultAP", value: [ "dols1AP", "dols2AP", "dols3AP" ] }
  , { key: "driverOverlayDefaultBP", value: [ "dols1BP", "dols2BP", "dols3BP" ] }
  , { key: "driverDefaultAP", value: [ "dds1AP", "dds2AP", "dds3AP" ] }
  , { key: "driverDefaultBP", value: [ "dds1BP", "dds2BP", "dds3BP" ] }
  ]

type Suggestions
  = Array
      { key :: String
      , value :: Array String
      }

type SuggestionDefinitions
  = Array
      { key :: String
      , value :: { en_us :: String, ta_in :: String, kn_in :: String, hi_in :: String, ml_in :: String, bn_in :: String }
      }

getMessageFromKey :: String -> String -> String
getMessageFromKey key language = do
  let
    decodedMessage = (getSuggestionfromKey key language)
  if decodedMessage == "" then do
    let
      suggestions = (suggestionsDefinitions "")

      message = filter (\item -> item.key == key) suggestions
    case head message of
      Just value -> case language of
        "EN_US" -> value.value.en_us
        "HI_IN" -> value.value.hi_in
        "KN_IN" -> value.value.kn_in
        "BN_IN" -> value.value.bn_in
        "ML_IN" -> value.value.ml_in
        "TA_IN" -> value.value.ta_in
        _ -> value.value.en_us
      Nothing -> key
  else
    decodedMessage

getSuggestionsfromKey :: String -> Array String
getSuggestionsfromKey key = do
  let
    decodedSuggestions = getSuggestionsfromLocal key
  case (head decodedSuggestions) of
    Just value ->
      if value == "error" then do
        let
          suggestions = (getSuggestions "")

          replies = filter (\item -> item.key == key) suggestions
        concatMap (\item -> item.value) replies
      else
        decodedSuggestions
    Nothing -> []
