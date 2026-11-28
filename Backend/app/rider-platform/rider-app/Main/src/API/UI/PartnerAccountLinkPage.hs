{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | The hosted page of partner account linking. Only the HTML lives here: NammaDSL has
-- no HTML response type, so this route is hand-written, as the business-email
-- verification page in API.UI.Registration is. The page's JSON calls are generated from
-- spec/API/PartnerAccountLink.yaml; the flow is in "Domain.Action.UI.PartnerAccountLink".
--
--   GET /partnerOrg/accountLink/page/{linkSecret}
module API.UI.PartnerAccountLinkPage
  ( API,
    handler,
  )
where

import API.UI.Registration (HTMLResponse (..))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as TE
import Environment
import EulerHS.Prelude
import Kernel.Utils.Servant.HTML
import Servant

type API =
  "partnerOrg"
    :> "accountLink"
    :> "page"
    :> Capture "linkSecret" Text
    :> Get '[HTML] HTMLResponse

handler :: FlowServer API
handler = page

-- | The page is static, and the link secret is read by the page's script from its own
-- URL, never written into the markup: what the page shows comes from /info as JSON and
-- is set with textContent, so nothing a partner or rider sends can inject markup.
page :: Text -> FlowHandler HTMLResponse
page _ = pure . HTMLResponse . BSL.fromStrict $ TE.encodeUtf8 pageHtml

pageHtml :: Text
pageHtml =
  "<!DOCTYPE html>\
  \<html lang='en'><head>\
  \<meta charset='utf-8'>\
  \<meta name='viewport' content='width=device-width,initial-scale=1'>\
  \<meta name='referrer' content='no-referrer'>\
  \<title>Link your Namma Yatri account</title>\
  \<style>\
  \body{font-family:system-ui,-apple-system,sans-serif;margin:0;padding:24px 16px;background:#fff;color:#111}\
  \.c{max-width:420px;margin:0 auto}\
  \h1{font-size:22px;margin:0 0 12px}\
  \p{line-height:1.45;color:#333}\
  \input{width:100%;box-sizing:border-box;font-size:18px;padding:12px;margin:8px 0;border:1px solid #bbb;border-radius:8px}\
  \button{width:100%;font-size:17px;padding:14px;margin-top:12px;border:0;border-radius:8px;background:#fcc32c;color:#111;font-weight:600}\
  \button.alt{background:#eee}\
  \button:disabled{opacity:.5}\
  \.err{color:#b00020;min-height:1.4em}\
  \.hide{display:none}\
  \</style></head><body><div class='c'>\
  \<h1>Link your Namma Yatri account</h1>\
  \<div id='s-load'><p>Loading…</p></div>\
  \<div id='s-otp' class='hide'>\
  \<p><b class='partner'></b> wants to book and manage Namma Yatri rides for you. Rides it books show in your Namma Yatri app, and you can cancel them there.</p>\
  \<p>Enter the OTP sent to <b id='mobile'></b>.</p>\
  \<input id='otp' inputmode='numeric' autocomplete='one-time-code' maxlength='4' placeholder='4-digit OTP'>\
  \<button id='verify'>Verify</button>\
  \<button id='resend' class='alt'>Resend OTP</button>\
  \</div>\
  \<div id='s-signup' class='hide'>\
  \<p>This number has no Namma Yatri account yet. Create one to continue.</p>\
  \<input id='first' placeholder='First name' maxlength='50'>\
  \<input id='last' placeholder='Last name (optional)' maxlength='50'>\
  \<button id='create'>Create account</button>\
  \</div>\
  \<div id='s-done' class='hide'><p>Your Namma Yatri account is linked to <b class='partner'></b>. You can close this page.</p></div>\
  \<div id='s-dead' class='hide'><p id='dead-msg'>This link is invalid or has expired. Start again from the app.</p></div>\
  \<p class='err' id='err'></p>\
  \</div><script>\
  \var base=location.pathname.replace(/\\/$/,'');\
  \var screens={ENTER_OTP:'s-otp',SIGNUP:'s-signup',DONE:'s-done'};\
  \function $(i){return document.getElementById(i)}\
  \function show(s){['s-load','s-otp','s-signup','s-done','s-dead'].forEach(function(x){$(x).classList.toggle('hide',x!==s)})}\
  \function err(m){$('err').textContent=m||''}\
  \function call(path,body){return fetch(base+path,{method:body===undefined?'GET':'POST',headers:{'content-type':'application/json'},body:body===undefined?undefined:JSON.stringify(body)}).then(function(r){return r.json().then(function(j){if(!r.ok){throw j}return j})})}\
  \function render(i){document.querySelectorAll('.partner').forEach(function(e){e.textContent=i.partnerName});$('mobile').textContent=i.maskedMobileNumber;err('');show(screens[i.step]);if(i.step==='ENTER_OTP'){$('otp').focus()}}\
  \function fail(e){var code=e&&e.errorCode;var msg=(e&&e.errorMessage)||'Something went wrong. Try again.';\
  \if(code==='ACCOUNT_LINK_NOT_FOUND'||code==='ACCOUNT_LINK_OTP_ATTEMPTS_EXCEEDED'||code==='ACCOUNT_LINK_PERSON_BLOCKED'){$('dead-msg').textContent=msg;show('s-dead');err('')}else{err(msg)}}\
  \function run(btn,p){$(btn).disabled=true;err('');p.then(function(r){if(r&&r.step){render(r)}}).catch(fail).then(function(){$(btn).disabled=false})}\
  \call('/info').then(render).catch(fail);\
  \$('verify').onclick=function(){run('verify',call('/verifyOtp',{otp:$('otp').value.trim()}))};\
  \$('resend').onclick=function(){run('resend',call('/resendOtp',{}).then(function(){err('OTP sent again.')}))};\
  \$('create').onclick=function(){run('create',call('/signup',{firstName:$('first').value.trim(),lastName:$('last').value.trim()||null}))};\
  \</script></body></html>"
