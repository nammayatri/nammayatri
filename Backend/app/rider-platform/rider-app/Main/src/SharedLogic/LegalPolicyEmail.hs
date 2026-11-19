module SharedLogic.LegalPolicyEmail
  ( RenderedLegalPolicyEmail (..),
    renderLegalPolicyEmail,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Domain.Types.EmailTemplate as DET
import qualified Domain.Types.PolicyAndComplianceDocument as DPCD
import Kernel.Prelude
import Lib.Finance.Invoice.RenderTemplate (buildTemplateChecked, escHtml)

data RenderedLegalPolicyEmail = RenderedLegalPolicyEmail
  { subject :: Text,
    html :: Text,
    missingKeys :: [Text]
  }

renderLegalPolicyEmail :: DET.EmailTemplate -> DET.LegalPolicyEmailConfig -> DPCD.PolicyAndComplianceDocument -> RenderedLegalPolicyEmail
renderLegalPolicyEmail template config policy =
  let vars =
        [ ("policyName", fromMaybe (show policy.policyType) (Map.lookup policy.policyType config.policyTypeLabels)),
          ("version", policy.version),
          ("url", policy.url),
          ("mandatoryStyle", if policy.isMandatory then "" else "display:none;"),
          ("optionalStyle", if policy.isMandatory then "display:none;" else "")
        ]
      (renderedSubject, subjectMissing) = buildTemplateChecked vars template.subjectTemplate
      (renderedHtml, htmlMissing) = buildTemplateChecked (map (\(k, v) -> (k, escHtml v)) vars) template.htmlTemplate
   in RenderedLegalPolicyEmail
        { subject = T.strip renderedSubject,
          html = renderedHtml,
          missingKeys = subjectMissing <> htmlMissing
        }
