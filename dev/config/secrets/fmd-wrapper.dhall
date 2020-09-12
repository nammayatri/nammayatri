let paymentPolicy = "{\"credit_type\":\"POSTPAID\",\"supported_currencies\":[\"INR\"],\"mode\":\"RTGS\",\"penalty_terms\":[\"Delay in payment after due date will incur 10 INR per day of non-payment\"],\"credit_duration\":\"P30D\",\"method\":\"ELECTRONIC\",\"settlement_type\":\"BULK\"}"

let payee = "{\"type\":\"bank_account\",\"bank_account\":{\"account_number\":\"999999999999\",\"account_holder_name\":\"Dunzo Digital Private Limited\",\"ifsc_code\":\"IFSC001\"},\"person\":{\"name\":{\"given_name\":\"Dunzo\"},\"gender\":\"\",\"phones\":[]}}"

in

{ dbUserId = "atlas"
, dbPassword = "atlas"
, paymentPolicy = paymentPolicy
, payee = payee
}
