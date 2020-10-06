let payee = "{\"type\":\"bank_account\",\"bank_account\":{\"account_number\":\"999999999999\",\"account_holder_name\":\"Dunzo Digital Private Limited\",\"ifsc_code\":\"IFSC001\"},\"person\":{\"name\":{\"given_name\":\"Dunzo\"},\"gender\":\"\",\"phones\":[]}}"

let dlPayee = "<payment endpoint json stringified>"

in

{ dbUserId = "atlas"
, dbPassword = "atlas"
, payee = payee
, dlPayee = dlPayee
}
