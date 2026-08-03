# Namma Yatri backend — live demo
# Proves the self-hosted backend works: login, OTP verify, authenticated API call.

$b = "http://localhost:8014"

Write-Host ""
Write-Host "=== 1. Backend containers running ===" -ForegroundColor Cyan
docker ps --format "table {{.Names}}\t{{.Status}}" | Select-String -Pattern "NAMES|ny-"

Write-Host ""
Write-Host "=== 2. Database: schema + seeded merchant ===" -ForegroundColor Cyan
docker exec ny-postgres psql -U postgres -d atlas_dev -c "SELECT (SELECT count(*) FROM information_schema.tables WHERE table_schema='atlas_app') AS tables, (SELECT count(*) FROM atlas_app.merchant) AS merchants, (SELECT short_id FROM atlas_app.merchant LIMIT 1) AS merchant_name;"

Write-Host ""
Write-Host "=== 2b. Service areas: Algeria ===" -ForegroundColor Cyan
docker exec ny-postgres psql -U postgres -d atlas_dev -c "SELECT region, ST_NPoints(geom) AS boundary_points FROM atlas_app.geometry WHERE region IN ('Algiers','Oran','Annaba') ORDER BY region;"
docker exec ny-postgres psql -U postgres -d atlas_dev -c "SELECT short_id, origin_restriction AS serves FROM atlas_app.merchant;"

Write-Host ""
Write-Host "=== 3. Login: request OTP ===" -ForegroundColor Cyan
$h = @{ "content-type" = "application/json"; "x-bundle-version" = "1.0.1"; "x-client-version" = "1.0.0" }
$body = '{"mobileCountryCode":"+91","mobileNumber":"9999900001","merchantId":"YATRI"}'
$auth = Invoke-RestMethod -Uri "$b/v2/auth" -Method Post -Headers $h -Body $body
Write-Host "   authId  : $($auth.authId)" -ForegroundColor Green
Write-Host "   attempts: $($auth.attempts)" -ForegroundColor Green

Write-Host ""
Write-Host "=== 4. Verify OTP -> get session token ===" -ForegroundColor Cyan
$verify = Invoke-RestMethod -Uri "$b/v2/auth/$($auth.authId)/verify" -Method Post `
    -Headers @{ "content-type" = "application/json" } `
    -Body '{"otp":"7891","deviceToken":"demo"}'
Write-Host "   token    : $($verify.token)" -ForegroundColor Green
Write-Host "   person id: $($verify.person.id)" -ForegroundColor Green
Write-Host "   mobile   : $($verify.person.maskedMobileNumber)  (encrypted + masked)" -ForegroundColor Green

Write-Host ""
Write-Host "=== 5. Authenticated business call: is this pickup point serviceable? ===" -ForegroundColor Cyan
$hdr = @{ "content-type" = "application/json"; "token" = $verify.token }
$points = @(
    @{ name = "Algiers - city centre";  lat = 36.7538;     lon = 3.0588 },
    @{ name = "Algiers - airport";      lat = 36.6910;     lon = 3.2154 },
    @{ name = "Oran - city centre";     lat = 35.6969;     lon = -0.6331 },
    @{ name = "Annaba - city centre";   lat = 36.9000;     lon = 7.7667 },
    @{ name = "Constantine (not ours)"; lat = 36.3650;     lon = 6.6147 },
    @{ name = "Bangalore, India";       lat = 12.9715987;  lon = 77.5945627 }
)
foreach ($p in $points) {
    $body = '{"location":{"lat":' + $p.lat + ',"lon":' + $p.lon + '}}'
    $r = Invoke-RestMethod -Uri "$b/v2/serviceability/origin" -Method Post -Headers $hdr -Body $body
    $colour = if ($r.serviceable) { "Green" } else { "DarkGray" }
    Write-Host ("   {0,-24} {1,10}, {2,-11}  serviceable: {3}" -f $p.name, $p.lat, $p.lon, $r.serviceable) -ForegroundColor $colour
}

Write-Host ""
Write-Host "=== DONE - backend fully operational, serving Algeria ===" -ForegroundColor Yellow
Write-Host ""
