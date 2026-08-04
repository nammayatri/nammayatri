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
docker exec ny-postgres psql -U postgres -d atlas_dev -c "SELECT region, ST_NPoints(geom) AS boundary_points FROM atlas_app.geometry WHERE region IN ('Algeria','Algiers','Oran','Annaba') ORDER BY region;"
docker exec ny-postgres psql -U postgres -d atlas_dev -c "SELECT short_id, origin_restriction AS currently_serving FROM atlas_app.merchant;"

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
    @{ name = "Algiers - city centre"; lat = 36.7538;    lon = 3.0588 },
    @{ name = "Algiers - airport";     lat = 36.6910;    lon = 3.2154 },
    @{ name = "Oran";                  lat = 35.6969;    lon = -0.6331 },
    @{ name = "Annaba";                lat = 36.9000;    lon = 7.7667 },
    @{ name = "Constantine";           lat = 36.3650;    lon = 6.6147 },
    @{ name = "Tamanrasset (Sahara)";  lat = 22.7850;    lon = 5.5228 },
    @{ name = "Tunis, Tunisia";        lat = 36.8065;    lon = 10.1815 },
    @{ name = "Oujda, Morocco";        lat = 34.6867;    lon = -1.9114 },
    @{ name = "Bangalore, India";      lat = 12.9715987; lon = 77.5945627 }
)
foreach ($p in $points) {
    $body = '{"location":{"lat":' + $p.lat + ',"lon":' + $p.lon + '}}'
    $r = Invoke-RestMethod -Uri "$b/v2/serviceability/origin" -Method Post -Headers $hdr -Body $body
    $colour = if ($r.serviceable) { "Green" } else { "DarkGray" }
    Write-Host ("   {0,-24} {1,10}, {2,-11}  serviceable: {3}" -f $p.name, $p.lat, $p.lon, $r.serviceable) -ForegroundColor $colour
}

Write-Host ""
Write-Host "=== 6. The DRIVER side (a separate service, port 8017) ===" -ForegroundColor Cyan
docker exec ny-postgres psql -U postgres -d atlas_dev -c "SELECT (SELECT count(*) FROM atlas_driver_offer_bpp.merchant) AS merchants, (SELECT count(*) FROM atlas_driver_offer_bpp.person WHERE role='DRIVER') AS drivers, (SELECT count(*) FROM atlas_driver_offer_bpp.vehicle) AS vehicles, (SELECT count(*) FROM atlas_driver_offer_bpp.fare_policy) AS fare_policies;"

Write-Host ""
Write-Host "=== 7. A driver signs up and logs in ===" -ForegroundColor Cyan
$d = "http://localhost:8017"
$mid = (docker exec ny-postgres psql -U postgres -d atlas_dev -At -c "SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id='NAMMA_YATRI_PARTNER';").Trim()
$dbody = '{"mobileNumber":"9999905555","mobileCountryCode":"+91","merchantId":"' + $mid + '"}'
$dauth = Invoke-RestMethod -Uri "$d/ui/auth" -Method Post -Headers @{ "content-type" = "application/json" } -Body $dbody
Write-Host "   driver authId: $($dauth.authId)" -ForegroundColor Green
$dver = Invoke-RestMethod -Uri "$d/ui/auth/$($dauth.authId)/verify" -Method Post `
    -Headers @{ "content-type" = "application/json" } `
    -Body '{"otp":"7891","deviceToken":"demo-driver"}'
Write-Host "   driver token : $($dver.token)" -ForegroundColor Green
Write-Host "   role         : $($dver.person.role)" -ForegroundColor Green

Write-Host ""
Write-Host "=== DONE - both sides operational, serving Algeria ===" -ForegroundColor Yellow
Write-Host ""
