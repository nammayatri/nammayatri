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
Write-Host "=== 8. Our own routing engine - real Algerian roads ===" -ForegroundColor Cyan
$routes = @(
    @{ name = "Algiers centre -> Bab Ezzouar"; from = "3.0588,36.7538"; to = "3.1836,36.7169" },
    @{ name = "Algiers -> Oran";               from = "3.0588,36.7538"; to = "-0.6331,35.6969" },
    @{ name = "Algiers -> Constantine";        from = "3.0588,36.7538"; to = "6.6147,36.3650" }
)
foreach ($r in $routes) {
    $o = Invoke-RestMethod -Uri "http://localhost:5001/route/v1/driving/$($r.from);$($r.to)?overview=false"
    $km = [math]::Round($o.routes[0].distance / 1000, 1)
    $hrs = $o.routes[0].duration / 3600
    $time = if ($hrs -ge 1) { "{0:N1} hours" -f $hrs } else { "{0:N0} min" -f ($hrs * 60) }
    Write-Host ("   {0,-32} {1,8} km   {2}" -f $r.name, $km, $time) -ForegroundColor Green
}
Write-Host "   (no Google, no API key - OpenStreetMap data on our own server)" -ForegroundColor DarkGray

Write-Host ""
Write-Host "=== 9. A real ride search in Algiers ===" -ForegroundColor Cyan
$search = @{
    fareProductType = "ONE_WAY"
    contents = @{
        origin = @{ address = @{ area="Alger Centre"; city="Algiers"; country="Algeria"; state="Alger"; building="1"; areaCode="16000"; street="Rue Didouche Mourad"; door="1" }; gps = @{ lat=36.7538; lon=3.0588 } }
        destination = @{ address = @{ area="Bab Ezzouar"; city="Algiers"; country="Algeria"; state="Alger"; building="2"; areaCode="16000"; street="Route"; door="2" }; gps = @{ lat=36.7169; lon=3.1836 } }
    }
} | ConvertTo-Json -Depth 6
$sr = Invoke-RestMethod -Uri "$b/v2/rideSearch" -Method Post -Headers @{ "content-type"="application/json"; "token"=$verify.token } -Body $search
Write-Host "   searchId      : $($sr.searchId)" -ForegroundColor Green
Write-Host "   route distance: $([math]::Round($sr.routeInfo.distance/1000,2)) km" -ForegroundColor Green
Write-Host "   route duration: $([math]::Round($sr.routeInfo.duration/60,1)) min" -ForegroundColor Green
Write-Host "   route points  : $($sr.routeInfo.points.Count)  (the actual line drawn on the map)" -ForegroundColor Green

Write-Host ""
Write-Host "=== DONE - both sides operational, serving Algeria, real routing ===" -ForegroundColor Yellow
Write-Host ""
