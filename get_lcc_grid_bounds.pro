PRO GET_LCC_GRID_BOUNDS, LCCGridInfo, $
                         minLon, maxLon, minLat, maxLat, $
                         CENTER = center

;+
; Orbit the circumference of a Lambert conformal grid to find the
; minimum and maximum longitudes and latitudes in the map domain.
;
; Keyword CENTER instructs the procedure to use grid centers. The
; default is to use the grid cell edges to establish bounds.
;-

  COMMON info, message

  minLon = !NULL
  maxLon = !NULL
  minLat = !NULL
  maxLat = !NULL

  infoSize = SIZE(LCCGridInfo)
  if (infoSize[2] ne 8) then begin
      ERR_MSG, 'Missing LCCGridInfo structure.'
      RETURN
  endif
  if NOT(ISA(LCCGridInfo.lonV, 'DOUBLE')) then STOP
  if NOT(ISA(LCCGridInfo.latD, 'DOUBLE')) then STOP
  if NOT(ISA(LCCGridInfo.latSec1, 'DOUBLE')) then STOP
  if NOT(ISA(LCCGridInfo.latSec2, 'DOUBLE')) then STOP
  if NOT(ISA(LCCGridInfo.eRadM, 'DOUBLE')) then STOP
  if NOT(ISA(LCCGridInfo.lat00, 'DOUBLE')) then STOP
  if NOT(ISA(LCCGridInfo.lon00, 'DOUBLE')) then STOP
  if NOT(ISA(LCCGridInfo.nCols, 'LONG')) then STOP
  if NOT(ISA(LCCGridInfo.nRows, 'LONG')) then STOP
  if NOT(ISA(LCCGridInfo.dx, 'DOUBLE')) then STOP
  if NOT(ISA(LCCGridInfo.dy, 'DOUBLE')) then STOP

;+
; Get Snyder parameters for grid/projection and corner coordinates.
;-
  LCC_GRIB2_TO_SNYDER, LCCGridInfo.latSec1, $
                       LCCGridInfo.latSec2, $
                       LCCGridInfo.latD, $
                       LCCGridInfo.lonV, $
                       LCCGridInfo.lat00, $
                       LCCGridInfo.lon00, $
                       LCCGridInfo.eRadM, $
                       lonV_rad, $
                       nSny, $
                       FSny, $
                       rho0, $
                       x00, $
                       y00

;+
; Generate x and y arrays to test.
;-
  if KEYWORD_SET(center) then edgeFlag = 0 else edgeFlag = 1
  x00Test = x00 - 0.5D * LCCGridInfo.dx * edgeFlag
  y00Test = y00 - 0.5D * LCCGridInfo.dy * edgeFlag
  nxTest = LCCGridInfo.nCols + 1L * edgeFlag
  nyTest = LCCGridInfo.nRows + 1L * edgeFlag
  xTest = x00Test + DINDGEN(nxTest) * LCCGridInfo.dx
  yTest = y00Test + DINDGEN(nyTest) * LCCGridInfo.dy

  minLon_rad = 1.0D8
  maxLon_rad = -1.0D8
  minLat_rad = 1.0D8
  maxLat_rad = -1.0D8

;+
; Bottom edge.
;-
  y = yTest[0]
  for cc = 0, nxTest - 1 do begin
      x = xTest[cc]
      rho = (nSny / ABS(nSny)) * $
            SQRT(x^2.0D + (rho0 - y)^2.0D) ; 14-10
      theta_rad = ATAN(x / (rho0 - y))     ; 14-11
      lat_rad = 2.0D * ATAN((LCCGridInfo.eRadM * $ ; 15-5
                             FSny / rho)^(1.0D / nSny)) - $
                !DPi / 2.0D
      lon_rad = theta_rad / nSny + lonV_rad ; 14-9
      minLon_rad = minLon_rad < lon_rad
      maxLon_rad = maxLon_rad > lon_rad
      minLat_rad = minLat_rad < lat_rad
      maxLat_rad = maxLat_rad > lat_rad
  endfor

;+
; Right edge.
;-
  x = xTest[nxTest - 1]
  for rc = 0, nyTest - 1 do begin
      y = yTest[rc]
      rho = (nSny / ABS(nSny)) * $
            SQRT(x^2.0D + (rho0 - y)^2.0D) ; 14-10
      theta_rad = ATAN(x / (rho0 - y))     ; 14-11
      lat_rad = 2.0D * ATAN((LCCGridInfo.eRadM * $ ; 15-5
                             FSny / rho)^(1.0D / nSny)) - $
                !DPi / 2.0D
      lon_rad = theta_rad / nSny + lonV_rad ; 14-9
      minLon_rad = minLon_rad < lon_rad
      maxLon_rad = maxLon_rad > lon_rad
      minLat_rad = minLat_rad < lat_rad
      maxLat_rad = maxLat_rad > lat_rad
  endfor

;+
; Top edge.
;-
  y = yTest[nyTest - 1]
  for cc = nxTest - 1, 0, -1 do begin
      x = xTest[cc]
      rho = (nSny / ABS(nSny)) * $
            SQRT(x^2.0D + (rho0 - y)^2.0D) ; 14-10
      theta_rad = ATAN(x / (rho0 - y))     ; 14-11
      lat_rad = 2.0D * ATAN((LCCGridInfo.eRadM * $ ; 15-5
                             FSny / rho)^(1.0D / nSny)) - $
                !DPi / 2.0D
      lon_rad = theta_rad / nSny + lonV_rad ; 14-9
      minLon_rad = minLon_rad < lon_rad
      maxLon_rad = maxLon_rad > lon_rad
      minLat_rad = minLat_rad < lat_rad
      maxLat_rad = maxLat_rad > lat_rad
  endfor

;+
; Left edge.
;-
  x = xTest[0]
  for rc = nyTest - 1, 0, -1 do begin
      y = yTest[rc]
      rho = (nSny / ABS(nSny)) * $
            SQRT(x^2.0D + (rho0 - y)^2.0D) ; 14-10
      theta_rad = ATAN(x / (rho0 - y))     ; 14-11
      lat_rad = 2.0D * ATAN((LCCGridInfo.eRadM * $ ; 15-5
                             FSny / rho)^(1.0D / nSny)) - $
                !DPi / 2.0D
      lon_rad = theta_rad / nSny + lonV_rad ; 14-9
      minLon_rad = minLon_rad < lon_rad
      maxLon_rad = maxLon_rad > lon_rad
      minLat_rad = minLat_rad < lat_rad
      maxLat_rad = maxLat_rad > lat_rad
  endfor

  radToDeg = 180.0D / !DPi
  minLon = minLon_rad * radToDeg
  maxLon = maxlon_rad * radToDeg
  minLat = minLat_rad * radToDeg
  maxLat = maxLat_rad * radToDeg

  RETURN

end
