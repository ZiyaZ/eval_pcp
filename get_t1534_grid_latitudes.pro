; Use wgrib to get latitudes of rows on the T1534 3072 x 1536 Gaussian
; grid used for the 13 km GFS.

; Greg Fall, OWP-Chanhassen
; April 2018

PRO GET_T1534_GRID_LATITUDES, GRIBFilePath, latGrid_GFS13

; Read a GFS 13 km GRIB file to get latitudes for the T1534 (3072 x
; 1536) Gaussian grid.

  if NOT(FILE_TEST(GRIBFilePath)) then STOP

  cmd='wgrib2 -g2clib 0 -match ":PRATE:" -grid ' + GRIBFilePath
  SPAWN, cmd, wgrib2Out, EXIT_STATUS = status
  if (status ne 0) then STOP
;7:15314696:grid_template=40:winds(N/S):
;	Gaussian grid: (3072 x 1536) units 1e-06 input WE:NS output WE:SN
;	number of latitudes between pole-equator=768 #points=4718592
;	lat 89.910324 to -89.910324
;	lon 0.000000 to 359.882813 by 0.117188
  if (N_ELEMENTS(wgrib2Out) ne 5) then STOP
;  if NOT(STREGEX(wgrib2Out[1], 'Gaussian grid: (', /BOOLEAN)) then STOP
  c1 = STRPOS(wgrib2Out[1], 'Gaussian grid: (') + 16
  if (c1 eq 15) then STOP
  c2 = STRPOS(wgrib2Out[1], ' x ', c1)
  if (c2 eq -1) then STOP
  nCols = LONG(STRMID(wgrib2Out[1], c1, c2 - c1))
  c1 = c2 + 3
  c2 = STRPOS(wgrib2Out[1], ')', c1)
  if (c2 eq -1) then STOP
  nRows = LONG(STRMID(wgrib2Out[1], c1, c2 - c1))
  latGrid_GFS13 = DBLARR(nRows)
  for rc = 1, nRows do begin
      cmd = 'wgrib2 -g2clib 0 -match ":PRATE:" -ijlat 1 ' + STRCRA(rc) + ' ' + GRIBFilePath
      SPAWN, cmd, wgrib2Out, EXIT_STATUS = status
      if (status ne 0) then STOP
      if (N_ELEMENTS(wgrib2Out) ne 1) then STOP
      c1 = STRPOS(wgrib2Out[0], 'lat=') + 4
      if (c1 eq 3) then STOP
      c2 = STRPOS(wgrib2Out[0], ',', c1)
      if (c2 eq -1) then STOP
      latGrid_GFS13[rc - 1] = DOUBLE(STRMID(wgrib2Out[0], c1, c2))
      print, rc, latGrid_GFS13[rc - 1]
  endfor

  RETURN

end
