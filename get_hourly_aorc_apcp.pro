PRO GET_HOURLY_AORC_APCP, $
    date_YYYYMMDDHH, $ ; hourly accumulation end date/time
    AORCDir, $
    scratchDir, $
    ndv, $
    APCPGrid, $
    AORC_GRID_INFO = AORCGridInfo, $
    VERBOSE = verbose


; Get hourly AORC total precipitation (APCP).

  COMMON info, message

  APCPGrid = !NULL


; Check arguments for correct type and valid contents.

  if NOT(ISA(date_YYYYMMDDHH, 'STRING')) then begin
      ERR_MSG, 'Target date/time argument must be a STRING.'
      RETURN
  endif
  if (STRLEN(date_YYYYMMDDHH) ne 10) then begin
      ERR_MSG, 'Invalid target date/time "' + $
               date_YYYYMMDDHH + $
               '" (required form is YYYYMMDDHH, 10 digits).'
      RETURN
  endif
  if NOT(STREGEX(date_YYYYMMDDHH, '[0-9]{10}', /BOOLEAN)) $
      then begin
      ERR_MSG, 'Invalid target date/time "' + $
               date_YYYYMMDDHH + $
               '" (required form is YYYYMMDDHH, all numeric).'
      RETURN
  endif

  date_Julian = YYYYMMDDHH_TO_JULIAN(date_YYYYMMDDHH)

  if NOT(ISA(AORCDir, 'STRING')) then begin
      ERR_MSG, 'Location of AORC archive must be a STRING.'
      RETURN
  endif

  if NOT(FILE_TEST(AORCDir, /DIRECTORY)) then begin
      ERR_MSG, 'AORC archive directory "' + AORCDir + '" not found.'
      RETURN
  endif

  if NOT(FILE_TEST(AORCDir, /READ)) then begin
      ERR_MSG, 'AORC archive directory "' + AORCDir + '" not readable.'
      RETURN
  endif

  if NOT(ISA(scratchDir, 'STRING')) then begin
      ERR_MSG, 'Location of scratch directory must be a STRING.'
      RETURN
  endif

  if NOT(FILE_TEST(scratchDir, /DIRECTORY)) then begin
      ERR_MSG, 'Scratch directory "' + scratchDir + '" not found.'
      RETURN
  endif

  if NOT(FILE_TEST(scratchDir, /READ)) then begin
      ERR_MSG, 'Scratch directory "' + scratchDir + '" not readable.'
      RETURN
  endif

  if NOT(FILE_TEST(scratchDir, /WRITE)) then begin
      ERR_MSG, 'Scratch directory "' + scratchDir + '" not writeable.'
      RETURN
  endif

  if NOT(ISA(ndv, 'FLOAT')) then $
      ERR_MSG, 'WARNING: no-data value should be a floating point value.'


; Break up date info and locate GRIB file.

  date_YYYYMMDD = STRMID(date_YYYYMMDDHH, 0, 8)
  date_YYYY = STRMID(date_YYYYMMDDHH, 0, 4)
  date_MM = STRMID(date_YYYYMMDDHH, 4, 2)
  date_DD = STRMID(date_YYYYMMDDHH, 6, 2)
  date_HH = STRMID(date_YYYYMMDDHH, 8, 2)

  GRIBDir = AORCDir + $
            '/' + date_YYYY + $
            '/' + date_MM + $
            '/' + date_DD

  GRIBFile = 'AORC-OWP_' + date_YYYYMMDD  + date_HH + $
             'z.grib2'

  GRIBFilePath = GRIBDir + '/' + GRIBFile
;  print, GRIBFilePath

  if NOT(FILE_TEST(GRIBFilePath)) then STOP

  tmpFile = '__' + GRIBFile + '__.tmp'

  GRIBField = ':APCP:surface:0-1 hour acc fcst:'
                             
  APCPGrid_ = !NULL

  DECODE_GRIB2_RECORD, GRIBFilePath, $
                       scratchDir, $
                       GRIBField, $
;                       'APCP', $
                       record, $
                       vt, $
                       lev, $
                       ftime, $
                       varAbbrev, $
                       varField, $
                       varUnits, $
                       nCols_, $
                       nRows_, $
                       APCPGrid_, $
                       NO_DATA_VALUE = ndv 
 if NOT(ISA(APCPGrid_)) then STOP



  if NOT(KEYWORD_SET(AORCGridInfo)) then begin

      AORCGridInfo = {minLon: 0.0D, $
                       maxLon: 0.0D, $
                       nCols: 0L, $
                       lonRes: 0.0D, $
                       minLat: 0.0D, $
                       maxLat: 0.0D, $
                       nRows: 0L, $
                       latRes: 0.0D}

      GET_GRIB2_LON_LAT_GRID_INFO, GRIBFile, $
                                   GRIBDir, $
                                   scratchDir, $
                                   GRIBField, $
;                                   'APCP', $
                                   nCols, $
                                   nRows, $
                                   lat1, lat2, latRes, $
                                   lon1, lon2, lonRes, $
                                   status

      if NOT(status) then begin
          ERR_MSG, 'Failed to read GRIB header from ' + $
                   GRIBFilePath
          RETURN
      endif


;     Round latitude/longitude/degree variables to the nearest 0.1
;     arcsec to improve precision.

      lat1 = ROUND(lat1 * 36000L) / 36000.0D
      lat2 = ROUND(lat2 * 36000L) / 36000.0D
      lon1 = ROUND(lon1 * 36000L) / 36000.0D
      lon2 = ROUND(lon2 * 36000L) / 36000.0D
      lonRes = ROUND(lonRes * 36000L) / 36000.0D
      latRes = ROUND(latRes * 36000L) / 36000.0D

      while (lon1 gt 180.0D) do lon1 = lon1 - 360.0D
      while (lon2 gt 180.0D) do lon2 = lon2 - 360.0D


      AORCGridInfo.nCols = nCols
      AORCGridInfo.lonRes = lonRes
      AORCGridInfo.nRows = nRows
      AORCGridInfo.latRes = latRes
      AORCGridInfo.minLon = lon1 - 0.5D * AORCGridInfo.lonRes
      AORCGridInfo.maxLon = lon2 + 0.5D * AORCGridInfo.lonRes
      AORCGridInfo.minLat = lat1 - 0.5D * AORCGridInfo.latRes
      AORCGridInfo.maxLat = lat2 + 0.5D * AORCGridInfo.latRes

;  print, nCols, nRows, lat1, lat2, lon1, lon2
  endif else begin


;     Verify grid dimensions.

      if (nCols_ ne AORCGridInfo.nCols) then STOP
      if (nRows_ ne AORCGridInfo.nRows) then STOP

  endelse

  APCPGrid = TEMPORARY(APCPGrid_)

;  help, APCPGrid
;  print, 'Units: ', varUnits
;  print,'max=', max(APCPGrid), ';    mean=', mean(APCPGrid > 0)

  RETURN

end
