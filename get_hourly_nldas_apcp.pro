PRO GET_HOURLY_NLDAS_APCP, $
    date_YYYYMMDDHH, $ ; hourly accumulation end date/time
    NLDASDir, $
    scratchDir, $
    ndv, $
    APCPGrid, $
    NLDAS_GRID_INFO = NLDASGridInfo, $
    VERBOSE = verbose


; Get hourly NLDAS total precipitation (APCP).

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

  if NOT(ISA(NLDASDir, 'STRING')) then begin
      ERR_MSG, 'Location of NLDAS archive must be a STRING.'
      RETURN
  endif

  if NOT(FILE_TEST(NLDASDir, /DIRECTORY)) then begin
      ERR_MSG, 'NLDAS archive directory "' + NLDASDir + '" not found.'
      RETURN
  endif

  if NOT(FILE_TEST(NLDASDir, /READ)) then begin
      ERR_MSG, 'NLDAS archive directory "' + NLDASDir + '" not readable.'
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

  GRIBDir = NLDASDir + $
            '/' + date_YYYY + $
            '/' + date_MM

  GRIBFile = 'NLDAS_FORA0125_H.A' + date_YYYYMMDD + '.' + date_HH + $
             '00.002.grb'

  GRIBFilePath = GRIBDir + '/' + GRIBFile

  if NOT(FILE_TEST(GRIBFilePath)) then begin
      ERR_MSG, GRIBFilePath + ' not found.'
;      STOP
      RETURN
  endif

  tmpFile = '__' + GRIBFile + '__.tmp'

  APCPGrid_ = !NULL

  DECODE_GRIB1_RECORD, GRIBFilePath, $
                       scratchDir, $
                       'APCP', $
                       units, $
                       refTime_YYYYMMDDHH, $
                       timeRange, $
                       P1, $
                       P2, $
                       nCols_, $
                       nRows_, $
                       APCPGrid_, $
                      NO_DATA_VALUE = ndv

  if NOT(ISA(APCPGrid_)) then STOP
  if (units ne 'kg/m^2') then STOP
  if (P1 ne 0.0) then STOP
  if (P2 ne 1.0) then STOP
  if (timeRange ne 'acc') then STOP
  if (JULIAN_TO_YYYYMMDDHH(YYYYMMDDHH_TO_JULIAN(refTime_YYYYMMDDHH) + $
                           P2 / 24.0D) ne date_YYYYMMDDHH) then STOP
  
  if NOT(KEYWORD_SET(NLDASGridInfo)) then begin

      NLDASGridInfo = {minLon: 0.0D, $
                       maxLon: 0.0D, $
                       nCols: 0L, $
                       lonRes: 0.0D, $
                       minLat: 0.0D, $
                       maxLat: 0.0D, $
                       nRows: 0L, $
                       latRes: 0.0D}

      GET_GRIB1_LON_LAT_GRID_INFO, GRIBFile, $
                                   GRIBDir, $
                                   'APCP', $
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


;     Adjust errors due to GRIB Version 1 encoding in grid bounds.

      if (lat1 ne 25.063D) then STOP
      if (lat2 ne 52.938D) then STOP
      lat1 = 25.0625D
      lat2 = 52.9375D

      if (lon1 ne -124.938D) then STOP
      if (lon2 ne -67.063D) then STOP
      lon1 = -124.9375D
      lon2 = -67.0625D

      NLDASGridInfo.nCols = nCols
      NLDASGridInfo.lonRes = lonRes
      NLDASGridInfo.nRows = nRows
      NLDASGridInfo.latRes = latRes
      NLDASGridInfo.minLon = lon1 - 0.5D * NLDASGridInfo.lonRes
      NLDASGridInfo.maxLon = lon2 + 0.5D * NLDASGridInfo.lonRes
      NLDASGridInfo.minLat = lat1 - 0.5D * NLDASGridInfo.latRes
      NLDASGridInfo.maxLat = lat2 + 0.5D * NLDASGridInfo.latRes

  endif else begin


;     Verify grid dimensions.

      if (nCols_ ne NLDASGridInfo.nCols) then STOP
      if (nRows_ ne NLDASGridInfo.nRows) then STOP

  endelse

  APCPGrid = TEMPORARY(APCPGrid_)


  RETURN


end
