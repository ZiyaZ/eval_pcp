
PRO GET_ACC_HOURLY_NWM_ANA, $
    AccEndDate_YYYYMMDDHH, $
    DurationHours, $
    NWMDIR, $
    ScratchDir, $
    NoDataValue, $
    NWMGrid

;+
; Accumulate 1-hour NWM forcing engine data (v1.2) in NWM projection
;-

;+
; Initialize output grid to !NULL.
;-
  NWMGrid = !NULL

;+
; Check arguments for correct type and valid contents.
;-
  if NOT(ISA(AccEndDate_YYYYMMDDHH, 'STRING')) then begin
      ERR_MSG, 'Target accumulation end date/time argument must be a STRING.'
      RETURN
  endif
  if (STRLEN(AccEndDate_YYYYMMDDHH) ne 10) then begin
      ERR_MSG, 'Invalid target accumulation end date/time "' + $
               AccEndDate_YYYYMMDDHH + $
               '" (required form is YYYYMMDDHH, 10 digits).'
      RETURN
  endif
  if NOT(STREGEX(AccEndDate_YYYYMMDDHH, '[0-9]{10}', /BOOLEAN)) $
      then begin
      ERR_MSG, 'Invalid target accumulation end date/time "' + $
               AccEndDate_YYYYMMDDHH + $
               '" (required form is YYYYMMDDHH, all numeric).'
      RETURN
  endif


  accEndDate_Julian = YYYYMMDDHH_TO_JULIAN(AccEndDate_YYYYMMDDHH)
  accEndDate_YYYY = STRMID(AccEndDate_YYYYMMDDHH, 0, 4)
  accEndDate_MM = STRMID(AccEndDate_YYYYMMDDHH, 4, 2)
  accEndDate_DD = STRMID(AccEndDate_YYYYMMDDHH, 6, 2)
  accEndDate_HH = STRMID(AccEndDate_YYYYMMDDHH, 8, 2)

  if NOT(ISA(NWMDir, 'STRING')) then begin
      ERR_MSG, 'Location of NWM data archive must be a STRING.'
      RETURN
  endif
  if NOT(FILE_TEST(NWMDir, /DIRECTORY)) then begin
      ERR_MSG, 'Stage IV archive directory "' + NWMDir + '" not found.'
      RETURN
  endif
  if NOT(FILE_TEST(NWMDir, /READ)) then begin
      ERR_MSG, 'Stage IV archive directory "' + NWMDir + '" not readable.'
      RETURN
  endif

  if NOT(ISA(ScratchDir, 'STRING')) then begin
      ERR_MSG, 'Location of scratch directory must be a STRING.'
      RETURN
  endif
  if NOT(FILE_TEST(ScratchDir, /DIRECTORY)) then begin
      ERR_MSG, 'Scratch directory "' + ScratchDir + '" not found.'
      RETURN
  endif
  if NOT(FILE_TEST(ScratchDir, /READ)) then begin
      ERR_MSG, 'Scratch directory "' + ScratchDir + '" not readable.'
      RETURN
  endif
  if NOT(FILE_TEST(ScratchDir, /WRITE)) then begin
      ERR_MSG, 'Scratch directory "' + ScratchDir + '" not writeable.'
      RETURN
  endif

  if NOT(ISA(NoDataValue, 'FLOAT')) then $
      USR_MSG, 'WARNING: no-data value should be a floating point value.'

;+
; Loop over hours and accumulate data.
;-
  for hc = 0, DurationHours - 1 do begin
      date_Julian = AccEndDate_Julian - $
                    DOUBLE(DurationHours - 1 - hc) / 24.0D
      date_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(date_Julian)

      date_YYYYMMDD = STRMID(date_YYYYMMDDHH, 0, 8)
      date_HH = STRMID(date_YYYYMMDDHH, 8, 2)

      dir = NWMDir + '/nwm.' + date_YYYYMMDD
      if NOT(FILE_TEST(dir, /DIRECTORY)) then begin
          ERR_MSG, 'Directory "' + dir + '" not found.'
          RETURN
      endif

      file = 'nwm.t' + date_HH + 'z.analysis_assim.forcing.tm02.conus.nc' 

      filePath = dir + '/' + file
      if NOT(FILE_TEST(filePath)) then begin
          ERR_MSG, 'File "' + filePath + '" not found.'
          stop
          RETURN
      endif
;+
;     Open the NetCDF file.
;-
      id = NCDF_OPEN(filePath)

;+
;     Locate and read the "RAINRATE" variable and its no-data value.
;- 

      varID_data = NCDF_VARID(id, 'RAINRATE')
      if (varID_data eq -1) then begin
          ERR_MSG, 'Missing "RAINRATE" variable in ' + filePath + '.'
          RETURN
      endif

      NCDF_ATTGET, id, varID_data, '_FillValue', ndv

      NCDF_VARGET, id, 'RAINRATE', hourlyNWMGrid
      NCDF_CLOSE, id
      ind = WHERE(hourlyNWMGrid eq ndv, count)
      if (count gt 0) then hourlyNWMGrid[ind] = NoDataValue
 
      if (hc eq 0) then begin

          accNWMGrid = hourlyNWMGrid * 3600.0

      endif else begin

          ind = WHERE((accNWMGrid eq NoDataValue) or $
                      (hourlyNWMGrid eq NoDataValue), count)
          accNWMGrid = accNWMGrid + hourlyNWMGrid * 3600.0
          if (count gt 0) then accNWMGrid[ind] = NoDataValue

      endelse

  endfor

;+
; Free memory.
;-
  hourlynwmGrid = !NULL

;+
; Copy results into the output variable.
;-
  NWMGrid = TEMPORARY(accNWMGrid)



  RETURN

end
