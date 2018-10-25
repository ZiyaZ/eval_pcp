PRO GET_ACC_HRRR_APCP_WEASD, $
    AccEndDate_YYYYMMDDHH, $     ; accum. end date/time
    DurationHours, $             ; accum. duration, usually 24
    TargetFcstHour, $            ; target forecast hour, usually 3
    MinSubFcstHour, $            ; min. substitute forecast hour, usually 1
    MaxSubFcstHour, $            ; max. substitute forecast hour, usually 6
    HRRRDir, $                   ; location of HRRR archive
    ScratchDir, $                ; location for temporary/cache files
    Ndv, $                       ; no data value
    accWEASDGrid, $
    accAPCPGrid, $
    HRRR_GRID_PROJ_INFO = HRRRGridProjInfo, $
    VERBOSE = verbose, $
    NO_SUBDIRS = noSubdirs, $   ; prevent using YYYY/MM/DD below HRRRDir
    USE_YYDOY = useYYDOY        ; use "YYDOY.hrrr" naming convention
                                ; instead of "hrrr.YYYYMMDDHH" convention

; Get accumulated hourly HRRR total precipitation (APCP) and water
; equivalent of accumulated snow depth (WEASD).

; This procedure was copied from the GET_ACCUM_HRRR_APCP_WEASD
; procedure used by National Snowfall Analysis v2, but does not
; perform the "deprojection" to geographic coordinates of the HRRR
; data.

  COMMON info, message

  accAPCPGrid = !NULL
  accWEASDGrid = !NULL


; Error handler for anRowsthing the main procedure code misses. Example:
; RESTORE encounters a file that was truncated because a disk filled.

  CATCH, errorStatus
  if (errorStatus ne 0) then begin
      ERR_MSG, !Error_State.Msg
      RETURN
  endif


; Check arguments for correct type and valid contents.

  if NOT(ISA(AccEndDate_YYYYMMDDHH, 'STRING')) then begin
      ERR_MSG, 'Target accumulation end date/time argument ' + $
               'must be a STRING.'
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

  AccEndDate_Julian = YYYYMMDDHH_TO_JULIAN(AccEndDate_YYYYMMDDHH)

  if (DurationHours lt 0) then begin
      ERR_MSG, 'Duration must be a positive integer number of hours.'
      RETURN
  endif

  ;; if (MinSubFcstHour gt TargetFcstHour) then begin
  ;;     ERR_MSG, 'Minimum substitute forecast hour may not be ' + $
  ;;              'larger than target forecast hour.'
  ;;     RETURN
  ;; endif

  ;; if (MaxSubFcstHour lt TargetFcstHour) then begin
  ;;     ERR_MSG, 'Maximum substitute forecast hour may not be ' + $
  ;;              'smaller than target forecast hour.'
  ;;     RETURN
  ;; endif

  if NOT(ISA(HRRRDir, 'STRING')) then begin
      ERR_MSG, 'Location of HRRR archive must be a STRING.'
      RETURN
  endif

  if NOT(FILE_TEST(HRRRDir, /DIRECTORY)) then begin
      ERR_MSG, 'HRRR archive directory "' + HRRRDir + '" not found.'
      RETURN
  endif

  if NOT(FILE_TEST(HRRRDir, /READ)) then begin
      ERR_MSG, 'HRRR archive directory "' + HRRRDir + '" not readable.'
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

  if NOT(ISA(Ndv, 'FLOAT')) then $
      ERR_MSG, 'WARNING: no-data value should be a floating point value.'

  savFile = 'HRRR_PRECIP' + $
            '_f' + STRING(TargetFcstHour, FORMAT = '(I2.2)') + $
            '_' + STRCRA(DurationHours) + 'h' + $
            '_ending_' + AccEndDate_YYYYMMDDHH + '.sav'

  if (FILE_TEST(ScratchDir + '/' + savFile)) then begin


;     Get data from cache file rather than reading HRRR data
;     directly.

      if KEYWORD_SET(verbose) then $
          USR_MSG, 'Reading ' + ScratchDir + '/' + savFile

      RESTORE, ScratchDir + '/' + savFile


;     Verify the contents of the save file.

      if NOT(ISA(accWEASDGrid_)) then begin
          ERR_MSG, 'No WEASD grid variable in ' + $
                   ScratchDir + '/' + savFile
          RETURN
      endif

      foo = SIZE(accWEASDGrid_)
      if (foo[0] ne 2) then begin
          ERR_MSG, 'WEASD grid in ' + ScratchDir + '/' + saveFile + $
                   ' is not a 2-D array.'
          RETURN
      endif

      nCols = foo[1]
      nRows = foo[2]

      if NOT(ISA(accAPCPGrid_)) then begin
          ERR_MSG, 'No APCP grid variable in ' + $
                   ScratchDir + '/' + savFile
          RETURN
      endif

      foo = SIZE(accAPCPGrid_)
      if (foo[0] ne 2) then begin
          ERR_MSG, 'APCP grid in ' + ScratchDir + '/' + saveFile + $
                   ' is not a 2-D array.'
          RETURN
      endif

      if (foo[1] ne nCols) then begin
          ERR_MSG, 'APCP grid column dimension (' + STRCRA(foo[1]) + ') ' + $
                   'does not match that of WEASD grid (' + STRCRA(nCols) + $
                   ').'
          RETURN
      endif

      if (foo[2] ne nRows) then begin
          ERR_MSG, 'APCP grid row dimension (' + STRCRA(foo[2]) + ') ' + $
                   'does not match that of WEASD grid (' + STRCRA(nRows) + $
                   ').'
          RETURN
      endif

      if NOT(ISA(ndv_)) then begin
          ERR_MSG, 'Missing "ndv_" variable in ' + $
                   ScratchDir + '/' + savFile
          RETURN
      endif

      if NOT(ISA(HRRRGridProjInfo_)) then begin
          ERR_MSG, 'No HRRR grid/projection info structure in ' + $
                   ScratchDir + '/' + savFile
          RETURN
      endif


;     Verify the structure of HRRRGridProjInfo_ and grid dimensions.

      foo = SIZE(HRRRGridProjInfo_)
      if (foo[0] ne 1) then begin
          ERR_MSG, 'HRRR_GRID_PROJ_INFO structure mismatch (non-scalar) ' + $
                   'in ' + ScratchDir + '/' + savFile
          RETURN
      endif

      if ((foo[1] ne 1) or (foo[2] ne 8)) then begin
          ERR_MSG, 'HRRR_GRID_PROJ_INFO structure mismatch ' + $
                   '(not a structure) ' + $
                   'in ' + ScratchDir + '/' + savFile
          RETURN
      endif

      savFileOK = 1
      tagNames = TAG_NAMES(HRRRGridProjInfo_)
      ind = WHERE(tagNames eq 'LONV', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "lonV" tag in HRRR_GRID_PROJ_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'LATD', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "latD" tag in HRRR_GRID_PROJ_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'LATSEC1', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "latSec1" tag in HRRR_GRID_PROJ_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'LATSEC2', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "latSec2" tag in HRRR_GRID_PROJ_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'ERADM', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "eRadM" tag in HRRR_GRID_PROJ_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'LAT00', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "lat00" tag in HRRR_GRID_PROJ_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'LON00', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "lon00" tag in HRRR_GRID_PROJ_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'NCOLS', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "nCols" tag in HRRR_GRID_PROJ_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'NROWS', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "nRows" tag in HRRR_GRID_PROJ_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'DX', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "dx" tag in HRRR_GRID_PROJ_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'DY', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "dy" tag in HRRR_GRID_PROJ_INFO.'
          savFileOK = 0
      endif

      if savFileOK then begin

          if NOT(ISA(HRRRGridProjInfo_.lonV, 'DOUBLE')) then begin
              ERR_MSG, 'HRRR_GRID_PROJ_INFO missing DOUBLE element ' + $
                       '"lonV" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(HRRRGridProjInfo_.latD, 'DOUBLE')) then begin
              ERR_MSG, 'HRRR_GRID_PROJ_INFO missing DOUBLE element ' + $
                       '"latD" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(HRRRGridProjInfo_.latSec1, 'DOUBLE')) then begin
              ERR_MSG, 'HRRR_GRID_PROJ_INFO missing DOUBLE element ' + $
                       '"latSec1" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(HRRRGridProjInfo_.latSec2, 'DOUBLE')) then begin
              ERR_MSG, 'HRRR_GRID_PROJ_INFO missing DOUBLE element ' + $
                       '"latSec2" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(HRRRGridProjInfo_.eRadM, 'DOUBLE')) then begin
              ERR_MSG, 'HRRR_GRID_PROJ_INFO missing DOUBLE element ' + $
                       '"eRadM" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(HRRRGridProjInfo_.lat00, 'DOUBLE')) then begin
              ERR_MSG, 'HRRR_GRID_PROJ_INFO missing DOUBLE element ' + $
                       '"lat00" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(HRRRGridProjInfo_.lon00, 'DOUBLE')) then begin
              ERR_MSG, 'HRRR_GRID_PROJ_INFO missing DOUBLE element ' + $
                       '"lon00" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(HRRRGridProjInfo_.nCols, 'LONG')) then begin
              ERR_MSG, 'HRRR_GRID_PROJ_INFO missing LONG element ' + $
                       '"nCols" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(HRRRGridProjInfo_.nRows, 'LONG')) then begin
              ERR_MSG, 'HRRR_GRID_PROJ_INFO missing LONG element ' + $
                       '"nRows" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(HRRRGridProjInfo_.dx, 'DOUBLE')) then begin
              ERR_MSG, 'HRRR_GRID_PROJ_INFO missing DOUBLE element ' + $
                       '"dx" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(HRRRGridProjInfo_.dy, 'DOUBLE')) then begin
              ERR_MSG, 'HRRR_GRID_PROJ_INFO missing DOUBLE element ' + $
                       '"dy" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if (nCols ne HRRRGridProjInfo_.nCols) then begin
              ERR_MSG, 'Grid columns in ' + $
                       ScratchDir + '/' + savFile + $
                       ' (' + STRCRA(nCols) + ') ' + $
                       'do not match HRRR_GRID_PROJ_INFO value ' + $
                       '(' + STRCRA(HRRRGridProjInfo_.nCols) + ').'
              savFileOK = 0
          endif

          if (nRows ne HRRRGridProjInfo_.nRows) then begin
              ERR_MSG, 'Grid rows in ' + $
                       ScratchDir + '/' + savFile + $
                       ' (' + STRCRA(nRows) + ') ' + $
                       'do not match HRRR_GRID_PROJ_INFO value ' + $
                       '(' + STRCRA(HRRRGridProjInfo_.nRows) + ').'
              savFileOK = 0
          endif

      endif

      if (savFileOK and KEYWORD_SET(HRRRGridProjInfo)) then begin


;         Verify HRRRGridProjInfo structure from caller against
;         HRRRGridProjInfo_ structure from save file.

          if NOT(COMPARE(HRRRGridProjInfo.lonV, HRRRGridProjInfo_.lonV)) $
              then begin
              ERR_MSG, 'HRRR_GRID_PROJ_INFO structure "lonV" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(HRRRGridProjInfo.latD, HRRRGridProjInfo_.latD)) $
              then begin
              ERR_MSG, 'HRRR_GRID_PROJ_INFO structure "latD" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(HRRRGridProjInfo.latSec1, $
                         HRRRGridProjInfo_.latSec1)) $
              then begin
              ERR_MSG, 'HRRR_GRID_PROJ_INFO structure "latSec1" ' + $
                       'mismatch between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(HRRRGridProjInfo.latSec2, $
                         HRRRGridProjInfo_.latSec2)) $
              then begin
              ERR_MSG, 'HRRR_GRID_PROJ_INFO structure "latSec2" ' + $
                       'mismatch between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(HRRRGridProjInfo.eRadM, $
                         HRRRGridProjInfo_.eRadM)) $
              then begin
              ERR_MSG, 'HRRR_GRID_PROJ_INFO structure "eRadM" ' + $
                       'mismatch between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(HRRRGridProjInfo.lat00, HRRRGridProjInfo_.lat00)) $
              then begin
              ERR_MSG, 'HRRR_GRID_PROJ_INFO structure "lat00" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              HRRRGridProjInfo.lat00 = HRRRGridProjInfo_.lat00
;              savFileOK = 0
          endif

          if NOT(COMPARE(HRRRGridProjInfo.lon00, HRRRGridProjInfo_.lon00)) $
              then begin
              ERR_MSG, 'HRRR_GRID_PROJ_INFO structure "lon00" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              HRRRGridProjInfo.lon00 = HRRRGridProjInfo_.lon00
;              savFileOK = 0
          endif

          if (HRRRGridProjInfo.nCols ne HRRRGridProjInfo_.nCols) then begin
              ERR_MSG, 'HRRR_GRID_PROJ_INFO structure "nCols" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if (HRRRGridProjInfo.nRows ne HRRRGridProjInfo_.nRows) then begin
              ERR_MSG, 'HRRR_GRID_PROJ_INFO structure "nRows" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif
          if NOT(COMPARE(HRRRGridProjInfo.dx, HRRRGridProjInfo_.dx)) $
              then begin
              ERR_MSG, 'HRRR_GRID_PROJ_INFO structure "dx" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(HRRRGridProjInfo.dy, HRRRGridProjInfo_.dy)) $
              then begin
              ERR_MSG, 'HRRR_GRID_PROJ_INFO structure "dy" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

      endif else begin


;         Copy HRRRGridProjInfo structure from save file.

          if savFileOK then HRRRGridProjInfo = HRRRGridProjInfo_

      endelse

      if NOT(savFileOK) then begin

          ERR_MSG, 'Unexpected structure/content in IDL save file ' + $
                   savFile + '; returning NULL grids.'
          RETURN

      endif

      if NOT(savFileOK) then begin

          ERR_MSG, 'Unexpected structure/content in IDL save file ' + $
                   savFile + '; returning NULL grids.'
          RETURN

      endif


;     Verify the no-data value is consistent; modify if it is not.

      if (ndv_ ne Ndv) then begin
          ERR_MSG, 'WARNING: value of "ndv_" variable in ' + $
                  ScratchDir + '/' + savFile + $
                  ' (' + STRCRA(ndv_) + ') differs from ' + $
                  ' argument "Ndv" ' + $
                  '(' + STRCRA(Ndv) + '). Will adapt.'
          ind = WHERE(accWEASDGrid_ eq ndv_, count)
          if (count gt 0) then accWEASDGrid_[ind] = Ndv
          ind = WHERE(accAPCPGrid_ eq ndv_, count)
          if (count gt 0) then accAPCPGrid_[ind] = Ndv
          ind = !NULL
      endif

      accAPCPGrid = TEMPORARY(accAPCPGrid_)
      accWEASDGrid = TEMPORARY(accWEASDGrid_)

      RETURN

  endif                         ;else begin

  perfect = 1B ; flag to indicate that we got our first choice for
               ; all input QPF grids

  for hc = 0, DurationHours - 1 do begin

      cycleDate_Julian = AccEndDate_Julian - $
                         DOUBLE(DurationHours - 1) / 24.0D + $
                         DOUBLE(hc) / 24.0D - $
                         DOUBLE(TargetFcstHour) / 24.0D
      cycleDate_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(cycleDate_Julian)
      cycleDate_YYYY = STRMID(cycleDate_YYYYMMDDHH, 0, 4)
      cycleDate_MM = STRMID(cycleDate_YYYYMMDDHH, 4, 2)
      cycleDate_DD = STRMID(cycleDate_YYYYMMDDHH, 6, 2)

      if KEYWORD_SET(noSubdirs) then $
          GRIBDir = HRRRDir $
      else $
          GRIBDir = HRRRDir + $
                    '/' + cycleDate_YYYY + $
                    '/' + cycleDate_MM + $
                    '/' + cycleDate_DD

;      if NOT(FILE_TEST(GRIBDir, /DIRECTORY)) then begin
;          ERR_MSG, 'Directory ' + GRIBDir + ' not found.'
;          RETURN
;      endif

      if KEYWORD_SET(useYYDOY) then begin

          cycleDate_YY = STRMID(cycleDate_YYYY, 2, 2)
          Jan1_Julian = YYYYMMDDHH_TO_JULIAN(cycleDate_YYYY + '010100')
          dayOfYear = FIX(cycleDate_Julian - Jan1_Julian) + 1
          cycleDate_DOY = STRING(dayOfYear, FORMAT = '(I3.3)')

          GRIBFile = cycleDate_YY + cycleDate_DOY + $
                     '.hrrr.t' + STRMID(cycleDate_YYYYMMDDHH, 8, 2) + $
                     'z.wrfsfcf' + $
                     STRING(TargetFcstHour, FORMAT = '(I2.2)') + $
                     '.grib2'

      endif else begin

          GRIBFile = 'hrrr.' + $
                     STRMID(cycleDate_YYYYMMDDHH, 0, 8) + $
                     '.t' + STRMID(cycleDate_YYYYMMDDHH, 8, 2) + $
                     'z.wrfsfcf' + $
                     STRING(TargetFcstHour, FORMAT = '(I2.2)') + $
                     '.grib2'

      endelse

      if NOT(FILE_TEST(GRIBDir + '/' + GRIBFile)) then begin

          perfect = 0B


;         Try a farther-out forecast.

          altFcstHour = TargetFcstHour + 1

          while (altFcstHour le MaxSubFcstHour) do begin

              cycleDate_Julian = AccEndDate_Julian - $
                                 DOUBLE(DurationHours - 1) / 24.0D + $
                                 DOUBLE(hc) / 24.0D - $
                                 DOUBLE(altFcstHour) / 24.0D
              cycleDate_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(cycleDate_Julian)
              cycleDate_YYYY = STRMID(cycleDate_YYYYMMDDHH, 0, 4)
              cycleDate_MM = STRMID(cycleDate_YYYYMMDDHH, 4, 2)
              cycleDate_DD = STRMID(cycleDate_YYYYMMDDHH, 6, 2)

              if KEYWORD_SET(noSubdirs) then $
                  GRIBDir = HRRRDir $
              else $
                  GRIBDir = HRRRDir + $
                            '/' + cycleDate_YYYY + $
                            '/' + cycleDate_MM + $
                            '/' + cycleDate_DD

              if KEYWORD_SET(useYYDOY) then begin
 
                  cycleDate_YY = STRMID(cycleDate_YYYY, 2, 2)
                  Jan1_Julian = YYYYMMDDHH_TO_JULIAN(cycleDate_YYYY + '010100')
                  dayOfYear = FIX(cycleDate_Julian - Jan1_Julian) + 1
                  cycleDate_DOY = STRING(dayOfYear, FORMAT = '(I3.3)')

                  GRIBFile = cycleDate_YY + cycleDate_DOY + $
                             '.hrrr.t' + STRMID(cycleDate_YYYYMMDDHH, 8, 2) + $
                             'z.wrfsfcf' + $
                             STRING(altFcstHour, FORMAT = '(I2.2)') + $
                             '.grib2'

              endif else begin

                  GRIBFile = 'hrrr.' + $
                             STRMID(cycleDate_YYYYMMDDHH, 0, 8) + $
                             '.t' + STRMID(cycleDate_YYYYMMDDHH, 8, 2) + $
                             'z.wrfsfcf' + $
                             STRING(altFcstHour, FORMAT = '(I2.2)') + $
                             '.grib2'
              endelse

              if FILE_TEST(GRIBDir + '/' + GRIBFile) then BREAK
              altFcstHour = altFcstHour + 1

          endwhile

          if ((altFcstHour le MaxSubFcstHour) and $
              KEYWORD_SET(verbose)) then $
              USR_MSG, 'WARNING: using ' + $
                       STRCRA(altFcstHour) + $
                       '-hour forecast for hour ' + $
                       STRCRA(hc + 1)

      endif

      if NOT(FILE_TEST(GRIBDir + '/' + GRIBFile)) then begin


;         Try a less-far-out forecast.

          altFcstHour = TargetFcstHour - 1

          while (altFcstHour ge MinSubFcstHour) do begin

              cycleDate_Julian = AccEndDate_Julian - $
                                 DOUBLE(DurationHours - 1) / 24.0D + $
                                 DOUBLE(hc) / 24.0D - $
                                 DOUBLE(altFcstHour) / 24.0D
              cycleDate_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(cycleDate_Julian)
              cycleDate_YYYY = STRMID(cycleDate_YYYYMMDDHH, 0, 4)
              cycleDate_MM = STRMID(cycleDate_YYYYMMDDHH, 4, 2)
              cycleDate_DD = STRMID(cycleDate_YYYYMMDDHH, 6, 2)

              if KEYWORD_SET(noSubdirs) then $
                  GRIBDir = HRRRDir $
              else $
                  GRIBDir = HRRRDir + $
                            '/' + cycleDate_YYYY + $
                            '/' + cycleDate_MM + $
                            '/' + cycleDate_DD

              if KEYWORD_SET(useYYDOY) then begin

                  cycleDate_YY = STRMID(cycleDate_YYYY, 2, 2)
                  Jan1_Julian = YYYYMMDDHH_TO_JULIAN(cycleDate_YYYY + '010100')
                  dayOfYear = FIX(cycleDate_Julian - Jan1_Julian) + 1
                  cycleDate_DOY = STRING(dayOfYear, FORMAT = '(I3.3)')

                  GRIBFile = cycleDate_YY + cycleDate_DOY + $
                             '.hrrr.t' + STRMID(cycleDate_YYYYMMDDHH, 8, 2) + $
                             'z.wrfsfcf' + $
                             STRING(altFcstHour, FORMAT = '(I2.2)') + $
                             '.grib2'

              endif else begin

                  GRIBFile = 'hrrr.' + $
                             STRMID(cycleDate_YYYYMMDDHH, 0, 8) + $
                             '.t' + STRMID(cycleDate_YYYYMMDDHH, 8, 2) + $
                             'z.wrfsfcf' + $
                             STRING(altFcstHour, FORMAT = '(I2.2)') + $
                             '.grib2'
              endelse

              if FILE_TEST(GRIBDir + '/' + GRIBFile) then BREAK
              altFcstHour = altFcstHour - 1

          endwhile

          if ((altFcstHour ge MinSubFcstHour) and $
              KEYWORD_SET(verbose)) then $
                  USR_MSG, 'WARNING: using ' + $
                           STRCRA(altFcstHour) + $
                           '-hour forecast for hour ' + $
                           STRCRA(hc + 1)
      endif

      if NOT(FILE_TEST(GRIBDir + '/' + GRIBFile)) then begin
          if KEYWORD_SET(verbose) then $
              ERR_MSG, 'No data found for hour ' + STRCRA(hc) + $
                       ' of HRRR accumulation ending ' + $
                       AccEndDate_YYYYMMDDHH + '.'
          RETURN
      endif


;     Define string for forecast portion of GRIB field pattern.

      fcstHour = TargetFcstHour
      if ISA(altFcstHour) then fcstHour = altFcstHour
      fcstString = STRCRA(fcstHour - 1) + '-' + $
                   STRCRA(fcstHour) + ' hour acc fcst'


;     Verify the expected WEASD record is present.

      matchStr = ':WEASD:surface:' + fcstString + ':'

      cmd = 'wgrib2 -g2clib 0 -match "' + matchStr + '" ' + $
            GRIBDir + '/' + GRIBFile

      SPAWN, cmd, GRIBOut, EXIT_STATUS = status
      if (status eq 0) then begin ; so far so good
          if (N_ELEMENTS(GRIBOut eq 1)) then begin ; still okay
              if (GRIBOut[0] eq '') then begin
                  ERR_MSG, 'Failed to decode "' + matchStr + '" ' + $
                           'record/s in ' + GRIBDir + '/' + GRIBFile
                  RETURN
              endif
          endif else begin
              ERR_MSG, 'WARNING: multiple matches for "' + matchStr + $
                       '" in ' + GRIBDir + '/' + GRIBFile
          endelse
      endif


;     Decode WEASD record.

      DECODE_GRIB2_FIELD, GRIBFile, GRIBDir, ScratchDir, $
                          matchStr, $
                          record, vt, lev, ftime, $
                          varAbbrev, varField, varUnits, $
                          nCols_, nRows_, $
                          hourlyWEASDProj, $
                          status, $
                          NO_DATA_VALUE = Ndv

      if NOT(status) then begin
          ERR_MSG, 'Failed to decode ":WEASD:" record from ' + $
                   GRIBDir + '/' + GRIBFile
          RETURN
      endif


;     Check parameters of WEASD GRIB record.

      if (varAbbrev ne 'WEASD') then begin
          ERR_MSG, 'GRIB file ' + GRIBDir + '/' + GRIBFile + $
                   ' "WEASD" record has invalid abbreviation ' + $
                   '"' + varAbbrev + '".'
          RETURN
      endif
      if (varField ne 'Water Equivalent of Accumulated Snow Depth') $
        then begin
          ERR_MSG, 'GRIB file ' + GRIBDir + '/' + GRIBFile + $
                   ' "WEASD" record has invalid field name ' + $
                   '"' + varField + '".'
          RETURN
      endif

      if (varUnits ne 'kg/m^2') then begin
          ERR_MSG, 'GRIB file ' + GRIBDir + '/' + GRIBFile + $
                   ' "WEASD" record has invalid units ' + $
                   '"' + varUnits + '".'
      endif

      if (hc eq 0) then begin


;         Establish grid size and projection parameters.

          nCols = nCols_
          nRows = nRows_

          GET_GRIB2_LAMBERT_CONFORMAL_GRID_INFO, $
            GRIBFile, GRIBDir, ScratchDir, $
            ':WEASD:', $
            nCols_, nRows_, $
            lat00, lon00, lonV, latD, latSec1, latSec2, latsp, lonsp, $
            dx, dy, $
            STATUS

          if NOT(status) then begin
              ERR_MSG, 'Failed to read Lambert conformal projection ' + $
                       'parameters from GRIB file ' + $
                       GRIBDir + '/' + GRIBFile + ' "WEASD" record.'
              RETURN
          endif


;         Confirm the NCEP sphere.

;         1. Grid template.
          cmd = 'wgrib2 -match ":WEASD:" -get_byte 3 13 2 ' + $
                GRIBDir + '/' + GRIBFile
          SPAWN, cmd, gridDefTemplate, EXIT_STATUS = status
          if ( status ne 0 ) then begin
              ERR_MSG, 'Failed to read grid definition section in ' + $
                       'GRIB file ' + $
                       GRIBDir + '/' + GRIBFile + '"WEASD" record.'
              RETURN
          endif
          if ( STRMID ( gridDefTemplate[0], $
                        STRLEN ( gridDefTemplate[0] ) - 4, $
                        4 ) ne '0,30' ) then begin
              ERR_MSG, 'Grid definition template number ' + $
                       '(see GRIB2 table 3.1) ' + $
                       '"' + gridDefTemplate[0] + '" not consistent with ' + $
                       ' Lambert Conformal projection.'
              RETURN
          endif
;         2. Shape of the earth.
          cmd = 'wgrib2 -match ":WEASD:" -get_byte 3 15 1 ' + $
                GRIBDir + '/' + GRIBFile
          SPAWN, cmd, shapeOfEarth, EXIT_STATUS = status
          if (status ne 0) then begin
              ERR_MSG, 'Failed to read Lambert conformal grid template ' + $
                       'in GRIB file ' + $
                       GRIBDir + '/' + GRIBFile + '"WEASD" record.'
              RETURN
          endif
          if (STRMID(shapeOfEarth[0], $
                     STRLEN(shapeOfEarth[0]) - 1, 1 ) ne '6' ) $
          then begin
;             '6' = spherical, radius 6371229.0 m
              ERR_MSG, 'NCEP sphere not detected in GRIB file ' + $
                       GRIBDir + '/' + GRIBFile + '"WEASD" record.'
              RETURN
          endif


;         Verify grid size is unchanged.

          if (nCols_ ne nCols) then begin
              ERR_MSG, 'Grid columns in GRIB file ' + $
                       GRIBDir + '/' + GRIBFile + ' "WEASD" record ' + $
                       'have changed.'
              RETURN
          endif
          if (nRows_ ne nRows) then begin
              ERR_MSG, 'Grid rows in GRIB file ' + $
                       GRIBDir + '/' + GRIBFile + ' "WEASD" record ' + $
                       'have changed.'
              RETURN
          endif

          if NOT(KEYWORD_SET(HRRRGridProjInfo)) then begin


;             Create the HRRRGridProjInfo_ structure to store grid and
;             projection parameters.

              HRRRGridProjInfo = $
                  {lonV:    lonV, $       ; orientation longitude
                   latD:    latD, $       ; lat where dx and dy are specified
                   latSec1: latSec1, $    ; first standard parallel
                   latSec2: latSec2, $    ; second standard parallel
                   eRadM:   6371229.0D, $ ; NCEP sphere
                   lat00:   lat00, $      ; deprojected lat of LL cell center
                   lon00:   lon00, $      ; deprojected lon of LL cell center
                   nCols:   nCols, $      ; # columns
                   nRows:   nRows, $      ; # rows
                   dx:      dx, $         ; x resolution at latD, meters
                   dy:      dy}           ; y resolution at latD, meters

          endif else begin


;             Verify HRRRGridProjInfo structure from caller against
;             contents of GRIB file.

              foo = SIZE(HRRRGridProjInfo)
              if (foo[0] ne 1) then begin
                  ERR_MSG, 'HRRR_GRID_PROJ_INFO structure mismatch ' + $
                           '(non-scalar).'
                  RETURN
              endif

              if ((foo[1] ne 1) or (foo[2] ne 8)) then begin
                  ERR_MSG, 'HRRR_GRID_PROJ_INFO structure mismatch ' + $
                           '(not a structure).'
                  RETURN
              endif

              tagNames = TAG_NAMES(HRRRGridProjInfo)
              ind = WHERE(tagNames eq 'LONV', count)
              if (count eq 0) then begin
                  ERR_MSG, 'No "lonV" tag in HRRR_GRID_PROJ_INFO.'
                  RETURN
              endif

              ind = WHERE(tagNames eq 'LATD', count)
              if (count eq 0) then begin
                  ERR_MSG, 'No "latD" tag in HRRR_GRID_PROJ_INFO.'
                  RETURN
              endif

              ind = WHERE(tagNames eq 'LATSEC1', count)
              if (count eq 0) then begin
                  ERR_MSG, 'No "latSec1" tag in HRRR_GRID_PROJ_INFO.'
                  RETURN
              endif

              ind = WHERE(tagNames eq 'LATSEC2', count)
              if (count eq 0) then begin
                  ERR_MSG, 'No "latSec2" tag in HRRR_GRID_PROJ_INFO.'
                  RETURN
              endif

              ind = WHERE(tagNames eq 'ERADM', count)
              if (count eq 0) then begin
                  ERR_MSG, 'No "eRadM" tag in HRRR_GRID_PROJ_INFO.'
                  RETURN
              endif

              ind = WHERE(tagNames eq 'LAT00', count)
              if (count eq 0) then begin
                  ERR_MSG, 'No "lat00" tag in HRRR_GRID_PROJ_INFO.'
                  RETURN
              endif

              ind = WHERE(tagNames eq 'LON00', count)
              if (count eq 0) then begin
                  ERR_MSG, 'No "lon00" tag in HRRR_GRID_PROJ_INFO.'
                  RETURN
              endif

              ind = WHERE(tagNames eq 'NCOLS', count)
              if (count eq 0) then begin
                  ERR_MSG, 'No "nCols" tag in HRRR_GRID_PROJ_INFO.'
                  RETURN
              endif

              ind = WHERE(tagNames eq 'NROWS', count)
              if (count eq 0) then begin
                  ERR_MSG, 'No "nRows" tag in HRRR_GRID_PROJ_INFO.'
                  RETURN
              endif

              ind = WHERE(tagNames eq 'DX', count)
              if (count eq 0) then begin
                  ERR_MSG, 'No "dx" tag in HRRR_GRID_PROJ_INFO.'
                  RETURN
              endif

              ind = WHERE(tagNames eq 'DY', count)
              if (count eq 0) then begin
                  ERR_MSG, 'No "dy" tag in HRRR_GRID_PROJ_INFO.'
                  RETURN
              endif

              if NOT(COMPARE(HRRRGridProjInfo.lonV, lonV)) $
              then begin
                  ERR_MSG, 'HRRR_GRID_PROJ_INFO structure "lonV" mismatch.'
                  RETURN
              endif

              if NOT(COMPARE(HRRRGridProjInfo.latD, latD)) $
              then begin
                  ERR_MSG, 'HRRR_GRID_PROJ_INFO structure "latD" mismatch.'
                  RETURN
              endif

              if NOT(COMPARE(HRRRGridProjInfo.latSec1, latSec1)) $
              then begin
                  ERR_MSG, 'HRRR_GRID_PROJ_INFO structure "latSec1" ' + $
                           'mismatch.'
                  RETURN
              endif

              if NOT(COMPARE(HRRRGridProjInfo.latSec2, latSec2)) $
              then begin
                  ERR_MSG, 'HRRR_GRID_PROJ_INFO structure "latSec2" ' + $
                           'mismatch.'
                  RETURN
              endif

              if NOT(COMPARE(HRRRGridProjInfo.eRadM, 6371229.0D)) $
              then begin
                  ERR_MSG, 'HRRR_GRID_PROJ_INFO structure "eRadM" ' + $
                           'mismatch.'
                  RETURN
              endif

              if NOT(COMPARE(HRRRGridProjInfo.lat00, lat00)) $
              then begin
                  ERR_MSG, 'WARNING: HRRR_GRID_PROJ_INFO structure ' + $
                           '"lat00" mismatch - replacing ' + $
                           STRCRA(STRING(HRRRGridProjInfo.lat00, $
                                         FORMAT = '(F25.17)')) + $
                           ' with ' + $
                           STRCRA(STRING(lat00, FORMAT = '(F25.17)'))
                  HRRRGridProjInfo.lat00 = lat00
;                  ERR_MSG, 'HRRR_GRID_PROJ_INFO structure "lat00" mismatch.'
;                  RETURN
              endif

              if NOT(COMPARE(HRRRGridProjInfo.lon00, lon00)) $
              then begin
                  ERR_MSG, 'WARNING: HRRR_GRID_PROJ_INFO structure ' + $
                           '"lon00" mismatch - replacing ' + $
                           STRCRA(STRING(HRRRGridProjInfo.lon00, $
                                         FORMAT = '(F25.17)')) + $
                           ' with ' + $
                           STRCRA(STRING(lon00, FORMAT = '(F25.17)'))
                  HRRRGridProjInfo.lon00 = lon00
;                  ERR_MSG, 'HRRR_GRID_PROJ_INFO structure "lon00" mismatch.'
;                  RETURN
              endif

              if (HRRRGridProjInfo.nCols ne nCols) then begin
                  ERR_MSG, 'HRRR_GRID_PROJ_INFO structure "nCols" mismatch.'
                  RETURN
              endif

              if (HRRRGridProjInfo.nRows ne nRows) then begin
                  ERR_MSG, 'HRRR_GRID_PROJ_INFO structure "nRows" mismatch.'
                  RETURN
              endif
              if NOT(COMPARE(HRRRGridProjInfo.dx, dx)) $
              then begin
                  ERR_MSG, 'HRRR_GRID_PROJ_INFO structure "dx" mismatch.'
                  RETURN
              endif

              if NOT(COMPARE(HRRRGridProjInfo.dy, dy)) $
              then begin
                  ERR_MSG, 'HRRR_GRID_PROJ_INFO structure "dy" mismatch.'
                  RETURN
              endif

          endelse

          accWEASDGrid_ = hourlyWEASDProj

      endif else begin

          if (nCols_ ne nCols) then begin
              ERR_MSG, 'Grid columns in GRIB file ' + $
                       GRIBDir + '/' + GRIBFile + ' "WEASD" record ' + $
                       'differ from previous.'
              RETURN
          endif
          if (nRows_ ne nRows) then begin
              ERR_MSG, 'Grid rows in GRIB file ' + $
                       GRIBDir + '/' + GRIBFile + ' "WEASD" record ' + $
                       'differ from previous.'
              RETURN
          endif

          ind = WHERE((accWEASDGrid_ eq Ndv) or $
                      (hourlyWEASDProj eq Ndv), count)
          accWEASDGrid_ = accWEASDGrid_ + hourlyWEASDProj
          if (count gt 0) then accWEASDGrid_[ind] = Ndv

      endelse


;     Verify the expected WEASD record is present.

      matchStr = ':APCP:surface:' + fcstString + ':'

      cmd = 'wgrib2 -g2clib 0 -match "' + matchStr + '" ' + $
            GRIBDir + '/' + GRIBFile

      SPAWN, cmd, GRIBOut, EXIT_STATUS = status
      if (status eq 0) then begin ; so far so good
          if (N_ELEMENTS(GRIBOut eq 1)) then begin ; still okay
              if (GRIBOut[0] eq '') then begin
                  ERR_MSG, 'Failed to decode "' + matchStr + '" ' + $
                           'record/s in ' + GRIBDir + '/' + GRIBFile
                  RETURN
              endif
          endif else begin
              ERR_MSG, 'WARNING: multiple matches for "' + matchStr + $
                       '" in ' + GRIBDir + '/' + GRIBFile
          endelse
      endif


;     Decode APCP record.

      if KEYWORD_SET(verbose) then $
          USR_MSG, 'Reading "APCP" from ' + GRIBDir + '/' + GRIBFile

      DECODE_GRIB2_FIELD, GRIBFile, GRIBDir, ScratchDir, $
                          ':APCP:surface:' + fcstString + ':', $
                          record, vt, lev, ftime, $
                          varAbbrev, varField, varUnits, $
                          nCols_, nRows_, $
                          hourlyAPCPProj, $
                          status, $
                          NO_DATA_VALUE = Ndv

      if NOT(status) then begin
          ERR_MSG, 'Failed to decode ":APCP:" record from ' + $
                   GRIBDir + '/' + GRIBFile
          RETURN
      endif


;     Check parameters of APCP GRIB record.

      if (varAbbrev ne 'APCP') then begin
          ERR_MSG, 'GRIB file ' + GRIBDir + '/' + GRIBFile + $
                   ' "APCP" record has invalid abbreviation ' + $
                   '"' + varAbbrev + '".'
          RETURN
      endif
      if (varField ne 'Total Precipitation') $
        then begin
          ERR_MSG, 'GRIB file ' + GRIBDir + '/' + GRIBFile + $
                   ' "APCP" record has invalid field name ' + $
                   '"' + varField + '".'
          RETURN
      endif

      if (varUnits ne 'kg/m^2') then begin
          ERR_MSG, 'GRIB file ' + GRIBDir + '/' + GRIBFile + $
                   ' "APCP" record has invalid units ' + $
                   '"' + varUnits + '".'
      endif

      if (nCols_ ne nCols) then begin
          ERR_MSG, 'Grid columns in GRIB file ' + $
                   GRIBDir + '/' + GRIBFile + ' "APCP" record ' + $
                   'differ from those previously established.'
          RETURN
      endif

      if (nRows_ ne nRows) then begin
          ERR_MSG, 'Grid rows in GRIB file ' + $
                   GRIBDir + '/' + GRIBFile + ' "APCP" record ' + $
                   'differ from those previously established.'
          RETURN
      endif

      if (hc eq 0) then begin

          accAPCPGrid_ = hourlyAPCPProj

      endif else begin

          ind = WHERE((accAPCPGrid_ eq Ndv) or $
                      (hourlyAPCPProj eq Ndv), count)
          accAPCPGrid_ = accAPCPGrid_ + hourlyAPCPProj
          if (count gt 0) then accAPCPGrid_[ind] = Ndv

      endelse

      altFcstHour = !NULL

  endfor


; Free memory.

  hourlyWEASDProj = !NULL
  hourlyAPCPProj = !NULL

  if perfect then begin


;     Create a cached copy of results for future calls to this
;     procedure with the same set of arguments.

      ndv_ = Ndv
      HRRRGridProjInfo_ = HRRRGridProjInfo

      SAVE, accWEASDGrid_, accAPCPGrid_, $
            ndv_, HRRRGridProjInfo_, $            
            FILE = ScratchDir + '/' + savFile

      if KEYWORD_SET(verbose) then $
          USR_MSG, 'APCP and WEASD accumulation grids saved to ' + $
                   ScratchDir + '/' + savFile

  endif

  accWEASDGrid = TEMPORARY(accWEASDGrid_)
  accAPCPGrid = TEMPORARY(accAPCPGrid_)

  RETURN

end


;; accEndDate_YYYYMMDDHH = '2017070712'
;; durationHours = 24
;; targetFcstHour = 2
;; minSubFcstHour = 1
;; maxSubFcstHour = 6
;; HRRRDir = '/nwcdev/archive/HRRR_archive'
;; scratchDir = '/disks/scratch'
;; ndv = -99999.0

;; GET_ACC_HRRR_APCP_WEASD, $
;;     accEndDate_YYYYMMDDHH, $     ; accum. end date/time
;;     durationHours, $             ; accum. duration, usually 24
;;     targetFcstHour, $            ; target forecast hour, usually 3
;;     minSubFcstHour, $            ; min. substitute forecast hour, usually 1
;;     maxSubFcstHour, $            ; max. substitute forecast hour, usually 6
;;     HRRRDir, $                   ; location of HRRR archive
;;     scratchDir, $                ; location for temporary/cache files
;;     ndv, $                       ; no data value
;;     accWEASDGrid, $
;;     accAPCPGrid, $
;;     HRRR_GRID_PROJ_INFO = HRRRGridProjInfo, $
;;     /VERBOSE

;; if NOT(ISA(accAPCPGrid)) then STOP

;; ; Locate shapefiles to draw on maps.

;;   vectorDir = '/nwcdev/nwc_forcings/GIS_data/Vector' ; for maps

;;   shapePathList = [vectorDir + '/' + $
;;                    'National_Boundary_Canada', $
;;                    vectorDir + '/' + $
;;                    'Provincial_Boundaries', $
;;                    vectorDir + '/' + $
;;                    'National_Boundary_Mexico_2004', $
;;                    vectorDir + '/' + $
;;                    'National_Boundary_Coterminous_US', $
;;                    vectorDir + '/' +  $
;;                    'State_Boundaries_Coterminous_US']


;; ; Define colors for precipitation grids in mm.

;;   qRed = [255, 000, 000, 127, 238, 255, 255, 255, 238, 205, 139, $
;;           145, 137, 016, 030]
;;   qGrn = [228, 139, 205, 255, 238, 215, 165, 127, 064, 000, 000, $
;;           044, 104, 078, 144]
;;   qBlu = [220, 000, 000, 000, 000, 000, 079, 000, 000, 000, 000, $
;;           238, 205, 139, 255]
;;   qEdges = [0.0, 0.1, 2.0, 5.0, 10.0, 15.0, 20.0, 25.0, 35.0, 50.0, $
;;             75.0, 100.0, 125.0, 150.0, 175.0, 500.0]




;; X_MAP_HRRR_GRID, accAPCPGrid, $
;;                  HRRRGridProjInfo, $
;;                  HRRRGridProjInfo.eRadM, $
;;                  qEdges, qRed, qGrn, qBlu, $
;;                  0, $
;;                  status, $
;;                  NDV = ndv, $
;;                  /SHOW_HIGH, $
;;                  XSIZE_TARGET = 1200, $
;;                  SHAPE_PATH_LIST = shapePathList, $
;;                  TITLE = 'Winning = Aggregated HRRR Precip', $
;;                  /COLORBAR, $
;;                  UNITS = 'mm'

;; end
