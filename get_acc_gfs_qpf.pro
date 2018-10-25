;+
; Read and aggregate archived GFS precipitation data from a single GFS
; cycle.
;
; Greg Fall, OWP-Chanhassen
; 2018-05-29
;-

PRO GET_ACC_GFS_QPF, CycleDate_YYYYMMDDHH, $
                     AccumHours, $
                     GFSDir, $
                     ScratchDir, $
                     NoDataValue, $
                     acc_GFS_QPF_grid, $
                     GFS_GRID_INFO = GFSGridInfo, $
                     CYCLE_OFFSET_HOURS = cycleOffsetHours

  acc_GFS_QPF_grid = !NULL

;+
; Check arguments for correct type and valid contents.
;-
 if NOT(ISA(CycleDate_YYYYMMDDHH, 'STRING')) then begin
      ERR_MSG, 'Target accumulation end date/time argument must be a STRING.'
      RETURN
  endif
  if (STRLEN(CycleDate_YYYYMMDDHH) ne 10) then begin
      ERR_MSG, 'Invalid target accumulation end date/time "' + $
               CycleDate_YYYYMMDDHH + $
               '" (required form is YYYYMMDDHH, 10 digits).'
      RETURN
  endif
  if NOT(STREGEX(CycleDate_YYYYMMDDHH, '[0-9]{10}', /BOOLEAN)) $
      then begin
      ERR_MSG, 'Invalid target accumulation end date/time "' + $
               CycleDate_YYYYMMDDHH + $
               '" (required form is YYYYMMDDHH, all numeric).'
      RETURN
  endif

  cycleDate_Julian = YYYYMMDDHH_TO_JULIAN(CycleDate_YYYYMMDDHH)
;  cycleDate_YYYY = STRMID(CycleDate_YYYYMMDDHH, 0, 4)
;  cycleDate_MM = STRMID(CycleDate_YYYYMMDDHH, 4, 2)
;  cycleDate_DD = STRMID(CycleDate_YYYYMMDDHH, 6, 2)
;  cycleDate_HH = STRMID(CycleDate_YYYYMMDDHH, 8, 2)

  if (AccumHours lt 0) then begin
      ERR_MSG, 'Accumulation length must be a positive integer # of hours.'
      RETURN
  endif
  if (AccumHours gt 120) then begin
      ERR_MSG, 'Accumulation length cannot exceed 240 hours.'
      RETURN
  endif

  if NOT(ISA(GFSDir, 'STRING')) then begin
      ERR_MSG, 'Location of GFS archive must be a STRING.'
      RETURN
  endif
  if NOT(FILE_TEST(GFSDir, /DIRECTORY)) then begin
      ERR_MSG, 'GFS archive directory "' + GFSDir + '" not found.'
      RETURN
  endif
  if NOT(FILE_TEST(GFSDir, /READ)) then begin
      ERR_MSG, 'GFS archive directory "' + GFSDir + '" not readable.'
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
; Set/check offset hours.
;-
  if NOT(KEYWORD_SET(cycleOffsetHours)) then cycleOffsetHours = 0
  if ((cycleOffsetHours + AccumHours) gt 120) then begin
      ERR_MSG, 'Given cycle offset of ' + STRCRA(cycleOffsetHours) + $
               ' hours, accumulation length cannot exceed ' + $
               STRCRA(120 - cycleOffsetHours) + ' hours.'
      RETURN
  endif

;+
; Look for a save file.
;-
  savFile = 'GFS_QPF_' + $
            'cycle_' + CycleDate_YYYYMMDDHH + '_' + $
            'offset_' + STRCRA(cycleOffsetHours) + 'h_' + $
            STRCRA(AccumHours) + 'h_accum.sav'

  if FILE_TEST(ScratchDir + '/' + savFile) then begin

;+
;     Get data from an IDL save file rather than reading HRRR data
;     directly.
;-
      RESTORE, ScratchDir + '/' + savFile

;+
;     Verify the contents of the save file.
;-
      if NOT(ISA(acc_GFS_QPF_grid_)) then begin
          ERR_MSG, 'No QPF grid variable in ' + ScratchDir + '/' + savFile
          RETURN
      endif

      foo = SIZE(acc_GFS_QPF_grid_)
      if (foo[0] ne 2) then begin
          ERR_MSG, 'QPF grid in ' + ScratchDir + '/' + saveFile + $
                   ' is not a 2-D array.'
          RETURN
      endif

      nCols = foo[1]
      nRows = foo[2]

      if NOT(ISA(ndv_)) then begin
          ERR_MSG, 'Missing "ndv_" variable in ' + $
                   ScratchDir + '/' + savFile
          RETURN
      endif

      if NOT(ISA(GFSGridInfo_)) then begin
          ERR_MSG, 'No GFS grid info structure in ' + $
                   ScratchDir + '/' + savFile
          RETURN
      endif

;+
;     Verify the structure of GFSGridInfo_ and grid dimensions.
;-
      foo = SIZE(GFSGridInfo_)
      if (foo[0] ne 1) then begin
          ERR_MSG, 'GFS_GRID_INFO structure mismatch (non-scalar) ' + $
                   'in ' + ScratchDir + '/' + savFile
          RETURN
      endif

      if ((foo[1] ne 1) or (foo[2] ne 8)) then begin
          ERR_MSG, 'GFS_GRID_INFO structure mismatch ' + $
                   '(not a structure) ' + $
                   'in ' + ScratchDir + '/' + savFile
          RETURN
      endif

      savFileOK = 1
      tagNames = TAG_NAMES(GFSGridInfo_)

      ;; GFSGridInfo = {minLon: 0.0D, $
      ;;                maxLon: 0.0D, $
      ;;                nCols: 0L, $
      ;;                lonRes: 0.0D, $
      ;;                minLat: 0.0D, $
      ;;                maxLat: 0.0D, $
      ;;                nRows: 0L, $
      ;;                rowCenterLat: DBLARR(nRows), $
      ;;                meanLatRes: 0.0D}

      ind = WHERE(tagNames eq 'MINLON', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "minLon" tag in GFS_GRID_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'MAXLON', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "maxLon" tag in GFS_GRID_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'NCOLS', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "nCols" tag in GFS_GRID_INFO.'
          savFileOK = 0
      endif
      ind = WHERE(tagNames eq 'LONRES', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "lonRes" tag in GFS_GRID_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'MINLAT', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "minLat" tag in GFS_GRID_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'MAXLAT', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "maxLat" tag in GFS_GRID_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'NROWS', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "nRows" tag in GFS_GRID_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'ROWCENTERLAT', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "rowCenterLat" tag in GFS_GRID_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'MEANLATRES', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "meanLatRes" tag in GFS_GRID_INFO.'
          savFileOK = 0
      endif

      if savFileOK then begin

          if NOT(ISA(GFSGridInfo_.minLon, 'DOUBLE')) then begin
              ERR_MSG, 'GFS_GRID_INFO missing DOUBLE element ' + $
                       '"minLon" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(GFSGridInfo_.maxLon, 'DOUBLE')) then begin
              ERR_MSG, 'GFS_GRID_INFO missing DOUBLE element ' + $
                       '"maxLon" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(GFSGridInfo_.nCols, 'LONG')) then begin
              ERR_MSG, 'GFS_GRID_INFO missing LONG element ' + $
                       '"nCols" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(GFSGridInfo_.lonRes, 'DOUBLE')) then begin
              ERR_MSG, 'GFS_GRID_INFO missing DOUBLE element ' + $
                       '"lonRes" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(GFSGridInfo_.minLat, 'DOUBLE')) then begin
              ERR_MSG, 'GFS_GRID_INFO missing DOUBLE element ' + $
                       '"minLat" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(GFSGridInfo_.maxLat, 'DOUBLE')) then begin
              ERR_MSG, 'GFS_GRID_INFO missing DOUBLE element ' + $
                       '"maxLat" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(GFSGridInfo_.nRows, 'LONG')) then begin
              ERR_MSG, 'GFS_GRID_INFO missing LONG element ' + $
                       '"nRows" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(GFSGridInfo_.rowCenterLat, 'DOUBLE', /ARRAY)) then begin
              ERR_MSG, 'GFS_GRID_INFO missing DBLARR element ' + $
                       '"rowCenterLat" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif else begin
              if (N_ELEMENTS(GFSGridInfo_.rowCenterLat) ne $
                  GFSGridInfo_.nRows) then begin
                  ERR_MSG, 'GFS_GRID_INFO "rowCenterLat" has ' + $
                           STRCRA(N_ELEMENTS(GFSGridInfo_.rowCenterLat)) + $
                           ' elements; "nRows" element indicates ' + $
                           STRCRA(GFSGridInfo_.nRows) + '.'
                  savFileOK = 0
              endif
          endelse

          if NOT(ISA(GFSGridInfo_.meanLatRes, 'DOUBLE')) then begin
              ERR_MSG, 'GFS_GRID_INFO missing DOUBLE element ' + $
                       '"meanLatRes" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if (nCols ne GFSGridInfo_.nCols) then begin
              ERR_MSG, 'Grid columns in ' + $
                       ScratchDir + '/' + savFile + $
                       ' (' + STRCRA(nCols) + ') ' + $
                       'do not match GFS_GRID_INFO value ' + $
                       '(' + STRCRA(GFSGridInfo_.nCols) + ').'
              savFileOK = 0
          endif

          if (nRows ne GFSGridInfo_.nRows) then begin
              ERR_MSG, 'Grid rows in ' + $
                       ScratchDir + '/' + savFile + $
                       ' (' + STRCRA(nRows) + ') ' + $
                       'do not match GFS_GRID_INFO value ' + $
                       '(' + STRCRA(GFSGridInfo_.nRows) + ').'
              savFileOK = 0
          endif

      endif

      if (savFileOK and KEYWORD_SET(GFSGridInfo)) then begin

;+
;         Verify GFSGridInfo structure from caller against
;         GFSGridInfo_ structure from save file.
;-
          if NOT(COMPARE(GFSGridInfo.minLon, GFSGridInfo_.minLon)) $
          then begin
              ERR_MSG, 'GFS_GRID_INFO structure "minLon" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(GFSGridInfo.maxLon, GFSGridInfo_.maxLon)) $
          then begin
              ERR_MSG, 'GFS_GRID_INFO structure "maxLon" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if (GFSGridInfo.nCols ne GFSGridInfo_.nCols) then begin
              ERR_MSG, 'GFS_GRID_INFO structure "nCols" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(GFSGridInfo.lonRes, GFSGridInfo_.lonRes)) $
              then begin
              ERR_MSG, 'GFS_GRID_INFO structure "lonRes" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(GFSGridInfo.minLat, GFSGridInfo_.minLat)) $
              then begin
              ERR_MSG, 'GFS_GRID_INFO structure "minLat" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(GFSGridInfo.maxLat, GFSGridInfo_.maxLat)) $
              then begin
              ERR_MSG, 'GFS_GRID_INFO structure "maxLat" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if (GFSGridInfo.nRows ne GFSGridInfo_.nRows) then begin
              ERR_MSG, 'GFS_GRID_INFO structure "nRows" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          for rc = 0, GFSGridInfo_.nRows - 1 do begin
              if NOT(COMPARE(GFSGridInfo.rowCenterLat[rc], $
                             GFSGridInfo_.rowCenterLat[rc])) then begin
                  ERR_MSG, 'GFS_GRID_INFO structure "rowCenterLat" ' + $
                           'mismatch between keyword and ' + $
                           ScratchDir + '/' + savFile + ' data.'
                  savFileOK = 0
                  BREAK
              endif
          endfor

          if NOT(COMPARE(GFSGridInfo.meanLatRes, $
                         GFSGridInfo_.meanLatRes)) $
          then begin
              ERR_MSG, 'GFS_GRID_INFO structure "meanLatRes" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif
 
      endif else begin

;+
;         Copy GFSGridInfo structure from save file.
;-
          if savFileOK then GFSGridInfo = GFSGridInfo_

      endelse

      if NOT(savFileOK) then begin

           ERR_MSG, 'Unexpected structure/content in IDL save file ' + $
                   savFile + '; returning NULL grid..'
          RETURN

      endif

;+
;     Verify the no-data value is consistent; modify if it is not.
;-
      if (ndv_ ne NoDataValue) then begin
          ERR_MSG, 'WARNING: value of "ndv_" variable in ' + $
                   ScratchDir + '/' + savFile + $
                   ' (' + STRCRA(ndv_) + ') differs from ' + $
                   ' argument "NoDataValue" ' + $
                   '(' + STRCRA(NoDataValue) + '). Will adapt.'
          ind = WHERE(acc_GFS_QPF_grid_ eq ndv_, count)
          if (count gt 0) then acc_GFS_QPF_grid_[ind] = NoDatavalue
          ind = !NULL
      endif

      acc_GFS_QPF_grid = TEMPORARY(acc_GFS_QPF_grid_)

      USR_MSG, 'GFS accumulation grid restored from ' + $
               ScratchDir + '/' + savFile

      RETURN

  endif

  if KEYWORD_SET(GFSGridInfo) then begin

;+
;     Verify the structure of GFSGridInfo.
;-
      foo = SIZE(GFSGridInfo)
      if (foo[0] ne 1) then begin
          ERR_MSG, 'GFS_GRID_INFO structure mismatch (non-scalar).'
          RETURN
      endif

      if ((foo[1] ne 1) or (foo[2] ne 8)) then begin
          ERR_MSG, 'GFS_GRID_INFO structure mismatch ' + $
                   '(not a structure).'
          RETURN
      endif

      structOk = 1

      tagNames = TAG_NAMES(GFSGridInfo)

      ind = WHERE(tagNames eq 'MINLON', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "minLon" tag in GFS_GRID_INFO.'
          structOk = 0
      endif
      ind = WHERE(tagNames eq 'MAXLON', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "maxLon" tag in GFS_GRID_INFO.'
          structOk = 0
      endif
      ind = WHERE(tagNames eq 'NCOLS', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "nCols" tag in GFS_GRID_INFO.'
          structOk = 0
      endif
      ind = WHERE(tagNames eq 'LONRES', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "lonRes" tag in GFS_GRID_INFO.'
          structOk = 0
      endif
      ind = WHERE(tagNames eq 'MINLAT', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "minLat" tag in GFS_GRID_INFO.'
          structOk = 0
      endif
      ind = WHERE(tagNames eq 'MAXLAT', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "maxLat" tag in GFS_GRID_INFO.'
          structOk = 0
      endif
      ind = WHERE(tagNames eq 'NROWS', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "nRows" tag in GFS_GRID_INFO.'
          structOk = 0
      endif
      ind = WHERE(tagNames eq 'ROWCENTERLAT', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "rowCenterLat" tag in GFS_GRID_INFO.'
          structOK = 0
      endif
      ind = WHERE(tagNames eq 'MEANLATRES', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "meanLatRes" tag in GFS_GRID_INFO.'
          structOK = 0
      endif

      if structOk then begin

          if NOT(ISA(GFSGridInfo.minLon, 'DOUBLE')) then begin
              ERR_MSG, 'GFS_GRID_INFO element "minLon" is not ' + $
                       'type DOUBLE.'
              structOk = 0
          endif
          if NOT(ISA(GFSGridInfo.maxLon, 'DOUBLE')) then begin
              ERR_MSG, 'GFS_GRID_INFO element "maxLon" is not ' + $
                       'type DOUBLE.'
              structOk = 0
          endif
          if NOT(ISA(GFSGridInfo.nCols, 'LONG')) then begin
              ERR_MSG, 'GFS_GRID_INFO element "nCols" is not ' + $
                       'type LONG.'
              structOk = 0
          endif
          if NOT(ISA(GFSGridInfo.lonRes, 'DOUBLE')) then begin
              ERR_MSG, 'GFS_GRID_INFO element "lonRes" is not ' + $
                       'type DOUBLE.'
              structOk = 0
          endif
          if NOT(ISA(GFSGridInfo.minLat, 'DOUBLE')) then begin
              ERR_MSG, 'GFS_GRID_INFO element "minLat" is not ' + $
                       'type DOUBLE.'
              structOk = 0
          endif
          if NOT(ISA(GFSGridInfo.maxLat, 'DOUBLE')) then begin
              ERR_MSG, 'GFS_GRID_INFO element "maxLat" is not ' + $
                       'type DOUBLE.'
              structOk = 0
          endif
          if NOT(ISA(GFSGridInfo.nRows, 'LONG')) then begin
              ERR_MSG, 'GFS_GRID_INFO element "nRows" is not ' + $
                       'type LONG.'
              structOk = 0
          endif
          if NOT(ISA(GFSGridInfo.rowCenterLat, 'DOUBLE', /ARRAY)) then begin
              ERR_MSG, 'GFS_GRID_INFO missing DBLARR element ' + $
                       '"rowCenterLat"'
              structOk = 0
          endif else begin
              if (N_ELEMENTS(GFSGridInfo.rowCenterLat) ne $
                             GFSGridInfo.nRows) then begin
                  ERR_MSG, 'GFS_GRID_INFO "rowCenterLat" has ' + $
                           STRCRA(N_ELEMENTS(GFSGridInfo.rowCenterLat)) + $
                           ' elements; "nRows" indicates ' + $
                           STRCRA(GFSGridInfo.nRows) + '.'
                  structOk = 0
              endif
          endelse
          if NOT(ISA(GFSGridInfo.meanLatRes, 'DOUBLE')) then begin
              ERR_MSG, 'GFS_GRID_INFO missing DOUBLE element ' + $
                       '"meanLatRes".'
              structOk = 0
          endif

      endif

      if NOT(structOk) then begin
          ERR_MSG, 'Unexpected structure/content in GFS_GRID_INFO ' + $
                   'structure.'
          RETURN
      endif

  endif


;+
; Loop over GFS cycle.
;-
  for hc = cycleOffsetHours + 1L, cycleOffsetHours + AccumHours do begin

;+
;     For hours 2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18,
;     etc., difference precipitation accumulations to get hourly
;     amounts. Begin that process by storing the GFS grid from the
;     previous forecast hour, which is an accumulation starting from
;     the last multiple of 6 in the forecast; e.g., if hc eq 16, then
;     the previous accumulation is the 12-15 hour accumulation, and
;     the current hour will give us the 12-16 hour
;     accumulation. Difference those, and you get the 15-16 hour
;     accumulation.
;-
      if (((hc - 1) mod 6) ne 0) then begin
          if NOT(ISA(ave_GFS_QPF_grid)) then STOP
          prev_ave_GFS_QPF_grid = ave_GFS_QPF_grid
          PRINT, 'hour ' + STRCRA(hc) + ', previous field was "' + $
                 GRIBField +'".'
      endif else prev_ave_GFS_QPF_grid = !NULL

;+
;     Locate input GFS data.
;-
      dir = GFSDir + '/' + STRMID(CycleDate_YYYYMMDDHH, 0, 8)
      if NOT(FILE_TEST(dir, /DIR)) then begin
          ERR_MSG, 'No data for ' + CycleDate_YYYYMMDDHH + ' GFS cycle.'
          RETURN
      endif

      if (hc lt 100) then $
          fcstStr = STRING(hc, FORMAT = '(I2.2)') $
      else $
          fcstStr = STRING(hc, FORMAT = '(I3.3)')

      file = 'gfs.' + STRMID(cycleDate_YYYYMMDDHH, 0, 8) + '.' + $
             't' + STRMID(cycleDate_YYYYMMDDHH, 8, 2) + 'z.' + $
             'sfluxgrbf' + fcstStr + '.grib2'

      filePath = dir + '/' + file

      if NOT(FILE_TEST(filePath)) then begin
          ERR_MSG, 'File ' + filePath + ' not found.'
          RETURN
      endif

;+
;     Decode precipitation.
;-
      GRIBField = ':PRATE:surface:'
      accStartHour = (hc - 1) / 6 * 6
      GRIBField = GRIBField + STRCRA(accStartHour) + '-' + STRCRA(hc) + $
                  ' hour ave fcst:'

      PRINT, 'Reading ' + file

      ave_GFS_QPF_grid = !NULL

      DECODE_GRIB2_RECORD, filePath, $
                           ScratchDir, $
                           GRIBField, $
                           record, $
                           vt, $
                           lev, $
                           ftime, $
                           varAbbrev, $
                           varField, $
                           varUnits, $
                           nCols_, $
                           nRows_, $
                           ave_GFS_QPF_grid, $
                           NO_DATA_VALUE = NoDataValue

      if NOT(ISA(ave_GFS_QPF_grid)) then STOP

;+
;     Convert from mm/hr to mm.
;-
      ind = WHERE(ave_GFS_QPF_grid eq NoDataValue, count)
      ave_GFS_QPF_grid = ave_GFS_QPF_grid * 3600.0
      if (count gt 0) then ave_GFS_QPF_grid[ind] = NoDataValue

      if NOT(ISA(GFSGridInfo)) then begin

          GFSGridInfo = {minLon: -1.0D, $
                         maxLon: -1.0D, $
                         nCols: nCols_, $
                         lonRes: -1.0D, $
                         minLat: -1.0D, $
                         maxLat: -1.0D, $
                         nRows: nRows_, $
                         rowCenterLat: DBLARR(nRows_), $
                         meanLatRes: 0.0D}

          nCols_T1534 = nCols_ & if nCols_T1534 ne 3072 then STOP
          GFSGridInfo.lonRes = 360.0D / nCols_T1534
          longrid_GFS13 = 0.0D + DINDGEN(nCols_T1534) * GFSGridInfo.lonRes
          GFSGridInfo.minLon = lonGrid_GFS13[0] - 0.5D * GFSGridInfo.lonRes
          while (GFSGridInfo.minLon lt 0.0D) do $
              GFSGridInfo.minLon = GFSGridInfo.minLon + 360.0D
          while (GFSGridInfo.minLon ge 360.0D) do $
              GFSGridInfo.minLon = GFSGridInfo.minLon - 360.0D
          GFSGridInfo.maxLon = lonGrid_GFS13[GFSGridInfo.nCols - 1] + $
                               0.5 * GFSGridInfo.lonRes
          while (GFSGridInfo.maxLon lt 0.0D) do $
              GFSGridInfo.maxLon = GFSGridInfo.maxLon + 360.0D
          while (GFSGridInfo.maxLon ge 360.0D) do $
              GFSGridInfo.maxLon = GFSGridInfo.maxLon - 360.0D

;+
;         Get actual GFS 13 km grid latitudes.
;-
          if FILE_TEST('latGrid_GFS13.sav') then $
              RESTORE, 'latGrid_GFS13.sav' $
          else begin
              GET_T1534_GRID_LATITUDES, filePath, latGrid_GFS13
          endelse
          nRows_T1534 = nRows_ & if nRows_T1534 ne 1536 then STOP

          GFSGridInfo.rowCenterLat = latGrid_GFS13
          GFSGridInfo.minLat = $
              latGrid_GFS13[0] - $
              0.5D * (latGrid_GFS13[1] - $
                      latGrid_GFS13[0])
          GFSGridInfo.maxLat = $
              latGrid_GFS13[GFSGridInfo.nRows - 1] + $
              0.5D * (latGrid_GFS13[GFSGridInfo.nRows - 1] - $
                      latGrid_GFS13[GFSGridInfo.nRows - 2])
          GFSGridInfo.meanLatRes = $
              (GFSGridInfo.maxLat - GFSGridInfo.minLat) / $
              GFSGridInfo.nRows

          HELP, GFSGridInfo, /STRUCT

      endif

      ;; if ((hc mod 6) eq 0) then begin ; plot 6 hour total

      ;;     WSET_OR_WINDOW, 3, XSIZE = 625, YSIZE = 324
      ;;     LOADCT, 12
      ;;     REAL_TVSCL, ave_GFS_QPF_grid[1932:2556, 938:1261], NDV = NoDataValue
      ;;     move = GET_KBRD(1)

      ;; endif

;+
;     Difference ave_GFS_QPF_grid and prev_ave_GFS_QPF_grid for hours
;     2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18, etc.
;-
      PRINT, 'hour ' + STRCRA(hc) + ', current field is "' + $
             GRIBField + '".'
      if (((hc - 1) mod 6) ne 0) then begin
          ind = WHERE((prev_ave_GFS_QPF_grid eq NoDataValue) or $
                      (ave_GFS_QPF_grid eq NoDataValue), $
                      count)
          wtPrev = (hc - 1) mod 6
          wtCurr = wtPrev + 1
          PRINT, 'hour ' + STRCRA(hc) + ', use ' + $
                 STRCRA(wtCurr) + ' * current - ' + $
                 STRCRA(wtPrev) + ' * previous'
          hourly_GFS_QPF_grid = ave_GFS_QPF_grid * wtCurr - $
                                prev_ave_GFS_QPF_grid * wtPrev
          if (count gt 0) then hourly_GFS_QPF_grid[ind] = NoDataValue
      endif else hourly_GFS_QPF_grid = ave_GFS_QPF_grid

;+
;     Aggregate hourly data into full-cycle accumulation grids.
;-
      if (hc eq (cycleOffsetHours + 1)) then begin
          acc_GFS_QPF_grid_ = hourly_GFS_QPF_grid
      endif else begin
          ind = WHERE((acc_GFS_QPF_grid_ eq NoDataValue) or $
                      (hourly_GFS_QPF_grid eq NoDataValue), count)
          acc_GFS_QPF_grid_ = acc_GFS_QPF_grid_ + hourly_GFS_QPF_grid
          if (count gt 0) then acc_GFS_QPF_grid_[ind] = NoDataValue
      endelse

  endfor

;+
; Free memory.
;-
  hourly_GFS_QPF_grid = !NULL
  prev_ave_GFS_QPF_grid = !NULL
  ave_GFS_QPF_grid = !NULL

;+
; Create an IDL save file of results for future calls to this
; procedure with the same arguments.
;-
;  acc_GFS_QPF_grid_ = acc_GFS_QPF_grid
  ndv_ = NoDataValue
  GFSGridInfo_ = GFSGridInfo

  SAVE, acc_GFS_QPF_grid_, ndv_, GFSGridInfo_, $
        FILE = ScratchDir + '/' + savFile

  USR_MSG, 'GFS accumulation grid saved to ' + $
           ScratchDir + '/' + savFile

;+
; Rename the output variable to return successfully.
;-
  acc_GFS_QPF_grid = TEMPORARY(acc_GFS_QPF_grid_)

  RETURN

end

;; scratchdir = '/disks/scratch/fall'                                                                   
;; gfsdir = '/nwcdev/archive/GFS_archive'                                                               
;; ndv = -99999.0                                                                                       
;; get_acc_gfs_qpf, '2018051512', 24, gfsdir, scratchdir, ndv, accgrid, $
;;                  gfs_grid_info = gfsgridinfo     

;; end
