PRO GET_ACC_HOURLY_AORC_APCP, $
    AccEndDate_YYYYMMDDHH, $
    DurationHours, $
    AORCDir, $
    ScratchDir, $
    NoDataValue, $
    accAPCPGrid, $
    AORC_GRID_INFO = AORCGridInfo

;+
; Accumulate 1-hour (AORC) precipitation forcings.
;-

;+
; Initialize output grid to !NULL
;-
  accAPCPGrid = !NULL

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

  if (DurationHours lt 0) then begin
      ERR_MSG, 'Duration must be a positive integer number of hours.'
      RETURN
  endif
  if (DurationHours gt 24) then begin
      ERR_MSG, 'Duration argument cannot exceed 24 hours.'
      RETURN
  endif

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
; Look for a save file.
;-
  savFile = 'AORC_APCP_' + STRCRA(DurationHours) + $
            'h_ending_' + AccEndDate_YYYYMMDDHH + '.sav'

  if (FILE_TEST(ScratchDir + '/' + savFile)) then begin

;+
;     Get data from an IDL save file rather than reading HRRR data
;     directly.
;-
      RESTORE, ScratchDir + '/' + savFile

;+
;     Verify the contents of the save file.
;-
      if NOT(ISA(accAPCPGrid_)) then begin
          ERR_MSG, 'No APCP grid variable in ' + ScratchDir + '/' + savFile
          RETURN
      endif

      foo = SIZE(accAPCPGrid_)
      if (foo[0] ne 2) then begin
          ERR_MSG, 'APCP grid in ' + ScratchDir + '/' + saveFile + $
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

      if NOT(ISA(AORCGridInfo_)) then begin
          ERR_MSG, 'No AORC grid info structure in ' + $
                   ScratchDir + '/' + savFile
          RETURN
      endif

;+
;     Verify the structure of AORCGridInfo_ and grid dimensions.
;-
      foo = SIZE(AORCGridInfo_)
      if (foo[0] ne 1) then begin
          ERR_MSG, 'AORC_GRID_INFO structure mismatch (non-scalar) ' + $
                   'in ' + ScratchDir + '/' + savFile
          RETURN
      endif

      if ((foo[1] ne 1) or (foo[2] ne 8)) then begin
          ERR_MSG, 'AORC_GRID_INFO structure mismatch ' + $
                   '(not a structure) ' + $
                   'in ' + ScratchDir + '/' + savFile
          RETURN
      endif

      savFileOK = 1
      tagNames = TAG_NAMES(AORCGridInfo_)

      ;; AORCGridInfo = {minLon: 0.0D, $
      ;;                 maxLon: 0.0D, $
      ;;                 nCols: 0L, $
      ;;                 lonRes: 0.0D, $
      ;;                 minLat: 0.0D, $
      ;;                 maxLat: 0.0D, $
      ;;                 nRows: 0L, $
      ;;                 latRes: 0.0D}

      ind = WHERE(tagNames eq 'MINLON', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "minLon" tag in AORC_GRID_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'MAXLON', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "maxLon" tag in AORC_GRID_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'NCOLS', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "nCols" tag in AORC_GRID_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'LONRES', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "lonRes" tag in AORC_GRID_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'MINLAT', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "minLat" tag in AORC_GRID_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'MAXLAT', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "maxLat" tag in AORC_GRID_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'NROWS', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "nRows" tag in AORC_GRID_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'LATRES', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "latRes" tag in AORC_GRID_INFO.'
          savFileOK = 0
      endif

      if savFileOK then begin

          if NOT(ISA(AORCGridInfo_.minLon, 'DOUBLE')) then begin
              ERR_MSG, 'AORC_GRID_INFO missing DOUBLE element ' + $
                       '"minLon" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(AORCGridInfo_.maxLon, 'DOUBLE')) then begin
              ERR_MSG, 'AORC_GRID_INFO missing DOUBLE element ' + $
                       '"maxLon" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(AORCGridInfo_.nCols, 'LONG')) then begin
              ERR_MSG, 'AORC_GRID_INFO missing LONG element ' + $
                       '"nCols" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(AORCGridInfo_.lonRes, 'DOUBLE')) then begin
              ERR_MSG, 'AORC_GRID_INFO missing DOUBLE element ' + $
                       '"lonRes" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(AORCGridInfo_.minLat, 'DOUBLE')) then begin
              ERR_MSG, 'AORC_GRID_INFO missing DOUBLE element ' + $
                       '"minLat" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(AORCGridInfo_.maxLat, 'DOUBLE')) then begin
              ERR_MSG, 'AORC_GRID_INFO missing DOUBLE element ' + $
                       '"maxLat" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(AORCGridInfo_.nRows, 'LONG')) then begin
              ERR_MSG, 'AORC_GRID_INFO missing LONG element ' + $
                       '"nRows" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(AORCGridInfo_.latRes, 'DOUBLE')) then begin
              ERR_MSG, 'AORC_GRID_INFO missing DOUBLE element ' + $
                       '"latRes" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if (nCols ne AORCGridInfo_.nCols) then begin
              ERR_MSG, 'Grid columns in ' + $
                       ScratchDir + '/' + savFile + $
                       ' (' + STRCRA(nCols) + ') ' + $
                       'do not match AORC_GRID_INFO value ' + $
                       '(' + STRCRA(AORCGridInfo_.nCols) + ').'
              savFileOK = 0
          endif

          if (nRows ne AORCGridInfo_.nRows) then begin
              ERR_MSG, 'Grid rows in ' + $
                       ScratchDir + '/' + savFile + $
                       ' (' + STRCRA(nRows) + ') ' + $
                       'do not match AORC_GRID_INFO value ' + $
                       '(' + STRCRA(AORCGridInfo_.nRows) + ').'
              savFileOK = 0
          endif

      endif

      if (savFileOK and KEYWORD_SET(AORCGridInfo)) then begin

;+
;         Verify AORCGridInfo structure from caller against
;         AORCGridInfo_ structure from save file.
;-
          if NOT(COMPARE(AORCGridInfo.minLon, AORCGridInfo_.minLon)) $
          then begin
              ERR_MSG, 'AORC_GRID_INFO structure "minLon" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(AORCGridInfo.maxLon, AORCGridInfo_.maxLon)) $
          then begin
              ERR_MSG, 'AORC_GRID_INFO structure "maxLon" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if (AORCGridInfo.nCols ne AORCGridInfo_.nCols) then begin
              ERR_MSG, 'AORC_GRID_INFO structure "nCols" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(AORCGridInfo.lonRes, AORCGridInfo_.lonRes)) $
              then begin
              ERR_MSG, 'AORC_GRID_INFO structure "lonRes" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(AORCGridInfo.minLat, AORCGridInfo_.minLat)) $
              then begin
              ERR_MSG, 'AORC_GRID_INFO structure "minLat" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(AORCGridInfo.maxLat, AORCGridInfo_.maxLat)) $
              then begin
              ERR_MSG, 'AORC_GRID_INFO structure "maxLat" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if (AORCGridInfo.nRows ne AORCGridInfo_.nRows) then begin
              ERR_MSG, 'AORC_GRID_INFO structure "nRows" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(AORCGridInfo.latRes, AORCGridInfo_.latRes)) $
          then begin
              ERR_MSG, 'AORC_GRID_INFO structure "latRes" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif
 
      endif else begin

;+
;         Copy AORCGridInfo structure from save file.

          if savFileOK then AORCGridInfo = AORCGridInfo_

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
          ind = WHERE(accAPCPGrid_ eq ndv_, count)
          if (count gt 0) then accAPCPGrid_[ind] = NoDatavalue
          ind = !NULL
      endif

      accAPCPGrid = TEMPORARY(accAPCPGrid_)

      USR_MSG, 'AORC accumulation grid restored from ' + $
               ScratchDir + '/' + savFile

      RETURN

  endif

;+
; The remainder of the procedure handles reading and accumulating the
; hourly APCP data (i.e., data is not available in and IDL save file).
;-

  if KEYWORD_SET(AORCGridInfo) then begin

;+
;     Verify the structure of AORCGridInfo.
;-
      foo = SIZE(AORCGridInfo)
      if (foo[0] ne 1) then begin
          ERR_MSG, 'AORC_GRID_INFO structure mismatch (non-scalar).'
          RETURN
      endif

      if ((foo[1] ne 1) or (foo[2] ne 8)) then begin
          ERR_MSG, 'AORC_GRID_INFO structure mismatch ' + $
                   '(not a structure).'
          RETURN
      endif

      structOk = 1

      tagNames = TAG_NAMES(AORCGridInfo)

      ind = WHERE(tagNames eq 'MINLON', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "minLon" tag in AORC_GRID_INFO.'
          structOk = 0
      endif
      ind = WHERE(tagNames eq 'MAXLON', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "maxLon" tag in AORC_GRID_INFO.'
          structOk = 0
      endif
      ind = WHERE(tagNames eq 'NCOLS', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "nCols" tag in AORC_GRID_INFO.'
          structOk = 0
      endif
      ind = WHERE(tagNames eq 'LONRES', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "lonRes" tag in AORC_GRID_INFO.'
          structOk = 0
      endif
      ind = WHERE(tagNames eq 'MINLAT', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "minLat" tag in AORC_GRID_INFO.'
          structOk = 0
      endif
      ind = WHERE(tagNames eq 'MAXLAT', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "maxLat" tag in AORC_GRID_INFO.'
          structOk = 0
      endif
      ind = WHERE(tagNames eq 'NROWS', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "nRows" tag in AORC_GRID_INFO.'
          structOk = 0
      endif
      ind = WHERE(tagNames eq 'LATRES', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "latRes" tag in AORC_GRID_INFO.'
          structOk = 0
      endif

      if structOk then begin

          if NOT(ISA(AORCGridInfo.minLon, 'DOUBLE')) then begin
              ERR_MSG, 'AORC_GRID_INFO element "minLon" is not ' + $
                       'type DOUBLE.'
              structOk = 0
          endif
          if NOT(ISA(AORCGridInfo.maxLon, 'DOUBLE')) then begin
              ERR_MSG, 'AORC_GRID_INFO element "maxLon" is not ' + $
                       'type DOUBLE.'
              structOk = 0
          endif
          if NOT(ISA(AORCGridInfo.nCols, 'LONG')) then begin
              ERR_MSG, 'AORC_GRID_INFO element "nCols" is not ' + $
                       'type LONG.'
              structOk = 0
          endif
          if NOT(ISA(AORCGridInfo.lonRes, 'DOUBLE')) then begin
              ERR_MSG, 'AORC_GRID_INFO element "lonRes" is not ' + $
                       'type DOUBLE.'
              structOk = 0
          endif
          if NOT(ISA(AORCGridInfo.minLat, 'DOUBLE')) then begin
              ERR_MSG, 'AORC_GRID_INFO element "minLat" is not ' + $
                       'type DOUBLE.'
              structOk = 0
          endif
          if NOT(ISA(AORCGridInfo.maxLat, 'DOUBLE')) then begin
              ERR_MSG, 'AORC_GRID_INFO element "maxLat" is not ' + $
                       'type DOUBLE.'
              structOk = 0
          endif
          if NOT(ISA(AORCGridInfo.nRows, 'LONG')) then begin
              ERR_MSG, 'AORC_GRID_INFO element "nRows" is not ' + $
                       'type LONG.'
              structOk = 0
          endif
          if NOT(ISA(AORCGridInfo.latRes, 'DOUBLE')) then begin
              ERR_MSG, 'AORC_GRID_INFO element "latRes" is not ' + $
                       'type DOUBLE.'
              structOk = 0
          endif

      endif

      if NOT(structOk) then begin
          ERR_MSG, 'Unexpected structure/content in AORC_GRID_INFO ' + $
                   'structure.'
          RETURN
      endif

  endif

;+
; Loop over hours and accumulate data.
;-
  for hc = 0, DurationHours - 1 do begin

      date_Julian = AccEndDate_Julian - $
                    DOUBLE(DurationHours - 1 - hc) / 24.0D
      date_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(date_Julian)

      hourlyAPCPGrid = !NULL

      GET_HOURLY_AORC_APCP, date_YYYYMMDDHH, $
                             AORCDir, $
                             ScratchDir, $
                             NoDataValue, $
                             hourlyAPCPGrid, $
                             AORC_GRID_INFO = AORCGridInfo

      if NOT(ISA(hourlyAPCPGrid)) then begin
          ERR_MSG, 'AORC for ' + date_YYYYMMDDHH + ' not found.'
          RETURN
      endif

      if (hc eq 0) then begin
          accAPCPGrid_ = hourlyAPCPGrid
      endif else begin
          ind = WHERE((accAPCPGrid_ eq NoDataValue) or $
                      (hourlyAPCPGrid eq NoDataValue), count)
          accAPCPGrid_ = accAPCPGrid_ + hourlyAPCPGrid
          if (count gt 0) then accAPCPGrid_[ind] = NoDataValue
      endelse

  endfor

;+
; Free memory.
;-
  hourlyAPCPGrid = !NULL

;+
; Create an IDL save file of results for future calls to this
; procedure with the same basic arguments.
;-
  ndv_ = NoDataValue
  AORCGridInfo_ = AORCGridInfo

  SAVE, accAPCPGrid_, ndv_, AORCGridInfo_, $
        FILE = ScratchDir + '/' + savFile

  USR_MSG, 'AORC accumulation grid saved to ' + $
           ScratchDir + '/' + savFile

;+
; Copy results into the output variable.
;-
  accAPCPGrid = TEMPORARY(accAPCPGrid_)


  RETURN

end
