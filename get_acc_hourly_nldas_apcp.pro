PRO GET_ACC_HOURLY_NLDAS_APCP, $
    AccEndDate_YYYYMMDDHH, $
    DurationHours, $
    NLDASDir, $
    ScratchDir, $
    NoDataValue, $
    accAPCPGrid, $
    NLDAS_GRID_INFO = NLDASGridInfo

;+
; Accumulate 1-hour North American Land Data Assimilation System
; (NLDAS) Version 2 precipitation forcings.
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
  savFile = 'NLDAS_APCP_' + STRCRA(DurationHours) + $
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

      if NOT(ISA(NLDASGridInfo_)) then begin
          ERR_MSG, 'No NLDAS grid info structure in ' + $
                   ScratchDir + '/' + savFile
          RETURN
      endif

;+
;     Verify the structure of NLDASGridInfo_ and grid dimensions.
;-
      foo = SIZE(NLDASGridInfo_)
      if (foo[0] ne 1) then begin
          ERR_MSG, 'NLDAS_GRID_INFO structure mismatch (non-scalar) ' + $
                   'in ' + ScratchDir + '/' + savFile
          RETURN
      endif

      if ((foo[1] ne 1) or (foo[2] ne 8)) then begin
          ERR_MSG, 'NLDAS_GRID_INFO structure mismatch ' + $
                   '(not a structure) ' + $
                   'in ' + ScratchDir + '/' + savFile
          RETURN
      endif

      savFileOK = 1
      tagNames = TAG_NAMES(NLDASGridInfo_)

      ;; NLDASGridInfo = {minLon: 0.0D, $
      ;;                 maxLon: 0.0D, $
      ;;                 nCols: 0L, $
      ;;                 lonRes: 0.0D, $
      ;;                 minLat: 0.0D, $
      ;;                 maxLat: 0.0D, $
      ;;                 nRows: 0L, $
      ;;                 latRes: 0.0D}

      ind = WHERE(tagNames eq 'MINLON', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "minLon" tag in NLDAS_GRID_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'MAXLON', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "maxLon" tag in NLDAS_GRID_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'NCOLS', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "nCols" tag in NLDAS_GRID_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'LONRES', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "lonRes" tag in NLDAS_GRID_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'MINLAT', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "minLat" tag in NLDAS_GRID_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'MAXLAT', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "maxLat" tag in NLDAS_GRID_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'NROWS', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "nRows" tag in NLDAS_GRID_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'LATRES', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "latRes" tag in NLDAS_GRID_INFO.'
          savFileOK = 0
      endif

      if savFileOK then begin

          if NOT(ISA(NLDASGridInfo_.minLon, 'DOUBLE')) then begin
              ERR_MSG, 'NLDAS_GRID_INFO missing DOUBLE element ' + $
                       '"minLon" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(NLDASGridInfo_.maxLon, 'DOUBLE')) then begin
              ERR_MSG, 'NLDAS_GRID_INFO missing DOUBLE element ' + $
                       '"maxLon" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(NLDASGridInfo_.nCols, 'LONG')) then begin
              ERR_MSG, 'NLDAS_GRID_INFO missing LONG element ' + $
                       '"nCols" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(NLDASGridInfo_.lonRes, 'DOUBLE')) then begin
              ERR_MSG, 'NLDAS_GRID_INFO missing DOUBLE element ' + $
                       '"lonRes" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(NLDASGridInfo_.minLat, 'DOUBLE')) then begin
              ERR_MSG, 'NLDAS_GRID_INFO missing DOUBLE element ' + $
                       '"minLat" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(NLDASGridInfo_.maxLat, 'DOUBLE')) then begin
              ERR_MSG, 'NLDAS_GRID_INFO missing DOUBLE element ' + $
                       '"maxLat" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(NLDASGridInfo_.nRows, 'LONG')) then begin
              ERR_MSG, 'NLDAS_GRID_INFO missing LONG element ' + $
                       '"nRows" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(NLDASGridInfo_.latRes, 'DOUBLE')) then begin
              ERR_MSG, 'NLDAS_GRID_INFO missing DOUBLE element ' + $
                       '"latRes" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if (nCols ne NLDASGridInfo_.nCols) then begin
              ERR_MSG, 'Grid columns in ' + $
                       ScratchDir + '/' + savFile + $
                       ' (' + STRCRA(nCols) + ') ' + $
                       'do not match NLDAS_GRID_INFO value ' + $
                       '(' + STRCRA(NLDASGridInfo_.nCols) + ').'
              savFileOK = 0
          endif

          if (nRows ne NLDASGridInfo_.nRows) then begin
              ERR_MSG, 'Grid rows in ' + $
                       ScratchDir + '/' + savFile + $
                       ' (' + STRCRA(nRows) + ') ' + $
                       'do not match NLDAS_GRID_INFO value ' + $
                       '(' + STRCRA(NLDASGridInfo_.nRows) + ').'
              savFileOK = 0
          endif

      endif

      if (savFileOK and KEYWORD_SET(NLDASGridInfo)) then begin

;+
;         Verify NLDASGridInfo structure from caller against
;         NLDASGridInfo_ structure from save file.
;-
          if NOT(COMPARE(NLDASGridInfo.minLon, NLDASGridInfo_.minLon)) $
          then begin
              ERR_MSG, 'NLDAS_GRID_INFO structure "minLon" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(NLDASGridInfo.maxLon, NLDASGridInfo_.maxLon)) $
          then begin
              ERR_MSG, 'NLDAS_GRID_INFO structure "maxLon" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if (NLDASGridInfo.nCols ne NLDASGridInfo_.nCols) then begin
              ERR_MSG, 'NLDAS_GRID_INFO structure "nCols" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(NLDASGridInfo.lonRes, NLDASGridInfo_.lonRes)) $
              then begin
              ERR_MSG, 'NLDAS_GRID_INFO structure "lonRes" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(NLDASGridInfo.minLat, NLDASGridInfo_.minLat)) $
              then begin
              ERR_MSG, 'NLDAS_GRID_INFO structure "minLat" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(NLDASGridInfo.maxLat, NLDASGridInfo_.maxLat)) $
              then begin
              ERR_MSG, 'NLDAS_GRID_INFO structure "maxLat" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if (NLDASGridInfo.nRows ne NLDASGridInfo_.nRows) then begin
              ERR_MSG, 'NLDAS_GRID_INFO structure "nRows" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(NLDASGridInfo.latRes, NLDASGridInfo_.latRes)) $
          then begin
              ERR_MSG, 'NLDAS_GRID_INFO structure "latRes" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif
 
      endif else begin

;+
;         Copy NLDASGridInfo structure from save file.

          if savFileOK then NLDASGridInfo = NLDASGridInfo_

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

      USR_MSG, 'NLDAS accumulation grid restored from ' + $
               ScratchDir + '/' + savFile

      RETURN

  endif

;+
; The remainder of the procedure handles reading and accumulating the
; hourly APCP data (i.e., data is not available in and IDL save file).
;-

  if KEYWORD_SET(NLDASGridInfo) then begin

;+
;     Verify the structure of NLDASGridInfo.
;-
      foo = SIZE(NLDASGridInfo)
      if (foo[0] ne 1) then begin
          ERR_MSG, 'NLDAS_GRID_INFO structure mismatch (non-scalar).'
          RETURN
      endif

      if ((foo[1] ne 1) or (foo[2] ne 8)) then begin
          ERR_MSG, 'NLDAS_GRID_INFO structure mismatch ' + $
                   '(not a structure).'
          RETURN
      endif

      structOk = 1

      tagNames = TAG_NAMES(NLDASGridInfo)

      ind = WHERE(tagNames eq 'MINLON', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "minLon" tag in NLDAS_GRID_INFO.'
          structOk = 0
      endif
      ind = WHERE(tagNames eq 'MAXLON', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "maxLon" tag in NLDAS_GRID_INFO.'
          structOk = 0
      endif
      ind = WHERE(tagNames eq 'NCOLS', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "nCols" tag in NLDAS_GRID_INFO.'
          structOk = 0
      endif
      ind = WHERE(tagNames eq 'LONRES', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "lonRes" tag in NLDAS_GRID_INFO.'
          structOk = 0
      endif
      ind = WHERE(tagNames eq 'MINLAT', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "minLat" tag in NLDAS_GRID_INFO.'
          structOk = 0
      endif
      ind = WHERE(tagNames eq 'MAXLAT', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "maxLat" tag in NLDAS_GRID_INFO.'
          structOk = 0
      endif
      ind = WHERE(tagNames eq 'NROWS', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "nRows" tag in NLDAS_GRID_INFO.'
          structOk = 0
      endif
      ind = WHERE(tagNames eq 'LATRES', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "latRes" tag in NLDAS_GRID_INFO.'
          structOk = 0
      endif

      if structOk then begin

          if NOT(ISA(NLDASGridInfo.minLon, 'DOUBLE')) then begin
              ERR_MSG, 'NLDAS_GRID_INFO element "minLon" is not ' + $
                       'type DOUBLE.'
              structOk = 0
          endif
          if NOT(ISA(NLDASGridInfo.maxLon, 'DOUBLE')) then begin
              ERR_MSG, 'NLDAS_GRID_INFO element "maxLon" is not ' + $
                       'type DOUBLE.'
              structOk = 0
          endif
          if NOT(ISA(NLDASGridInfo.nCols, 'LONG')) then begin
              ERR_MSG, 'NLDAS_GRID_INFO element "nCols" is not ' + $
                       'type LONG.'
              structOk = 0
          endif
          if NOT(ISA(NLDASGridInfo.lonRes, 'DOUBLE')) then begin
              ERR_MSG, 'NLDAS_GRID_INFO element "lonRes" is not ' + $
                       'type DOUBLE.'
              structOk = 0
          endif
          if NOT(ISA(NLDASGridInfo.minLat, 'DOUBLE')) then begin
              ERR_MSG, 'NLDAS_GRID_INFO element "minLat" is not ' + $
                       'type DOUBLE.'
              structOk = 0
          endif
          if NOT(ISA(NLDASGridInfo.maxLat, 'DOUBLE')) then begin
              ERR_MSG, 'NLDAS_GRID_INFO element "maxLat" is not ' + $
                       'type DOUBLE.'
              structOk = 0
          endif
          if NOT(ISA(NLDASGridInfo.nRows, 'LONG')) then begin
              ERR_MSG, 'NLDAS_GRID_INFO element "nRows" is not ' + $
                       'type LONG.'
              structOk = 0
          endif
          if NOT(ISA(NLDASGridInfo.latRes, 'DOUBLE')) then begin
              ERR_MSG, 'NLDAS_GRID_INFO element "latRes" is not ' + $
                       'type DOUBLE.'
              structOk = 0
          endif

      endif

      if NOT(structOk) then begin
          ERR_MSG, 'Unexpected structure/content in NLDAS_GRID_INFO ' + $
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

      GET_HOURLY_NLDAS_APCP, date_YYYYMMDDHH, $
                             NLDASDir, $
                             ScratchDir, $
                             NoDataValue, $
                             hourlyAPCPGrid, $
                             NLDAS_GRID_INFO = NLDASGridInfo

      if NOT(ISA(hourlYAPCPGrid)) then begin
          ERR_MSG, 'NLDAS for ' + date_YYYYMMDDHH + ' not found.'
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
  NLDASGridInfo_ = NLDASGridInfo

  SAVE, accAPCPGrid_, ndv_, NLDASGridInfo_, $
        FILE = ScratchDir + '/' + savFile

  USR_MSG, 'NLDAS accumulation grid saved to ' + $
           ScratchDir + '/' + savFile

;+
; Copy results into the output variable.
;-
  accAPCPGrid = TEMPORARY(accAPCPGrid_)


  RETURN

end
