PRO GET_ACC_HOURLY_MRMS_QPE, $
    AccEndDate_YYYYMMDDHH, $
    DurationHours, $
    MRMSDir, $
    ScratchDir, $
    NoDataValue, $
    accQPEGrid, $
    RADAR_ONLY = RadarOnly, $
    MRMS_GRID_INFO = MRMSGridInfo, $
    MAX_MISSING_HOURS = maxMissingHours

;+
; Accumulate 1-hour Multi-Radar Multi-Sensor (MRMS) quantitative
; precipitation estimates (QPE).
;-

;+
; Initialize output grid to !NULL
;-
  accQPEGrid = !NULL

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
;  if (DurationHours gt 24) then begin
;      ERR_MSG, 'Duration argument cannot exceed 24 hours.'
;      RETURN
;  endif

  if NOT(ISA(MRMSDir, 'STRING')) then begin
      ERR_MSG, 'Location of MRMS archive must be a STRING.'
      RETURN
  endif

  if NOT(FILE_TEST(MRMSDir, /DIRECTORY)) then begin
      ERR_MSG, 'MRMS archive directory "' + MRMSDir + '" not found.'
      RETURN
  endif

  if NOT(FILE_TEST(MRMSDir, /READ)) then begin
      ERR_MSG, 'MRMS archive directory "' + MRMSDir + '" not readable.'
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

  if NOT(KEYWORD_SET(MaxMissingHours)) then MaxMissingHours = 0

  if (MaxMissingHours lt 0) then begin
      ERR_MSG, 'Maximum bad/missing hours must be a nonnegative integer ' + $
               'number of hours.'
      RETURN
  endif

  if (MaxMissingHours ge DurationHours) then begin
      ERR_MSG, 'Maximum bad/missing hours must be less than duration.'
      RETURN
  endif

;+
; Look for a save file.
;-
  if KEYWORD_SET(RadarOnly) then $
      savFile = 'MRMS_Radar_Only_QPE_' + STRCRA(DurationHours) + $
                'h_ending_' + AccEndDate_YYYYMMDDHH + '.sav' $
  else $
      savFile = 'MRMS_Gauge_Corrected_QPE_' + STRCRA(DurationHours) + $
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
      if NOT(ISA(accQPEGrid_)) then begin
          ERR_MSG, 'No QPE grid variable in ' + ScratchDir + '/' + savFile
          RETURN
      endif

      foo = SIZE(accQPEGrid_)
      if (foo[0] ne 2) then begin
          ERR_MSG, 'QPE grid in ' + ScratchDir + '/' + saveFile + $
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

      if NOT(ISA(MRMSGridInfo_)) then begin
          ERR_MSG, 'No MRMS grid info structure in ' + $
                   ScratchDir + '/' + savFile
          RETURN
      endif

;+
;     Verify the structure of MRMSGridInfo_ and grid dimensions.
;-
      foo = SIZE(MRMSGridInfo_)
      if (foo[0] ne 1) then begin
          ERR_MSG, 'MRMS_GRID_INFO structure mismatch (non-scalar) ' + $
                   'in ' + ScratchDir + '/' + savFile
          RETURN
      endif

      if ((foo[1] ne 1) or (foo[2] ne 8)) then begin
          ERR_MSG, 'MRMS_GRID_INFO structure mismatch ' + $
                   '(not a structure) ' + $
                   'in ' + ScratchDir + '/' + savFile
          RETURN
      endif

      savFileOK = 1
      tagNames = TAG_NAMES(MRMSGridInfo_)

      ;; MRMSGridInfo = {minLon: 0.0D, $
      ;;                 maxLon: 0.0D, $
      ;;                 nCols: 0L, $
      ;;                 lonRes: 0.0D, $
      ;;                 minLat: 0.0D, $
      ;;                 maxLat: 0.0D, $
      ;;                 nRows: 0L, $
      ;;                 latRes: 0.0D}

      ind = WHERE(tagNames eq 'MINLON', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "minLon" tag in MRMS_GRID_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'MAXLON', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "maxLon" tag in MRMS_GRID_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'NCOLS', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "nCols" tag in MRMS_GRID_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'LONRES', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "lonRes" tag in MRMS_GRID_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'MINLAT', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "minLat" tag in MRMS_GRID_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'MAXLAT', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "maxLat" tag in MRMS_GRID_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'NROWS', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "nRows" tag in MRMS_GRID_INFO.'
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'LATRES', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "latRes" tag in MRMS_GRID_INFO.'
          savFileOK = 0
      endif

      if savFileOK then begin

          if NOT(ISA(MRMSGridInfo_.minLon, 'DOUBLE')) then begin
              ERR_MSG, 'MRMS_GRID_INFO missing DOUBLE element ' + $
                       '"minLon" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(MRMSGridInfo_.maxLon, 'DOUBLE')) then begin
              ERR_MSG, 'MRMS_GRID_INFO missing DOUBLE element ' + $
                       '"maxLon" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(MRMSGridInfo_.nCols, 'LONG')) then begin
              ERR_MSG, 'MRMS_GRID_INFO missing LONG element ' + $
                       '"nCols" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(MRMSGridInfo_.lonRes, 'DOUBLE')) then begin
              ERR_MSG, 'MRMS_GRID_INFO missing DOUBLE element ' + $
                       '"lonRes" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(MRMSGridInfo_.minLat, 'DOUBLE')) then begin
              ERR_MSG, 'MRMS_GRID_INFO missing DOUBLE element ' + $
                       '"minLat" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(MRMSGridInfo_.maxLat, 'DOUBLE')) then begin
              ERR_MSG, 'MRMS_GRID_INFO missing DOUBLE element ' + $
                       '"maxLat" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(MRMSGridInfo_.nRows, 'LONG')) then begin
              ERR_MSG, 'MRMS_GRID_INFO missing LONG element ' + $
                       '"nRows" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(MRMSGridInfo_.latRes, 'DOUBLE')) then begin
              ERR_MSG, 'MRMS_GRID_INFO missing DOUBLE element ' + $
                       '"latRes" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if (nCols ne MRMSGridInfo_.nCols) then begin
              ERR_MSG, 'Grid columns in ' + $
                       ScratchDir + '/' + savFile + $
                       ' (' + STRCRA(nCols) + ') ' + $
                       'do not match MRMS_GRID_INFO value ' + $
                       '(' + STRCRA(MRMSGridInfo_.nCols) + ').'
              savFileOK = 0
          endif

          if (nRows ne MRMSGridInfo_.nRows) then begin
              ERR_MSG, 'Grid rows in ' + $
                       ScratchDir + '/' + savFile + $
                       ' (' + STRCRA(nRows) + ') ' + $
                       'do not match MRMS_GRID_INFO value ' + $
                       '(' + STRCRA(MRMSGridInfo_.nRows) + ').'
              savFileOK = 0
          endif

      endif

      if (savFileOK and KEYWORD_SET(MRMSGridInfo)) then begin

;+
;         Verify MRMSGridInfo structure from caller against
;         MRMSGridInfo_ structure from save file.
;-
          if NOT(COMPARE(MRMSGridInfo.minLon, MRMSGridInfo_.minLon)) $
          then begin
              ERR_MSG, 'MRMS_GRID_INFO structure "minLon" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(MRMSGridInfo.maxLon, MRMSGridInfo_.maxLon)) $
          then begin
              ERR_MSG, 'MRMS_GRID_INFO structure "maxLon" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if (MRMSGridInfo.nCols ne MRMSGridInfo_.nCols) then begin
              ERR_MSG, 'MRMS_GRID_INFO structure "nCols" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(MRMSGridInfo.lonRes, MRMSGridInfo_.lonRes)) $
              then begin
              ERR_MSG, 'MRMS_GRID_INFO structure "lonRes" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(MRMSGridInfo.minLat, MRMSGridInfo_.minLat)) $
              then begin
              ERR_MSG, 'MRMS_GRID_INFO structure "minLat" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(MRMSGridInfo.maxLat, MRMSGridInfo_.maxLat)) $
              then begin
              ERR_MSG, 'MRMS_GRID_INFO structure "maxLat" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if (MRMSGridInfo.nRows ne MRMSGridInfo_.nRows) then begin
              ERR_MSG, 'MRMS_GRID_INFO structure "nRows" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(MRMSGridInfo.latRes, MRMSGridInfo_.latRes)) $
          then begin
              ERR_MSG, 'MRMS_GRID_INFO structure "latRes" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif
 
      endif else begin

;+
;         Copy MRMSGridInfo structure from save file.

          if savFileOK then MRMSGridInfo = MRMSGridInfo_

      endelse

      if NOT(savFileOK) then begin

          ERR_MSG, 'Unexpected structure/content in IDL save file ' + $
                   savFile + '; returning NULL grid..'
stop
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
          ind = WHERE(accQPEGrid_ eq ndv_, count)
          if (count gt 0) then accQPEGrid_[ind] = NoDatavalue
          ind = !NULL
      endif

      accQPEGrid = TEMPORARY(accQPEGrid_)

      USR_MSG, 'MRMS accumulation grid restored from ' + $
               ScratchDir + '/' + savFile

      RETURN

  endif

  if KEYWORD_SET(MRMSGridInfo) then begin

;+
;     Verify the structure of MRMSGridInfo.
;-
      foo = SIZE(MRMSGridInfo)
      if (foo[0] ne 1) then begin
          ERR_MSG, 'MRMS_GRID_INFO structure mismatch (non-scalar).'
          RETURN
      endif

      if ((foo[1] ne 1) or (foo[2] ne 8)) then begin
          ERR_MSG, 'MRMS_GRID_INFO structure mismatch ' + $
                   '(not a structure).'
          RETURN
      endif

      structOk = 1

      tagNames = TAG_NAMES(MRMSGridInfo)

      ind = WHERE(tagNames eq 'MINLON', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "minLon" tag in MRMS_GRID_INFO.'
          structOk = 0
      endif
      ind = WHERE(tagNames eq 'MAXLON', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "maxLon" tag in MRMS_GRID_INFO.'
          structOk = 0
      endif
      ind = WHERE(tagNames eq 'NCOLS', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "nCols" tag in MRMS_GRID_INFO.'
          structOk = 0
      endif
      ind = WHERE(tagNames eq 'LONRES', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "lonRes" tag in MRMS_GRID_INFO.'
          structOk = 0
      endif
      ind = WHERE(tagNames eq 'MINLAT', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "minLat" tag in MRMS_GRID_INFO.'
          structOk = 0
      endif
      ind = WHERE(tagNames eq 'MAXLAT', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "maxLat" tag in MRMS_GRID_INFO.'
          structOk = 0
      endif
      ind = WHERE(tagNames eq 'NROWS', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "nRows" tag in MRMS_GRID_INFO.'
          structOk = 0
      endif
      ind = WHERE(tagNames eq 'LATRES', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "latRes" tag in MRMS_GRID_INFO.'
          structOk = 0
      endif

      if structOk then begin

          if NOT(ISA(MRMSGridInfo.minLon, 'DOUBLE')) then begin
              ERR_MSG, 'MRMS_GRID_INFO element "minLon" is not ' + $
                       'type DOUBLE.'
              structOk = 0
          endif
          if NOT(ISA(MRMSGridInfo.maxLon, 'DOUBLE')) then begin
              ERR_MSG, 'MRMS_GRID_INFO element "maxLon" is not ' + $
                       'type DOUBLE.'
              structOk = 0
          endif
          if NOT(ISA(MRMSGridInfo.nCols, 'LONG')) then begin
              ERR_MSG, 'MRMS_GRID_INFO element "nCols" is not ' + $
                       'type LONG.'
              structOk = 0
          endif
          if NOT(ISA(MRMSGridInfo.lonRes, 'DOUBLE')) then begin
              ERR_MSG, 'MRMS_GRID_INFO element "lonRes" is not ' + $
                       'type DOUBLE.'
              structOk = 0
          endif
          if NOT(ISA(MRMSGridInfo.minLat, 'DOUBLE')) then begin
              ERR_MSG, 'MRMS_GRID_INFO element "minLat" is not ' + $
                       'type DOUBLE.'
              structOk = 0
          endif
          if NOT(ISA(MRMSGridInfo.maxLat, 'DOUBLE')) then begin
              ERR_MSG, 'MRMS_GRID_INFO element "maxLat" is not ' + $
                       'type DOUBLE.'
              structOk = 0
          endif
          if NOT(ISA(MRMSGridInfo.nRows, 'LONG')) then begin
              ERR_MSG, 'MRMS_GRID_INFO element "nRows" is not ' + $
                       'type LONG.'
              structOk = 0
          endif
          if NOT(ISA(MRMSGridInfo.latRes, 'DOUBLE')) then begin
              ERR_MSG, 'MRMS_GRID_INFO element "latRes" is not ' + $
                       'type DOUBLE.'
              structOk = 0
          endif

      endif

      if NOT(structOk) then begin
          ERR_MSG, 'Unexpected structure/content in MRMS_GRID_INFO ' + $
                   'structure.'
          RETURN
      endif

  endif

;+
; Establish the wgrib2 version, which will affect the way QPE field
; strings are defined. The wgrib2Version string should be "vX.X.X.X"
; followed by a space and a mess of names and other information. We
; just want that X.X.X.X part, which we then convert to an integer.
;-
  SPAWN, 'wgrib2 -version', wgrib2Version ;, EXIT_STATUS = status
  if (N_ELEMENTS(wgrib2Version) eq 0) then begin
      ERR_MSG, 'Unable to determine wgrib2 version.'
      RETURN
  endif
  wgrib2Version = wgrib2Version[0]

  wgrib2Version = STREGEX(wgrib2version, '^v[0-9.]+ ', /EXTRACT)
  if (STRLEN(wgrib2Version) eq 0) then begin
      ERR_MSG, 'Unable to interpret wgrib2 version.'
      RETURN
  endif

  wgrib2Version = STRMID(wgrib2Version, 1, STRLEN(wgrib2Version) - 2)

  STR_REPLACE, wgrib2Version, '.', ''
  if (STREGEX(wgrib2Version, '[^0-9]') ne -1) then begin
      ERR_MSG, 'Unable to extract wgrib2 version.'
      RETURN
  endif

  wgrib2Version = FIX(wgrib2Version)

;+
; Locate MRMS data directory.
;-
  if KEYWORD_SET(RadarOnly) then begin
      dir = MRMSDir + '/RadarOnly_QPE_01H'
      if (wgrib2Version gt 202) then $
          GRIBField = 'RadarOnlyQPE01H Radar precipitation accumulation ' + $
                      '1-hour [mm]:' $
      else $
          GRIBField = ':var discipline=209 master_table=255 ' + $
                      'parmcat=6 parm=2:'
  endif else begin
      dir = MRMSDir + '/GaugeCorr_QPE_01H'
      if (wgrib2Version gt 202) then $
          GRIBField = 'GaugeCorrQPE01H Local gauge bias corrected ' + $
                      'radar precipitation accumulation 1-hour [mm]:' $
      else $
          GRIBField = ':var discipline=209 master_table=255 ' + $
                      'parmcat=6 parm=9:'
  endelse

  if NOT(FILE_TEST(dir, /DIRECTORY)) then begin
      ERR_MSG, 'MRMS directory "' + dir + '" not found.'
      RETURN
  endif

  if NOT(FILE_TEST(dir, /READ)) then begin
      ERR_MSG, 'MRMS directory "' + dir + '" is not readable.'
      RETURN
  endif

;+
; Loop over hours and accumulate data.
;-

  perfect_ = 1B
  numMissingHours = 0

  for hc = 0, DurationHours - 1 do begin

      if (numMissingHours gt MaxMissingHours) then BREAK

      date_Julian = AccEndDate_Julian - $
                    DOUBLE(DurationHours - 1 - hc) / 24.0D
      date_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(date_Julian)

      hourlyQPEGrid = !NULL
print, date_yyyymmddhh
      if KEYWORD_SET(RadarOnly) then $
          GET_HOURLY_MRMS_QPE, date_YYYYMMDDHH, $
                               MRMSDir, $
                               ScratchDir, $
                               NoDataValue, $
                               hourlyQPEGrid, $
                               MRMS_GRID_INFO = MRMSGridInfo, $
                               /RADAR_ONLY $
      else $
          GET_HOURLY_MRMS_QPE, date_YYYYMMDDHH, $
                               MRMSDir, $
                               ScratchDir, $
                               NoDataValue, $
                               hourlyQPEGrid, $
                               MRMS_GRID_INFO = MRMSGridInfo

      if NOT(ISA(hourlyQPEGrid)) then begin
          ERR_MSG, 'MRMS for ' + date_YYYYMMDDHH + ' not found.'
          perfect_ = 0B
          numMissingHours++
          CONTINUE
      endif

;      if (hc eq 0) then begin
      if NOT(ISA(accQPEGrid_)) then begin
          accQPEGrid_ = hourlyQPEGrid
      endif else begin
          ind = WHERE((accQPEGrid_ eq NoDataValue) or $
                      (hourlyQPEGrid eq NoDataValue), count)
          accQPEGrid_ = accQPEGrid_ + hourlyQPEGrid
          if (count gt 0) then accQPEGrid_[ind] = NoDataValue
      endelse

  endfor

  if (numMissingHours gt MaxMissingHours) then begin
      ERR_MSG, 'Insufficient data available.'
      RETURN
  endif

;+
; Free memory.
;-
  hourlyQPEGrid = !NULL

  if perfect_ then begin

;+
;     Create an IDL save file of results for future calls to this
;     procedure with the same basic arguments.
;-
      ndv_ = NoDataValue
      MRMSGridInfo_ = MRMSGridInfo

      SAVE, accQPEGrid_, ndv_, MRMSGridInfo_, $
            FILE = ScratchDir + '/' + savFile

      USR_MSG, 'MRMS accumulation grid saved to ' + $
               ScratchDir + '/' + savFile

  endif

;+
; Copy results into the output variable.
;-
  accQPEGrid = TEMPORARY(accQPEGrid_)

  RETURN

end
