PRO GET_STAGE4_HRAP_QPE, accEndDate_YYYYMMDDHH, $ ; end of accumulation
                         durationHours, $ ; number of hours to aggregate
                         st4Dir, $        ; location of GRIB archive
                         scratchDir, $    ; location for temp files
                         ndv, $
                         QPEGrid, $
                         HRAP_GRID_PROJ_INFO = HRAPGridProjInfo, $
                         VERBOSE = verbose, $
                         CORRECT_FOR_MISSING = correctForMissing

  MESSAGE, 'This subroutine double-counts the first file it reads. ' + $
           'Do not use it. GET_STAGE4_QPE is the recommended replacement.'

  COMMON info, message

  QPEGrid = !NULL


; Check arguments for correct type and valid contents.

  if NOT(ISA(accEndDate_YYYYMMDDHH, 'STRING')) then begin
help, accumenddate_yyyymmddhh
      ERR_MSG, 'Target accumulation end date/time argument must be a STRING.'
      RETURN
  endif
  if (STRLEN(accEndDate_YYYYMMDDHH) ne 10) then begin
      ERR_MSG, 'Invalid target accumulation end date/time "' + $
               accEndDate_YYYYMMDDHH + $
               '" (required form is YYYYMMDDHH, 10 digits).'
      RETURN
  endif
  if NOT(STREGEX(accEndDate_YYYYMMDDHH, '[0-9]{10}', /BOOLEAN)) $
      then begin
      ERR_MSG, 'Invalid target accumulation end date/time "' + $
               accEndDate_YYYYMMDDHH + $
               '" (required form is YYYYMMDDHH, all numeric).'
      RETURN
  endif

  accEndDate_Julian = YYYYMMDDHH_TO_JULIAN(accEndDate_YYYYMMDDHH)
  accEndDate_YYYY = STRMID(accEndDate_YYYYMMDDHH, 0, 4)
  accEndDate_MM = STRMID(accEndDate_YYYYMMDDHH, 4, 2)
  accEndDate_DD = STRMID(accEndDate_YYYYMMDDHH, 6, 2)
  accEndDate_HH = STRMID(accEndDate_YYYYMMDDHH, 8, 2)

  if (durationHours lt 0) then begin
      ERR_MSG, 'Duration must be a positive integer number of hours.'
      RETURN
  endif

  if (durationHours gt 999) then begin
      ERR_MSG, 'Duration argument cannot exceed 999 hours.'
      RETURN
  endif

  if ((durationHours mod 6) ne 0) then begin
      ERR_MSG, 'Duration argument must be a multiple of 6 (' + $
               STRCOMPRESS(durationHours, /REMOVE_ALL) + ' provided).'
      RETURN
  endif

  if NOT(ISA(st4Dir, 'STRING')) then begin
      ERR_MSG, 'Location of Stage IV archive must be a STRING.'
      RETURN
  endif

  if NOT(FILE_TEST(st4Dir, /DIRECTORY)) then begin
      ERR_MSG, 'Stage IV archive directory "' + st4Dir + '" not found.'
      RETURN
  endif

  if NOT(FILE_TEST(st4Dir, /READ)) then begin
      ERR_MSG, 'Stage IV archive directory "' + st4Dir + '" not readable.'
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

  if KEYWORD_SET(HRAPGridProjInfo) then begin


;     Verify the structure of HRAPGridProjInfo

      foo = SIZE(HRAPGridProjInfo)
      if (foo[0] ne 1) then begin
          ERR_MSG, 'HRAP_GRID_PROJ_INFO structure mismatch (non-scalar).'
          RETURN
      endif

  endif

; FIX INDENT START


; Read Stage IV data from GRIB files and cache as a GeoTIFF.

  if (durationHours eq 6) then begin


;     Read the data from a single GRIB file.

      accEndDate_YYYYMMDD = accEndDate_YYYY + accEndDate_MM + $
                              accEndDate_DD
      accEndDate_YYYYMMDDHH = accEndDate_YYYY + accEndDate_MM + $
                                accEndDate_DD + accEndDate_HH

      GRIBDir = st4Dir + '/StageIV' + accEndDate_YYYY + '/' + $
                accEndDate_YYYYMMDD
      GRIBFile = 'ST4.' + accEndDate_YYYYMMDDHH + '.' + $
                 STRING(durationHours, FORMAT = '(I2.2)') + 'h'

      ext = 'gz'

      if NOT(FILE_TEST(GRIBDir + '/' + GRIBFile + '.' + ext)) $
      then begin
          ext = 'Z'
          if NOT(FILE_TEST(GRIBDir + '/' + GRIBFile + '.' + ext)) $
          then begin
              ERR_MSG, 'Input Stage IV file ' + $
                       GRIBDir + '/' + GRIBFile + '.[gz,Z] not found.'
              RETURN
          endif
      endif
  
    
;     Create a probably-unique copy of the file in the scratchDir.

      cmd = 'date -u +%Y%m%d%H%M%S'
      SPAWN, cmd, dateStr, EXIT_STATUS = status
      if (status ne 0) then begin
          ERR_MSG, 'SPAWN of "' + cmd + '" failed.'
          RETURN
      endif
      dateStr = dateStr[0]
      pidStr = STRCRA(GETPID())

      FILE_COPY, GRIBDir + '/' + GRIBFile + '.' + ext, $
                 scratchDir + '/' + $
                 '__tmp__' + GRIBFile + $
                 '.' + dateStr + '.' + pidStr + '.' + ext

      SPAWN, 'gunzip ' + $
             scratchDir + '/' + $
             '__tmp__' + GRIBFile  + $
             '.' + dateStr + '.' + pidStr + '.' + ext, $
             EXIT_STATUS = status

      if (status ne 0) then begin
          ERR_MSG, 'Failed to gunzip GRIB file copy ' + $
                   scratchDir + '/' + $
                   '__tmp__' + GRIBFile  + $
                   '.' + dateStr + '.' + pidStr + '.' + ext
          FILE_DELETE, scratchDir + '/' + GRIBFile + '.' + ext, $
                       /ALLOW_NONEXISTENT
          RETURN
      endif

      GRIBFile = '__tmp__' + GRIBFile + $
                 '.' + dateStr + '.' + pidStr

      if NOT(FILE_TEST(scratchDir + '/' + GRIBFile)) then begin
          ERR_MSG, 'GRIB file copy ' + $
                   scratchDir + '/' + GRIBFile + $
                   ' not found after successful gunzip.'
          FILE_DELETE, scratchDir + '/' + GRIBFile + '.' + ext, $
                       /ALLOW_NONEXISTENT
          RETURN
      endif


;     Read the projected data.

      DECODE_GRIB1_FIELD, GRIBFile, scratchDir, scratchDir, $
                          GRIBFile + '.2', $
                          'APCP', $
                          varUnits, $
                          refTime_YYYYMMDDHH, $
                          timeRange, $
                          P1, $
                          P2, $
                          nx_, ny_, $
                          QPEGrid_, $
                          status, $
                          NO_DATA_VALUE = ndv

      if NOT(status) then begin
          ERR_MSG, 'Failed to decode APCP from ' + $
                   scratchDir + '/' + GRIBFile + '.2'
          FILE_DELETE, scratchDir + '/' + GRIBFile + '.2', $
                       /ALLOW_NONEXISTENT
          FILE_DELETE, scratchDir + '/' + GRIBFile, $
                       /ALLOW_NONEXISTENT
          RETURN
      endif

      if (varUnits ne 'kg/m^2') then begin
          ERR_MSG, 'GRIB record units "' + varUnits + '" ' + $
                   'does not match expected value "kg/m^2".'
          FILE_DELETE, scratchDir + '/' + GRIBFile
          RETURN
      endif

      nx = nx_
      ny = ny_

      if NOT(KEYWORD_SET(HRAPGridProjInfo)) then begin


;         Create the HRAPGridProjInfo structure to store grid and
;         projection parameters.


;         Get grid geometry and projection information.

          GET_GRIB1_POLAR_STEREO_INFO, $
              GRIBFile, scratchDir, $
              'APCP', $
              nx_, ny_, $
              lat00, lon00, latd, lonv, $
              dx, dy, $
              status

          if NOT(status) then begin
              ERR_MSG, 'Failed to read GRIB header from ' + $
                       scratchDir + '/' + GRIBFile
              FILE_DELETE, scratchDir + '/' + GRIBFile
              RETURN
          endif
              
          if (nx_ ne nx) then begin
              ERR_MSG, 'GRIB header # columns (' + $
                       STRCOMPRESS(nx_, /REMOVE_ALL) + $
                       ') does not match established value (' + $
                       STRCOMPRESS(nx, /REMOVE_ALL)
              FILE_DELETE, scratchDir + '/' + GRIBFile
              RETURN
          endif

          if (ny_ ne ny) then begin
              ERR_MSG, 'GRIB header # rows (' + $
                       STRCOMPRESS(ny_, /REMOVE_ALL) + $
                       ') does not match established value (' + $
                       STRCOMPRESS(ny, /REMOVE_ALL)
              FILE_DELETE, scratchDir + '/' + GRIBFile
              RETURN
          endif


;         Get the grid description section.

          ;; cmd = 'wgrib -GDS10 ' + scratchDir + '/' + GRIBFile
          ;; SPAWN, cmd, GDS, EXIT_STATUS = status
          ;; if ((status ne 0) or (N_ELEMENTS(GDS) ne 1)) then begin
          ;;     ERR_MSG, 'Failed to read grid description section ' + $
          ;;              'from ' + scratchDir + '/' + GRIBFile
          ;;     FILE_DELETE, scratchDir + '/' + GRIBFile
          ;;     RETURN
          ;; endif
          ;; GDS = GDS[0]
          ;; GDS = STRMID(GDS, STRPOS(GDS, 'GDS10=') + 6)
          ;; GDSBytes = FIX(STRSPLIT(GDS, ' ', /EXTRACT))
          ;; if (N_ELEMENTS(GDSBytes) ne 32) then begin
          ;;     ERR_MSG, 'Failed to interpret grid description section ' + $
          ;;              'from ' + scratchDir + '/' + GRIBFile
          ;;     FILE_DELETE, scratchDir + '/' + GRIBFile
          ;;     RETURN
          ;; endif
          ;; if (GDSBytes[5] ne 5) then begin ; Polar Stereographic
          ;;     ERR_MSG, 'Data representation type for ' + $
          ;;              scratchDir + '/' + GRIBFile + $
          ;;              ' is not polar stereographic.'
          ;;     FILE_DELETE, scratchDir + '/' + GRIBFile
          ;;     RETURN
          ;; endif
          ; nx = 256 * GDSBytes[6] + GDSBytes[7]
          ; ny = 256 * GDSBytes[8] + GDSBytes[9]
          ; lat00 = (256L^2L * GDSBytes[10] + $
          ;          256L * GDSBytes[11] + $
          ;          GDSBytes[12]) / 1000.0D
          ; lon00 = (256L^2L * GDSBytes[13] + $
          ;          256L * GDSBytes[14] + $
          ;          GDSBytes[15]) / 1000.0D <-- this one does not work
          ; 


;         Create data structure for the HRAP grid and projection.

          if (dx ne 4763.0D) then STOP
          if (dy ne 4763.0D) then STOP
          dx = 4762.5D
          dy = 4762.5D

          if (lat00 ne 23.117D) then STOP
          if (lon00 ne -119.023D) then STOP
          lat00 = 23.1167684D ; 23.11676840356748386
          lon00 = -119.0236000D ; -119.02360004492176415

          HRAPGridProjInfo = $
              {lonV:  lonV, $       ; orientation longitude
               latD:  latD, $       ; lat where dx and dy are specified
               eRadM: 6371200.0D, $ ; NCEP sphere radius, meters
               lat00: lat00, $      ; deprojected lat of LL cell center
               lon00: lon00, $      ; deprojected lon of LL cell center
               nCols: nx, $         ; # columns
               nRows: ny, $         ; # rows
               dx:    dx, $         ; x resolution at latD, meters
               dy:    dy}           ; y resolution at latD, meters

      endif else begin

          if (nx ne HRAPGridProjInfo.nCols) then STOP
          if (ny ne HRAPGridProjInfo.nRows) then STOP

      endelse

      FILE_DELETE, scratchDir + '/' + GRIBFile

  endif else begin


;     Generate the data by aggregating 6-hour accumulations.

      numSt4Cycles = durationHours / 6
      numSt4CyclesFound = 0L

      for cc = 0, numSt4Cycles - 1 do begin

          st4Date_Julian = accEndDate_Julian - $
                           DOUBLE((numSt4Cycles - 1 - cc) * 6) / 24.0D
;              st4Date_Julian = accEndDate_Julian - DOUBLE(cc * 6) / 24.0D
          st4Date_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(st4Date_Julian)
          st4Date_YYYYMMDD = STRMID(st4Date_YYYYMMDDHH, 0, 8)
          st4Date_YYYY = STRMID(st4Date_YYYYMMDDHH, 0, 4)
          st4Date_MM = STRMID(st4Date_YYYYMMDDHH, 4, 2)
          st4Date_DD = STRMID(st4Date_YYYYMMDDHH, 6, 2)
          GRIBDir = st4Dir + '/StageIV' + st4Date_YYYY + $
                    '/' + st4Date_YYYYMMDD
          GRIBFile = 'ST4.' + st4Date_YYYYMMDDHH + '.06h'

          ext = 'gz'

          if NOT(FILE_TEST(GRIBDir + '/' + GRIBFile + '.' + ext)) $
          then begin
              ext = 'Z'
              if NOT(FILE_TEST(GRIBDir + '/' + GRIBFile + '.' + ext)) $
              then begin
                  ERR_MSG, 'Input StageIV file ' + $
                           GRIBDir + '/' + GRIBFile + '.[gz,Z] not found.'
                  RETURN
              endif
          endif


;         Create a probably-unique copy of the file in the scratchDir.

          cmd = 'date -u +%Y%m%d%H%M%S'
          SPAWN, cmd, dateStr, EXIT_STATUS = status
          if (status ne 0) then begin
              ERR_MSG, 'SPAWN of "' + cmd + '" failed.'
              RETURN
          endif
          dateStr = dateStr[0]
          pidStr = STRCRA(GETPID())

          FILE_COPY, GRIBDir + '/' + GRIBFile + '.' + ext, $
                     scratchDir + '/' + $
                     '__tmp__' + GRIBFile + $
                     '.' + dateStr + '.' + pidStr + '.' + ext

          SPAWN, 'gunzip ' + scratchDir + '/' + $
                 '__tmp__' + GRIBFile + $
                 '.' + dateStr + '.' + pidStr + '.' + ext, $
                 EXIT_STATUS = status

          if (status ne 0) then begin
              ERR_MSG, 'Failed to gunzip GRIB file copy ' + $
                       scratchDir + '/' + $
                       '__tmp__' + GRIBFile  + $
                       '.' + dateStr + '.' + pidStr + '.' + ext
              FILE_DELETE, scratchDir + '/' + GRIBFile + '.' + ext, $
                           /ALLOW_NONEXISTENT
              RETURN
          endif

          GRIBFile = '__tmp__' + GRIBFile + $
                     '.' + dateStr + '.' + pidStr

          if NOT(FILE_TEST(scratchDir + '/' + GRIBFile)) then begin
              ERR_MSG, 'GRIB file copy ' + $
                       scratchDir + '/' + GRIBFile + $
                       ' not found after successful gunzip.'
              FILE_DELETE, scratchDir + '/' + GRIBFile + '.' + ext, $
                           /ALLOW_NONEXISTENT
              RETURN
          endif


;         Read the projected data.

          DECODE_GRIB1_FIELD, GRIBFile, scratchDir, scratchDir, $
                              GRIBFile + '.2', $
                              'APCP', $
                              varUnits, $
                              refTime_YYYYMMDDHH, $
                              timeRange, $
                              P1, $
                              P2, $
                              nx_, ny_, $
                              hourlyAPCPProj, $
                              status, $
                              NO_DATA_VALUE = ndv

          if NOT(status) then begin
              ERR_MSG, 'Failed to decode APCP from ' + $
                       scratchDir + '/' + GRIBFile + '.2'
              FILE_DELETE, scratchDir + '/' + GRIBFile + '.2', $
                           /ALLOW_NONEXISTENT
              FILE_DELETE, scratchDir + '/' + GRIBFile, $
                           /ALLOW_NONEXISTENT
              RETURN
          endif

          if (varUnits ne 'kg/m^2') then begin
              ERR_MSG, 'GRIB record units "' + varUnits + '" ' + $
                       'does not match expected value "kg/m^2".'
              FILE_DELETE, scratchDir + '/' + GRIBFile
              RETURN
          endif

          ;; ind = WHERE(hourlyAPCPProj eq ndv, count)
          ;; if (count gt 400000L) then begin
          ;;     ERR_MSG, 'Incomplete grid for ' + $
          ;;              st4Date_YYYYMMDDHH + '; skipping'
          ;;     FILE_DELETE, scratchDir + '/' + GRIBFile
          ;;     CONTINUE
          ;; endif

          if (numSt4CyclesFound eq 0) then begin
              QPEGrid_ = hourlyAPCPProj
              nx = nx_
              ny = ny_
          endif

          if ((numSt4CyclesFound eq 0) and $
              NOT(KEYWORD_SET(HRAPGridProjInfo))) then begin


;             Get grid geometry and projection information.

              GET_GRIB1_POLAR_STEREO_INFO, $
                  GRIBFile, scratchDir, $
                  'APCP', $
                  nx_, ny_, $
                  lat00, lon00, latd, lonv, $
                  dx, dy, $
                  status

              if NOT(status) then begin
                  ERR_MSG, 'Failed to read GRIB header from ' + $
                           scratchDir + '/' + GRIBFile
                  FILE_DELETE, scratchDir + '/' + GRIBFile
                  RETURN
              endif

              if (nx_ ne nx) then begin
                  ERR_MSG, 'GRIB header # columns (' + $
                           STRCOMPRESS(nx_, /REMOVE_ALL) + $
                           ') does not match established value (' + $
                           STRCOMPRESS(nx, /REMOVE_ALL)
                  FILE_DELETE, scratchDir + '/' + GRIBFile
                  RETURN
              endif

              if (ny_ ne ny) then begin
                  ERR_MSG, 'GRIB header # rows (' + $
                           STRCOMPRESS(ny_, /REMOVE_ALL) + $
                           ') does not match established value (' + $
                           STRCOMPRESS(ny, /REMOVE_ALL)
                  FILE_DELETE, scratchDir + '/' + GRIBFile
                  RETURN
              endif


;             Create data structure for the HRAP grid and projection.

              HRAPGridProjInfo = $
                  {lonV:  lonV, $ ; orientation longitude
                   latD:  latD, $ ; latitude where dx and dy are specified
                   eRadM: 6371200.0D, $ ; HRAP grid earth radius, meters
                   lat00: lat00, $ ; deprojected latitude of LL cell center
                   lon00: lon00, $ ; deprojected longitude of LL cell center
                   nCols: nx, $    ; # columns
                   nRows: ny, $    ; # rows
                   dx:    dx, $    ; x resolution at latD, meters
                   dy:    dy}      ; y resolution at latD, meters

              endif else begin


;                 As long as the grid size matches previous files,
;                 assume grid geometry and projection information has
;                 not changed.

                  if (nx_ ne nx) then begin
                      ERR_MSG, 'GRIB record # columns (' + $
                               STRCOMPRESS(nx_, /REMOVE_ALL) + $
                               ') does not match established value (' + $
                               STRCOMPRESS(nx, /REMOVE_ALL)
                      FILE_DELETE, scratchDir + '/' + GRIBFile
                      RETURN
                  endif

                  if (ny_ ne ny) then begin
                      ERR_MSG, 'GRIB record # rows (' + $
                               STRCOMPRESS(ny_, /REMOVE_ALL) + $
                               ') does not match established value (' + $
                               STRCOMPRESS(ny, /REMOVE_ALL)
                      FILE_DELETE, scratchDir + '/' + GRIBFile
                      RETURN
                  endif


;                 Aggregate data.

                  ind = WHERE((QPEGrid_ eq ndv) or $
                              (hourlyAPCPProj eq ndv), count)
                  QPEGrid_ = QPEGrid_ + hourlyAPCPProj
                  if (count gt 0) then QPEGrid_[ind] = ndv

              endelse

              FILE_DELETE, scratchDir + '/' + GRIBFile

              numSt4CyclesFound++

          endfor

          hourlyAPCPProj = !NULL ; clear memory

          if (KEYWORD_SET(correctForMissing) and $
              (numSt4CyclesFound lt numSt4Cycles)) then begin
              ind = WHERE(QPEGrid_ eq ndv, count)
              adjFactor = FLOAT(numSt4Cycles) / FLOAT(numSt4CyclesFound)
              QPEGrid_ = QPEGrid_ * adjFactor
              if (count gt 0) then QPEGrid_[ind] = ndv
              ERR_MSG, 'WARNING: adjusted accumulated Stage IV QPE ' + $
                       'by ' + STRCRA(adjFactor) + $
                       ' to correct for missing data.'
          endif

      endelse

; FIX INDENT END

  QPEGrid = TEMPORARY(QPEGrid_)

  RETURN

end
