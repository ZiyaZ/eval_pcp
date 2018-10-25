;+
; Read and aggregate archived GFS precipitation data from a single GFS
; cycle, then perform Mountain Mapper downscaling to the National
; Water Model grid, replicating the precipitation downscaling process
; for NWM medium range forcing engine version 2.0.
;
; Greg Fall, OWP-Chanhassen
; 2018-05-31
;-

PRO GET_ACC_GFS_QPF_NWM_V2_0, CycleDate_YYYYMMDDHH, $
                              AccumHours, $
                              GFSDir, $
                              MMDir, $
                              ScratchDir, $
                              NoDataValue, $
                              acc_GFS_QPF_NWMv20_grid, $
                              GFS_GRID_INFO = GFSGridInfo, $
                              NWM_GRID_PROJ_INFO = NWMGridInfo, $
                              LON_GRID_NWM = lonGrid_NWM, $
                              LAT_GRID_NWM = latGrid_NWM, $
                              I_GFS_NWM = i_GFS_NWM, $
                              J_GFS_NWM = j_GFS_NWM

  acc_GFS_QPF_NWMv20_grid = !NULL

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
  cycleDate_MM = STRMID(CycleDate_YYYYMMDDHH, 4, 2)
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

  if NOT(ISA(MMDir, 'STRING')) then begin
      ERR_MSG, 'Location of Mountain Mapper grids must be a STRING.'
      RETURN
  endif
  if NOT(FILE_TEST(MMDir, /DIRECTORY)) then begin
      ERR_MSG, 'Mountain Mapper grid directory "' + MMDir + '" not found.'
      RETURN
  endif
  if NOT(FILE_TEST(MMDir, /READ)) then begin
      ERR_MSG, 'Mountain Mapper grid directory "' + MMDir + '" not readable.'
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
; Get the NWM grid/projection information.
;-
  if NOT(KEYWORD_SET(NWMGridInfo)) then $
      GET_NWM_GRID_PROJ_INFO, NWMGridInfo

;+
; Get longitudes and latitudes of NWM cells, for regridding things to
; the NWM grid.
;-
  if (NOT(KEYWORD_SET(lonGrid_NWM)) or $
      NOT(KEYWORD_SET(latGrid_NWM))) then begin
      USR_MSG, 'Getting NWM grid longitudes and latitudes.'
      GET_LON_LAT_OF_LCC_GRID_CENTERS, NWMGridInfo, $
                                       lonGrid_NWM, $
                                       latGrid_NWM
  endif

;+
; Determine if the QPF accumulation occurs during different calendar
; months, which will require performing the Mountain Mapper
; downscaling process twice.
;-
  lastTimeStepStart_Julian = cycleDate_Julian + $
                             DOUBLE(AccumHours - 1) / 24.0D
  lastTimeStepStart_YYYYMMDDHH = $
      JULIAN_TO_YYYYMMDDHH(lastTimeStepStart_Julian)
  lastTimeStepStart_YYYY = STRMID(lastTimeStepStart_YYYYMMDDHH, 0, 4)
  lastTimeStepStart_MM = STRMID(lastTimeStepStart_YYYYMMDDHH, 4, 2)

  numMonths = FIX(lastTimeStepStart_MM) - FIX(cycleDate_MM) + 1
  if (numMonths gt 2) then STOP

;+
; Calculate the Julian time for 00Z on the first day of the month
; during which the last time step occurs. If the accumulation occurs
; during different calendar months, this is 00Z on the first day of
; the second month. It is needed to set the CYCLE_OFFSET_HOURS keyword
; for reading GFS data for the portion of the accumulation occurring
; during the second month.
;-
  lastTimeStepMonthStart_Julian = $
      YYYYMMDDHH_TO_JULIAN(lastTimeStepStart_YYYY + $
                           lastTimeStepStart_MM + $
                           '0100')

;+
; Read and downscale GFS data for the accumulation period, performing
; the process separately for each calendar month in which the
; accumulation occurs.
;-
  for mc = 0, numMonths - 1 do begin

;+
;     Determine applicable month of PRISM climatology.
;-
      PRISM_month = ((FIX(cycleDate_MM) + mc - 1) mod 12) + 1

;+
;     Read Mountain Mapper numerator.
;-
      PRISMNumerFile = 'PRISM_Precip_Clim_' + $
                       STRMID(MONTH_NAME(PRISM_month), 0, 3) + $
                       '_NWM_Mtn_Mapper_Numer.nc'
      if NOT(FILE_TEST(MMDir + '/' + PRISMNumerFile)) then STOP

      id = NCDF_OPEN(MMDir + '/' + PRISMNumerFile)
      NCDF_VARGET, id, 'Data', PRISMNumerGrid
      NCDF_ATTGET, id, 'Data', 'no_data_value', ndv_
      if (ndv_ ne NoDataValue) then begin
          ind = WHERE(PRISMNumerGrid eq ndv_, count)
          if (count gt 0) then PRISMNumerGrid[ind] = NoDataValue
      endif
      NCDF_CLOSE, id
      foo = SIZE(PRISMNumerGrid)
      if (foo[0] ne 2) then STOP
      if (foo[1] ne NWMGridInfo.nCols) then STOP
      if (foo[2] ne NWMGridInfo.nRows) then STOP
      ind = WHERE(PRISMNumerGrid ne NoDataValue, count)
      if (count eq 0) then STOP
      PRINT, PRISMNumerFile + ' min - max: ' + $
             STRCRA(MIN(PRISMNumerGrid[ind])) + ' - ' + $
             STRCRA(MAX(PRISMNumerGrid[ind]))

;+
;     Read Mountain Mapper denominator.
;-
      PRISMDenomFile = 'PRISM_Precip_Clim_' + $
                       STRMID(MONTH_NAME(PRISM_month), 0, 3) + $
                       '_NWM_Mtn_Mapper_Denom.nc'

      if NOT(FILE_TEST(MMDir + '/' + PRISMDenomFile)) then STOP

      id = NCDF_OPEN(MMDir + '/' + PRISMDenomFile)
      NCDF_VARGET, id, 'Data', PRISMDenomGrid
      NCDF_ATTGET, id, 'Data', 'no_data_value', ndv_
      if (ndv_ ne NoDataValue) then begin
          ind = WHERE(PRISMDenomGrid eq ndv_, count)
          if (count gt 0) then PRISMDenomGrid[ind] = NoDataValue
      endif
      NCDF_CLOSE, id
      foo = SIZE(PRISMDenomGrid)
      if (foo[0] ne 2) then STOP
      if (foo[1] ne NWMGridInfo.nCols) then STOP
      if (foo[2] ne NWMGridInfo.nRows) then STOP
      ind = WHERE(PRISMDenomGrid ne NoDataValue, count)
      if (count eq 0) then STOP
      PRINT, PRISMDenomFile + ' min - max: ' + $
             STRCRA(MIN(PRISMDenomGrid[ind])) + ' - ' + $
             STRCRA(MAX(PRISMDenomGrid[ind]))

      ; mc = 0, cycle_offset_hours = 0
      ;         accum hours = start of next month 00Z - cycle date
      ; mc = 1, cycle_offset_hours = start of month 00Z - cycle date
      ;         accum hours = accum hours -
      ;                       (start of month 00Z - cycle date0

      cycleOffsetHours = mc * ROUND((lastTimeStepMonthStart_Julian - $
                                     cycleDate_Julian) * 24.0D)

      if (numMonths eq 2) then begin
          if (mc eq 0) then begin
              thisAccumHours = $
                  ROUND((lastTimeStepMonthStart_Julian - $
                         cycleDate_Julian) * 24.0D)
          endif else begin
              thisAccumHours = $
                  AccumHours - $
                  ROUND((lastTimeStepMonthStart_Julian - $
                         cycleDate_Julian) * 24.0D)
          endelse
      endif else thisAccumHours = accumHours

;+
;     Get the unmodified GFS QPF. The GET_ACC_GFS_QPF procedure will
;     get or verify GFSGridInfo, so that structure does not need to be
;     verified here.
;-
      acc_GFS_QPF_grid = !NULL
      GET_ACC_GFS_QPF, CycleDate_YYYYMMDDHH, $
                       thisAccumHours, $
                       GFSDir, $
                       ScratchDir, $
                       NoDataValue, $
                       acc_GFS_QPF_grid, $
                       GFS_GRID_INFO = GFSGridInfo, $
                       CYCLE_OFFSET_HOURS = cycleOffsetHours

      if NOT(ISA(acc_GFS_QPF_grid)) then begin
          ERR_MSG, 'Failed to get ' + STRCRA(AccumHours) + '-hour QPF ' + $
                   'accumulation for ' + CycleDate_YYYYMMDDHH + $
                   ' GFS cycle ' + $
                   'with ' + STRCRA(cycleOffsetHours) + '-hour offset.'
          RETURN
      endif

      ;; WSET_OR_WINDOW, 3, XSIZE = 625, YSIZE = 324
      ;; LOADCT, 12
      ;; REAL_TVSCL, acc_GFS_QPF_grid[1932:2556, 938:1261], NDV = NoDataValue
      ;; move = GET_KBRD(1)

;+
;     Get GFS column and row indices of the NWM grid. These will
;     enable us to regrid GFS data to the NWM grid.
;-
      if (NOT(ISA(i_GFS_NWM)) or NOT(ISA(j_GFS_NWM))) then begin

          USR_MSG, 'Getting GFS grid indices of NWM grid cells.'

          if FILE_TEST('ij_GFS_13km_T1534_NWM.sav') then begin

              RESTORE, 'ij_GFS_13km_T1534_NWM.sav'
              if (NOT(ISA(i_GFS_NWM)) or NOT(ISA(j_GFS_NWM))) then STOP

          endif else begin

;+
;             The GFS longitude axis is uniform, so calulating
;             i_GFS_NWM is very simple.
;-
              GFSMinLonCtr = GFSGridInfo.minLon + 0.5D * GFSGridInfo.lonRes
              while (GFSMinLonCtr ge 360.0D) do $
                  GFSMinLonCtr = GFSMinLonCtr - 360.0D
              i_GFS_NWM = (lonGrid_NWM + 360.0D - GFSMinLonCtr) / $
                          GFSGridInfo.lonRes

;+
;             Interpolate between GFS latitudes to estimate j_GFS_NWM
;             values. The GFS latitude axis is not perfectly uniform,
;             though it is so close it hardly pays to do this. :P
;-
              j_GFS_NWM = MAKE_ARRAY(NWMGridInfo.nCols, $
                                     NWMGridInfo.nRows, $
                                     VALUE = -1.0D)
              PROGRESS_HASH_INIT, NWMGridInfo.nRows, hashes, lastProgress
              for j = 0L, NWMGridInfo.nRows - 1L do begin
                  PROGRESS_HASH_UPDATE, j, NWMGridInfo.nRows - 1L, $
                                        hashes, lastProgress
                  for i = 0L, NWMGridInfo.nCols - 1L do begin
                      latSample = latGrid_NWM[i, j]
                      j1 = MAX(WHERE(GFSGridInfo.rowCenterLat le latSample))
                      if (j1 eq -1L) then STOP
                      j2 = j1 + 1L
                      if (GFSGridInfo.rowCenterLat[j2] le latSample) then STOP
                      if (j2 ge GFSGridInfo.nRows) then STOP
                      j_GFS_NWM[i, j] = $
                          DOUBLE(j1) + $
                          (latSample - GFSGridInfo.rowCenterLat[j1]) / $
                          (GFSGridInfo.rowCenterLat[j2] - $
                           GFSGridInfo.rowCenterLat[j1])
                  endfor
              endfor
              SAVE, i_GFS_NWM, j_GFS_NWM, FILE = 'ij_GFS_13km_T1534_NWM.sav'
          endelse

      endif

;+
;     Regrid the GFS QPF to the NWM grid using bilinear interpolation.
;-
      acc_GFS_QPF_NWM_grid = REGRID_BILIN(acc_GFS_QPF_grid, $
                                          i_GFS_NWM, $
                                          j_GFS_NWM, $
                                          NoDataValue) ;, $
                                          ;/GENEROUS)

      ;; WSET_OR_WINDOW, 2, XSIZE = 4608 / 6, YSIZE = 3840 / 6
      ;; LOADCT, 12
      ;; REAL_TVSCL, REBIN(acc_GFS_QPF_NWM_grid, 4608 / 6, 3840 / 6, /SAMPLE), $
      ;;             NDV = NoDataValue
      ;; move = GET_KBRD(1)

;+
;     Perform Mountain Mapper adjustment.
;-
      ind = WHERE((acc_GFS_QPF_NWM_grid ne NoDataValue) and $
                  (PRISMNumerGrid ne NoDataValue) and $
                  (PRISMDenomGrid ne NoDataValue) and $
                  (PRISMDenomGrid gt 0.01), count)
      if (count gt 0) then $
          acc_GFS_QPF_NWM_grid[ind] = $
          acc_GFS_QPF_NWM_grid[ind] * $
          PRISMNumerGrid[ind] / PRISMDenomGrid[ind]

      if (mc eq 0) then begin
          tot_GFS_QPF_NWM_grid = acc_GFS_QPF_NWM_grid
      endif else begin
          ind = WHERE((tot_GFS_QPF_NWM_grid eq NoDataValue) or $
                      (acc_GFS_QPF_NWM_grid eq NoDataValue), count)
          tot_GFS_QPF_NWM_grid = tot_GFS_QPF_NWM_grid + $
                                    acc_GFS_QPF_NWM_grid
          if (count gt 0) then tot_GFS_QPF_NWM_grid[ind] = NoDataValue
      endelse

  endfor

  acc_GFS_QPF_NWMv20_grid = TEMPORARY(tot_GFS_QPF_NWM_grid)

  RETURN

end

;; cycleDate_YYYYMMDDHH = '2018053012'
;; accumHours = 120
;; GFSDir = '/nwcdev/archive/GFS_archive'
;; MMDir = '/net/lfs0data10/NWM_params/mountain_mapper_GFS_1km_from_T1534'
;; scratchDir = '/disks/scratch/fall'
;; ndv = -99999.0
;; GET_ACC_GFS_QPF_NWM_V2_0, cycleDate_YYYYMMDDHH, $
;;                           accumHours, $
;;                           GFSDir, $
;;                           MMDir, $
;;                           scratchDir, $
;;                           ndv, $
;;                           acc_GFS_QPF_NWMv20_grid, $
;;                           GFS_GRID_INFO = GFSGridInfo, $
;;                           NWM_GRID_PROJ_INFO = NWMGridInfo, $
;;                           LON_GRID_NWM = lonGrid_NWM, $
;;                           LAT_GRID_NWM = latGrid_NWM
;; ;+
;; ; Define colors for precipitation grids in mm.
;; ;-
;;   qRed = [255, 000, 000, 127, 238, 255, 255, 255, 238, 205, 139, $
;;           145, 137, 016, 030]
;;   qGrn = [228, 139, 205, 255, 238, 215, 165, 127, 064, 000, 000, $
;;           044, 104, 078, 144]
;;   qBlu = [220, 000, 000, 000, 000, 000, 079, 000, 000, 000, 000, $
;;           238, 205, 139, 255]
;;   qEdges = [0.0, 0.1, 2.0, 5.0, 10.0, 15.0, 20.0, 25.0, 35.0, 50.0, $
;;             75.0, 100.0, 125.0, 150.0, 175.0, 500.0]

;; ;+
;; ; Define polygons to draw over maps.
;; ;-
;;   vectorDir = '/nwcdev/nwc_forcings/GIS_data/Vector'

;;   shapePathList = [vectorDir + '/' + $
;;                    'National_Boundary_Canada', $
;;                    vectorDir + '/' + $
;;                    'National_Boundary_Mexico_2004', $
;;                    vectorDir + '/' + $
;;                    'states_no_water', $
;;                    vectorDir + '/' + $
;;                    'national_no_water']

;;   X_MAP_NWM_GRID, acc_GFS_QPF_NWMv20_grid, $
;;                   qEdges, qRed, qGrn, qBlu, $
;;                   0, $
;;                   status, $
;;                   /SHOW_HIGH, $
;;                   XSIZE_TARGET = 1000, $
;;                   SHAPE_PATH_LIST = shapePathList, $
;;                   TITLE = 'GFS ' + cycleDate_YYYYMMDDHH + ', ' + $
;;                   STRCRA(accumHours) + '-hour Precip', $
;;                   /COLORBAR, $
;;                   UNITS = 'Millimeters'
;; end
