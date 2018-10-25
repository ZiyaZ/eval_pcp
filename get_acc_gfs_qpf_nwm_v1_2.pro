;+
; Read and aggregate archived GFS precipitation data from a single GFS
; cycle, then perform bilinear regridding to the National Water Model
; (NWM) grid, which replicates the precipitation downscaling process
; for NWM medium range forcing engine versions up to 1.2.
;
; Greg Fall, OWP-Chanhassen
; 2018-05-31
;-

PRO GET_ACC_GFS_QPF_NWM_V1_2, CycleDate_YYYYMMDDHH, $
                              AccumHours, $
                              GFSDir, $
                              ScratchDir, $
                              NoDataValue, $
                              acc_GFS_QPF_NWMv12_grid, $
                              GFS_GRID_INFO = GFSGridInfo, $
                              NWM_GRID_PROJ_INFO = NWMGridInfo, $
                              LON_GRID_NWM = lonGrid_NWM, $
                              LAT_GRID_NWM = latGrid_NWM, $
                              I_GFS_NWM = i_GFS_NWM, $
                              J_GFS_NWM = j_GFS_NWM

  acc_GFS_QPF_NWMv12_grid = !NULL

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
; Get the unmodified GFS QPF. The GET_ACC_GFS_QPF procedure will get
; or verify GFSGridInfo, so that structure does not need to be
; verified here.
;-
  GET_ACC_GFS_QPF, CycleDate_YYYYMMDDHH, $
                   AccumHours, $
                   GFSDir, $
                   ScratchDir, $
                   NoDataValue, $
                   acc_GFS_QPF_grid, $
                   GFS_GRID_INFO = GFSGridInfo

  if NOT(ISA(acc_GFS_QPF_grid)) then begin
      ERR_MSG, 'Failed to get ' + STRCRA(AccumHours) + '-hour QPF ' + $
               'accumulation for ' + CycleDate_YYYYMMDDHH + ' GFS cycle.'
      RETURN
  endif

;+
; Get GFS column and row indices of the NWM grid. These will
; enable us to regrid GFS data to the NWM grid.
;-
  if (NOT(ISA(i_GFS_NWM)) or NOT(ISA(j_GFS_NWM))) then begin

      USR_MSG, 'Getting GFS grid indices of NWM grid cells.'

      if FILE_TEST('ij_GFS_13km_T1534_NWM.sav') then begin

          RESTORE, 'ij_GFS_13km_T1534_NWM.sav'
          if (NOT(ISA(i_GFS_NWM)) or NOT(ISA(j_GFS_NWM))) then STOP

      endif else begin

;+
;         The GFS longitude axis is uniform, so calulating
;         i_GFS_NWM is very simple.
;-
          GFSMinLonCtr = GFSGridInfo.minLon + 0.5D * GFSGridInfo.lonRes
          while (GFSMinLonCtr ge 360.0D) do $
              GFSMinLonCtr = GFSMinLonCtr - 360.0D
          i_GFS_NWM = (lonGrid_NWM + 360.0D - GFSMinLonCtr) / $
                      GFSGridInfo.lonRes

;+
;         Interpolate between GFS latitudes to estimate j_GFS_NWM
;         values. The GFS latitude axis is not perfectly uniform,
;         though it is so close it hardly pays to do this. :P
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
; Regrid the GFS QPF to the NWM grid using bilinear interpolation,
; which faithfully mimics the ESMF regridding process used in
; National Water Model versions up to 1.2.
;-
  acc_GFS_QPF_NWMv12_grid = REGRID_BILIN(acc_GFS_QPF_grid, $
                                         i_GFS_NWM, $
                                         j_GFS_NWM, $
                                         NoDataValue);, $
                                         ;/GENEROUS)

  RETURN

end

;; cycleDate_YYYYMMDDHH = '2018053012'
;; accumHours = 120
;; GFSDir = '/nwcdev/archive/GFS_archive'
;; MMDir = '/net/lfs0data10/NWM_params/mountain_mapper_GFS_1km_from_T1534'
;; scratchDir = '/disks/scratch/fall'
;; ndv = -99999.0
;; GET_ACC_GFS_QPF_NWM_V1_2, cycleDate_YYYYMMDDHH, $
;;                           accumHours, $
;;                           GFSDir, $
;;                           scratchDir, $
;;                           ndv, $
;;                           acc_GFS_QPF_NWMv12_grid, $
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

;;   X_MAP_NWM_GRID, acc_GFS_QPF_NWMv12_grid, $
;;                   qEdges, qRed, qGrn, qBlu, $
;;                   1, $
;;                   status, $
;;                   /SHOW_HIGH, $
;;                   XSIZE_TARGET = 1000, $
;;                   SHAPE_PATH_LIST = shapePathList, $
;;                   TITLE = 'GFS ' + cycleDate_YYYYMMDDHH + ', ' + $
;;                   STRCRA(accumHours) + '-hour Precip', $
;;                   /COLORBAR, $
;;                   UNITS = 'Millimeters'
;; end
