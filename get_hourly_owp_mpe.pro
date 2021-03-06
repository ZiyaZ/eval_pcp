PRO GET_HOURLY_OWP_MPE, $
    AccEndDate_YYYYMMDDHH, $
    FileTemplate, $ ; file convention, using "YYYYMMDDHH" for the date
    MPEDir, $
    ScratchDir, $
    NoDataValue, $
    hourlyMPEGrid, $
    HRAP_GRID_PROJ_INFO = HRAPGridProjInfo

;+
; Get hourly OWP Stage IV (MPE) precipitation produced by the
; IDL program refine_mpe.pro.
;-

;+
; Initialize output grid to !NULL.
;-
  hourlyMPEGrid = !NULL

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


  if NOT(ISA(FileTemplate, 'STRING')) then begin
      ERR_MSG, 'Template for OWP MPE file names must be a STRING.'
      RETURN
  endif

  templateDatePos = STRPOS(FileTemplate, 'YYYYMMDDHH')
  if (templateDatePos eq -1) then begin
      ERR_MSG, 'Template for OWP MPE file names must include ' + $
               '"YYYYMMDDHH".'
      RETURN
  endif

  if NOT(ISA(MPEDir, 'STRING')) then begin
      ERR_MSG, 'Location of Stage IV archive must be a STRING.'
      RETURN
  endif
  if NOT(FILE_TEST(MPEDir, /DIRECTORY)) then begin
      ERR_MSG, 'Stage IV archive directory "' + MPEDir + '" not found.'
      RETURN
  endif
  if NOT(FILE_TEST(MPEDir, /READ)) then begin
      ERR_MSG, 'Stage IV archive directory "' + MPEDir + '" not readable.'
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

  if KEYWORD_SET(HRAPGridProjInfo) then begin

;+
;     Verify the structure of HRAPGridProjInfo.
;
;     HRAPGridProjInfo = $
;       {lonV:  lonV, $           ; orientation longitude
;        latD:  latD, $           ; lat where dx and dy are specified
;        eRadM: 6371200.0D, $     ; NCEP sphere radius, meters
;        lat00: lat00, $          ; deprojected lat of LL cell center
;        lon00: lon00, $          ; deprojected lon of LL cell center
;        nCols: nx, $             ; # columns
;        nRows: ny, $             ; # rows
;        dx:    dx, $             ; x resolution at latD, meters
;        dy:    dy}               ; y resolution at latD, meters
;-
      foo = SIZE(HRAPGridProjInfo)
      if (foo[0] ne 1) then begin
          ERR_MSG, 'HRAP_GRID_PROJ_INFO structure mismatch (non-scalar).'
          RETURN
      endif

      if ((foo[1] ne 1) or (foo[2] ne 8)) then begin
          ERR_MSG, 'HRAP_GRID_PROJ_INFO structure mismatch ' + $
                   '(not a structure).'
          RETURN
      endif

      structOk = 1

      tagNames = TAG_NAMES(HRAPGridProjInfo)

      ind = WHERE(tagNames eq 'LONV', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "lonV" tag in HRAP_GRID_PROJ_INFO.'
          structOk = 0
      endif
      ind = WHERE(tagNames eq 'LATD', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "latD" tag in HRAP_GRID_PROJ_INFO.'
          structOk = 0
      endif
      ind = WHERE(tagNames eq 'ERADM', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "eRadM" tag in HRAP_GRID_PROJ_INFO.'
          structOk = 0
      endif
      ind = WHERE(tagNames eq 'LAT00', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "lat00" tag in HRAP_GRID_PROJ_INFO.'
          structOk = 0
      endif
      ind = WHERE(tagNames eq 'LON00', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "lon00" tag in HRAP_GRID_PROJ_INFO.'
          structOk = 0
      endif
      ind = WHERE(tagNames eq 'NCOLS', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "nCols" tag in HRAP_GRID_PROJ_INFO.'
          structOk = 0
      endif
      ind = WHERE(tagNames eq 'NROWS', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "nRows" tag in HRAP_GRID_PROJ_INFO.'
          structOk = 0
      endif
      ind = WHERE(tagNames eq 'DX', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "dx" tag in HRAP_GRID_PROJ_INFO.'
          structOk = 0
      endif
      ind = WHERE(tagNames eq 'DY', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "dy" tag in HRAP_GRID_PROJ_INFO.'
          structOk = 0
      endif

      if structOk then begin

          if NOT(ISA(HRAPGridProjInfo.lonV, 'DOUBLE')) then begin
              ERR_MSG, 'HRAP_GRID_PROJ_INFO element "lonV" is not ' + $
                       'type DOUBLE.'
              structOk = 0
          endif
          if NOT(ISA(HRAPGridProjInfo.latD, 'DOUBLE')) then begin
              ERR_MSG, 'HRAP_GRID_PROJ_INFO element "latD" is not ' + $
                       'type DOUBLE.'
              structOk = 0
          endif
          if NOT(ISA(HRAPGridProjInfo.eRadM, 'DOUBLE')) then begin
              ERR_MSG, 'HRAP_GRID_PROJ_INFO element "eRadM" is not ' + $
                       'type DOUBLE.'
              structOk = 0
          endif
          if NOT(ISA(HRAPGridProjInfo.lat00, 'DOUBLE')) then begin
              ERR_MSG, 'HRAP_GRID_PROJ_INFO element "lat00" is not ' + $
                       'type DOUBLE.'
              structOk = 0
          endif
          if NOT(ISA(HRAPGridProjInfo.lon00, 'DOUBLE')) then begin
              ERR_MSG, 'HRAP_GRID_PROJ_INFO element "lon00" is not ' + $
                       'type DOUBLE.'
              structOk = 0
          endif
          if NOT(ISA(HRAPGridProjInfo.nCols, 'LONG')) then begin
              ERR_MSG, 'HRAP_GRID_PROJ_INFO element "nCols" is not ' + $
                       'type LONG.'
              structOk = 0
          endif
          if NOT(ISA(HRAPGridProjInfo.nRows, 'LONG')) then begin
              ERR_MSG, 'HRAP_GRID_PROJ_INFO element "nRows" is not ' + $
                       'type LONG.'
              structOk = 0
          endif
          if NOT(ISA(HRAPGridProjInfo.dx, 'DOUBLE')) then begin
              ERR_MSG, 'HRAP_GRID_PROJ_INFO element "dx" is not ' + $
                       'type DOUBLE.'
              structOk = 0
          endif
          if NOT(ISA(HRAPGridProjInfo.dy, 'DOUBLE')) then begin
              ERR_MSG, 'HRAP_GRID_PROJ_INFO element "dy" is not ' + $
                       'type DOUBLE.'
              structOk = 0
          endif

      endif

      if NOT(structOk) then begin
          ERR_MSG, 'Unexpected structure/content in HRAP_GRID_PROJ_INFO ' + $
                   'structure.'
          RETURN
      endif

  endif

;+
; Get hourly OWP data.
;-

    date_Julian = AccEndDate_Julian
    date_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(date_Julian)

    date_YYYYMMDD = STRMID(date_YYYYMMDDHH, 0, 8)

    dir = MPEDir + '/' + date_YYYYMMDD
    if NOT(FILE_TEST(dir, /DIRECTORY)) then begin
        ERR_MSG, 'Directory "' + dir + '" not found.'
        RETURN
    endif

    file = FileTemplate
    STR_REPLACE, file, 'YYYYMMDDHH', date_YYYYMMDDHH

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
;     Locate and read the "Data" variable and its no-data value.
;-
    varID_data = NCDF_VARID(id, 'Data')
    if (varID_data eq -1) then begin
        ERR_MSG, 'Missing "Data" variable in ' + filePath + '.'
        RETURN
    endif

    NCDF_ATTGET, id, varID_data, '_FillValue', ndv

    NCDF_VARGET, id, 'Data', hourlyMPEGrid
    foo = SIZE(hourlyMPEGrid)
    nCols_ = foo[1]
    nRows_ = foo[2]

    if NOT(ISA(HRAPGridProjInfo)) then begin

        NCDF_ATTGET, id, varID_data, 'grid_mapping', gridMappingName
        varID_gm = NCDF_VARID(id, gridMappingName)
        NCDF_ATTGET, id, varID_gm, $
                     'straight_vertical_longitude_from_pole', $
                     lonV
        NCDF_ATTGET, id, varID_gm, $
                     'standard_parallel', $
                     latD
        NCDF_ATTGET, id, varID_gm, $
                     'semi_major_axis', $
                     eRadM
        NCDF_ATTGET, id, varID_gm, $
                     'inverse_flattening', $
                     invFlat
        if (invFlat ne 0.0D) then STOP
        NCDF_ATTGET, id, varID_gm, $
                     'lower_left_cell_center_latitude', $
                     lat00
        NCDF_ATTGET, id, varID_gm, $
                     'lower_left_cell_center_longitude', $
                     lon00
        NCDF_ATTGET, id, varID_gm, $
                     'resolution_at_standard_parallel', $
                     dx
        dy = dx
        nCols = nCols_
        nRows = nRows_

        HRAPGridProjInfo = $
            {lonV:  lonV, $   ; orientation longitude
             latD:  latD, $   ; lat where dx and dy are specified
             eRadM: eRadM, $  ; NCEP sphere radius, meters
             lat00: lat00, $  ; deprojected lat of LL cell center
             lon00: lon00, $  ; deprojected lon of LL cell center
             nCols: nCols, $  ; # columns
             nRows: nRows, $  ; # rows
             dx:    dx, $     ; x resolution at latD, meters
             dy:    dy}       ; y resolution at latD, meters

    endif

    if (nCols_ ne HRAPGridProjInfo.nCols) then STOP
    if (nRows_ ne HRAPGridProjInfo.nRows) then STOP

    NCDF_CLOSE, id
    ind = WHERE(hourlyMPEGrid eq ndv, count)
    if (count gt 0) then hourlyMPEGrid[ind] = NoDataValue


  RETURN


end
