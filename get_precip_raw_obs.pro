PRO GET_PRECIP_RAW_OBS, ObsDate_YYYYMMDDHH, $
                        DurationHours, $
                        MinLon, MaxLon, MinLat, MaxLat, $
                        Ndv, $
                        PGHost, WebPGHost, $
                        ScratchFilePath, $
                        SavFilePath, $
                        precipReport, $
                        OTHER_DURATIONS = otherDurToAgg, $
                        FORCE_WEB_DB = ForceWebDB


;durationhours = 6
;other_duration_to_aggregate = [1]

;+
; Get precip_raw observations for a given ObsDate_YYYYMMDDHH
; (+/- 5 minutes) from the NOHRSC database/s over a specified spatial
; domain.
;-

  COMMON info, message

  precipReport = !NULL

;+
; If the SavFilePath exists, try to get data from there.
;-
  if FILE_TEST(SavFilePath) then begin

      USR_MSG, string(10B) + 'Restoring observation data from ' + SavFilePath
      RESTORE, SavFilePath

      if NOT(ISA(precipReport)) then STOP
      if NOT(ISA(numPrecipReports)) then STOP
      if (ndv__ ne Ndv) then STOP
      if (obsDate_YYYYMMDDHH__ ne ObsDate_YYYYMMDDHH) then STOP
      IF (durationHours__ ne DurationHours) then STOP
      if (minLon__ ne MinLon) then STOP
      if (maxLon__ ne MaxLon) then STOP
      if (minLat__ ne MinLat) then STOP
      if (maxLat__ ne MaxLat) then STOP

      USR_MSG, 'Observations restored from ' + SavFilePath + string(10B)

      RETURN

  endif

  if FILE_TEST(ScratchFilePath) then $
      USR_MSG, 'WARNING: scratch file ' + ScratchFilePath + $
               ' will be overwritten.'

;+
; Check arguments for correct type and valid contents.
;-
  if NOT(ISA(ObsDate_YYYYMMDDHH, 'STRING')) then begin
      ERR_MSG, 'Observation date/time argument must be a STRING.'
      RETURN
  endif
  if (STRLEN(ObsDate_YYYYMMDDHH) ne 10) then begin
      ERR_MSG, 'Invalid observation date/time "' + $
               ObsDate_YYYYMMDDHH + $
               '" (required form is YYYYMMDDHH, 10 digits).'
     RETURN
  endif
  if NOT(STREGEX(ObsDate_YYYYMMDDHH, '[0-9]{10}', /BOOLEAN)) $
      then begin
      ERR_MSG, 'Invalid observation date/time "' + $
               ObsDate_YYYYMMDDHH + $
               '" (required form is YYYYMMDDHH, all numeric).'
      RETURN
  endif

  obsDate_Julian = YYYYMMDDHH_TO_JULIAN(ObsDate_YYYYMMDDHH)

  if NOT(ISA(MinLon, 'DOUBLE')) then $
      minLon_ = DOUBLE(MinLon) $
  else $
      minLon_ = MinLon

  if NOT(ISA(MaxLon, 'DOUBLE')) then $
      maxLon_ = DOUBLE(MaxLon) $
  else $
      maxLon_ = MaxLon

  if NOT(ISA(MinLat, 'DOUBLE')) then $
      minLat_ = DOUBLE(MinLat) $
  else $
      minLat_ = MinLat

  if NOT(ISA(MaxLat, 'DOUBLE')) then $
      maxLat_ = DOUBLE(MaxLat) $
  else $
      maxLat_ = MaxLat

  if NOT(ISA(Ndv, 'FLOAT')) then $
      ERR_MSG, 'WARNING: no-data value should be a floating point value.'

  if NOT(ISA(PGHost, 'STRING')) then begin
      ERR_MSG, 'PostgreSQL "PGHOST" argument must be a STRING.'
      RETURN
  endif

  if NOT(ISA(WebPGHost, 'STRING')) then begin
      ERR_MSG, 'PostgreSQL "WEBPGHOST" argument must be a STRING.'
      RETURN
  endif

;+
; Make sure the PGHOSTADDR environment variable is not set.
;-
  PGHostAddr = GETENV('PGHOSTADDR')
  if (PGHostAddr ne '') then begin
      ERR_MSG, 'Environment variable PGHOSTADDR must not be set.'
      RETURN
  endif

;+
; Generate strings for SQL statements defining the analysis domain.
;-
  minLonStr = STRCRA(STRING(minLon_, FORMAT='(F25.17)'))
  maxLonStr = STRCRA(STRING(maxLon_, FORMAT='(F25.17)'))
  minLatStr = STRCRA(STRING(minLat_, FORMAT='(F25.17)'))
  maxLatStr = STRCRA(STRING(maxLat_, FORMAT='(F25.17)'))

;+
; Choose a server: operational database or web database.
;-
  date_Julian = SYSTIME(/JULIAN, /UTC)

  if ((date_Julian - obsDate_Julian) gt 28.0D) then $
    database = 'web' $
  else $
    database = 'ops'
  if KEYWORD_SET(ForceWebDB) then database = 'web'

;+
; Get hourly precipitation observations.
;-
  obsDate_long = JULIAN_TO_YYYYMMDDHHMMSS(obsDate_Julian)

  startDate_Julian = obsDate_Julian - 5.0D / 1440.0D
  startDate_long = JULIAN_TO_YYYYMMDDHHMMSS(startDate_Julian)

  finishDate_Julian = obsDate_Julian + 5.0D / 1440.0D
  finishDate_long = JULIAN_TO_YYYYMMDDHHMMSS(finishDate_Julian)

  obsDate = STRMID(obsDate_long, 0, 4) + '-' + $
            STRMID(obsDate_long, 4, 2) + '-' + $
            STRMID(obsDate_long, 6, 2) + ' ' + $
            STRMID(obsDate_long, 8, 2) + ':' + $
            STRMID(obsDate_long, 10, 2) + ':' + $
            '00'

  startDate = STRMID(startDate_long, 0, 4) + '-' + $
              STRMID(startDate_long, 4, 2) + '-' + $
              STRMID(startDate_long, 6, 2) + ' ' + $
              STRMID(startDate_long, 8, 2) + ':' + $
              STRMID(startDate_long, 10, 2) + ':' + $
              '00'

  finishDate = STRMID(finishDate_long, 0, 4) + '-' + $
               STRMID(finishDate_long, 4, 2) + '-' + $
               STRMID(finishDate_long, 6, 2) + ' ' + $
               STRMID(finishDate_long, 8, 2) + ':' + $
               STRMID(finishDate_long, 10, 2) + ':' + $
               '00'

;; Anders:
;;
;; select *
;; from
;; (
;; select *, 
;; first_value( diff ) over ( partition by station_id, nearest order by diff) as min_diff
;; from
;; (
;; select *, 
;;        case when date <= nearest then nearest - date
;;        else date - nearest + '00:00:01'::interval end as diff
;; from
;; (
;; select *, 
;;        date_trunc( 'hour', date + '00:30:00'::interval ) as nearest
;; from precip_raw
;; where duration = 1 and
;;       date >= '2016-11-09 23:30:00' and
;;       date <  '2016-11-10 06:30:00'
;; ) as t1
;; ) as t2
;; ) as t3
;; where diff = min_diff;

;; ever so slightly faster:

;; select *
;; from
;; (
;; select *, 
;; min ( diff ) over ( partition by station_id, nearest) as min_diff
;; from
;; (
;; select *, 
;;        case when date <= nearest then nearest - date
;;        else date - nearest + '00:00:01'::interval end as diff
;; from
;; (
;; select *, 
;;        date_trunc( 'hour', date + '00:30:00'::interval ) as nearest
;; from precip_raw
;; where duration = 1 and
;;       date >= '2016-11-09 23:30:00' and
;;       date <  '2016-11-10 06:30:00'
;; ) as t1
;; ) as t2
;; ) as t3
;; where diff = min_diff;"

  durationSeconds = DurationHours * 3600L

  if (database eq 'web') then begin

      statement = 'psql -d web_data -h ' + WebPGHost + ' -t -A -c ' + $
                  '"' + $
;                  'set enable_nestloop = off; ' + $
                  'select ' + $
                  't1.obj_identifier, ' + $
                  'trim(t1.station_id), ' + $
                  'trim(t1.name), ' + $
                  'trim(t1.station_type), ' + $
                  't1.coordinates[0], ' + $
                  't1.coordinates[1], ' + $
                  't1.elevation, ' + $
                  't2.date, ' + $
                  't2.value ' + $
                  'from point.allstation as t1, ' + $
                  'point.obs_precip_raw as t2 ' + $
                  'where ' + $
                  ;; 't1.coordinates[0] >= ' + minLonStr + ' ' + $
                  ;; 'and t1.coordinates[0] <= ' + maxLonStr + ' ' + $
                  ;; 'and t1.coordinates[1] >= ' + minLatStr + ' ' + $
                  ;; 'and t1.coordinates[1] <= ' + maxLatStr + ' ' + $
                  ;; 'and t2.date >= ''' + startDate + ''' ' + $
;                  't2.date >= ''' + startDate + ''' ' + $
;                  'and t2.date <= ''' + finishDate + ''' ' + $
                  't2.date = ''' + obsDate + ''' ' + $
;                  'and (date_part(''minute'',date) >= 55 or ' + $
;                  'date_part(''minute'',date) <= 5) ' + $
                  'and t2.duration = ' + STRCRA(durationSeconds) + ' ' + $
                  'and t2.value >= 0.0 ' + $
                  'and t2.value < 99.5 ' + $
                  'and t2.value is not NULL ' + $
                  'and t1.obj_identifier = t2.obj_identifier ' + $
                  'order by t1.station_id, t2.date;" > ' + $
                  ScratchFilePath

  endif else begin

      statement = 'psql -d operations -h ' + PGHost + ' -t -A -c ' + $
                  '"' + $
                  'set search_path = nohrsc; ' + $
                  'select ' + $
                  'station.obj_identifier, ' + $
                  'trim(station.station_id), ' + $
                  'trim(station.name), ' + $
                  'trim(station.station_type), ' + $
                  'station.longitude, ' + $
                  'station.latitude, ' + $
                  'station.elevation, ' + $
                  'precip_raw.date, ' + $
                  'precip_raw.value ' + $
                  'from station, precip_raw ' + $
                  'where station.longitude >= ' + minLonStr + ' ' + $
                  'and station.longitude <= ' + maxLonStr + ' ' + $
                  'and station.latitude >= ' + minLatStr + ' ' + $
                  'and station.latitude <= ' + maxLatStr + ' ' + $
                  'and precip_raw.date >= ''' + $
                  startDate + ''' ' + $
                  'and precip_raw.date <= ''' + $
                  finishDate + ''' ' + $
;                  'and (date_part(''minute'',date) >= 55 or ' + $
;                       'date_part(''minute'',date) <= 5) ' + $
                  'and precip_raw.duration = ' + $
                  STRCRA(DurationHours) + ' ' + $
                  'and precip_raw.qc = 1 ' + $
                  'and precip_raw.value >= 0.0 ' + $
                  'and precip_raw.value < 99.5 ' + $
                  'and station.station_type != ''Stranger''' + $
                  'and station.station_id = precip_raw.station_id ' + $
                  'order by station.station_id, precip_raw.date;" > ' + $
                  ScratchFilePath

  endelse

  USR_MSG, statement
  SPAWN, statement, EXIT_STATUS = status

  if (status ne 0) then begin
      ERR_MSG, 'psql statement failed: ' + statement
      RETURN
  endif

  if NOT(FILE_TEST(ScratchFilePath)) then begin
      ERR_MSG, 'Output file ' + ScratchFilePath + ' not found.'
      RETURN
  endif

;+
; Count the number of reports.
;-
  cmd = 'cat ' + ScratchFilePath + ' | wc -l'

  SPAWN, cmd, numPrecipReports, EXIT_STATUS = status
  if (status ne 0) then begin
      ERR_MSG, 'Command "' + cmd + '" failed.'
      RETURN
  endif

  if (N_ELEMENTS(numPrecipReports) ne 1) then begin
      ERR_MSG, 'Command "' + cmd + '" output invalid.'
      STOP
  endif

  numPrecipReports = LONG(numPrecipReports[0])

  if (numPrecipReports eq 0) then begin
      ERR_MSG, 'No reports found.'
      RETURN
  endif

;  USR_MSG, string(10B) + '--> Found ' + STRCRA(numPrecipReports) + $
;           ' precip reports.'
  USR_MSG, 'Found ' + STRCRA(numPrecipReports) + ' precip_raw reports.'

;+
; Place results in a structure.
;-
  precipReport_ = REPLICATE({obj_id: 0L, $
                             station_id: '', $
                             station_name: '', $
                             station_type: '', $
                             longitude: 0.0d, $
                             latitude: 0.0d, $
                             elevation: 0L, $
                             date: '', $
                             value_meters: Ndv}, $
                            numPrecipReports)

  OPENR, lun, ScratchFilePath, /GET_LUN
  line = ''

  for rc = 0L, numPrecipReports - 1L do begin

      READF, lun, line
      report = STRSPLIT(line, '|', /EXTRACT)

      if (N_ELEMENTS(report) ne 9) then begin
          ERR_MSG, 'Incomplete report "' + report + '".'
          STOP
      endif

      precipReport_[rc].obj_id = LONG(report[0])
      precipReport_[rc].station_id = report[1]
      precipReport_[rc].station_name = report[2]
      precipReport_[rc].station_type = report[3]
      precipReport_[rc].longitude = DOUBLE(report[4])
      precipReport_[rc].latitude = DOUBLE(report[5])
      precipReport_[rc].elevation = LONG(report[6])
      precipReport_[rc].date = report[7]
      precipReport_[rc].value_meters = FLOAT(report[8])

  endfor

  if NOT(EOF(lun)) then begin
      ERR_MSG, 'WARNING: read all reports but have not reached the ' + $
               'end of the file.'
      STOP
  endif

  FREE_LUN, lun
  FILE_DELETE, ScratchFilePath

;+
; If the web database was used, eliminate stations outside the
; specified domain.
;-
  if (database eq 'web') then begin

      ind = WHERE((precipReport_.longitude ge minLon_) and $
                  (precipReport_.longitude le maxLon_) and $
                  (precipReport_.latitude ge minLat_) and $
                  (precipReport_.latitude lt maxLat_), count)

      if (count eq 0) then begin
          ERR_MSG, 'No reports found in specified domain.'
          RETURN
      endif

      precipReport_ = precipReport_[ind]
      numPrecipReports = count
      USR_MSG, 'Specified domain includes ' + STRCRA(numPrecipReports) + $
               ' reports.'

  endif

;+
; If any stations delivered multiple reports, choose the one closest
; to the analysis time (i.e., the top of the hour).
;-
  useFlag = BYTARR(numPrecipReports) & useFlag[*] = 1B

  for rc = 0L, numPrecipReports - 1L do begin
      ind = WHERE((precipReport_.obj_id eq $
                   precipReport_[rc].obj_id) and $
                  (useFlag eq 1B), count)
      if (count eq 0) then STOP ; PROGRAMMING ERROR
      if (count eq 1) then CONTINUE
      useFlag[ind] = 0B ; set all matches including the current to DO NOT USE
      dateList = precipReport_[ind].date
      dateList_Julian = $
          JULDAY(FIX(STRMID(dateList, 5, 2)), $      ; month
                 FIX(STRMID(dateList, 8, 2)), $      ; day
                 FIX(STRMID(dateList, 0, 4)), $      ; year
                 FIX(STRMID(dateList, 11, 2)), $     ; hour
                 FIX(STRMID(dateLIst, 14, 2)), $     ; minute
                 FIX(STRMID(dateList, 17, 2)))       ; second
      ;; dateList_Julian = DBLARR(count)
      ;; for ic = 0, count - 1 do begin
      ;;     dateList_Julian[ic] = $
      ;;         JULDAY(FIX(STRMID(dateList[ic], 5, 2)), $  ; month
      ;;                FIX(STRMID(dateList[ic], 8, 2)), $  ; day
      ;;                FIX(STRMID(dateList[ic], 0, 4)), $  ; year
      ;;                FIX(STRMID(dateList[ic], 11, 2)), $ ; hour
      ;;                FIX(STRMID(dateLIst[ic], 14, 2)), $ ; minute
      ;;                FIX(STRMID(dateList[ic], 17, 2)))   ; second
      ;; endfor
      timeDiff = MIN(ABS(obsDate_Julian - dateList_Julian), minInd)
      useFlag[ind[minInd]] = 1B
  endfor

  ind = WHERE(useFlag eq 1B, count)
  if (count eq 0) then STOP ; PROGRAMMING ERROR

  precipReport_ = precipReport_[ind]
  if (count ne numPrecipReports) then $
      USR_MSG, 'After removing duplicates, ' + STRCRA(count) + $
               ' reports remain.'
  numPrecipReports = count

;+
; Rename the output structure to indicate this procedure has succeeded.
;-
  precipReport = TEMPORARY(precipReport_)

;+
; If the SavFilePath does not exist, store data there. Only do this if
; the web database was used. Station object identifiers will be
; inconsistent if both databases are allowed to produce save files.
;-
  if (NOT(FILE_TEST(SavFilePath)) and (database eq 'web')) then begin

      ndv__ = Ndv
      obsDate_YYYYMMDDHH__ = ObsDate_YYYYMMDDHH
      durationHours__ = DurationHours
      minLon__ = MinLon
      maxLon__ = MaxLon
      minLat__ = MinLat
      maxLat__ = MaxLat

      SAVE, precipReport, numPrecipReports, $
            ndv__, $
            obsDate_YYYYMMDDHH__, durationHours__, $
            minLon__, maxLon__, minLat__, maxLat__, $
            FILE = SavFilePath

  endif

  RETURN

end
