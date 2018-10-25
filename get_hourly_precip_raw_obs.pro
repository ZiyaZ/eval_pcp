PRO GET_HOURLY_PRECIP_RAW_OBS, startDate_YYYYMMDDHH, $
                               finishDate_YYYYMMDDHH, $
                               minLon_, maxLon_, minLat_, maxLat_, $
                               ndv, $
                               PGHost, webPGHost, $
                               scratchFilePath, $
                               savFilePath, $
                               precipReport

  COMMON info, message

  precipReport = !NULL

  if FILE_TEST(savFilePath) then begin

      USR_MSG, string(10B) +'Restoring observation data from ' + savFilePath
      RESTORE, savFilePath

      if NOT(ISA(precipReport)) then STOP
      if (ndv__ ne ndv) then STOP
      if (startDate_YYYYMMDDHH__ ne startDate_YYYYMMDDHH) then STOP
      if (finishDate_YYYYMMDDHH__ ne finishDate_YYYYMMDDHH) then STOP
      if (minLon__ ne minLon_) then STOP
      if (maxLon__ ne maxLon_) then STOP
      if (minLat__ ne minLat_) then STOP
      if (maxLat__ ne maxLat_) then STOP

      USR_MSG, 'Observations restored from ' + savFilePath + string(10B)

      RETURN

  endif

  if FILE_TEST(scratchFilePath) then $
      USR_MSG, 'WARNING: scratch file ' + scratchFilePath + $
               ' will be overwritten.'


;; Check arguments for correct type and valid contents.
;
;  if NOT(ISA(startDate_YYYYMMDDHH, 'STRING')) then begin
;      ERR_MSG, 'Start date/time argument must be a STRING.'
;      RETURN
;  endif
;  if (STRLEN(startDate_YYYYMMDDHH) ne 10) then begin
;      ERR_MSG, 'Invalid start date/time "' + $
;               startDate_YYYYMMDDHH + $
;               '" (required form is YYYYMMDDHH, 10 digits).'
;      RETURN
;  endif
;  if NOT(STREGEX(startDate_YYYYMMDDHH, '[0-9]{10}', /BOOLEAN)) $
;      then begin
;      ERR_MSG, 'Invalid start date/time "' + $
;               startDate_YYYYMMDDHH + $
;               '" (required form is YYYYMMDDHH, all numeric).'
;      RETURN
;  endif
;
;  if NOT(ISA(finishDate_YYYYMMDDHH, 'STRING')) then begin
;      ERR_MSG, 'Finish date/time argument must be a STRING.'
;      RETURN
;  endif
;  if (STRLEN(finishDate_YYYYMMDDHH) ne 10) then begin
;      ERR_MSG, 'Invalid finish date/time "' + $
;               finishDate_YYYYMMDDHH + $
;               '" (required form is YYYYMMDDHH, 10 digits).'
;      RETURN
;  endif
;  if NOT(STREGEX(finishDate_YYYYMMDDHH, '[0-9]{10}', /BOOLEAN)) $
;      then begin
;      ERR_MSG, 'Invalid finish date/time "' + $
;               finishDate_YYYYMMDDHH + $
;               '" (required form is YYYYMMDDHH, all numeric).'
;      RETURN
;  endif
;
  startDate_Julian = YYYYMMDDHH_TO_JULIAN(startDate_YYYYMMDDHH)
  finishDate_Julian = YYYYMMDDHH_TO_JULIAN(finishDate_YYYYMMDDHH)

;  if (startDate_Julian gt finishDate_Julian) then begin
;      ERR_MSG, 'Start date/time may not be later than ' + $
;               'finish date/time.'
;      RETURN
;  endif
;
  if NOT(ISA(minLon_, 'DOUBLE')) then $
      minLon = DOUBLE(minLon_) $
  else $
      minLon = minLon_

  if NOT(ISA(maxLon_, 'DOUBLE')) then $
      maxLon = DOUBLE(maxLon_) $
  else $
      maxLon = maxLon_

  if NOT(ISA(minLat_, 'DOUBLE')) then $
      minLat = DOUBLE(minLat_) $
  else $
      minLat = minLat_

  if NOT(ISA(maxLat_, 'DOUBLE')) then $
      maxLat = DOUBLE(maxLat_) $
  else $
      maxLat = maxLat_
;
;  if NOT(ISA(ndv, 'FLOAT')) then $
;      ERR_MSG, 'WARNING: no-data value should be a floating point value.'
;
;  if NOT(ISA(PGHost, 'STRING')) then begin
;      ERR_MSG, 'PostgreSQL "PGHOST" argument must be a STRING.'
;      RETURN
;  endif
;
;  if NOT(ISA(webPGHost, 'STRING')) then begin
;      ERR_MSG, 'PostgreSQL "WEBPGHOST" argument must be a STRING.'
;      RETURN
;  endif
;
;
;; Make sure the PGHOSTADDR environment variable is not set.
;
;  PGHostAddr = GETENV('PGHOSTADDR')
;  if (PGHostAddr ne '') then begin
;      ERR_MSG, 'Environment variable PGHOSTADDR must not be set.'
;      RETURN
;  endif


; Generate strings for SQL statements defining the analysis domain.

  minLonStr = STRCRA(STRING(minLon, FORMAT='(F25.17)'))
  maxLonStr = STRCRA(STRING(maxLon, FORMAT='(F25.17)'))
  minLatStr = STRCRA(STRING(minLat, FORMAT='(F25.17)'))
  maxLatStr = STRCRA(STRING(maxLat, FORMAT='(F25.17)'))


; Choose a server: operational database or web database.

  date_Julian = SYSTIME(/JULIAN, /UTC)

  if ((date_Julian - startDate_Julian) gt 28.0D) then $
    database = 'web' $
  else $
    database = 'ops'


; Get hourly precipitation observations.

  startDate = STRMID(startDate_YYYYMMDDHH, 0, 4) + '-' + $
              STRMID(startDate_YYYYMMDDHH, 4, 2) + '-' + $
              STRMID(startDate_YYYYMMDDHH, 6, 2) + ' ' + $
              STRMID(startDate_YYYYMMDDHH, 8, 2) + ':' + $
              '00:00'

  finishDate = STRMID(finishDate_YYYYMMDDHH, 0, 4) + '-' + $
               STRMID(finishDate_YYYYMMDDHH, 4, 2) + '-' + $
               STRMID(finishDate_YYYYMMDDHH, 6, 2) + ' ' + $
               STRMID(finishDate_YYYYMMDDHH, 8, 2) + ':' + $
               '00:00'




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


  if (database eq 'web') then begin

      statement = 'psql -d web_data -h ' + webPGHost + ' -t -A -c ' + $
                  '"' + $
                  'select ' + $
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
                  't1.coordinates[0] >= ' + minLonStr + ' ' + $
                  'and t1.coordinates[0] <= ' + maxLonStr + ' ' + $
                  'and t1.coordinates[1] >= ' + minLatStr + ' ' + $
                  'and t1.coordinates[1] <= ' + maxLatStr + ' ' + $
                  'and t2.date >= ''' + $
                  startDate + ''' ' + $
                  'and t2.date <= ''' + $
                  finishDate + ''' ' + $
                  'and (date_part(''minute'',date) >= 55 or ' + $
                  'date_part(''minute'',date) <= 5) ' + $
                  'and t2.duration = 3600 ' + $
                  'and t2.value >= 0.0 ' + $
                  'and t2.value < 99.5 ' + $
                  'and t2.value is not NULL ' + $
                  'and t1.obj_identifier = t2.obj_identifier ' + $
                  'order by t1.station_id, t2.date;" > ' + $
                  scratchFilePath

  endif else begin

      statement = 'psql -d operations -h ' + PGHost + ' -t -A -c ' + $
                  '"' + $
                  'set search_path = nohrsc; ' + $
                  'select ' + $
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
                  'and (date_part(''minute'',date) >= 55 or ' + $
                       'date_part(''minute'',date) <= 5) ' + $
                  'and precip_raw.duration = 1 ' + $
                  'and precip_raw.qc = 1 ' + $
                  'and precip_raw.value >= 0.0 ' + $
                  'and precip_raw.value < 99.5 ' + $
                  'and station.station_id = precip_raw.station_id ' + $
                  'order by station.station_id, precip_raw.date;" > ' + $
                  scratchFilePath

  endelse

  SPAWN, statement, EXIT_STATUS = status

  if (status ne 0) then begin
      ERR_MSG, 'psql statement failed: ' + statement
      RETURN
  endif

  if NOT(FILE_TEST(scratchFilePath)) then begin
      ERR_MSG, 'Output file ' + scratchFilePath + ' not found.'
      RETURN
  endif


; Count the number of reports.

  ;; numPrecipReports = N_ELEMENTS(result)

  ;; if (result[0] eq '') then begin
  ;;     numPrecipReports = 0
  ;;     RETURN
  ;; endif

  cmd = 'cat ' + scratchFilePath + ' | wc -l'

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

  USR_MSG, string(10B) + '--> Found ' + STRCRA(numPrecipReports) + ' precip reports.'


; Count the number of stations.

  ;; numStations = 0
  ;; prevStationID = ''
  ;; for rc = 0, numPrecipReports - 1 do begin
  ;;     report = STRSPLIT(result[rc], '|', /EXTRACT)
  ;;     stationID = report[0]
  ;;     if (stationID ne prevStationID) then begin
  ;;         numStations++
  ;;         prevStationID = stationID
  ;;     endif
  ;; endfor

  cmd = 'cat ' + scratchFilePath + ' | awk -F ''|'' ''{print $1}'' | ' + $
        'sort | uniq | wc -l'

  SPAWN, cmd, numStations, EXIT_STATUS = status
  if (status ne 0) then begin
      ERR_MSG, 'Command "' + cmd + '" failed.'
      RETURN
  endif

  if (N_ELEMENTS(numStations) ne 1) then begin
      ERR_MSG, 'Command "' + cmd + '" output invalid.'
      STOP
  endif

  numStations = LONG(numStations[0])

  USR_MSG, '--> Found data for ' + STRCRA(numStations) + ' unique stations.' + string(10B)


; Place results in a structure.

  numHours = ROUND((finishDate_Julian - startDate_Julian) * 24.0D + 1.0D)

  precipReport = REPLICATE({station_id: '', $
                            station_name: '', $
                            station_type: '', $
                            longitude: 0.0d, $
                            latitude: 0.0d, $
                            elevation: 0L, $
                            value_meters: REPLICATE(ndv, numHours)}, $
                           numStations)

  multipleFlag = INTARR(numStations)

  prevID = ''
  sc = -1L ; station index
  prevTimeInd = -1L

  OPENR, lun, scratchFilePath, /GET_LUN
  line = ''

  for rc = 0, numPrecipReports - 1 do begin

      READF, lun, line
      report = STRSPLIT(line, '|', /EXTRACT)

      if (N_ELEMENTS(report) ne 8) then $
          MESSAGE, 'Incomplete report "' + report + '".'

      id = report[0]

      if (id ne prevId) then begin
          sc++
          precipReport[sc].station_id = id
          precipReport[sc].station_name = report[1]
          precipReport[sc].station_type = report[2]
          precipReport[sc].longitude = DOUBLE(report[3])
          precipReport[sc].latitude = DOUBLE(report[4])
          precipReport[sc].elevation = LONG(report[5])
;          valueMeters = FLOAT(report[7])
          value_meters = MAKE_ARRAY(numHours, /FLOAT, VALUE = ndv)
          prevID = id
;print, 'new station ' + id
      endif

      time = report[6]

      time_Julian = JULDAY(FIX(STRMID(time, 5, 2)), $
                           FIX(STRMID(time, 8, 2)), $
                           FIX(STRMID(time, 0, 4)), $
                           FIX(STRMID(time, 11, 2)), $
                           FIX(STRMID(time, 14, 2)), $
                           FIX(STRMID(time, 17, 2)))

      time_Julian_hour = ROUND(time_Julian * 24.0D) / 24.0D
      time_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(time_Julian_hour)
      timeInd = ROUND((time_Julian_Hour - startDate_Julian) * 24.0D)

      if (timeInd ne prevTimeInd) then begin
          minGapMinutes = 1440.0D  ; reset gap to one day
          prevTimeInd = timeInd
      endif

      gapMinutes = ABS(time_Julian - time_Julian_hour) * 1440.0D
      if (gapMinutes lt minGapMinutes) then begin
          if (minGapMinutes ne 1440.0D) then multipleFlag[sc] = 1
          value_meters[timeInd] = FLOAT(report[7])
          minGapMinutes = gapMinutes
      endif

      precipReport[sc].value_meters = value_meters

  endfor

  if NOT(EOF(lun)) then begin
      ERR_MSG, 'WARNING: read all reports but have not reached the ' + $
               'end of the file.'
      STOP
  endif

  FREE_LUN, lun

  FILE_DELETE, scratchFilePath

  if NOT(FILE_TEST(savFilePath)) then begin

      ndv__ = ndv
      startDate_YYYYMMDDHH__ = startDate_YYYYMMDDHH
      finishDate_YYYYMMDDHH__ = finishDate_YYYYMMDDHH
      minLon__ = minLon_
      maxLon__ = maxLon_
      minLat__ = minLat_
      maxLat__ = maxLat_

      SAVE, precipReport, numPrecipReports, $
            ndv__, $
            startDate_YYYYMMDDHH__, finishDate_YYYYMMDDHH__, $
            minLon__, maxLon__, minLat__, maxLat__, $
            FILE = savFilePath

  endif

  RETURN

end
