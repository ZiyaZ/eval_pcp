
PRO EPVS_01, eventPrecipObs_mm, $
             eventPrecipMdl_mm, $
             eventPrecipThresh_mm, $
             ndv, $
             ec, $
             a_hits, $
             b_false_positives, $
             c_misses, $
             d_correct_negatives, $
             totAbsRelErr, $
             totLogMultBias, $
             totLogMultBiasSquared, $
             AUDIT = audit

;+
; Calculate across-the-spatial domain event statistics for "modeled"
; precipitation vs. observed precipitation, for
; eval_pcp_vs_sites.pro.
;-

;+
; Fill out contingency table.
;-

  okInd = WHERE((eventPrecipObs_mm ne ndv) and $
                (eventPrecipMdl_mm ne ndv), count)
  if (count eq 0) then begin
      ERR_MSG, 'No data available for comparison.'
      RETURN
  endif
  aInd = WHERE((eventPrecipObs_mm[okInd] gt eventPrecipThresh_mm) and $
               (eventPrecipMdl_mm[okInd] gt eventPrecipThresh_mm), $
               aCount)          ; hits
  a_hits[ec] = aCount
  bInd = WHERE((eventPrecipObs_mm[okInd] le eventPrecipThresh_mm) and $
               (eventPrecipMdl_mm[okInd] gt eventPrecipThresh_mm), $
               bCount)          ; false positives
  b_false_positives[ec] = bCount
  cInd = WHERE((eventPrecipObs_mm[okInd] gt eventPrecipThresh_mm) and $
               (eventPrecipMdl_mm[okInd] le eventPrecipThresh_mm), $
               cCount)          ; misses
  c_misses[ec] = cCount
  dInd = WHERE((eventPrecipObs_mm[okInd] le eventPrecipThresh_mm) and $
               (eventPrecipMdl_mm[okInd] le eventPrecipThresh_mm), $
               dCount)          ; correct negatives
  d_correct_negatives[ec] = dCount

  if KEYWORD_SET(audit) then STOP

;+
; Gather data for continuous statistical skill measures.
;-
  if (aCount gt 0) then begin
      numer1 = eventPrecipMdl_mm[okInd[aInd]]
      numer2 = eventPrecipMdl_mm[okInd[aInd]] - $
               eventPrecipObs_mm[okInd[aInd]]
      denom = eventPrecipObs_mm[okInd[aInd]]
      totAbsRelErr[ec] = TOTAL(ABS(numer2) / denom)
      totLogMultBias[ec] = TOTAL(ALOG10(numer1 / denom))
      totLogMultBiasSquared[ec] = TOTAL((ALOG10(numer1 / denom))^2.0)
      ;corr[ec] = CORRELATE(eventPrecipObs_mm[okInd[aInd]], eventPrecipMdl_mm[okInd[aInd]])
  endif

  if KEYWORD_SET(audit) then STOP

  RETURN

end

PRO EPVS_02, ndvInt, $ ; no-data value for categorical inputs
             ndvFlt, $ ; no-data value for continuous inputs
             ec1, ec2, $              ; event index range for collection
             a_hits, $                ; # events
             b_false_positives, $     ; # events
             c_misses, $              ; # events
             d_correct_negatives, $   ; # events
             totAbsRelErr, $          ; # events
             totLogMultBias, $        ; # events
             totLogMultBiasSquared, $ ; # events
             aec, $                   ; collected event index
             POD, $                   ; # collected events
             FAR, $                   ; # collected events
             meanAbsRelErr, $         ; # collected events
             geomMeanMultBias, $      ; # collected events
             stdDevLogMultBias, $     ; # collected events
             HSS, $                   ; HeidkeSkillScore, # collected events
             CSI, $                   ; Critical Success Index # collected events
             AUDIT = audit

;+
; Calculate event-collection statistics for "modeled" precipitation
; vs. observed precipitation, for eval_pcp_vs_sites.pro.
;-
  aSample = a_hits[ec1:ec2] ; e.g., [0, 100, 0]
;  foo = WHERE(aSample eq ndvInt, bar)
;  if (bar gt 0) then STOP       ; PROGRAMMING ERROR - no values of aSample
;                                ; can have values of ndvInt if days
;                                ; ec1:ec2 have been run through
;                                ; EPVS_01.
  bSample = b_false_positives[ec1:ec2]
;  foo = WHERE(bSample eq ndvInt, bar)
;  if (bar gt 0) then STOP       ; PROGRAMMING ERROR - as for aSample
  cSample = c_misses[ec1:ec2]
;  foo = WHERE(cSample eq ndvInt, bar)
;  if (bar gt 0) then STOP       ; PROGRAMMING ERROR - as for aSample

  dSample = d_correct_negatives[ec1:ec2]
;+
; Inventory hits.
;- 
  minHits = 10
  hitInd = WHERE((aSample ne ndvInt) and (aSample gt 0), hitCount)
  totalHits = TOTAL(aSample[hitInd], /INT) ; e.g. 100

  if KEYWORD_SET(audit) then STOP

  if (totalHits lt minHits) then RETURN

  ind = WHERE((aSample ne ndvInt) and $
              (cSample ne ndvInt) and $
              ((aSample + cSample) gt 0), count)
  if (count gt 0) then $
      POD[aec] = TOTAL(aSample[ind]) / TOTAL(aSample[ind] + cSample[ind])

  ind = WHERE((aSample ne ndvInt) and $
              (bSample ne ndvInt) and $
              ((aSample + bSample) gt 0), count)
  if (count gt 0) then $
      FAR[aec] = TOTAL(bSample[ind]) / TOTAL(aSample[ind] + bSample[ind])

  ind = WHERE((aSample ne ndvInt) and $
              (bSample ne ndvInt) and $
              (cSample ne ndvInt) and $
              (dSample ne ndvInt), count)

  if (count gt 0) then begin
      aTotal = TOTAL(aSample[ind])
      bTotal = TOTAL(bSample[ind])
      cTotal = TOTAL(cSample[ind])
      dTotal = TOTAL(dSample[ind])
;      HSS[aec] = 2.0 * (aTotal * bTotal - bTotal * cTotal) / $
;                              ((aTotal + cTotal) * (cTotal + dTotal) + $
;                               (aTotal + bTotal) * (bTotal + dTotal))
      HSS[aec] = 2.0 * (aTotal * dTotal - bTotal * cTotal) / $
                              ((aTotal + cTotal) * (cTotal + dTotal) + $
                               (aTotal + bTotal) * (bTotal + dTotal))
  endif

  ind = WHERE((aSample ne ndvInt) and $
              (bSample ne ndvInt) and $
              (cSample ne ndvInt) and $
              ((aSample+bSample+cSample) gt 0), count)
  if (count gt 0) then $
     CSI[aec] = TOTAL(aSample[ind]) / TOTAL(aSample[ind] + bSample[ind] + cSample[ind])



  numer = totAbsRelErr[ec1:ec2] ; e.g., [ndv, x.x, ndv]
  numer2 = totLogMultBias[ec1:ec2]
  numer3 = totLogMultBiasSquared[ec1:ec2]
  ;denom = a_hits[ec1:ec2]

  ;ind = WHERE((aSample ne ndvInt) and (aSample gt 0), count)
  if (totalHits gt 0) then begin

      ind = WHERE(numer ne ndvFlt, count)
      if (count eq 0) then STOP ; PROGRAMMING ERROR: there should be some
                                ; non-no-data-values in numer because
                                ; there is a positive number of
                                ; totalHits
      killme = aSample[ind]
      foo = WHERE(killme eq 0, bar)
      if (bar gt 0) then STOP   ; PROGRAMMING ERROR: a_hits zero vs. nonzero
                                ; should perfectly match numer no-data
                                ; vs. data.
      killme = numer2[ind]
      foo = WHERE(killme eq ndvFlt, bar)
      if (bar gt 0) then STOP   ; PROGRAMMING ERROR - numer2 data/no-data
                                ; should perfectly match numer
                                ; data/no-data
      killme = numer3[ind]
      foo = WHERE(killme eq ndvFlt, bar)
      if (bar gt 0) then STOP   ; PROGRAMMING ERROR - numer3 data/no-data
                                ; should perfectly match numer
                                ; data/no-data
      killme = !NULL
      foo = !NULL
      bar = !NULL

      meanAbsRelErr[aec] = TOTAL(numer[ind]) / FLOAT(totalHits)
      geomMeanMultBias[aec] = 10.0^(TOTAL(numer2[ind]) / FLOAT(totalHits))
      if (totalHits gt 1) then $
          stdDevLogMultBias[aec] = SQRT(TOTAL(numer3[ind]) / $
                                        (FLOAT(totalHits) - 1.0) - $
                                        (TOTAL(numer2[ind]) / $
                                         (FLOAT(totalHits) - 1.0))^2.0)
  endif

  if KEYWORD_SET(audit) then STOP

  RETURN

end

PRO EPVS_03, sitePrecipObs_mm, $ ; IN - time series of observations at site
             sitePrecipMdl_mm, $ ; IN - time series of model values at site
             eventPrecipThresh_mm, $
             ndv, $
             sc, $ ; index for following arrays
             a_hits, $              ; OUT - # hits
             b_false_positives, $   ; OUT - # false positives
             c_misses, $            ; OUT - # misses
             d_correct_negatives, $ ; OUT - # correct negatives
             geomMeanMultBias, $    ; OUT - Geom. mean mult. bias
             meanAbsRelErr, $       ; OUT - mean absolute relative error
             stdDevLogMultBias, $   ; OUT - standard dev in Log mult bias
             POD, $                 ; OUT - probability of detection
             FAR, $                 ; OUT - false alarm ratio
             PCorr, $               ; OUT - CORRELATE()
             RCorr, $               ; OUT - R_CORRELATE()
             HSS, $                 ; OUT - Heidke Skill Score
             CSI                    ; OUT - Critical Success Index
;+
; Calculate across-the-time-domain event statistics for "modeled"
; precipitation vs. observed precipitation, for
; eval_pcp_vs_sites.pro.
;-

;+
; Fill out contingency table.
;-
  okInd = WHERE((sitePrecipObs_mm ne ndv) and $
                (sitePrecipMdl_mm ne ndv), count)

  if (count eq 0) then RETURN

  aInd = $
      WHERE((sitePrecipObs_mm[okInd] gt eventPrecipThresh_mm) and $
            (sitePrecipMdl_mm[okInd] gt eventPrecipThresh_mm), $
            aCount)             ; hits
  a_hits[sc] = aCount
  bInd = $
      WHERE((sitePrecipObs_mm[okInd] le eventPrecipThresh_mm) and $
            (sitePrecipMdl_mm[okInd] gt eventPrecipThresh_mm), $
            bCount)             ; false positives
  b_false_positives[sc] = bCount
  cInd = $
      WHERE((sitePrecipObs_mm[okInd] gt eventPrecipThresh_mm) and $
            (sitePrecipMdl_mm[okInd] le eventPrecipThresh_mm), $
            cCount)             ; misses
  c_misses[sc] = cCount
  dInd = $
      WHERE((sitePrecipObs_mm[okInd] le eventPrecipThresh_mm) and $
            (sitePrecipMdl_mm[okInd] le eventPrecipThresh_mm), $
            dCount)             ; correct negatives
  d_correct_negatives[sc] = dCount

  if (aCount gt 5) then begin

;      numer1 = sitePrecipMdl_mm[okInd[aInd]]
      absDiff = ABS(sitePrecipMdl_mm[okInd[aInd]] - $
                    sitePrecipObs_mm[okInd[aInd]])
      denom = sitePrecipObs_mm[okInd[aInd]]
      logBias = ALOG10(sitePrecipMdl_mm[okInd[aInd]] / denom)
      geomMeanMultBias[sc] = 10.0^(MEAN(logBias))
      stdDevLogMultBias[sc] = STDDEV(logBias)
      meanAbsRelErr[sc] = MEAN(absDiff / denom)
      POD[sc] = FLOAT(a_hits[sc]) / $
                (FLOAT(a_hits[sc]) + FLOAT(c_misses[sc]))
      FAR[sc] = FLOAT(b_false_positives[sc]) / $
                (FLOAT(a_hits[sc]) + FLOAT(b_false_positives[sc]))

      if (((a_hits[sc] + c_misses[sc]) gt 0) and $
          ((a_hits[sc] + b_false_positives[sc]) gt 0) and $
          (MIN(sitePrecipObs_mm[okInd]) ne MAX(sitePrecipObs_mm[okInd])) and $
          (MIN(sitePrecipMdl_mm[okInd]) ne MAX(sitePrecipMdl_mm[okInd]))) $
          then begin
          PCorr[sc] = CORRELATE(sitePrecipObs_mm[okInd], $
                                sitePrecipMdl_mm[okInd] )
          rCorrTemp = R_CORRELATE(sitePrecipObs_mm[okInd], $
                                  sitePrecipMdl_mm[okInd] )
          RCorr[sc] = rCorrTemp[0]
      endif 

      HSS[sc] = 2.0*FLOAT(a_hits[sc]*d_correct_negatives[sc]-b_false_positives[sc]*c_misses[sc])/ $
              FLOAT((a_hits[sc]+c_misses[sc])*(c_misses[sc]+d_correct_negatives[sc]) + $
               (a_hits[sc]+b_false_positives[sc])*(b_false_positives[sc]+d_correct_negatives[sc]))
 
      CSI[sc] = FLOAT(a_hits[sc])/FLOAT(a_hits[sc] + c_misses[sc] + b_false_positives[sc])

      if NOT(FINITE(geomMeanMultBias[sc])) then STOP
      if NOT(FINITE(stdDevLogMultBias[sc])) then STOP
      if NOT(FINITE(meanAbsRelErr[sc])) then STOP
      if NOT(FINITE(POD[sc])) then STOP
      if NOT(FINITE(FAR[sc])) then STOP
      if NOT(FINITE(PCorr[sc])) then STOP
      if NOT(FINITE(RCorr[sc])) then STOP
      if (HSS[sc] gt 1.0) then STOP
      if ((CSI[sc] gt 1.0) or (CSI[sc] lt 0.0)) then STOP

  endif

  RETURN

end

PRO ELIMINATE_TEXT_VERT_OVERLAP, textArr

;+
; Adjust the positions of IDL TEXT objects so they do not overlap
; vertically.
;-

  numText = N_ELEMENTS(textArr)
  if (numText lt 2) then RETURN

;+
; Set up array of positions for all TEXT objects.
;-
  allPos = FLTARR(4, numText)

  for tc = 0, numText - 1 do begin

;+
;     Make sure each element of textArr is an object reference.
;-
      if NOT(ISA(textArr[tc], 'OBJREF')) then begin
          ERR_MSG, 'Input argument must be an array of TEXT objects.'
          RETURN
      endif

;+
;     Make sure each element of textArr has a "position" property.'
;-
      if NOT(ISA(textArr[tc].position)) then begin
          ERR_MSG, 'Missing "position" property in TEXT object.'
          RETURN
      endif

;+
;     Make sure position is a 4-element array.
;-
      if (N_ELEMENTS(textArr[tc].position) ne 4) then begin
          ERR_MSG, 'Property "position" in TEXT object is not a ' + $
                   '4-element array.'
          RETURN
      endif

      allPos[*, tc] = textArr[tc].position

  endfor

  y1 = REFORM(allPos[1, *]) ; lower y coordinate of text objects
  y2 = REFORM(allPos[3, *]) ; upper y coordinate of text objects

;+
; Wherever the upper y coordinate of one or more text object is lower
; than the lower y coordinate of the text object above it, separate
; them.
;-
  order = SORT(y1)

;zyz  overlap = y1[order[1:numText - 1]] lt y2[order[0:numText - 2]]
  overlap = y1[order[0:numText - 1]] lt y2[order[0:numText - 2]]
  thereIsOverlap = TOTAL(overlap, /INTEGER) < 1
  numIters = 0
  maxIters = 250

  while (thereIsOverlap and (numIters lt maxIters)) do begin

      numIters++

      for pc = 0, numText - 2 do begin

          if NOT(overlap[pc]) then CONTINUE

;+
;         Move both text objects by a small distance.
;-
          tcAbove = order[pc + 1]
          abovePos = textArr[tcAbove].position
          abovePos[1] = abovePos[1] + 0.005
          abovePos[3] = abovePos[3] + 0.005
          if (abovePos[3] gt 1.0) then begin
              drop = abovePos[3] - 1.0
              abovePos[1] = abovePos[1] - drop
              abovePos[3] = abovePos[3] - drop
          endif
          textArr[tcAbove].position = abovePos
          allPos[*, tcAbove] = abovePos
;          WAIT, 0.1

          tcBelow = order[pc]
          belowPos = textArr[tcBelow].position
          belowPos[1] = belowPos[1] - 0.005
          belowPos[3] = belowPos[3] - 0.005
          if (belowPos[1] lt 0.0) then begin
              raise = -belowPos[1]
              belowPos[1] = belowPos[1] + raise
              belowPos[3] = belowPos[3] + raise
          endif
          textArr[tcBelow].position = belowPos
          allPos[*, tcBelow] = belowPos
;          WAIT, 0.1

          y1 = REFORM(allPos[1, *])
          y2 = REFORM(allPos[3, *])

          order = SORT(y1)
          overlap = y1[order[1:numText - 1]] lt y2[order[0:numText - 2]]
          thereIsOverlap = TOTAL(overlap, /INTEGER) < 1

      endfor

  endwhile


  RETURN

end




;                             ****************
;                             * MAIN PROGRAM *
;                             ****************


  !EXCEPT = 1


; Evaluate aggregate gridded precipitation forecasts and analyses
; against station data from NOHRSC.
; Precipitation data include: EMC_MPE, OWP_MPE, HRRR_f01-f03, MRMS_GC/RO
;   RAP_f01-f03, NLDAS, NLDAS_MM, NWM_tm02, HRRRX-f01-f03


;  threshold_in = [0.01, 0.1, 0.25, 0.5, 1.0, 1.5, 2.0, 3.0]
;  threshold_mm = threshold_in * 25.4
;  numThreshold = N_ELEMENTS(threshold_mm)
;  eventThreshold_mm = 0.0

;  eventThreshold_in = 0.50
;  eventThreshold_in = 0.25
;  eventThreshold_in = 1.0

  minLon = -130.0D
  maxLon = -60.0D
  minLat = 20.0D
  maxLat = 55.0D
  domainName = 'CONUS'
  domainAbbrev = 'CONUS'
  vectorDir = '/nwcdev/nwc_forcings/GIS_data/Vector' ; for maps

  ndv = -99999.0
  degToRad = !DPi / 180.0D

  loginInfo = GET_LOGIN_INFO()
  processID = GETPID()
  PGHost = 'ddb0'
  webPGHost = 'wdb0'

  st4Dir = '/nwcdev/archive/StageIV_archive'
  MPEDir = '/nwcdev/nwc_forcings/projects/anl_ext_2017_2018/' + $
           'OWP_MPE'
  OWP_MPE_fileTemplate = 'ST4_OWP_RQIThresh0.5_RQIStep20_RQITestOff_' + $
                       'ThreshMM0_RRFcst02.YYYYMMDDHH.01h.nc'
  HRRRDir = '/nwcdev/archive/HRRR_archive'
  HRRRXDir = '/nwcdev/archive/HRRRX_archive'
  MRMSDir = '/nwcdev/archive/MRMS_archive'
  RAPDir = '/nwcdev/archive/RAP_archive'
  NLDASDir = '/nwcdev/archive/NLDAS_archive'
  AORCDir = '/nwcdev/archive/AORC_archive' 
  RTMADir = '/nwcdev/archive/RTMA_archive'
  NWMPRDir = '/net/lfs0data5/NWM_v1.2_archive'
  ;scratchDir = '/disks/scratch/' + loginInfo.user_name
  scratchDir = '/net/lfs0data10/eval_pcp_scratch'
; CONFIG! for output directory
  outputDir = 'CONUS_Winter_2017_2018_Snow_OWP_MPE_HRRR_RAP_f01f02f03'
;  outputDir = '/nwcdev/nwc_forcings/projects/precip/multi_precip_eval_daily/myout'
  if NOT(FILE_TEST(outputDir, /DIRECTORY)) then begin
      ERR_MSG, 'Output directory "' + outputDir + '" not found.'
      FILE_MKDIR, outputDir
  endif

  accEventHours = 24L ; size of accumulation period to evaluate
  eventName = 'Day'
  if ((accEventHours mod 6) ne 0) then STOP

;  startDate_YYYYMMDDHH = '2017053112' ; start of first accumulation period
;  finishDate_YYYYMMDDHH = '2017100112' ; end of last accumulation period
;  timeDomainStr = 'JJAS 2017'
;  timeDomainAbbrev = 'JJAS_2017'

  startDate_YYYYMMDDHH = '2015060112' 
  finishDate_YYYYMMDDHH = '2018012012'
  timeDomainStr = 'June 2015 - Jan. 2018'
  timeDomainAbbrev = 'June_2015_to_Jan_2018'

  startDate_YYYYMMDDHH = '2017010112'
  finishDate_YYYYMMDDHH = '2017040112'
  timeDomainStr = 'Jan - Mar 2017'
  timeDomainAbbrev = 'Jan_Mar_2017'

  startDate_YYYYMMDDHH = '2017010112'
  finishDate_YYYYMMDDHH = '2017020112'
  timeDomainStr = 'Jan 2017'
  timeDomainAbbrev = 'Jan_2017'

  startDate_YYYYMMDDHH = '2017010112'
  finishDate_YYYYMMDDHH = '2018010112'
  timeDomainStr = '2017'
  timeDomainAbbrev = '2017'

  startDate_YYYYMMDDHH = '2014100112' 
  finishDate_YYYYMMDDHH = '2018030112'
  timeDomainStr = 'Oct. 2014 - Feb. 2018'
  timeDomainAbbrev = 'Oct_2014_to_Feb_2018'

  startDate_YYYYMMDDHH = '2014100112'
  finishDate_YYYYMMDDHH = '2017100112'
  timeDomainStr = 'Oct. 2014 - Sep. 2017'
  timeDomainAbbrev = 'Oct_2014_to_Sep_2017'

  startDate_YYYYMMDDHH = '2018061012'
  finishDate_YYYYMMDDHH = '2018081012'
  timeDomainStr = 'Partial Summer 2018'
  timeDomainAbbrev = 'Partial_Summer_2018'

  startDate_YYYYMMDDHH = '2014100112'
  finishDate_YYYYMMDDHH = '2018080112'
  timeDomainStr = 'Oct. 2014 - Jul. 2018'
  timeDomainAbbrev = 'Oct_2014_to_Jul_2018'

  startDate_YYYYMMDDHH = '2018060112'
  finishDate_YYYYMMDDHH = '2018090112'
  timeDomainStr = 'JJA 2018'
  timeDomainAbbrev = 'JJA_2018'

  startDate_YYYYMMDDHH = '2018071212'
  finishDate_YYYYMMDDHH = '2018091112'
  timeDomainStr = 'HRRR v3 2018'
  timeDomainAbbrev = 'HRRR_v3_2018'

  startDate_YYYYMMDDHH = '2017100112'
  finishDate_YYYYMMDDHH = '2018050112'
  timeDomainStr = 'Winter 2017-18 (Snow)'
  timeDomainAbbrev = 'Winter_2017_2018_Snow'
; CONFIG!
;  startDate_YYYYMMDDHH = '2016020112'
;  finishDate_YYYYMMDDHH = '2016050112'
;  timeDomainStr = 'FMA 2016'
;  timeDomainAbbrev = 'FMA_2016'


;************************************************************************
;+ 
;  ############################################
;  #   DEFINE PARAMETERS BELOW FOR YOUR RUN   #
;  ############################################
;-

  reportingThreshold = 0.5 ; CONFIG! 0.9 = 90% or more
  eventAggFactor = 3 ; CONFIG! 1, 3, 7, 14
  if (eventAggFactor gt 1) then eventName = eventName + 's'
  eventThreshold_in = 0.1 ; CONFIG! 0.1; 0.25, 0.5, 1.0, 2.0
  eventThreshold_mm = eventThreshold_in * 25.4
  fullDomain = 1 ; CONFIG! if 0, boxOrShape needs to be defined
  coldSeason = 1 ; CONFIG! if 1, only use obs associated with cold temps
  checkHRRRWeight = 0 ; CONFIG!
  domainName = 'CONUS' ; CONFIG!
  domainAbbrev = 'CONUS' ; CONFIG!

;+
; Uncomment the next line to only consider areas where HRRR receives
; significant weight, on average, for NWM v1.2 analysis/assimilation
; cycles.
;-
;  checkHRRRWeight = 1 & domainName = 'CONUS (HRRR)' & domainAbbrev = 'CONUS_HRRR'

  boxOrShape = 1 ; CONFIG! 1 = box, 2 = shape, used when fullDomain = 0

  if fullDomain then begin 
      boxOrShape = 0
      domainName = 'CONUS'
      domainAbbrev = 'CONUS'
  endif
  if NOT(fullDomain) then begin 
     if (boxOrShape eq 1) then begin

; --------------------------------------------------------------
;+
;     Example for boxOrShape = 1:
;-
         subMinLon = -117.0D
         subMaxLon = -109.0D
         subMinLat = 42.0D
         subMaxLat = 49.0D
         domainName = 'U.S. Northern Rockies'
         domainAbbrev = 'US_Northern_Rockies'


;+
;     for Eastern CONUS
;-
         subMinLon = -100.0D
         subMaxLon = -60.0D
         subMinLat = 20.0D
         subMaxLat = 55.0D
         domainName = 'Eastern CONUS'
         domainAbbrev = 'Eastern_CONUS'
;        CONFIG!

;+
;     for Western CONUS
;-
         subMinLon = -130.0D
         subMaxLon = -100.0D
         subMinLat = 20.0D
         subMaxLat = 55.0D
         domainName = 'Western CONUS'
         domainAbbrev = 'Western_CONUS'
;        CONFIG!

         print, STRING(10B),'Running for ', domainAbbrev, $
                ' with the boxOrShape=', boxOrShape,STRING(10B)

     endif else if (boxOrShape eq 2) then begin

;;    boxOrShape = 2 ; 1 = box, 2 = shape, used when fullDomain = 0
; --------------------------------------------------------------
;+
;     Example for boxOrShape = 2:  (For RFCs)
;-
         shapeFilePath = vectorDir + '/' + 'RFC_Boundaries_Coterminous_US'
         shapeID = 'NWRFC'  ; MBRFC, CNRFC, NWRFC, etc
         shapeIDField = 'NAME'
         domainName = shapeID
         domainAbbrev = shapeID
;
; --------------------------------------------------------------
;+
;     Example for boxOrShape = 2:  (For Ecoregions)
;     Analysis across CEC Level III ecoregions.
;-
;;       shapeFilePath = vectorDir + '/' + 'NA_CEC_Eco_Level3_WGS84'

;;       shapeIDField = 'NA_L3NAME'

;;       shapeID = 'Southern Rockies'
;;       domainName = 'U.S. Southern Rockies'
;;       domainAbbrev = 'US_Southern_Rockies'

;;       shapeID = 'Middle Rockies'
;;       domainName = 'U.S. Middle Rockies'
;;       domainAbbrev = 'US_Middle_Rockies'
         print, 'Running for ', domainAbbrev, $
                ' with the boxOrShape=', boxOrShape

     endif else  begin
        print, 'Need to give boxOrShape option (1 or 2)'
        print, 'You are running for ', domainAbbrev, $
                ' with the boxOrShape = ', boxOrShape
        stop
     endelse
  endif


;+
; To subset data within the above bounds, you can use polygons in a
; shapefile, or you can use a box in longitude and latitude.
;-
;
;+
; Set sampling method: 0 = nearest neighbor, 1 = bilinear
;-
  sampling = 1

  if (sampling eq 1) then $
      samplingStr = 'Bilinear' $
  else $
      samplingStr = 'Neighbor'

  subtitle = 'Threshold: ' + $
             FORMAT_FLOAT(eventThreshold_mm) + ' mm = ' + $
             FORMAT_FLOAT(eventThreshold_in) + ' in, ' + $
             'Aggregation: ' + STRCRA(eventAggFactor) + ' ' + $
             eventName + ', ' + $
             ;'Sampling: ' + samplingStr + ', ' + $
             'Domain: ' + domainName

;************************************************************************

;+
; Set flags for input elements. This flag determines whether an
; element is included in the analysis.
;-
  inputFlag = [0, $             ; EMC MPE, hourly
               0, $             ; EMC MPE, 6-hourly
               0, $             ; EMC MPE, 24-hourly
               1, $             ; OWP MPE
               1, $             ; HRRR f01
               1, $             ; HRRR f02
               1, $             ; HRRR f03
               0, $             ; HRRRX f01
               0, $             ; HRRRX f02
               0, $             ; HRRRX f03
               0, $             ; MRMS radar-only
               0, $             ; MRMS gauge-corrected
               1, $             ; RAP f01
               1, $             ; RAP f02
               1, $             ; RAP f03
               0, $             ; NWM Analysis Assimilatioon Forcing data
               0, $             ; NLDAS
               0, $             ; Mountain Mapper downscaled NLDAS
               0]               ; AORC hourly CONFIG!

  inputFlagName = ['EMC_MPE_1', $
                   'EMC_MPE_6', $
                   'EMC_MPE_24', $
                   'OWP_MPE', $
                   'HRRR_f01', $
                   'HRRR_f02', $
                   'HRRR_f03', $
                   'HRRRX_f01', $
                   'HRRRX_f02', $
                   'HRRRX_f03', $
                   'MRMS_RO', $
                   'MRMS_GC', $
                   'RAP_f01', $
                   'RAP_f02', $
                   'RAP_f03', $
                   'NWM_tm02', $
                   'NLDAS', $
                   'NLDAS_MM', $
                   'AORC']

  inputGroupAbbrev = ''

  for i = 0, N_ELEMENTS(inputFlag) - 1 do begin
      if (inputFlag[i] eq 1 ) then $
         inputGroupAbbrev = inputGroupAbbrev + inputFlagName[i] + '_'

  endfor

;  EMC_MPE_1 = 0
;  EMC_MPE_6 = 1
;  EMC_MPE_24 = 2
;  OWP_MPE = 3
;  HRRR_f01 = 4
;  HRRR_f02 = 5
;  HRRR_f03 = 6
;  MRMS_RO = 7
;  MRMS_GC = 8
;  RAP_f01 = 9
;  RAP_f02 = 10
;  RAP_f03 = 11
;  NLDAS = 12
;  NLDAS_MM = 13

  if (TOTAL(inputFlag, /INT) eq 0) then STOP

  startDate_Julian = YYYYMMDDHH_TO_JULIAN(startDate_YYYYMMDDHH)
  finishDate_Julian = YYYYMMDDHH_TO_JULIAN(finishDate_YYYYMMDDHH)
  if (startDate_Julian ge finishDate_Julian) then STOP

;  dateLabel = LABEL_DATE(DATE_FORMAT = '%Y!C%M %D!C%HZ')
  dateLabel = LABEL_DATE(DATE_FORMAT = '%Y!C%M %D')

  numHours = ROUND((finishDate_Julian - startDate_Julian) * 24.0D)
  if ((numHours mod accEventHours) ne 0) then begin
      ERR_MSG, 'Start/finish dates span ' + STRCRA(numHours) + $
               ' hours, which is not a multiple of the configured ' + $
               ' event accumulation period (' + STRCRA(accEventHours) + $
               ' hours).'
      STOP
  endif

  numEvents = numHours / accEventHours

;+
; Define colors for precipitation grids in mm.
;-
  qRed = [255, 000, 000, 127, 238, 255, 255, 255, 238, 205, 139, $
          145, 137, 016, 030]
  qGrn = [228, 139, 205, 255, 238, 215, 165, 127, 064, 000, 000, $
          044, 104, 078, 144]
  qBlu = [220, 000, 000, 000, 000, 000, 079, 000, 000, 000, 000, $
          238, 205, 139, 255]
  qEdges = [0.0, 0.1, 2.0, 5.0, 10.0, 15.0, 20.0, 25.0, 35.0, 50.0, $
            75.0, 100.0, 125.0, 150.0, 175.0, 500.0]

;+
; Define polygons to draw over maps.
;-
  shapePathList = [vectorDir + '/' + $
                   'National_Boundary_Canada', $
                   vectorDir + '/' + $
                   'National_Boundary_Mexico_2004', $
                   vectorDir + '/' + $
                   'states_no_water', $
                   vectorDir + '/' + $
                   'national_no_water', $
                   vectorDir + '/' + $
                   'prv_ca', $
                   vectorDir + '/' + $
                   'RFC_Boundaries_Coterminous_US']
;                   vectorDir + '/' + $
;                   'NA_CEC_Eco_Level3_WGS84']
;                   vectorDir + '/' + $
;                   'RFC_Boundaries_Coterminous_US']


; Pantone "color of the year" for 2004-2017, for line colors.
; https://codepen.io/cassiocardoso/details/JbBgxP

  pRed = [000, 228, 079, 222, 156, 089, 240, 065, 218, 240, 022, $
          181, 149, 140, 136, 255]
  pGrn = [000, 088, 176, 205, 027, 092, 191, 182, 079, 084, 156, $
          101, 082, 164, 176, 255]
  pBlu = [000, 062, 174, 191, 049, 161, 089, 171, 112, 066, 120, $
          167, 081, 207, 075, 255]

; 2004 [1]: Tigerlily
; 2005 [1]: Blue Turquoise
; 2006 [2]: Sand Dollar
; 2007 [3]: Chili Pepper
; 2008 [4]: Blue Iris
; 2009 [5]: Mimosa
; 2010 [6]: Turquoise
; 2011 [7]: Honeysuckle
; 2012 [8]: Tangerine Tango
; 2013 [9]: Emerald
; 2014 [10]: Radiant Orchid
; 2015 [11]: Marsala
; 2016 [12]: Serenity
; 2017 [13]: Greenery

; 2006 "Sand Dollar" (222, 205, 191) is a little too desaturated.
  pRed[3] = 222
  pGrn[3] = 171
  pBlu[3] = 129

;+
; Assign 24-bit colors to input elements.
;-
  ;EMC_MPE_rgb = !Color.Chocolate
  EMC_MPE_1_rgb = !Color.Gray
  EMC_MPE_6_rgb = !Color.Peru
  EMC_MPE_24_rgb = !Color.Dark_Khaki
  OWP_MPE_rgb = !Color.Tomato
  HRRR_f01_rgb = !Color.Navy
  HRRR_f02_rgb = !Color.Blue
  HRRR_f03_rgb = !Color.Dodger_Blue
  MRMS_RO_rgb = !Color.Dark_Turquoise
  MRMS_GC_rgb = !Color.Teal
  RAP_f01_rgb = !Color.Dark_Olive_Green
  RAP_f02_rgb = !Color.Green
  RAP_f03_rgb = !Color.Lime_Green
  NWM_tm02_rgb = !Color.Medium_Purple
  NLDAS_rgb = !Color.Purple
  NLDAS_MM_rgb = !Color.Medium_Purple
  AORC_rgb = !Color.Medium_Purple
  HSS_rgb = !Color.Medium_Purple
  CSI_rgb = !Color.Medium_Purple

;+
; Define colors for multiplicative bias. Red means dry, blue means
; wet.
;-
  bEdges = [0.01, 0.1, 0.25, 0.50, 0.75, 1.25, 2.0, 4.0, 10.0, 100.0]

  bRed = [191, $
          207, 246, 255, $
          230, $
          023, 000, 075, $
          191]
  bGrn = [143, $
          000, 113, 216, $
          255, $
          216, 113, 000, $
          096]
  bBlu = [096, $
          075, 000, 023, $
          230, $
          255, 246, 246, $
          191]

;+
; Define colors for mean absolute relative error. The colors range
; from green to red.
;-
  mEdges = [0.01, 0.1, 0.15, 0.20, 0.25, 0.35, 0.50, 1.00, 2.0, 4.0]
  mEdges = [0.0, 0.2, 0.4, 0.6, 0.8, 1.0, 2.0, 4.0]

  mRed = [026, 145, 217, 255, 254, 252, 215]
  mGrn = [152, 207, 239, 255, 224, 141, 048]
  mBlu = [080, 096, 139, 191, 139, 089, 039]

;+
; Borrow MARE colors (above) for POD/FAR, reversing them for POD so
; high values are green.
;-
  podEdges = [0.0, 0.25, 0.35, 0.45, 0.55, 0.65, 0.75, 1.0]
  podRed = REVERSE(mRed)
  podGrn = REVERSE(mGrn)
  podBlu = REVERSE(mBlu)

  farEdges = podEdges
  farRed = mRed
  farGrn = mGrn
  farBlu = mBlu

;+ 
; Define colors for Heidke Skill Score. The colors range from red to green
;-
  hssEdges = [-4.0, -1.0, -0.5, 0.0, 0.25, 0.5, 0.75, 1.0]
  hssRed = REVERSE(mRed)
  hssGrn = REVERSE(mGrn)
  hssBlu = REVERSE(mBlu)

;+ 
; Define colors for Critical Success Index. The colors range from red to green
;-
  csiEdges = [0.0, 0.15, 0.30, 0.45, 0.60, 0.75, 0.90, 1.0]
  csiRed = REVERSE(mRed)
  csiGrn = REVERSE(mGrn)
  csiBlu = REVERSE(mBlu)


;+
; Define colors for reporting frequency.
;-
  fRed = [227, 251, 255, 253, 051, 178, 031, 166, 106, 202]
  fGrn = [026, 154, 127, 191, 160, 223, 120, 206, 061, 178]
  fBlu = [028, 153, 000, 111, 044, 138, 180, 227, 154, 214]
  fEdges = FINDGEN(11) * 10.0

  if fullDomain then boxOrShape = 0

;  if NOT(fullDomain) then begin
  if (NOT(fullDomain) and boxOrShape eq 2 )then begin

      GF_GET_SHAPE_BOUNDS, shapeFilePath, shapeID, shapeIDField, $
                           subMinLon, subMaxLon, subMinLat, subMaxLat

  endif ;else begin

;      subMinLon = minLon
;      subMaxLon = maxLon
;      subMinLat = minLat
;      subMaxLat = maxLat

;  endelse

  if NOT(ISA(NWMGridInfo)) then begin

;+
;     Define the NWM grid/projection.
;-
      GET_NWM_GRID_PROJ_INFO, NWMGridInfo

  endif

;+
; Get longitudes and latitudes of NWM cells, which is needed for map
; generation..
;-
  if (NOT(ISA(lonGrid_NWM)) or NOT(ISA(latGrid_NWM))) then begin
      PRINT, 'Getting NWM grid longitudes and latitudes.'
      GET_LON_LAT_OF_LCC_GRID_CENTERS, NWMGridInfo, $
                                       lonGrid_NWM, $
                                       latGrid_NWM
  endif

;  if (NOT(fullDomain) and (boxOrShape eq 1)) then begin ; !!!!!! GF REMOVE BOXORSHAPE EQ 1 CONDITION !!!!!!!
  if NOT(fullDomain) then begin

      ind = WHERE((lonGrid_NWM ge subMinLon) and $
                  (latGrid_NWM ge subMinLat) and $
                  (lonGrid_NWM le subMaxLon) and $
                  (latGrid_NWM le subMaxLat), count)
      if (count eq 0) then STOP
      minCol = MIN(ind mod NWMGridInfo.nCols)
      maxCol = MAX(ind mod NWMGridInfo.nCols)
      minRow = MIN(ind / NWMGridInfo.nCols)
      maxRow = MAX(ind / NWMGridInfo.nCols)

      ;; minCol = MIN(WHERE(lonGrid_NWM ge subMinLon) mod NWMGridInfo.nCols)
      ;; maxCol = MAX(WHERE(lonGrid_NWM le subMaxLon) mod NWMGridInfo.nCols)
      ;; minRow = MIN(WHERE(latGrid_NWM ge subMinLat) / NWMGridInfo.nCols)
      ;; maxRow = MAX(WHERE(latGrid_NWM le subMaxLat) / NWMGridInfo.nCols)

      plotGridInfo = NWMGridInfo
      plotGridInfo.nCols = maxCol - minCol + 1L
      plotGridInfo.nRows = maxRow - minRow + 1L
      plotGridInfo.lon00 = lonGrid_NWM[minCol, minRow]
      plotGridInfo.lat00 = latGrid_NWM[minCol, minRow]

  endif else plotGridInfo = NWMGridInfo

;+
; Read NWM v1.2 HRRR weights for HRRR + RAP + MRMS blending in
; analysis/assimilation cycle forcing engine, and generate a single
; binary grid indicating areas where HRRR weight is commonly
; significant.
;-
  if checkHRRRWeight then begin

      HRRRWtDir = '/net/lfs0data10/NWM_params/rain_bias'
      tmpDir = '/disks/scratch'
;      ndv = -99999.0

      for m = 1, 12 do begin

          HRRRWtFile = 'HRRR_WGT_RQI0.9_m' + STRING(m, FORMAT = '(I2.2)') + $
                       '_v1.1_geosmth.grb2'

          DECODE_GRIB2_RECORD, HRRRWtDir + '/' + HRRRWtFile, $
                               tmpDir, $
                               ':POP:', $
                               record, $
                               verificationTime, $
                               level, $
                               fcstTimeRange, $
                               varAbbrev, $
                               varField, $
                               varUnits, $
                               numCols_, $
                               numRows_, $
                               HRRRWtGrid, $
                               NO_DATA_VALUE = ndv

          if (numCols_ ne NWMGridInfo.nCols) then STOP
          if (numRows_ ne NWMGridInfo.nRows) then STOP

          ;; if (m eq 1) then begin

          ;;     GET_GRIB2_LAMBERT_CONFORMAL_GRID_INFO, $
          ;;         HRRRWtFile, $
          ;;         HRRRWtDir, $
          ;;         scratchDir, $
          ;;         ':POP:', $
          ;;         nCols_, $
          ;;         nRows_, $
          ;;         lat1, $
          ;;         lon1, $
          ;;         lonV, $
          ;;         latD, $
          ;;         latSec1, $
          ;;         latSec2, $
          ;;         latSP, $
          ;;         lonSP, $
          ;;         dx, $
          ;;         dy, $
          ;;         status

          ;;     if NOT(status) then STOP

          ;;     if (numCols_ ne NWMGridInfo.nCols) then STOP
          ;;     if (numRows_ ne NWMGridInfo.nRows) then STOP

          ;; endif

          if (m eq 1) then begin

              totHRRRWtGrid = HRRRWtGrid

          endif else begin

              ind = WHERE((totHRRRWtGrid eq ndv) or $
                          (HRRRWtGrid eq ndv), count)
              totHRRRWtGrid = totHRRRWtGrid + HRRRWtGrid
              if (count gt 0) then totHRRRWtGrid[ind] = ndv

              if (m eq 12) then begin
                  aveHRRRWtGrid = TEMPORARY(totHRRRWtGrid) / 12.0
                  if (count gt 0) then aveHRRRWtGrid[ind] = ndv
              endif

          endelse

      endfor

  endif

;+
; Graphics setup.
;-
;  SET_PLOT, 'X'
;  DEVICE, SET_FONT = 'DejaVuSans', /TT_FONT

;+
; First get observations for the entire analysis period. Doing this
; first will enable us to assemble a complete list of observing
; stations, which will simplify the process of sampling grids for all
; stations. If we require data from all stations for all events this
; is not necessary, but doing it this way provides greater
; flexibility.
;-
  precipObs_Meters = !NULL
  numEvents = numHours / accEventHours

  for ec = 0L, numEvents - 1L do begin

      eventStartDate_Julian = startDate_Julian + $
                              DOUBLE(ec * accEventHours) / 24.0D
      eventFinishDate_Julian = eventStartDate_Julian + $
                               DOUBLE(accEventHours) / 24.0D
      eventFinishDate_YYYYMMDDHH = $
          JULIAN_TO_YYYYMMDDHH(eventFinishDate_Julian)

;+
;     Get observations.
;-
      cmd = 'date -u +%Y%m%d%H%M%S'
      SPAWN, cmd, currentDateStr, EXIT_STATUS = status
      if (status ne 0) then begin
          ERR_MSG, 'Command "' + cmd + '" failed.'
          STOP
      endif
      currentDateStr = currentDateStr[0]
      scratchFile = 'eval_pcp_vs_sites.pro.' + currentDateStr + $
                    '.pid' + STRCRA(processID)
      saveFile = 'precip_raw_' + STRCRA(accEventHours) + 'h_' + $
                 eventFinishDate_YYYYMMDDHH + '.sav'
      saveFile = scratchDir + '/' + $
                 'precip_raw_' + STRCRA(accEventHours) + 'h_' + $
                 eventFinishDate_YYYYMMDDHH + '.sav'

      GET_PRECIP_RAW_OBS, eventFinishDate_YYYYMMDDHH, $
                          accEventHours, $
                          minLon, $
                          maxLon, $
                          minLat, $
                          maxLat, $
                          ndv, $
                          PGHost, $
                          webPGHost, $
                          scratchDir + '/' + scratchFile, $
                          saveFile, $
                          precipReport, $
                          /FORCE_WEB_DB

      numPrecipReports = N_ELEMENTS(precipReport)

      if (numPrecipReports eq 0) then begin
          ERR_MSG, 'WARNING: no precip reports for ' + $
                   STRCRA(accEventHours) + '-hour precipitation ' + $
                   'ending ' + eventFinishDate_YYYYMMDDHH
          if (ec eq 0) then STOP ; will not work if first event
;          CONTINUE
          newNumStations = numStations
          fullSDInd = !NULL
          addPRToSDInd = !NULL
          GOTO, EXPAND_PRECIP_OBS_ARRAY
      endif else begin
          USR_MSG, 'Received ' + STRCRA(numPrecipReports) + $
                   ' precip_raw reports for ' + $
                   eventFinishDate_YYYYMMDDHH
      endelse

;+
;     Eliminate object ID values greater than 1000000, which causes us
;     to lose a few "stranger" reports, but prevents bonkers memory
;     usage in calls to CGSETINTERSECTION, which we need.
;-
      maxObjID = 1000001L
      ind = WHERE(precipReport.obj_id lt maxObjID, count)
      if (count eq 0) then begin
          ERR_MSG, 'No precip reports for ' + $
                   STRCRA(accEventHours) + '-hour precipitation ' + $
                   'ending ' + eventFinishDate_YYYYMMDDHH + $
                   ' had object IDs less than ' + $
                   STRCRA(maxObjID)
      endif else begin
          if (count ne numPrecipReports) then begin
              USR_MSG, 'Eliminated ' + $
                       STRCRA(numPrecipReports - count) + $
                       ' stations with object IDs exceeding ' + $
                       STRCRA(maxObjID - 1L)
              precipReport = precipReport[ind]
              numPrecipReports = count
          endif
      endelse
      ind = !NULL

;+
;     Break apart precipitation data into station metadata and
;     observations. The date tag is not important, since all
;     observations gathered by GET_PRECIP_RAW_OBS occur in a 10 minute
;     window very close to the eventFinishDate_YYYYMMDDHH.
;-
      fullSDInd = !NULL
      addPRToSDInd = !NULL

      if (ec eq 0) then begin

          if (numPrecipReports gt 0) then begin

              precipObs_Meters = precipReport.value_meters

              stationData = REPLICATE({obj_id: 0L, $
                                       station_id: '', $
                                       station_name: '', $
                                       station_type: '', $
                                       longitude: 0.0D, $
                                       latitude: 0.0D, $
                                       elevation: 0L, $
                                       use: 1}, $
                                      numPrecipReports)

              for sc = 0L, numPrecipReports - 1L do begin
                  stationData[sc].obj_id = precipReport[sc].obj_id
                  stationData[sc].station_id = precipReport[sc].station_id
                  stationData[sc].station_name = precipReport[sc].station_name
                  stationData[sc].station_type = precipReport[sc].station_type
                  stationData[sc].longitude = precipReport[sc].longitude
                  stationData[sc].latitude = precipReport[sc].latitude
                  stationData[sc].elevation = precipReport[sc].elevation
              endfor

              numStations = numPrecipReports

          endif else begin

;+
;             Create stationData with an imaginary station. It will be
;             eliminated later.
;-
              stationData = REPLICATE({obj_id: -1L, $
                                       station_id: 'NOTASTATION', $
                                       station_name: 'NOTASTATION', $
                                       station_type: 'NOTASTATION', $
                                       longitude: 0.5D * (minLon + maxLon), $
                                       latitude: 0.5D * (minLat + maxLat), $
                                       elevation: -9999L, $
                                       use: 0}, $
                                      numPrecipReports)
              precipObs_Meters = ndv

              numPrecipReports = 1
              numStations = 1
 
          endelse

          newNumStations = numStations
          USR_MSG, 'Initialized station data with ' + $
                   STRCRA(numStations) + $
                   ' stations.'

      endif else begin

;+
;         For stations that are already in stationData, find their
;         index and verify their metadata. For those that are not, add
;         them.
;-
          if (numPrecipReports gt 0) then begin

              commonObjID = CGSETINTERSECTION(stationData.obj_id, $
                                              precipReport.obj_id, $
                                              COUNT = commonCount, $
                                              INDICES_A = SDInd, $
                                              INDICES_B = PRInd)

;+
;             Note: stationData[SDInd].obj_id eq
;                   precipReport[PRInd].obj_id
;-

;+
;             Make a list of stationData indices for all stations in
;             precipReport, with a -1 indicating new stations (i.e.,
;             stations that are not part of stationData yet).
;-
              fullSDInd = MAKE_ARRAY(numPrecipReports, VALUE = -1L)
              if (commonCount gt 0) then fullSDInd[PRInd] = SDInd

;+
;             Add new reporting stations to stationData.
;-
              addPRToSDInd = WHERE(fullSDInd eq -1L, addCount)
              for nsdi = 0L, addCount - 1L do begin
                  if (nsdi eq 0L) then begin
                      nsd = REPLICATE({obj_id: 0L, $
                                       station_id: '', $
                                       station_name: '', $
                                       station_type: '', $
                                       longitude: 0.0D, $
                                       latitude: 0.0D, $
                                       elevation: 0L, $
                                       use: 1}, $
                                      addCount)
                  endif
                  pri = addPRToSDInd[nsdi]
                  nsd[nsdi].obj_id = precipReport[pri].obj_id
                  nsd[nsdi].station_id = precipReport[pri].station_id
                  nsd[nsdi].station_name = precipReport[pri].station_name
                  nsd[nsdi].station_type = precipReport[pri].station_type
                  nsd[nsdi].longitude = precipReport[pri].longitude
                  nsd[nsdi].latitude = precipReport[pri].latitude
                  nsd[nsdi].elevation = precipReport[pri].elevation
              endfor

              if (addCount gt 0) then begin
                  stationData = [stationData, nsd]
                  PRINT, 'Station count increased from ' + $
                         STRCRA(numStations) + ' to ' + $
                         STRCRA(numStations + addCount)
              endif else begin
                  USR_MSG, 'All stations for ' + $
                           eventFinishDate_YYYYMMDDHH + $
                           ' were previous reporters.'
              endelse

          endif else addCount = 0 ; else for (numPrecipReports gt 0)

          newNumStations = numStations + addCount
          USR_MSG, 'New # stations = ' + STRCRA(newNumStations) + $
                   ' (added ' + STRCRA(addCount) + ')'

      endelse

;+
;     Expand precipObs_Meters to include new reports for existing
;     stations at the current time (section B of the diagram below)
;     and new stations (section "D" of the diagram below).
;
;          | <------- newNumStations -------> |
;          | <-- numStations ---> | addCount  |
;           ----------------------------------
;       0  | ooo...          ...o | xxxxxxxxx |
;       1  | .        (A)       . | xxxxxxxxx |
;       2  | .                  . | xx     xx |
;       .  | . precipObs_Meters . | xx (C) xx |
;       .  | .                  . | xx     xx |
;       .  | .                  . | xxxxxxxxx |
;     ec-1 | ooo...          ...o | xxxxxxxxx |
;          |----------------------|-----------|
;       ec | ooxxxoxx (B) xxoxxxo | oo (D) oo |
;           ----------------------------------
;-

EXPAND_PRECIP_OBS_ARRAY:

;+
;     Define new array of stations x events data (this fills diagram
;     section (C) with no-data values by default.
;-
      newPrecipObs_Meters = $
          MAKE_ARRAY(newNumStations, ec + 1, VALUE = ndv)

;+
;     Fill diagram section (A) (existing data)
;-
      newPrecipObs_Meters[0:numStations - 1, 0:ec - 1] = precipObs_Meters

;+
;     Fill diagram section (B) (new data at subset of existing
;     stations).
;-
      if ISA(fullSDInd) then begin

          ind = WHERE(fullSDInd ne -1L, count)

          if (count gt 0) then begin
              newPrecipObs_Meters[fullSDInd[ind], ec] = $
                  precipReport[ind].value_meters
          endif
          
      endif

;+
;     Fill diagram section (D) (new data at new stations).
;-
      if ISA(addPRToSDInd) then begin

;          if (addCount eq 0) then STOP ; PROGRAMMING CHECK
          if (addCount gt 0) then $
              newPrecipObs_Meters[numStations:newNumStations - 1, ec] = $
              precipReport[addPRToSDInd].value_meters

      endif

;+
;     Update variables, promoting all data to section (A)
;-
      precipObs_Meters = TEMPORARY(newPrecipObs_Meters)
      numStations = TEMPORARY(newNumStations)

  endfor

;+
; Eliminate stations outside the desired domain, if necessary.
;-
  if NOT(fullDomain) then begin

      if (boxOrShape eq 1) then begin ; box

          USR_MSG, 'Locating stations in "' + domainName + $
                   '" rectangle.'

          ind = WHERE((stationData.longitude gt subMinLon) and $
                      (stationData.longitude lt subMaxLon) and $
                      (stationData.latitude gt subMinLat) and $
                      (stationData.latitude lt subMaxLat), $
                      count)

      endif else begin                ; shapefile

          USR_MSG, 'Locating stations in "' + shapeID + '" polygon/s.'

          inShape = POINTS_IN_SHAPE(stationData.longitude, $
                                    stationData.latitude, $
                                    shapeFilePath, $
                                    shapeID, $
                                    shapeIDField, $
                                    /ECHO, $
                                    /CHECK_ALL_POLYGONS)
          ind = WHERE(inShape, count)

      endelse

      if (count eq 0) then begin
          USR_MSG, 'No data for stations in "' + domainName + '".'
          STOP
      endif

      PRINT, STRCRA(count) + ' of ' + STRCRA(numStations) + $
             ' stations fall in "' + domainName + '".'

      if (count lt numStations) then begin
          numStations = count
          precipObs_Meters = precipObs_Meters[ind, *]
          stationData = stationData[ind]
      endif

  endif

;+
; Count CoCoRaHS.
;-
  ind = WHERE(stationData.station_type eq 'COCORAHS', count)
  USR_MSG, 'Before eliminating all but the best reporters, data set ' + $
           'includes ' + STRCRA(count) + ' CoCoRaHS sites.'

;+
; Keep only the most consistent reporters.
;-
  numOK = (precipObs_Meters ne ndv) # REPLICATE(1, numEvents)
  ind = WHERE(numOK ge ROUND(reportingThreshold * numEvents), count)
  if (count eq 0) then begin
      USR_MSG, 'No station reported for ' + $
               STRCRA(ROUND(reportingThreshold * numEvents)) + $
               ' or more of the ' + STRCRA(numEvents) + ' ' + $
               STRCRA(accEventHours) + '-hour events covered by ' + $
               'the analysis period.'
      STOP
  endif else begin
      PRINT, STRCRA(count) + ' of ' + STRCRA(numStations) + $
             ' stations reported for ' + $
               STRCRA(ROUND(reportingThreshold * numEvents)) + $
               ' or more of the ' + STRCRA(numEvents) + ' ' + $
               STRCRA(accEventHours) + '-hour events covered by ' + $
               'the analysis period.'
      numStations = count
      precipObs_Meters = precipObs_Meters[ind, *]
      stationData = stationData[ind]
  endelse
  reportingFreq = FLOAT(numOK[ind]) / FLOAT(numEvents)

;+
; Create and eliminate sites on a blacklist.
;-
  blacklistID = ['BTLW4']
  blacklistCount = N_ELEMENTS(blacklistID)
  blacklistOutCount = 0L
  for sc = 0, N_ELEMENTS(blacklistiD) - 1 do begin
      ind = WHERE(stationData.station_id eq blacklistID[sc], count)
      if (count eq 0) then CONTINUE
      if (count ne 1) then STOP
      stationData[ind[0]].use = 0
      blacklistOutCount++
  endfor
  if (blacklistOutCount gt 0) then $
      PRINT, 'Removing ' + STRCRA(blacklistOutCount) + $
             ' of ' + $
             STRCRA(blacklistCount) + $
             ' blacklisted sites from observations.'

  ind = WHERE(stationData.use eq 1, count)
  numStations = count
  precipObs_Meters = precipObs_Meters[ind, *]
  stationData = stationData[ind]
  reportingFreq = reportingFreq[ind]

;+
; Keep only those reporters at locations where HRRR is significant, if
; checkHRRRWeight is set.
;-
  if checkHRRRWeight then begin

      if (ISA(NWMGridInfo) and $
          (NOT(ISA(iNWM)) or NOT(ISA(jNWM)))) then begin

;+
;         Calculate station locations on the NWM grid.
;-
  ;; if (count eq 0) then STOP
  ;; numStations = count
  ;; precipObs_Meters = precipObs_Meters[ind, *]
  ;; stationData = stationData[ind]
  ;; ind = WHERE(stationData.station_type eq 'COCORAHS', count)

;+
;         Get Snyder parameters for HRRR grid/projection.
;-
          LCC_GRIB2_TO_SNYDER, NWMGridInfo.latSec1, $
                               NWMGridInfo.latSec2, $
                               NWMGridInfo.latD, $
                               NWMGridInfo.lonV, $
                               NWMGridInfo.lat00, $
                               NWMGridInfo.lon00, $
                               NWMGridInfo.eRadM, $
                               lonV_rad, $
                               nSny, $
                               FSny, $
                               rho0, $
                               x00, $
                               y00

          lon_rad = stationData.longitude * degToRad
          lat_rad = stationData.latitude * degToRad

          rho = NWMGridInfo.eRadM * FSny / $
                (TAN(!DPi / 4.0D + lat_rad / 2.0D))^nSny ; Synder 15-1
          theta_rad = nSny * (lon_rad - lonV_rad)

          lon_rad = !NULL
          lat_rad = !NULL

          x = rho * SIN(theta_rad)            ; x location on HRRR grid
          y = rho0 - rho * COS(theta_rad)     ; y location on HRRR grid

          rho = !NULL
          theta_rad = !NULL

          iNWM = (x - x00) / NWMGridInfo.dx
          jNWM = (y - y00) / NWMGridInfo.dy

          x = !NULL
          y = !NULL

      endif

;+
;     Eliminate stations where HRRR is not significant for NWM v1.2.
;-
      aveHRRRWt = MAKE_ARRAY(numStations, VALUE = ndv)
      ind = WHERE((iNWM ge 0L) and (iNWM lt NWMGridInfo.nCols) and $
                  (jNWM ge 0L) and (jNWM lt NWMGridInfo.nRows), $
                  count)
      if (count gt 0) then $
          aveHRRRWt[ind] = aveHRRRWtGrid[iNWM[ind], jNWM[ind]]

      ind = WHERE((aveHRRRWt ne ndv) and $
                  (aveHRRRWt gt 0.0), count)

      if (count eq 0) then begin
          USR_MSG, 'No stations exist where HRRR is significant.'
          STOP
      endif else begin
          PRINT, STRCRA(count) + ' of ' + STRCRA(numStations) + $
                 ' stations occur at locations where' + $
                 ' HRRR is significant.'
          numStations = count
          precipObs_Meters = precipObs_Meters[ind, *]
          stationData = stationData[ind]
          reportingFreq = reportingFreq[ind]
      endelse

  endif

;+
; Keep only reporters at locations where the average daily temperature
; is below a statistically-determined threshold and the minimum daily
; temperature is below freezing.
;-
  if coldSeason then begin

      maxMissingTmpHours = ROUND(0.125 * accEventHours)

      for ec = 0L, numEvents - 1L do begin

          eventStartDate_Julian = startDate_Julian + $
                                  DOUBLE(ec * accEventHours) / 24.0D
          eventFinishDate_Julian = eventStartDate_Julian + $
                                   DOUBLE(accEventHours) / 24.0D
          eventFinishDate_YYYYMMDDHH = $
              JULIAN_TO_YYYYMMDDHH(eventFinishDate_Julian)

          aveTmpThreshold = $
              GET_MAX_AVE_DAILY_TEMP_FOR_SNOW(eventFinishDate_YYYYMMDDHH)

          minTmpGrid = !NULL
          maxTmpGrid = !NULL
          aveTmpGrid = !NULL

          GET_MIN_MAX_AVE_RTMA184_2M_TEMP, $
              eventFinishDate_YYYYMMDDHH, $
              accEventHours, $
              maxMissingTmpHours, $
              RTMADir, $
              scratchDir, $
              ndv, $
              minTmpGrid, $
              maxTmpGrid, $
              aveTmpGrid, $
              perfect, $
              RTMA184_GRID_PROJ_INFO = RTMA184GridInfo

          if (NOT(ISA(aveTmpGrid)) or NOT(ISA(minTmpGrid))) then begin

              PRINT, 'WARNING: failed to generate min/ave temperature ' + $
                     'data for ' + eventFinishDate_YYYYMMDDHH + $
                     '; eliminating all observations for this date.'
              precipObs_Meters[*, ec] = ndv

              CONTINUE

          endif

          if (NOT(ISA(iRTMA184)) or NOT(ISA(jRTMA184))) then begin

;+
;             Get Snyder parameters for 2.5 km RTMA grid (184).
;-
              LCC_GRIB2_TO_SNYDER, RTMA184GridInfo.latSec1, $
                                   RTMA184GridInfo.latSec2, $
                                   RTMA184GridInfo.latD, $
                                   RTMA184GridInfo.lonV, $
                                   RTMA184GridInfo.lat00, $
                                   RTMA184GridInfo.lon00, $
                                   RTMA184GridInfo.eRadM, $
                                   lonV_rad, $
                                   nSny, $
                                   FSny, $
                                   rho0, $
                                   x00, $
                                   y00

              lon_rad = stationData.longitude * degToRad
              lat_rad = stationData.latitude * degToRad

              rho = RTMA184GridInfo.eRadM * FSny / $
                    (TAN(!DPi / 4.0D + lat_rad / 2.0D))^nSny ; Synder 15-1
              theta_rad = nSny * (lon_rad - lonV_rad)

              lon_rad = !NULL
              lat_rad = !NULL

              x = rho * SIN(theta_rad)    ; x location on HRRR grid
              y = rho0 - rho * COS(theta_rad) ; y location on HRRR grid

              rho = !NULL
              theta_rad = !NULL

              iRTMA184 = (x - x00) / RTMA184GridInfo.dx
              jRTMA184 = (y - y00) / RTMA184GridInfo.dy

              x = !NULL
              y = !NULL

          endif

;+
;         Set observations to the no-data value where it is too warm
;         for snow to be likely.
;-
          minTmp = REGRID_BILIN(minTmpGrid, iRTMA184, jRTMA184, ndv)
          aveTmp = REGRID_BILIN(aveTmpGrid, iRTMA184, jRTMA184, ndv)
          noSnowInd = WHERE((aveTmp eq ndv) or $
                            (minTmp eq ndv) or $
                            ((((aveTmp - 273.15) ge aveTmpThreshold) or $
                              ((minTmp - 273.15) ge 0.0)) and $
                             (precipObs_Meters[*, ec] ne ndv)), $
                            count)
          if (count gt 0) then begin
              precipObs_Meters[noSnowInd, ec] = ndv
              PRINT, 'Event ', STRCRA(ec) + $
                     ' of ' + STRCRA(numEvents - 1) + $
                     ' (' + eventFinishDate_YYYYMMDDHH + '): ' + $
                     'eliminated ' + STRCRA(count) + ' non-cold-season obs.'
          endif

      endfor

  endif

;+
; Do a breakdown of station type.
;-
  PRINT, 'Breakdown of main station types:'

;+
; Any station with "ASOS" in its station type is classified as an
; ASOS.
;-
  isASOS = STREGEX(stationData.station_type, 'ASOS', /BOOLEAN)
  PRINT, '  ASOS: ' + STRCRA(TOTAL(isASOS, /INT)) + $
         ' (' + STRCRA(TOTAL(isASOS) / numStations * 100) + '%)'

;+
; Any remaining station with "COOP" in its station type is classified
; as a COOP.
;-
  isCOOP = (isASOS eq 0) * $
           STREGEX(stationData.station_type, 'COOP', /BOOLEAN)
  PRINT, '  COOP: ' + STRCRA(TOTAL(isCOOP, /INT)) + $
         ' (' + STRCRA(TOTAL(isCOOP) / numStations * 100) + '%)'

;+
; Any remaining station with "GOES" in its station type that is also
; in a separate list of HADS stations is classified as HADS.
;-
  isGOES = (isASOS eq 0) * (isCOOP eq 0) * $
           STREGEX(stationData.station_type, 'GOES', /BOOLEAN)
  isHADS = MAKE_ARRAY(numStations, VALUE = 0B)
  if FILE_TEST('HADS.id') then begin
      ind = WHERE(isGOES eq 1, count)
      for tc = 0, count - 1 do begin
          stationID = stationData[ind[tc]].station_id
          cmd = 'grep  -q "^' + stationID + '$" HADS.id'
          SPAWN, cmd, EXIT_STATUS = status
          if (status eq 0) then isHADS[ind[tc]] = 1
      endfor
      PRINT, '  HADS: ' + STRCRA(TOTAL(isHADS, /INT)) + $
             ' (' + STRCRA(TOTAL(isHADS) / numStations * 100) + '%)'
  endif

;+
; Break down remaining types in descending order of prominence.
;-
  ind = WHERE((isCOOP eq 0) and (isASOS eq 0) and (isHADS eq 0), count)
  other = count

  if (count gt 0) then begin
      nonCoopType = stationData[ind].station_type
      nonCoopType = nonCoopType[SORT(nonCoopType)]
      nonCoopType = nonCoopType[UNIQ(nonCoopType)]
      numNonCoopTypes = N_ELEMENTS(nonCoopType)
      nonCoopCount = LONARR(numNonCoopTypes)
      for tc = 0, numNonCoopTypes - 1 do $
          nonCoopCount[tc] = $
              N_ELEMENTS(WHERE(stationData[ind].station_type eq $
                               nonCoopType[tc]))
      order = REVERSE(SORT(nonCoopCount))
      nonCoopType = nonCoopType[order]
      nonCoopCount = nonCoopCount[order]
      for tc = 0, numNonCoopTypes - 1 do begin
          isThisType = stationData[ind].station_type eq nonCoopType[tc]
          percent = TOTAL(isThisType) / numStations * 100
          if (percent lt 1) then BREAK
          other = other - TOTAL(isThisType, /INT)
          PRINT, '  ' + nonCoopType[tc] + ': ' + $
                 STRCRA(TOTAL(isThisType, /INT)) + $
                 ' (' + STRCRA(TOTAL(isThisType) / numStations * 100) + '%)'
      endfor
      if (tc lt numNonCoopTypes) then begin ; we broke
          PRINT, '  OTHER: ' + STRCRA(other) + $
                 ' (' + STRCRA(FLOAT(other) / numStations * 100) + '%)'
      endif
  endif

;+
; Convert observation data from meters to millimeters.
;-
  ind = WHERE(precipObs_Meters eq ndv, count)
  precipObs_mm = TEMPORARY(precipObs_Meters) * 1000.0
  if (count gt 0) then precipObs_mm[ind] = ndv

;+
; Make a rough plot of station locations with colors to indicate
; reporting frequency.
;-
  title = domainName + ' Precipitation Reporters!C!DMinimum ' + $
          FORMAT_FLOAT(reportingThreshold * 100) + '% reporting frequency, ' + $
          timeDomainStr + '; ' + STRCRA(numStations) + ' sites'
  pEdges = FINDGEN(N_ELEMENTS(pRed) + 1)
  PNGImage = domainAbbrev + '_stations_' + $
             FORMAT_FLOAT(reportingThreshold * 100) + '_percent_' + $
             timeDomainAbbrev + '.png'
  PNGImage = outputDir + '/' + PNGImage
  dummyGrid = MAKE_ARRAY(plotGridInfo.nCols, plotGridInfo.nRows, VALUE = ndv)
  MAKE_LCC_MAP_PNG, dummyGrid, ndv, $
                    fEdges, fRed, fGrn, fBlu, $
                    plotGridInfo, $
                    title, $
                    'Reporting Frequency (% of Days)', $
                    PNGImage, $
                    POINT_LON = stationData.longitude, $
                    POINT_LAT = stationData.latitude, $
                    POINT_VAL = reportingFreq * 100.0, $
                    TARGET_IMAGE_SIZE = 1000, $
                    MAP_SHAPE_PATH = shapePathList, $
                    POINT_BIN_EDGES = FEdges, $
                    POINT_RED = fRed, $
                    POINT_GRN = fGrn, $
                    POINT_BLU = fBlu, $
                    /NO_GRID, $
                    /NO_CONTINENTS, $
                    /NO_USA, $
                    /BLACK_ON_WHITE
  PRINT, 'Plotted stations to ' + PNGImage

  ;; mapAspect = (maxLon - minLon) / (maxLat - minLat)
  ;; xSize = 1000
  ;; ySize = ROUND(xSize / mapAspect)
  ;; WSET_OR_WINDOW, 0, XSIZE = xSize, YSIZE = ySize
  ;; PLOT, stationData.longitude, stationData.latitude, $
  ;;       XRANGE = [minLon, maxLon], XSTYLE = 1, $
  ;;       YRANGE = [minLat, maxLat], YSTYLE = 1, $
  ;;       /NODATA
  ;; OPLOT, stationData.longitude, stationData.latitude, $
  ;;        PSYM = 4, COLOR = 100
  ;; if (count gt 0) then begin ; CoCoRaHS
  ;;     if (count eq 1) then ind = [ind[0], ind[0]]
  ;;     OPLOT, stationData[ind].longitude, stationData[ind].latitude, $
  ;;            PSYM = 4, SYMSIZE = 2
  ;; endif

;+
; Based on how many values of each score you want, decide how to
; aggregate multiple events, if necessary. To roughly achieve the
; targetXCount (this is the number of values along the x axis in a
; time series of skill measures across the analysis period), you would
; collect groups of events. For example, if the analysis period
; were 300 events (usually events = days) long, but targetXCount =
; 100, then groups of 3 events (usually 3 days of data at a time)
; would be collected, and skill measures would be calculated for those
; collections and associated with the mean of the times attributed to
; the data. The result would be 100 skill measures, even though data
; was gathered for 300 events. If eventAggFactor is 1, then
; numAggEvents = numEvents and there is no collecting of multiple
; events.
;-
  timeAxis = startDate_Julian + $
             (1.0D + DINDGEN(numEvents)) * DOUBLE(accEventHours) / 24.0D

;  targetXCount = 360 ; 3 times a month for 10 years...
;  targetXCount = 132 ; 
;  targetXCount = 100
;  targetXCount = 5
;  targetXCount = 16
;  targetXCount = 122 ; daily for a year
;  eventAggFactor = CEIL(FLOAT(numEvents) / FLOAT(targetXCount)) > 1

;  eventAggFactor = 3
  numAggEvents = numEvents / eventAggFactor ; integer division!
  numInputEvents = numAggEvents * eventAggFactor
  aggTimeAxis = REBIN(timeAxis[0:numInputEvents - 1], numAggEvents)

;+
; Initialize skill/performance arrays:
;   0. Arrays of gridded inputs sampled at station locations.
;   1. Contingency table counts:
;      a. Hits,
;      b. False positives (type I errors),
;      c. Misses (type II errors),
;      d. Correct negatives;
;   2. Total absolute relative error (total_ARE) for above-threshold
;      results (hits).
;   3. Total of log multiplicative bias (total_LMB) for hits.
;   4. Total of squared log multiplicative bias (total_LMB2) for
;      hits.
;   ~ Items 2-4 are used to calculate: ~
;   6. Mean absolute relative error (MARE) for hits;
;   7. Geometric mean multiplicative bias (GMMB) for hits.
;   8. Standard deviation in log multiplicative bias (SDLMB) for
;      hits.
;   9. Heidke Skill Score (HSS). The HSS measures the fractional improvement 
;      of the forecast over the standard forecast.  
;      HSS = 2*(a*d - b*c)/[(a+c)(c+d) + (a+b)(b+d)]
;      Ref: http://www.eumetrain.org/data/4/451/english/msg/ver_categ_forec/uos3/uos3_ko1.htm
;  10. Threat score (critical success index) (CSI). Measures the fraction of 
;      observed and/or forecast events that were correctly predicted.
;      CSI = a/(a+b+c)  -- Ref: http://www.cawcr.gov.au/projects/verification/verif_web_page.html
;-
  ndvInt = -9999

  obsACCount = MAKE_ARRAY(numEvents, VALUE = ndvInt) ; observed events
  obsBDCount = MAKE_ARRAY(numEvents, VALUE = ndvInt) ; observed non-events

  for i = 0, N_ELEMENTS(inputFlag)-1 do begin
      if (inputFlag[i] eq 1) then begin
         var_stations = inputFlagName[i] + '_stations'
         (SCOPE_VARFETCH(var_stations, /ENTER, LEVEL = 1)) = $
            MAKE_ARRAY(numStations, numEvents, VALUE = ndv)
         var_a = 'a_' + inputFlagName[i]
         (SCOPE_VARFETCH(var_a, /ENTER, LEVEL = 1)) = $ 
             MAKE_ARRAY(numEvents, VALUE = ndvInt)
         var_b = 'b_' + inputFlagName[i]
         (SCOPE_VARFETCH(var_b, /ENTER, LEVEL = 1)) = $ 
             MAKE_ARRAY(numEvents, VALUE = ndvInt)
         var_c = 'c_' + inputFlagName[i]
         (SCOPE_VARFETCH(var_c, /ENTER, LEVEL = 1)) = $ 
             MAKE_ARRAY(numEvents, VALUE = ndvInt)
         var_d = 'd_' + inputFlagName[i]
         (SCOPE_VARFETCH(var_d, /ENTER, LEVEL = 1)) = $ 
             MAKE_ARRAY(numEvents, VALUE = ndvInt)
         var_totARE = 'total_ARE_' + inputFlagName[i]
         (SCOPE_VARFETCH(var_totARE, /ENTER, LEVEL = 1)) = $ 
             MAKE_ARRAY(numEvents, VALUE = ndv)
         var_totLMB = 'total_LMB_' + inputFlagName[i]
         (SCOPE_VARFETCH(var_totLMB, /ENTER, LEVEL = 1)) = $ 
             MAKE_ARRAY(numEvents, VALUE = ndv)
         var_totLMB2 = 'total_LMB2_' + inputFlagName[i]
         (SCOPE_VARFETCH(var_totLMB2, /ENTER, LEVEL = 1)) = $ 
             MAKE_ARRAY(numEvents, VALUE = ndv)
         var_MARE = 'MARE_' + inputFlagName[i]
         (SCOPE_VARFETCH(var_MARE, /ENTER, LEVEL = 1)) = $ 
             MAKE_ARRAY(numAggEvents, VALUE = ndv)
         var_GMMB = 'GMMB_' + inputFlagName[i]
         (SCOPE_VARFETCH(var_GMMB, /ENTER, LEVEL = 1)) = $ 
             MAKE_ARRAY(numAggEvents, VALUE = ndv)
         var_SDLMB = 'SDLMB_' + inputFlagName[i]
         (SCOPE_VARFETCH(var_SDLMB, /ENTER, LEVEL = 1)) = $ 
             MAKE_ARRAY(numAggEvents, VALUE = ndv)
         var_POD = 'POD_' + inputFlagName[i]
         (SCOPE_VARFETCH(var_POD, /ENTER, LEVEL = 1)) = $ 
             MAKE_ARRAY(numAggEvents, VALUE = ndv)
         var_FAR = 'FAR_' + inputFlagName[i]
         (SCOPE_VARFETCH(var_FAR, /ENTER, LEVEL = 1)) = $ 
             MAKE_ARRAY(numAggEvents, VALUE = ndv)
         var_PCorr = 'PCorr_' + inputFlagName[i]
         (SCOPE_VARFETCH(var_PCorr, /ENTER, LEVEL = 1)) = $
             MAKE_ARRAY(numAggEvents, VALUE = ndv)
         var_RCorr = 'RCorr_' + inputFlagName[i]
         (SCOPE_VARFETCH(var_RCorr, /ENTER, LEVEL = 1)) = $
             MAKE_ARRAY(numAggEvents, VALUE = ndv)
         var_HSS = 'HSS_' + inputFlagName[i]
         (SCOPE_VARFETCH(var_HSS, /ENTER, LEVEL = 1)) = $ 
             MAKE_ARRAY(numAggEvents, VALUE = ndv)
         var_CSI = 'CSI_' + inputFlagName[i]
         (SCOPE_VARFETCH(var_CSI, /ENTER, LEVEL = 1)) = $ 
             MAKE_ARRAY(numAggEvents, VALUE = ndv)

      endif
  endfor

;+
; Loop over events again, gathering and resampling gridded
; precipitation to compare with observations.
;-
  iHRAP = !NULL & jHRAP = !NULL
  iHRRR = !NULL & jHRRR = !NULL
  iMRMS = !NULL & jMRMS = !NULL
  iRAP = !NULL & jRAP = !NULL
  iNLDAS = !NULL & jNLDAS = !NULL
  iNWM = !NULL & jNWM = !NULL
  iAORC = !NULL & jAORC = !NULL
  iRTMA = !NULL & jRTMA = !NULL

  for ec = 0, numEvents - 1 do begin

      eventStartDate_Julian = startDate_Julian + $
                              DOUBLE(ec * accEventHours) / 24.0D
      eventFinishDate_Julian = eventStartDate_Julian + $
                               DOUBLE(accEventHours) / 24.0D
      eventFinishDate_YYYYMMDDHH = $
          JULIAN_TO_YYYYMMDDHH(eventFinishDate_Julian)

      if (eventFinishDate_YYYYMMDDHH eq '2016122112') then begin
          PRINT, 'Skipping ' + eventFinishDate_YYYYMMDDHH + $
                 ' due to poor data quality on this date.'
          CONTINUE
      endif

      PRINT, STRCRA(accEventHours) + ' hours ending ' + $
             eventFinishDate_YYYYMMDDHH

;+
;     Copy inputFlag into temporary variables. This way if something
;     goes wrong with any of them for this time step, they can
;     be "turned off" for the remainder of this step.
;-
      for i = 0, N_ELEMENTS(inputFlag)-1 do begin
          use_var = 'use_' + inputFlagName[i]
          (SCOPE_VARFETCH(use_var,/ENTER, level=1)) = inputFlag[i]
      endfor

;+
;     Extract precipitation observations for this time. Convert to mm
;     for consistency with gridded data.
;-
      eventPrecipObs_mm = precipObs_mm[*, ec]
;      ind = WHERE(eventPrecipObs_mm eq ndv, count)
;      eventPrecipObs_mm = eventPrecipObs_mm * 1000.0
;      if (count gt 0) then eventPrecipObs_mm[ind] = ndv

;+
;     Some BASIC quality control, inventorying and at times
;     eliminating high reports to indicate possible QC
;     problems. Eliminating erroneous zero reports is harder and we do
;     not attempt that here.
;-

;+
;     Any report exceeding the "noWay" threshold will be
;     set to a no-data value. Naturally we need to be careful with
;     this measure (e.g. hurricane Harvey in 2017).
;-
      noWay = 508.0             ; 508 mm = 20 in

      ind = WHERE((eventPrecipObs_mm ne ndv) and $
                  (eventPrecipObs_mm gt noWay), count)
      if (count gt 0) then begin
          PRINT, '  Eliminating ' + STRCRA(count) + $
                 ' reports > ' + $
                 STRCRA(noWay) + ' mm (' + $
                 STRCRA(noWay / 25.4) + ' in) ' + $
                 'because no way:'
          PRINT, '  Station ID | type | value:'
          PRINT, '  ' + stationData[ind].station_id + ' | ' + $
                 stationData[ind].station_type + ' | ' + $
                 STRCRA(eventPrecipObs_mm[ind])
          eventPrecipObs_mm[ind] = ndv
      endif

;+
;     If fewer than highGroupMinCount stations report amounts
;     exceeding highThreshold_mm, then they are assumed to be bogus
;     reports and are set to no-data values.
;-
      highThreshold_mm = 254.0  ; 254 mm = 10 in
      highGroupMinCount = 10

      ind = WHERE((eventPrecipObs_mm ne ndv) and $
                  (eventPrecipObs_mm gt highThreshold_mm), count)
      if (count gt 0) then begin
          PRINT, '  # stations reporting precip > ' + $
                 STRCRA(highThreshold_mm) + ' mm (' + $
                 STRCRA(highThreshold_mm / 25.4) + ' in): ' + $
                 STRCRA(count)
          i1 = 0
          i2 = (count - 1) < 9
          PRINT, '  Station ID | type | value:'
          PRINT, '  ' + stationData[ind[i1:i2]].station_id + ' | ' + $
                 stationData[ind[i1:i2]].station_type + ' | ' + $
                 STRCRA(eventPrecipObs_mm[ind[i1:i2]])
          if (count lt highGroupMinCount) then begin
              PRINT, '  Eliminating ' + STRCRA(count) + $
                     ' reports > ' + $
                     STRCRA(highThreshold_mm) + ' mm (' + $
                     STRCRA(highThreshold_mm / 25.4) + ' in); ' + $
                     'need at least ' + $
                     STRCRA(highGroupMinCount) + $
                     ' to form a consensus.'
              eventPrecipObs_mm[ind] = ndv
          endif
      endif

      ind = WHERE((eventPrecipObs_mm ne ndv) and $
                  (eventPrecipObs_mm le eventThreshold_mm), $
                  obsNonEventCount)

      ind = WHERE((eventPrecipObs_mm ne ndv) and $
                  (eventPrecipObs_mm gt eventThreshold_mm), $
                  obsEventCount)

      PRINT, '  Observed events (A+C): ' + STRCRA(obsEventCount)
      PRINT, '           non-events (B+D): ' + STRCRA(obsNonEventCount)

      obsACCount[ec] = obsEventCount
      obsBDCount[ec] = obsNonEventCount

;+
;     Read operational (EMC) MPE data, hourly
;-
      if use_EMC_MPE_1 then begin

          EMC_MPE_1_grid = !NULL

          for hc = 0, accEventHours - 1 do begin

              hourFinishDate_Julian = $
                  eventFinishDate_Julian - $
                  DOUBLE(accEventHours - 1 - hc) / 24.0D

              hourFinishDate_YYYYMMDDHH = $
                  JULIAN_TO_YYYYMMDDHH(hourFinishDate_Julian)

              hourlyGrid = !NULL

              GET_STAGE4_QPE, hourFinishDate_YYYYMMDDHH, $
                              1, $
                              st4Dir, $
                              scratchDir, $
                              ndv, $
                              hourlyGrid, $
                              HRAP_GRID_PROJ_INFO = HRAPGridInfo

              if NOT(ISA(hourlyGrid)) then begin
                  USR_MSG, 'No 1-hour EMC MPE data for ' + $
                           hourFinishDate_YYYYMMDDHH
                  use_EMC_MPE_1 = 0
                  BREAK
              endif

              if (hc eq 0) then begin
                  EMC_MPE_1_Grid = hourlyGrid
              endif else begin
                  ind = WHERE((EMC_MPE_1_Grid eq ndv) or $
                              (hourlyGrid eq ndv), count)
                  EMC_MPE_1_Grid = EMC_MPE_1_Grid + hourlyGrid
                  if (count gt 0) then EMC_MPE_1_Grid[ind] = ndv
              endelse

          endfor

          hourlyGrid = !NULL

      endif

;+
;     Read operational (EMC) MPE data, 6-hourly
;-
      if use_EMC_MPE_6 then begin

          EMC_MPE_6_grid = !NULL

          numCycles = accEventHours / 6

          for cc = 0, numCycles - 1 do begin

              cycleFinishDate_Julian = $
                  eventFinishDate_Julian - $
                  DOUBLE(numCycles - 1 - cc) * 6.0D / 24.0D

              cycleFinishDate_YYYYMMDDHH = $
                  JULIAN_TO_YYYYMMDDHH(cycleFinishDate_Julian)

              cycleGrid = !NULL
              GET_STAGE4_QPE, cycleFinishDate_YYYYMMDDHH, $
                              6, $
                              st4Dir, $
                              scratchDir, $
                              ndv, $
                              cycleGrid, $
                              HRAP_GRID_PROJ_INFO = HRAPGridInfo

              if NOT(ISA(cycleGrid)) then begin
                  USR_MSG, 'No 6-hour EMC MPE data for ' + $
                           cycleFinishDate_YYYYMMDDHH
                  use_EMC_MPE_6 = 0
                  BREAK
              endif

              if (cc eq 0) then begin
                  EMC_MPE_6_Grid = cycleGrid
              endif else begin
                  ind = WHERE((EMC_MPE_6_Grid eq ndv) or $
                              (cycleGrid eq ndv), count)
                  EMC_MPE_6_Grid = EMC_MPE_6_Grid + cycleGrid
                  if (count gt 0) then EMC_MPE_6_Grid[ind] = ndv
              endelse

          endfor

          cycleGrid = !NULL

      endif

;+
;     Read operational (EMC) MPE data, 24-hourly
;-
      if use_EMC_MPE_24 then begin

          if (accEventHours ne 24) then STOP ; NOT SUPPORTED

          EMC_MPE_24_grid = !NULL

          GET_STAGE4_QPE, eventFinishDate_YYYYMMDDHH, $
                          accEventHours, $
                          st4Dir, $
                          scratchDir, $
                          ndv, $
                          EMC_MPE_24_Grid, $
                          HRAP_GRID_PROJ_INFO = HRAPGridInfo

          if NOT(ISA(EMC_MPE_24_grid)) then begin
              USR_MSG, 'No 24-hour EMC MPE data for ' + $
                       eventFinishDate_YYYYMMDDHH
              use_EMC_MPE_24 = 0
          endif

      endif

;+
;     Read OWP MPE data.
;-
      if use_OWP_MPE then begin

          OWP_MPE_grid = !NULL

          GET_ACC_HOURLY_OWP_MPE, $
              eventFinishDate_YYYYMMDDHH, $
              accEventHours, $
              OWP_MPE_fileTemplate, $
              MPEDir, $
              scratchDir, $
              ndv, $
              OWP_MPE_grid, $
              HRAP_GRID_PROJ_INFO = HRAPGridInfo

          if NOT(ISA(OWP_MPE_grid)) then begin
              USR_MSG, 'No OWP MPE data for ' + eventFinishDate_YYYYMMDDHH
              use_OWP_MPE = 0
          endif

      endif

;;       if (use_EMC_MPE_6 and use_OWP_MPE) then begin

;; ;+
;; ;         Explore differences between 6-hour EMC MPE and OWP MPE,
;; ;         which theoretically should not differ.
;; ;-
;;           EMCInd = WHERE(EMC_MPE_6_grid ne ndv, EMCCount)
;;           PRINT, '*** EMC good-data count: ' + EMCCount
;;           OWPInd = where(OWP_MPE_grid ne ndv, OWPCount)
;;           PRINT, '*** OWP good-data count: ' + OWPCount
;;           if (OWPCount ne EMCCount) then $
;;               MESSAGE, '*** Compare OWP and EMC (no-data counts differ)'
;;           if (MAX(ABS(EMCInd - OWPInd)) gt 0) then $
;;               MESSAGE, '*** Compare OWP AND EMC (no-data indices differ)'
;;           EMCInd = !NULL
;;           OWPInd = !NULL
;;           ind = WHERE((EMC_MPE_6_grid ne ndv) and $
;;                       (OWP_MPE_grid ne ndv), count)
;;           if (MAX(ABS(EMC_MPE_6_grid[ind] - OWP_MPE_grid[ind])) gt 1.0e-4) $
;;               then MESSAGE, '*** Compare OWP and EMC (grids differ)'

;;           PRINT, '*** Done comparing OWP vs. EMC grids for ' + $
;;                  eventFinishDate_YYYYMMDDHH

;;       endif

;+
;     Read HRRR f01 data.
;-
      if use_HRRR_f01 then begin

          HRRR_f01_WEASD_grid = !NULL
          HRRR_f01_grid = !NULL

          ;; GET_ACC_HRRR_APCP_WEASD, $
          ;;     eventFinishDate_YYYYMMDDHH, $
          ;;     accEventHours, $
          ;;     1, 2, 6, $
          ;;     HRRRDir, $
          ;;     scratchDir, $
          ;;     ndv, $
          ;;     HRRR_f01_WEASD_grid, $
          ;;     HRRR_f01_grid, $
          ;;     HRRR_GRID_PROJ_INFO = HRRRGridInfo

          GET_ACCUM_HRRR_LC_APCP_WEASD, $
              eventFinishDate_YYYYMMDDHH, $
              accEventHours, $
              1, 2, 3, $
              HRRRDir, $
              scratchDir, $
              ndv, $
              HRRR_f01_grid, $
              HRRR_f01_WEASD_grid, $
              perfect, $
              MAX_NUM_SUB_FCST_HOURS = accEventHours / 6, $
              HRRR_GRID_PROJ_INFO = HRRRGridInfo

          if NOT(ISA(HRRR_f01_grid)) then use_HRRR_f01 = 0
          HRRR_f01_WEASD_grid = !NULL

      endif

;+
;     Read HRRRX f01 data.
;-
      if use_HRRRX_f01 then begin

          HRRRX_f01_grid = !NULL

          GET_ACC_HRRRX_APCP, $
              eventFinishDate_YYYYMMDDHH, $
              accEventHours, $
              1, 2, 6, $
              HRRRXDir, $
              scratchDir, $
              ndv, $
              HRRRX_f01_grid, $
              HRRR_GRID_PROJ_INFO = HRRRGridInfo

          if NOT(ISA(HRRRX_f01_grid)) then use_HRRRX_f01 = 0

      endif

;+
;     Read HRRR f02 data.
;-
      if use_HRRR_f02 then begin

          HRRR_f02_WEASD_grid = !NULL
          HRRR_f02_grid = !NULL

          ;; GET_ACC_HRRR_APCP_WEASD, $
          ;;     eventFinishDate_YYYYMMDDHH, $
          ;;     accEventHours, $
          ;;     2, 1, 6, $
          ;;     HRRRDir, $
          ;;     scratchDir, $
          ;;     ndv, $
          ;;     HRRR_f02_WEASD_grid, $
          ;;     HRRR_f02_grid, $
          ;;     HRRR_GRID_PROJ_INFO = HRRRGridInfo

          GET_ACCUM_HRRR_LC_APCP_WEASD, $
              eventFinishDate_YYYYMMDDHH, $
              accEventHours, $
              2, 1, 3, $
              HRRRDir, $
              scratchDir, $
              ndv, $
              HRRR_f02_grid, $
              HRRR_f02_WEASD_grid, $
              perfect, $
              MAX_NUM_SUB_FCST_HOURS = accEventHours / 6, $
              HRRR_GRID_PROJ_INFO = HRRRGridInfo

          if NOT(ISA(HRRR_f02_grid)) then use_HRRR_f02 = 0
          HRRR_f02_WEASD_grid = !NULL

      endif

;+
;     Read HRRRX f02 data.
;-
      if use_HRRRX_f02 then begin

          HRRRX_f02_grid = !NULL

          GET_ACC_HRRRX_APCP, $
              eventFinishDate_YYYYMMDDHH, $
              accEventHours, $
              2, 1, 6, $
              HRRRXDir, $
              scratchDir, $
              ndv, $
              HRRRX_f02_grid, $
              HRRR_GRID_PROJ_INFO = HRRRGridInfo

          if NOT(ISA(HRRRX_f02_grid)) then use_HRRRX_f02 = 0

      endif

;+
;     Read HRRR f03 data.
;-
      if use_HRRR_f03 then begin

          HRRR_f03_WEASD_grid = !NULL
          HRRR_f03_grid = !NULL

          ;; GET_ACC_HRRR_APCP_WEASD, $
          ;;     eventFinishDate_YYYYMMDDHH, $
          ;;     accEventHours, $
          ;;     3, 1, 6, $
          ;;     HRRRDir, $
          ;;     scratchDir, $
          ;;     ndv, $
          ;;     HRRR_f03_WEASD_grid, $
          ;;     HRRR_f03_grid, $
          ;;     HRRR_GRID_PROJ_INFO = HRRRGridInfo

          GET_ACCUM_HRRR_LC_APCP_WEASD, $
              eventFinishDate_YYYYMMDDHH, $
              accEventHours, $
              3, 1, 2, $
              HRRRDir, $
              scratchDir, $
              ndv, $
              HRRR_f03_grid, $
              HRRR_f03_WEASD_grid, $
              perfect, $
              MAX_NUM_SUB_FCST_HOURS = accEventHours / 6, $
              HRRR_GRID_PROJ_INFO = HRRRGridInfo

          if NOT(ISA(HRRR_f03_grid)) then use_HRRR_f03 = 0
          HRRR_f03_WEASD_grid = !NULL

      endif

;+
;     Read HRRRX f03 data.
;-
      if use_HRRRX_f03 then begin

          HRRRX_f03_grid = !NULL

          GET_ACC_HRRRX_APCP, $
              eventFinishDate_YYYYMMDDHH, $
              accEventHours, $
              3, 1, 6, $
              HRRRXDir, $
              scratchDir, $
              ndv, $
              HRRRX_f03_grid, $
              HRRR_GRID_PROJ_INFO = HRRRGridInfo

          if NOT(ISA(HRRRX_f03_grid)) then use_HRRRX_f03 = 0

      endif


;+
;     Read MRMS radar-only data.
;-
      if use_MRMS_RO then begin

          MRMS_RO_grid = !NULL

          GET_ACC_HOURLY_MRMS_QPE, eventFinishDate_YYYYMMDDHH, $
                                   accEventHours, $
                                   MRMSDir, $
                                   scratchDir, $
                                   ndv, $
                                   MRMS_RO_grid, $
                                   MRMS_GRID_INFO = MRMSGridInfo, $
                                   /RADAR_ONLY

          if NOT(ISA(MRMS_RO_grid)) then use_MRMS_RO = 0

      endif

;+
;     Read MRMS gauge-corrected data.
;-
      if use_MRMS_GC then begin

          MRMS_GC_grid = !NULL

          GET_ACC_HOURLY_MRMS_QPE, eventFinishDate_YYYYMMDDHH, $
                                   accEventHours, $
                                   MRMSDir, $
                                   scratchDir, $
                                   ndv, $
                                   MRMS_GC_grid, $
                                   MRMS_GRID_INFO = MRMSGridInfo

          if NOT(ISA(MRMS_GC_grid)) then use_MRMS_GC = 0

      endif


;+
;     Read RAP f01 data.
;-
      if use_RAP_f01 then begin

          ;; RAP_f01_ACPCP_grid = !NULL
          ;; RAP_f01_NCPCP_grid = !NULL
          RAP_f01_grid = !NULL
          RAP_f01_WEASD_grid = !NULL

          ;; GET_ACC_RAP130_ACPCP_NCPCP_WEASD, $
          ;;     eventFinishDate_YYYYMMDDHH, $
          ;;     accEventHours, $
          ;;     1, 2, 3, $
          ;;     RAPDir, $
          ;;     scratchDir, $
          ;;     ndv, $
          ;;     RAP_f01_ACPCP_grid, $
          ;;     RAP_f01_NCPCP_grid, $
          ;;     RAP_f01_WEASD_grid, $
          ;;     RAP_GRID_PROJ_INFO = RAPGridInfo

          ;; if (NOT(ISA(RAP_f01_ACPCP_grid)) or $
          ;;     NOT(ISA(RAP_f01_NCPCP_grid))) then begin
          ;;     USR_MSG, 'No RAP f01 data for ' + eventFinishDate_YYYYMMDDHH
          ;;     use_RAP_f01 = 0
          ;; endif else begin
          ;;     ind = WHERE((RAP_f01_ACPCP_grid eq ndv) or $
          ;;             (RAP_f01_NCPCP_grid eq ndv), count)
          ;;     RAP_f01_grid = RAP_f01_ACPCP_grid + RAP_f01_NCPCP_grid
          ;;     if (count gt 0) then RAP_f01_grid[ind] = ndv
          ;;     RAP_f01_WEASD_grid = !NULL
          ;; endelse

          GET_ACCUM_RAP130_APCP_WEASD, $
              eventFinishDate_YYYYMMDDHH, $
              accEventHours, $
              1, 2, 3, $
              RAPDir, $
              scratchDir, $
              ndv, $
              RAP_f01_grid, $
              RAP_f01_WEASD_grid, $
              perfect, $
              MAX_NUM_SUB_FCST_HOURS = accEventHours / 6, $
              RAP_GRID_PROJ_INFO = RAPGridInfo

          if NOT(ISA(RAP_f01_grid)) then begin
              USR_MSG, 'No RAP f01 data for ' + eventFinishDate_YYYYMMDDHH
              use_RAP_f01 = 0
          endif

          RAP_f01_WEASD_grid = !NULL

      endif

;+
;     Read RAP f02 data.
;-
      if use_RAP_f02 then begin

          ;; RAP_f02_ACPCP_grid = !NULL
          ;; RAP_f02_NCPCP_grid = !NULL
          RAP_f02_grid = !NULL
          RAP_f02_WEASD_grid = !NULL

          ;; GET_ACC_RAP130_ACPCP_NCPCP_WEASD, $
          ;;     eventFinishDate_YYYYMMDDHH, $
          ;;     accEventHours, $
          ;;     2, 1, 3, $
          ;;     RAPDir, $
          ;;     scratchDir, $
          ;;     ndv, $
          ;;     RAP_f02_ACPCP_grid, $
          ;;     RAP_f02_NCPCP_grid, $
          ;;     RAP_f02_WEASD_grid, $
          ;;     RAP_GRID_PROJ_INFO = RAPGridInfo

          ;; if (NOT(ISA(RAP_f02_ACPCP_grid)) or $
          ;;     NOT(ISA(RAP_f02_NCPCP_grid))) then begin
          ;;     USR_MSG, 'No RAP f02 data for ' + eventFinishDate_YYYYMMDDHH
          ;;     use_RAP_f02 = 0
          ;; endif else begin
          ;;     ind = WHERE((RAP_f02_ACPCP_grid eq ndv) or $
          ;;                 (RAP_f02_NCPCP_grid eq ndv), count)
          ;;     RAP_f02_grid = RAP_f02_ACPCP_grid + RAP_f02_NCPCP_grid
          ;;     if (count gt 0) then RAP_f02_grid[ind] = ndv

          ;;     RAP_f02_WEASD_grid = !NULL
          ;; endelse

          GET_ACCUM_RAP130_APCP_WEASD, $
              eventFinishDate_YYYYMMDDHH, $
              accEventHours, $
              2, 1, 3, $
              RAPDir, $
              scratchDir, $
              ndv, $
              RAP_f02_grid, $
              RAP_f02_WEASD_grid, $
              perfect, $
              MAX_NUM_SUB_FCST_HOURS = accEventHours / 6, $
              RAP_GRID_PROJ_INFO = RAPGridInfo

          if NOT(ISA(RAP_f02_grid)) then begin
              USR_MSG, 'No RAP f02 data for ' + eventFinishDate_YYYYMMDDHH
              use_RAP_f02 = 0
          endif

          RAP_f02_WEASD_grid = !NULL

      endif

;+
;     Read RAP f03 data.
;-
      if use_RAP_f03 then begin

          ;; RAP_f03_ACPCP_grid = !NULL
          ;; RAP_f03_NCPCP_grid = !NULL
          RAP_f03_grid = !NULL
          RAP_f03_WEASD_grid = !NULL

          ;; GET_ACC_RAP130_ACPCP_NCPCP_WEASD, $
          ;;     eventFinishDate_YYYYMMDDHH, $
          ;;     accEventHours, $
          ;;     3, 1, 2, $
          ;;     RAPDir, $
          ;;     scratchDir, $
          ;;     ndv, $
          ;;     RAP_f03_ACPCP_grid, $
          ;;     RAP_f03_NCPCP_grid, $
          ;;     RAP_f03_WEASD_grid, $
          ;;     RAP_GRID_PROJ_INFO = RAPGridInfo

          ;; if (NOT(ISA(RAP_f03_ACPCP_grid)) or $
          ;;     NOT(ISA(RAP_f03_NCPCP_grid))) then begin
          ;;     USR_MSG, 'No RAP f03 data for ' + eventFinishDate_YYYYMMDDHH
          ;;     use_RAP_f03 = 0
          ;; endif else begin
          ;;     ind = WHERE((RAP_f03_ACPCP_grid eq ndv) or $
          ;;                 (RAP_f03_NCPCP_grid eq ndv), count)
          ;;     RAP_f03_grid = RAP_f03_ACPCP_grid + RAP_f03_NCPCP_grid
          ;;     if (count gt 0) then RAP_f03_grid[ind] = ndv

          ;;     RAP_f03_WEASD_grid = !NULL
          ;; endelse

          GET_ACCUM_RAP130_APCP_WEASD, $
              eventFinishDate_YYYYMMDDHH, $
              accEventHours, $
              3, 1, 2, $
              RAPDir, $
              scratchDir, $
              ndv, $
              RAP_f03_grid, $
              RAP_f03_WEASD_grid, $
              perfect, $
              MAX_NUM_SUB_FCST_HOURS = accEventHours / 6, $
              RAP_GRID_PROJ_INFO = RAPGridInfo

          if NOT(ISA(RAP_f03_grid)) then begin
              USR_MSG, 'No RAP f03 data for ' + eventFinishDate_YYYYMMDDHH
              use_RAP_f03 = 0
          endif

          RAP_f03_WEASD_grid = !NULL

      endif

;+
;     Read NLDAS data.
;-
      if use_NLDAS then begin

;          NLDAS_ACPC_grid = !NULL
          NLDAS_grid = !NULL

          GET_ACC_HOURLY_NLDAS_APCP, $
              eventFinishDate_YYYYMMDDHH, $
              accEventHours, $
              NLDASDir, $
              scratchDir, $
              ndv, $
              NLDAS_grid, $
              NLDAS_GRID_INFO = NLDASGridInfo

          if (NOT(ISA(NLDAS_grid))) then begin
              USR_MSG, 'No NLDAS data for ' + eventFinishDate_YYYYMMDDHH
              use_NLDAS = 0
          endif

      endif

;+
;     Read NWM FE (v1.2) data.
;-
      if use_NWM_tm02 then begin

          NWM_tm02_grid = !NULL

          GET_ACC_HOURLY_NWM_ANA, $
              eventFinishDate_YYYYMMDDHH, $
              accEventHours, $
              NWMPRDir, $
              scratchDir, $
              ndv, $
              NWM_tm02_grid

          if (NOT(ISA(NWM_tm02_grid))) then begin
              USR_MSG, 'No NWM data for ' + eventFinishDate_YYYYMMDDHH
              use_NWM_tm02 = 0
          endif

      endif

;+
;     Read downscaled NLDAS data. This is going to be temporary data
;     in IDL save files generated by the program
;     /nwcdev/nwc_forcings/projects/mountain_mapper_2017/
;     mm_test_nldas.pro
;-
      if use_NLDAS_MM then begin

;          NLDAS_MM_ACPC_grid = !NULL
          NLDAS_MM_grid = !NULL

          savFile = 'NLDAS_MM_APCP_24h_ending_' + $
                    eventFinishDate_YYYYMMDDHH + '.sav'
          if NOT(FILE_TEST(scratchDir + '/' + savFile)) then begin

              USR_MSG, 'No NLDAS MM data for ' + eventFinishDate_YYYYMMDDHH
              use_NLDAS_MM = 0

          endif else begin

              RESTORE, scratchDir + '/' + savFile
              if NOT(ISA(accAPCPGrid_)) then STOP
              if NOT(ISA(ndv_)) then STOP
              if NOT(ISA(NWMGridProjInfo_)) then STOP
              mmGridSize = SIZE(accAPCPGrid_)
              if (mmGridSize[0] ne 2) then STOP
              if (mmGridSize[1] ne NWMGridProjInfo_.nCols) then STOP
              if (mmGridSize[2] ne NWMGridProjInfo_.nRows) then STOP
              NLDAS_MM_grid = TEMPORARY(accAPCPGrid_)

              if NOT(ISA(NWMGridInfo)) then begin
                  NWMGridInfo = NWMGridProjInfo_
              endif else begin
                  if (NWMGridProjInfo_.nCols ne NWMGridInfo.nCols) then STOP
                  if (NWMGridProjInfo_.nRows ne NWMGridInfo.nRows) then STOP
              endelse

          endelse

      endif

;+
;     Read AORC data.
;-
      if use_AORC then begin

          AORC_grid = !NULL

          GET_ACC_HOURLY_AORC_APCP, $
              eventFinishDate_YYYYMMDDHH, $
              accEventHours, $
              AORCDir, $
              scratchDir, $
              ndv, $
              AORC_grid, $
              AORC_GRID_INFO = AORCGridInfo

          if (NOT(ISA(AORC_grid))) then begin
              USR_MSG, 'No AORC data for ' + eventFinishDate_YYYYMMDDHH
              use_AORC = 0
          endif

      endif

;+
;     Calculate station locations on the HRAP grid.
;-
      if (use_EMC_MPE_1 or use_EMC_MPE_6 or use_EMC_MPE_24 or $
          use_OWP_MPE) then begin

          if (ISA(HRAPGridInfo) and $
              (NOT(ISA(iHRAP)) or NOT(ISA(jHRAP)))) then begin

              lonV_rad = HRAPGridInfo.lonV * degToRad
              latD_rad = HRAPGridInfo.latD * degToRad
              lat00_rad = HRAPGridInfo.lat00 * degToRad
              lon00_rad = HRAPGridInfo.lon00 * degToRad

              rho00 = HRAPGridInfo.eRadM * $
                      (1.0D + SIN(latD_rad)) * $
                      TAN(!DPi / 4.0D - lat00_rad / 2.0D)
              theta00 = lon00_rad - lonV_rad

              x00 = rho00 * SIN(theta00)
              y00 = -rho00 * COS(theta00)

              lon_rad = stationData.longitude * degToRad
              lat_rad = stationData.latitude * degToRad

              rho = HRAPGridInfo.eRadM * $
                    (1.0D + SIN(latD_rad)) * $
                    TAN(!DPi / 4.0D - lat_rad / 2.0D)
              theta_rad = lon_rad - lonV_rad

              lon_rad = !NULL
              lat_rad = !NULL

              x = rho * SIN(theta_rad)
              y = -rho * COS(theta_rad)

              rho = !NULL
              theta_rad = !NULL

              iHRAP = (x - x00) / HRAPGridInfo.dx
              jHRAP = (y - y00) / HRAPGridInfo.dy

              x = !NULL
              y = !NULL

          endif

      endif

;+
;     Calculate station locations on the HRRR grid.
;-
      if (use_HRRR_f01 or use_HRRR_f02 or use_HRRR_f03 or $
          use_HRRRX_f01 or use_HRRRX_f02 or use_HRRRX_f03) then begin

          if (ISA(HRRRGridInfo) and $
              (NOT(ISA(iHRRR)) or NOT(ISA(jHRRR)))) then begin
 
;+
;             Get Snyder parameters for HRRR grid/projection.
;-
              LCC_GRIB2_TO_SNYDER, HRRRGridInfo.latSec1, $
                               HRRRGridInfo.latSec2, $
                               HRRRGridInfo.latD, $
                               HRRRGridInfo.lonV, $
                               HRRRGridInfo.lat00, $
                               HRRRGridInfo.lon00, $
                               HRRRGridInfo.eRadM, $
                               lonV_rad, $
                               nSny, $
                               FSny, $
                               rho0, $
                               x00, $
                               y00

              lon_rad = stationData.longitude * degToRad
              lat_rad = stationData.latitude * degToRad

              rho = HRRRGridInfo.eRadM * FSny / $
                    (TAN(!DPi / 4.0D + lat_rad / 2.0D))^nSny ; Synder 15-1
              theta_rad = nSny * (lon_rad - lonV_rad)

              lon_rad = !NULL
              lat_rad = !NULL

              x = rho * SIN(theta_rad)        ; x location on HRRR grid
              y = rho0 - rho * COS(theta_rad) ; y location on HRRR grid

              rho = !NULL
              theta_rad = !NULL

              iHRRR = (x - x00) / HRRRGridInfo.dx
              jHRRR = (y - y00) / HRRRGridInfo.dy

              x = !NULL
              y = !NULL

          endif

      endif

;+
;     Calculate station locations on the MRMS grid.
;-
      if (use_MRMS_GC or use_MRMS_RO) then begin

          if (ISA(MRMSGridInfo) and $
              (NOT(ISA(iMRMS)) or NOT(ISA(jMRMS)))) then begin

              lon1_MRMS = MRMSGridInfo.minLon + 0.5D * MRMSGridInfo.lonRes
              lat1_MRMS = MRMSGridInfo.minLat + 0.5D * MRMSGridInfo.latRes

              iMRMS = (stationData.longitude - lon1_MRMS) / MRMSGridInfo.lonRes
              jMRMS = (stationData.latitude - lat1_MRMS) / MRMSGridInfo.latRes

          endif

      endif

;+
;     Calculate station locations on the RAP grid.
;-
      if (use_RAP_f01 or use_RAP_f02 or use_RAP_f03) then begin

          if (ISA(RAPGridInfo) and $
              (NOT(ISA(iRAP)) or NOT(ISA(jRAP)))) then begin

;+
;             Get Snyder parameters for RAP grid/projection.
;-
              LCC_GRIB2_TO_SNYDER, RAPGridInfo.latSec1, $
                                   RAPGridInfo.latSec2, $
                                   RAPGridInfo.latD, $
                                   RAPGridInfo.lonV, $
                                   RAPGridInfo.lat00, $
                                   RAPGridInfo.lon00, $
                                   RAPGridInfo.eRadM, $
                                   lonV_rad, $
                                   nSny, $
                                   FSny, $
                                   rho0, $
                                   x00, $
                                   y00

              lon_rad = stationData.longitude * degToRad
              lat_rad = stationData.latitude * degToRad

              rho = RAPGridInfo.eRadM * FSny / $
                    (TAN(!DPi / 4.0D + lat_rad / 2.0D))^nSny ; Synder 15-1
              theta_rad = nSny * (lon_rad - lonV_rad)

              lon_rad = !NULL
              lat_rad = !NULL

              x = rho * SIN(theta_rad)        ; x location on RAP grid
              y = rho0 - rho * COS(theta_rad) ; y location on RAP grid

              rho = !NULL
              theta_rad = !NULL

              iRAP = (x - x00) / RAPGridInfo.dx
              jRAP = (y - y00) / RAPGridInfo.dy

              x = !NULL
              y = !NULL

          endif

      endif

;+
;     Calculate station locations on the NLDAS grid.
;-
      if (use_NLDAS) then begin

          if (ISA(NLDASGridInfo) and $
              (NOT(ISA(iNLDAS)) or NOT(ISA(jNLDAS)))) then begin

              lon1_NLDAS = NLDASGridInfo.minLon + 0.5D * NLDASGridInfo.lonRes
              lat1_NLDAS = NLDASGridInfo.minLat + 0.5D * NLDASGridInfo.latRes

              iNLDAS = (stationData.longitude - lon1_NLDAS) / $
                       NLDASGridInfo.lonRes
              jNLDAS = (stationData.latitude - lat1_NLDAS) / $
                       NLDASGridInfo.latRes

          endif

      endif

;+
;     Calculate station locations on the NLDAS-MM (NWM) grid.
;-
      if (use_NLDAS_MM or use_NWM_tm02) then begin

          if (ISA(NWMGridInfo) and $
              (NOT(ISA(iNWM)) or NOT(ISA(jNWM)))) then begin

;+
;             Get Snyder parameters for HRRR grid/projection.
;-
              LCC_GRIB2_TO_SNYDER, NWMGridInfo.latSec1, $
                                   NWMGridInfo.latSec2, $
                                   NWMGridInfo.latD, $
                                   NWMGridInfo.lonV, $
                                   NWMGridInfo.lat00, $
                                   NWMGridInfo.lon00, $
                                   NWMGridInfo.eRadM, $
                                   lonV_rad, $
                                   nSny, $
                                   FSny, $
                                   rho0, $
                                   x00, $
                                   y00

              lon_rad = stationData.longitude * degToRad
              lat_rad = stationData.latitude * degToRad

              rho = NWMGridInfo.eRadM * FSny / $
                    (TAN(!DPi / 4.0D + lat_rad / 2.0D))^nSny ; Synder 15-1
              theta_rad = nSny * (lon_rad - lonV_rad)

              lon_rad = !NULL
              lat_rad = !NULL

              x = rho * SIN(theta_rad)        ; x location on HRRR grid
              y = rho0 - rho * COS(theta_rad) ; y location on HRRR grid

              rho = !NULL
              theta_rad = !NULL

              iNWM = (x - x00) / NWMGridInfo.dx
              jNWM = (y - y00) / NWMGridInfo.dy

              x = !NULL
              y = !NULL

          endif

      endif

;+
;     Calculate station locations on the AORC grid.
;-
      if (use_AORC) then begin

          if (ISA(AORCGridInfo) and $
              (NOT(ISA(iAORC)) or NOT(ISA(jAORC)))) then begin

              lon1_AORC = AORCGridInfo.minLon + 0.5D * AORCGridInfo.lonRes
              lat1_AORC = AORCGridInfo.minLat + 0.5D * AORCGridInfo.latRes

              iAORC = (stationData.longitude - lon1_AORC) / $
                       AORCGridInfo.lonRes
              jAORC = (stationData.latitude - lat1_AORC) / $
                       AORCGridInfo.latRes

          endif

      endif

;+ 
;     Do following each case of use_* in a loop
;-

      PRINT, 'Calculating event statistics for event #' + STRCRA(ec)

      for i = 0, N_ELEMENTS(inputFlag) - 1 do begin 

          use_val = 'use_' + inputFlagName[i]

          if (SCOPE_VARFETCH(use_val,/ENTER, level=1)) then begin

;+
;             Sample data at station locations.
;             Forming names first
;-          
              event_st = 'event_' + inputFlagName[i] + '_stations'
              var_grid = inputFlagName[i] + '_grid'
              var_st = inputFlagName[i] + '_stations'

;+
;             Sample data at station locations.
;-
              if (inputFlagName[i] eq 'EMC_MPE_1' or $
                  inputFlagName[i] eq 'EMC_MPE_6' or $
                  inputFlagName[i] eq 'EMC_MPD_24' or $
                  inputFlagName[i] eq 'OWP_MPE') then begin
                  i_station = iHRAP
                  j_station = jHRAP
              endif
              if (inputFlagName[i] eq 'MRMS_GC' or $
                  inputFlagName[i] eq 'MRMS_RO') then begin
                  i_station = iMRMS
                  j_station = jMRMS
              endif
              if (inputFlagName[i] eq 'HRRR_f01' or $
                  inputFlagName[i] eq 'HRRR_f02' or $
                  inputFlagName[i] eq 'HRRR_f03' or $
                  inputFlagName[i] eq 'HRRRX_f01' or $
                  inputFlagName[i] eq 'HRRRX_f02' or $
                  inputFlagName[i] eq 'HRRRX_f03') then begin
                  i_station = iHRRR
                  j_station = jHRRR
              endif
              if (inputFlagName[i] eq 'RAP_f01' or $
                  inputFlagName[i] eq 'RAP_f02' or $
                  inputFlagName[i] eq 'RAP_f03') then begin
                  i_station = iRAP
                  j_station = jRAP
              endif
              if (inputFlagName[i] eq 'NLDAS') then begin
                  i_station = iNLDAS
                  j_station = jNLDAS
              endif
              if ( inputFlagName[i] eq 'NLDAS_MM' or $
                   inputFlagName[i] eq 'NWM_tm02') then begin
                  i_station = iNWM
                  j_station = jNWM
              endif
              if ( inputFlagName[i] eq 'AORC') then begin
                  i_station = iAORC
                  j_station = jAORC
              endif

              if (sampling eq 1) then $
                  (SCOPE_VARFETCH(event_st, /ENTER, level=1)) = $
                  REGRID_BILIN(SCOPE_VARFETCH(var_grid, /ENTER, level=1), $
                               i_station, j_station, ndv) $
              else $
                  (SCOPE_VARFETCH(event_st, /ENTER, level=1)) = $
                  REGRID_NEIGHBOR(SCOPE_VARFETCH(var_grid, /ENTER, level=1), $
                                  i_station, j_station, ndv)

              (SCOPE_VARFETCH(var_st, /ENTER, level=1))[*, ec] = $
                  (SCOPE_VARFETCH(event_st, /ENTER, level=1))

              (SCOPE_VARFETCH(var_grid,/ENTER, level=1)) = !NULL

;+
;             Calculate across-the-domain event statistics.
;-
              var_a = 'a_' + inputFlagName[i]
              var_b = 'b_' + inputFlagName[i]
              var_c = 'c_' + inputFlagName[i]
              var_d = 'd_' + inputFlagName[i]
              var_totARE = 'total_ARE_' + inputFlagName[i]
              var_totLMB = 'total_LMB_' + inputFlagName[i]
              var_totLMB2 = 'total_LMB2_' + inputFlagName[i]

              EPVS_01, eventPrecipObs_mm, $
                       (SCOPE_VARFETCH(event_st,/ENTER, level=1)), $ 
                       eventThreshold_mm, $
                       ndv, $
                       ec, $
                       (SCOPE_VARFETCH(var_a, /ENTER, LEVEL = 1)), $
                       (SCOPE_VARFETCH(var_b, /ENTER, LEVEL = 1)), $
                       (SCOPE_VARFETCH(var_c, /ENTER, LEVEL = 1)), $
                       (SCOPE_VARFETCH(var_d, /ENTER, LEVEL = 1)), $
                       (SCOPE_VARFETCH(var_totARE, /ENTER, LEVEL = 1)), $
                       (SCOPE_VARFETCH(var_totLMB, /ENTER, LEVEL = 1)), $
                       (SCOPE_VARFETCH(var_totLMB2, /ENTER, LEVEL = 1))

              killme_a = SCOPE_VARFETCH(var_a, /ENTER, LEVEL = 1)
              killme_b = SCOPE_VARFETCH(var_b, /ENTER, LEVEL = 1)
              ;killme_c = SCOPE_VARFETCH(var_c, /ENTER, LEVEL = 1)

              PRINT, inputFlagName[i] + ' has ' + $
                     STRCRA(killme_a[ec] + killme_b[ec]) + $
                     ' modeled events.'

          endif else begin

              if (inputFlag[i] eq 1) then $
                  PRINT, inputFlagName[i] + ' has no data.'

          endelse

      endfor

;+
;     *************************************************
;     * End of data gathering, beginning of analysis. *
;     *************************************************
;-

;+
;     Only plot results at the end of each aggregation of events.
;-
      if (((ec + 1) mod eventAggFactor) ne 0) then CONTINUE

;+
;     Only clean up vertical positions of lables with
;     ELIMINATE_TEXT_VERT_OVERLAP when we are close to the end, since
;     it can take several seconds and is only really needed at the
;     end.
;-
      if ((FLOAT(ec) / FLOAT(numEvents - 1)) gt 0.98) then $
          cleanUp = 1 $
      else $
          cleanUp = 0

;+
;     Generate statistics for aggregate events, including probability
;     of detection (POD), false alarm ratio (FAR), mean absolute
;     relative error (MARE), geometric mean multiplicative bias
;     (GMMB), and standard deviation of log multiplicative bias
;     (SDLMB).
;-
      aec = ec / eventAggFactor      ; index (from 0) of aggregate event
      ec1 = aec * eventAggFactor     ; index of 1st event in aggregate event
      ec2 = ec1 + eventAggFactor - 1 ; index of 2nd event in aggregate event

      PRINT, 'Aggregating data for events ' + STRCRA(ec1) + ' to ' + $
             STRCRA(ec2) + ' (aggregate event index ' + STRCRA(aec) + ')'

      for i = 0, N_ELEMENTS(inputFlag) - 1 do begin

          if (inputFlag[i] eq 1) then begin
;          use_val = 'use_' + inputFlagName[i]
;          if (SCOPE_VARFETCH(use_val,/ENTER, level=1)) then begin
             var_a = 'a_' + inputFlagName[i]
             var_b = 'b_' + inputFlagName[i]
             var_c = 'c_' + inputFlagName[i]
             var_d = 'd_' + inputFlagName[i]
             var_totARE = 'total_ARE_' + inputFlagName[i]
             var_totLMB = 'total_LMB_' + inputFlagName[i]
             var_totLMB2 = 'total_LMB2_' + inputFlagName[i]
             var_POD = 'POD_' + inputFlagName[i]
             var_FAR = 'FAR_' + inputFlagName[i]
             var_MARE = 'MARE_' + inputFlagName[i]
             var_GMMB = 'GMMB_' + inputFlagName[i]
             var_SDLMB = 'SDLMB_' + inputFlagName[i]
             var_HSS = 'HSS_' + inputFlagName[i]
             var_CSI = 'CSI_' + inputFlagName[i]

             EPVS_02, ndvInt, $
                      ndv, $
                      ec1, ec2, $
                      (SCOPE_VARFETCH(var_a, /ENTER, LEVEL = 1)), $
                      (SCOPE_VARFETCH(var_b, /ENTER, LEVEL = 1)), $
                      (SCOPE_VARFETCH(var_c, /ENTER, LEVEL = 1)), $
                      (SCOPE_VARFETCH(var_d, /ENTER, LEVEL = 1)), $
                      (SCOPE_VARFETCH(var_totARE, /ENTER, LEVEL = 1)), $
                      (SCOPE_VARFETCH(var_totLMB, /ENTER, LEVEL = 1)), $
                      (SCOPE_VARFETCH(var_totLMB2, /ENTER, LEVEL = 1)), $
                      aec, $
                      (SCOPE_VARFETCH(var_POD, /ENTER, LEVEL = 1)), $
                      (SCOPE_VARFETCH(var_FAR, /ENTER, LEVEL = 1)), $
                      (SCOPE_VARFETCH(var_MARE, /ENTER, LEVEL = 1)), $
                      (SCOPE_VARFETCH(var_GMMB, /ENTER, LEVEL = 1)), $
                      (SCOPE_VARFETCH(var_SDLMB, /ENTER, LEVEL = 1)), $
                      (SCOPE_VARFETCH(var_HSS, /ENTER, LEVEL = 1)), $
                      (SCOPE_VARFETCH(var_CSI, /ENTER, LEVEL = 1))

;+
;             Compute map correlations
;-
              var_st = inputFlagName[i] + '_stations'
              obs = precipObs_mm[*, ec1:ec2]
              mdl = (SCOPE_VARFETCH(var_st, /ENTER, LEVEL = 1))[*, ec1:ec2]
;              aInd = WHERE((obs ne ndv) and $
;                           ((obs * 1000.0) gt eventThreshold_mm) and $
;                           (mdl ne ndv) and $
;                           (mdl gt eventThreshold_mm), $
;                           aCount)
              ;MESSAGE, 'Need to define "PCorr_" and "RCorr_" variables.'
              var_pCorr = 'PCorr_' + inputFlagName[i]
              var_rCorr = 'RCorr_' + inputFlagName[i]
              ind = WHERE((obs ne ndv) and (mdl ne ndv), count)
              if ((count gt 5) and $
;                  (MAX(obs[ind]) gt eventThreshold_mm) and $
;                  (MAX(mdl[ind]) gt eventThreshold_mm) and $
                  (MIN(obs[ind]) ne MAX(obs[ind])) and $
                  (MIN(mdl[ind]) ne MAX(mdl[ind]))) then begin
                  pCorr = CORRELATE(obs[ind], mdl[ind])
                  (SCOPE_VARFETCH(var_pCorr, /ENTER, LEVEL = 1))[aec] = $
                      pCorr
                  rCorr = R_CORRELATE(obs[ind], mdl[ind])
                  (SCOPE_VARFETCH(var_rCorr, /ENTER, LEVEL = 1))[aec] = $
                      rCorr[0]


              endif

          endif

      endfor

;+
;     It may be worthwhile to CONTINUE for low values of ec at this
;     point. Producing plots inside the "for ec" loop provides
;     feedback on results as the analysis proceeds, but it does slow
;     the analysis significantly and is not necessary.
;- 

;      if (ec lt 300) then CONTINUE
;      if (ec lt 50) then CONTINUE
;      if (ec lt 0.99 * (numEvents - 1)) then CONTINUE
;      if ((FLOAT(ec) / FLOAT(numEvents - 1)) lt 0.98) then CONTINUE

;+
;     Display results for all completed time steps.
;-
;      TVLCT, pRed, pGrn, pBlu

;+
; Establish units for time axes on plots.
;-
      xTickUnits = 'Time'
      if ((finishDate_Julian - startDate_Julian) gt 365.0) then begin
          xTickUnits = 'Months'
      endif
      xMinor = -1

;+
;     Plot time series of mean absolute relative error (MARE).
;-

;+
;     Set up y axis.
;-
      killme = []
      for i = 0, N_ELEMENTS(inputFlag)-1 do begin
          var_MARE = 'MARE_' + inputFlagName[i]
          if (inputFlag[i] eq 1) then $
             killme = [killme, (SCOPE_VARFETCH(var_MARE, /ENTER, LEVEL = 1))]
             
      endfor
      ind = WHERE(killme ne ndv, count)
      if (count eq 0) then CONTINUE
      yMax = MAX(killme[ind]) < 10.0
      killme = !NULL

;+
;     Identify times for which there is data.
;-
      tFlag = INTARR(numAggEvents)
      for i = 0, N_ELEMENTS(inputFlag)-1 do begin
          var_MARE = 'MARE_' + inputFlagName[i]
          if (inputFlag[i] eq 1) then $
             tFlag = tFlag + ((SCOPE_VARFETCH(var_MARE, /ENTER, LEVEL = 1)) ne ndv)
      endfor

      ind = WHERE(tFlag gt 0, count)
;      if (count le 1) then begin
;         print, 'numAggEvents too small'
;         stop
;      endif
      if (count gt 1) then begin

          if ISA(p0) then begin
              p0.window.SetCurrent
              p0.window.Erase
              current = 1
          endif else begin
              current = 0
          endelse

          p0 = PLOT(aggTimeAxis[ind], REPLICATE(ndv, count), $
                    TITLE = 'Mean Absolute Relative Error (MARE) vs. ' + $
                    '24-hour 12Z Observations!C' + $
                    subtitle, $
                    FONT_NAME = 'DejaVuSansBold', FONT_STYLE = 'Bold', $
                    XRANGE = [aggTimeAxis[ind[0]], $
                              aggTimeAxis[ind[count - 1]] + $
                              0.16 * (aggTimeAxis[ind[count - 1]] - $
                                      aggTimeAxis[ind[0]])], $
                    ;XSTYLE = 1, $
                    XTICKUNITS = xTickUnits, $
                    XMINOR = xMinor, $
                    XTICKFORMAT = 'LABEL_DATE', $
                    YTITLE = 'MARE (dimensionless)', $
                    YRANGE = [0, yMax], $
                    POS = [0.1, 0.08, 0.95, 0.9], $
                    WINDOW_TITLE = 'Mean Absolute Relative Error', $
                    DIMENSIONS = [960, 540], $
                    LOCATION = [200, 200], $
                    /NODATA, $
                    CURRENT = current)
          (p0.axes)[0].thick = 2
          (p0.axes)[1].thick = 2
          (p0.axes)[2].thick = 2
          (p0.axes)[3].thick = 2
          (p0.axes)[3].ticklen = 0.01

;+
;         Refine minor ticks on horizontal axes. The default counts
;         for minor tick intervals are often a bit weird. Make them
;         one minor tick per month if possible.
;-
          numTicks = N_ELEMENTS((p0.axes)[0].tickValues)
          if (numTicks gt 1) then begin
              tickInterval = $
                  MEAN(((p0.axes)[0].tickValues)[1:numTicks - 1] - $
                       ((p0.axes)[0].tickValues)[0:numTicks - 2])
              if (tickInterval gt 28) then begin
                  tickMonths = ROUND(tickInterval / 30.4375)
                  xMinor = tickMonths - 1    ; minor ticks for months
              endif else begin
                  xMinor = tickInterval -  1 ; minor ticks for days
              endelse
              (p0.axes)[0].minor = xMinor
              (p0.axes)[2].minor = xMinor
          endif

          xRangeWidth = (p0.xRange)[1] - (p0.xRange)[0]
          yRangeWidth = (p0.yRange)[1] - (p0.yRange)[0]

          t0 = []

          for i = 0, N_ELEMENTS(inputFlag)-1 do begin

              p0_var = 'p0_' + STRING(STRCRA(i+1), FORMAT = '(i02)')
              t0_var = 't0_' +  STRING(STRCRA(i+1), FORMAT = '(i02)')

              if NOT(inputFlag[i]) then CONTINUE

              var_rgb = inputFlagName[i] + '_rgb'
              var_MARE = 'MARE_' + inputFlagName[i]
              ind = WHERE((SCOPE_VARFETCH(var_MARE, /ENTER, $
                                          LEVEL = 1)) ne ndv, count)
              if (count gt 1) then begin

                  (SCOPE_VARFETCH(p0_var, /ENTER, LEVEL = 1)) = $
                      PLOT(aggTimeAxis[ind], $
                           (SCOPE_VARFETCH(var_MARE, /ENTER, $
                                           LEVEL = 1))[ind], $
                           /OVERPLOT, $
                           COLOR = (SCOPE_VARFETCH(var_rgb, /ENTER, $
                                                   LEVEL = 1)), $
                           THICK = 2)

                  (SCOPE_VARFETCH(t0_var, /ENTER, LEVEL = 1)) = $
                      TEXT(aggTimeAxis[ind[count - 1]] + $
                           0.01 * xRangeWidth, $
                           (SCOPE_VARFETCH(var_MARE, /ENTER, $
                                           LEVEL = 1))[ind[count - 1]] - $
                           0.01 * yRangeWidth, $
                           inputFlagName[i], $
                           /DATA, CLIP = 0, $
                           FONT_COLOR = (SCOPE_VARFETCH(var_rgb, /ENTER, $
                                                        LEVEL = 1)), $
                           FONT_NAME = 'DejaVuSansBold', $
                           FONT_STYLE = 'Bold')

                  t0 = [t0, (SCOPE_VARFETCH(t0_var, /ENTER, LEVEL = 1))]

              endif

          endfor

          if cleanUp then ELIMINATE_TEXT_VERT_OVERLAP, t0

      endif
;+
;     Plot time series of geometric mean of multiplicative bias (GMMB).
;-

;+
;     Set up y axis.
;-
      killme = []
      for i = 0, N_ELEMENTS(inputFlag)-1 do begin
          var_GMMB = 'GMMB_' + inputFlagName[i]
          if (inputFlag[i] eq 1) then begin 
             killme = [killme, (SCOPE_VARFETCH(var_GMMB, /ENTER, LEVEL = 1))]
          endif
      endfor
      ind = WHERE(killme ne ndv, count)
      if (count eq 0) then STOP
      yMin = 10.0^(-MAX(ABS([ALOG10(MIN(killme[ind])), $
                             ALOG10(MAX(killme[ind]))])))
      yMax = 10.0^MAX(ABS([ALOG10(MIN(killme[ind])), $
                           ALOG10(MAX(killme[ind]))]))

;+ GF 20180827
;+
;     Adjust yMin and yMax as needed to get at least three tick marks.
;-
;      PRINT, 'yMin, yMax before: ', yMin, yMax

      yCount = 1
      yMin = yMin + 0.001
      yMax = yMax - 0.001

      while (yCount lt 3) do begin

          yMin = yMin - 0.001
          yMax = yMax + 0.001
;+
;         Set the major tick interval to an order of magnitude below
;         the scale of the range.
;-
          interval = 10.0^(FLOOR(ALOG10(yMax) - ALOG10(yMin)) - 1)
;+
;          Place the [first,last] tick mark just [above,below]
;          [yStart,yFinish].
;-
          yStart = CEIL(yMin / interval) * interval
          yFinish = FLOOR(yMax / interval) * interval

;+
;         Complete setting of ticks.
;-
          yCount = ROUND((yFinish - yStart) / interval) + 1

      endwhile

;      PRINT, 'yMin, yMax after: ', yMin, yMax
;- GF 20180827

      killme = !NULL

;+
;     Identify times for which there is data.
;-
      tFlag = INTARR(numAggEvents)
      for i = 0, N_ELEMENTS(inputFlag)-1 do begin
          var_GMMB = 'GMMB_' + inputFlagName[i]
          if (inputFlag[i] eq 1) then begin 
             tFlag = tFlag + ((SCOPE_VARFETCH(var_GMMB, /ENTER, LEVEL = 1)) ne ndv)
          endif
      endfor
      ind = WHERE(tFlag gt 0, count)
      if (count gt 1) then begin

          if ISA(p1) then begin
              p1.window.SetCurrent
              p1.window.Erase
              current = 1
          endif else begin
              current = 0
          endelse

          p1 = PLOT(aggTimeAxis[ind], REPLICATE(ndv, count), $
                    TITLE = 'Geometric Mean Multiplicative Bias vs. ' + $
                    '24-hour 12Z Observations!C' + $
                    subtitle, $
                    FONT_NAME = 'DejaVuSansBold', FONT_STYLE = 'Bold', $
                    XRANGE = [aggTimeAxis[ind[0]], $
                              aggTimeAxis[ind[count - 1]] + $
                              0.16 * (aggTimeAxis[ind[count - 1]] - $
                                      aggTimeAxis[ind[0]])], $
                    ;XSTYLE = 1, $
                    XTICKUNITS = xTickUnits, $
                    XMINOR = xMinor, $
                    XTICKFORMAT = 'LABEL_DATE', $
                    YTITLE = 'Geometric Mean Multiplicative Bias', $
                    ;/YLOG, $
                    YRANGE = [ALOG10(yMin), ALOG10(yMax)], $
                    POS = [0.1, 0.08, 0.95, 0.9], $
                    WINDOW_TITLE = 'Geometric Mean Multiplicative Bias', $
                    DIMENSIONS = [960, 540], $
                    LOCATION = [300, 300], $
                    /NODATA, $
                    CURRENT = current)
          (p1.axes)[0].thick = 2
          (p1.axes)[1].thick = 2
          (p1.axes)[2].thick = 2
          (p1.axes)[3].thick = 2
          (p1.axes)[3].ticklen = 0.01

;+
;         Refine minor ticks on horizontal axes. The default counts
;         for minor tick intervals are often a bit weird. Make them
;         one minor tick per month if possible.
;-
          numTicks = N_ELEMENTS((p1.axes)[0].tickValues)
          if (numTicks gt 1) then begin
              tickInterval = $
                  MEAN(((p1.axes)[0].tickValues)[1:numTicks - 1] - $
                       ((p1.axes)[0].tickValues)[0:numTicks - 2])
              if (tickInterval gt 28) then begin
                  tickMonths = ROUND(tickInterval / 30.4375)
                  xMinor = tickMonths - 1   ; minor ticks for months
              endif else begin
                  xMinor = tickInterval - 1 ; minor ticks for days
              endelse
              (p1.axes)[0].minor = xMinor
              (p1.axes)[2].minor = xMinor
          endif

          xRangeWidth = (p1.xRange)[1] - (p1.xRange)[0]
          yRangeWidth = (p1.yRange)[1] - (p1.yRange)[0]

          p1_xAxis = PLOT(p1.xRange, [0.0, 0.0], $
                          /OVERPLOT, $
                          LINESTYLE = 2)

          t1 = []

          for i = 0, N_ELEMENTS(inputFlag)-1 do begin
              p1_var = 'p1_' + STRING(STRCRA(i+1), FORMAT = '(i02)')
              t1_var = 't1_' +  STRING(STRCRA(i+1), FORMAT = '(i02)')
              if (inputFlag[i] eq 1) then begin
                 var_rgb = inputFlagName[i] + '_rgb'
                 var_GMMB = 'GMMB_' + inputFlagName[i]
                 ind = WHERE((SCOPE_VARFETCH(var_GMMB, /ENTER, LEVEL = 1)) ne ndv, count)
                 if (count gt 1) then begin
                    (SCOPE_VARFETCH(p1_var, /ENTER, LEVEL = 1)) = $
                                 PLOT(aggTimeAxis[ind], $
                                ALOG10((SCOPE_VARFETCH(var_GMMB, /ENTER, $ 
                                       LEVEL = 1))[ind]), $
                                /OVERPLOT, $
                                COLOR = (SCOPE_VARFETCH(var_rgb, /ENTER, LEVEL = 1)), $
                                THICK = 2)
                    (SCOPE_VARFETCH(t1_var, /ENTER, LEVEL = 1)) = $
                                TEXT(aggTimeAxis[ind[count - 1]] + $
                                0.01 * xRangeWidth, $
                               ALOG10((SCOPE_VARFETCH(var_GMMB, /ENTER, $
                                   LEVEL = 1))[ind[count - 1]]) - $
                               0.01 * yRangeWidth, $
                               inputFlagName[i], $
                               /DATA, CLIP = 0, $
                               FONT_COLOR = (SCOPE_VARFETCH(var_rgb, /ENTER, $
                                   LEVEL = 1)), $
                               FONT_NAME = 'DejaVuSansBold', $
                               FONT_STYLE = 'Bold')
                    t1 = [t1, (SCOPE_VARFETCH(t1_var, /ENTER, LEVEL = 1))]
                 endif
              endif
          endfor

          if cleanUp then ELIMINATE_TEXT_VERT_OVERLAP, t1

;+
;         Convert log values on the y axis to straight bias
;         values. Eliminate minor tick marks to prevent confusion, and
;         set ticks to reasonable bias values.
;-
          (p1.axes)[1].minor = 0
          (p1.axes)[3].minor = 0 ; GF 20180827
;          yTickV = (p1.axes)[1].tickvalues
;          yTickName = FORMAT_FLOAT(10.0^yTickV)
;          (p1.axes)[1].tickname = yTickName

;+
;         Set the major tick interval to an order of magnitude below
;         the scale of the range.
;-
          interval = 10.0^(FLOOR(ALOG10(yMax) - ALOG10(yMin)) - 1)

;+
;         Place the [first,last] tick mark just [above,below]
;         [yStart,yFinish].
;-
          yStart = CEIL(yMin / interval) * interval
          yFinish = FLOOR(yMax / interval) * interval

;+
;         Complete setting of ticks.
;-
          yCount = ROUND((yFinish - yStart) / interval) + 1
          yTickRatioVals = yStart + FINDGEN(yCount) * interval 
          yTickV = ALOG10(yTickRatioVals)
          yTickName = FORMAT_FLOAT(yTickRatioVals)
          (p1.axes)[1].tickvalues = yTickV
          (p1.axes)[1].tickname = yTickName
          (p1.axes)[1].ticklen = 0.02
          (p1.axes)[3].tickvalues = yTickV ; GF 20180827
          (p1.axes)[3].tickname = REPLICATE(' ', yCount) ; GF 20180827
          (p1.axes)[3].ticklen = 0.01 ; GF 20180827

      endif

;+
;     Plot time series of standard deviation in log multiplicative
;     bias (SDLMB).
;-

;+
;     Set up y axis.
;-
      killme = []
      for i = 0, N_ELEMENTS(inputFlag)-1 do begin
          var_SDLMB = 'SDLMB_' + inputFlagName[i]
          if (inputFlag[i] eq 1) then begin 
             killme = [killme, (SCOPE_VARFETCH(var_SDLMB, /ENTER, LEVEL = 1))]
          endif
      endfor
      ind = WHERE(killme ne ndv, count)
      if (count eq 0) then STOP
      yMin = MIN(killme[ind])
      yMax = MAX(killme[ind])
      killme = !NULL

;+
;     Identify times for which there is data.
;-
      tFlag = INTARR(numAggEvents)
      for i = 0, N_ELEMENTS(inputFlag)-1 do begin
          var_SDLMB = 'SDLMB_' + inputFlagName[i]
          if (inputFlag[i] eq 1) then $
             tFlag = tFlag + ((SCOPE_VARFETCH(var_SDLMB, /ENTER, LEVEL = 1)) ne ndv)
      endfor
      ind = WHERE(tFlag gt 0, count)
      if (count gt 1) then begin

          if ISA(p2) then begin
              p2.window.SetCurrent
              p2.window.Erase
              current = 1
          endif else begin
              current = 0
          endelse

          p2 = PLOT(aggTimeAxis[ind], REPLICATE(ndv, count), $
                    TITLE = 'Standard Deviation in Log Multiplicative Bias ' + $
                    'vs. 24-hour 12Z Observations!C' + $
                    subtitle, $
                    FONT_NAME = 'DejaVuSansBold', FONT_STYLE = 'Bold', $
                    XRANGE = [aggTimeAxis[ind[0]], $
                              aggTimeAxis[ind[count - 1]] + $
                              0.16 * (aggTimeAxis[ind[count - 1]] - $
                                      aggTimeAxis[ind[0]])], $
                    ;XSTYLE = 1, $
                    XTICKUNITS = xTickUnits, $
                    XMINOR = xMinor, $
                    XTICKFORMAT = 'LABEL_DATE', $
                    YTITLE = 'Std. Dev. in Log Mult. Bias', $
                    YRANGE = [yMin, yMax], $
                    POS = [0.1, 0.08, 0.95, 0.9], $
                    WINDOW_TITLE = 'Std. Dev. in Log Multiplicative Bias', $
                    DIMENSIONS = [960, 540], $
                    LOCATION = [400, 400], $
                    /NODATA, $
                    CURRENT = current)
          (p2.axes)[0].thick = 2
          (p2.axes)[1].thick = 2
          (p2.axes)[2].thick = 2
          (p2.axes)[3].thick = 2
          (p2.axes)[3].ticklen = 0.01

;+
;         Refine minor ticks on horizontal axes. The default counts
;         for minor tick intervals are often a bit weird. Make them
;         one minor tick per month if possible.
;-
          numTicks = N_ELEMENTS((p2.axes)[0].tickValues)
          if (numTicks gt 1) then begin
              tickInterval = $
                  MEAN(((p2.axes)[0].tickValues)[1:numTicks - 1] - $
                       ((p2.axes)[0].tickValues)[0:numTicks - 2])
              if (tickInterval gt 28) then begin
                  tickMonths = ROUND(tickInterval / 30.4375)
                  xMinor = tickMonths - 1   ; minor ticks for months
              endif else begin
                  xMinor = tickInterval - 1 ; minor ticks for days
              endelse
              (p2.axes)[0].minor = xMinor
              (p2.axes)[2].minor = xMinor
          endif

          xRangeWidth = (p2.xRange)[1] - (p2.xRange)[0]
          yRangeWidth = (p2.yRange)[1] - (p2.yRange)[0]

          t2 = []
          for i = 0, N_ELEMENTS(inputFlag)-1 do begin
              p2_var = 'p2_' + STRING(STRCRA(i+1), FORMAT = '(i02)')
              t2_var = 't2_' +  STRING(STRCRA(i+1), FORMAT = '(i02)')
              if (inputFlag[i] eq 1) then begin
                 var_rgb = inputFlagName[i] + '_rgb'
                 var_SDLMB = 'SDLMB_' + inputFlagName[i]
                 ind = WHERE((SCOPE_VARFETCH(var_SDLMB, /ENTER, $
                              LEVEL = 1)) ne ndv, count)
                 if (count gt 1) then begin
                    (SCOPE_VARFETCH(p2_var, /ENTER, LEVEL = 1)) = $
                                 PLOT(aggTimeAxis[ind], $
                                (SCOPE_VARFETCH(var_SDLMB, /ENTER, LEVEL = 1))[ind], $
                                /OVERPLOT, $
                                COLOR = (SCOPE_VARFETCH(var_rgb, /ENTER, LEVEL = 1)), $
                                THICK = 2)
                    (SCOPE_VARFETCH(t2_var, /ENTER, LEVEL = 1)) = $
                                TEXT(aggTimeAxis[ind[count - 1]] + $
                                0.01 * xRangeWidth, $
                               (SCOPE_VARFETCH(var_SDLMB, /ENTER, $
                                   LEVEL = 1))[ind[count - 1]] - $
                               0.01 * yRangeWidth, $
                               inputFlagName[i], $
                               /DATA, CLIP = 0, $
                               FONT_COLOR = (SCOPE_VARFETCH(var_rgb, /ENTER, $
                                   LEVEL = 1)), $
                               FONT_NAME = 'DejaVuSansBold', $
                               FONT_STYLE = 'Bold')
                    t2 = [t2, (SCOPE_VARFETCH(t2_var, /ENTER, LEVEL = 1))]
                 endif
              endif
          endfor

          if cleanUp then ELIMINATE_TEXT_VERT_OVERLAP, t2

      endif

;+
;     Plot time series of POD/FAR.
;-

;+
;     Identify times for which there is data.
;-
      tFlag = INTARR(numAggEvents)

      for i = 0, N_ELEMENTS(inputFlag)-1 do begin
          var_POD = 'POD_' + inputFlagName[i]
          var_FAR = 'FAR_' + inputFlagName[i]
          if (inputFlag[i] eq 1) then begin
             tFlag = tFlag + ((SCOPE_VARFETCH(var_POD, /ENTER, LEVEL = 1)) ne ndv)
             tFlag = tFlag + ((SCOPE_VARFETCH(var_FAR, /ENTER, LEVEL = 1)) ne ndv)
          endif
      endfor

      ind = WHERE(tFlag gt 0, count)
      if (count gt 1) then begin

          if ISA(p3) then begin
              p3.window.SetCurrent
              p3.window.Erase
              current = 1
          endif else begin
              current = 0
          endelse

          p3 = PLOT(aggTimeAxis[ind], REPLICATE(ndv, count), $
                    TITLE = 'POD and FAR ' + $
                    'vs. 24-hour 12Z Observations!C' + $
                    subtitle, $
                    FONT_NAME = 'DejaVuSansBold', FONT_STYLE = 'Bold', $
                    XRANGE = [aggTimeAxis[ind[0]], $
                              aggTimeAxis[ind[count - 1]] + $
                              0.16 * (aggTimeAxis[ind[count - 1]] - $
                                      aggTimeAxis[ind[0]])], $
                    ;XSTYLE = 1, $
                    XTICKUNITS = xTickUnits, $
                    XMINOR = xMinor, $
                    XTICKFORMAT = 'LABEL_DATE', $
                    YTITLE = 'Dimensionless', $
                    YRANGE = [0.0, 1.0], $
                    POS = [0.1, 0.08, 0.95, 0.9], $
                    WINDOW_TITLE = 'Prob. of Detection, False Alarm Ratio', $
                    DIMENSIONS = [960, 540], $
                    LOCATION = [500, 500], $
                    /NODATA, $
                    CURRENT = current)
          (p3.axes)[0].thick = 2
          (p3.axes)[1].thick = 2
          (p3.axes)[2].thick = 2
          (p3.axes)[3].thick = 2
          (p3.axes)[3].ticklen = 0.01

;+
;         Refine minor ticks on horizontal axes. The default counts
;         for minor tick intervals are often a bit weird. Make them
;         one minor tick per month if possible.
;-
          numTicks = N_ELEMENTS((p3.axes)[0].tickValues)
          if (numTicks gt 1) then begin
              tickInterval = $
                  MEAN(((p3.axes)[0].tickValues)[1:numTicks - 1] - $
                       ((p3.axes)[0].tickValues)[0:numTicks - 2])
              if (tickInterval gt 28) then begin
                  tickMonths = ROUND(tickInterval / 30.4375)
                  xMinor = tickMonths - 1   ; minor ticks for months
              endif else begin
                  xMinor = tickInterval - 1 ; minor ticks for days
              endelse
              (p3.axes)[0].minor = xMinor
              (p3.axes)[2].minor = xMinor
          endif

          xRangeWidth = (p3.xRange)[1] - (p3.xRange)[0]
          yRangeWidth = (p3.yRange)[1] - (p3.yRange)[0]

          t3 = []

          for i = 0, N_ELEMENTS(inputFlag)-1 do begin
              p3_var = 'p3_' + STRING(STRCRA(i*2+1), FORMAT = '(i02)')
              t3_var = 't3_' +  STRING(STRCRA(i*2+1), FORMAT = '(i02)')
              p3_var2 = 'p3_' + STRING(STRCRA(i*2+2), FORMAT = '(i02)')
              t3_var2 = 't3_' +  STRING(STRCRA(i*2+2), FORMAT = '(i02)')
              if (inputFlag[i] eq 1) then begin
                 var_rgb = inputFlagName[i] + '_rgb'
                 var_POD = 'POD_' + inputFlagName[i]
                 var_FAR = 'FAR_' + inputFlagName[i]
                 ind = WHERE((SCOPE_VARFETCH(var_POD, /ENTER, $
                              LEVEL = 1)) ne ndv, count)
                 if (count gt 1) then begin
                    (SCOPE_VARFETCH(p3_var, /ENTER, LEVEL = 1)) = $
                                 PLOT(aggTimeAxis[ind], $
                                (SCOPE_VARFETCH(var_POD, /ENTER, LEVEL = 1))[ind], $
                                /OVERPLOT, $
                                COLOR = (SCOPE_VARFETCH(var_rgb, /ENTER, LEVEL = 1)), $
                                THICK = 2)
                    (SCOPE_VARFETCH(t3_var, /ENTER, LEVEL = 1)) = $
                                TEXT(aggTimeAxis[ind[count - 1]] + $
                                0.01 * xRangeWidth, $
                               (SCOPE_VARFETCH(var_POD, /ENTER, $
                                   LEVEL = 1))[ind[count - 1]] - $
                               0.01 * yRangeWidth, $
                               'POD ' + inputFlagName[i], $
                               /DATA, CLIP = 0, $
                               FONT_COLOR = (SCOPE_VARFETCH(var_rgb, /ENTER, $
                                   LEVEL = 1)), $
                               FONT_NAME = 'DejaVuSansBold', $
                               FONT_SIZE = 9, $
                               FONT_STYLE = 'Bold')
                    t3 = [t3, (SCOPE_VARFETCH(t3_var, /ENTER, LEVEL = 1))]
                 endif

                 ind = WHERE((SCOPE_VARFETCH(var_FAR, /ENTER, $
                              LEVEL = 1)) ne ndv, count)
                 if (count gt 1) then begin
                    (SCOPE_VARFETCH(p3_var2, /ENTER, LEVEL = 1)) = $
                                 PLOT(aggTimeAxis[ind], $
                                (SCOPE_VARFETCH(var_FAR, /ENTER, LEVEL = 1))[ind], $
                                /OVERPLOT, $
                                COLOR = (SCOPE_VARFETCH(var_rgb, /ENTER, LEVEL = 1)), $
                                THICK = 2)
                    (SCOPE_VARFETCH(t3_var2, /ENTER, LEVEL = 1)) = $
                                TEXT(aggTimeAxis[ind[count - 1]] + $
                                0.01 * xRangeWidth, $
                               (SCOPE_VARFETCH(var_FAR, /ENTER, $
                                   LEVEL = 1))[ind[count - 1]] - $
                               0.01 * yRangeWidth, $
                               'FAR ' + inputFlagName[i], $
                               /DATA, CLIP = 0, $
                               FONT_COLOR = (SCOPE_VARFETCH(var_rgb, /ENTER, $
                                   LEVEL = 1)), $
                               FONT_NAME = 'DejaVuSansBold', $
                               FONT_SIZE = 9, $
                               FONT_STYLE = 'Bold')
                    t3 = [t3, (SCOPE_VARFETCH(t3_var2, /ENTER, LEVEL = 1))]
                 endif

              endif
          endfor
          if cleanUp then ELIMINATE_TEXT_VERT_OVERLAP, t3


      endif

;+
;     Plot time series of Pearson correlation coefficient (PCorr).
;-

;+
;     Set up y axis.
;-
      killme = []
      for i = 0, N_ELEMENTS(inputFlag)-1 do begin
          if (inputFlag[i] eq 0) then CONTINUE
          var_PCorr = 'PCorr_' + inputFlagName[i]
          killme = [killme, (SCOPE_VARFETCH(var_PCorr, /ENTER, LEVEL = 1))]
             
      endfor
      ind = WHERE(killme ne ndv, count)

      if (count eq 0) then CONTINUE
      yMax = MAX(killme[ind]) < 10.0
      killme = !NULL

;+
;     Identify times for which there is data.
;-
      tFlag = INTARR(numAggEvents)
      for i = 0, N_ELEMENTS(inputFlag)-1 do begin
          var_PCorr = 'PCorr_' + inputFlagName[i]
          if (inputFlag[i] eq 1) then $
             tFlag = tFlag + ((SCOPE_VARFETCH(var_PCorr, /ENTER, LEVEL = 1)) ne ndv)
      endfor

      ind = WHERE(tFlag gt 0, count)
;      if (count le 1) then begin
;         print, 'numAggEvents too small'
;         stop
;      endif

      if (count gt 1) then begin

          if ISA(p4) then begin
              p4.window.SetCurrent
              p4.window.Erase
              current = 1
          endif else begin
              current = 0
          endelse

          p4 = PLOT(aggTimeAxis[ind], REPLICATE(ndv, count), $
                    TITLE = 'Correlation Coefficient (PCorr) vs. ' + $
                    '24-hour 12Z Observations!C' + $
                    subtitle, $
                    FONT_NAME = 'DejaVuSansBold', FONT_STYLE = 'Bold', $
                    XRANGE = [aggTimeAxis[ind[0]], $
                              aggTimeAxis[ind[count - 1]] + $
                              0.16 * (aggTimeAxis[ind[count - 1]] - $
                                      aggTimeAxis[ind[0]])], $
                    ;XSTYLE = 1, $
                    XTICKUNITS = xTickUnits, $
                    XMINOR = xMinor, $
                    XTICKFORMAT = 'LABEL_DATE', $
                    YTITLE = 'PCorr (dimensionless)', $
                    YRANGE = [0, yMax], $
                    POS = [0.1, 0.08, 0.95, 0.9], $
                    WINDOW_TITLE = 'Correlation Coefficent (PCorr)', $
                    DIMENSIONS = [960, 540], $
                    LOCATION = [200, 200], $
                    /NODATA, $
                    CURRENT = current)
          (p4.axes)[0].thick = 2
          (p4.axes)[1].thick = 2
          (p4.axes)[2].thick = 2
          (p4.axes)[3].thick = 2
          (p4.axes)[3].ticklen = 0.01

;+
;         Refine minor ticks on horizontal axes. The default counts
;         for minor tick intervals are often a bit weird. Make them
;         one minor tick per month if possible.
;-
          numTicks = N_ELEMENTS((p4.axes)[0].tickValues)
          if (numTicks gt 1) then begin
              tickInterval = $
                  MEAN(((p4.axes)[0].tickValues)[1:numTicks - 1] - $
                       ((p4.axes)[0].tickValues)[0:numTicks - 2])
              if (tickInterval gt 28) then begin
                  tickMonths = ROUND(tickInterval / 30.4375)
                  xMinor = tickMonths - 1    ; minor ticks for months
              endif else begin
                  xMinor = tickInterval -  1 ; minor ticks for days
              endelse
              (p4.axes)[0].minor = xMinor
              (p4.axes)[2].minor = xMinor
          endif

          xRangeWidth = (p4.xRange)[1] - (p4.xRange)[0]
          yRangeWidth = (p4.yRange)[1] - (p4.yRange)[0]

          t4 = []

          for i = 0, N_ELEMENTS(inputFlag)-1 do begin

              p4_var = 'p4_' + STRING(STRCRA(i+1), FORMAT = '(i02)')
              t4_var = 't4_' +  STRING(STRCRA(i+1), FORMAT = '(i02)')

              if NOT(inputFlag[i]) then CONTINUE

              var_rgb = inputFlagName[i] + '_rgb'
              var_PCorr = 'PCorr_' + inputFlagName[i]
              ind = WHERE((SCOPE_VARFETCH(var_PCorr, /ENTER, $
                                          LEVEL = 1)) ne ndv, count)
              if (count gt 1) then begin

                  (SCOPE_VARFETCH(p4_var, /ENTER, LEVEL = 1)) = $
                      PLOT(aggTimeAxis[ind], $
                           (SCOPE_VARFETCH(var_PCorr, /ENTER, $
                                           LEVEL = 1))[ind], $
                           /OVERPLOT, $
                           COLOR = (SCOPE_VARFETCH(var_rgb, /ENTER, $
                                                   LEVEL = 1)), $
                           THICK = 2)

                  (SCOPE_VARFETCH(t4_var, /ENTER, LEVEL = 1)) = $
                      TEXT(aggTimeAxis[ind[count - 1]] + $
                           0.01 * xRangeWidth, $
                           (SCOPE_VARFETCH(var_PCorr, /ENTER, $
                                           LEVEL = 1))[ind[count - 1]] - $
                           0.01 * yRangeWidth, $
                           inputFlagName[i], $
                           /DATA, CLIP = 0, $
                           FONT_COLOR = (SCOPE_VARFETCH(var_rgb, /ENTER, $
                                                        LEVEL = 1)), $
                           FONT_NAME = 'DejaVuSansBold', $
                           FONT_STYLE = 'Bold')

                  t4 = [t4, (SCOPE_VARFETCH(t4_var, /ENTER, LEVEL = 1))]

              endif

          endfor

          if cleanUp then ELIMINATE_TEXT_VERT_OVERLAP, t4

      endif




















;+
;     Plot time series of Spearman rank correlation coefficient (RCorr).
;-

;+
;     Set up y axis.
;-
      killme = []
      for i = 0, N_ELEMENTS(inputFlag)-1 do begin
          if (inputFlag[i] eq 0) then CONTINUE
          var_RCorr = 'RCorr_' + inputFlagName[i]
          killme = [killme, (SCOPE_VARFETCH(var_RCorr, /ENTER, LEVEL = 1))]
             
      endfor
      ind = WHERE(killme ne ndv, count)

      if (count eq 0) then CONTINUE
      yMax = MAX(killme[ind]) < 10.0
      killme = !NULL

;+
;     Identify times for which there is data.
;-
      tFlag = INTARR(numAggEvents)
      for i = 0, N_ELEMENTS(inputFlag)-1 do begin
          var_Rcorr = 'RCorr_' + inputFlagName[i]
          if (inputFlag[i] eq 1) then $
             tFlag = tFlag + ((SCOPE_VARFETCH(var_RCorr, /ENTER, LEVEL = 1)) ne ndv)
      endfor

      ind = WHERE(tFlag gt 0, count)
;      if (count le 1) then begin
;         print, 'numAggEvents too small'
;         stop
;      endif

      if (count gt 1) then begin

          if ISA(p5) then begin
              p5.window.SetCurrent
              p5.window.Erase
              current = 1
          endif else begin
              current = 0
          endelse

          p5 = PLOT(aggTimeAxis[ind], REPLICATE(ndv, count), $
                    TITLE = 'Rank Correlation (RCorr) vs. ' + $
                    '24-hour 12Z Observations!C' + $
                    subtitle, $
                    FONT_NAME = 'DejaVuSansBold', FONT_STYLE = 'Bold', $
                    XRANGE = [aggTimeAxis[ind[0]], $
                              aggTimeAxis[ind[count - 1]] + $
                              0.16 * (aggTimeAxis[ind[count - 1]] - $
                                      aggTimeAxis[ind[0]])], $
                    ;XSTYLE = 1, $
                    XTICKUNITS = xTickUnits, $
                    XMINOR = xMinor, $
                    XTICKFORMAT = 'LABEL_DATE', $
                    YTITLE = 'RCorr (dimensionless)', $
                    YRANGE = [0, yMax], $
                    POS = [0.1, 0.08, 0.95, 0.9], $
                    WINDOW_TITLE = 'Rank Correlation (RCorr)', $
                    DIMENSIONS = [960, 540], $
                    LOCATION = [200, 200], $
                    /NODATA, $
                    CURRENT = current)
          (p5.axes)[0].thick = 2
          (p5.axes)[1].thick = 2
          (p5.axes)[2].thick = 2
          (p5.axes)[3].thick = 2
          (p5.axes)[3].ticklen = 0.01

;+
;         Refine minor ticks on horizontal axes. The default counts
;         for minor tick intervals are often a bit weird. Make them
;         one minor tick per month if possible.
;-
          numTicks = N_ELEMENTS((p5.axes)[0].tickValues)
          if (numTicks gt 1) then begin
              tickInterval = $
                  MEAN(((p5.axes)[0].tickValues)[1:numTicks - 1] - $
                       ((p5.axes)[0].tickValues)[0:numTicks - 2])
              if (tickInterval gt 28) then begin
                  tickMonths = ROUND(tickInterval / 30.4375)
                  xMinor = tickMonths - 1    ; minor ticks for months
              endif else begin
                  xMinor = tickInterval -  1 ; minor ticks for days
              endelse
              (p5.axes)[0].minor = xMinor
              (p5.axes)[2].minor = xMinor
          endif

          xRangeWidth = (p5.xRange)[1] - (p5.xRange)[0]
          yRangeWidth = (p5.yRange)[1] - (p5.yRange)[0]

          t5 = []

          for i = 0, N_ELEMENTS(inputFlag)-1 do begin

              p5_var = 'p5_' + STRING(STRCRA(i+1), FORMAT = '(i02)')
              t5_var = 't5_' +  STRING(STRCRA(i+1), FORMAT = '(i02)')

              if NOT(inputFlag[i]) then CONTINUE

              var_rgb = inputFlagName[i] + '_rgb'
              var_RCorr = 'RCorr_' + inputFlagName[i]
              ind = WHERE((SCOPE_VARFETCH(var_RCorr, /ENTER, $
                                          LEVEL = 1)) ne ndv, count)
              if (count gt 1) then begin

                  (SCOPE_VARFETCH(p5_var, /ENTER, LEVEL = 1)) = $
                      PLOT(aggTimeAxis[ind], $
                           (SCOPE_VARFETCH(var_RCorr, /ENTER, $
                                           LEVEL = 1))[ind], $
                           /OVERPLOT, $
                           COLOR = (SCOPE_VARFETCH(var_rgb, /ENTER, $
                                                   LEVEL = 1)), $
                           THICK = 2)

                  (SCOPE_VARFETCH(t5_var, /ENTER, LEVEL = 1)) = $
                      TEXT(aggTimeAxis[ind[count - 1]] + $
                           0.01 * xRangeWidth, $
                           (SCOPE_VARFETCH(var_RCorr, /ENTER, $
                                           LEVEL = 1))[ind[count - 1]] - $
                           0.01 * yRangeWidth, $
                           inputFlagName[i], $
                           /DATA, CLIP = 0, $
                           FONT_COLOR = (SCOPE_VARFETCH(var_rgb, /ENTER, $
                                                        LEVEL = 1)), $
                           FONT_NAME = 'DejaVuSansBold', $
                           FONT_STYLE = 'Bold')

                  t5 = [t5, (SCOPE_VARFETCH(t5_var, /ENTER, LEVEL = 1))]

              endif

          endfor

          if cleanUp then ELIMINATE_TEXT_VERT_OVERLAP, t5

      endif

;+
;     Plot time series of Heidke Skill Score (HSS)
;-

;+
;     Set up y axis.
;-
      killme = []
      for i = 0, N_ELEMENTS(inputFlag)-1 do begin
          if (inputFlag[i] eq 0) then CONTINUE
          var_HSS = 'HSS_' + inputFlagName[i]
          killme = [killme, (SCOPE_VARFETCH(var_HSS, /ENTER, LEVEL = 1))]
             
      endfor
      ind = WHERE(killme ne ndv, count)

      if (count eq 0) then CONTINUE
      yMax = MAX(killme[ind]) < 1.5  ; should be less or equal to 1.0
      killme = !NULL

;+
;     Identify times for which there is data.
;-
      tFlag = INTARR(numAggEvents)
      for i = 0, N_ELEMENTS(inputFlag)-1 do begin
          var_HSS = 'HSS_' + inputFlagName[i]
          if (inputFlag[i] eq 1) then $
             tFlag = tFlag + ((SCOPE_VARFETCH(var_HSS, /ENTER, LEVEL = 1)) ne ndv)
      endfor

      ind = WHERE(tFlag gt 0, count)

      if (count gt 1) then begin

          if ISA(p6) then begin
              p6.window.SetCurrent
              p6.window.Erase
              current = 1
          endif else begin
              current = 0
          endelse

          p6 = PLOT(aggTimeAxis[ind], REPLICATE(ndv, count), $
                    TITLE = 'Heidke Skill Score (HSS) vs. ' + $
                    '24-hour 12Z Observations!C' + $
                    subtitle, $
                    FONT_NAME = 'DejaVuSansBold', FONT_STYLE = 'Bold', $
                    XRANGE = [aggTimeAxis[ind[0]], $
                              aggTimeAxis[ind[count - 1]] + $
                              0.16 * (aggTimeAxis[ind[count - 1]] - $
                                      aggTimeAxis[ind[0]])], $
                    ;XSTYLE = 1, $
                    XTICKUNITS = xTickUnits, $
                    XMINOR = xMinor, $
                    XTICKFORMAT = 'LABEL_DATE', $
                    YTITLE = 'HSS (dimensionless)', $
                    YRANGE = [0.0, yMax], $
                    POS = [0.1, 0.08, 0.95, 0.9], $
                    WINDOW_TITLE = 'Heidke Skill Score (HSS)', $
                    DIMENSIONS = [960, 540], $
                    LOCATION = [200, 200], $
                    /NODATA, $
                    CURRENT = current)
          (p6.axes)[0].thick = 2
          (p6.axes)[1].thick = 2
          (p6.axes)[2].thick = 2
          (p6.axes)[3].thick = 2
          (p6.axes)[3].ticklen = 0.01

;+
;         Refine minor ticks on horizontal axes. The default counts
;         for minor tick intervals are often a bit weird. Make them
;         one minor tick per month if possible.
;-
          numTicks = N_ELEMENTS((p6.axes)[0].tickValues)
          if (numTicks gt 1) then begin
              tickInterval = $
                  MEAN(((p6.axes)[0].tickValues)[1:numTicks - 1] - $
                       ((p6.axes)[0].tickValues)[0:numTicks - 2])
              if (tickInterval gt 28) then begin
                  tickMonths = ROUND(tickInterval / 30.4375)
                  xMinor = tickMonths - 1    ; minor ticks for months
              endif else begin
                  xMinor = tickInterval -  1 ; minor ticks for days
              endelse
              (p6.axes)[0].minor = xMinor
              (p6.axes)[2].minor = xMinor
          endif

          xRangeWidth = (p6.xRange)[1] - (p6.xRange)[0]
          yRangeWidth = (p6.yRange)[1] - (p6.yRange)[0]

          t6 = []

          for i = 0, N_ELEMENTS(inputFlag)-1 do begin

              p6_var = 'p6_' + STRING(STRCRA(i+1), FORMAT = '(i02)')
              t6_var = 't6_' +  STRING(STRCRA(i+1), FORMAT = '(i02)')

              if NOT(inputFlag[i]) then CONTINUE

              var_rgb = inputFlagName[i] + '_rgb'
              var_HSS = 'HSS_' + inputFlagName[i]
              ind = WHERE((SCOPE_VARFETCH(var_HSS, /ENTER, $
                                          LEVEL = 1)) ne ndv, count)
              if (count gt 1) then begin

                  (SCOPE_VARFETCH(p6_var, /ENTER, LEVEL = 1)) = $
                      PLOT(aggTimeAxis[ind], $
                           (SCOPE_VARFETCH(var_HSS, /ENTER, $
                                           LEVEL = 1))[ind], $
                           /OVERPLOT, $
                           COLOR = (SCOPE_VARFETCH(var_rgb, /ENTER, $
                                                   LEVEL = 1)), $
                           THICK = 2)

                  (SCOPE_VARFETCH(t6_var, /ENTER, LEVEL = 1)) = $
                      TEXT(aggTimeAxis[ind[count - 1]] + $
                           0.01 * xRangeWidth, $
                           (SCOPE_VARFETCH(var_HSS, /ENTER, $
                                           LEVEL = 1))[ind[count - 1]] - $
                           0.01 * yRangeWidth, $
                           inputFlagName[i], $
                           /DATA, CLIP = 0, $
                           FONT_COLOR = (SCOPE_VARFETCH(var_rgb, /ENTER, $
                                                        LEVEL = 1)), $
                           FONT_NAME = 'DejaVuSansBold', $
                           FONT_STYLE = 'Bold')

                  t6 = [t6, (SCOPE_VARFETCH(t6_var, /ENTER, LEVEL = 1))]

              endif

          endfor

          if cleanUp then ELIMINATE_TEXT_VERT_OVERLAP, t6

      endif




;+
;     Plot time series of Critical Success Index (CSI)
;-

;+
;     Set up y axis.
;-
      killme = []
      for i = 0, N_ELEMENTS(inputFlag)-1 do begin
          if (inputFlag[i] eq 0) then CONTINUE
          var_CSI = 'CSI_' + inputFlagName[i]
          killme = [killme, (SCOPE_VARFETCH(var_CSI, /ENTER, LEVEL = 1))]
             
      endfor
      ind = WHERE(killme ne ndv, count)

      if (count eq 0) then CONTINUE
      yMax = MAX(killme[ind]) < 1.5  ; should be less or equal to 1.0
      killme = !NULL

;+
;     Identify times for which there is data.
;-
      tFlag = INTARR(numAggEvents)
      for i = 0, N_ELEMENTS(inputFlag)-1 do begin
          var_CSI = 'CSI_' + inputFlagName[i]
          if (inputFlag[i] eq 1) then $
             tFlag = tFlag + ((SCOPE_VARFETCH(var_CSI, /ENTER, LEVEL = 1)) ne ndv)
      endfor

      ind = WHERE(tFlag gt 0, count)

      if (count gt 1) then begin

          if ISA(p7) then begin
              p7.window.SetCurrent
              p7.window.Erase
              current = 1
          endif else begin
              current = 0
          endelse

          p7 = PLOT(aggTimeAxis[ind], REPLICATE(ndv, count), $
                    TITLE = 'Critical Success Index (CSI) vs. ' + $
                    '24-hour 12Z Observations!C' + $
                    subtitle, $
                    FONT_NAME = 'DejaVuSansBold', FONT_STYLE = 'Bold', $
                    XRANGE = [aggTimeAxis[ind[0]], $
                              aggTimeAxis[ind[count - 1]] + $
                              0.16 * (aggTimeAxis[ind[count - 1]] - $
                                      aggTimeAxis[ind[0]])], $
                    ;XSTYLE = 1, $
                    XTICKUNITS = xTickUnits, $
                    XMINOR = xMinor, $
                    XTICKFORMAT = 'LABEL_DATE', $
                    YTITLE = 'CSI (dimensionless)', $
                    YRANGE = [0.0, yMax], $
                    POS = [0.1, 0.08, 0.95, 0.9], $
                    WINDOW_TITLE = 'Critical Success Index (CSI)', $
                    DIMENSIONS = [960, 540], $
                    LOCATION = [200, 200], $
                    /NODATA, $
                    CURRENT = current)
          (p7.axes)[0].thick = 2
          (p7.axes)[1].thick = 2
          (p7.axes)[2].thick = 2
          (p7.axes)[3].thick = 2
          (p7.axes)[3].ticklen = 0.01

;+
;         Refine minor ticks on horizontal axes. The default counts
;         for minor tick intervals are often a bit weird. Make them
;         one minor tick per month if possible.
;-
          numTicks = N_ELEMENTS((p7.axes)[0].tickValues)
          if (numTicks gt 1) then begin
              tickInterval = $
                  MEAN(((p7.axes)[0].tickValues)[1:numTicks - 1] - $
                       ((p7.axes)[0].tickValues)[0:numTicks - 2])
              if (tickInterval gt 28) then begin
                  tickMonths = ROUND(tickInterval / 30.4375)
                  xMinor = tickMonths - 1    ; minor ticks for months
              endif else begin
                  xMinor = tickInterval -  1 ; minor ticks for days
              endelse
              (p7.axes)[0].minor = xMinor
              (p7.axes)[2].minor = xMinor
          endif

          xRangeWidth = (p7.xRange)[1] - (p7.xRange)[0]
          yRangeWidth = (p7.yRange)[1] - (p7.yRange)[0]

          t7 = []

          for i = 0, N_ELEMENTS(inputFlag)-1 do begin

              p7_var = 'p7_' + STRING(STRCRA(i+1), FORMAT = '(i02)')
              t7_var = 't7_' +  STRING(STRCRA(i+1), FORMAT = '(i02)')

              if NOT(inputFlag[i]) then CONTINUE

              var_rgb = inputFlagName[i] + '_rgb'
              var_CSI = 'CSI_' + inputFlagName[i]
              ind = WHERE((SCOPE_VARFETCH(var_CSI, /ENTER, $
                                          LEVEL = 1)) ne ndv, count)
              if (count gt 1) then begin

                  (SCOPE_VARFETCH(p7_var, /ENTER, LEVEL = 1)) = $
                      PLOT(aggTimeAxis[ind], $
                           (SCOPE_VARFETCH(var_CSI, /ENTER, $
                                           LEVEL = 1))[ind], $
                           /OVERPLOT, $
                           COLOR = (SCOPE_VARFETCH(var_rgb, /ENTER, $
                                                   LEVEL = 1)), $
                           THICK = 2)

                  (SCOPE_VARFETCH(t7_var, /ENTER, LEVEL = 1)) = $
                      TEXT(aggTimeAxis[ind[count - 1]] + $
                           0.01 * xRangeWidth, $
                           (SCOPE_VARFETCH(var_CSI, /ENTER, $
                                           LEVEL = 1))[ind[count - 1]] - $
                           0.01 * yRangeWidth, $
                           inputFlagName[i], $
                           /DATA, CLIP = 0, $
                           FONT_COLOR = (SCOPE_VARFETCH(var_rgb, /ENTER, $
                                                        LEVEL = 1)), $
                           FONT_NAME = 'DejaVuSansBold', $
                           FONT_STYLE = 'Bold')

                  t7 = [t7, (SCOPE_VARFETCH(t7_var, /ENTER, LEVEL = 1))]

              endif

          endfor

          if cleanUp then ELIMINATE_TEXT_VERT_OVERLAP, t7

      endif




















      WAIT, 0.5

  endfor

  PNGImage = inputGroupAbbrev + $
             domainAbbrev + '_' + $
             FORMAT_FLOAT(eventThreshold_in) + 'in_' + $
              'EAF' + STRTRIM(STRING(eventAggFactor), 1) + '_' + $
             timeDomainAbbrev + '_' + $
             'MARE.png'
  PNGImage = outputDir + '/' + PNGImage
  p0.Save, PNGImage, WIDTH = 1920
  PRINT, 'Saved MARE time series to ' + PNGImage

  PNGImage = inputGroupAbbrev + $
             domainAbbrev + '_' + $
             FORMAT_FLOAT(eventThreshold_in) + 'in_' + $
              'EAF' + STRTRIM(STRING(eventAggFactor), 1) + '_' + $
             timeDomainAbbrev + '_' + $
             'GMMB.png'
  PNGImage = outputDir + '/' + PNGImage
  p1.Save, PNGImage, WIDTH = 1920
  PRINT, 'Saved GMMB time series to ' + PNGImage

  PNGImage = inputGroupAbbrev + $
             domainAbbrev + '_' + $
             FORMAT_FLOAT(eventThreshold_in) + 'in_' + $
              'EAF' + STRTRIM(STRING(eventAggFactor), 1) + '_' + $
             timeDomainAbbrev + '_' + $
             'SDLMB.png'
  PNGImage = outputDir + '/' + PNGImage
  p2.Save, PNGImage, WIDTH = 1920
  PRINT, 'Saved SDLMB time series to ' + PNGImage

  PNGImage = inputGroupAbbrev + $
             domainAbbrev + '_' + $
             FORMAT_FLOAT(eventThreshold_in) + 'in_' + $
              'EAF' + STRTRIM(STRING(eventAggFactor), 1) + '_' + $
             timeDomainAbbrev + '_' + $
             'POD_FAR.png'
  PNGImage = outputDir + '/' + PNGImage
  p3.Save, PNGImage, WIDTH = 1920
  PRINT, 'Saved POD/FAR time series to ' + PNGImage

  PNGImage = inputGroupAbbrev + $
             domainAbbrev + '_' + $
             FORMAT_FLOAT(eventThreshold_in) + 'in_' + $
              'EAF' + STRTRIM(STRING(eventAggFactor), 1) + '_' + $
             timeDomainAbbrev + '_' + $
             'PCorr.png'
  PNGImage = outputDir + '/' + PNGImage
  p4.Save, PNGImage, WIDTH = 1920
  PRINT, 'Saved PCorr time series to ' + PNGImage

  PNGImage = inputGroupAbbrev + $
             domainAbbrev + '_' + $
             FORMAT_FLOAT(eventThreshold_in) + 'in_' + $
              'EAF' + STRTRIM(STRING(eventAggFactor), 1) + '_' + $
             timeDomainAbbrev + '_' + $
             'RCorr.png'
  PNGImage = outputDir + '/' + PNGImage
  p5.Save, PNGImage, WIDTH = 1920
  PRINT, 'Saved RCorr time series to ' + PNGImage

  PNGImage = inputGroupAbbrev + $
             domainAbbrev + '_' + $
             FORMAT_FLOAT(eventThreshold_in) + 'in_' + $
              'EAF' + STRTRIM(STRING(eventAggFactor), 1) + '_' + $
             timeDomainAbbrev + '_' + $
             'HSS.png'
  PNGImage = outputDir + '/' + PNGImage
  p6.Save, PNGImage, WIDTH = 1920
  PRINT, 'Saved HSS time series to ' + PNGImage

  PNGImage = inputGroupAbbrev + $
             domainAbbrev + '_' + $
             FORMAT_FLOAT(eventThreshold_in) + 'in_' + $
              'EAF' + STRTRIM(STRING(eventAggFactor), 1) + '_' + $
             timeDomainAbbrev + '_' + $
             'CSI.png'
  PNGImage = outputDir + '/' + PNGImage
  p7.Save, PNGImage, WIDTH = 1920
  PRINT, 'Saved CSI time series to ' + PNGImage

  ;GOTO, SKIP_MAP


;+
; Generate comparisons to show skill measures at stations (i.e.,
; across the spatial dimension).
;-
  map_obsACCount = $
      MAKE_ARRAY(numStations, VALUE = ndvInt) ; observed events
  map_obsBDCount = $
      MAKE_ARRAY(numStations, VALUE = ndvInt) ; observed non-events

  for i = 0, N_ELEMENTS(inputFlag)-1 do begin
      if (inputFlag[i] eq 1) then begin
         var_map_a = 'map_a_' + inputFlagName[i]
         var_map_b = 'map_b_' + inputFlagName[i]
         var_map_c = 'map_c_' + inputFlagName[i]
         var_map_d = 'map_d_' + inputFlagName[i]
         var_map_GMMB = 'map_GMMB_' + inputFlagName[i]
         var_map_MARE = 'map_MARE_' + inputFlagName[i]
         var_map_SDLMB = 'map_SDLMB_' + inputFlagName[i]
         var_map_POD = 'map_POD_' + inputFlagName[i]
         var_map_FAR = 'map_FAR_' + inputFlagName[i]
         var_map_PCorr = 'map_PCorr_' + inputFlagName[i]
         var_map_RCorr = 'map_RCorr_' + inputFlagName[i]
         var_map_HSS = 'map_HSS_' + inputFlagName[i]
         var_map_CSI = 'map_CSI_' + inputFlagName[i]
         (SCOPE_VARFETCH(var_map_a, /ENTER, LEVEL = 1)) = $
            MAKE_ARRAY(numStations,  VALUE = ndv)
         (SCOPE_VARFETCH(var_map_b, /ENTER, LEVEL = 1)) = $
            MAKE_ARRAY(numStations,  VALUE = ndv)
         (SCOPE_VARFETCH(var_map_c, /ENTER, LEVEL = 1)) = $
            MAKE_ARRAY(numStations,  VALUE = ndv)
         (SCOPE_VARFETCH(var_map_d, /ENTER, LEVEL = 1)) = $
            MAKE_ARRAY(numStations,  VALUE = ndv)
         (SCOPE_VARFETCH(var_map_GMMB, /ENTER, LEVEL = 1)) = $
            MAKE_ARRAY(numStations,  VALUE = ndv)
         (SCOPE_VARFETCH(var_map_MARE, /ENTER, LEVEL = 1)) = $
            MAKE_ARRAY(numStations,  VALUE = ndv)
         (SCOPE_VARFETCH(var_map_SDLMB, /ENTER, LEVEL = 1)) = $
            MAKE_ARRAY(numStations,  VALUE = ndv)
         (SCOPE_VARFETCH(var_map_POD, /ENTER, LEVEL = 1)) = $
            MAKE_ARRAY(numStations,  VALUE = ndv)
         (SCOPE_VARFETCH(var_map_FAR, /ENTER, LEVEL = 1)) = $
            MAKE_ARRAY(numStations,  VALUE = ndv)
         (SCOPE_VARFETCH(var_map_PCorr, /ENTER, LEVEL = 1)) = $
            MAKE_ARRAY(numStations,  VALUE = ndv)
         (SCOPE_VARFETCH(var_map_RCorr, /ENTER, LEVEL = 1)) = $
            MAKE_ARRAY(numStations,  VALUE = ndv)
         (SCOPE_VARFETCH(var_map_HSS, /ENTER, LEVEL = 1)) = $
            MAKE_ARRAY(numStations,  VALUE = ndv)
         (SCOPE_VARFETCH(var_map_CSI, /ENTER, LEVEL = 1)) = $
            MAKE_ARRAY(numStations,  VALUE = ndv)
      endif
  endfor

  for sc = 0L, numStations - 1L do begin

;+
;     Extract precipitation observations for this site.
;-        
      sitePrecipObs_mm = REFORM(precipObs_mm[sc, *])
;      ind = WHERE(sitePrecipObs_mm eq ndv, count)
;      sitePrecipObs_mm = sitePrecipObs_mm * 1000.0
;      if (count gt 0) then sitePrecipObs_mm[ind] = ndv

;+
;     Inventory observed events and non-events.
;-
      ind = WHERE((sitePrecipObs_mm ne ndv) and $
                  (sitePrecipObs_mm le eventThreshold_mm), $
                  obsNonEventCount)

      ind = WHERE((sitePrecipObs_mm ne ndv) and $
                  (sitePrecipObs_mm gt eventThreshold_mm), $
                  obsEventCount)

      map_obsACCount[sc] = obsEventCount
      map_obsBDCount[sc] = obsNonEventCount

      for i = 0, N_ELEMENTS(inputFlag) - 1 do begin

          if (inputFlag[i] eq 1) then begin

              var_site = 'site' + inputFlagName[i]
              var_stations = inputFlagName[i] + '_stations'
              var_map_a = 'map_a_' + inputFlagName[i]
              var_map_b = 'map_b_' + inputFlagName[i]
              var_map_c = 'map_c_' + inputFlagName[i]
              var_map_d = 'map_d_' + inputFlagName[i]
              var_map_GMMB = 'map_GMMB_' + inputFlagName[i]
              var_map_MARE = 'map_MARE_' + inputFlagName[i]
              var_map_SDLMB = 'map_SDLMB_' + inputFlagName[i]
              var_map_POD = 'map_POD_' + inputFlagName[i]
              var_map_FAR = 'map_FAR_' + inputFlagName[i]
              var_map_PCorr = 'map_PCorr_' + inputFlagName[i]
              var_map_RCorr = 'map_RCorr_' + inputFlagName[i]
              var_map_HSS = 'map_HSS_' + inputFlagName[i]
              var_map_CSI = 'map_CSI_' + inputFlagName[i]

              (SCOPE_VARFETCH(var_site, /ENTER, LEVEL = 1)) = $
                  REFORM((SCOPE_VARFETCH(var_stations, $
                                         /ENTER, LEVEL = 1))[sc, *])
              EPVS_03, sitePrecipObs_mm, $
                       (SCOPE_VARFETCH(var_site, /ENTER, LEVEL = 1)), $
                       eventThreshold_mm, $
                       ndv, $
                       sc, $
                       (SCOPE_VARFETCH(var_map_a, /ENTER, LEVEL = 1)), $
                       (SCOPE_VARFETCH(var_map_b, /ENTER, LEVEL = 1)), $
                       (SCOPE_VARFETCH(var_map_c, /ENTER, LEVEL = 1)), $
                       (SCOPE_VARFETCH(var_map_d, /ENTER, LEVEL = 1)), $
                       (SCOPE_VARFETCH(var_map_GMMB, /ENTER, LEVEL = 1)), $
                       (SCOPE_VARFETCH(var_map_MARE, /ENTER, LEVEL = 1)), $
                       (SCOPE_VARFETCH(var_map_SDLMB, /ENTER, LEVEL = 1)), $
                       (SCOPE_VARFETCH(var_map_POD, /ENTER, LEVEL = 1)), $
                       (SCOPE_VARFETCH(var_map_FAR, /ENTER, LEVEL = 1)), $
                       (SCOPE_VARFETCH(var_map_PCorr, /ENTER, LEVEL = 1)), $
                       (SCOPE_VARFETCH(var_map_RCorr, /ENTER, LEVEL = 1)), $
                       (SCOPE_VARFETCH(var_map_HSS, /ENTER, LEVEL = 1)), $
                       (SCOPE_VARFETCH(var_map_CSI, /ENTER, LEVEL = 1))

          endif

      endfor

  endfor
 
  PRINT, 'MAP DATA GENERATED.  PLOT FOLLOWS'


;;  if NOT(ISA(NWMGridInfo)) then begin
;;
;;;+
;;;     Define the NWM grid/projection.
;;;-
;;      GET_NWM_GRID_PROJ_INFO, NWMGridInfo
;;
;;  endif

;;;+
;;; Get longitudes and latitudes of NWM cells, for regridding things to
;;; the NWM grid.
;;;-
;;  if (NOT(ISA(lonGrid_NWM)) or NOT(ISA(latGrid_NWM))) then begin
;;      PRINT, 'Getting NWM grid longitudes and latitudes.'
;;      GET_LON_LAT_OF_LCC_GRID_CENTERS, NWMGridInfo, $
;;                                       lonGrid_NWM, $
;;                                       latGrid_NWM
;;  endif
;;
;;  if (NOT(fullDomain) and (boxOrShape eq 1)) then begin
;;
;;      ind = WHERE((lonGrid_NWM ge subMinLon) and $
;;                  (latGrid_NWM ge subMinLat) and $
;;                  (lonGrid_NWM le subMaxLon) and $
;;                  (latGrid_NWM le subMaxLat), count)
;;      if (count eq 0) then STOP
;;      minCol = MIN(ind mod NWMGridInfo.nCols)
;;      maxCol = MAX(ind mod NWMGridInfo.nCols)
;;      minRow = MIN(ind / NWMGridInfo.nCols)
;;      maxRow = MAX(ind / NWMGridInfo.nCols)
;;
;;      ;; minCol = MIN(WHERE(lonGrid_NWM ge subMinLon) mod NWMGridInfo.nCols)
;;      ;; maxCol = MAX(WHERE(lonGrid_NWM le subMaxLon) mod NWMGridInfo.nCols)
;;      ;; minRow = MIN(WHERE(latGrid_NWM ge subMinLat) / NWMGridInfo.nCols)
;;      ;; maxRow = MAX(WHERE(latGrid_NWM le subMaxLat) / NWMGridInfo.nCols)
;;
;;      plotGridInfo = NWMGridInfo
;;      plotGridInfo.nCols = maxCol - minCol + 1L
;;      plotGridInfo.nRows = maxRow - minRow + 1L
;;      plotGridInfo.lon00 = lonGrid_NWM[minCol, minRow]
;;      plotGridInfo.lat00 = latGrid_NWM[minCol, minRow]
;;
;;  endif else plotGridInfo = NWMGridInfo


;+
; Generate GMMB maps for all input sources..
;-
  dummyGrid = MAKE_ARRAY(plotGridInfo.nCols, plotGridInfo.nRows, VALUE = ndv)
  PRINT, STRING(10B), 'Generating GMMB maps ...'

  for i = 0, N_ELEMENTS(inputFlag) - 1 do begin

      if (inputFlag[i] eq 1) then begin
         var_map_GMMB = 'map_GMMB_' + inputFlagName[i]
         ind = WHERE((SCOPE_VARFETCH(var_map_GMMB, /ENTER, LEVEL = 1)) ne ndv, count)
         if (count gt 0) then begin
            title = inputFlagName[i] + ' Mean Bias, ' + timeDomainStr + $
                  '!C!D' + subTitle
            PNGImage = inputFlagName[i] + '_GMMB_' + $
                       domainAbbrev + '_' + $
                       FORMAT_FLOAT(eventThreshold_in) + 'in_' + $
                       'EAF' + STRTRIM(STRING(eventAggFactor), 1) + $
                       '_' + timeDomainAbbrev + '.png'
            PNGImage = outputDir + '/' + PNGImage
            MAKE_LCC_MAP_PNG, dummyGrid, ndv, $
                              bEdges, bRed, bGrn, bBlu, $
                              plotGridInfo, $
                              title, $
                              'dimensionless', $
                              PNGImage, $
                              /SHOW_LOW, $
                              /SHOW_HIGH, $
                              POINT_LON = stationData[ind].longitude, $
                              POINT_LAT = stationData[ind].latitude, $
                              POINT_VAL =  $
                                 (SCOPE_VARFETCH(var_map_GMMB, /ENTER, $
                                                     LEVEL = 1))[ind], $
                              TARGET_IMAGE_SIZE = 1000, $
                              MAP_SHAPE_PATH = shapePathList, $
                              POINT_BIN_EDGES = bEdges, $
                              POINT_RED = bRed, $
                              POINT_GRN = bGrn, $
                              POINT_BLU = bBlu, $
                              /NO_GRID, $
                              /NO_CONTINENTS, $
                              /NO_USA, $
                              /BLACK_ON_WHITE

         endif
      endif
  endfor

;+
; Generate MARE maps for all input sources.
;-
  PRINT, STRING(10B), 'Generating MARE maps ...'

  for i = 0, N_ELEMENTS(inputFlag) - 1 do begin

      if (inputFlag[i] eq 1) then begin

         var_map_MARE = 'map_MARE_' + inputFlagName[i]

         ind = WHERE((SCOPE_VARFETCH(var_map_MARE, $
                                     /ENTER, LEVEL = 1)) ne ndv, count)
         if (count gt 0) then begin
            title = inputFlagName[i] + ' Mean Abs Rel Err, ' + timeDomainStr + $
                  '!C!D' + subTitle
            PNGImage = inputFlagName[i] + '_MARE_' + $
                       domainAbbrev + '_' + $
                       FORMAT_FLOAT(eventThreshold_in) + 'in_' + $
                       'EAF' + STRTRIM(STRING(eventAggFactor), 1) + '_' + $
                       timeDomainAbbrev + '.png'
            PNGImage = outputDir + '/' + PNGImage
            MAKE_LCC_MAP_PNG, dummyGrid, ndv, $
                              mEdges, mRed, mGrn, mBlu, $
                              plotGridInfo, $
                              title, $
                              'dimensionless', $
                              PNGImage, $
                              /SHOW_LOW, $
                              /SHOW_HIGH, $
                              POINT_LON = stationData[ind].longitude, $
                              POINT_LAT = stationData[ind].latitude, $
                              POINT_VAL =  $
                                 (SCOPE_VARFETCH(var_map_MARE, /ENTER, $
                                                     LEVEL = 1))[ind], $
                              TARGET_IMAGE_SIZE = 1000, $
                              MAP_SHAPE_PATH = shapePathList, $
                              POINT_BIN_EDGES = mEdges, $
                              POINT_RED = mRed, $
                              POINT_GRN = mGrn, $
                              POINT_BLU = mBlu, $
                              /NO_GRID, $
                              /NO_CONTINENTS, $
                              /NO_USA, $
                              /BLACK_ON_WHITE

        endif

      endif

  endfor


;+
; Determine SDLMB range.
;-
  PRINT, STRING(10B), 'Generating SDLMB maps ...'
  maxSDLMB = -1.0
  for i = 0, N_ELEMENTS(inputFlag) - 1 do begin
      if NOT(inputFlag[i]) then CONTINUE
      var_map_SDLMB = 'map_SDLMB_' + inputFlagName[i]
      ind = WHERE((SCOPE_VARFETCH(var_map_SDLMB, /ENTER, LEVEL = 1)) ne ndv, $
                  count)
      if (count gt 0) then begin
          maxSDLMB = maxSDLMB > MAX((SCOPE_VARFETCH(var_map_SDLMB, $
                                                    /ENTER, $
                                                    LEVEL = 1))[ind])
      endif
  endfor
  maxSDLMB = CEIL(maxSDLMB * 10.0) / 10.0
  sEdges = FINDGEN(8) / 7.0 * maxSDLMB

;+
; Generate SDLMB maps for all input sources.
;-
  for i = 0, N_ELEMENTS(inputFlag) - 1 do begin

      if (inputFlag[i] eq 1) then begin

         var_map_SDLMB = 'map_SDLMB_' + inputFlagName[i]
         ind = WHERE((SCOPE_VARFETCH(var_map_SDLMB, /ENTER, LEVEL = 1)) ne ndv, count)
         if (count gt 0) then begin

             title = inputFlagName[i] + ' Std. Dev. in Log Mult. Bias, ' + $
                     timeDomainStr + $
                     '!C!D' + subTitle
             PNGImage = inputFlagName[i] + '_SDLMB_' + $
                        domainAbbrev + '_' + $
                        FORMAT_FLOAT(eventThreshold_in) + 'in_' + $
                        'EAF' + STRTRIM(STRING(eventAggFactor), 1) + '_' + $
                        timeDomainAbbrev + '.png'
             PNGImage = outputDir + '/' + PNGImage
             MAKE_LCC_MAP_PNG, dummyGrid, ndv, $
                               sEdges, mRed, mGrn, mBlu, $
                               plotGridInfo, $
                               title, $
                               'dimensionless', $
                               PNGImage, $
                               /SHOW_LOW, $
                               /SHOW_HIGH, $
                               POINT_LON = stationData[ind].longitude, $
                               POINT_LAT = stationData[ind].latitude, $
                               POINT_VAL =  $
                               (SCOPE_VARFETCH(var_map_SDLMB, /ENTER, $
                                               LEVEL = 1))[ind], $
                               TARGET_IMAGE_SIZE = 1000, $
                               MAP_SHAPE_PATH = shapePathList, $
                               POINT_BIN_EDGES = sEdges, $
                               POINT_RED = mRed, $
                               POINT_GRN = mGrn, $
                               POINT_BLU = mBlu, $
                               /NO_GRID, $
                               /NO_CONTINENTS, $
                               /NO_USA, $
                               /BLACK_ON_WHITE

         endif

     endif

  endfor

;+
; Generate POD maps.
;-
  PRINT, STRING(10B), 'Generating POD maps ...'
  dummyGrid = MAKE_ARRAY(plotGridInfo.nCols, plotGridInfo.nRows, VALUE = ndv)

  for i = 0, N_ELEMENTS(inputFlag) - 1 do begin

      if NOT(inputFlag[i]) then CONTINUE

         var_map_POD = 'map_POD_' + inputFlagName[i]
         ind = WHERE((SCOPE_VARFETCH(var_map_POD, /ENTER, LEVEL = 1)) ne ndv, $
                     count)

         if (count gt 0) then begin

            title = inputFlagName[i] + ' Probability of Detection, ' + $
                    timeDomainStr + $
                    '!C!D' + subTitle

            PNGImage = inputFlagName[i] + '_POD_' + $
                       domainAbbrev + '_' + $
                       FORMAT_FLOAT(eventThreshold_in) + 'in_' + $
                       'EAF' + STRTRIM(STRING(eventAggFactor), 1) + $
                       '_' + timeDomainAbbrev + '.png'
            PNGImage = outputDir + '/' + PNGImage

            MAKE_LCC_MAP_PNG, dummyGrid, ndv, $
                              podEdges, podRed, podGrn, podBlu, $
                              plotGridInfo, $
                              title, $
                              'dimensionless', $
                              PNGImage, $
                              /SHOW_LOW, $
                              /SHOW_HIGH, $
                              POINT_LON = stationData[ind].longitude, $
                              POINT_LAT = stationData[ind].latitude, $
                              POINT_VAL =  $
                                 (SCOPE_VARFETCH(var_map_POD, /ENTER, $
                                                 LEVEL = 1))[ind], $
                              TARGET_IMAGE_SIZE = 1000, $
                              MAP_SHAPE_PATH = shapePathList, $
                              POINT_BIN_EDGES = podEdges, $
                              POINT_RED = podRed, $
                              POINT_GRN = podGrn, $
                              POINT_BLU = podBlu, $
                              /NO_GRID, $
                              /NO_CONTINENTS, $
                              /NO_USA, $
                              /BLACK_ON_WHITE
        endif

  endfor

;+
; Generate FAR maps.
;-
  PRINT, STRING(10B), 'Generating FAR maps ...'
  dummyGrid = MAKE_ARRAY(plotGridInfo.nCols, plotGridInfo.nRows, VALUE = ndv)

  for i = 0, N_ELEMENTS(inputFlag) - 1 do begin

      if NOT(inputFlag[i]) then CONTINUE

         var_map_FAR = 'map_FAR_' + inputFlagName[i]
         ind = WHERE((SCOPE_VARFETCH(var_map_FAR, /ENTER, LEVEL = 1)) ne ndv, $
                     count)

         if (count gt 0) then begin

            title = inputFlagName[i] + ' False Alarm Ratio, ' + $
                    timeDomainStr + $
                    '!C!D' + subTitle

            PNGImage = inputFlagName[i] + '_FAR_' + $
                       domainAbbrev + '_' + $
                       FORMAT_FLOAT(eventThreshold_in) + 'in_' + $
                       'EAF' + STRTRIM(STRING(eventAggFactor), 1) + $
                       '_' + timeDomainAbbrev + '.png'
            PNGImage = outputDir + '/' + PNGImage

            MAKE_LCC_MAP_PNG, dummyGrid, ndv, $
                              farEdges, farRed, farGrn, farBlu, $
                              plotGridInfo, $
                              title, $
                              'dimensionless', $
                              PNGImage, $
                              /SHOW_LOW, $
                              /SHOW_HIGH, $
                              POINT_LON = stationData[ind].longitude, $
                              POINT_LAT = stationData[ind].latitude, $
                              POINT_VAL =  $
                                 (SCOPE_VARFETCH(var_map_FAR, /ENTER, $
                                                 LEVEL = 1))[ind], $
                              TARGET_IMAGE_SIZE = 1000, $
                              MAP_SHAPE_PATH = shapePathList, $
                              POINT_BIN_EDGES = farEdges, $
                              POINT_RED = farRed, $
                              POINT_GRN = farGrn, $
                              POINT_BLU = farBlu, $
                              /NO_GRID, $
                              /NO_CONTINENTS, $
                              /NO_USA, $
                              /BLACK_ON_WHITE
        endif

  endfor

;+
; Generate PCorr maps.
;-
  PRINT, STRING(10B), 'Generating PCorr maps ...'
  dummyGrid = MAKE_ARRAY(plotGridInfo.nCols, plotGridInfo.nRows, VALUE = ndv)

  for i = 0, N_ELEMENTS(inputFlag) - 1 do begin

      if NOT(inputFlag[i]) then CONTINUE

         var_map_PCorr = 'map_PCorr_' + inputFlagName[i]
         ind = WHERE((SCOPE_VARFETCH(var_map_PCorr, /ENTER, LEVEL = 1)) ne ndv, $
                     count)

         if (count gt 0) then begin

            title = inputFlagName[i] + ' Correlation Coeffiecent, ' + $
                    timeDomainStr + $
                    '!C!D' + subTitle

            PNGImage = inputFlagName[i] + '_PCorr_' + $
                       domainAbbrev + '_' + $
                       FORMAT_FLOAT(eventThreshold_in) + 'in_' + $
                       'EAF' + STRTRIM(STRING(eventAggFactor), 1) + $
                       '_' + timeDomainAbbrev + '.png'
            PNGImage = outputDir + '/' + PNGImage

            MAKE_LCC_MAP_PNG, dummyGrid, ndv, $
                              podEdges, podRed, podGrn, podBlu, $
                              plotGridInfo, $
                              title, $
                              'dimensionless', $
                              PNGImage, $
                              /SHOW_LOW, $
                              /SHOW_HIGH, $
                              POINT_LON = stationData[ind].longitude, $
                              POINT_LAT = stationData[ind].latitude, $
                              POINT_VAL =  $
                                 (SCOPE_VARFETCH(var_map_PCorr, /ENTER, $
                                                 LEVEL = 1))[ind], $
                              TARGET_IMAGE_SIZE = 1000, $
                              MAP_SHAPE_PATH = shapePathList, $
                              POINT_BIN_EDGES = podEdges, $
                              POINT_RED = podRed, $
                              POINT_GRN = podGrn, $
                              POINT_BLU = podBlu, $
                              /NO_GRID, $
                              /NO_CONTINENTS, $
                              /NO_USA, $
                              /BLACK_ON_WHITE
        endif

  endfor

;+
; Generate RCorr maps.
;-
  PRINT, STRING(10B), 'Generating RCorr maps ...'
  dummyGrid = MAKE_ARRAY(plotGridInfo.nCols, plotGridInfo.nRows, VALUE = ndv)

  for i = 0, N_ELEMENTS(inputFlag) - 1 do begin

      if NOT(inputFlag[i]) then CONTINUE

         var_map_RCorr = 'map_RCorr_' + inputFlagName[i]
         ind = WHERE((SCOPE_VARFETCH(var_map_RCorr, $
                                     /ENTER, LEVEL = 1)) ne ndv, count)

         if (count gt 0) then begin

            title = inputFlagName[i] + ' Rank Correlation, ' + $
                    timeDomainStr + $
                    '!C!D' + subTitle

            PNGImage = inputFlagName[i] + '_RCorr_' + $
                       domainAbbrev + '_' + $
                       FORMAT_FLOAT(eventThreshold_in) + 'in_' + $
                       'EAF' + STRTRIM(STRING(eventAggFactor), 1) + $
                       '_' + timeDomainAbbrev + '.png'
            PNGImage = outputDir + '/' + PNGImage

            MAKE_LCC_MAP_PNG, dummyGrid, ndv, $
                              podEdges, podRed, podGrn, podBlu, $
                              plotGridInfo, $
                              title, $
                              'dimensionless', $
                              PNGImage, $
                              /SHOW_LOW, $
                              /SHOW_HIGH, $
                              POINT_LON = stationData[ind].longitude, $
                              POINT_LAT = stationData[ind].latitude, $
                              POINT_VAL =  $
                                 (SCOPE_VARFETCH(var_map_RCorr, /ENTER, $
                                                 LEVEL = 1))[ind], $
                              TARGET_IMAGE_SIZE = 1000, $
                              MAP_SHAPE_PATH = shapePathList, $
                              POINT_BIN_EDGES = podEdges, $
                              POINT_RED = podRed, $
                              POINT_GRN = podGrn, $
                              POINT_BLU = podBlu, $
                              /NO_GRID, $
                              /NO_CONTINENTS, $
                              /NO_USA, $
                              /BLACK_ON_WHITE
        endif

  endfor

  dummyGrid = !NULL

;+
; Generate HSS maps.
;-
  PRINT, STRING(10B), 'Generating HSS maps ...'
  dummyGrid = MAKE_ARRAY(plotGridInfo.nCols, plotGridInfo.nRows, VALUE = ndv)

  for i = 0, N_ELEMENTS(inputFlag) - 1 do begin

      if NOT(inputFlag[i]) then CONTINUE

         var_map_RCorr = 'map_HSS_' + inputFlagName[i]
         ind = WHERE((SCOPE_VARFETCH(var_map_HSS, $
                                     /ENTER, LEVEL = 1)) ne ndv, count)

         if (count gt 0) then begin

            title = inputFlagName[i] + ' Heidke Skill Score, ' + $
                    timeDomainStr + $
                    '!C!D' + subTitle

            PNGImage = inputFlagName[i] + '_HSS_' + $
                       domainAbbrev + '_' + $
                       FORMAT_FLOAT(eventThreshold_in) + 'in_' + $
                       'EAF' + STRTRIM(STRING(eventAggFactor), 1) + $
                       '_' + timeDomainAbbrev + '.png'
            PNGImage = outputDir + '/' + PNGImage

            MAKE_LCC_MAP_PNG, dummyGrid, ndv, $
                              hssEdges, hssRed, hssGrn, hssBlu, $
                              plotGridInfo, $
                              title, $
                              'dimensionless', $
                              PNGImage, $
                              /SHOW_LOW, $
                              /SHOW_HIGH, $
                              POINT_LON = stationData[ind].longitude, $
                              POINT_LAT = stationData[ind].latitude, $
                              POINT_VAL =  $
                                 (SCOPE_VARFETCH(var_map_HSS, /ENTER, $
                                                 LEVEL = 1))[ind], $
                              TARGET_IMAGE_SIZE = 1000, $
                              MAP_SHAPE_PATH = shapePathList, $
                              POINT_BIN_EDGES = hssEdges, $
                              POINT_RED = hssRed, $
                              POINT_GRN = hssGrn, $
                              POINT_BLU = hssBlu, $
                              /NO_GRID, $
                              /NO_CONTINENTS, $
                              /NO_USA, $
                              /BLACK_ON_WHITE
        endif

  endfor

  dummyGrid = !NULL


;+
; Generate CSI maps.
;-
  PRINT, STRING(10B), 'Generating CSI maps ...'
  dummyGrid = MAKE_ARRAY(plotGridInfo.nCols, plotGridInfo.nRows, VALUE = ndv)

  for i = 0, N_ELEMENTS(inputFlag) - 1 do begin

      if NOT(inputFlag[i]) then CONTINUE

         var_map_RCorr = 'map_CSI_' + inputFlagName[i]
         ind = WHERE((SCOPE_VARFETCH(var_map_CSI, $
                                     /ENTER, LEVEL = 1)) ne ndv, count)

         if (count gt 0) then begin

            title = inputFlagName[i] + ' Critical Success Index, ' + $
                    timeDomainStr + $
                    '!C!D' + subTitle

            PNGImage = inputFlagName[i] + '_CSI_' + $
                       domainAbbrev + '_' + $
                       FORMAT_FLOAT(eventThreshold_in) + 'in_' + $
                       'EAF' + STRTRIM(STRING(eventAggFactor), 1) + $
                       '_' + timeDomainAbbrev + '.png'
            PNGImage = outputDir + '/' + PNGImage

            MAKE_LCC_MAP_PNG, dummyGrid, ndv, $
                              csiEdges, csiRed, csiGrn, csiBlu, $
                              plotGridInfo, $
                              title, $
                              'dimensionless', $
                              PNGImage, $
                              /SHOW_LOW, $
                              /SHOW_HIGH, $
                              POINT_LON = stationData[ind].longitude, $
                              POINT_LAT = stationData[ind].latitude, $
                              POINT_VAL =  $
                                 (SCOPE_VARFETCH(var_map_CSI, /ENTER, $
                                                 LEVEL = 1))[ind], $
                              TARGET_IMAGE_SIZE = 1000, $
                              MAP_SHAPE_PATH = shapePathList, $
                              POINT_BIN_EDGES = csiEdges, $
                              POINT_RED = csiRed, $
                              POINT_GRN = csiGrn, $
                              POINT_BLU = csiBlu, $
                              /NO_GRID, $
                              /NO_CONTINENTS, $
                              /NO_USA, $
                              /BLACK_ON_WHITE
        endif

  endfor

  dummyGrid = !NULL

;+
; Output CSV files for MARE, GMMB, SDLMB, POD, and FAR 
; at station locations in one loop
;
  PRINT, STRING(10B), 'Outputing CSV files ...'
  statsName = ['GMMB', 'MARE', 'SDLMB', 'POD', 'FAR', 'PCorr', 'RCorr', 'HSS', 'CSI']

  for k = 0, N_ELEMENTS(statsName) - 1 do begin

      for i = 0, N_ELEMENTS(inputFlag) - 1 do begin

          if NOT(inputFlag[i]) then CONTINUE

          var_map_STATS = 'map_' + statsName[k] + '_' + inputFlagName[i]

          ind = WHERE((SCOPE_VARFETCH(var_map_STATS, /ENTER, LEVEL=1)) ne ndv, $
                      count)

          for j = 0, count - 1 do begin

              if (j eq 0) then begin
                  CSVFileName = inputFlagName[i] + '_' + statsName[k] + '_' + $
                                domainAbbrev + '_' + $
                                FORMAT_FLOAT(eventThreshold_in) + 'in_' + $
                                'EAF' + STRTRIM(STRING(eventAggFactor), 1) + $
                                '_' + timeDomainAbbrev + '.csv'
                  CSVFileName = outputDir + '/' + CSVFileName 
                  OPENW, lun, CSVFileName, /GET_LUN
                  PRINTF, lun, 'lon,lat,' + statsName[k] + ',station_id'
              endif

              PRINTF, lun, $
                      STRCOMPRESS(STRING(stationData[ind[j]].longitude, $
                                         FORMAT = '(F10.5)'), $
                                  /REMOVE_ALL) + ',' + $
                      STRCOMPRESS(STRING(stationData[ind[j]].latitude, $
                                         FORMAT = '(F9.5)'), $
                                  /REMOVE_ALL) + ',' + $
                      STRCRA((SCOPE_VARFETCH(var_map_STATS, $
                                        /ENTER, LEVEL = 1))[ind[j]]) + ',' + $
                      stationData[ind[j]].station_id

              if (j eq (count - 1)) then FREE_LUN, lun

          endfor

      endfor

  endfor


SKIP_MAP:

;+
; Generate output files of stats time series for external analyses/ploting
; NOTE:  make sure no space in timeDomainAbbrev and domainName
;-
  
  PRINT, STRING(10B), 'Outputing time series to a text file ...'
  stats_fname = inputGroupAbbrev + timeDomainAbbrev + $
                '_Threshold' + FORMAT_FLOAT(eventThreshold_in) + 'in' + $ 
                '_EAF' + STRTRIM(STRING(eventAggFactor), 1) + $
                '_' + domainAbbrev + '.txt'
  stats_fname = outputDir + '/' + stats_fname
  PRINT, stats_fname
  OPENW, lun, stats_fname, /GET_LUN
  for i = 0, N_ELEMENTS(inputFlag)-1 do begin
      if (inputFlag[i] eq 1) then begin
          PRINTF, lun, 'Statistics for ', inputFlagName[i],':'
          PRINTF, lun, $
          FORMAT='(2x,"YMDH",11x,"MARE",9x,"POD",9x,"FAR",11x,"GMMB",9x,"SDLMB",8x,"PCORR",8x,"RCorr",8x,"HSS", 9x, "CSI")'
          mare = SCOPE_VARFETCH('MARE_' + inputFlagName[i])
          pod = SCOPE_VARFETCH('POD_' + inputFlagName[i])
          far = SCOPE_VARFETCH('FAR_' + inputFlagName[i])
          gmmb = SCOPE_VARFETCH('GMMB_' + inputFlagName[i])
          sdlmb = SCOPE_VARFETCH('SDLMB_' + inputFlagName[i])
          pcorr = SCOPE_VARFETCH('PCorr_' + inputFlagName[i])
          rcorr = SCOPE_VARFETCH('RCorr_' + inputFlagName[i])
          hss = SCOPE_VARFETCH('HSS_' + inputFlagName[i])
          csi = SCOPE_VARFETCH('CSI_' + inputFlagName[i])
          for j = 0, N_ELEMENTS(aggTimeAxis)-1 do begin

                 PRINTF, lun, FORMAT = '(12A,8F13.4)', $
                         JULIAN_TO_YYYYMMDDHH(aggTimeAxis[j]), $
                         mare[j],pod[j],far[j],gmmb[j],sdlmb[j],pcorr[j],rcorr[j],hss[j],csi[j]
          endfor

          ind_mare = WHERE(mare ne ndv, count_mare)
          ind_pod = WHERE(pod ne ndv, count_pod)
          ind_far = WHERE(far ne ndv, count_far)
          ind_gmmb = WHERE(gmmb ne ndv, count_gmmb)
          ind_sdlmb = WHERE(sdlmb ne ndv, count_sdlmb)
          ind_pcorr = WHERE(pcorr ne ndv, count_pcorr)
          ind_rcorr = WHERE(rcorr ne ndv, count_rcorr)
          ind_hss = WHERE(hss ne ndv, count_hss)
          ind_csi = WHERE(csi ne ndv, count_csi)
          PRINTF, lun, FORMAT = '(3X,"MEAN:",1X, 9F13.4)', $
                  MEAN(mare[ind_mare]),MEAN(pod[ind_pod]),MEAN(far[ind_far]),$
                  MEAN(gmmb[ind_gmmb]),MEAN(sdlmb[ind_sdlmb]), $
                  MEAN(pcorr[ind_pcorr]), MEAN(rcorr[ind_rcorr]),$
                  MEAN(hss[ind_hss]), MEAN(csi[ind_csi])
          PRINTF, lun, STRING(10B)
      endif
  endfor

  stat_var=MAKE_ARRAY(9,N_ELEMENTS(inputFlag), N_ELEMENTS(aggTimeAxis),VALUE=-88888.00)
  stat_name=MAKE_ARRAY(9,VALUE='')
  data_name=MAKE_ARRAY(N_ELEMENTS(inputFlag),VALUE='')
  k = 0  ; number of selected input
  for i = 0, N_ELEMENTS(inputFlag)-1 do begin
      
      if (inputFlag[i] eq 1) then begin
          stat_var[0,k,*] = SCOPE_VARFETCH('MARE_' + inputFlagName[i])
          stat_var[1,k,*] = SCOPE_VARFETCH('POD_' + inputFlagName[i])
          stat_var[2,k,*] = SCOPE_VARFETCH('FAR_' + inputFlagName[i])
          stat_var[3,k,*] = SCOPE_VARFETCH('GMMB_' + inputFlagName[i])
          stat_var[4,k,*] = SCOPE_VARFETCH('SDLMB_' + inputFlagName[i])
          stat_var[5,k,*] = SCOPE_VARFETCH('PCorr_' + inputFlagName[i])
          stat_var[6,k,*] = SCOPE_VARFETCH('RCorr_' + inputFlagName[i])
          stat_var[7,k,*] = SCOPE_VARFETCH('HSS_' + inputFlagName[i])
          stat_var[8,k,*] = SCOPE_VARFETCH('CSI_' + inputFlagName[i])
          stat_name[0] = 'MARE'
          stat_name[1] = 'POD'
          stat_name[2] = 'FAR'
          stat_name[3] = 'GMMB'
          stat_name[4] = 'SDLMB'
          stat_name[5] = 'PCorr'
          stat_name[6] = 'RCorr'
          stat_name[7] = 'HSS'
          stat_name[8] = 'CSI'
          data_name[k] = inputFlagName[i]
          
         
          k = k + 1
      endif
  endfor

  PRINTF, lun, "Statistics arranged by category"
  for l = 0, 8 do begin ; nine statistics
          PRINTF, lun, stat_name[l]
          PRINTF, lun, FORMAT = '("YYYYMMDDHH ",4x, 13A-13)',  (data_name[0:k])
;          PRINTF, lun, FORMAT = '("YYYYMMDDHH ",4x, 13A-13)',  data_name[0], data_name[1], $
;                  data_name[2],data_name[3],data_name[4],data_name[5],data_name[6]
;             PRINTF, lun, JULIAN_TO_YYYYMMDDHH(aggTimeAxis), stat_var[l,0:k-1]
          for j = 0, N_ELEMENTS(aggTimeAxis)-1 do begin
             PRINTF, lun, FORMAT = '(12A,(F11.3))', JULIAN_TO_YYYYMMDDHH(aggTimeAxis[j]), $
                     (stat_var[l,0:k-1,j])
;                     stat_var[l,0,j], stat_var[l,1,j],stat_var[l,2,j], $
;                     stat_var[l,3,j], stat_var[l,4,j],stat_var[l,5,j],stat_var[l,6,j], $
;                     stat_var[l,7,j[, stat_var[l,8,j]
          endfor
          PRINTF, lun, STRING(10B)
  endfor

  CLOSE, lun


  PRINT, STRING(10B), 'All Done!'
          


end
