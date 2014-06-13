PRO RAD_MAP_DATA2ASCII,date                                     $
    ,TIME               = time                                  $
    ,NORTH              = north                                 $
    ,SOUTH              = south                                 $
    ,MIN_LAT            = min_lat                               $
    ,INT_HEMI           = int_hemi                              $
    ,STEPTIME           = timeStep                              $
    ,STATUS             = status                                $
    ,OUTPUT_FILE        = output_file                           $
    ,LOG_FILE           = log_file

COMMON rad_data_blk
status  = 1
IF N_ELEMENTS(date) EQ 1 THEN date = [date,date]
IF N_ELEMENTS(time) EQ 0 THEN time = [0000,2400]
IF N_ELEMENTS(time) EQ 1 THEN time = [time,time]
IF ~KEYWORD_SET(output_file) THEN output_file = 'superdarn.txt'
IF ~KEYWORD_SET(timeStep) THEN timeStep = 0
IF ~KEYWORD_SET(min_lat) THEN min_lat = 50.

; check int_hemi and north(0) and south(1)

IF ~KEYWORD_SET(int_hemi) THEN BEGIN 
    IF KEYWORD_SET(north) THEN BEGIN
        int_hemi = 0.
    ENDIF ELSE BEGIN
        IF KEYWORD_SET(south) THEN BEGIN
            int_hemi = 1.
        ENDIF ELSE BEGIN
            int_hemi = 0.
        ENDELSE
    ENDELSE
ENDIF

SFJUL,date,time,sJul,fJul

IF int_hemi EQ 0 THEN readHemi =  1
IF int_hemi EQ 1 THEN readHemi = -1

RAD_MAP_READ,date,TIME=time,HEMISPHERE=readHemi

IF rad_map_info[int_hemi].nrecs EQ 0L THEN BEGIN
    status=2
    RETURN
ENDIF

IF fJul LT sJul THEN fJul = sJul

avail_juls  = (*RAD_MAP_DATA[int_hemi]).sjuls
good        = WHERE(avail_juls GE sJul AND avail_juls LE fJul,cnt)
IF cnt EQ 0 THEN BEGIN
    nMaps = 0
ENDIF ELSE BEGIN
    avail_juls = avail_juls[good]
    ;Calculate vector of time steps.
    IF KEYWORD_SET(timeStep) THEN BEGIN
        dt          = fJul -sJul
        dt_minutes  = ROUND(dt*24.*60.)
        nMaps       = CEIL(dt_minutes / timeStep) + 1
        minute_vec  = (DINDGEN(nMaps) * timeStep)
        jul_vec     = minute_vec/(60.*24.) + sJul

        good        = WHERE(jul_vec LE fJul)
        jul_vec     = jul_vec[good]
        nMaps       = N_ELEMENTS(jul_vec)

        IF time[0] EQ 0 THEN BEGIN
            min_diff_minutes = MIN(ABS(sJul-avail_juls),inx) * 24. * 60.
            IF min_diff_minutes LE 5 THEN jul_vec[0] = avail_juls[inx]
        ENDIF
        IF time[1] EQ 0 OR time[1] EQ 2359 THEN BEGIN
            min_diff_minutes = MIN(ABS(fJul-avail_juls),inx) * 24. * 60.
            IF min_diff_minutes LE 5 THEN jul_vec[nMaps-1] = avail_juls[inx]
        ENDIF
    ENDIF ELSE BEGIN
        jul_vec     = (*RAD_MAP_DATA[int_hemi]).sjuls
        good        = WHERE(jul_vec GE sJul AND jul_vec LE fJul,cnt)
        IF CNT GT 0 THEN BEGIN 
            jul_vec = jul_vec[good]
            nMaps   = N_ELEMENTS(jul_vec)
        ENDIF ELSE BEGIN
            nMaps   = 0
        ENDELSE
    ENDELSE
ENDELSE

IF nMaps EQ 0 THEN BEGIN
    PRINT, 'No data!!!!  Very sorry!!!!'
    RETURN
ENDIF

;Give 10 second leeway because timestamps on data aren't always exactly right.
;This statement doesn't actually change any data timestamps, it just broadens the search for data very slightly.
jul_vec = jul_vec - (10./(60.*60.*24.))
nLon    = 180
nLat    = 90 - min_lat

lonVec  = FINDGEN(nLon) * 2.
latVec  = FINDGEN(nLat) + min_lat

lonGrd  = FLTARR(nLon,nLat)
xGrd    = FLTARR(nLon,nLat)
FOR kk = 0,nLat-1 DO BEGIN
    lonGrd[*,kk] = lonVec
      xGrd[*,kk] = kk + 1
ENDFOR

latGrd  = FLTARR(nLon,nLat)
yGrd    = FLTARR(nLon,nLat)
FOR kk = 0,nLon-1 DO BEGIN
    latGrd[kk,*] = latVec
      yGrd[kk,*] = kk + 1
ENDFOR
IF int_hemi EQ 1 THEN latGrd = -1.*latGrd

xArr    = REFORM(xGrd,nLat*nLon)
yArr    = REFORM(yGrd,nLat*nLon)

inxArr  = '['+NUMSTR(xArr)+','+NUMSTR(yArr)+']'
inxArr  = TRANSPOSE(inxArr)


; Start log file. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
IF KEYWORD_SET(log_file) THEN BEGIN     ;Only print header once!
    OPENW,2,log_file,WIDTH=500;,/APPEND
    PRINTF,2,'Starting RAD_MAP_DATA2ASCII...'
    CLOSE,2
ENDIF

;nMaps   = N_ELEMENTS((*RAD_MAP_DATA[int_hemi]).sjuls)
startInx    = 0
completed   = 0D ; Keep track of dates/times that have been completed.
FOR kk = 0,nMaps-1 DO BEGIN
    desired_jul = jul_vec[kk]

    recordInx   = WHERE((*RAD_MAP_DATA[int_hemi]).sjuls GE desired_jul, cnt)
    recordInx   = recordInx[0]
    IF cnt NE 0 THEN BEGIN
        timeJul = (*RAD_MAP_DATA[int_hemi]).sjuls[recordInx] 

        ;Skip scan if it is 1.5 or more minutes away from desired time and we are getting more than one scan.
        desired_jul_dt = timeJul - desired_jul
        desired_dt_min = desired_jul_dt*24.*60.
        IF (desired_dt_min GE 2.0) AND (nMaps GT 1) THEN BEGIN
            CONTINUE
        ENDIF
    ENDIF ELSE CONTINUE

    check       = WHERE(completed EQ timeJul,cnt)
    IF cnt NE 0 THEN CONTINUE   ; Don't run through scans twice, even if the time step is less than the actual cadence of the data.
    completed   = [completed, timeJul]
    
    CALDAT,timeJul,myMonth,myDay,myYear,myHour,myMin,mySec
    myMonth_str = STRING(myMonth,FORMAT='(I02)')
    myDay_str   = STRING(myDay,FORMAT='(I02)')
    myYear_str  = STRING(myYear,FORMAT='(I04)')
    myHour_str  = STRING(myHour,FORMAT='(I02)')
    myMin_str   = STRING(myMin,FORMAT='(I02)')
    mySec_str   = STRING(mySec,FORMAT='(I02)')

    timeStr     = myYear_str + '-' +    $
                  myMonth_str + '-' +   $
                  myDay_str + '/' +     $
                  myHour_str + ':' +    $
                  myMin_str + ':' +     $
                  mySec_str

    timestamp   = strarr(1,nlat*nlon)
    timestamp[*]= timestr

    PRINFO,timestr
    IF KEYWORD_SET(log_file) THEN BEGIN
        OPENW,2,log_file,WIDTH=500,/APPEND
        PRINTF,2,'RAD_MAP_DATA2ASCII: '+timestr
        CLOSE,2
    ENDIF

    recid       = startinx
    recIdVec    = STRARR(1,nLat*nLon)
    recIdVec[*] = NUMSTR(recId)

    bnd         = (*(*rad_map_data[int_hemi]).bvecs[recordInx])
    IF bnd[0].lat LT -999 THEN BEGIN
        PRINT,'No H-M boundary data exists in Map file'
        CONTINUE
    ENDIF
    latMin      = MIN(bnd[*].lat,/abs) 
    ;latMin      =   (*RAD_MAP_DATA[int_hemi]).latmin[recordInx]
    order       =   (*RAD_MAP_DATA[int_hemi]).fit_order[recordInx]
    coeffs      = (*(*rad_map_data[int_hemi]).coeffs[recordInx])


    ; Define the lat/lon grid. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    pos         = [[REFORM(latGrd,nLat*nlon)], [REFORM(lonGrd,nLat*nlon)]]
    pos         = TRANSPOSE(pos)

    pos$        = STRING(pos)
    IF KEYWORD_SET(int_hemi) THEN BEGIN
        ;Original code flipped the lons around and added 180 deg, presumably for doing the
        ;'look through the top of the Earth' view.  This is not what we want here when actually
        ;calculating potential values and putting them into an ASCII file.
        ;This was corrected after Kshitija brought this to our attention in April 2014.
        ; //NAF - 10 April 2014
        ;slon    = -pos[1,*] + 180 ;This is NOT what we want.

        slon    = pos[1,*]  ; This is what we want.
        inx     = WHERE(slon LT 0,cnt)
        IF cnt NE 0 THEN slon[inx] = slon[inx] + 360.
        pos$[1,*]       = STRING(slon)

        slon_sort       = SORT(slon[0:nLon-1])
        south_sort      = LONARR(nLat*nLon)
        FOR ss = 0,nLat-1 DO BEGIN
            south_sort[ss*nLon:(ss+1)*nLon-1] = ss*nLon + slon_sort
        ENDFOR
    ENDIF

    
    ; Calculate E Field. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    _pos        = pos
    _pos[0,*]   = ABS(_pos[0,*])
    e_field_st  = RAD_MAP_CALC_EFIELD(int_hemi,recordInx,GRID=_pos)

    e_field     = e_field_st.efiarr
    q           = WHERE(e_field[0,*] NE 0,qc)
    IF qc NE 0 THEN BEGIN
        e_field[0,q] = -e_field[0,q]
    ENDIF
    IF KEYWORD_SET(int_hemi) THEN e_field = -TEMPORARY(e_field)
;    e_field[0,*] = 0.03    
;    e_field[1,*] = -0.03
    e_field$    = STRING(e_field)

    ; Calculate Fitted Velocity Vectors. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    theta       = (90.0 - pos[0,*])*!dtor
    Re          = !re*1000.
    Altitude    = 300.0*1000.0
    bpolar      = -.62e-4
    bmag        = bpolar*(1.0-3.0*Altitude/Re)*sqrt(3.0*cos(theta)^2+1.0)/2.0

    vels        =  e_field
    vels[0,*]   =  -e_field[1,*]/bmag
    vels[1,*]   =  e_field[0,*]/bmag

    ;IF KEYWORD_SET(int_hemi) THEN vels = -TEMPORARY(vels)

    vels$       = STRING(vels)

    ; Calculate Potential ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    _pos        = pos
    _pos[0,*]   = ABS(_pos[0,*])
    v_st        = RAD_MAP_CALC_POTENTIAL(int_hemi,recordInx,GRID=_pos)
    v           = TRANSPOSE(1000.*v_st.potarr)
    v$          = STRING(v)

    ; Output to text file. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    IF ~KEYWORD_SET(fileStarted) THEN BEGIN     ;Only print header once!
        format  = '(1A-1,2A-8,7A-18,A25)'
        ;format  = '(2A-8,7A-18,A25)'
        OPENW,1,output_file,WIDTH=500;,/APPEND
        fileStarted = 1
    ENDIF

    str$    = '#SuperDARN Data file.'
    PRINTF,1,str$
    str$    = '#For North Vectors, positive means North and negative means South.'
    PRINTF,1,str$
    str$    = '#For East Vectors, positive means East and negative means West.'
    PRINTF,1,str$
    str$    = '#'
    PRINTF,1,str$

;    str$        = '# ' + 'Number of Contributing Radars: '  + NUMSTR((*RAD_MAP_DATA[int_hemi]).stnum[recordInx])
;    PRINTF,1,str$
    str$        = 'd ' + timeStr
    PRINTF,1,str$

    str$        = '> ' + 'Number of Measured Vectors (VCNUM): '  + NUMSTR((*RAD_MAP_DATA[int_hemi]).vcnum[recordInx])
    PRINTF,1,str$

;    vcnum$      =   'VCNUM: '  + NUMSTR((*RAD_MAP_DATA[int_hemi]).vcnum[recordInx])
;    modnum$     =   'MODNUM: ' + NUMSTR((*RAD_MAP_DATA[int_hemi]).modnum[recordInx])
;    bndnum$     =   'BNDNUM: ' + NUMSTR((*RAD_MAP_DATA[int_hemi]).bndnum[recordInx])
;    str$        = '# ' + vcnum$ + ', ' + modnum$ + ', ' + bndnum$
;    PRINTF,1,str$

    str$        = '> IMF Model: RG96 ' + (*RAD_MAP_DATA[int_hemi]).imf_model[recordInx] + ', Fit Order: ' + NUMSTR(order)
    PRINTF,1,str$
    
    bx$         =   'Bx=' + NUMSTR((*RAD_MAP_DATA[int_hemi]).b_imf[recordInx,0]) + ' nT'
    by$         =   'By=' + NUMSTR((*RAD_MAP_DATA[int_hemi]).b_imf[recordInx,1]) + ' nT'
    bz$         =   'Bz=' + NUMSTR((*RAD_MAP_DATA[int_hemi]).b_imf[recordInx,2]) + ' nT'
    str$        = '> OMNI IMF:  ' + bx$ + ', ' + by$ + ', ' + bz$
    PRINTF,1,str$

    drop$        = 'Drop=' + NUMSTR((*RAD_MAP_DATA[int_hemi]).pot_drop[recordInx]/1000.) + ' kV'
    min$         = 'Min='  + NUMSTR((*RAD_MAP_DATA[int_hemi]).pot_min[recordInx]/1000.) + ' kV'
    max$         = 'Max='  + NUMSTR((*RAD_MAP_DATA[int_hemi]).pot_max[recordInx]/1000.) + ' kV'
    str$        = '> Potential: ' + drop$ + ', ' + min$ + ', ' + max$
    PRINTF,1,str$

    str$    = '#'
    PRINTF,1,str$
     
    str$    = ['#','Record', 'Indices',  'mlat',  'mlon', 'EField_north', 'EField_east', 'Fitted_Vel_North', 'Fitted_Vel_East', 'Potential','TimeStamp']
    PRINTF,1,str$,FORMAT=format
    str$    = ['#',      '',        '', '[deg]', '[deg]',        '[V/m]',       '[V/m]',            '[m/s]',           '[m/s]',       '[V]',     '[UT]']
    PRINTF,1,str$,FORMAT=format

    spacer      = recIdVec
    spacer[*]   = ' '
    str$        = [spacer, recIdVec, inxArr, pos$, e_field$, vels$, v$, timeStamp]

    IF KEYWORD_SET(int_hemi) THEN str$ = str$[*,south_sort]
    ;str$        = [recIdVec, inxArr, pos$, e_field$, vels$, v$, timeStamp]
    PRINTF,1,str$,FORMAT=format
    
    ;Used for time stepping.
    lastTime    = timeJul
    startInx++
ENDFOR
CLOSE,1

IF KEYWORD_SET(fileStarted) THEN status  = 0 ELSE status = 3
END
