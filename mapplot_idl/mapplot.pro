;+ 
; NAME: 
; MAPPLOT
; 
; PURPOSE: 
; This procedue will load SuperDARN Electric Field, Fitted Velocity, and Potential data from a text
; file and generate plots in postscript files of this data.  All postscrips are output to the current
; working directory and given descriptive names.
;
; This routine can also be used to load the contents of this data file into array within IDL.
;
; The input data file should have the
; following format (including 6 header lines):
;
;-----START FILE EXAMPLE-----
; SuperDARN Data file.
; For North Vectors, positive means North and negative means South.
; For East Vectors, positive means East and negative means West.
; 
; Record  Indices mlat              mlon              EField_north      EField_east       Fitted_Vel_North  Fitted_Vel_East   Potential                         TimeStamp
;                 [deg]             [deg]             [V/m]             [V/m]             [m/s]             [m/s]             [V]                                    [UT]    
; 0       [1,1]         60.0000           0.00000           0.00000           0.00000           0.00000           0.00000            0.0000000        2008-03-08/11:01:00
; 0       [1,2]         60.0000           2.00000           0.00000           0.00000           0.00000           0.00000            0.0000000        2008-03-08/11:01:00
;----- END FILE EXAMPLE -----
;
; You can have as many "events" or "timestamps" as the limitations your computer system will support.  Latitude values should run from 50 to 89 degrees in 1 deg steps.
; Longitude values should run from 0 to 358 in 2 degree steps. Most parameters for adjusting plot output are set at the top of this file.
;
; This routine depends on a base IDL 7 installation, as well as the following routines, which should accompany this file:
;       get_color_index.pro
;       plot_colorbar.pro
;       rad_load_colortable.pro
;       overlay_polar_vectors.pro
;
; Although many of these routines are from the Virginia Tech DaViT toolkit, the versions accompanying this file
; have been modified to run without the entire toolkit.
; 
; CATEGORY: 
; Graphics
; 
; CALLING SEQUENCE: 
; MAPPLOT
;
; KEYWORD PARAMETERS: 
;
; VELOCITY: Set this keyword to plot fitted velocity vectors.  This, overplotted on potential contours, is the default.
;
; EFIELD: Set this keyword to plot electric field vectors.
;
; POTENTIAL: Set this keyword to plot the potential pattern.
;
; TEST: Set this keyword to only plot the first timestep contained within a file.
;
; DATAARRAY: Set this keyword equal to a variable name that will contain the loaded data.  The heading and units
; of this 8xn array will be:
;  [ mlat,  mlon, EField_north, EField_east, Fitted_Vel_North, Fitted_Vel_East, Potential, JulianTimeStamp]
;   [deg], [deg],        [V/m],       [V/m],            [m/s],           [m/s],       [V],           [Days]
;
; DATAFILE: Set this to a string containing the name of file to use as the source of the data.
; This defaults to 'superdarn.txt'.
;
; EXAMPLE:
;  Plot potential contours and fitted velocity vectors from '20080309_1100-2100.txt'.  Also, load data into variable 'data'.
;       IDL> MAPPLOT,DATAFILE='20080309_1100-2100.txt',/POTENTIAL,/VELOCITY,DATAARRAY=data
;
;  Load all data into variable 'data', but don't plot anything.
;       IDL> MAPPLOT,DATAFILE='20080309_1100-2100.txt',/LOADONLY,DATAARRAY=data
;
; NOTES:
; The EFIELD or VELOCITY may be selected simultaneousely with the POTENTIAL.  In this case, the
; EFIELD or VELOCITY vectors will be overplotted on on the potential patterns.  You may not, however,
; plot the EFIELD and VELOCITY field simultanously.
;
; Many plotting parameters are set as variables at the beginning of this file.  If you don't like what you get out
; of this routine, try adjusting these parameters.
;
; By default, not every vector in the datafile is plotted.  Rather, only even latitudes and longitudes divisible
; by 10 are plotted.  Also, vectors with identically zero length are omitted.
;
; 
;
; MODIFICATION HISTORY: 
; Written by Nathaniel Frissell, Nov. 11, 2011
; Revised by Nathaniel Frissell, Nov. 15, 2011
; Revised by Nathaniel Frissell, Apr. 16, 2012
;   > Added Southern Hemisphere Plotting Capability
; Revised by Nathaniel Frissell, Apr. 10, 2014
;   > Reversing Southern Hemisphere so that left is East and right is West
;   > Default add 180 rotation to Southern Hemisphere
;   > Added text to plot indicating hemisphere
;-
PRO MAPPLOT                                                             $
    ,VELOCITY           = velocity                                      $
    ,POTENTIAL          = potential                                     $
    ,EFIELD             = efield                                        $
    ,TEST               = test                                          $
    ,DATAARRAY          = dataArray                                     $
    ,LOADONLY           = loadOnly                                      $
    ,DATAFILE           = dataFile

; Define plot parameters. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
thick           = 4     ;Applied globally.
;These are sent to IDL's MAP_SET procedure. See IDL's documentation for details.
p0lat           = 90
p0lon           = 0
rot             = 0.
latmin          = 50.
lonLab          = 55.
lonGrdStep      = 15.       ;Same as MAP_SET's londel
latGrdStep      = 10.       ;Same as MAP_SET's latdel

; Potential Pattern Plotting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
neg_linestyle   = 0
pos_linestyle   = 5
n_levels        = 10.
diffc           = 6.

; Electric Field and Fitted Velocity Vector Plotting ;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Please don't try and plot both at the same time!!!
; Set scale to force a colorbar scale for all electric field plots.  For this to
; work, dynamicColorBar must = 0.
;scale           = [-1, 1]              

; If dynamicColorBar=1, then the colorbar maximum will be set to the maximum value 
; of eField vector on an individual map.  If  dynamicColorBar=0 and scale is not
; set, the colorbar maximum will be fixed at the value of the largest eField vector
; in the entire data file.
dynamicColorBar         = 0

; LatModulo and lonModulo: Only plot vectors at latitudes or longintude that are integer
; divisible by this number.  Set = 0 disable this feature.
latModulo               = 2 
lonModulo               = 10

;Settings to control electric field vector plotting.
ef_scale                = [0.,80.]      ;Scale for electric field.  Commentout to enable auto-scaling.
ef_len_factor           = 0.0005        ;Use this to scale the length of electric field vectors.
ef_greaterThan          = 0             ;Don't plot vectors <= to this magnitude.
ef_arrowHeadSize        = 100
ef_arrowHeadThick       = 3
ef_arrowStemThick       = 3

;Settings to control fitted velocity vector plotting.
vel_scale               = [0.,1600.]    ;Scale for electric field.  Commentout to enable auto-scaling.
vel_len_factor          = 0.00003       ;Use this to scale the length of velocity vectors.
vel_greaterThan         = 0             ;Don't plot vectors <= to this magnitude.
vel_arrowHeadSize       = 150
vel_arrowHeadThick      = 3
vel_arrowStemThick      = 3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Data Loading and Visualization Starts Here ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

!P.NOERASE      = 1
!P.CHARTHICK    = thick
!P.THICK        = thick
!X.THICK        = thick
!Y.THICK        = thick

IF  KEYWORD_SET(dataFile) THEN file = dataFile ELSE file = 'superdarn.txt'
IF ~KEYWORD_SET(potential) AND ~KEYWORD_SET(efield) AND ~KEYWORD_SET(velocity) THEN BEGIN
    potential   = 1
    velocity    = 1
    efield      = 0
ENDIF

IF KEYWORD_SET(efield) AND KEYWORD_SET(velocity) THEN BEGIN
    PRINT,'Right now, I can only plot the EField and Velocity separately.'
    PRINT,'There, I am choosing fitted velocities.'
    velocity    = 1
    efield      = 0
ENDIF

IF KEYWORD_SET(efield)   AND KEYWORD_SET(ef_scale)  THEN scale = ef_scale
IF KEYWORD_SET(velocity) AND KEYWORD_SET(vel_scale) THEN scale = vel_scale

type    = ''
IF KEYWORD_SET(potential) THEN type += '.potential'
IF KEYWORD_SET(efield)    THEN type += '.efield'
IF KEYWORD_SET(velocity)  THEN type += '.velocity'
outfile         = file + type + '.ps'

; Dimensions for reforming vectors into a grid.  This needs to change if ;;;;;;;
; the incoming grid is not in steps of 1 deg lat and 2 deg lon. ;;;;;;;;;;;;;;;;
gridDims        = [180,40]

;Expected format of input file:
;First 6 lines are a header, data follows.
;[Record, Indices, mlat, mlon, EField_north, EField_east, Fitted_vel_North, Fitted_Vel_east, Potential, TimeStamp (YYYY-MM-DD/hh:mm:ss)]
;[     0,       1,    2,    3,            4,           5,                6,               7,         8,                               9]

nHeaders= 6
nCols   = 10

rcdInx  = 0
indInx  = 1
mLatInx = 2
mLonInx = 3
efnInx  = 4
efeInx  = 5
velnInx = 6
veleInx = 7
potInx  = 8
timeInx = 9

; Read entire data file into a string array. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
nLines  = FILE_LINES(file)
OPENR,unit,file,/GET_LUN
inArr   = STRARR(nLines)
READF,unit,inArr
FREE_LUN,unit

linesData       = WHERE(~STRCMP(inArr,'#',1),cnt)
IF cnt EQ 0 THEN BEGIN
    PRINT,'No data in file.  Sorry.'
    STOP
END
inArr           = inArr[linesData]
dateLines       = STRCMP(inArr,'d',1)
plotComment     = STRCMP(inArr,'>',1)
dataLines       = ~(dateLines OR plotComment)

; Parse all data into separate rows. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
dataArr         = STRARR(cnt,nCols)
pcArr           = STRARR(cnt,2)
currentDate     = ''
FOR kk=0L,cnt-1 DO BEGIN
    IF dataLines[kk] THEN BEGIN
        dataArr[kk,*] = STRSPLIT(inArr[kk],' ',/EXTRACT)
        CONTINUE
    ENDIF
    IF dateLines[kk] THEN BEGIN
        currentDate     = inArr[kk]
        CONTINUE
    ENDIF
    IF plotComment[kk] THEN BEGIN
        pcArr[kk,0] = inArr[kk]
        pcArr[kk,1] = currentDate
        CONTINUE
    ENDIF
ENDFOR

;Remove empty lines from plot comments.
;Clean up plot comment strings.
pcInx   = WHERE(plotComment,cnt)
IF cnt NE 0 THEN BEGIN
    pcArr       = pcArr[pcInx,*]
    pcArr       = STRMID(pcArr,1)
    pcArr       = STRTRIM(pcArr,2)
ENDIF

;Remove empty plot comment lines from data array.
dataInx = WHERE(dataLines,cnt)
IF cnt EQ 0 THEN BEGIN
    PRINT,'No data in file.  Sorry.'
    STOP
END
dataArr = dataArr[dataInx,*]

;Parse date string and convert to julian date.
;date[n,*]   =  [year, month, day, hr, min, sec]
;               [   0,     1,   2,  3,   4,   5]
timeStr = dataArr[*,timeInx]
date    = LONG(STRMID(dataArr[*,timeInx],[0,5,8,11,14,17],[4,2,2,2,2,2]))
juls    = REFORM(JULDAY(date[1,*], date[2,*], date[0,*], date[3,*], date[4,*], date[5,*]))

mLats   = FLOAT(dataArr[*,mLatInx])
mLons   = FLOAT(dataArr[*,mLonInx])
efn     = FLOAT(dataArr[*,efnInx])  * 1000.     ;NS Componenent of EField [mV/m]
efe     = FLOAT(dataArr[*,efeInx])  * 1000.     ;EW Componenent of EField [mV/m]
veln    = FLOAT(dataArr[*,velnInx])             ;NS Componenent of fitted velocity [m/s]
vele    = FLOAT(dataArr[*,veleInx])             ;EW Componenent of fitted velocity [m/s]
v       = FLOAT(dataArr[*,potInx])  / 1000.     ;Put potential in kV.

;Check for northern/southern hemisphere.
negLats = WHERE(mLats LT 0, cnt)
IF cnt GT 0 THEN BEGIN
    int_hemi    = 1
    rot         = rot + 180.
ENDIF ELSE BEGIN
    int_hemi = 0
ENDELSE


; Put the data into an array for export and easy use. ;;;;;;;;;;;;;;;;;;;;;;;;;;
; Then tell the user some information about it. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
dataArray = TRANSPOSE([[mLats],[mLons],[efn/1000.],[efe/1000.],[veln],[vele],[v*1000.],[juls]])

PRINT,'You have loaded the following SuperDARN Datafile: ' + file
PRINT,'The data can be accessed through the variable you specified in the DATAARRAY keyword.'
PRINT,'For North Vectors, positive means North and negative means South.'
PRINT,'For East Vectors, positive means East and negative means West.'
PRINT,''
PRINT,'Here are the column headings and units for this data array:'
PRINT,'[ mlat,  mlon, EField_north, EField_east, Fitted_Vel_North, Fitted_Vel_East, Potential, JulianTimeStamp]'
PRINT,' [deg], [deg],        [V/m],       [V/m],            [m/s],           [m/s],       [V],           [Days]'
PRINT,''
HELP,dataArray

IF KEYWORD_SET(loadonly) THEN RETURN

; Compute magnitude and colors of eField Vectors. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
IF KEYWORD_SET(efield) THEN BEGIN
    efMag   = SQRT(efn^2 + efe^2)
    IF ~KEYWORD_SET(dynamicColorBar) THEN BEGIN
        IF ~KEYWORD_SET(scale) THEN scale = [0, MAX(efMag,/NAN)]
        efColors  = GET_COLOR_INDEX(efMag,SCALE=scale,CONTINUOUS=continuous)
    ENDIF
ENDIF   ; EField

; Compute magnitude and colors of fitted velocity vectors. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
IF KEYWORD_SET(velocity) THEN BEGIN
    velMag  = SQRT(veln^2 + vele^2)
    IF ~KEYWORD_SET(dynamicColorBar) THEN BEGIN
        IF ~KEYWORD_SET(scale) THEN scale = [0, MAX(velMag,/NAN)]
        velColors  = GET_COLOR_INDEX(velMag,SCALE=scale,CONTINUOUS=continuous)
    ENDIF
ENDIF   ; Fitted Velocity

; Determine how many events there are and what are their julian timestamps. ;;;;
events  = juls[UNIQ(juls)]
nEvents = N_ELEMENTS(events)

; Open postscript device. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SET_PLOT,'ps'
DEVICE, FILENAME=outfile                                        $
    ,/HELVETICA                                                 $
    ,/COLOR                                                     $
    ,BITS               = 8                                     $
    ,/PORTRAIT                                                  $
    ,/INCHES                                                    $
    ,XOFFSET            = 0.25                                  $
    ,YOFFSET            = 0.25                                  $
    ,XSIZE              = 8.0                                   $
    ,YSIZE              = 10.5                                  $
    ,FONT_SIZE          = 18                                    $
    ,SCALE_FACTOR       = 1

; Start plotting. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FOR kk=0L,nEvents-1 DO BEGIN
    eventInx    = WHERE(juls EQ events[kk])
    evTimeStr   = timeStr[eventInx]
    evV         = REFORM(v[eventInx],gridDims)

    IF N_ELEMENTS(evMLats) EQ 0 THEN BEGIN
        evMLats     = REFORM(mLats[eventInx],gridDims)
        evMLons     = REFORM(mLons[eventInx],gridDims)
        IF KEYWORD_SET(int_hemi) THEN BEGIN
            evMLats = ABS(evMLats)
            ;evMLons = -evMLons + 180.
        ENDIF
    ENDIF

    ; Plot title. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    IF KEYWORD_SET(potential) THEN param = 'Potential'
    IF KEYWORD_SET(efield)    THEN param = 'Electric Field'
    IF KEYWORD_SET(velocity)  THEN param = 'Velocity'
    IF KEYWORD_SET(potential) AND KEYWORD_SET(efield)   THEN param = 'Potential & Electric Field'
    IF KEYWORD_SET(potential) AND KEYWORD_SET(velocity) THEN param = 'Potential & Velocity'
    IF KEYWORD_SET(potential) AND KEYWORD_SET(efield)   AND KEYWORD_SET(velocity) THEN  $
        param = 'Potential, Electric Field, and Velocity'

    ypos        = 0.86
    print,param,' '+evTimeStr[0]
    title$ = 'SuperDARN ' + param
    XYOUTS,0.115,ypos,title$,/NORMAL,CHARSIZE=1.50

    ypos        -= 0.02
    IF ~int_hemi THEN hemi_txt = '!CNorthern Hemisphere' ELSE hemi_txt = '!CSouthern Hemisphere'
    title$ = evTimeStr[0] + '!CData File: ' + file + hemi_txt
    XYOUTS,0.115,ypos-0.005,title$,/NORMAL,CHARSIZE=0.90


    ;PlotComments.
    pcInx       = WHERE(pcArr[*,1] EQ evTimeStr[0],cnt)
    IF cnt NE 0 THEN BEGIN
        str$    = pcArr[pcInx[0],0]
        FOR ll=1,cnt-1 DO BEGIN
            str$    += '!C' + pcArr[pcInx[ll],0]
        ENDFOR
    ENDIF

    XYOUTS,0.500,ypos,str$,/NORMAL,CHARSIZE=0.60
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    MAP_SET,p0lat,p0lon,rot                                     $
        ,POSITION               = [0.115, 0.12, 0.93, 0.83]     $
        ,/AZIMUTHAL                                             $
        ,/ISOTROPIC                                             $
        ,LIMIT                  = [latmin, -180, 90, 180]       $
        ,/HORIZON                                               $
        ,/NOERASE                                               $
        ,REVERSE                = int_hemi                      $
        ,E_GRID={COLOR:255}
;        ,TITLE                  = title$


    IF KEYWORD_SET(potential) THEN BEGIN
        ; Load in potential pattern colorbar. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        RAD_LOAD_COLORTABLE, /BLUEWHITERED
        ncol2           = 253/2
        bottom          = 1
        neg_color = round((1.-findgen(n_levels)/(n_levels-1.))*ncol2) + bottom
        neg_color[0] -= 7
        pos_color = round(findgen(n_levels)/(n_levels-1.)*(ncol2-1)) + ncol2 + bottom + 1 
        pos_color[0] += 7

        ; Overlay Contours - Filling ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ; in order to get the filling of the negative contours right, 
        ; we take minus the potential and use the same levels as
        ; for the positive contours, just in blue

        ; negative
        CONTOUR,-evV,evMLons,evMLats                                $
            ,/OVERPLOT                                              $
            ,XSTYLE                 = 4                             $
            ,YSTYLE                 = 4                             $
            ,NOCLIP                 = 0                             $
            ,THICK                  = thick                         $
            ,C_LINESTYLE            = neg_linestyle                 $
            ,C_COLOR                = neg_color                     $
            ,C_CHARSIZE             = c_charsize                    $
            ,C_CHARTHICK            = c_charthick                   $
            ,LEVELS                 = 3 + findgen(n_levels)*diffc   $
            ,MAX_VALUE              = 100                           $
            ,/FOLLOW                                                $
            ,PATH_XY                = path_xy                       $
            ,PATH_INFO              = path_info

        FOR i = 0, N_ELEMENTS(path_info) - 1 DO BEGIN
                IF path_info[i].high_low EQ 0b THEN CONTINUE
                s = [indgen(path_info[i].n), 0]
                ; Plot the closed paths:
                POLYFILL, path_xy[*, path_info[i].offset + s], /norm, color=neg_color[path_info[i].level], noclip=0
        ENDFOR
        ; positive
        CONTOUR,evV,evMLons,evMLats                                 $
            ,/OVERPLOT                                              $
            ,XSTYLE                 = 4                             $
            ,YSTYLE                 = 4                             $
            ,NOCLIP                 = 0                             $
            ,THICK                  = thick                         $
            ,C_LINESTYLE            = pos_linestyle                 $
            ,C_COLOR                = pos_color                     $
            ,C_CHARSIZE             = c_charsize                    $
            ,C_CHARTHICK            = c_charthick                   $
            ,LEVELS                 = 3 + findgen(n_levels)*diffc   $
            ,MAX_VALUE              = 100                           $
            ,/FOLLOW                                                $
            ,PATH_XY                = path_xy                       $
            ,PATH_INFO              = path_info
        FOR i = 0, N_ELEMENTS(path_info) - 1 DO BEGIN
                IF path_info[i].high_low EQ 0b THEN CONTINUE
                s = [indgen(path_info[i].n), 0]
                ; Plot the closed paths:
                POLYFILL, path_xy[*, path_info[i].offset + s], /norm, color=pos_color[path_info[i].level], noclip=0
        ENDFOR

        ; Overlay Contours - Lines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ; negative
        CONTOUR,evV,evMLons,evMLats                                        $
            ,/OVERPLOT                                                      $
            ,XSTYLE                 = 4                                     $
            ,YSTYLE                 = 4                                     $
            ,THICK                  = thick                                 $
            ,C_LINESTYLE            = neg_linestyle                         $
            ,COLOR                  = neg_color[n_elements(neg_color)-1]    $
            ,C_CHARSIZE             = c_charsize                            $
            ,C_CHARTHICK            = c_charthick                           $
            ,LEVELS                 = -57+findgen(n_levels)*diffc           $
            ,MAX_VALUE              = 100                                   $
            ,/FOLLOW                                                        $
            ,NOCLIP                 = 0

        ; positive
        CONTOUR,evV,evMLons,evMLats                                         $
            ,/OVERPLOT                                                      $
            ,XSTYLE                 = 4                                     $
            ,YSTYLE                 = 4                                     $
            ,THICK                  = thick                                 $
            ,C_LINESTYLE            = pos_linestyle                         $
            ,COLOR                  = pos_color[n_elements(pos_color)-1]    $
            ,C_CHARSIZE             = c_charsize                            $
            ,C_CHARTHICK            = c_charthick                           $
            ,LEVELS                 = 3+findgen(n_levels)*diffc             $
            ,MAX_VALUE              = 100                                   $
            ,/FOLLOW                                                        $
            ,NOCLIP                 = 0

        ; Put in symbols at the maximum and minimum points. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        dims = SIZE(evV, /DIM)
        pot_max = MAX(evV, maxind, MIN=pot_min, SUBSCRIPT_MIN=minind)
        kref = maxind MOD dims[0]
        mref = FIX(maxind/dims[0])
        pot_size    = 1.5
        pot_thick   = 6
        XYOUTS, evMLons[kref,mref], evMLats[kref,mref],'+',color=foreground,ALIGNMENT=0.5,CHARSIZE=pot_size,CHARTHICK=pot_thick
;        PLOTS, evMLons[kref,mref], evMLats[kref,mref], psym=1, symsize=0.75, thick=thick, $
;                color=foreground, noclip=0

        kref = minind MOD dims[0]
        mref = FIX(minind/dims[0])
        XYOUTS, evMLons[kref,mref], evMLats[kref,mref],'-',color=foreground,ALIGNMENT=0.5,CHARSIZE=pot_size,CHARTHICK=pot_thick
;        plots, evMLons[kref,mref], evMLats[kref,mref], psym=7, symsize=0.75, thick=thick, $
;                color=foreground, noclip=0
    ENDIF       ;Potential

    IF KEYWORD_SET(efield) THEN BEGIN
        IF KEYWORD_SET(dynamicColorBar) THEN BEGIN
            scale       = [0, MAX(efMag[eventInx],/NAN)]
            colorVec    = GET_COLOR_INDEX(efMag[eventInx],SCALE=scale,CONTINUOUS=continuous)
        ENDIF ELSE BEGIN
            colorVec    = efColors[eventInx]
        ENDELSE

        RAD_LOAD_COLORTABLE
        OVERLAY_POLAR_VECTORS,mLons[eventInx],mLats[eventInx],efe[eventInx],efn[eventInx]               $
            ,INT_HEMI           = int_hemi                                                              $
            ,SCALE_FACTOR       = ef_len_factor                                                         $
            ,COLOR              = colorVec                                                              $
            ,GREATERTHAN        = ef_greaterThan                                                        $
            ,HSIZE              = ef_arrowHeadSize                                                      $
            ,HTHICK             = ef_arrowHeadThick                                                     $
            ,THICK              = ef_arrowStemThick                                                     $
            ,LATMODULO          = latModulo                                                             $
            ,LONMODULO          = lonModulo
    ENDIF       ;Efield vectors.

    IF KEYWORD_SET(velocity) THEN BEGIN
        IF KEYWORD_SET(dynamicColorBar) THEN BEGIN
            scale       = [0, MAX(velMag[eventInx],/NAN)]
            colorVec    = GET_COLOR_INDEX(velMag[eventInx],SCALE=scale,CONTINUOUS=continuous)
        ENDIF ELSE BEGIN
            colorVec    = velColors[eventInx]
        ENDELSE

        RAD_LOAD_COLORTABLE
        OVERLAY_POLAR_VECTORS,mLons[eventInx],mLats[eventInx],vele[eventInx],veln[eventInx]             $
            ,INT_HEMI           = int_hemi                                                              $
            ,SCALE_FACTOR       = vel_len_factor                                                        $
            ,COLOR              = colorVec                                                              $
            ,GREATERTHAN        = vel_greaterThan                                                       $
            ,HSIZE              = vel_arrowHeadSize                                                     $
            ,HTHICK             = vel_arrowHeadThick                                                    $
            ,THICK              = vel_arrowStemThick                                                    $
            ,LATMODULO          = latModulo                                                             $
            ,LONMODULO          = lonModulo
    ENDIF       ;Fitted velocity vectors.


nLatGrd         = FLOOR( (90-ABS(latMin)) / latGrdStep) + 1
lats            = FINDGEN(nLatGrd) * latGrdStep + latMin
IF KEYWORD_SET(int_hemi) THEN lats = -lats
latNames        = STRTRIM(STRING(lats,FORMAT='(I)'),1)


    MAP_GRID                                                    $
        ,/GRID                                                  $
        ,/LABEL                                                 $
        ,LATS                   = ABS(lats)                     $
        ,LATNAMES               = latNames                      $
        ,LONLAB                 = lonLab                        $
        ,LONDEL                 = lonGrdStep                    $
        ,/HORIZON

    IF KEYWORD_SET(efield)   THEN PLOT_COLORBAR,CONTINUOUS=continuous,LEGEND='Electric Field Magnitude [mV/m]',SCALE=scale
    IF KEYWORD_SET(velocity) THEN PLOT_COLORBAR,CONTINUOUS=continuous,LEGEND='Velocity [m/s]',SCALE=scale

;    ;Make a box around everything.
;    xc  = [0.09, 0.95]
;    yc  = [0.02, 0.9]
;    thk = 0.001
;
;    POLYFILL,/NORMAL,[xc[0],xc[0],xc[0]+thk,xc[0]+thk],[yc[0],yc[1],yc[1],yc[0]]
;    POLYFILL,/NORMAL,[xc[1],xc[1],xc[1]+thk,xc[1]+thk],[yc[0],yc[1],yc[1],yc[0]]
;    POLYFILL,/NORMAL,[xc[0],xc[0],xc[1],xc[1]],[yc[0],yc[0]+thk,yc[0]+thk,yc[0]]
;    POLYFILL,/NORMAL,[xc[0],xc[0],xc[1],xc[1]],[yc[1],yc[1]+thk,yc[1]+thk,yc[1]]
;
    IF KEYWORD_SET(test) THEN BEGIN
        DEVICE,/CLOSE
        SET_PLOT,'x'
        RETURN
    ENDIF ELSE BEGIN
        ERASE
    ENDELSE
ENDFOR

DEVICE,/CLOSE
SET_PLOT,'x'
END
