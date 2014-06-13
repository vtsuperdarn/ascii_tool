;+ 
; NAME: 
; OVERLAY_POLAR_VECTORS
; 
; PURPOSE: 
; This procedue will load plot arrow vectors in the form of:
;       Latitude, Longitude, East-West Component, North-South Component
; on a map projection.  Azimuthal projections are recommended.
;
; Vector endpoints are calculated by assuming that the map projection is a flat,
; polar plot and then converting everything into cartesian coordinates.
;
; THIS HAS ONLY BEEN TESTED WITH THE NORTHERN HEMISPHERE!
; 
; CATEGORY: 
; Graphics
; 
; CALLING SEQUENCE: 
; OVERLAY_POLAR_VECTORS
;
; INPUTS:
; LON: Scalar or array of longintudes representing the vector startiong position.
;
; LAT: Scalar or array of latitudes representing the vector startiong position.
;
; EWVEC: Scalar or array of east-west componenent of the vector.
;
; NSVEC: Scalar or array of east-west componenent of the vector.
;
; KEYWORD PARAMETERS: 
; SCALE_FACTOR: Multiply the magnitude of the plotted vectors by this value.
;
; COLOR: Scalar or array of color indices to be used for each plotted vector.
;
; HSIZE: Scalar for arrowhead size.  If not using default, 100 is a good value.
;
; HTHICK: Scalar for arrowhead thickness.
;
; THICK: Scale for arrowstem thickness.
;
; GREATERTHAN: Only plot vectors with magnitude greater than this value.
;
; LATMODULO: Only plot vectors at latitudes that are integer multiples of this
; keyword value.  Omit or set to zero to disable this.
;
; LONMODULO: Only plot vectors at longitudes that are integer multiples of this
; keyword value.  Omit or set to zero to disable this.
;
; MODIFICATION HISTORY: 
; Written by Nathaniel Frissell, Nov. 11, 2011
; Revised by Nathaniel Frissell, Apr. 11, 2014
;   >Fixed southern hemisphere vector plotting issue
;-
PRO OVERLAY_POLAR_VECTORS,lon,lat,_ewVec,_nsVec                                 $
    ,INT_HEMI           = int_hemi                                              $
    ,SCALE_FACTOR       = scale_factor                                          $
    ,COLOR              = _color                                                $
    ,HSIZE              = hsize                                                 $
    ,HTHICK             = hthick                                                $
    ,THICK              = thick                                                 $
    ,GREATERTHAN        = greaterThan                                           $
    ,LATMODULO          = latModulo                                             $
    ,LONMODULO          = lonModulo

lat0    = lat
lon0    = lon

ewVec   = _ewVec
nsVec   = _nsVec

IF KEYWORD_SET(int_hemi) THEN BEGIN
    lat0        = ABS(lat0); & lon0 = -lon0 + 180.
    ewVec       = -_ewVec
    nsVec       = -_nsVec
ENDIF

;Make sure the color variable is defined.
IF (N_ELEMENTS(_color) GT 0) AND (N_ELEMENTS(color) NE N_ELEMENTS(lat0)) THEN BEGIN
    color = INTARR(N_ELEMENTS(lat0)) + _color
ENDIF ELSE IF N_ELEMENTS(_color) EQ 0 THEN BEGIN
    color = INTARR(N_ELEMENTS(lat0)) + 0
ENDIF ELSE color = _color

IF ~KEYWORD_SET(scale_factor) THEN scale_factor = 1.

;Give a way to not plot vectors below a certain magnitude.
IF N_ELEMENTS(greaterThan) NE 0 THEN BEGIN
    mag         = SQRT(ewVec^2 + nsVec^2)
    inx         = WHERE(mag GT greaterThan, cnt)
    IF cnt NE 0 THEN BEGIN
        lat0    = lat0[inx]
        lon0    = lon0[inx]
        ewVec   = ewVec[inx]
        nsVec   = nsVec[inx]
        color   = color[inx]
    ENDIF
ENDIF

;Assume we are looking at a polar plot of the Earth down to the equator.  Make the equator a unit circle.
;Convert latitudes and longitudes into cartesian coordinates.
; 0 deg lon is -y direction
;90 deg lon is +x direction
;theta is co-latitude

theta0  = 90. - lat0
x0      =  SIN(lon0*!DTOR) * theta0/90.
y0      = -COS(lon0*!DTOR) * theta0/90.

;Convert EW and NS vectors into cartesian coordinates.
xVec    = ewVec*COS(lon0*!DTOR) - nsVec*SIN(lon0*!DTOR)
yVec    = ewVec*SIN(lon0*!DTOR) + nsVec*COS(lon0*!DTOR)

;Find endpoints of vector in cartesian coordinates.
x1      = x0 + scale_factor*xVec
y1      = y0 + scale_factor*yVec

;Convert endpoints to lat/lon.
lon1    = !RADEG * ATAN(x1,-y1)
lat1    = 90. * (1. - SQRT(x1^2 + y1^2))

;Plot all the vectors!
FOR ak=0,N_ELEMENTS(lat0)-1 DO BEGIN
    IF KEYWORD_SET(lonModulo) THEN $
        IF (lon0[ak] MOD lonModulo) NE 0 THEN CONTINUE

    IF KEYWORD_SET(latModulo) THEN $
        IF (lat0[ak] MOD latModulo) NE 0 THEN CONTINUE

    ARROW,lon0[ak],lat0[ak],lon1[ak],lat1[ak],/DATA,COLOR=color[ak]     $
        ,HSIZE=hsize,HTHICK=hthick,THICK=thick
ENDFOR

END
