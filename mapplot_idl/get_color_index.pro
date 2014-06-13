;+ 
; NAME: 
; GET_COLOR_INDEX
; 
; PURPOSE: 
; This is a simplified version of DaViT's GET_COLOR_INDEX for 
; stand-alone distribution.
;
; This function calculates the color index for a given data value.
; The color index depends on the index of the first color to use
; (BOTTOM), the number of colors in the color table (NCOLORS) and the
; number of color steps (COLORSTEPS). It also depends on the SCALE and
; the SCALE_VALUES. 
; 
; CATEGORY: 
; Graphics
; 
; CALLING SEQUENCE: 
; Result = GET_COLOR_INDEX(Values)
;
; INPUTS:
; Values: A scalar or array of numeric values for which to find the
; correct color index.
;
; OUTPUTS:
; This function returns the color index to use if the value is plotted.
;
; KEYWORD PARAMETERS:
; COLORSTEPS: The number of steps in the color table. Default is 8.
;
; NCOLORS: The number of usable colors in the color table. Default is 253.
;
; BOTTOM: The first color index of the color table to use. Default is 1.
;
; SCALE: A 2-element value giving the minimum and maximum value
; to which to scale the data value. Default is based on the maximum of the incoming values.
;
; SCALE_VALUES: The values in the color scale. Usually, the values are scaled
; linearly between MINVAL and MAXVAL
;  SCALE_VALUES = SCALE[0] + FINDGEN(COLORSTEPS) * ( SCALE[1] - SCALE[0] ) / COLORSTEPS
; However, by setting this keyword to a numeric array of values, you can force
; non-linear scaling. The number of elements of SCALE_VALUES must be the same as
; COLORSTEPS+1. If you set this keyword, SCALE will be reset by the first and last value
; of SCALE_VALUES.
;
; ROTATE: The color table loaded in IDL is NEVER changed by DaViT. However, when plotting
; velocities, it is usually desired that the order of the colors is changed. Set this keyword
; to rotate the color values, i.e. the colors at low indeces are used for high data values
; and the colors at high indeces are used for low data values.
;
; SHIFT: The color table loaded in IDL is NEVER changed by DaViT. However, when plotting
; velocities, it is usually desired that the order of the colors is changed. Set this keyword
; to shift the color values, i.e. the color indeces previously associated with low and high
; data values are now associated with values close to the middle of the value scale.
;-
FUNCTION GET_COLOR_INDEX,values                         $
        ,SCALE          = scale                         $
        ,SC_VALUES      = sc_values                     $
        ,CONTINUOUS     = continuous                    $
	,COLORSTEPS     = colorsteps                    $
        ,NCOLORS        = ncolors                       $
        ,BOTTOM         = bottom                        $
	,ROTATE         = _rotate                       $
        ,SHIFT          = _shift

IF ~KEYWORD_SET(colorsteps)     THEN _colorsteps = 8 ELSE _colorsteps = colorsteps
IF ~KEYWORD_SET(ncolors)        THEN ncolors = 253
IF N_ELEMENTS(bottom) EQ 0      THEN Bottom = 1
IF  KEYWORD_SET(colorSteps)     THEN _colorsteps = ncolors

IF ~KEYWORD_SET(scale) THEN BEGIN
    valMax  = MAX(values,/ABSOLUTE,/NAN)
    valMin  = MIN(values,/NAN)
    IF valMin NE 0 THEN _scale = valMax*[-1,1]
    _scale = get_default_range(param)
ENDIF ELSE _scale = scale

IF ~KEYWORD_SET(sc_values) THEN $
    sc_values = scale[0] + FINDGEN(_colorsteps+1)*(_scale[1] - _scale[0])/float(_colorsteps) $
ELSE BEGIN
    IF KEYWORD_SET(colorsteps) THEN BEGIN
            IF colorsteps NE n_elements(sc_values)-1 THEN $
            PRINT, 'Number of values in SC_VALUES is not equal to COLOSTEPS, adjusting.'
    ENDIF
    _colorsteps = N_ELEMENTS(sc_values)-1
    _scale = [MIN(sc_values), MAX(sc_values)]
ENDELSE

; these are the indeces in the color table that are
; available for plotting
cin = FIX( FINDGEN(_colorsteps)/(_colorsteps-1.)*(ncolors-1) )+bottom

; shift or rotate the color indeces
IF KEYWORD_SET(_rotate) THEN cin = ROTATE(cin, 2)
IF KEYWORD_SET(_shift)  THEN cin = SHIFT(cin, _colorsteps/2)

; get color index within cin
_values = (values > _scale[0]) < _scale[1]
ret = BYTARR(N_ELEMENTS(values))
FOR i=0, _colorsteps-1 DO BEGIN
	tmp = WHERE( _values GE sc_values[i] AND _values LT sc_values[i+1], ng )
	IF ng GT 0 THEN ret[tmp] = cin[i]
ENDFOR
tmp = WHERE( _values EQ sc_values[i], ng )
IF ng GT 0 THEN ret[tmp] = cin[i-1]

IF N_ELEMENTS(ret) EQ 1 THEN ret = ret[0]

RETURN, RET
END
