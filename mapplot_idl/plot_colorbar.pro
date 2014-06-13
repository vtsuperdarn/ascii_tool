;+ 
; NAME: 
; PLOT_COLORBAR 
; 
; PURPOSE: 
; This procedure plots a horizontal colorbar.  It is a simplified version of
; DaViT's PLOT_COLORBAR for stand-alone distribution.
; 
; CATEGORY: 
; Graphics
; 
; CALLING SEQUENCE: 
; PLOT_COLORBAR
;
; KEYWORD PARAMETERS: 
; POSITION: Set this to a 4-elements array specifying the colorbar's position
; if you don't want this routine to calculate where to put it.
;
; LEGEND: Set this to a string to overwrite the default of the colorbar's title
; depending on the loaded parameter.
;
; LEVEL_FORMAT: Set this to a format code to use for the labels.
;
; CHARSIZE: Set this to a number to override the default charsize.
;
; SCALE: Set this to a 2-element vector indicating the scale of the colorbar.
; If omitted, the scale from the common block USER_PREFS is used. 
;
; CHARTHICK: Set this to a number to override the default character thickness.
;
; NO_LABELS:
;
; STEPS: Set this keyword to give the number of steps in the colorbar. If not
; provided, the output from GET_COLORSTEPS() is used.
;
; MODIFICATION HISTORY: 
; Based on Steve Milan's PLOT_COLOURBAR.
; Written by Lasse Clausen, Nov. 24, 2009
; Completly rewritten by Lasse Clausen, Jan. 18, 2011
; Modified by Nathaniel Frissell, Nov. 10, 2011
;-
PRO PLOT_COLORBAR                                                       $
    ,position           = position                                      $
    ,charsize           = charsize                                      $
    ,charthick          = charthick                                     $
    ,scale              = _scale                                        $
    ,sc_values          = sc_values                                     $
    ,legend             = legend                                        $
    ,nlevels            = nlevels                                       $
    ,level_format       = level_format                                  $
    ,level_values       = level_values                                  $
    ,colorsteps         = colorsteps                                    $
    ,continuous         = continuous                                    $
    ,no_labels          = no_labels                                     $
    ,rotate             = rotate                                        $
    ,shift              = shift                                         $
    ,xthick             = xthick                                        $
    ,ythick             = ythick                                        $
    ,logarithmic        = logarithmic

if ~keyword_set(_scale) then _scale = [-150,150]

if n_elements(_scale) ne 2 then begin
	PRINT, 'SCALE must be 2-element vector.'
	return
endif

if n_elements(legend) eq 0 then legend = ' '

IF ~KEYWORD_SET(charsize) THEN charsize = 1 

if ~keyword_set(charthick) then charthick = !p.charthick

IF KEYWORD_SET(position) THEN bpos = position ELSE bpos = [0.15,0.10,0.9,0.15]

; get color preferences
foreground  = 0
ncolors     = 253
bottom      = 1

if ~keyword_set(_colorsteps) then _colorsteps = 8
if  keyword_set(continuous) then _colorsteps = ncolors

if ~keyword_set(sc_values) then $
	_sc_values = _scale[0] + FINDGEN(_colorsteps+1)*(_scale[1] - _scale[0])/float(_colorsteps) $
else begin
	_sc_values = sc_values
	if keyword_set(colorsteps) then begin
		if colorsteps ne n_elements(_sc_values)-1 then $
			PRINT, 'Number of values in SC_VALUES is not equal to COLOSTEPS, adjusting.'
	endif
	_colorsteps = n_elements(_sc_values)-1
	_scale = [min(sc_values), max(sc_values)]
endelse

if ~keyword_set(level_format) then BEGIN
   IF MAX(_scale,/ABSOLUTE) LT 10 THEN level_format = '(F5.2)' ELSE level_format = '(I)'
ENDIF

if n_elements(nlevels) eq 0 then $
	_nlevels = ( _colorsteps gt 60 ? 8 : _colorsteps ) $
else $
	_nlevels = nlevels

if n_elements(level_values) eq 0 then begin
	if keyword_set(sc_values) then $
		_level_values = _sc_values $
	else $
		_level_values = _scale[0] + FINDGEN(_nlevels+1)*(_scale[1] - _scale[0])/float(_nlevels)
endif else begin
	_level_values = level_values
	if keyword_set(nlevels) then begin
		if nlevels+1 ne n_elements(_level_values) then begin
			prinfo, 'NLEVELS must be N_ELEMENTS(LEVEL_VALUES)-1. Adjusting.'
			_nlevels = n_elements(_level_values) - 1
		endif
	endif else $
		_nlevels = n_elements(_level_values) - 1
endelse

; these are the indeces in the color table that are
; available for plotting
cin = FIX( FINDGEN(_colorsteps)/(_colorsteps-1.)*(ncolors-1) )+bottom

; shift or rotate the color indeces
if keyword_set(_rotate) then $
	cin = rotate(cin, 2)
if keyword_set(_shift) then $
	cin = shift(cin, _colorsteps/2)

; set ranges
crange = [0, _colorsteps]
wrange = [0,1]
ticks = _nlevels
if strcmp(level_format, 'label_date') then begin
	tickname = strarr(n_elements(_level_values))
	for i=0, n_elements(_level_values)-1 do begin
		tickname[i] = label_date(0, 0, _level_values[i])
	endfor
endif else $
	tickname = strtrim(string(_level_values,format=level_format),2)
if keyword_set(no_labels) then tickname = replicate(' ', ticks+1)
if keyword_set(logarithmic) then tickname = ''

; Plot horizontal colorbar. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
plot, [0,0], xstyle=5, ystyle=5, yrange=wrange, xrange=crange, $
        position=bpos, /nodata
for c=0, _colorsteps-1 do $
        polyfill, c+[0,1,1,0,0], [0,0,1,1,0], color=cin[c]
plot, [0,0], xstyle=5, ystyle=1, yrange=wrange, xrange=crange, $
        position=bpos, /nodata, xticks=ticks, xtick_get=tickvals, $
        yticks=1, ytickname=replicate(' ', 2), $
        xthick=xthick, ythick=ythick, xlog=logarithmic
axis, xaxis=0, xstyle=1, xrange=_scale, xticks=ticks, $
        xtickname=tickname, xtitle=legend, charthick=charthick, charsize=charsize, $
        xthick=xthick, ythick=ythick, xlog=logarithmic
axis, xaxis=1, xstyle=1, xrange=_scale, xticks=ticks, $
        xtickname=replicate(' ', ticks+1), xticklen=1, charthick=charthick, charsize=charsize, $
        xthick=xthick, ythick=ythick, xlog=logarithmic
return
END
