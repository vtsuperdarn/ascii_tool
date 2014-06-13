;+
; NAME:
; RAD_LOAD_COLORTABLE
;
; PURPOSE:
; This is a simplified version of DaViT's RAD_LOAD_COLORTABLE for
; stand-alone distribution.
;
; CATEGORY:
; Graphics
;
; CALLING SEQUENCE:
; RAD_LOAD_COLORTABLE
;
; KEYWORD PARAMETERS:
; AJ: This color table loads by default.
;
; BLUEWHITERED: Set this keyword to load a color table ranging from blue (lowest)
; through light gray (middle) to red (highest).
;
; MODIFICATION HISTORY:
; Based on Steve Milan's CUT_COL_TAB.
; Written by Lasse Clausen, Nov. 24, 2009
; Modified by Nathaniel Frissell, Nov. 10, 2011
;-
PRO RAD_LOAD_COLORTABLE,BLUEWHITERED=bluewhitered, AJ=aj

ncolors = 253
bottom  = 1
black   = 0
white   = 255
gray    = 254

red   = bytarr(ncolors)
green = bytarr(ncolors)
blue  = bytarr(ncolors)

IF keyword_set(bluewhitered) then BEGIN
	gray_base=0.95
	red[0:ncolors/2-1]         = reverse(((ncolors-1.)-2.*findgen(ncolors/2))*gray_base)
	green[0:ncolors/2-1]       = reverse(((ncolors-1.)-2.*findgen(ncolors/2))*gray_base)
	blue[0:ncolors/2-1]        = reverse(2.*findgen(ncolors/2)*(1.-gray_base) + (ncolors-1.)*gray_base)
	red[ncolors/2:ncolors-1]   = 2.*findgen(ncolors/2+1)*(1.-gray_base) + (ncolors-1.)*gray_base
	green[ncolors/2:ncolors-1] = ((ncolors-1.)-2.*findgen(ncolors/2+1))*gray_base
	blue[ncolors/2:ncolors-1]  = ((ncolors-1.)-2.*findgen(ncolors/2+1))*gray_base
ENDIF ELSE BEGIN
	rcol = reverse(shift(reverse([ 37,     255,     255,     255,     124,       0,       0,       0]), 4))
	gcol = reverse(shift(reverse([255,     248,     135,      23,       0,       6,     209,     255]), 4))
	bcol = reverse(shift(reverse([  0,       0,       0,       0,     255,     255,     255,     188]), 4))
	nncolors = n_elements(rcol)
	d = ncolors/(nncolors-1)
	for i=0, nncolors-2 do begin
		red[findgen(d+(i eq nncolors-2))+i*d] = rcol[i]+findgen(d+(i eq nncolors-2))/float(d-1+(i eq nncolors-2))*(rcol[i+1]-rcol[i])
		green[findgen(d+(i eq nncolors-2))+i*d] = gcol[i]+findgen(d+(i eq nncolors-2))/float(d-1+(i eq nncolors-2))*(gcol[i+1]-gcol[i])
		blue[findgen(d+(i eq nncolors-2))+i*d] = bcol[i]+findgen(d+(i eq nncolors-2))/float(d-1+(i eq nncolors-2))*(bcol[i+1]-bcol[i])
	ENDFOR
ENDELSE

ored   = bytarr(256)
ogreen = bytarr(256)
oblue  = bytarr(256)
ored[bottom:bottom+ncolors-1]   = red
ogreen[bottom:bottom+ncolors-1] = green
oblue[bottom:bottom+ncolors-1]  = blue

; Black and white
ored[black]   = 0
oblue[black]  = 0
ogreen[black] = 0
ored[white]   = 255
oblue[white]  = 255
ogreen[white] = 255

; Ground scatter colour (grey)
ored[gray]    = 200
oblue[gray]   = 200
ogreen[gray]  = 190

IF !D.NAME NE 'NULL' THEN TVLCT,ored,ogreen,oblue

END
