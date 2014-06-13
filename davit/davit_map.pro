PRO DAVIT_MAP
date    = 20110412
time    = [1201,1210]

scale   = [0,1500]
coords  = 'magn'
hemisphere      = -1

SFJUL,date,time,sjul,fjul

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Potential Plots ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
fileName$       = DIR('davit_potential.ps',/PS)
lpJul           = sJul
dateLoaded      = 0
WHILE lpJul[0] LE fJul DO BEGIN
    SFJUL,lpDate,lpTime,lpJul,fJul,/JUL_TO_DATE

    RAD_MAP_READ,lpDate[0],HEMISPHERE=1
    RAD_MAP_PLOT                                                    $
        ,DATE               = lpDate[0]                             $
        ,TIME               = lpTime[0]                             $
        ,SCALE              = scale                                 $
        ,HEMISPHERE         = 1                                     $
        ,XRANGE             = xRange                                $
        ,YRANGE             = yRange                                $
        ,COORDS             = coords                                $
        ,/NEW_PAGE                                                  $
        ,/VECTORS                                                   $
;        ,/POTENTIAL                                                 $
;        ,/EFIELD                                                    $
        ,/COAST                                                     $
        ,/NO_FILL

    RAD_MAP_READ,lpDate[0],HEMISPHERE=-1 
    RAD_MAP_PLOT                                                    $
        ,DATE               = lpDate[0]                             $
        ,TIME               = lpTime[0]                             $
        ,SCALE              = scale                                 $
        ,HEMISPHERE         = -1                                    $
        ,XRANGE             = xRange                                $
        ,YRANGE             = yRange                                $
        ,COORDS             = coords                                $
        ,/NEW_PAGE                                                  $
        ,/VECTORS                                                   $
        ,/POTENTIAL                                                 $
;        ,/EFIELD                                                    $
        ,/COAST                                                     $
        ,/NO_FILL
    PRINT,lpDate,lpTime
    lpJul   = lpJul + 2. / 60. / 24.
    
PS_CLOSE
STOP
ENDWHILE
END
