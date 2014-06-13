PRO dataout

date            = 20120122
stime           = 0000
etime           = 0030
hemi            = 0
timeStep        = 10

sDate           = 20080229
eDate           = 20080229
stime           = 0000
etime           = 0100
hemi            = 0
timeStep        = 10
;min_lat         = 50.


RAD_MAP_DATA2ASCII,[sDate,eDate]                                $
    ,TIME               = [stime,etime]                         $
    ,INT_HEMI           = hemi                                  $
    ,STEPTIME           = timeStep                              $
    ,STATUS             = status                                $
    ,MIN_LAT            = min_lat                               $
    ,OUTPUT_FILE        = fileName                              $
    ,LOG_FILE           = 'ascii_log.txt'

MAPPLOT,DATAFILE=fileName,/POTENTIAL,/VELOCITY,/TEST
MAPPLOT,DATAFILE=fileName,/VELOCITY,/TEST
MAPPLOT,DATAFILE=fileName,/POTENTIAL,/TEST
MAPPLOT,DATAFILE=fileName,/POTENTIAL,/EFIELD,/TEST

PRINT,'Status: ',status
IF KEYWORD_SET(status) THEN PRINT,'NODATA'

SPAWN,'pspng *.ps'
STOP
END
