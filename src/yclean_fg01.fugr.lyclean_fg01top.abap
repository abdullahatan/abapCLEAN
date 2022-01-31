FUNCTION-POOL yclean_fg01.                  "MESSAGE-ID ..

* INCLUDE LYCLEAN_FG01D...                   " Local class definition

CONSTANTS: BEGIN OF mc_logger,
             _object    TYPE balobj_d VALUE 'YCLEAN',
             _subobject TYPE balsubobj VALUE '',
           END OF mc_logger.

CONSTANTS: BEGIN OF mc_msg,
             id      TYPE symsgid    VALUE 'YCLEAN',
             success TYPE bapi_mtype VALUE 'S',
             error   TYPE bapi_mtype VALUE 'E',
             warning TYPE bapi_mtype VALUE 'W',
             info    TYPE bapi_mtype VALUE 'I',
             abort   TYPE bapi_mtype VALUE 'A',
           END OF mc_msg.
