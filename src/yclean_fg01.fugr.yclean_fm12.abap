FUNCTION yclean_fm12.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_RLDNR) TYPE  RLDNR
*"     VALUE(IV_BUKRS) TYPE  BUKRS
*"     VALUE(IV_KOKRS) TYPE  KOKRS
*"     VALUE(IV_GJAHR) TYPE  GJAHR
*"     VALUE(IV_POPER) TYPE  YCLEAN_TT04
*"     VALUE(IV_BELNR) TYPE  YCLEAN_TT08 OPTIONAL
*"     VALUE(IV_LOGID) TYPE  BALNREXT
*"  EXCEPTIONS
*"      CONTAINS_ERROR
*"----------------------------------------------------------------------
  DATA: _msg TYPE c LENGTH 150.
  DATA(_logger) = yclean_bc_logger_toolkit=>get_instance(
  EXPORTING
    iv_log_object = mc_logger-_object
    iv_log_subobject = mc_logger-_subobject
    iv_extnumber = iv_logid ).

  IF lines( iv_poper ) GT 1.
    CONCATENATE LINES OF iv_poper INTO DATA(_poper) SEPARATED BY '~'.
  ELSE.
    _poper = VALUE #( iv_poper[ 1 ]-poper OPTIONAL ).
  ENDIF.
  _logger->add_success( iv_msgid = mc_msg-id iv_msgno = '002' iv_msgv1 = TEXT-t15 iv_msgv2 = |Kontrol Kodu:{ iv_kokrs };Yıl:{ iv_gjahr }| iv_msgv3 = |Dönem:{ _poper  }| ).
  _logger->post( iv_refresh = abap_true ).
  TRY.
      yclean_cl04=>_rb1_rundat(
        EXPORTING
          iv_rldnr = iv_rldnr
          iv_bukrs = iv_bukrs
          iv_gjahr = iv_gjahr
          iv_kokrs = iv_kokrs
          iv_poper = iv_poper[]
          iv_belnr = iv_belnr[]
        IMPORTING
          ev_subrc  = DATA(_subrc)
          ev_result = DATA(_result) ).
      IF _subrc IS INITIAL.
        _logger->add_success( iv_msgid = mc_msg-id iv_msgno = '019' iv_msgv1 = TEXT-t15 ).
      ELSE.
        _logger->add_error( iv_msgid = mc_msg-id iv_msgno = '004' iv_msgv1 = TEXT-t15 iv_msgv2 = _result ).
      ENDIF.
    CATCH cx_amdp_error INTO DATA(lv_exp).
      CLEAR: _msg. _msg = lv_exp->get_longtext( ).
      _logger->add_error( iv_msgid = mc_msg-id iv_msgno = '005' iv_msgv1 = TEXT-t15 iv_msgv2 = _msg+0(50) iv_msgv3 = _msg+50(50) iv_msgv4 = _msg+100(50) ).
  ENDTRY.
  _logger->post( iv_refresh = abap_true ).
ENDFUNCTION.
