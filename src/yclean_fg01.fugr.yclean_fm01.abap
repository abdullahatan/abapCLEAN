FUNCTION yclean_fm01.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_KIND) TYPE  YCLEAN_DE01
*"     VALUE(IV_BUKRS) TYPE  BUKRS
*"     VALUE(IV_GJAHR) TYPE  GJAHR
*"     VALUE(IV_BSTAT) TYPE  BSTAT_D
*"     VALUE(IV_LDGRP) TYPE  STRING OPTIONAL
*"     VALUE(IV_BELNR) TYPE  STRING OPTIONAL
*"     VALUE(IV_LOGID) TYPE  BALNREXT
*"  EXCEPTIONS
*"      PARAMETER_ERROR
*"      KIND_INVALID
*"----------------------------------------------------------------------

  DATA(_logger) = yclean_bc_logger_toolkit=>get_instance(
    EXPORTING
      iv_log_object = mc_logger-_object
      iv_log_subobject = mc_logger-_subobject
      iv_extnumber = iv_logid ).

  CASE iv_kind.
    WHEN 'RB1'.
      IF iv_bstat IS INITIAL.
        MESSAGE e008(yclean) RAISING parameter_error WITH 'IV_BSTAT'.
      ENDIF.
      _logger->add_success( iv_msgid = mc_msg-id iv_msgno = '002' iv_msgv1 = TEXT-t01 iv_msgv2 = |{ TEXT-p01 }:{ iv_bukrs }| iv_msgv3 = |{ TEXT-p05 }:{ iv_gjahr }| iv_msgv4 = |{ TEXT-p02 }:{ iv_bstat }| ).
      _logger->post( iv_refresh = abap_true iv_commit = abap_true ).
      TRY.
          yclean_cl01=>_rb1a_rundat(
            EXPORTING
              iv_bukrs  = iv_bukrs
              iv_gjahr  = iv_gjahr
              iv_bstat  = iv_bstat
              iv_belnr  = iv_belnr
            IMPORTING
              ev_subrc  = DATA(_subrc)
              ev_rowcnt = DATA(_rowcnt)
              ev_result = DATA(_result) ).
          IF _subrc IS INITIAL.
            _logger->add_success( iv_msgid = mc_msg-id iv_msgno = '003' iv_msgv1 = TEXT-t01 iv_msgv2 = |{ _rowcnt NUMBER = USER }| ).
          ELSE.
            _logger->add_error( iv_msgid = mc_msg-id iv_msgno = '004' iv_msgv1 = TEXT-t01 iv_msgv2 = _result ).
          ENDIF.
        CATCH cx_amdp_error INTO DATA(lv_exp).
          _logger->add_error( iv_msgid = mc_msg-id iv_msgno = '005' iv_msgv1 = TEXT-t01 iv_msgv2 = lv_exp->get_text( ) ).
      ENDTRY.
      _logger->post( iv_refresh = abap_true ).
    WHEN 'RB2'.
      IF iv_bstat IS INITIAL OR iv_ldgrp IS INITIAL.
        MESSAGE e008(yclean) RAISING parameter_error WITH 'IV_BSTAT' '&' 'IV_LDGRP'.
      ENDIF.
      _logger->add_success( iv_msgid = mc_msg-id iv_msgno = '002' iv_msgv1 = TEXT-t08 iv_msgv2 = |{ TEXT-p01 }:{ iv_bukrs }| iv_msgv3 = |{ TEXT-p05 }:{ iv_gjahr }| iv_msgv4 = |{ TEXT-p02 }:{ iv_bstat }| ).
      _logger->post( iv_refresh = abap_true iv_commit = abap_true ).
      TRY.
          yclean_cl01=>_rb1b_rundat(
            EXPORTING
              iv_bukrs  = iv_bukrs
              iv_gjahr  = iv_gjahr
              iv_bstat  = iv_bstat
              iv_ldgrp  = iv_ldgrp
            IMPORTING
              ev_subrc  = _subrc
              ev_rowcnt = _rowcnt
              ev_result = _result ).
          IF _subrc IS INITIAL.
            _logger->add_success( iv_msgid = mc_msg-id iv_msgno = '003' iv_msgv1 = TEXT-t08 iv_msgv2 = |{ _rowcnt NUMBER = USER }| ).
          ELSE.
            _logger->add_error( iv_msgid = mc_msg-id iv_msgno = '004' iv_msgv1 = TEXT-t08 iv_msgv2 = _result ).
          ENDIF.
        CATCH cx_amdp_error INTO lv_exp.
          _logger->add_error( iv_msgid = mc_msg-id iv_msgno = '005' iv_msgv1 = TEXT-t08 iv_msgv2 = lv_exp->get_text( ) ).
      ENDTRY.
      _logger->post( iv_refresh = abap_true ).
    WHEN OTHERS.
      MESSAGE e006(yclean) RAISING kind_invalid.
  ENDCASE.

ENDFUNCTION.
