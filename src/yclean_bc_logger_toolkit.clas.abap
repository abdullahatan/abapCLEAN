*----------------------------------------------------------------------*
*       CLASS YCLEAN_BC_LOGGER_TOOLKIT DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class YCLEAN_BC_LOGGER_TOOLKIT definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IV_LOG_OBJECT type BALOBJ_D
      !IV_LOG_SUBOBJECT type BALSUBOBJ
      !IV_EXTNUMBER type BALNREXT
    exceptions
      LOG_HEADER_INCONSISTENT
      LOGGING_ERROR .
  class-methods GET_INSTANCE
    importing
      !IV_LOG_OBJECT type BALOBJ_D
      !IV_LOG_SUBOBJECT type BALSUBOBJ
      !IV_EXTNUMBER type BALNREXT
    returning
      value(RO_LOGGER_INSTANCE) type ref to YCLEAN_BC_LOGGER_TOOLKIT
    exceptions
      LOG_HEADER_INCONSISTENT
      LOGGING_ERROR .
  methods ADD_ERROR
    importing
      !IV_MSGID type SY-MSGID default 'D3'
      !IV_MSGNO type SY-MSGNO default '232'
      !IV_MSGV1 type ANY optional
      !IV_MSGV2 type ANY optional
      !IV_MSGV3 type ANY optional
      !IV_MSGV4 type ANY optional
      !IV_PROBCLASS type BALPROBCL optional
    exceptions
      LOGGING_ERROR .
  methods ADD_INFO
    importing
      !IV_MSGID type SY-MSGID default 'D3'
      !IV_MSGNO type SY-MSGNO default '232'
      !IV_MSGV1 type ANY optional
      !IV_MSGV2 type ANY optional
      !IV_MSGV3 type ANY optional
      !IV_MSGV4 type ANY optional
      !IV_PROBCLASS type BALPROBCL optional
    exceptions
      LOGGING_ERROR .
  methods ADD_SUCCESS
    importing
      !IV_MSGID type SY-MSGID default 'D3'
      !IV_MSGNO type SY-MSGNO default '232'
      !IV_MSGV1 type ANY optional
      !IV_MSGV2 type ANY optional
      !IV_MSGV3 type ANY optional
      !IV_MSGV4 type ANY optional
      !IV_PROBCLASS type BALPROBCL optional
    exceptions
      LOGGING_ERROR .
  methods ADD_WARNING
    importing
      !IV_MSGID type SY-MSGID default 'D3'
      !IV_MSGNO type SY-MSGNO default '232'
      !IV_MSGV1 type ANY optional
      !IV_MSGV2 type ANY optional
      !IV_MSGV3 type ANY optional
      !IV_MSGV4 type ANY optional
      !IV_PROBCLASS type BALPROBCL optional
    exceptions
      LOGGING_ERROR .
  methods ADD_MSG
    importing
      !IV_MSGTY type MSGTY default 'I'
      !IV_MSGID type SY-MSGID default 'D3'
      !IV_MSGNO type SY-MSGNO default '232'
      !IV_MSGV1 type ANY optional
      !IV_MSGV2 type ANY optional
      !IV_MSGV3 type ANY optional
      !IV_MSGV4 type ANY optional
      !IV_PROBCLASS type BALPROBCL optional
    exceptions
      LOGGING_ERROR .
  methods ADD_MSG_LONG
    importing
      !IV_MSGTY type SY-MSGTY default 'I'
      !IV_MESSAGE type CHAR200
    exceptions
      LOGGING_ERROR .
  methods HAS_MESSAGES
    importing
      value(IM_MSGTYP) type BAPI_MTYPE optional
    returning
      value(RV_EXIST) type ABAP_BOOL .
  methods POST
    importing
      !IV_IN_UPDATE_TASK type ABAP_BOOL default ABAP_FALSE
      !IV_REFRESH type ABAP_BOOL default ABAP_FALSE
      !IV_COMMIT type ABAP_BOOL default ABAP_FALSE
    preferred parameter IV_IN_UPDATE_TASK
    exceptions
      LOGGING_ERROR
      SAVE_NOT_ALLOWED
      NO_MESSAGES_EXIST .
  methods SHOW
    exceptions
      DISPLAY_ERROR
      NO_MESSAGES_EXIST
      LOGGING_ERROR .
  methods MAP_BAPIRET
    returning
      value(R_RETURN) type BAPIRET2_T .
  methods FREE_MSG .
  PROTECTED SECTION.

    METHODS get_log_messages
      RETURNING
        VALUE(rt_log_messages) TYPE bal_t_msg .
    METHODS set_log_messages
      IMPORTING
        !it_log_messages TYPE bal_t_msg .
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_s_logger_instance,
        log_object    TYPE        balobj_d,
        log_subobject TYPE        balsubobj,
        extnumber     TYPE        balnrext,
        logger        TYPE REF TO yclean_bc_logger_toolkit,
      END OF t_s_logger_instance .
    TYPES:
      t_t_logger_instances TYPE STANDARD TABLE OF t_s_logger_instance
                                  WITH KEY log_object log_subobject extnumber .

    DATA mv_log_handle TYPE balloghndl .
    DATA mv_log_object TYPE balobj_d .
    DATA mv_log_subobject TYPE balsubobj .
    DATA mv_extnumber TYPE balnrext .
    DATA mt_log_messages TYPE bal_t_msg .
    CLASS-DATA mt_logger_instances TYPE t_t_logger_instances .
ENDCLASS.



CLASS YCLEAN_BC_LOGGER_TOOLKIT IMPLEMENTATION.


  METHOD add_error.

    add_msg(
      EXPORTING
        iv_msgty      = 'E'
        iv_msgid      = iv_msgid
        iv_msgno      = iv_msgno
        iv_msgv1      = iv_msgv1
        iv_msgv2      = iv_msgv2
        iv_msgv3      = iv_msgv3
        iv_msgv4      = iv_msgv4
        iv_probclass  = iv_probclass
      EXCEPTIONS
        logging_error = 1
        OTHERS        = 2
    ).
    IF sy-subrc EQ 1.
      RAISE logging_error.
    ENDIF.

  ENDMETHOD.                    "add_error


  METHOD add_info.

    add_msg(
      EXPORTING
        iv_msgty      = 'I'
        iv_msgid      = iv_msgid
        iv_msgno      = iv_msgno
        iv_msgv1      = iv_msgv1
        iv_msgv2      = iv_msgv2
        iv_msgv3      = iv_msgv3
        iv_msgv4      = iv_msgv4
        iv_probclass  = iv_probclass
      EXCEPTIONS
        logging_error = 1
        OTHERS        = 2
    ).
    IF sy-subrc EQ 1.
      RAISE logging_error.
    ENDIF.

  ENDMETHOD.                    "ADD_INFO


  METHOD add_msg.

    DATA: ls_msg  TYPE bal_s_msg,
          lv_time TYPE timestamp.

    ls_msg-msgty = iv_msgty.
    ls_msg-msgid = iv_msgid.
    ls_msg-msgno = iv_msgno.
    ls_msg-msgv1 = iv_msgv1.
    ls_msg-msgv2 = iv_msgv2.
    ls_msg-msgv3 = iv_msgv3.
    ls_msg-msgv4 = iv_msgv4.
    ls_msg-probclass = iv_probclass.

    GET TIME.
    TRY.
        CALL METHOD cl_abap_tstmp=>systemtstmp_syst2utc
          EXPORTING
            syst_date = sy-datum
            syst_time = sy-uzeit
          IMPORTING
            utc_tstmp = lv_time.
      CATCH cx_parameter_invalid_range.
        RAISE logging_error.
    ENDTRY.

    ls_msg-time_stmp = lv_time.
    APPEND ls_msg TO mt_log_messages.


  ENDMETHOD.                    "add_msg


  METHOD add_msg_long.

    DATA: lv_msgv1 TYPE sy-msgv1 VALUE space,
          lv_msgv2 TYPE sy-msgv2 VALUE space,
          lv_msgv3 TYPE sy-msgv3 VALUE space,
          lv_msgv4 TYPE sy-msgv4 VALUE space.

    lv_msgv1 = iv_message(50).
    lv_msgv2 = iv_message+50(50).
    lv_msgv3 = iv_message+100(50).
    lv_msgv4 = iv_message+150(50).

    add_msg(
      EXPORTING
        iv_msgty      = iv_msgty
        iv_msgid      = 'D3'
        iv_msgno      = '232'
        iv_msgv1      = lv_msgv1
        iv_msgv2      = lv_msgv2
        iv_msgv3      = lv_msgv3
        iv_msgv4      = lv_msgv4
      EXCEPTIONS
        logging_error = 1
   ).
    IF sy-subrc EQ 1.
      RAISE logging_error.
    ENDIF.

  ENDMETHOD.                    "add_msg_long


  METHOD add_success.

    add_msg(
  EXPORTING
    iv_msgty      = 'S'
    iv_msgid      = iv_msgid
    iv_msgno      = iv_msgno
    iv_msgv1      = iv_msgv1
    iv_msgv2      = iv_msgv2
    iv_msgv3      = iv_msgv3
    iv_msgv4      = iv_msgv4
    iv_probclass  = iv_probclass
  EXCEPTIONS
    logging_error = 1
    OTHERS        = 2
).
    IF sy-subrc EQ 1.
      RAISE logging_error.
    ENDIF.

  ENDMETHOD.                    "add_success


  METHOD add_warning.

    add_msg(
      EXPORTING
        iv_msgty      = 'W'
        iv_msgid      = iv_msgid
        iv_msgno      = iv_msgno
        iv_msgv1      = iv_msgv1
        iv_msgv2      = iv_msgv2
        iv_msgv3      = iv_msgv3
        iv_msgv4      = iv_msgv4
        iv_probclass  = iv_probclass
      EXCEPTIONS
        logging_error = 1
        OTHERS        = 2
    ).
    IF sy-subrc EQ 1.
      RAISE logging_error.
    ENDIF.

  ENDMETHOD.                    "add_warning


  METHOD constructor.

    DATA: ls_log  TYPE bal_s_log,
          ls_mdef TYPE bal_s_mdef.

    ls_log-object    = iv_log_object.
    ls_log-subobject = iv_log_subobject.
    ls_log-extnumber = iv_extnumber.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = ls_log
      IMPORTING
        e_log_handle            = mv_log_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.

    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1.
          RAISE log_header_inconsistent.
        WHEN OTHERS.
          RAISE logging_error.
      ENDCASE.
    ENDIF.

    ls_mdef-log_handle = mv_log_handle.

    CALL FUNCTION 'BAL_GLB_MSG_DEFAULTS_SET'
      EXPORTING
        i_s_msg_defaults = ls_mdef
      EXCEPTIONS
        OTHERS           = 0.

    IF sy-subrc <> 0.
      RAISE logging_error.
    ENDIF.

    mv_log_object    = iv_log_object.
    mv_log_subobject = iv_log_subobject.
    mv_extnumber     = iv_extnumber.

  ENDMETHOD.                    "constructor


  METHOD free_msg.

    FREE me->mt_log_messages[].

  ENDMETHOD.                    "free_msg


  METHOD get_instance.

    DATA: ls_instance TYPE t_s_logger_instance.
    READ TABLE mt_logger_instances WITH KEY log_object    = iv_log_object
                                            log_subobject = iv_log_subobject
                                            extnumber     = iv_extnumber INTO ls_instance.
    IF sy-subrc NE 0.
      ls_instance-log_object = iv_log_object.
      ls_instance-log_subobject = iv_log_subobject.
      ls_instance-extnumber = iv_extnumber.

      CREATE OBJECT ls_instance-logger
        EXPORTING
          iv_log_object           = iv_log_object
          iv_log_subobject        = iv_log_subobject
          iv_extnumber            = iv_extnumber
        EXCEPTIONS
          log_header_inconsistent = 1
          logging_error           = 2
          OTHERS                  = 3.
      IF sy-subrc <> 0.
        CASE sy-subrc.
          WHEN 1.
            RAISE log_header_inconsistent.
          WHEN OTHERS.
            RAISE logging_error.
        ENDCASE.
      ENDIF.
      ro_logger_instance = ls_instance-logger.
      INSERT ls_instance INTO TABLE mt_logger_instances.
    ELSE.
      ro_logger_instance = ls_instance-logger.
    ENDIF.

  ENDMETHOD.                    "get_instance


  METHOD get_log_messages.

    rt_log_messages = mt_log_messages.

  ENDMETHOD.                    "get_log_messages


  METHOD has_messages.

    DATA: lt_msgty_range TYPE RANGE OF bapi_mtype,
          ls_msgty_range LIKE LINE OF lt_msgty_range,
          lr_log         TYPE REF TO bal_s_msg.

    IF im_msgtyp IS NOT INITIAL.
      CLEAR: ls_msgty_range.
      ls_msgty_range-sign   = 'I'.
      ls_msgty_range-option = 'EQ'.
      ls_msgty_range-low    = im_msgtyp.
      APPEND ls_msgty_range TO lt_msgty_range.
    ENDIF.
    LOOP AT mt_log_messages REFERENCE INTO lr_log
                            WHERE msgty IN lt_msgty_range.
      rv_exist = abap_true.
      EXIT.
    ENDLOOP.

  ENDMETHOD.                    "has_messages


  METHOD map_bapiret.

    CHECK me->mt_log_messages[] IS NOT INITIAL.

    CLEAR: r_return.
    CALL FUNCTION 'MSAM_MO_MAP_BAL_TO_BAPIRET'
      EXPORTING
        bal_return = me->mt_log_messages
      IMPORTING
        return     = r_return.

  ENDMETHOD.                    "map_bapiret


  METHOD post.

    FIELD-SYMBOLS: <log_message> TYPE bal_s_msg.

    IF iv_in_update_task EQ abap_true.
      RAISE save_not_allowed.
    ENDIF.

    IF has_messages( ) EQ abap_false.
      RAISE no_messages_exist.
    ENDIF.

    LOOP AT mt_log_messages ASSIGNING <log_message>.

      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle = mv_log_handle
          i_s_msg      = <log_message>
        EXCEPTIONS
          OTHERS       = 9.
      IF sy-subrc <> 0.
        RAISE logging_error.
      ENDIF.
    ENDLOOP.

    DATA lt_log_handle TYPE bal_t_logh.
    APPEND mv_log_handle TO lt_log_handle.

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_in_update_task = iv_in_update_task
        i_t_log_handle   = lt_log_handle
      EXCEPTIONS
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      RAISE logging_error.
    ENDIF.

    IF iv_commit EQ abap_true.
      COMMIT WORK AND WAIT.
    ENDIF.

    IF iv_refresh EQ abap_true.
      me->free_msg( ).
    ENDIF.

  ENDMETHOD.                    "post


  METHOD set_log_messages.

    mt_log_messages = it_log_messages.

  ENDMETHOD.                    "set_log_messages


  METHOD show.

    DATA: lt_log_handle TYPE bal_t_logh.

    IF has_messages( ) EQ abap_false.
      RAISE no_messages_exist.
    ENDIF.

    REFRESH: lt_log_handle.
    APPEND mv_log_handle TO lt_log_handle.
    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_t_log_handle       = lt_log_handle
      EXCEPTIONS
        profile_inconsistent = 1
        internal_error       = 2
        no_data_available    = 3
        no_authority         = 4
        OTHERS               = 5.
    IF sy-subrc <> 0.
      RAISE display_error.
    ENDIF.

  ENDMETHOD.                    "show
ENDCLASS.
