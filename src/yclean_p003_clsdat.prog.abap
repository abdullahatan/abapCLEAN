*&---------------------------------------------------------------------*
*& Include          YCLEAR_P001_CLSDAT
*&---------------------------------------------------------------------*
TYPE-POOLS: vrm.
TABLES: sscrfields, ckmlcr, mldoc.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-s01.
  PARAMETERS:
    p_rb1 RADIOBUTTON GROUP gr1 DEFAULT 'X' USER-COMMAND cmd1 MODIF ID rd1,
    p_rb2 RADIOBUTTON GROUP gr1 MODIF ID rd2,
    p_rb3 RADIOBUTTON GROUP gr1 MODIF ID rd3.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-s02.
  PARAMETERS:
    p_bdatj TYPE ckmlcr-bdatj OBLIGATORY DEFAULT sy-datum MODIF ID m01,
    p_curtp TYPE ckmlcr-curtp AS LISTBOX VISIBLE LENGTH 6 DEFAULT '30' OBLIGATORY MODIF ID m02.

  SELECT-OPTIONS:
    s_kalnr  FOR ckmlcr-kalnr NO INTERVALS MODIF ID m03,
    s_docref FOR mldoc-docref NO INTERVALS MODIF ID m04.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-s03.
  PARAMETERS:
    p_prll TYPE int1 AS LISTBOX VISIBLE LENGTH 6 DEFAULT 1 OBLIGATORY MODIF ID m06,
    p_rz12 TYPE rzlli_apcl DEFAULT '' MODIF ID m06.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-s04.
  PARAMETERS: p_cb1 AS CHECKBOX DEFAULT ''.
SELECTION-SCREEN END OF BLOCK b4.

CLASS application DEFINITION DEFERRED.
DATA: app_session TYPE REF TO application.

*----------------------------------------------------------------------*
*       CLASS application DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS application DEFINITION .

  PUBLIC SECTION.
    CLASS-DATA:
      app TYPE REF TO application.

    CONSTANTS: BEGIN OF mc_cons,
                 rz12 TYPE fieldname VALUE 'P_RZ12',
               END OF mc_cons.

    DATA: mv_tabname  TYPE char40,
          mv_taskname TYPE char40,
          mv_logid    TYPE balnrext.

    CLASS-METHODS:
      initialization,
      at_selection_screen,
      at_selection_screen_output,
      at_selection_screen_request
        IMPORTING
          !iv_fieldname TYPE clike,
      start_of_selection,
      end_of_selection,
      app_instance
        RETURNING
          VALUE(mo_app) TYPE REF TO application.

    METHODS:
      listbox_build,
      f4_finanscal_perio
        CHANGING
          cv_spmon TYPE s031-spmon,
      f4_server_group
        CHANGING
          cv_rz12 TYPE rzlli_apcl,
      rundat_control
        EXCEPTIONS
          contains_error,
      retrieve_dat
        EXCEPTIONS
          contains_error,
      generate_guid
        RETURNING
          VALUE(rv_guid) TYPE guid_16,
      popup_confirm
        IMPORTING
          !im_titlebar     TYPE clike
          !im_question     TYPE clike
        RETURNING
          VALUE(rv_answer) TYPE char1,
      show_message_tool
        IMPORTING
          !iv_msgdat TYPE bapiret2_tab.

  PRIVATE SECTION.
    DATA: mt_msgdat TYPE STANDARD TABLE OF bapiret2 WITH DEFAULT KEY.

ENDCLASS .                    "application DEFINITION

*----------------------------------------------------------------------*
*       CLASS application IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS application IMPLEMENTATION .

  METHOD initialization.
    app->listbox_build( ).
  ENDMETHOD.                    "initialization
  METHOD at_selection_screen.

    CASE sy-ucomm.
      WHEN 'ONLI'.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.                    "at_selection_screen
  METHOD at_selection_screen_output.

    LOOP AT SCREEN.
      CLEAR: app_session->mv_tabname.
      CASE abap_true.
        WHEN p_rb1.
          FREE: s_kalnr[], s_docref[].
          app_session->mv_tabname = TEXT-t01.
          IF screen-group1 EQ 'M04'.
            screen-active = 0.
          ENDIF.
        WHEN p_rb2.
          FREE: s_kalnr[], s_docref[].
          app_session->mv_tabname = TEXT-t01.
          IF screen-group1 EQ 'M03'.
            screen-active = 0.
          ENDIF.
          app_session->mv_tabname = TEXT-t02.
        WHEN p_rb3.
          FREE: s_kalnr[], s_docref[].
          app_session->mv_tabname = TEXT-t03.
          IF screen-group1 EQ 'M03'.
            screen-active = 0.
          ENDIF.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.

  ENDMETHOD.                    "at_selection_screen_output
  METHOD at_selection_screen_request.

    CASE iv_fieldname.
      WHEN 'P_RZ12'.
        app->f4_server_group(
          CHANGING
            cv_rz12 = p_rz12 ).
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.                    "at_selection_screen_request
  METHOD start_of_selection.

    IF sy-uname <> 'AHMETS' AND sy-uname <> 'ABDULLAHA'.
      MESSAGE s017(yclean) DISPLAY LIKE 'E'. RETURN.
    ENDIF.

    app->retrieve_dat(
      EXCEPTIONS
        contains_error = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.                    "start_of_selection
  METHOD end_of_selection.


  ENDMETHOD.                    "end_of_selection
  METHOD app_instance.

    FREE: mo_app, app.
    IF application=>app IS NOT BOUND.
      CREATE OBJECT application=>app.
    ENDIF.
    mo_app = app.

  ENDMETHOD.                    "app_instance
  METHOD listbox_build.

    DATA: _values TYPE vrm_values,
          _id     TYPE vrm_id.

    FREE: _values, _id.
    _id = 'P_PRLL'.
    _values = VALUE #( BASE _values ( key = '01' ) ( key = '02' ) ( key = '03' ) ( key = '04' ) ( key = '06' ) ( key = '12' ) ).
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = _id
        values = _values.

    FREE: _values, _id.
    _id = 'P_CURTP'.
    _values = VALUE #( BASE _values ( key = '30' ) ( key = '40' ) ).
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = _id
        values = _values.

  ENDMETHOD.                    "listbox_build
  METHOD f4_finanscal_perio.

    DATA: l_returncode1 TYPE sy-subrc,
          l_monat1      TYPE isellist-month,
          l_hlp_repid1  TYPE sy-repid.

    l_monat1 = sy-datum+0(6).
    CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
      EXPORTING
        actual_month               = l_monat1
        factory_calendar           = ' '
        holiday_calendar           = ' '
        language                   = sy-langu
        start_column               = 8
        start_row                  = 5
      IMPORTING
        selected_month             = l_monat1
        return_code                = l_returncode1
      EXCEPTIONS
        factory_calendar_not_found = 1
        holiday_calendar_not_found = 2
        month_not_found            = 3
        OTHERS                     = 4.
    IF sy-subrc = 0 AND l_returncode1 = 0.
      cv_spmon = l_monat1.
    ENDIF.

  ENDMETHOD.                    "f4_finanscal_perio
  METHOD f4_server_group.

    DATA: return_tab TYPE STANDARD TABLE OF ddshretval.
    SELECT classname, applserver, grouptype FROM rzllitab INTO TABLE @DATA(t_rzllitab) WHERE grouptype EQ 'S'.
    IF sy-subrc IS INITIAL.
      FREE: return_tab.
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield    = 'CLASSNAME'
          dynpprog    = sy-repid
          dynpnr      = sy-dynnr
          dynprofield = 'P_RZ12'
          value_org   = 'S'
        TABLES
          value_tab   = t_rzllitab
          return_tab  = return_tab.
      READ TABLE return_tab REFERENCE INTO DATA(_return) INDEX 1.
      IF sy-subrc IS INITIAL.
        cv_rz12 = _return->fieldval.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "f4_server_group
  METHOD rundat_control.

  ENDMETHOD.                    "rundat_control
  METHOD retrieve_dat.

    DATA: _where   TYPE string,
          t_kalnr  TYPE yclean_tt06,
          t_docref TYPE yclean_tt07,
          _result  TYPE string.

    CHECK popup_confirm( im_titlebar = TEXT-h01 im_question = |{ mv_tabname } { TEXT-q01 }| ) EQ '1'.
    CASE abap_true.
*--------------------------------------------------------------------*
*-&Step-1: Delete From CKMLCR ->
*--------------------------------------------------------------------*
      WHEN p_rb1.

        FREE: t_kalnr.
        LOOP AT s_kalnr REFERENCE INTO DATA(_kalnr).
          APPEND VALUE #( kalnr = _kalnr->low ) TO t_kalnr.
        ENDLOOP.

        DATA(_output) = cl_demo_output=>new( )->begin_section( |Günlük Bilgileri({ TEXT-t01 })| )->begin_section( |Nesne:| )->write_text( |YCLEAN| )->next_section( |Harici tanıtıcı:| ).

        NEW yclean_cl03( )->_monat_splitdat(
          EXPORTING
            im_split       = p_prll
          RECEIVING
            rt_splitdat    = DATA(t_splitdat)
          EXCEPTIONS
            contains_error = 1
            OTHERS         = 2 ).
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
        ENDIF.

        LOOP AT t_splitdat REFERENCE INTO DATA(_splitdat).
          CLEAR: mv_logid, mv_taskname.
          mv_taskname = |CKMLCR_{ p_bdatj }_{ p_curtp }/T{ _splitdat->line }|.
          mv_logid = |{ mv_taskname }-{ generate_guid( ) }|.

          CALL FUNCTION 'YCLEAN_FM09' "STARTING NEW TASK mv_taskname DESTINATION IN GROUP p_rz12
            EXPORTING
              iv_bdatj       = p_bdatj
              iv_poper       = _splitdat->monats[]
              iv_curtp       = p_curtp
              iv_kalnr       = t_kalnr
              iv_logid       = mv_logid
            EXCEPTIONS
              contains_error = 1
              OTHERS         = 2.

          _output->write_text( |{ mv_logid ALPHA = OUT }| ).

        ENDLOOP.
        IF p_cb1 EQ abap_false.
          _output->display( ).
        ENDIF.
*--------------------------------------------------------------------*
*-&Step-2: Delete From MLDOC ->
*--------------------------------------------------------------------*
      WHEN p_rb2.
        FREE: t_docref.
        LOOP AT s_docref REFERENCE INTO DATA(_docref).
          APPEND VALUE #( docref = _docref->low ) TO t_docref.
        ENDLOOP.

        _output = cl_demo_output=>new( )->begin_section( |Günlük Bilgileri({ TEXT-t01 })| )->begin_section( |Nesne:| )->write_text( |YCLEAN| )->next_section( |Harici tanıtıcı:| ).

        NEW yclean_cl03( )->_jahrper_splitdat(
          EXPORTING
            im_gjahr       = p_bdatj
            im_split       = p_prll
          RECEIVING
            rt_splitdat    = DATA(t_jahrperdat)
          EXCEPTIONS
            contains_error = 1
            OTHERS         = 2 ).
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
        ENDIF.

        LOOP AT t_jahrperdat REFERENCE INTO DATA(_jahrperdat).
          CLEAR: mv_logid, mv_taskname.
          mv_taskname = |MLDOC_{ p_bdatj }_{ p_curtp }/T{ _jahrperdat->line }|.
          mv_logid = |{ mv_taskname }-{ generate_guid( ) }|.

          CALL FUNCTION 'YCLEAN_FM10' STARTING NEW TASK mv_taskname DESTINATION IN GROUP p_rz12
            EXPORTING
              iv_jahrper     = _jahrperdat->jahrpers[]
              iv_curtp       = p_curtp
              iv_docref      = t_docref
              iv_logid       = mv_logid
            EXCEPTIONS
              contains_error = 1
              OTHERS         = 2.

          _output->write_text( |{ mv_logid ALPHA = OUT }| ).

        ENDLOOP.
        IF p_cb1 EQ abap_false.
          _output->display( ).
        ENDIF.
*--------------------------------------------------------------------*
*-&Step-2: Delete From MLDOC_EXTRACT ->
*--------------------------------------------------------------------*
      WHEN p_rb3.
        FREE: t_docref.
        LOOP AT s_docref REFERENCE INTO _docref.
          APPEND VALUE #( docref = _docref->low ) TO t_docref.
        ENDLOOP.

        _output = cl_demo_output=>new( )->begin_section( |Günlük Bilgileri({ TEXT-t01 })| )->begin_section( |Nesne:| )->write_text( |YCLEAN| )->next_section( |Harici tanıtıcı:| ).

        FREE: t_jahrperdat.
        NEW yclean_cl03( )->_jahrper_splitdat(
          EXPORTING
            im_gjahr       = p_bdatj
            im_split       = p_prll
          RECEIVING
            rt_splitdat    = t_jahrperdat
          EXCEPTIONS
            contains_error = 1
            OTHERS         = 2 ).
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
        ENDIF.

        LOOP AT t_jahrperdat REFERENCE INTO _jahrperdat.
          CLEAR: mv_logid, mv_taskname.
          mv_taskname = |MLDOC_EXTRACT{ p_bdatj }_{ p_curtp }/T{ _jahrperdat->line }|.
          mv_logid = |{ mv_taskname }-{ generate_guid( ) }|.

          CALL FUNCTION 'YCLEAN_FM11' STARTING NEW TASK mv_taskname DESTINATION IN GROUP p_rz12
            EXPORTING
              iv_jahrper     = _jahrperdat->jahrpers[]
              iv_curtp       = p_curtp
              iv_docref      = t_docref
              iv_logid       = mv_logid
            EXCEPTIONS
              contains_error = 1
              OTHERS         = 2.

          _output->write_text( |{ mv_logid ALPHA = OUT }| ).

        ENDLOOP.
        IF p_cb1 EQ abap_false.
          _output->display( ).
        ENDIF.
    ENDCASE.


  ENDMETHOD .                    "retrieve_dat
  METHOD generate_guid.
    CALL FUNCTION 'GUID_CREATE'
      IMPORTING
        ev_guid_16 = rv_guid.
  ENDMETHOD .                    "generate_guid
  METHOD popup_confirm.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = im_titlebar
        text_question         = im_question
        text_button_1         = TEXT-d01
        text_button_2         = TEXT-d02
        popup_type            = 'ICON_MESSAGE_CRITICAL'
        default_button        = '2'
        display_cancel_button = abap_true
      IMPORTING
        answer                = rv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

  ENDMETHOD.                    "popup_confirm
  METHOD show_message_tool.

    CHECK iv_msgdat[] IS NOT INITIAL.

    DATA: lt_message_tab TYPE esp1_message_tab_type,
          lr_message_tab TYPE REF TO esp1_message_wa_type,
          lr_messtab     TYPE REF TO bapiret2.


    LOOP AT iv_msgdat REFERENCE INTO lr_messtab.
      APPEND INITIAL LINE TO lt_message_tab
         REFERENCE INTO lr_message_tab.
      lr_message_tab->msgty = lr_messtab->type       .
      lr_message_tab->msgid = lr_messtab->id         .
      lr_message_tab->msgno = lr_messtab->number     .
      lr_message_tab->msgv1 = lr_messtab->message_v1 .
      lr_message_tab->msgv2 = lr_messtab->message_v2 .
      lr_message_tab->msgv3 = lr_messtab->message_v3 .
      lr_message_tab->msgv4 = lr_messtab->message_v4 .
    ENDLOOP.

    CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
      TABLES
        i_message_tab = lt_message_tab[].

  ENDMETHOD.                    "show_message_tool
ENDCLASS .                    "application IMPLEMENTATION
