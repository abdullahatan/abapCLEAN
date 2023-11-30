*&---------------------------------------------------------------------*
*& Include          YCLEAR_P001_CLSDAT
*&---------------------------------------------------------------------*
TYPE-POOLS: vrm.
TABLES: sscrfields, acdoca.

SELECTION-SCREEN BEGIN OF BLOCK b1.
  SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN COMMENT /1(40) blok_1.
  PARAMETERS:
    p_rb1 RADIOBUTTON GROUP gr1 USER-COMMAND cmd1 MODIF ID rd1 DEFAULT 'X',
    p_rb2 RADIOBUTTON GROUP gr1 MODIF ID rd2,
    p_rb3 RADIOBUTTON GROUP gr1 MODIF ID rd3,
    p_rb4 RADIOBUTTON GROUP gr1 MODIF ID rd4.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2.
  SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN COMMENT /1(40) blok_2.
  PARAMETERS:
    p_rldnr TYPE acdoca-rldnr OBLIGATORY DEFAULT '0L' MODIF ID m01,
    p_bukrs TYPE acdoca-rbukrs OBLIGATORY DEFAULT '1001' MODIF ID m02,
    p_kokrs TYPE cobk-kokrs OBLIGATORY DEFAULT '1001' MODIF ID m03.

  SELECT-OPTIONS:
    s_belnr FOR acdoca-co_belnr NO INTERVALS MODIF ID m06 MATCHCODE OBJECT cfindoc_all,
    s_objnr FOR acdoca-objnr NO INTERVALS MODIF ID m07.

  SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN COMMENT /1(40) blok_3.
  PARAMETERS:
    p_gjahr TYPE acdoca-gjahr DEFAULT sy-datum MODIF ID m04,
    p_poper TYPE acdoca-poper DEFAULT sy-datum+4(2) MODIF ID m05.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3.
  SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN COMMENT /1(40) blok_4.
  PARAMETERS:
    p_prll TYPE int1 AS LISTBOX VISIBLE LENGTH 6 DEFAULT 1 OBLIGATORY MODIF ID m08,
    p_rz12 TYPE rzlli_apcl DEFAULT '' MODIF ID m09.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4.
  SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN COMMENT /1(40) blok_5.
  PARAMETERS: p_cb1 AS CHECKBOX DEFAULT ''.
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK b5.
  SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN COMMENT /1(40) blok_6 MODIF ID exp.
  PARAMETERS: p_cb2 AS CHECKBOX DEFAULT '' MODIF ID exp.
SELECTION-SCREEN END OF BLOCK b5.

CLASS application DEFINITION DEFERRED.
DATA: app_session TYPE REF TO application,
      mv_memid    TYPE char2,
      mv_expmode  TYPE xfeld.

*----------------------------------------------------------------------*
*       CLASS application DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS application DEFINITION .

  PUBLIC SECTION.

    TYPES:
      mtt_belnr_rng TYPE RANGE OF co_belnr,
      mtt_objnr_rng TYPE RANGE OF j_objnr.

    CLASS-DATA:
      app TYPE REF TO application.

    DATA:
      mv_tabname  TYPE char40,
      mv_taskname TYPE char40,
      mv_logid    TYPE balnrext.

    CONSTANTS:
      BEGIN OF mc_cons,
        rz12 TYPE fieldname VALUE 'P_RZ12',
      END OF mc_cons.

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
      select_option_restrict,
      listbox_build
        IMPORTING
          im_kind TYPE char10
        EXCEPTIONS
          handle_error,
      f4_finanscal_perio
        CHANGING
          cv_spmon TYPE s031-spmon,
      f4_server_group
        CHANGING
          cv_rz12 TYPE rzlli_apcl,
      rundat_control
        EXCEPTIONS
          acdoca_not_deleted,
      retrieve_dat
        IMPORTING
          !im_rldnr TYPE rldnr
          !im_bukrs TYPE bukrs
          !im_kokrs TYPE kokrs
          !im_gjahr TYPE gjahr
          !im_poper TYPE poper
          !im_prll  TYPE int1
          !im_rz12  TYPE rzlli_apcl OPTIONAL
          !im_belnr TYPE mtt_belnr_rng OPTIONAL
          !im_objnr TYPE mtt_objnr_rng OPTIONAL
          !im_xshow TYPE xfeld OPTIONAL
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
    blok_1 = TEXT-s01.
    blok_2 = TEXT-s02.
    blok_3 = TEXT-s03.
    blok_4 = TEXT-s04.
    blok_5 = TEXT-s05.
    blok_6 = TEXT-s06.
    SET PARAMETER ID 'GR1' FIELD 'R1'.
    app->select_option_restrict( ).
    app->listbox_build(
      EXPORTING
        im_kind      = 'DAYS'
      EXCEPTIONS
        handle_error = 1
        OTHERS       = 2 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    p_cb2 = abap_false.
    mv_expmode = space.
  ENDMETHOD.                    "initialization
  METHOD at_selection_screen.
    CASE sy-ucomm.
      WHEN 'ONLI'.
        CLEAR: app_session->mv_tabname.
        CASE abap_true.
          WHEN p_rb1.
            FREE: app_session->mv_tabname.
            app_session->mv_tabname = TEXT-t01.
          WHEN p_rb2.
            FREE: app_session->mv_tabname.
            app_session->mv_tabname = TEXT-t02.
          WHEN p_rb3.
            FREE: app_session->mv_tabname.
            app_session->mv_tabname = TEXT-t03.
          WHEN p_rb4.
            FREE: app_session->mv_tabname.
            app_session->mv_tabname = TEXT-t04.
        ENDCASE.

        IF p_gjahr IS INITIAL OR p_poper IS INITIAL.
          MESSAGE e025(yclean).
        ENDIF.

        app->rundat_control(
          EXCEPTIONS
            acdoca_not_deleted = 1
            OTHERS             = 2 ).
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

      WHEN 'CMD1'.
        IF p_gjahr IS INITIAL OR p_poper IS INITIAL.
          MESSAGE s025(yclean) DISPLAY LIKE 'E'. RETURN.
        ELSE.
          FREE: mv_memid.
          mv_memid = COND #( WHEN p_rb1 EQ abap_true THEN 'R1'
                             WHEN p_rb2 EQ abap_true THEN 'R2'
                             WHEN p_rb3 EQ abap_true THEN 'R3'
                             WHEN p_rb4 EQ abap_true THEN 'R4' ELSE 'R1').
          SET PARAMETER ID 'GR1' FIELD mv_memid.
        ENDIF.
        app->listbox_build(
          EXPORTING
            im_kind      = COND #( WHEN p_rb4 EQ abap_true THEN 'MONTHS' ELSE 'DAYS' )
          EXCEPTIONS
            handle_error = 1
          OTHERS       = 2  ).
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        CASE abap_true.
          WHEN p_rb1.
            FREE: s_belnr, s_objnr.
          WHEN p_rb2.
            FREE: s_belnr, s_objnr.
          WHEN p_rb3.
            FREE: s_belnr, s_objnr.
          WHEN p_rb4.
            FREE: s_belnr, s_objnr.
            app->rundat_control(
              EXCEPTIONS
                acdoca_not_deleted = 1
                OTHERS             = 2 ).
            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.
        ENDCASE.
      WHEN 'EXPERT'.
        mv_expmode = abap_true.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.                    "at_selection_screen
  METHOD at_selection_screen_output.

    FREE: mv_memid.
    GET PARAMETER ID 'GR1' FIELD mv_memid.
    IF mv_memid = 'R1'.
      p_rb1 = abap_true. p_rb2 = abap_false. p_rb3 = abap_false. p_rb4 = abap_false.
    ELSEIF mv_memid = 'R2'.
      p_rb1 = abap_false. p_rb2 = abap_true. p_rb3 = abap_false. p_rb4 = abap_false.
    ELSEIF mv_memid = 'R3'.
      p_rb1 = abap_false. p_rb2 = abap_false. p_rb3 = abap_true. p_rb4 = abap_false.
    ELSEIF mv_memid = 'R4'.
      p_rb1 = abap_false. p_rb2 = abap_false. p_rb3 = abap_false. p_rb4 = abap_true.
    ELSE.
      p_rb1 = abap_true. p_rb2 = abap_false. p_rb3 = abap_false. p_rb4 = abap_false.
    ENDIF.

    LOOP AT SCREEN.
      IF screen-group1 EQ 'EXP'.
        screen-active = COND #( WHEN mv_expmode EQ abap_true THEN 1 ELSE 0 ).
      ENDIF.
      IF screen-name CS 'BLOK'.
        screen-intensified = 1.
      ENDIF.
      IF screen-group1 EQ 'M01' OR screen-group1 EQ 'M02' OR screen-group1 EQ 'M03'.
        screen-input = 0.
      ENDIF.
      IF screen-group1 EQ 'M03'.
        screen-active = COND #( WHEN p_rb3 EQ abap_true OR p_rb4 EQ abap_true THEN 0 ELSE 1 ).
      ENDIF.
      IF screen-group1 EQ 'M04'.
        screen-required = 2.
      ENDIF.
      IF screen-group1 EQ 'M05'.
        screen-required = 2.
      ENDIF.
      IF screen-group1 EQ 'M06'.
        screen-active = COND #( WHEN p_rb3 EQ abap_true THEN 0 ELSE 1 ).
      ENDIF.
      IF screen-group1 EQ 'M07'.
        screen-active = COND #( WHEN p_rb3 EQ abap_true THEN 1 ELSE 0 ).
      ENDIF.
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

*->Custom çalıştırılabilmesi için geliştirme yapıldı, sakın çalıştırma!!!
    IF sy-uname <> 'AATAN' AND sy-uname <> 'AHMETS'  .
      MESSAGE s017(yclean) DISPLAY LIKE 'E'. RETURN.
    ENDIF.

    app->retrieve_dat(
    EXPORTING
      im_rldnr = p_rldnr
      im_bukrs = p_bukrs
      im_kokrs = p_kokrs
      im_gjahr = p_gjahr
      im_poper = p_poper
      im_belnr = s_belnr[]
      im_objnr = s_objnr[]
      im_prll  = p_prll
      im_rz12  = p_rz12
      im_xshow = p_cb1
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
  METHOD select_option_restrict.

    DATA(lv_opt_list) = 'OPT_LIST'.
    DATA(it_options) = VALUE sscr_opt_list_tab( ( name = lv_opt_list options-eq = abap_true ) ).

    DATA(it_assignment) = VALUE sscr_ass_tab( ( kind = 'S' name = 'S_BELNR' sg_main = 'I' op_main = lv_opt_list ) ( kind = 'S' name = 'S_OBJNR' sg_main = 'I' op_main = lv_opt_list ) ).
    DATA(it_restrictions) = VALUE sscr_restrict( opt_list_tab = it_options ass_tab = it_assignment ).

    CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
      EXPORTING
        restriction            = it_restrictions
      EXCEPTIONS
        too_late               = 1
        repeated               = 2
        selopt_without_options = 3
        selopt_without_signs   = 4
        invalid_sign           = 5
        empty_option_list      = 6
        invalid_kind           = 7
        repeated_kind_a        = 8
        OTHERS                 = 9.

  ENDMETHOD.                    "select_option_restrict
  METHOD listbox_build.

    DATA: _values TYPE vrm_values,
          _id     TYPE vrm_id.

    FREE: _values, _id.
    _id = 'P_PRLL'.

    CASE im_kind.
      WHEN 'DAYS'.
        p_prll = 1.
        _values = VALUE #( BASE _values ( key = '01' ) ).
        CALL FUNCTION 'VRM_SET_VALUES'
          EXPORTING
            id     = _id
            values = _values.

      WHEN 'MONTHS'.
        p_prll = 8.
        _values = VALUE #( BASE _values ( key = '01' ) ( key = '02' ) ( key = '04' ) ( key = '06' ) ( key = '08' ) ( key = '10' ) ( key = '15' ) ).
        CALL FUNCTION 'VRM_SET_VALUES'
          EXPORTING
            id     = _id
            values = _values.

      WHEN OTHERS.
        MESSAGE e024(yclean).
    ENDCASE.

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

    CHECK p_cb2 IS INITIAL.

    IF NOT p_rb4 IS INITIAL.
      SELECT SINGLE coep~belnr
        FROM v_coep_ori_view AS coep
        INNER JOIN acdoca
          ON acdoca~co_belnr EQ coep~belnr
          WHERE coep~kokrs EQ @p_kokrs
            AND acdoca~rldnr EQ @p_rldnr
            AND acdoca~rbukrs EQ @p_bukrs
            AND acdoca~gjahr EQ @p_gjahr
            AND acdoca~poper EQ @p_poper
            AND acdoca~co_belnr IN @s_belnr
            AND acdoca~objnr LIKE 'EO%'
            INTO @DATA(_coep).
      IF sy-subrc IS INITIAL.
        MESSAGE s020(yclean) RAISING acdoca_not_deleted.
      ENDIF.

      SELECT SINGLE cobk~belnr
        FROM cobk
        INNER JOIN acdoca
          ON acdoca~co_belnr EQ cobk~belnr
          WHERE cobk~kokrs EQ @p_kokrs
            AND acdoca~rldnr EQ @p_rldnr
            AND acdoca~rbukrs EQ @p_bukrs
            AND acdoca~gjahr EQ @p_gjahr
            AND acdoca~poper EQ @p_poper
            AND acdoca~co_belnr IN @s_belnr
            AND acdoca~objnr LIKE 'EO%'
            INTO @DATA(_cobk).
      IF sy-subrc IS INITIAL.
        MESSAGE s020(yclean) RAISING acdoca_not_deleted.
      ENDIF.

      SELECT SINGLE cosp_bak~objnr
        FROM cosp_bak
        INNER JOIN acdoca
          ON acdoca~objnr EQ cosp_bak~objnr AND
             acdoca~gjahr EQ cosp_bak~gjahr
          WHERE acdoca~rldnr EQ @p_rldnr
            AND acdoca~rbukrs EQ @p_bukrs
            AND acdoca~gjahr EQ @p_gjahr
            AND acdoca~poper EQ @p_poper
            AND acdoca~co_belnr NE @space
            AND acdoca~co_belnr NE @space
            AND acdoca~objnr LIKE 'EO%'
            AND cosp_bak~objnr IN @s_objnr
            INTO @DATA(_cosp_bak).
      IF sy-subrc IS INITIAL.
        MESSAGE s020(yclean) RAISING acdoca_not_deleted.
      ENDIF.

      SELECT SINGLE coka~objnr
        FROM coka
        INNER JOIN acdoca
          ON acdoca~objnr EQ coka~objnr AND
             acdoca~gjahr EQ coka~gjahr
          WHERE acdoca~rldnr EQ @p_rldnr
            AND acdoca~rbukrs EQ @p_bukrs
            AND acdoca~gjahr EQ @p_gjahr
            AND acdoca~poper EQ @p_poper
            AND acdoca~co_belnr NE @space
            AND acdoca~co_belnr NE @space
            AND acdoca~objnr LIKE 'EO%'
            AND coka~objnr IN @s_objnr
            INTO @DATA(_coka).
      IF sy-subrc IS INITIAL.
        MESSAGE s020(yclean) RAISING acdoca_not_deleted.
      ENDIF.
*// Count optimize edilemiyor!!! Canlıda kontrol edilecek...
*      SELECT acdoca~objnr, acdoca~gjahr
*        FROM acdoca
*        INTO @DATA(v_acdoca)
*        WHERE acdoca~rldnr EQ @p_rldnr
*          AND acdoca~rbukrs EQ @p_bukrs
*          AND acdoca~gjahr EQ @p_gjahr
*          AND acdoca~objnr LIKE 'EO%'
*          AND acdoca~co_belnr IN @s_belnr.
*        SELECT COUNT(*) FROM coka
*          WHERE objnr = v_acdoca-objnr
*            AND gjahr = v_acdoca-gjahr.
*        IF sy-dbcnt IS NOT INITIAL.
*          DATA(_error) = abap_true.
*          EXIT.
*        ENDIF.
*      ENDSELECT.
*      IF _error EQ abap_true.
*        MESSAGE s020(yclean) RAISING acdoca_not_deleted.
*      ENDIF.
    ENDIF.


  ENDMETHOD.                    "rundat_control
  METHOD retrieve_dat.

    DATA: _where  TYPE string,
          t_poper TYPE yclean_tt04,
          t_budat TYPE yclean_tt01,
          t_belnr TYPE yclean_tt08,
          t_objnr TYPE yclean_tt09,
          _result TYPE string.

    CHECK popup_confirm( im_titlebar = TEXT-h01 im_question = |{ mv_tabname } { TEXT-q01 }| ) EQ '1'.
    CASE abap_true.
*--------------------------------------------------------------------*
*-&Step-1: Delete From COEP ->
*--------------------------------------------------------------------*
      WHEN p_rb1.
        FREE: t_belnr.
        LOOP AT s_belnr REFERENCE INTO DATA(_belnr).
          APPEND VALUE #( belnr = _belnr->low ) TO t_belnr.
        ENDLOOP.

        DATA(_output) = cl_demo_output=>new( )->begin_section( |Günlük Bilgileri({ TEXT-t01 })| )->begin_section( |Nesne:| )->write_text( |YCLEAN| )->next_section( |Harici tanıtıcı:| ).

        NEW yclean_cl04( )->_monat_splitdat(
          EXPORTING
            im_split       = im_prll
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
          mv_taskname = |COEP_{ p_kokrs }_{ p_gjahr }_{ p_poper }/T{ _splitdat->line }|.
          mv_logid = |{ mv_taskname }-{ generate_guid( ) }|.

          FREE: t_poper.
          t_poper = VALUE #( ( poper = im_poper ) ).

          CALL FUNCTION 'YCLEAN_FM12' STARTING NEW TASK mv_taskname DESTINATION IN GROUP p_rz12
            EXPORTING
              iv_rldnr       = im_rldnr
              iv_bukrs       = im_bukrs
              iv_kokrs       = im_kokrs
              iv_gjahr       = im_gjahr
              iv_poper       = t_poper
              iv_belnr       = t_belnr
              iv_logid       = mv_logid
            EXCEPTIONS
              contains_error = 1
              OTHERS         = 2.
          _output->write_text( |{ mv_logid ALPHA = OUT }| ).
        ENDLOOP.
        IF im_xshow EQ abap_false.
          _output->display( ).
        ENDIF.
*--------------------------------------------------------------------*
*-&Step-2: Delete From COBK ->
*--------------------------------------------------------------------*
      WHEN p_rb2.
        FREE: t_belnr.
        LOOP AT s_belnr REFERENCE INTO _belnr.
          APPEND VALUE #( belnr = _belnr->low ) TO t_belnr.
        ENDLOOP.

        _output = cl_demo_output=>new( )->begin_section( |Günlük Bilgileri({ TEXT-t02 })| )->begin_section( |Nesne:| )->write_text( |YCLEAN| )->next_section( |Harici tanıtıcı:| ).

        NEW yclean_cl04( )->_monat_splitdat(
          EXPORTING
            im_split       = im_prll
          RECEIVING
            rt_splitdat    = t_splitdat
          EXCEPTIONS
            contains_error = 1
            OTHERS         = 2 ).
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
        ENDIF.

        LOOP AT t_splitdat REFERENCE INTO _splitdat.
          CLEAR: mv_logid, mv_taskname.
          mv_taskname = |COBK_{ p_kokrs }_{ p_gjahr }_{ p_poper }/T{ _splitdat->line }|.
          mv_logid = |{ mv_taskname }-{ generate_guid( ) }|.

          FREE: t_poper.
          t_poper = VALUE #( ( poper = im_poper ) ).

          CALL FUNCTION 'YCLEAN_FM13' STARTING NEW TASK mv_taskname DESTINATION IN GROUP p_rz12
            EXPORTING
              iv_rldnr       = im_rldnr
              iv_bukrs       = im_bukrs
              iv_kokrs       = im_kokrs
              iv_gjahr       = im_gjahr
              iv_poper       = t_poper
              iv_belnr       = t_belnr
              iv_logid       = mv_logid
            EXCEPTIONS
              contains_error = 1
              OTHERS         = 2.
          _output->write_text( |{ mv_logid ALPHA = OUT }| ).
        ENDLOOP.
        IF im_xshow EQ abap_false.
          _output->display( ).
        ENDIF.
*--------------------------------------------------------------------*
*-&Step-3: Clear From COSP_COKA ->
*--------------------------------------------------------------------*
      WHEN p_rb3.
        FREE: t_objnr.
        LOOP AT im_objnr REFERENCE INTO DATA(_objnr).
          APPEND VALUE #( objnr = _objnr->low ) TO t_objnr.
        ENDLOOP.

        _output = cl_demo_output=>new( )->begin_section( |Günlük Bilgileri({ TEXT-t03 })| )->begin_section( |Nesne:| )->write_text( |YCLEAN| )->next_section( |Harici tanıtıcı:| ).

        NEW yclean_cl04( )->_monat_splitdat(
          EXPORTING
            im_split       = im_prll
          RECEIVING
            rt_splitdat    = t_splitdat
          EXCEPTIONS
            contains_error = 1
            OTHERS         = 2 ).
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
        ENDIF.

        LOOP AT t_splitdat REFERENCE INTO _splitdat.
          CLEAR: mv_logid, mv_taskname.
          mv_taskname = |COSP_COKA_{ p_bukrs }_{ p_gjahr }_{ p_poper }/T{ _splitdat->line }|.
          mv_logid = |{ mv_taskname }-{ generate_guid( ) }|.

          FREE: t_poper.
          t_poper = VALUE #( ( poper = im_poper ) ).

          CALL FUNCTION 'YCLEAN_FM14' STARTING NEW TASK mv_taskname DESTINATION IN GROUP p_rz12
            EXPORTING
              iv_rldnr       = im_rldnr
              iv_bukrs       = im_bukrs
              iv_gjahr       = im_gjahr
              iv_poper       = t_poper
              iv_objnr       = t_objnr
              iv_logid       = mv_logid
            EXCEPTIONS
              contains_error = 1
              OTHERS         = 2.
          _output->write_text( |{ mv_logid ALPHA = OUT }| ).
        ENDLOOP.
        IF im_xshow EQ abap_false.
          _output->display( ).
        ENDIF.
*--------------------------------------------------------------------*
*-&Step-4: Update From ACDOCA ->
*--------------------------------------------------------------------*
      WHEN p_rb4.
        FREE: t_belnr.
        LOOP AT s_belnr REFERENCE INTO _belnr.
          APPEND VALUE #( belnr = _belnr->low ) TO t_belnr.
        ENDLOOP.

        _output = cl_demo_output=>new( )->begin_section( |Günlük Bilgileri({ TEXT-t04 })| )->begin_section( |Nesne:| )->write_text( |YCLEAN| )->next_section( |Harici tanıtıcı:| ).

        NEW yclean_cl04( )->_period_splitdat(
          EXPORTING
            im_spmon    = CONV #( |{ im_gjahr }{ im_poper+1(2) }| )
            im_split    = im_prll
          RECEIVING
            rt_budatdat = DATA(t_budats)
          EXCEPTIONS
            contains_error = 1
            OTHERS         = 2 ).
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
        ENDIF.

        LOOP AT t_budats REFERENCE INTO DATA(_budats).
          CLEAR: mv_logid, mv_taskname.
          mv_taskname = |ACDOCA_{ p_bukrs }_{ p_gjahr }_{ p_poper }/T{ _budats->line }|.
          mv_logid = |{ mv_taskname }-{ generate_guid( ) }|.

          FREE: t_budat.
          t_budat = VALUE #( FOR <lines> IN _budats->budat ( budat = <lines>-budat ) ).

          CALL FUNCTION 'YCLEAN_FM15' STARTING NEW TASK mv_taskname DESTINATION IN GROUP p_rz12
            EXPORTING
              iv_rldnr       = im_rldnr
              iv_bukrs       = im_bukrs
              iv_gjahr       = im_gjahr
              iv_poper       = im_poper
              iv_budat       = t_budat
              iv_belnr       = t_belnr
              iv_logid       = mv_logid
            EXCEPTIONS
              contains_error = 1
              OTHERS         = 2.
          _output->write_text( |{ mv_logid ALPHA = OUT }| ).
        ENDLOOP.
        IF im_xshow EQ abap_false.
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
