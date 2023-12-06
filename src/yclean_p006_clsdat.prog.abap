*&---------------------------------------------------------------------*
*& Include          YCLEAN_P006_CLSDAT
*&---------------------------------------------------------------------*
TABLES: acdoca.

PARAMETERS:
  p_rldnr TYPE acdoca-rldnr OBLIGATORY DEFAULT '0L',
  p_bukrs TYPE bukrs OBLIGATORY DEFAULT '1001',
  p_kokrs TYPE kokrs OBLIGATORY DEFAULT '1001',
  p_gjahr TYPE gjahr OBLIGATORY DEFAULT '2021'.

SELECT-OPTIONS:
  s_poper FOR acdoca-poper NO INTERVALS.

SELECTION-SCREEN SKIP 1.
PARAMETERS:
  p_cobk TYPE xfeld DEFAULT 'X' USER-COMMAND cbx,
  p_coep TYPE xfeld DEFAULT 'X' USER-COMMAND cbx,
  p_cosp TYPE xfeld DEFAULT 'X' USER-COMMAND cbx,
  p_coka TYPE xfeld DEFAULT 'X' USER-COMMAND cbx,
  p_acdo TYPE xfeld DEFAULT 'X' USER-COMMAND cbx,
  p_ceow TYPE xfeld DEFAULT ' ' USER-COMMAND cbx.

DATA: mv_expert TYPE xfeld.

CLASS lcl_sqlhelp DEFINITION.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF mty_sumdat,
        tabnam TYPE tabname,
        rldnr  TYPE acdoca-rldnr,
        rbukrs TYPE acdoca-rbukrs,
        gjahr  TYPE acdoca-gjahr,
        poper  TYPE acdoca-poper,
        count  TYPE int8,
      END OF mty_sumdat,
      mtt_sumdat TYPE STANDARD TABLE OF mty_sumdat WITH DEFAULT KEY.

    TYPES:
      BEGIN OF mty_acdoca,
        rldnr  TYPE acdoca-rldnr,
        rbukrs TYPE acdoca-rbukrs,
        gjahr  TYPE acdoca-gjahr,
        poper  TYPE acdoca-poper,
        objnr  TYPE acdoca-objnr,
      END OF mty_acdoca.

    TYPES:
      BEGIN OF mty_summary,
        rldnr  TYPE acdoca-rldnr,
        rbukrs TYPE acdoca-rbukrs,
        gjahr  TYPE acdoca-gjahr,
        poper  TYPE acdoca-poper,
        count  TYPE int8,
      END OF mty_summary.

    TYPES:
      BEGIN OF mty_cobelnr,
        rldnr  TYPE acdoca-rldnr,
        rbukrs TYPE acdoca-rbukrs,
        gjahr  TYPE acdoca-gjahr,
        poper  TYPE acdoca-poper,
        belnr  TYPE acdoca-co_belnr,
      END OF mty_cobelnr.

    TYPES:
      BEGIN OF mty_writing,
        rldnr  TYPE acdoca-rldnr,
        rbukrs TYPE acdoca-rbukrs,
        gjahr  TYPE acdoca-gjahr,
        poper  TYPE acdoca-poper,
        count  TYPE char20,
      END OF mty_writing,
      mtt_writing TYPE STANDARD TABLE OF mty_writing.

    TYPES:
      BEGIN OF mty_poper,
        poper TYPE poper,
      END OF mty_poper,
      mtt_popers TYPE SORTED TABLE OF mty_poper WITH UNIQUE KEY primary_key COMPONENTS poper.

    CLASS-DATA:
      mo_sqlhelper TYPE REF TO lcl_sqlhelp,
      mt_poperdat  TYPE mtt_popers.

    CLASS-METHODS:
      instance_app
        RETURNING
          VALUE(ro_sqlhelper) TYPE REF TO lcl_sqlhelp.

    DATA:
      ms_sumdat  TYPE mty_sumdat,
      mt_acdoca  TYPE STANDARD TABLE OF mty_acdoca,
      mt_summary TYPE STANDARD TABLE OF mty_summary,
      mt_cobelnr TYPE STANDARD TABLE OF mty_cobelnr.

    METHODS:
      at_selection_screen,
      at_selection_screen_output,
      run_sql_query
        IMPORTING
          im_cobk          TYPE xfeld
          im_coep          TYPE xfeld
          im_cosp          TYPE xfeld
          im_coka          TYPE xfeld
          im_acdo          TYPE xfeld
        RETURNING
          VALUE(rt_sumdat) TYPE mtt_sumdat,
      write_sql_resulst
        IMPORTING
          VALUE(im_sumdat) TYPE mtt_sumdat.

  PRIVATE SECTION.

    DATA: mt_writing TYPE mtt_writing.

    METHODS:
      _cobk_sql_count
        RETURNING
          VALUE(rt_sumdat) TYPE mtt_sumdat
        EXCEPTIONS
          record_not_available,
      _coep_sql_count
        RETURNING
          VALUE(rt_sumdat) TYPE mtt_sumdat
        EXCEPTIONS
          record_not_available,
      _cosp_sql_count
        RETURNING
          VALUE(rt_sumdat) TYPE mtt_sumdat
        EXCEPTIONS
          record_not_available,
      _coka_sql_count
        RETURNING
          VALUE(rt_sumdat) TYPE mtt_sumdat
        EXCEPTIONS
          record_not_available,
      _acdo_sql_count
        RETURNING
          VALUE(rt_sumdat) TYPE mtt_sumdat
        EXCEPTIONS
          record_not_available,
      _coew_sql_count
        RETURNING
          VALUE(rt_sumdat) TYPE mtt_sumdat
        EXCEPTIONS
          record_not_available.

ENDCLASS.

CLASS lcl_sqlhelp IMPLEMENTATION.

  METHOD instance_app.

    IF mo_sqlhelper IS NOT BOUND.
      CREATE OBJECT mo_sqlhelper.
    ENDIF.
    ro_sqlhelper ?= mo_sqlhelper.

    IF lcl_sqlhelp=>mt_poperdat[] IS INITIAL.
      DO 16 TIMES.
        INSERT VALUE #( poper = sy-index ) INTO TABLE lcl_sqlhelp=>mt_poperdat.
      ENDDO.
    ENDIF.

  ENDMETHOD.
  METHOD at_selection_screen.

    CASE sy-ucomm.
      WHEN 'ONLI'.
        IF p_cobk IS INITIAL AND p_coep IS INITIAL AND p_cosp IS INITIAL AND p_coka IS INITIAL AND p_acdo IS INITIAL.
          MESSAGE 'En az bir tablo seçimi yapınız!' TYPE 'S'.
        ENDIF.
      WHEN 'EXPERT'.
        mv_expert = abap_true.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.
  METHOD at_selection_screen_output.

    LOOP AT SCREEN.
      IF screen-name CS 'P_BUKRS' OR screen-name CS 'P_KOKRS'.
        screen-input = 0.
      ENDIF.
      IF screen-name EQ 'P_CEOW'.
        screen-active = COND #( WHEN mv_expert EQ abap_true THEN 1 ELSE 0 ).
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.

    IF p_cobk IS INITIAL AND p_coep IS INITIAL AND p_cosp IS INITIAL AND p_coka IS INITIAL AND p_acdo IS INITIAL.
      p_cobk = p_coep = p_cosp = p_coka = p_acdo = abap_true.
      MESSAGE 'En az bir tablo seçimi yapınız!' TYPE 'S'.
    ENDIF.

  ENDMETHOD.
  METHOD _cobk_sql_count.

    CHECK NOT p_cobk IS INITIAL.

    OPEN CURSOR WITH HOLD @DATA(cursor) FOR
      SELECT DISTINCT rldnr, rbukrs, gjahr, poper, co_belnr AS belnr
        FROM acdoca
        WHERE rldnr EQ @p_rldnr
          AND rbukrs EQ @p_bukrs
          AND gjahr EQ @p_gjahr
          AND poper IN @s_poper
          AND co_belnr NE @space
          AND objnr LIKE 'EO%'
          GROUP BY rldnr, rbukrs, gjahr, poper, co_belnr.

    FREE: mt_cobelnr, rt_sumdat.
    WHILE sy-subrc = 0.
      FETCH NEXT CURSOR @cursor INTO TABLE @mt_cobelnr PACKAGE SIZE 500000000.

      SELECT acdoca~rldnr, acdoca~rbukrs, acdoca~gjahr, acdoca~poper, COUNT( * ) AS count
        FROM cobk
          INNER JOIN @mt_cobelnr AS acdoca
            ON acdoca~belnr EQ cobk~belnr
               WHERE cobk~kokrs = @p_kokrs
               GROUP BY acdoca~rldnr, acdoca~rbukrs, acdoca~gjahr, acdoca~poper
               INTO TABLE @DATA(t_count).

      LOOP AT t_count ASSIGNING FIELD-SYMBOL(<count_dat>).
        CLEAR: ms_sumdat.
        ms_sumdat = CORRESPONDING #( <count_dat> ).
        ms_sumdat-tabnam = 'COBK'.
        COLLECT ms_sumdat INTO rt_sumdat.
      ENDLOOP.
      FREE: mt_cobelnr.
    ENDWHILE.
    IF rt_sumdat[] IS INITIAL.
      LOOP AT mt_poperdat REFERENCE INTO DATA(mr_poper) WHERE poper IN s_poper.
        rt_sumdat = VALUE #( ( tabnam = 'COBK'
                               rldnr = p_rldnr
                               rbukrs = p_bukrs
                               gjahr = p_gjahr
                               poper = mr_poper->poper ) ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
  METHOD _coep_sql_count.

    CHECK NOT p_coep IS INITIAL.

    OPEN CURSOR WITH HOLD @DATA(cursor) FOR
      SELECT DISTINCT rldnr, rbukrs, gjahr, poper, co_belnr AS belnr
        FROM acdoca
        WHERE rldnr EQ @p_rldnr
          AND rbukrs EQ @p_bukrs
          AND gjahr EQ @p_gjahr
          AND poper IN @s_poper
          AND co_belnr NE @space
          AND objnr LIKE 'EO%'
          GROUP BY rldnr, rbukrs, gjahr, poper, co_belnr.

    FREE: mt_cobelnr, rt_sumdat.
    WHILE sy-subrc = 0.
      FETCH NEXT CURSOR @cursor INTO TABLE @mt_cobelnr PACKAGE SIZE 500000000.

      SELECT acdoca~rldnr, acdoca~rbukrs, acdoca~gjahr, acdoca~poper, COUNT( * ) AS count
        FROM v_coep_ori_view AS coep
          INNER JOIN @mt_cobelnr AS acdoca
            ON acdoca~belnr EQ coep~belnr
            WHERE coep~kokrs = @p_kokrs
            GROUP BY acdoca~rldnr, acdoca~rbukrs, acdoca~gjahr, acdoca~poper
            INTO TABLE @DATA(t_count).

      LOOP AT t_count ASSIGNING FIELD-SYMBOL(<count_dat>).
        CLEAR: ms_sumdat.
        ms_sumdat = CORRESPONDING #( <count_dat> ).
        ms_sumdat-tabnam = 'COEP'.
        COLLECT ms_sumdat INTO rt_sumdat.
      ENDLOOP.

      FREE: mt_cobelnr.
    ENDWHILE.

    IF rt_sumdat[] IS INITIAL.
      LOOP AT mt_poperdat REFERENCE INTO DATA(mr_poper) WHERE poper IN s_poper.
        rt_sumdat = VALUE #( ( tabnam = 'COEP'
                               rldnr = p_rldnr
                               rbukrs = p_bukrs
                               gjahr = p_gjahr
                               poper = mr_poper->poper ) ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
  METHOD _cosp_sql_count.

    CHECK NOT p_cosp IS INITIAL.

    OPEN CURSOR WITH HOLD @DATA(cursor) FOR
      SELECT DISTINCT rldnr, rbukrs, gjahr, poper, objnr
        FROM acdoca
        WHERE rldnr EQ @p_rldnr
          AND rbukrs EQ @p_bukrs
          AND gjahr EQ @p_gjahr
          AND poper IN @s_poper
          AND co_belnr NE @space
          AND objnr LIKE 'EO%'
          GROUP BY rldnr, rbukrs, gjahr, poper, objnr.

    FREE: mt_acdoca, rt_sumdat.
    WHILE sy-subrc = 0.
      FETCH NEXT CURSOR @cursor INTO TABLE @mt_acdoca PACKAGE SIZE 500000000.

      SELECT acdoca~rldnr, acdoca~rbukrs, acdoca~gjahr, acdoca~poper, COUNT( * ) AS count
        FROM cosp_bak AS cosp
          INNER JOIN @mt_acdoca AS acdoca
            ON acdoca~gjahr EQ cosp~gjahr AND
               acdoca~objnr EQ cosp~objnr
               GROUP BY acdoca~rldnr, acdoca~rbukrs, acdoca~gjahr, acdoca~poper
                INTO TABLE @DATA(t_count).

      LOOP AT t_count ASSIGNING FIELD-SYMBOL(<count_dat>).
        CLEAR: ms_sumdat.
        ms_sumdat = CORRESPONDING #( <count_dat> ).
        ms_sumdat-tabnam = 'COSP'.
        COLLECT ms_sumdat INTO rt_sumdat.
      ENDLOOP.

      FREE: mt_acdoca.
    ENDWHILE.

    IF rt_sumdat[] IS INITIAL.
      LOOP AT mt_poperdat REFERENCE INTO DATA(mr_poper) WHERE poper IN s_poper.
        rt_sumdat = VALUE #( ( tabnam = 'COSP'
                               rldnr = p_rldnr
                               rbukrs = p_bukrs
                               gjahr = p_gjahr
                               poper = mr_poper->poper ) ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
  METHOD _coka_sql_count.

    CHECK NOT p_coka IS INITIAL.

    OPEN CURSOR WITH HOLD @DATA(cursor) FOR
      SELECT DISTINCT rldnr, rbukrs, gjahr, poper, objnr
        FROM acdoca
        WHERE rldnr EQ @p_rldnr
          AND rbukrs EQ @p_bukrs
          AND gjahr EQ @p_gjahr
          AND poper IN @s_poper
          AND co_belnr NE @space
          AND objnr LIKE 'EO%'
          GROUP BY rldnr, rbukrs, gjahr, poper, objnr.

    FREE: mt_acdoca, rt_sumdat.
    WHILE sy-subrc = 0.
      FETCH NEXT CURSOR @cursor INTO TABLE @mt_acdoca PACKAGE SIZE 500000000.

      SELECT acdoca~rldnr, acdoca~rbukrs, acdoca~gjahr, acdoca~poper, COUNT( * ) AS count
        FROM coka
          INNER JOIN @mt_acdoca AS acdoca
            ON acdoca~gjahr EQ coka~gjahr AND
               acdoca~objnr EQ coka~objnr
               GROUP BY acdoca~rldnr, acdoca~rbukrs, acdoca~gjahr, acdoca~poper
                INTO TABLE @DATA(t_count).

      LOOP AT t_count ASSIGNING FIELD-SYMBOL(<count_dat>).
        CLEAR: ms_sumdat.
        ms_sumdat = CORRESPONDING #( <count_dat> ).
        ms_sumdat-tabnam = 'COKA'.
        COLLECT ms_sumdat INTO rt_sumdat.
      ENDLOOP.

      FREE: mt_acdoca.
    ENDWHILE.

    IF rt_sumdat[] IS INITIAL.
      LOOP AT mt_poperdat REFERENCE INTO DATA(mr_poper) WHERE poper IN s_poper.
        rt_sumdat = VALUE #( ( tabnam = 'COKA'
                               rldnr = p_rldnr
                               rbukrs = p_bukrs
                               gjahr = p_gjahr
                               poper = mr_poper->poper ) ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
  METHOD _acdo_sql_count.

    CHECK NOT p_acdo IS INITIAL.

    OPEN CURSOR WITH HOLD @DATA(cursor) FOR
      SELECT rldnr, rbukrs, gjahr, poper, COUNT( * ) AS count
        FROM acdoca
        WHERE rldnr EQ @p_rldnr
          AND rbukrs EQ @p_bukrs
          AND gjahr EQ @p_gjahr
          AND poper IN @s_poper
          AND co_belnr NE @space
          AND objnr LIKE 'EO%'
          GROUP BY rldnr, rbukrs, gjahr, poper.

    FREE: mt_acdoca, rt_sumdat.
    WHILE sy-subrc = 0.
      FETCH NEXT CURSOR @cursor INTO TABLE @mt_summary PACKAGE SIZE 500000000.

      LOOP AT mt_summary ASSIGNING FIELD-SYMBOL(<summary>).
        CLEAR: ms_sumdat.
        ms_sumdat = CORRESPONDING #( <summary> ).
        ms_sumdat-tabnam = 'ACDOCA'.
        COLLECT ms_sumdat INTO rt_sumdat.
      ENDLOOP.

      FREE: mt_summary.
    ENDWHILE.

    IF rt_sumdat[] IS INITIAL.
      LOOP AT mt_poperdat REFERENCE INTO DATA(mr_poper) WHERE poper IN s_poper.
        rt_sumdat = VALUE #( ( tabnam = 'ACDOCA'
                               rldnr = p_rldnr
                               rbukrs = p_bukrs
                               gjahr = p_gjahr
                               poper = mr_poper->poper ) ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
  METHOD _coew_sql_count.

    CHECK NOT p_ceow IS INITIAL.

    OPEN CURSOR WITH HOLD @DATA(cursor) FOR
      SELECT DISTINCT rldnr, rbukrs, gjahr, poper, co_belnr AS belnr
        FROM acdoca
        WHERE rldnr EQ @p_rldnr
          AND rbukrs EQ @p_bukrs
          AND gjahr EQ @p_gjahr
          AND poper IN @s_poper
          AND co_belnr NE @space
          AND objnr LIKE 'EO%'
          GROUP BY rldnr, rbukrs, gjahr, poper, co_belnr.

    FREE: mt_cobelnr, rt_sumdat.
    WHILE sy-subrc = 0.
      FETCH NEXT CURSOR @cursor INTO TABLE @mt_cobelnr PACKAGE SIZE 500000000.

      SELECT acdoca~rldnr, acdoca~rbukrs, acdoca~gjahr, acdoca~poper, COUNT( * ) AS count
        FROM v_coep_view AS coep
          INNER JOIN @mt_cobelnr AS acdoca
            ON acdoca~belnr EQ coep~belnr
            WHERE coep~kokrs = @p_kokrs
            GROUP BY acdoca~rldnr, acdoca~rbukrs, acdoca~gjahr, acdoca~poper
            INTO TABLE @DATA(t_count).

      LOOP AT t_count ASSIGNING FIELD-SYMBOL(<count_dat>).
        CLEAR: ms_sumdat.
        ms_sumdat = CORRESPONDING #( <count_dat> ).
        ms_sumdat-tabnam = 'V_COEP'.
        COLLECT ms_sumdat INTO rt_sumdat.
      ENDLOOP.

      FREE: mt_cobelnr.
    ENDWHILE.
    IF rt_sumdat[] IS INITIAL.
      LOOP AT mt_poperdat REFERENCE INTO DATA(mr_poper) WHERE poper IN s_poper.
        rt_sumdat = VALUE #( ( tabnam = 'V_COEP'
                               rldnr = p_rldnr
                               rbukrs = p_bukrs
                               gjahr = p_gjahr
                               poper = mr_poper->poper ) ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
  METHOD run_sql_query.

    APPEND LINES OF _cobk_sql_count( ) TO rt_sumdat.
    APPEND LINES OF _coep_sql_count( ) TO rt_sumdat.
    APPEND LINES OF _cosp_sql_count( ) TO rt_sumdat.
    APPEND LINES OF _coka_sql_count( ) TO rt_sumdat.
    APPEND LINES OF _acdo_sql_count( ) TO rt_sumdat.
    APPEND LINES OF _coew_sql_count( ) TO rt_sumdat.

  ENDMETHOD.
  METHOD write_sql_resulst.

    cl_demo_output=>begin_section( 'CO-PA CLEAN' ).
    LOOP AT im_sumdat INTO DATA(_sumdat) GROUP BY ( tabnam = _sumdat-tabnam )
                      ASSIGNING FIELD-SYMBOL(<router_tab>).

      cl_demo_output=>next_section( |TABLE NAME: { <router_tab>-tabnam }| ).
      FREE: mt_writing.
      LOOP AT GROUP <router_tab> ASSIGNING FIELD-SYMBOL(<members>).
        APPEND INITIAL LINE TO mt_writing REFERENCE INTO DATA(_writing).
        _writing->* = CORRESPONDING #( <members> EXCEPT count ).
        _writing->count = |{ <members>-count NUMBER = USER }|.
      ENDLOOP.
      cl_demo_output=>write_data(
        EXPORTING
          value = mt_writing
          name  = 'Result;' ).
    ENDLOOP.
    cl_demo_output=>display( ).


  ENDMETHOD.

ENDCLASS.
