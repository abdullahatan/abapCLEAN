CLASS yclean_cl04 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_amdp_marker_hdb .

    TYPES:
      BEGIN OF ty_monatdat,
        poper TYPE poper,
      END OF ty_monatdat .
    TYPES:
      tt_monatdat TYPE STANDARD TABLE OF ty_monatdat WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_splitdat,
        line   TYPE sy-tabix,
        monats TYPE tt_monatdat,
      END OF ty_splitdat .
    TYPES:
      tt_splitdat TYPE STANDARD TABLE OF ty_splitdat WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_jahrper,
        jahrper TYPE jahrper,
      END OF ty_jahrper .
    TYPES:
      tt_jahrpers TYPE STANDARD TABLE OF ty_jahrper WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_jahrperdat,
        line     TYPE sy-tabix,
        jahrpers TYPE tt_jahrpers,
      END OF ty_jahrperdat .
    TYPES:
      tt_jahrperdat TYPE STANDARD TABLE OF ty_jahrperdat WITH DEFAULT KEY .

    CLASS-METHODS _rb1_rundat
      IMPORTING
        VALUE(iv_rldnr)  TYPE rldnr
        VALUE(iv_bukrs)  TYPE bukrs
        VALUE(iv_kokrs)  TYPE kokrs
        VALUE(iv_gjahr)  TYPE gjahr
        VALUE(iv_poper)  TYPE yclean_tt04
        VALUE(iv_belnr)  TYPE yclean_tt08
      EXPORTING
        VALUE(ev_subrc)  TYPE syst_subrc
        VALUE(ev_result) TYPE string
      RAISING
        cx_amdp_error .

    CLASS-METHODS _rb2_rundat
      IMPORTING
        VALUE(iv_rldnr)  TYPE rldnr
        VALUE(iv_bukrs)  TYPE bukrs
        VALUE(iv_kokrs)  TYPE kokrs
        VALUE(iv_gjahr)  TYPE gjahr
        VALUE(iv_poper)  TYPE yclean_tt04
        VALUE(iv_belnr)  TYPE yclean_tt08
      EXPORTING
        VALUE(ev_subrc)  TYPE syst_subrc
        VALUE(ev_result) TYPE string
      RAISING
        cx_amdp_error .

    CLASS-METHODS _rb3_rundat
      IMPORTING
        VALUE(iv_rldnr)  TYPE rldnr
        VALUE(iv_bukrs)  TYPE bukrs
        VALUE(iv_gjahr)  TYPE gjahr
        VALUE(iv_poper)  TYPE yclean_tt04
        VALUE(iv_objnr)  TYPE yclean_tt09
      EXPORTING
        VALUE(ev_subrc)  TYPE syst_subrc
        VALUE(ev_result) TYPE string
      RAISING
        cx_amdp_error .

    CLASS-METHODS _monat_splitdat
      IMPORTING
        !im_split          TYPE int1
      RETURNING
        VALUE(rt_splitdat) TYPE tt_splitdat
      EXCEPTIONS
        contains_error.
PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS YCLEAN_CL04 IMPLEMENTATION.

  METHOD _monat_splitdat.

    DATA: _mod     TYPE n LENGTH 2,
          _poper   TYPE poper,
          _tabix   TYPE sy-tabix,
          t_monats TYPE tt_monatdat.

    IF NOT im_split BETWEEN 1 AND 12.
      MESSAGE e018(yclean) RAISING contains_error.
    ENDIF.

    _mod = floor( 12 / im_split ).

    DO im_split TIMES.
      ADD 1 TO _tabix.
      IF _tabix EQ im_split.
        DO.
          ADD 1 TO _poper.
          APPEND VALUE #( poper = _poper ) TO t_monats.
          IF _poper EQ 12.
            EXIT.
          ENDIF.
        ENDDO.
      ELSE.
        DO _mod TIMES.
          ADD 1 TO _poper.
          APPEND VALUE #( poper = _poper ) TO t_monats.
        ENDDO.
      ENDIF.
      APPEND VALUE #( line = _tabix monats = t_monats[] ) TO rt_splitdat.
      FREE: t_monats.
    ENDDO.

  ENDMETHOD.
  METHOD _rb1_rundat BY DATABASE PROCEDURE
                     FOR HDB
                     LANGUAGE SQLSCRIPT
                     USING coep acdoca.

    DECLARE lv_count NUMBER(10);

    SELECT COUNT (*) INTO lv_count FROM :iv_belnr;
    IF :lv_count = 0 THEN
        DELETE FROM coep as db WHERE( db.mandt, db.kokrs, db.belnr )
                                  IN ( SELECT DISTINCT coep.mandt, coep.kokrs, coep.belnr
                                            FROM coep
                                            inner join acdoca
                                                on acdoca.co_belnr = coep.belnr
                                            WHERE coep.mandt  = Session_context('CLIENT')
                                              AND coep.kokrs  = :iv_kokrs
                                              AND acdoca.rldnr = :iv_rldnr
                                              AND acdoca.rbukrs = :iv_bukrs
                                              AND acdoca.gjahr = :iv_gjahr
                                              AND acdoca.poper IN( SELECT DISTINCT poper FROM :iv_poper )
                                              AND acdoca.objnr LIKE 'EO%' );
    ELSE
        DELETE FROM coep as db WHERE( db.mandt, db.kokrs, db.belnr )
                                  IN ( SELECT DISTINCT coep.mandt, coep.kokrs, coep.belnr
                                            FROM coep
                                            INNER JOIN acdoca
                                                ON acdoca.co_belnr = coep.belnr
                                            WHERE coep.mandt  = Session_context('CLIENT')
                                              AND coep.kokrs  = :iv_kokrs
                                              AND acdoca.rldnr = :iv_rldnr
                                              AND acdoca.rbukrs = :iv_bukrs
                                              AND acdoca.rldnr = :iv_rldnr
                                              AND acdoca.rbukrs = :iv_bukrs
                                              AND acdoca.gjahr = :iv_gjahr
                                              AND acdoca.poper IN( SELECT DISTINCT poper FROM :iv_poper )
                                              AND acdoca.objnr LIKE 'EO%'
                                              AND acdoca.co_belnr IN( SELECT DISTINCT belnr FROM :iv_belnr)  );
    END IF ;
    ev_subrc = 0;
    ev_result = 'Entity has been deleted successfully.';


  ENDMETHOD.
  METHOD _rb2_rundat BY DATABASE PROCEDURE
                     FOR HDB
                     LANGUAGE SQLSCRIPT
                     USING cobk acdoca.

    DECLARE lv_count NUMBER(10);

    SELECT COUNT (*) INTO lv_count FROM :iv_belnr;
    IF :lv_count = 0 THEN
        DELETE FROM cobk as db WHERE( db.mandt, db.kokrs, db.belnr )
                                  IN ( SELECT DISTINCT cobk.mandt, cobk.kokrs, cobk.belnr
                                            FROM cobk
                                            INNER JOIN acdoca
                                                ON acdoca.co_belnr = cobk.belnr
                                            WHERE cobk.mandt  = Session_context('CLIENT')
                                              AND cobk.kokrs  = :iv_kokrs
                                              AND acdoca.rldnr = :iv_rldnr
                                              AND acdoca.rbukrs = :iv_bukrs
                                              AND acdoca.gjahr = :iv_gjahr
                                              AND acdoca.poper IN( SELECT DISTINCT poper FROM :iv_poper )
                                              AND acdoca.objnr LIKE 'EO%' );
    ELSE
        DELETE FROM cobk as db WHERE( db.mandt, db.kokrs, db.belnr )
                                  IN ( SELECT DISTINCT cobk.mandt, cobk.kokrs, cobk.belnr
                                            FROM cobk
                                            INNER JOIN acdoca
                                                ON acdoca.co_belnr = cobk.belnr
                                            WHERE cobk.mandt  = Session_context('CLIENT')
                                              AND cobk.kokrs  = :iv_kokrs
                                              AND acdoca.rldnr = :iv_rldnr
                                              AND acdoca.rbukrs = :iv_bukrs
                                              AND acdoca.gjahr = :iv_gjahr
                                              AND acdoca.poper IN( SELECT DISTINCT poper FROM :iv_poper )
                                              AND acdoca.objnr LIKE 'EO%'
                                              AND acdoca.co_belnr IN( SELECT DISTINCT belnr FROM :iv_belnr)  );
    END IF ;
    ev_subrc = 0;
    ev_result = 'Entity has been deleted successfully.';


  ENDMETHOD.
  METHOD _rb3_rundat BY DATABASE PROCEDURE
                     FOR HDB
                     LANGUAGE SQLSCRIPT
                     USING cosp_bak coka acdoca.

    DECLARE lv_count NUMBER(10);

    SELECT COUNT (*) INTO lv_count FROM :iv_objnr;
    IF :lv_count = 0 THEN
        DELETE FROM cosp_bak as db WHERE( db.mandt, db.objnr, db.gjahr )
                                      IN ( SELECT DISTINCT cosp_bak.mandt, cosp_bak.objnr, cosp_bak.gjahr
                                            FROM cosp_bak
                                            INNER JOIN acdoca
                                                ON acdoca.objnr = cosp_bak.objnr AND
                                                   acdoca.gjahr = cosp_bak.gjahr
                                             WHERE cosp_bak.mandt  = Session_context('CLIENT')
                                               AND acdoca.rldnr = :iv_rldnr
                                               AND acdoca.rbukrs = :iv_bukrs
                                               AND acdoca.gjahr = :iv_gjahr
                                               AND acdoca.poper IN( SELECT DISTINCT poper FROM :iv_poper )
                                               AND acdoca.objnr LIKE 'EO%' );

        DELETE FROM coka as db WHERE( db.mandt, db.objnr, db.gjahr )
                                  IN ( SELECT DISTINCT coka.mandt, coka.objnr, coka.gjahr
                                         FROM coka
                                         INNER JOIN acdoca
                                             ON acdoca.objnr = coka.objnr AND
                                                acdoca.gjahr = coka.gjahr
                                         WHERE coka.mandt  = Session_context('CLIENT')
                                           AND acdoca.rldnr = :iv_rldnr
                                           AND acdoca.rbukrs = :iv_bukrs
                                           AND acdoca.gjahr = :iv_gjahr
                                           AND acdoca.poper IN( SELECT DISTINCT poper FROM :iv_poper )
                                           AND acdoca.objnr LIKE 'EO%' );

    ELSE
        DELETE FROM cosp_bak as db WHERE( db.mandt, db.objnr, db.gjahr )
                                      IN ( SELECT DISTINCT cosp_bak.mandt, cosp_bak.objnr, cosp_bak.gjahr
                                            FROM cosp_bak
                                            INNER JOIN acdoca
                                                ON acdoca.objnr = cosp_bak.objnr AND
                                                   acdoca.gjahr = cosp_bak.gjahr
                                             WHERE cosp_bak.mandt  = Session_context('CLIENT')
                                               AND acdoca.rldnr = :iv_rldnr
                                               AND acdoca.rbukrs = :iv_bukrs
                                               AND acdoca.gjahr = :iv_gjahr
                                               AND acdoca.poper IN( SELECT DISTINCT poper FROM :iv_poper )
                                               AND acdoca.objnr LIKE 'EO%'
                                               AND acdoca.objnr IN( SELECT DISTINCT belnr FROM :iv_objnr ) );

        DELETE FROM coka as db WHERE( db.mandt, db.objnr, db.gjahr )
                                  IN ( SELECT DISTINCT coka.mandt, coka.objnr, coka.gjahr
                                         FROM coka
                                         INNER JOIN acdoca
                                             ON acdoca.objnr = coka.objnr AND
                                                acdoca.gjahr = coka.gjahr
                                         WHERE coka.mandt  = Session_context('CLIENT')
                                           AND acdoca.rldnr = :iv_rldnr
                                           AND acdoca.rbukrs = :iv_bukrs
                                           AND acdoca.gjahr = :iv_gjahr
                                           AND acdoca.poper IN( SELECT DISTINCT poper FROM :iv_poper )
                                           AND acdoca.objnr LIKE 'EO%'
                                           AND acdoca.objnr IN( SELECT DISTINCT belnr FROM :iv_objnr ) );

    END IF ;
    ev_subrc = 0;
    ev_result = 'Entity has been deleted successfully.';


  ENDMETHOD.
ENDCLASS.
