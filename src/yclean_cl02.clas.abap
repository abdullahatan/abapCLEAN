CLASS yclean_cl02 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_amdp_marker_hdb .

    TYPES:
      BEGIN OF ty_fkdat,
        fkdat TYPE fkdat,
      END OF ty_fkdat,
      tt_fkdat TYPE STANDARD TABLE OF ty_fkdat WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_kschl,
        kschl TYPE kscha,
      END OF ty_kschl,
      tt_kschl TYPE STANDARD TABLE OF ty_kschl WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_splitdat,
        line  TYPE sy-tabix,
        fkdat TYPE tt_fkdat,
      END OF ty_splitdat,
      tt_splitdat TYPE STANDARD TABLE OF ty_splitdat WITH DEFAULT KEY.

    CONSTANTS:
      BEGIN OF mc_cons,
        save    TYPE yclean_de02 VALUE 'SAVE',
        delete  TYPE yclean_de02 VALUE 'DELETE',
        restore TYPE yclean_de02 VALUE 'RESTORE',
      END OF mc_cons.

    CLASS-METHODS _rb1_rundat
      IMPORTING
        VALUE(iv_fkart)  TYPE fkart
        VALUE(it_fkdat)  TYPE tt_fkdat
        VALUE(iv_where)  TYPE string
        VALUE(iv_kappl)  TYPE kappl
        VALUE(iv_kwert)  TYPE vfprc_element_value
        VALUE(it_kschl)  TYPE tt_kschl
      EXPORTING
        VALUE(ev_subrc)  TYPE syst_subrc
        VALUE(ev_rowcnt) TYPE syst-tabix
        VALUE(ev_result) TYPE string
      RAISING
        cx_amdp_error .

    CLASS-METHODS _rb2_rundat
      IMPORTING
        VALUE(iv_fkart)  TYPE fkart
        VALUE(it_fkdat)  TYPE tt_fkdat
        VALUE(iv_where)  TYPE string
        VALUE(iv_kappl)  TYPE kappl
        VALUE(iv_kwert)  TYPE vfprc_element_value
        VALUE(it_kschl)  TYPE tt_kschl
      EXPORTING
        VALUE(ev_subrc)  TYPE syst_subrc
        VALUE(ev_rowcnt) TYPE syst-tabix
        VALUE(ev_result) TYPE string
      RAISING
        cx_amdp_error .

    CLASS-METHODS _rb3_rundat
      IMPORTING
        VALUE(iv_fkart)  TYPE fkart
        VALUE(it_fkdat)  TYPE tt_fkdat
        VALUE(iv_where)  TYPE string
      EXPORTING
        VALUE(ev_subrc)  TYPE syst_subrc
        VALUE(ev_rowcnt) TYPE syst_tabix
        VALUE(ev_result) TYPE string
      RAISING
        cx_amdp_error .

    CLASS-METHODS _delete_before
      IMPORTING
        VALUE(iv_fkart)  TYPE fkart
        VALUE(it_fkdat)  TYPE tt_fkdat
        VALUE(iv_where)  TYPE string
        VALUE(iv_kappl)  TYPE kappl
        VALUE(iv_kwert)  TYPE vfprc_element_value
        VALUE(it_kschl)  TYPE tt_kschl
      EXPORTING
        VALUE(ev_subrc)  TYPE syst_subrc
        VALUE(ev_result) TYPE string
      RAISING
        cx_amdp_error .
    CLASS-METHODS _period_splitdat
      IMPORTING
        !im_spmon          TYPE spmon
        !im_split          TYPE int1
      RETURNING
        VALUE(rt_splitdat) TYPE tt_splitdat .
PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS yclean_cl02 IMPLEMENTATION.

  METHOD _rb1_rundat BY DATABASE PROCEDURE
                     FOR HDB
                     LANGUAGE SQLSCRIPT
                     USING vbrk prcd_elements yclean_t01.

*&---------------------------------------------------------------------*
*&  PRCD_ELMENTS Save Dat->
*&---------------------------------------------------------------------*

    DECLARE numberofentities NUMBER(10);

    t_vbrkdat = SELECT DISTINCT knumv, fkdat, bzirk
                    FROM vbrk
                    WHERE mandt = Session_context('CLIENT')
                      AND fkart = :iv_fkart
                      AND fkdat IN( SELECT DISTINCT fkdat FROM :it_fkdat );

*-> Apply filter->
    t_vbrk_result = APPLY_FILTER ( :t_vbrkdat, :iv_where );

    t_prcddat = SELECT DISTINCT prcd.client, prcd.knumv, prcd.kposn, prcd.stunr, prcd.zaehk
                    FROM prcd_elements AS prcd
                    WHERE prcd.client = Session_context('CLIENT')
                      AND prcd.knumv IN( SELECT DISTINCT knumv FROM :t_vbrk_result )
                      AND prcd.kappl = :iv_kappl
                      AND prcd.kschl IN( SELECT DISTINCT kschl FROM :it_kschl )
                      AND prcd.kwert = :iv_kwert;

    SELECT COUNT (*) INTO numberofentities FROM :t_prcddat;
    IF numberofentities <> 0
    THEN
        ev_rowcnt = :numberofentities;

        UPSERT yclean_t01 ( client, knumv, kposn, stunr, zaehk ) SELECT * FROM :t_prcddat;

        SELECT COUNT (*) INTO numberofentities FROM yclean_t01 AS db
            WHERE( db.client, db.knumv, db.kposn, db.stunr, db.zaehk ) IN ( SELECT Session_context( 'CLIENT' ), knumv, kposn, stunr, zaehk FROM :t_prcddat );
        IF numberofentities = :ev_rowcnt
        THEN
            ev_subrc = 0;
            ev_result = 'Data saved successfully.';
        ELSE
            ev_subrc = 4;
            ev_result = 'Data save failed!';
        END IF;
    ELSE
        ev_subrc = 4;
        ev_result = 'Your entried value is not valid.';
    END IF;

  ENDMETHOD.
  METHOD _rb2_rundat BY DATABASE PROCEDURE
                     FOR HDB
                     LANGUAGE SQLSCRIPT
                     USING vbrk prcd_elements.
*&---------------------------------------------------------------------*
*&  PRCD_ELMENTS Delete Dat->
*&---------------------------------------------------------------------*

    DECLARE numberofentities INTEGER :=0;
    DECLARE _knumv VARCHAR(10);

    t_vbrkdat = SELECT DISTINCT knumv, fkdat, bzirk
                FROM vbrk
                WHERE mandt = Session_context('CLIENT')
                  AND fkart = :iv_fkart
                  AND fkdat IN( SELECT DISTINCT fkdat FROM :it_fkdat ) WITH HINT(NO_INLINE);

*-> Apply filter->
    t_vbrk_result = APPLY_FILTER ( :t_vbrkdat, :iv_where );

    t_resultdat = SELECT DISTINCT prcd.knumv
                    FROM prcd_elements AS prcd
                    WHERE prcd.client = Session_context('CLIENT')
                      AND prcd.knumv IN( SELECT DISTINCT knumv FROM :t_vbrk_result )
                      AND prcd.kappl = :iv_kappl
                      AND prcd.kschl IN( SELECT DISTINCT kschl FROM :it_kschl )
                      AND prcd.kwert = :iv_kwert;

    SELECT COUNT ( * ) INTO numberofentities
        FROM prcd_elements AS prcd
        WHERE prcd.client = Session_context('CLIENT')
          AND prcd.knumv IN( SELECT DISTINCT knumv FROM :t_resultdat )
          AND prcd.kappl = :iv_kappl
          AND prcd.kschl IN( SELECT DISTINCT kschl FROM :it_kschl )
          AND prcd.kwert = :iv_kwert;
    IF numberofentities > 0
    THEN
        ev_rowcnt = :numberofentities;

        BEGIN
            DECLARE CURSOR cDelete FOR SELECT knumv FROM :t_resultdat;
            OPEN cDelete;
            WHILE 1 <> 2 DO
                FETCH cDelete INTO _knumv;
                IF cDelete::NOTFOUND THEN
                    BREAK;
               ELSE
                    DELETE FROM prcd_elements
                        WHERE client = Session_context('CLIENT')
                          AND knumv = _knumv
                          AND kappl = :iv_kappl
                          AND kschl IN( SELECT DISTINCT kschl FROM :it_kschl )
                          AND kwert = :iv_kwert;
                END IF;
            END WHILE;
        END;

        SELECT COUNT ( * ) INTO numberofentities
            FROM prcd_elements AS prcd
            WHERE prcd.client = Session_context('CLIENT')
              AND prcd.knumv IN( SELECT DISTINCT knumv FROM :t_resultdat )
              AND prcd.kappl = :iv_kappl
              AND prcd.kschl IN( SELECT DISTINCT kschl FROM :it_kschl )
              AND prcd.kwert = :iv_kwert;
        IF :numberofentities = 0
        THEN
            ev_subrc = 0;
            ev_result = 'Entity has been deleted successfully.';
         ELSE
            ev_subrc = 4;
            ev_result = 'Entity has not been deleted.';
         END IF;
    ELSE
        ev_subrc = 4;
        ev_result = 'Your entried value is not valid.';
    END IF;

  ENDMETHOD.
  METHOD _rb3_rundat BY DATABASE PROCEDURE
                     FOR HDB
                     LANGUAGE SQLSCRIPT
                     USING vbrk yclean_t01 prcd_elements zprcd_elements.

*&---------------------------------------------------------------------*
*&  PRCD_ELMENTS Restored Dat->
*&---------------------------------------------------------------------*

    DECLARE numberofentities INTEGER :=0;

    t_vbrkdat = SELECT DISTINCT knumv, fkdat, bzirk
                    FROM vbrk
                    WHERE mandt = Session_context('CLIENT')
                      AND fkart = :iv_fkart
                      AND fkdat IN( SELECT DISTINCT fkdat FROM :it_fkdat );

*-> Apply filter->
    t_vbrk_result = APPLY_FILTER ( :t_vbrkdat, :iv_where );

    t_cleandat = SELECT DISTINCT prcd.*
                    FROM zprcd_elements AS prcd
                    WHERE( client, knumv, kposn, stunr, zaehk)
                       IN( SELECT client, knumv, kposn, stunr, zaehk FROM  yclean_t01 WHERE knumv IN( SELECT knumv FROM :t_vbrk_result ) );


    numberofentities = RECORD_COUNT(:t_cleandat);
    IF numberofentities > 0
    THEN
        ev_rowcnt = :numberofentities;

        UPSERT prcd_elements SELECT * FROM :t_cleandat;

        SELECT ::ROWCOUNT INTO numberofentities FROM DUMMY;
        IF numberofentities = :ev_rowcnt
        THEN
            ev_subrc = 0;
            ev_result = 'Data restored successfully.';
        ELSE
            ev_subrc = 4;
            ev_result = 'Data restored fail!';
        END IF;
    ELSE
        ev_subrc = 4;
        ev_result = 'Your entried value is not valid.';
    END IF;

  ENDMETHOD.
  METHOD _delete_before BY DATABASE PROCEDURE
                        FOR HDB
                        LANGUAGE SQLSCRIPT
                         USING vbrk prcd_elements yclean_t01.

*&---------------------------------------------------------------------*
*&  PRCD_ELMENTS - YCLEAN_T01 Delete Controls->
*&---------------------------------------------------------------------*

    DECLARE _prdc_rowcount NUMBER(10);
    DECLARE _save_rowcount NUMBER(10);

    t_vbrkdat = SELECT DISTINCT knumv, fkdat, bzirk
                    FROM vbrk
                    WHERE mandt = Session_context('CLIENT')
                      AND fkart = :iv_fkart
                      AND fkdat IN( SELECT DISTINCT fkdat FROM :it_fkdat );

*-> Apply filter->
    t_vbrk_result = APPLY_FILTER ( :t_vbrkdat, :iv_where );

    SELECT COUNT( * ) INTO _prdc_rowcount
        FROM prcd_elements AS prcd
        WHERE PRCD.knumv IN( SELECT DISTINCT knumv FROM :t_vbrk_result )
          AND PRCD.KAPPL = :iv_kappl
          AND PRCD.KSCHL IN( SELECT DISTINCT kschl FROM :it_kschl )
          AND PRCD.KWERT = :iv_kwert;

    SELECT COUNT( * ) INTO _save_rowcount
        FROM yclean_t01 as prcd
            WHERE prcd.knumv IN( SELECT DISTINCT knumv FROM :t_vbrk_result );

    IF _save_rowcount <> _prdc_rowcount
    THEN
        ev_subrc = 4;
        ev_result = 'Data not backed up! Cannot be deleted!';
    ELSE
        ev_subrc = 0;
        ev_result = 'Data backup successful.';
    END IF;

  ENDMETHOD.
  METHOD _period_splitdat.

    TYPES: BEGIN OF ty_keydat,
             date TYPE datum,
           END OF ty_keydat,
           tt_keydat TYPE STANDARD TABLE OF ty_keydat WITH EMPTY KEY.

    DATA: _splitdat TYPE REF TO ty_splitdat,
          _begdat   TYPE datum,
          _enddat   TYPE datum,
          _keydat   TYPE tt_keydat,
          _tabix    TYPE sy-tabix,
          _mod      TYPE n LENGTH 2.


    FREE: _begdat, _begdat, _keydat.
    _begdat = |{ im_spmon }01|.

    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = _begdat
      IMPORTING
        last_day_of_month = _enddat
      EXCEPTIONS
        day_in_no_date    = 1
        OTHERS            = 2.

    APPEND VALUE #( date = _begdat ) TO _keydat.
    DO.
      ADD 1 TO _begdat.
      APPEND VALUE #( date = _begdat ) TO _keydat.
      IF _begdat EQ _enddat.
        EXIT.
      ENDIF.
    ENDDO.

    FREE: _mod.
    _mod = lines( _keydat ) / im_split.

    FREE: rt_splitdat, _tabix.
    DO im_split TIMES.
      ADD 1 TO _tabix.
      CREATE DATA _splitdat LIKE LINE OF rt_splitdat.
      _splitdat->line = _tabix.
      LOOP AT _keydat ASSIGNING FIELD-SYMBOL(<fs_keydat>) FROM 1 TO _mod.
        APPEND VALUE #( fkdat = <fs_keydat>-date ) TO _splitdat->fkdat.
      ENDLOOP.
      IF sy-subrc IS INITIAL.
        APPEND _splitdat->* TO rt_splitdat.
      ENDIF.
      DELETE _keydat FROM 1 TO _mod.
    ENDDO.
    IF NOT _keydat[] IS INITIAL.
      READ TABLE rt_splitdat REFERENCE INTO _splitdat INDEX im_split.
      IF sy-subrc IS INITIAL.
        LOOP AT _keydat ASSIGNING <fs_keydat>.
          APPEND VALUE #( fkdat = <fs_keydat>-date ) TO _splitdat->fkdat.
        ENDLOOP.
      ENDIF.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
