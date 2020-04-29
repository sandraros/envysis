CLASS zcl_envysis DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES :
      type_s_e071_objkey TYPE seok_trkey,
      type_t_e071_key    TYPE STANDARD TABLE OF type_s_e071_objkey WITH EMPTY KEY,
      soft_or_hard       TYPE c LENGTH 1,
      BEGIN OF type_s_e071_rel,
        soft_or_hard TYPE soft_or_hard,
        subobject    TYPE type_s_e071_objkey,
      END OF type_s_e071_rel,
      type_t_e071_rel TYPE STANDARD TABLE OF type_s_e071_rel WITH DEFAULT KEY,
      BEGIN OF type_s_e071_rel2,
        object       TYPE type_s_e071_objkey,
        soft_or_hard TYPE soft_or_hard,
        subobject    TYPE type_s_e071_objkey,
      END OF type_s_e071_rel2,
      type_t_e071_rel2 TYPE STANDARD TABLE OF type_s_e071_rel2 WITH EMPTY KEY,
      type_spad(5)     TYPE c,
      BEGIN OF type_s_wbobj_key,
        object  TYPE e071-object, "DYNP, PROG, FUGR, FUNC, etc,
        BEGIN OF docu,
          docu_id     TYPE dokhl-id,
          docu_object TYPE dokhl-object,
        END OF docu,
        include TYPE progname,
        trkorr  TYPE trkorr,
        BEGIN OF tobj,
          objectname TYPE objh-objectname,
          objecttype TYPE objh-objecttype,
        END OF tobj,
        BEGIN OF s_sott,
          paket   TYPE sotr_pack,
          concept TYPE sotr_conc,
        END OF s_sott,
        BEGIN OF s_spcs,
          spad_type TYPE type_spad,
          codepage  TYPE tcp00-cpcodepage,
        END OF s_spcs,
        BEGIN OF s_spsv,
          spad_type TYPE type_spad,
          server    TYPE tspsv-server,
        END OF s_spsv,
        BEGIN OF s_spdv,
          spad_type TYPE type_spad,
          device    TYPE tsp03-padest,
        END OF s_spdv,
        BEGIN OF splo,
          spad_type    TYPE type_spad,
          paper_format TYPE tsp1d-papart,
        END OF splo,
        BEGIN OF s_prin,
          spad_type    TYPE type_spad,
          printer_type TYPE tsp0a-patype,
        END OF s_prin,
        BEGIN OF s_slom,
          spad_type             TYPE type_spad,
          logical_output_system TYPE tsploms-name,
        END OF s_slom,
        BEGIN OF s_soms,
          spad_type          TYPE type_spad,
          read_output_system TYPE tsproms-name,
        END OF s_soms,
        BEGIN OF s_scp,
          bcset_id TYPE scpr_id,
          category TYPE scpr_ctgry,
        END OF s_scp,
        file    TYPE cts_guid32,
        BEGIN OF s_dynp,
          program_name  TYPE d020s-prog,
          screen_number TYPE d020s-dnum,
        END OF s_dynp,
        BEGIN OF s_vari,
          variant_name TYPE vari-variant,
          program_name TYPE vari-report,
        END OF s_vari,
        BEGIN OF s_mess,
          msg_class_name TYPE t100-arbgb,
          msg_number     TYPE t100-msgnr,
        END OF s_mess,
        BEGIN OF s_meth,
          class_name  TYPE seoclsname, " or interface
          method_name TYPE seocpdname,
        END OF s_meth,
        BEGIN OF s_wdyc,
          webdynpro_name  TYPE wdy_component_name,
          controller_name TYPE wdy_controller_name,
        END OF s_wdyc,
        BEGIN OF s_wdyv,
          webdynpro_name TYPE wdy_component_name,
          view_name      TYPE wdy_view_name,
        END OF s_wdyv,
        BEGIN OF s_wapp,
          appl_name TYPE o2applname,
          page_name TYPE o2page,
        END OF s_wapp,
      END OF type_s_wbobj_key,
      BEGIN OF ty_excluding,
        software_components TYPE RANGE OF dlvunit,
      END OF ty_excluding.
*        tt_excluding type STANDARD TABLE OF ty_excluding with EMPTY KEY.

    CLASS-METHODS get_used_objects_first_level
      IMPORTING
        is_e071_key TYPE type_s_e071_objkey
      EXPORTING
        et_e071_key TYPE type_t_e071_rel
      RAISING
        cx_enh_root.

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter is_e071_key | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter levels | 0 = all levels <p class="shorttext synchronized" lang="en"></p>
    "! @parameter et_e071_rel2 | <p class="shorttext synchronized" lang="en"></p>
    "! @raising cx_enh_root | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS get_used_objects_multi_levels
      IMPORTING
        is_e071_key  TYPE type_s_e071_objkey
        excluding    TYPE ty_excluding
        levels       TYPE i DEFAULT 0
      EXPORTING
        et_e071_rel2 TYPE type_t_e071_rel2
      RAISING
        cx_enh_root.

    CLASS-METHODS get_subobj
      IMPORTING
        is_object    TYPE type_s_e071_objkey
      EXPORTING
        et_subobject TYPE type_t_e071_key
      RAISING
        zcx_envysis.

  PROTECTED SECTION.

  PRIVATE SECTION.

    CLASS-DATA gt_e071_key TYPE type_t_e071_rel.

    CLASS-METHODS build_e071_key
      IMPORTING
        is_wbobj_key TYPE  type_s_wbobj_key
      CHANGING
        es_e071_key  TYPE  seok_trkey.
    CLASS-METHODS get_include_required_objects
      IMPORTING
        i_include TYPE progname.

    CLASS-METHODS get_devc_required_objects
      IMPORTING
        i_package_name TYPE devclass.

    CLASS-METHODS tablstruc_field
      IMPORTING
        is_dd03p    TYPE dd03p
        is_e071_key TYPE type_s_e071_objkey.

    CLASS-METHODS collect
      IMPORTING
        soft_or_hard TYPE soft_or_hard
        y            TYPE type_s_e071_objkey.  "subobject

    CLASS-METHODS ddif_typeinfo_get
      IMPORTING
        i_tabname TYPE tabname
      CHANGING
        e_object  TYPE ddtypekind.

    CLASS-METHODS rs_progname_concatenate
      IMPORTING
        VALUE(fugr_group)            TYPE rs38l-area OPTIONAL
        VALUE(fugr_include_number)   TYPE tfdir-include OPTIONAL
        VALUE(sldb_name)             TYPE ldbd-ldbname OPTIONAL
        VALUE(menu_name)             TYPE tstc-tcode OPTIONAL
        VALUE(type_name)             TYPE trdir-name OPTIONAL
        VALUE(mst_name)              TYPE dd02l-tabname OPTIONAL
        VALUE(cntx_name)             TYPE trdir-name OPTIONAL
        VALUE(clas_name)             TYPE seoclass-clsname OPTIONAL
        VALUE(intf_name)             TYPE seoclass-clsname OPTIONAL
      EXPORTING
        VALUE(fugr_progname_group)   TYPE trdir-name
        VALUE(fugr_progname_include) TYPE trdir-name
        VALUE(fugr_progname_top)     TYPE trdir-name
        VALUE(fugr_progname_uxx)     TYPE trdir-name
        VALUE(sldb_progname_db)      TYPE trdir-name
        VALUE(sldb_progname_sel)     TYPE trdir-name
        VALUE(menu_progname)         TYPE trdir-name
        VALUE(type_progname)         TYPE trdir-name
        VALUE(mst_progname)          TYPE trdir-name
        VALUE(cntx_progname)         TYPE trdir-name
        VALUE(intf_progname)         TYPE trdir-name
        VALUE(clas_progname)         TYPE trdir-name
      EXCEPTIONS
        delimiter_error.


    CLASS-METHODS tr_check_type
      IMPORTING
        wi_e071     TYPE e071
      EXPORTING
        we_lock_key TYPE tlock_int
        we_tadir    TYPE tadir.
    CLASS-METHODS enhancement_implementation
      IMPORTING
        name TYPE enhname
      RAISING
        cx_enh_root.

ENDCLASS.



CLASS zcl_envysis IMPLEMENTATION.

  METHOD tr_check_type.

    DATA ls_e071 TYPE e071.

    " TR_CHECK_TYPE returns empty when object = VARX !
    " With VARI, it returns program name
    ls_e071 = wi_e071.
    IF ls_e071-object = 'VARX'.
      ls_e071-object = 'VARI'.
    ENDIF.
    CALL FUNCTION 'TR_CHECK_TYPE'
      EXPORTING
        wi_e071     = ls_e071
      IMPORTING
        we_tadir    = we_tadir
        we_lock_key = we_lock_key.
  ENDMETHOD.

  METHOD ddif_typeinfo_get.
    DATA l_typename TYPE typename.
    DATA l_typekind TYPE ddtypekind.

    CLEAR e_object.

    l_typename = i_tabname.
    CALL FUNCTION 'DDIF_TYPEINFO_GET'
      EXPORTING
        typename = l_typename
      IMPORTING
        typekind = l_typekind.
    e_object = l_typekind.
  ENDMETHOD.

  METHOD build_e071_key.
    DATA ls_ko100 TYPE ko100.

    " determine if type (PROG, CLAS, DYNP, etc.) corresponds to R3TR or LIMU
    CALL FUNCTION 'TR_GET_PGMID_FOR_OBJECT'
      EXPORTING
        iv_object      = is_wbobj_key-object
      IMPORTING
        es_type        = ls_ko100
      EXCEPTIONS
        illegal_object = 1
        OTHERS         = 2.

    CLEAR es_e071_key.
    es_e071_key-pgmid  = ls_ko100-pgmid. "usually R3TR or LIMU
    es_e071_key-object = is_wbobj_key-object.

    CASE is_wbobj_key-object.

      WHEN 'DOCU'.
        es_e071_key-obj_name(2) = is_wbobj_key-docu-docu_id.
        es_e071_key-obj_name+2 = is_wbobj_key-docu-docu_object.


      WHEN 'SOTT' OR 'SOTU'.
        es_e071_key-obj_name(30) = is_wbobj_key-s_sott-paket.
        es_e071_key-obj_name+30 = is_wbobj_key-s_sott-concept.


      WHEN 'TOBJ'.
        CONCATENATE is_wbobj_key-tobj-objectname
                    is_wbobj_key-tobj-objecttype
            INTO es_e071_key-obj_name.


      WHEN 'MERG' OR 'RELE' OR 'COMM'.
        es_e071_key-obj_name = is_wbobj_key-trkorr.


      WHEN 'SPCS'.
        es_e071_key-obj_name = is_wbobj_key-s_spcs-codepage.
      WHEN 'SPSV'.
        es_e071_key-obj_name = is_wbobj_key-s_spsv-server.
      WHEN 'SPDV'.
        SELECT SINGLE name FROM tsp03d INTO es_e071_key-obj_name
              WHERE padest = is_wbobj_key-s_spdv-device.
      WHEN 'SPLO'.
        es_e071_key-obj_name = is_wbobj_key-splo-paper_format.
      WHEN 'PRIN'.
        es_e071_key-obj_name = is_wbobj_key-s_prin-printer_type.
      WHEN 'SLOM'.
        es_e071_key-obj_name = is_wbobj_key-s_slom-logical_output_system.
      WHEN 'SOMS'.
        es_e071_key-obj_name = is_wbobj_key-s_soms-read_output_system.


      WHEN 'SCP1'.
        es_e071_key-obj_name = is_wbobj_key-s_scp-bcset_id.
      WHEN 'SCP2'.
        es_e071_key-obj_name = is_wbobj_key-s_scp-bcset_id.


      WHEN 'FILE'.
        es_e071_key-obj_name = is_wbobj_key-file.

      WHEN 'REPO'.
        es_e071_key-obj_name = is_wbobj_key-include.

      WHEN 'DYNP'.
        es_e071_key-pgmid    = 'LIMU'.
        CONSTANTS: gc_dynp TYPE i VALUE 4,
                   gc_prog TYPE i VALUE 40,
                   gc_vari TYPE i VALUE 14,
                   gc_clas TYPE i VALUE 30,
                   gc_meth TYPE i VALUE 61,
                   gc_wdyn TYPE i VALUE 30,
                   gc_wdyc TYPE i VALUE 30,
                   gc_wdyv TYPE i VALUE 30,
                   gc_wapa TYPE i VALUE 30,
                   gc_wapp TYPE i VALUE 30.
        es_e071_key-obj_name+gc_prog(gc_dynp) = is_wbobj_key-s_dynp-screen_number.
        es_e071_key-obj_name(gc_prog)         = is_wbobj_key-s_dynp-program_name.

      WHEN 'VARI' OR 'VARX'.
        es_e071_key-obj_name+gc_prog(gc_vari) = is_wbobj_key-s_vari-variant_name.
        es_e071_key-obj_name(gc_prog)         = is_wbobj_key-s_vari-program_name.

      WHEN 'MESS'.
        es_e071_key-pgmid    = 'LIMU'.
        CONCATENATE is_wbobj_key-s_mess-msg_class_name
                    is_wbobj_key-s_mess-msg_number
              INTO es_e071_key-obj_name.

      WHEN 'METH'.
        es_e071_key-pgmid    = 'LIMU'.
        es_e071_key-obj_name+gc_clas(gc_meth) = is_wbobj_key-s_meth-method_name.
        es_e071_key-obj_name(gc_clas)         = is_wbobj_key-s_meth-class_name.

* Web Dynpro controller
      WHEN 'WDYC'.
        es_e071_key-obj_name+gc_wdyn(gc_wdyc) = is_wbobj_key-s_wdyc-controller_name.
        es_e071_key-obj_name(gc_wdyn) = is_wbobj_key-s_wdyc-webdynpro_name.

* Web Dynpro view
      WHEN 'WDYV'.
        es_e071_key-obj_name+gc_wdyn(gc_wdyv) = is_wbobj_key-s_wdyv-view_name.
        es_e071_key-obj_name(gc_wdyn) = is_wbobj_key-s_wdyv-webdynpro_name.

* Page/Controller of a BSP Application
      WHEN 'WAPD' OR 'WAPP'.
        es_e071_key-obj_name+gc_wapa(gc_wapp) = is_wbobj_key-s_wapp-page_name.
        es_e071_key-obj_name(gc_wapa) = is_wbobj_key-s_wapp-appl_name.

      WHEN OTHERS.
* FUGR, CLAS, FUNC, PROG, etc.
        es_e071_key-obj_name = is_wbobj_key-object.

    ENDCASE.
  ENDMETHOD.

  METHOD get_subobj.

    DATA: lt_t100        TYPE TABLE OF t100,
          ls_t100        TYPE t100,
          lt_varid       TYPE TABLE OF varid,
          ls_varid       TYPE varid,
          ls_e071_key    TYPE type_s_e071_objkey,
          ls_e071        TYPE e071,
          lt_vrso_source TYPE TABLE OF vrso,
          ls_vrso        TYPE vrso.

    REFRESH et_subobject.

    IF is_object-pgmid = 'R3TR'.
      CASE is_object-object.

          "---------------------
          " Message class
          "---------------------
        WHEN 'MSAG'.
          SELECT * FROM t100 INTO TABLE lt_t100 WHERE arbgb = is_object-obj_name.
          LOOP AT lt_t100 INTO ls_t100.
            ls_e071_key-pgmid    = 'LIMU'.
            ls_e071_key-object   = 'MESS'.
            CONCATENATE is_object-obj_name ls_t100-msgnr INTO ls_e071_key-obj_name.
            APPEND ls_e071_key TO et_subobject.
          ENDLOOP.

          "---------------------
          " infoObject
          "---------------------
        WHEN 'IOBJ'.
* ajouter DTEL, TABL, VIEW, etc.
          TYPE-POOLS rsd.
          DATA l_iobjnm TYPE rsd_s_iobj-iobjnm.
          DATA l_dtelnm TYPE rollname.
          l_iobjnm = is_object-obj_name.
          CALL FUNCTION 'RSD_DTELNM_GET_FOR_IOBJ'
            EXPORTING
              i_iobjnm   = l_iobjnm
            IMPORTING
              e_dtelnm   = l_dtelnm
            EXCEPTIONS
              name_error = 1
              OTHERS     = 2.
          IF sy-subrc = 0.
            ls_e071_key-pgmid    = 'R3TR'.
            ls_e071_key-object   = 'DTEL'.
            ls_e071_key-obj_name = l_dtelnm.
            APPEND ls_e071_key TO et_subobject.
*          mac_collect2 is_subobject 'H' '' ls_e071_key is_subrequest ''.
          ENDIF.
          DATA ls_iobj_detail TYPE bapi6108.
          DATA ls_return TYPE bapiret2.
          CLEAR ls_return.
          CALL FUNCTION 'BAPI_IOBJ_GETDETAIL'
            EXPORTING
              infoobject = l_iobjnm
            IMPORTING
              details    = ls_iobj_detail
              return     = ls_return.
          IF ls_return IS INITIAL.
            DEFINE mac_iobj_table.
              IF ls_iobj_detail-&1 IS NOT INITIAL.
                ls_e071_key-pgmid    = 'R3TR'.
                ls_e071_key-object   = 'TABL'.
                ls_e071_key-obj_name = ls_iobj_detail-&1.
                APPEND ls_e071_key TO et_subobject.
*              mac_collect2 is_subobject 'H' '' ls_e071_key is_subrequest ''.
              ENDIF.
            END-OF-DEFINITION.
            mac_iobj_table: chktab, chntab, chttab, txttab.
          ENDIF.
          "---------------------
          " DSO
          "---------------------
        WHEN 'ODSO'.
          DATA l_odsobject TYPE rsdodsobject.
          DATA l_tablnm TYPE tabname.
          DATA l_ttypename TYPE ttypename.
          DATA l_viewnm TYPE viewname.
          l_odsobject = is_object-obj_name.
          CLEAR : l_tablnm, l_ttypename, l_viewnm.
          CALL METHOD cl_rsd_odso=>get_tablnm
            EXPORTING
              i_odsobject = l_odsobject
            IMPORTING
              e_tablnm    = l_tablnm
              e_ttypename = l_ttypename
              e_viewnm    = l_viewnm.
          IF l_tablnm IS NOT INITIAL.
            ls_e071_key-pgmid    = 'R3TR'.
            ls_e071_key-object   = 'TABL'.
            ls_e071_key-obj_name = l_tablnm.
            APPEND ls_e071_key TO et_subobject.
*          mac_collect2 is_subobject 'H' '' ls_e071_key is_subrequest ''.
          ENDIF.
          IF l_ttypename IS NOT INITIAL.
            ls_e071_key-pgmid    = 'R3TR'.
            ls_e071_key-object   = 'TTYP'.
            ls_e071_key-obj_name = l_ttypename.
            APPEND ls_e071_key TO et_subobject.
*          mac_collect2 is_subobject 'H' '' ls_e071_key is_subrequest ''.
          ENDIF.
          IF l_viewnm IS NOT INITIAL.
            ls_e071_key-pgmid    = 'R3TR'.
            ls_e071_key-object   = 'VIEW'.
            ls_e071_key-obj_name = l_viewnm.
            APPEND ls_e071_key TO et_subobject.
*          mac_collect2 is_subobject 'H' '' ls_e071_key is_subrequest ''.
          ENDIF.

        WHEN OTHERS.

          " Call TRINT_RESOLVE_OBJ on each system to check presence of all subobjects
          "   Caution: this subobjets n'existent pas forcément.
          " If subobject belongs to a frame object, check that this last one exists in the
          "   same transport request or one transported before.
          "   For example, if it contains FUGR object, all its LIMU FUNC are
          "   also transported.
          ls_e071-object = is_object-object.
          ls_e071-obj_name = is_object-obj_name.
          REFRESH lt_vrso_source.
          CALL FUNCTION 'TRINT_RESOLVE_OBJ'
            EXPORTING
              is_e071             = ls_e071
            TABLES
              et_vrso             = lt_vrso_source
            EXCEPTIONS
              not_versionable     = 1
              communication_error = 2
              OTHERS              = 3.
          IF sy-subrc <> 0.
* TODO
*            DEFINE exception1.
*              lcx_dev_cross_ref_fm_call=>excname2 = &1.
*              RAISE EXCEPTION TYPE lcx_dev_cross_ref_fm_call.
*            END-OF-DEFINITION.
*            lcx_dev_cross_ref_fm_call=>fbname2 = 'TRINT_RESOLVE_OBJ'.
*            CASE sy-subrc.
*              WHEN 1. exception1 'NOT_VERSIONABLE'.
*              WHEN 2. exception1 'COMMUNICATION_ERROR'.
*              WHEN 3. exception1 'OTHERS'.
*            ENDCASE.
          ENDIF.
          LOOP AT lt_vrso_source INTO ls_vrso.
            ls_e071_key-pgmid    = 'LIMU'.
            ls_e071_key-object   = ls_vrso-objtype.
            ls_e071_key-obj_name = ls_vrso-objname.
            APPEND ls_e071_key TO et_subobject.
          ENDLOOP.
      ENDCASE.
    ENDIF.

    CASE is_object-object.
      WHEN 'PROG' OR 'REPS' OR 'REPO'.
* propose automatically system variants
        SELECT * FROM varid CLIENT SPECIFIED
              INTO TABLE lt_varid
              WHERE mandt     = '000'  "system variants are only in client 000
                AND report    = is_object-obj_name
                AND transport = space. "system variant
        LOOP AT lt_varid INTO ls_varid.
          ls_e071_key-pgmid    = 'LIMU'.
          ls_e071_key-object   = 'VARX'.
          CONCATENATE ls_varid-report ls_varid-variant INTO ls_e071_key-obj_name RESPECTING BLANKS.
          APPEND ls_e071_key TO et_subobject.
        ENDLOOP.
    ENDCASE.

  ENDMETHOD.

  METHOD collect.
    DATA ls_e071_key TYPE type_s_e071_rel.

    ls_e071_key-soft_or_hard = soft_or_hard.
    ls_e071_key-subobject    = y.
*    APPEND ls_e071_key TO gt_e071_key.
    COLLECT ls_e071_key INTO gt_e071_key.

  ENDMETHOD.

  METHOD get_devc_required_objects.
    DATA lo_package TYPE REF TO if_package.
    DATA ls_e071_key TYPE type_s_e071_objkey.
    DATA is_e071_key TYPE type_s_e071_objkey.

    CALL METHOD cl_package=>load_package
      EXPORTING
        i_package_name             = i_package_name
      IMPORTING
        e_package                  = lo_package
      EXCEPTIONS
        object_not_existing        = 1
        unexpected_error           = 2
        intern_err                 = 3
        object_locked_and_modified = 4
        OTHERS                     = 5.
    IF sy-subrc <> 0.
      " TODO
    ENDIF.
    IF lo_package IS BOUND.

      " SUB-PACKAGES
      DATA lto_package_sub TYPE scompaklis.
      CALL METHOD lo_package->get_sub_packages
        IMPORTING
          e_sub_packages   = lto_package_sub
        EXCEPTIONS
          object_invalid   = 1
          leaf_package     = 2
          unexpected_error = 3
          OTHERS           = 4.
      IF sy-subrc <> 0.
      " TODO
      ENDIF.
      DATA lo_package_sub TYPE REF TO if_package.
      LOOP AT lto_package_sub INTO lo_package_sub.
        ls_e071_key-pgmid    = 'R3TR'.
        ls_e071_key-object   = 'DEVC'.
        ls_e071_key-obj_name = lo_package_sub->package_name.
        collect( soft_or_hard = 'S' y = ls_e071_key ).
      ENDLOOP.

      " SUPER PACKAGE
      DATA lo_package_super TYPE REF TO if_package.
      CALL METHOD lo_package->get_super_package
        IMPORTING
          e_super_package = lo_package_super
        EXCEPTIONS
          root_package    = 1
          OTHERS          = 2.
      IF sy-subrc = 0.

        ls_e071_key-pgmid    = 'R3TR'.
        ls_e071_key-object   = 'DEVC'.
        ls_e071_key-obj_name = lo_package_super->package_name.
        collect( soft_or_hard = 'H' y = ls_e071_key ).

        DATA lo_interface TYPE REF TO if_package_interface.
        DATA lto_interface TYPE tpak_package_interface_list.
        CALL METHOD lo_package->get_interfaces
          IMPORTING
            e_package_interfaces = lto_interface.
        LOOP AT lto_interface INTO lo_interface.
          ls_e071_key-pgmid    = 'R3TR'.
          ls_e071_key-object   = 'PINF'.
          ls_e071_key-obj_name = lo_interface->interface_name.
          collect( soft_or_hard = 'S' y = ls_e071_key ).
        ENDLOOP.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD get_include_required_objects.
    DATA:
      ls_e071_key            TYPE type_s_e071_objkey,
      lt_incl                TYPE TABLE OF rseuinc,
      lt_cross               TYPE TABLE OF cross,
      l_progname             TYPE syrepid,
      l_subc                 TYPE trdir-subc,
      lte_main               TYPE TABLE OF d010inc-master,
      l_object               TYPE e071-object,
      ls_wbobj_key           TYPE type_s_wbobj_key,
      l_class_is_name        TYPE  c,
      l_class_name           TYPE  seoclsname,
      l_class_is_method_name TYPE  c,
      l_class_method_name    TYPE  seocpdname,
      is_e071_key            TYPE type_s_e071_objkey.
    FIELD-SYMBOLS:
      <ls_incl>  TYPE rseuinc,
      <ls_cross> TYPE cross,
      <l_main>   TYPE d010inc-master.

    l_progname = i_include.


    " Get list of INCLUDE statements.
    CALL FUNCTION 'RS_GET_ALL_INCLUDES'
      EXPORTING
        program      = l_progname
      TABLES
        includetab   = lt_incl
      EXCEPTIONS
        not_existent = 0
        no_program   = 0
        OTHERS       = 3.
    IF sy-subrc <> 0.
      " TODO
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
* 2010-11-22
    " D010INC table may sometimes contain an erroneous entry:
    "   for classname===...===CP, RS_GET_ALL_INCLUDES returns
    "   packagename===...===P that doesn't exist
    DELETE lt_incl WHERE master+30 = 'P'.

    " For each include, get all objects it is using.
    LOOP AT lt_incl ASSIGNING <ls_incl>.

      " Determine the type of the include
      CALL FUNCTION 'RS_PROGNAME_SPLIT'
        EXPORTING
          progname_with_namespace = <ls_incl>-master
        IMPORTING
          class_is_name           = l_class_is_name
          class_name              = l_class_name
          class_is_method_name    = l_class_is_method_name
          class_method_name       = l_class_method_name
        EXCEPTIONS
          delimiter_error         = 1
          OTHERS                  = 2.
      IF sy-subrc <> 0.
        " TODO
      ENDIF.
      IF l_class_is_name = 'X'.
        " The include refers to a class or interface
        "   (section or class frame program)
        DATA lo_clif TYPE REF TO if_oo_clif_incl_naming.
        CALL METHOD cl_oo_include_naming=>get_instance_by_include
          EXPORTING
            progname      = <ls_incl>-master
          RECEIVING
            cifref        = lo_clif
          EXCEPTIONS
            no_objecttype = 1
            OTHERS        = 2.
        ls_e071_key-pgmid    = 'R3TR'.
        ls_e071_key-object   = lo_clif->transport_key-object."CLAS or INTF
        ls_e071_key-obj_name = lo_clif->transport_key-obj_name.
        collect( soft_or_hard = 'H' y = ls_e071_key ).
      ELSEIF l_class_method_name IS NOT INITIAL.
        " The include refers to a class method
        ls_wbobj_key-object = 'METH'.
        ls_wbobj_key-s_meth-class_name = l_class_name.
        ls_wbobj_key-s_meth-method_name = l_class_method_name.
        CALL METHOD build_e071_key
          EXPORTING
            is_wbobj_key = ls_wbobj_key
          CHANGING
            es_e071_key  = ls_e071_key.
        collect( soft_or_hard = 'H' y = ls_e071_key ).
      ELSE.
        ls_e071_key-pgmid    = 'R3TR'.
        ls_e071_key-object   = 'PROG'.
        ls_e071_key-obj_name = <ls_incl>-master.
        collect( soft_or_hard = 'H' y = ls_e071_key ).
      ENDIF.
    ENDLOOP.


    " Get all DDIC types referenced in the includes
    APPEND l_progname TO lt_incl.

    DATA lt_wbcrossgt TYPE TABLE OF wbcrossgt.
    DATA ls_wbcrossgt TYPE wbcrossgt.
    DATA l_ddictype TYPE wbcrossgt-name.
    DATA l_remain TYPE wbcrossgt-name.
    SELECT * FROM wbcrossgt INTO TABLE lt_wbcrossgt
          FOR ALL ENTRIES IN lt_incl
          WHERE include = lt_incl-master
            AND otype = 'TY'.
    LOOP AT lt_wbcrossgt INTO ls_wbcrossgt.
      SPLIT ls_wbcrossgt-name AT '\' INTO l_ddictype l_remain.
      " Get DDIC object type
      IF l_remain IS INITIAL.
        DATA l_tabname TYPE tabname.
        l_tabname = l_ddictype.
        CALL METHOD ddif_typeinfo_get
          EXPORTING
            i_tabname = l_tabname
          CHANGING
            e_object  = l_object.
        IF l_object IS INITIAL.
          " If it's not a DDIC type, then it's a class or interface
          DATA l_clstype TYPE seoclass-clstype.
          SELECT SINGLE clstype FROM seoclass INTO l_clstype
                WHERE clsname = l_ddictype.
          IF sy-subrc = 0.
            IF l_clstype = 0.
              l_object = 'CLAS'.
            ELSE.
              l_object = 'INTF'.
            ENDIF.
          ENDIF.
        ENDIF.
        IF l_object IS NOT INITIAL.
          ls_e071_key-pgmid    = 'R3TR'.
          ls_e071_key-object   = l_object.
          ls_e071_key-obj_name = l_ddictype.
          collect( soft_or_hard = 'H' y = ls_e071_key ).
        ENDIF.
      ELSE.
        l_remain = l_remain+3. "remove TY:
        DATA ls_dd03p TYPE dd03p.
        SELECT SINGLE * FROM dd03l INTO CORRESPONDING FIELDS OF ls_dd03p
              WHERE tabname = l_ddictype
                AND fieldname = l_remain
                AND as4local = 'A'
                AND as4vers = 0.
        IF sy-subrc = 0.
          CALL METHOD tablstruc_field
            EXPORTING
              is_dd03p    = ls_dd03p
              is_e071_key = is_e071_key.
        ENDIF.
      ENDIF.
      " Note: we don't want to know if data/type comes from a type-pool
      "   because it's a little bit complex (DA and TY); instead, we use the
      "   type-pools declarations referenced in CROSS table (see below)
    ENDLOOP.

    " Note: cross-name = "?" for objects dynamically called.
    SELECT * FROM cross
        INTO TABLE lt_cross
        FOR ALL ENTRIES IN lt_incl
        WHERE include = lt_incl-master
          AND name    NE '?'.

    LOOP AT lt_cross ASSIGNING <ls_cross>.
      CASE <ls_cross>-type.

        WHEN 'B'.
          ls_wbobj_key-object = 'DYNP'.
          ls_wbobj_key-s_dynp-screen_number = <ls_cross>-prog.
          ls_wbobj_key-s_dynp-program_name = <ls_cross>-include.
          CALL METHOD build_e071_key
            EXPORTING
              is_wbobj_key = ls_wbobj_key
            CHANGING
              es_e071_key  = ls_e071_key.
          collect( soft_or_hard = 'H' y = ls_e071_key ).

          " CALL FUNCTION.
          "   R3TR FUGR - LIMU FUNC
          "   R3TR ENQU ( LOCK OBJECTS )
        WHEN 'F'.

          IF <ls_cross>-name(9) = 'ENQUEUE_E'
                  OR <ls_cross>-name(9) = 'DEQUEUE_E'.
            ls_e071_key-pgmid    = 'R3TR'.
            ls_e071_key-object   = 'ENQU'.
            ls_e071_key-obj_name = <ls_cross>-name+8.
            collect( soft_or_hard = 'H' y = ls_e071_key ).
          ELSE.
            ls_e071_key-pgmid    = 'LIMU'.
            ls_e071_key-object   = 'FUNC'.
            ls_e071_key-obj_name = <ls_cross>-name.
            collect( soft_or_hard = 'H' y = ls_e071_key ).
          ENDIF.

          " TYPE-POOLS
        WHEN 'G'.
          ls_e071_key-pgmid    = 'R3TR'.
          ls_e071_key-object   = 'TYPE'.
          ls_e071_key-obj_name = <ls_cross>-name.
          collect( soft_or_hard = 'H' y = ls_e071_key ).

          " CHECKPOINT GROUP
        WHEN 'H'.
          ls_e071_key-pgmid    = 'R3TR'.
          ls_e071_key-object   = 'ACID'.
          ls_e071_key-obj_name = <ls_cross>-name.
          collect( soft_or_hard = 'H' y = ls_e071_key ).

          " MESSAGE NUMBER
          "   R3TR MSAG
          "   LIMU MESS
        WHEN 'N'.
          ls_e071_key-pgmid    = 'R3TR'.
          ls_e071_key-object   = 'MSAG'.
          ls_e071_key-obj_name = <ls_cross>-name(20).
          collect( soft_or_hard = 'H' y = ls_e071_key ).
          IF NOT <ls_cross>-name+20(3) IS INITIAL.
            ls_wbobj_key-object = 'MESS'.
            ls_wbobj_key-s_mess-msg_number = <ls_cross>-name+20(3).
            ls_wbobj_key-s_mess-msg_class_name = <ls_cross>-name(20).
            CALL METHOD build_e071_key
              EXPORTING
                is_wbobj_key = ls_wbobj_key
              CHANGING
                es_e071_key  = ls_e071_key.
            collect( soft_or_hard = 'H' y = ls_e071_key ).
          ENDIF.

          " SEARCH HELP (M = in dynpro, V = in program).
          "   R3TR SHLD or R3TR SHLP
        WHEN 'M' OR 'V'.
          ls_e071_key-pgmid    = 'R3TR'.
          ls_e071_key-object   = 'SHLP'.
          ls_e071_key-obj_name = <ls_cross>-name.
          collect( soft_or_hard = 'H' y = ls_e071_key ).

          " GET PARAMETER or SET PARAMETER
        WHEN 'P'.
          ls_e071_key-pgmid    = 'R3TR'.
          ls_e071_key-object   = 'PARA'.
          ls_e071_key-obj_name = <ls_cross>-name.
          collect( soft_or_hard = 'H' y = ls_e071_key ).

          " SUBMIT
        WHEN 'R'.
          ls_e071_key-pgmid    = 'R3TR'.
          ls_e071_key-object   = 'PROG'.
          ls_e071_key-obj_name = <ls_cross>-name.
          collect( soft_or_hard = 'H' y = ls_e071_key ).

          " Use of structure, table, view, table type or data element
        WHEN 'S'.
          DATA l_typename TYPE typename.
          DATA l_gotstate TYPE ddgotstate.
          DATA l_typekind TYPE ddtypekind.
          l_typename = <ls_cross>-name.
          CALL FUNCTION 'DDIF_TYPEINFO_GET'
            EXPORTING
              typename = l_typename
            IMPORTING
              typekind = l_typekind
              gotstate = l_gotstate.
          IF l_typekind IS NOT INITIAL.
            ls_e071_key-pgmid    = 'R3TR'.
            ls_e071_key-object   = l_typekind. "TTYP, TABL, VIEW, DTEL
            ls_e071_key-obj_name = <ls_cross>-name.
            collect( soft_or_hard = 'H' y = ls_e071_key ).
          ENDIF.

          " CALL TRANSACTION
        WHEN 'T'.
          ls_e071_key-pgmid    = 'R3TR'.
          ls_e071_key-object   = 'TRAN'.
          ls_e071_key-obj_name = <ls_cross>-name.
          collect( soft_or_hard = 'H' y = ls_e071_key ).

          " PERFORM cross-name IN PROGRAM cross-prog
        WHEN 'U'.
          ls_e071_key-pgmid    = 'R3TR'.
          ls_e071_key-object   = 'PROG'.
          ls_e071_key-obj_name = <ls_cross>-prog.
          collect( soft_or_hard = 'H' y = ls_e071_key ).

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.



  METHOD get_used_objects_first_level.

    DATA:
      ls_e071_key  TYPE type_s_e071_objkey,
      ls_wbobj_key TYPE type_s_wbobj_key,
      lt_dd03p     TYPE TABLE OF dd03p,
      lt_dd08v     TYPE TABLE OF dd08v,
      lt_dd35v     TYPE TABLE OF dd35v,
      l_domname    TYPE ddobjname,
      l_rollname   TYPE ddobjname,
      l_devclass   TYPE tadir-devclass,
      l_progname   TYPE syrepid,
      l_include    TYPE trdir-name,
      l_subc       TYPE trdir-subc,
      l_lockobject TYPE ddobjname,
      ls_dd01v     TYPE dd01v,
      ls_dd04v     TYPE dd04v,
      ls_dd30v     TYPE dd30v,
      l_shlpname   TYPE ddobjname,
      l_object     TYPE e071-object,
      ls_dd25v     TYPE dd25v,
      ls_e071      TYPE e071,
      ls_tadir     TYPE tadir,
      l_ddobjname  TYPE ddobjname,
      lt_dd26v     TYPE TABLE OF dd26v,
      ls_dd26v     TYPE dd26v.
    FIELD-SYMBOLS:
      <ls_dd03p> TYPE dd03p,
      <ls_dd08v> TYPE dd08v,
      <ls_dd35v> TYPE dd35v.

    ls_e071_key = is_e071_key.

    REFRESH et_e071_key.
    REFRESH gt_e071_key.

    DATA ls_ko100 TYPE ko100.
    CALL FUNCTION 'TR_GET_PGMID_FOR_OBJECT'
      EXPORTING
        iv_object      = ls_e071_key-object
      IMPORTING
        es_type        = ls_ko100
      EXCEPTIONS
        illegal_object = 1
        OTHERS         = 2.
    IF sy-subrc = 0.
      ls_e071_key-pgmid = ls_ko100-pgmid.
    ENDIF.


    MOVE-CORRESPONDING is_e071_key TO ls_e071.
    CALL METHOD tr_check_type
      EXPORTING
        wi_e071  = ls_e071
      IMPORTING
        we_tadir = ls_tadir.

    IF ls_tadir-object <> 'DEVC'.
      SELECT SINGLE devclass FROM tadir INTO l_devclass
            WHERE pgmid     = ls_tadir-pgmid
              AND object    = ls_tadir-object
              AND obj_name  = ls_tadir-obj_name.
      IF sy-subrc = 0.
        ls_e071_key-pgmid    = 'R3TR'.
        ls_e071_key-object   = 'DEVC'.
        ls_e071_key-obj_name = l_devclass.
        collect( soft_or_hard = 'S' y = ls_e071_key ).
      ENDIF.
    ENDIF.

    " Add main program
    "  LIMU METH YYYY  XXXX -> Add R3TR CLAS YYYY
    "  LIMU DYNP YYYY     XXXX -> Add R3TR PROG YYYY
    "  LIMU DYNP SAPLYYYY XXXX -> Add R3TR FUGR YYYY
    "  LIMU MESS YYYY  XXX -> Add R3TR MSAG YYYY
    " etc.
    IF is_e071_key-pgmid = 'LIMU'.
      ls_e071_key-pgmid    = ls_tadir-pgmid.
      ls_e071_key-object   = ls_tadir-object.
      ls_e071_key-obj_name = ls_tadir-obj_name.
      collect( soft_or_hard = 'H' y = ls_e071_key ).
    ENDIF.

    ls_e071_key = is_e071_key.

    CASE ls_e071_key-object.

      WHEN 'DEVC'.
        "---------------------
        " DEVELOPMENT CLASS
        "---------------------
        " Only the super-package is considered a "used object".
        " NB 1: The sub-packages should not be considered as "used objects",
        "   otherwise a sub-package A
        "   having a super-package B, having a super-package C, would lead to
        "   analyze all objects below the super-package C at all levels.
        " NB 2: the objects contained in a package should not be
        "   considered as "used objects", otherwise a used object of sub-package A
        "   having a super-package B, having a super-package C, would lead to
        "   analyze all objects below the super-package C at all levels.
        SELECT SINGLE parentcl
            FROM tdevc
            WHERE devclass = @is_e071_key-obj_name
              AND parentcl <> @space
            INTO @DATA(parentcl).
        IF sy-subrc = 0.
          ls_e071_key-pgmid    = 'R3TR'.
          ls_e071_key-object   = 'DEVC'.
          ls_e071_key-obj_name = parentcl.
          collect( soft_or_hard = 'H' y = ls_e071_key ).
        ENDIF.
*        DATA l_package_name TYPE devclass.
*        l_package_name = is_e071_key-obj_name.
*        CALL METHOD get_devc_required_objects
*          EXPORTING
*            i_package_name = l_package_name.


      WHEN 'TRAN'.
        "---------------------
        " transaction code
        "---------------------

        DATA l_tcode TYPE tcode.

        l_tcode = is_e071_key-obj_name.
        DATA(lo_transaction) = zcl_tcode=>load( l_tcode ).

        CASE lo_transaction->type.
          WHEN 'O'.
            IF lo_transaction->s_object-local_class = 'X'.
              ls_e071_key-pgmid    = 'R3TR'.
              ls_e071_key-object   = 'PROG'.
              ls_e071_key-obj_name = lo_transaction->s_object-program_name.
              collect( soft_or_hard = 'H' y = ls_e071_key ).
*              mac_collect is_e071_key 'H' '' ls_e071_key ''.
            ELSE.
              ls_e071_key-pgmid   = 'LIMU'.
              ls_wbobj_key-object = 'METH'.
              ls_wbobj_key-s_meth-method_name = lo_transaction->s_object-method_name.
              ls_wbobj_key-s_meth-class_name = lo_transaction->s_object-global_class_name.
              CALL METHOD build_e071_key
                EXPORTING
                  is_wbobj_key = ls_wbobj_key
                CHANGING
                  es_e071_key  = ls_e071_key.
              collect( soft_or_hard = 'H' y = ls_e071_key ).
*              mac_collect is_e071_key 'H' '' ls_e071_key ''.
            ENDIF.
            IF NOT lo_transaction->s_object-auth_object IS INITIAL.
              ls_e071_key-pgmid    = 'R3TR'.
              ls_e071_key-object   = 'SUSO'.
              ls_e071_key-obj_name = lo_transaction->s_object-auth_object.
              collect( soft_or_hard = 'H' y = ls_e071_key ).
*              mac_collect is_e071_key 'H' '' ls_e071_key ''.
            ENDIF.
          WHEN 'D'.
            ls_e071_key-pgmid = 'LIMU'.
            ls_wbobj_key-object = 'DYNP'.
            ls_wbobj_key-s_dynp-program_name = lo_transaction->s_dialog-program_name.
            ls_wbobj_key-s_dynp-screen_number = lo_transaction->s_dialog-screen_number.
            CALL METHOD build_e071_key
              EXPORTING
                is_wbobj_key = ls_wbobj_key
              CHANGING
                es_e071_key  = ls_e071_key.
            collect( soft_or_hard = 'H' y = ls_e071_key ).
*            mac_collect is_e071_key 'H' '' ls_e071_key ''.
            IF NOT lo_transaction->s_dialog-auth_object IS INITIAL.
              ls_e071_key-pgmid    = 'R3TR'.
              ls_e071_key-object   = 'SUSO'.
              ls_e071_key-obj_name = lo_transaction->s_dialog-auth_object.
              collect( soft_or_hard = 'H' y = ls_e071_key ).
*              mac_collect is_e071_key 'H' '' ls_e071_key ''.
            ENDIF.
          WHEN 'P'.
            IF NOT lo_transaction->s_parameter-called_tcode IS INITIAL.
              ls_e071_key-pgmid    = 'R3TR'.
              ls_e071_key-object   = 'TRAN'.
              ls_e071_key-obj_name = lo_transaction->s_parameter-called_tcode.
              collect( soft_or_hard = 'H' y = ls_e071_key ).
*              mac_collect is_e071_key 'H' '' ls_e071_key ''.
            ENDIF.
            IF NOT lo_transaction->s_parameter-program_name IS INITIAL.
              ls_e071_key-pgmid   = 'LIMU'.
              ls_wbobj_key-object = 'DYNP'.
              ls_wbobj_key-s_dynp-program_name = lo_transaction->s_parameter-program_name.
              ls_wbobj_key-s_dynp-screen_number = lo_transaction->s_parameter-screen_number.
              CALL METHOD build_e071_key
                EXPORTING
                  is_wbobj_key = ls_wbobj_key
                CHANGING
                  es_e071_key  = ls_e071_key.
              collect( soft_or_hard = 'H' y = ls_e071_key ).
*              mac_collect is_e071_key 'H' '' ls_e071_key ''.
            ENDIF.
          WHEN 'V'.
            ls_e071_key-pgmid    = 'R3TR'.
            ls_e071_key-object   = 'TRAN'.
            ls_e071_key-obj_name = lo_transaction->s_variant-called_tcode.
            collect( soft_or_hard = 'H' y = ls_e071_key ).
*            mac_collect is_e071_key 'H' '' ls_e071_key ''.
            IF NOT lo_transaction->s_variant-transac_variant IS INITIAL.
              ls_e071_key-object   = 'STVI'.
              ls_e071_key-obj_name = lo_transaction->s_variant-transac_variant.
              collect( soft_or_hard = 'H' y = ls_e071_key ).
*              mac_collect is_e071_key 'H' '' ls_e071_key ''.
            ENDIF.
          WHEN 'R'.
            ls_e071_key-pgmid    = 'R3TR'.
            ls_e071_key-object   = 'PROG'.
            ls_e071_key-obj_name = lo_transaction->s_report-program_name.
            collect( soft_or_hard = 'H' y = ls_e071_key ).
*            mac_collect is_e071_key 'H' '' ls_e071_key ''.
            IF NOT lo_transaction->s_report-auth_object IS INITIAL.
              ls_e071_key-pgmid    = 'R3TR'.
              ls_e071_key-object   = 'SUSO'.
              ls_e071_key-obj_name = lo_transaction->s_report-auth_object.
              collect( soft_or_hard = 'H' y = ls_e071_key ).
*              mac_collect is_e071_key 'H' '' ls_e071_key ''.
            ENDIF.
            IF NOT lo_transaction->s_report-program_variant IS INITIAL.
              ls_e071_key-pgmid   = 'LIMU'.
              " TODO : could be also VARX (&CUS...)
              ls_wbobj_key-object = 'VARI'.
              ls_wbobj_key-s_vari-program_name = lo_transaction->s_report-program_name.
              ls_wbobj_key-s_vari-variant_name = lo_transaction->s_report-program_variant.
              CALL METHOD build_e071_key
                EXPORTING
                  is_wbobj_key = ls_wbobj_key
                CHANGING
                  es_e071_key  = ls_e071_key.
              collect( soft_or_hard = 'H' y = ls_e071_key ).
*              mac_collect is_e071_key 'H' '' ls_e071_key ''.
            ENDIF.
        ENDCASE.

      WHEN 'ENHO'.
        "---------------------
        " ENHANCEMENT Implementation
        "---------------------
        TRY.
            enhancement_implementation( name = CONV enhname( is_e071_key-obj_name ) ).
          CATCH cx_enh_root ##NO_HANDLER.
        ENDTRY.

      WHEN 'PROG' OR 'REPS' OR 'REPO'.
        "---------------------
        " PROGRAM
        "---------------------
        CASE ls_e071_key-object.
          WHEN 'REPS' OR 'REPO'.
            SELECT SINGLE subc FROM trdir INTO l_subc WHERE name = l_progname.
            CASE l_subc.
              WHEN '1' OR 'M' OR 'F' OR 'S' OR 'K'.
                ls_e071_key-pgmid    = 'LIMU'.
                ls_e071_key-object   = 'REPT'.
                ls_e071_key-obj_name = is_e071_key-obj_name.
                collect( soft_or_hard = 'H' y = ls_e071_key ).
            ENDCASE.
        ENDCASE.
        l_include = is_e071_key-obj_name.
        CALL METHOD get_include_required_objects
          EXPORTING
            i_include = l_include.

      WHEN 'INTF' OR 'INTD'.
        "---------------------
        " interface pool
        "---------------------
        DATA l_interface_name TYPE seoclass-clsname.
        l_interface_name = is_e071_key-obj_name.
        rs_progname_concatenate(
          EXPORTING
            intf_name       = l_interface_name
          IMPORTING
            intf_progname   = l_include
          EXCEPTIONS
            delimiter_error = 1
            OTHERS          = 2 ).
        CALL METHOD get_include_required_objects
          EXPORTING
            i_include = l_include.

      WHEN 'CLAS'.
        "---------------------
        " class pool
        "---------------------
        DATA l_class_name TYPE seoclass-clsname.
        l_class_name = is_e071_key-obj_name.
        rs_progname_concatenate(
          EXPORTING
            clas_name       = l_class_name
          IMPORTING
            clas_progname   = l_include
          EXCEPTIONS
            delimiter_error = 1
            OTHERS          = 2 ).
        IF sy-subrc = 0.
          CALL METHOD get_include_required_objects
            EXPORTING
              i_include = l_include.
        ENDIF.

      WHEN 'METH'.
        "---------------------
        " method
        "---------------------
        ls_wbobj_key-s_meth = is_e071_key-obj_name.
        cl_oo_classname_service=>get_method_include(
          EXPORTING
            mtdkey                = VALUE seocpdkey(
                clsname = ls_wbobj_key-s_meth-class_name
                cpdname = ls_wbobj_key-s_meth-method_name )
            with_enhancements     = 'X'
            with_alias_resolution = 'X'
          RECEIVING
            result                = l_include
          EXCEPTIONS
            class_not_existing    = 1
            method_not_existing   = 2
            OTHERS                = 3 ).
        IF sy-subrc = 0.
          CALL METHOD get_include_required_objects
            EXPORTING
              i_include = l_include.
        ENDIF.

      WHEN 'FUNC'.
        "---------------------
        " function
        "---------------------
        DATA l_funcname TYPE rs38l-name.
        CALL FUNCTION 'FUNCTION_EXISTS'
          EXPORTING
            funcname           = l_funcname
          IMPORTING
            str_area           = l_include
          EXCEPTIONS
            function_not_exist = 1.
        IF sy-subrc = 0.
          CALL METHOD get_include_required_objects
            EXPORTING
              i_include = l_include.
        ENDIF.

      WHEN 'TYPE' OR 'TYPD'.
        "---------------------
        " TYPE-POOL
        "---------------------
        DATA l_type_name TYPE trdir-name.
        l_type_name = is_e071_key-obj_name.
        rs_progname_concatenate(
          EXPORTING
            type_name       = l_type_name
          IMPORTING
            type_progname   = l_include
          EXCEPTIONS
            delimiter_error = 1
            OTHERS          = 2 ).
        CALL METHOD get_include_required_objects
          EXPORTING
            i_include = l_include.

      WHEN 'FUGT'.
        "---------------------
        " FUNCTION GROUP
        "---------------------
        ls_e071_key-pgmid    = 'R3TR'.
        ls_e071_key-object   = 'FUGR'.
        ls_e071_key-obj_name = is_e071_key-obj_name.
        collect( soft_or_hard = 'H' y = ls_e071_key ).
*        mac_collect is_e071_key 'H' '' ls_e071_key ''.

      WHEN 'FUGR'.
        DATA l_function_group TYPE rs38l-area.
        l_function_group = is_e071_key-obj_name.
        rs_progname_concatenate(
          EXPORTING
            fugr_group          = l_function_group
          IMPORTING
            fugr_progname_group = l_include
          EXCEPTIONS
            delimiter_error     = 1
            OTHERS              = 2 ).
        IF sy-subrc = 0.
          CALL METHOD get_include_required_objects
            EXPORTING
              i_include = l_include.
        ENDIF.

      WHEN 'TABT'.
        "---------------------
        " TABLE or STRUCTURE
        "---------------------
        ls_e071_key-pgmid    = 'R3TR'.
        ls_e071_key-object   = 'TABL'.
        ls_e071_key-obj_name = is_e071_key-obj_name.
        collect( soft_or_hard = 'H' y = ls_e071_key ).
*        mac_collect is_e071_key 'H' '' ls_e071_key ''.
      WHEN 'TABL' OR 'TABD'.
        DATA l_name TYPE ddobjname.
        l_name = is_e071_key-obj_name.
        CALL FUNCTION 'DDIF_TABL_GET'
          EXPORTING
            name          = l_name
          TABLES
            dd03p_tab     = lt_dd03p
            dd08v_tab     = lt_dd08v
            dd35v_tab     = lt_dd35v
          EXCEPTIONS
            illegal_input = 1
            OTHERS        = 2.
        IF sy-subrc = 0.
          " Fields : data elements, domaines, tables de contrôles
          LOOP AT lt_dd03p ASSIGNING <ls_dd03p>.
            CALL METHOD tablstruc_field
              EXPORTING
                is_dd03p    = <ls_dd03p>
                is_e071_key = is_e071_key.

            IF NOT <ls_dd03p>-checktable IS INITIAL.
              ls_e071_key-pgmid    = 'R3TR'.
              ls_e071_key-object   = 'TABL'.
              ls_e071_key-obj_name = <ls_dd03p>-checktable.
              collect( soft_or_hard = 'H' y = ls_e071_key ).
*              mac_collect is_e071_key 'H' '' ls_e071_key ''.
            ENDIF.
          ENDLOOP.
          " Foreign keys
          LOOP AT lt_dd08v ASSIGNING <ls_dd08v>.
            ls_e071_key-pgmid    = 'R3TR'.
            ls_e071_key-object   = 'TABL'.
            ls_e071_key-obj_name = <ls_dd08v>-checktable.
            collect( soft_or_hard = 'H' y = ls_e071_key ).
*            mac_collect is_e071_key 'H' '' ls_e071_key ''.
          ENDLOOP.
* Search helps
          LOOP AT lt_dd35v ASSIGNING <ls_dd35v>.
            IF NOT <ls_dd35v>-shlpname IS INITIAL.
              ls_e071_key-pgmid    = 'R3TR'.
              ls_e071_key-object   = 'SHLP'.
              ls_e071_key-obj_name = <ls_dd35v>-shlpname.
              collect( soft_or_hard = 'H' y = ls_e071_key ).
*              mac_collect is_e071_key 'H' '' ls_e071_key ''.
            ENDIF.
          ENDLOOP.
        ENDIF.

      WHEN 'VIEW' OR 'VIED'.
        "---------------------
        " VIEW
        "---------------------
        l_lockobject = is_e071_key-obj_name.
        CALL FUNCTION 'DDIF_VIEW_GET'
          EXPORTING
            name          = l_ddobjname
          IMPORTING
            dd25v_wa      = ls_dd25v
          TABLES
            dd26v_tab     = lt_dd26v
          EXCEPTIONS
            illegal_input = 1
            OTHERS        = 2.
        IF sy-subrc = 0.
          LOOP AT lt_dd26v INTO ls_dd26v.
            ls_e071_key-pgmid    = 'R3TR'.
            ls_e071_key-object   = 'TABL'.
            ls_e071_key-obj_name = ls_dd26v-tabname.
            collect( soft_or_hard = 'H' y = ls_e071_key ).
*            mac_collect is_e071_key 'H' '' ls_e071_key ''.
          ENDLOOP.
        ENDIF.

      WHEN 'TTYP' OR 'TTYD'.
        "---------------------
        " TABLE TYPE
        "---------------------
        l_ddobjname = is_e071_key-obj_name.
        DATA ls_dd40v TYPE dd40v.
        CALL FUNCTION 'DDIF_TTYP_GET'
          EXPORTING
            name          = l_ddobjname
          IMPORTING
            dd40v_wa      = ls_dd40v
          EXCEPTIONS
            illegal_input = 1
            OTHERS        = 2.
        IF sy-subrc = 0.
          ls_e071_key-pgmid    = 'R3TR'.
          CALL METHOD ddif_typeinfo_get
            EXPORTING
              i_tabname = ls_dd40v-rowtype
            CHANGING
              e_object  = ls_e071_key-object.
          ls_e071_key-obj_name = ls_dd40v-rowtype.
          collect( soft_or_hard = 'H' y = ls_e071_key ).
*          mac_collect is_e071_key 'H' '' ls_e071_key ''.
        ENDIF.

      WHEN 'DTEL' OR 'DTED'.
        "---------------------
        " DATA ELEMENT
        "---------------------
        l_rollname = is_e071_key-obj_name.
        DATA ls_tpara TYPE tpara.
        CALL FUNCTION 'DDIF_DTEL_GET'
          EXPORTING
            name          = l_rollname
          IMPORTING
            dd04v_wa      = ls_dd04v
            tpara_wa      = ls_tpara
          EXCEPTIONS
            illegal_input = 1
            OTHERS        = 2.
        IF sy-subrc = 0.
          IF NOT ls_dd04v-domname IS INITIAL.
            ls_e071_key-pgmid    = 'R3TR'.
            ls_e071_key-object   = 'DOMA'.
            ls_e071_key-obj_name = ls_dd04v-domname.
            collect( soft_or_hard = 'H' y = ls_e071_key ).
*            mac_collect is_e071_key 'H' '' ls_e071_key ''.
          ENDIF.
          IF NOT ls_dd04v-shlpname IS INITIAL.
            ls_e071_key-pgmid    = 'R3TR'.
            ls_e071_key-object   = 'SHLP'.
            ls_e071_key-obj_name = ls_dd04v-shlpname.
            collect( soft_or_hard = 'H' y = ls_e071_key ).
*            mac_collect is_e071_key 'H' '' ls_e071_key ''.
          ENDIF.
          IF NOT ls_tpara-paramid IS INITIAL.
            ls_e071_key-pgmid    = 'R3TR'.
            ls_e071_key-object   = 'PARA'.
            ls_e071_key-obj_name = ls_tpara-paramid.
            collect( soft_or_hard = 'H' y = ls_e071_key ).
*            mac_collect is_e071_key 'H' '' ls_e071_key ''.
          ENDIF.
        ENDIF.

      WHEN 'DOMA' OR 'DOMD'.
        "---------------------
        " DOMAIN
        "---------------------
        l_domname = is_e071_key-obj_name.
        CALL FUNCTION 'DDIF_DOMA_GET'
          EXPORTING
            name          = l_domname
          IMPORTING
            dd01v_wa      = ls_dd01v
          EXCEPTIONS
            illegal_input = 1
            OTHERS        = 2.
        IF sy-subrc = 0.
          IF NOT ls_dd01v-entitytab IS INITIAL.
            ls_e071_key-pgmid    = 'R3TR'.
            ls_e071_key-object   = 'TABL'.
            ls_e071_key-obj_name = ls_dd01v-entitytab.
            collect( soft_or_hard = 'H' y = ls_e071_key ).
*            mac_collect is_e071_key 'H' '' ls_e071_key ''.
          ENDIF.
        ENDIF.

      WHEN 'SHLP' OR 'SHLD'.
        "---------------------
        " SEARCH HELP
        "---------------------
        l_shlpname = is_e071_key-obj_name.
        CALL FUNCTION 'DDIF_SHLP_GET'
          EXPORTING
            name          = l_shlpname
          IMPORTING
            dd30v_wa      = ls_dd30v
          EXCEPTIONS
            illegal_input = 1
            OTHERS        = 2.
        IF sy-subrc = 0.
          IF NOT ls_dd30v-selmethod IS INITIAL.
            CALL METHOD ddif_typeinfo_get
              EXPORTING
                i_tabname = ls_dd30v-selmethod
              CHANGING
                e_object  = l_object.
            ls_e071_key-object   = l_object.
            ls_e071_key-obj_name = ls_dd30v-selmethod.
            collect( soft_or_hard = 'H' y = ls_e071_key ).
*            mac_collect is_e071_key 'H' '' ls_e071_key ''.
          ENDIF.
          IF NOT ls_dd30v-texttab IS INITIAL.
            ls_e071_key-pgmid    = 'R3TR'.
            ls_e071_key-object   = 'TABL'.
            ls_e071_key-obj_name = ls_dd30v-texttab.
            collect( soft_or_hard = 'H' y = ls_e071_key ).
*            mac_collect is_e071_key 'H' '' ls_e071_key ''.
          ENDIF.
        ENDIF.

      WHEN 'ENQU'.
        "---------------------
        " LOCK OBJECT
        "---------------------
        l_lockobject = is_e071_key-obj_name.
        CALL FUNCTION 'DDIF_ENQU_GET'
          EXPORTING
            name          = l_lockobject
          IMPORTING
            dd25v_wa      = ls_dd25v
          EXCEPTIONS
            illegal_input = 1
            OTHERS        = 2.
        IF sy-subrc = 0.
          ls_e071_key-pgmid    = 'R3TR'.
          ls_e071_key-object   = 'TABL'.
          ls_e071_key-obj_name = ls_dd25v-roottab.
          collect( soft_or_hard = 'H' y = ls_e071_key ).
*          mac_collect is_e071_key 'H' '' ls_e071_key ''.
        ENDIF.

      WHEN 'IOBC' "infoObject catalog
        OR 'IOBJ' "infoObject
        OR 'RSPC' "process chain
        OR 'TRFN' "transformation
        OR 'AREA' "infoArea
        OR 'APCO' "Application Components
        OR 'ODSO' "Data Store Object
        OR 'ROUT' "ABAP code (routine)
        OR 'TRCS'."infoSource

        "---------------------
        " BI objects
        "---------------------
        DATA lo_awb TYPE REF TO cl_rso_repository.
        TYPE-POOLS: rs, rsoc.
        DATA: l_collect_grouping TYPE rso_collect_grouping,
              lt_object          TYPE rso_t_tlogo,
              ls_object          TYPE rso_s_tlogo,
              l_search_levels    TYPE i,
              lth_association    TYPE rso_th_association.
        FIELD-SYMBOLS: <ls_association> TYPE rso_s_association,
                       <ls_object>      TYPE rso_s_tlogo_objref_proxy.


        lo_awb = cl_rso_repository=>get_repository( ).
        REFRESH lt_object.
        ls_object-tlogo = is_e071_key-object.
        ls_object-objnm = is_e071_key-obj_name.
        APPEND ls_object TO lt_object.
        l_collect_grouping = rsoc_c_collect_grouping-required."Only necessary objects
        l_search_levels = 2.
        REFRESH lth_association.
        CALL METHOD lo_awb->transport_wizard
          EXPORTING
            i_t_object         = lt_object
            i_collect_grouping = l_collect_grouping
            i_search_level     = l_search_levels
          IMPORTING
            e_th_association   = lth_association
          EXCEPTIONS
            object_not_found   = 0.
        IF sy-subrc = 0.
          READ TABLE lth_association ASSIGNING <ls_association>
                WITH TABLE KEY
                  tlogo   = ls_object-tlogo
                  objnm   = ls_object-objnm
                  objvers = 'A'.
          IF sy-subrc = 0.
            LOOP AT <ls_association>-objects ASSIGNING <ls_object>.
              ls_e071_key-pgmid    = 'R3TR'.
              ls_e071_key-object   = <ls_object>-tlogo.
              ls_e071_key-obj_name = <ls_object>-objnm.
              collect( soft_or_hard = 'H' y = ls_e071_key ).
*              mac_collect is_e071_key 'H' '' ls_e071_key ''.
            ENDLOOP.
          ENDIF.
        ENDIF.
** ajouter tous les infoObjets qu'il contient
*        DATA lt_rsdiobciobj TYPE TABLE OF rsdiobciobj.
*        FIELD-SYMBOLS <ls_rsdiobciobj> TYPE rsdiobciobj.
*        SELECT * FROM rsdiobciobj INTO TABLE lt_rsdiobciobj
*              WHERE infoobjcat = is_e071_key-obj_name
*                AND objvers    = 'A'.
*        LOOP AT lt_rsdiobciobj ASSIGNING <ls_rsdiobciobj>.
*          ls_e071_key-pgmid    = 'R3TR'.
*          ls_e071_key-object   = 'IOBJ'.
*          ls_e071_key-obj_name = <ls_rsdiobciobj>-iobjnm.
*          mac_collect is_e071_key 'H' '' ls_e071_key ''.
*        ENDLOOP.

        CASE is_e071_key-object.
          WHEN 'RSPC'.
            "---------------------
            " Process chain
            "---------------------
*            DATA lt_rspcchain TYPE TABLE OF rspcchain.
*            FIELD-SYMBOLS <ls_rspcchain> TYPE rspcchain.
*            SELECT * FROM rspcchain INTO TABLE lt_rspcchain
*                  WHERE chain_id = is_e071_key-obj_name
*                    AND objvers  = 'A'.
*            LOOP AT lt_rspcchain ASSIGNING <ls_rspcchain>.
*            ENDLOOP.
*READ TABLE lt_rspcchain ASSIGNING <ls_rspcchain>
*      WITH KEY
*        type = 'TRIGGER'.
*IF SY-subrc = 0.
*DO.
* <ls_rspcchain>-event IS NOT INITIAL.
*IF <ls_rspcchain>-next IS INITIAL.
*  EXIT.
*ENDIF.
*READ TABLE lt_rspcchain ASSIGNING <ls_rspcchain>
*      WITH KEY
*        type = 'TRIGGER'.
*IF SY-subrc <> 0.
*  EXIT.
*ENDIF.
*ENDDO.

          WHEN 'TRFN'.
            "---------------------
            " transformation
            "---------------------
** générer les cas d'emploi du programme généré de la transformation
*        DATA ls_rstran TYPE rstran.
** OBJ_name could be 034QILJDF4LOEOSUXEYPWMS7K6X9K6JU
*        SELECT SINGLE * FROM rstran INTO ls_rstran
*              WHERE tranid = is_e071_key-obj_name
*                AND objvers = 'A'.
*        IF sy-subrc = 0.
** rstran-tranprog could be 3KHXIECTOI8OJA7D9NQTQZIWB
** prog -> GP3KHXIECTOI8OJA7D9NQTQZIWB
*          SUBMIT saprseui
*                WITH repname = l_include
*                AND RETURN.
*          CONCATENATE 'GP' ls_rstran-tranprog INTO l_include.
*          ls_e071_key-pgmid    = 'R3TR'.
*          ls_e071_key-object   = 'PROG'.
*          ls_e071_key-obj_name = l_include.
*          mac_collect is_e071_key 'H' '' ls_e071_key ''.
*        ENDIF.
        ENDCASE.

    ENDCASE.

    "---------------------
    " ENVIRONMENT ANALYSIS
    "---------------------
    DATA:
      ls_envi_types TYPE envi_types,
      lt_envi       TYPE senvi_tab,
      l_deep        TYPE i,
      l_obj_type    TYPE seu_obj,
      l_objname     TYPE sobj_name.
*    l_PARALLEL type abap_bool.
    CLEAR ls_envi_types WITH 'X'.
    l_obj_type = is_e071_key-object.
    l_objname = is_e071_key-obj_name.
    REFRESH lt_envi.
    l_deep = 1.
    CALL FUNCTION 'REPOSITORY_ENVIRONMENT_ALL'
      EXPORTING
        obj_type          = l_obj_type
        environment_types = ls_envi_types
        object_name       = l_objname
        deep              = l_deep
      TABLES
        environment_tab   = lt_envi.

    FIELD-SYMBOLS <ls_envi> TYPE senvi.

    LOOP AT lt_envi ASSIGNING <ls_envi>.
      l_object = <ls_envi>-type.
      CALL FUNCTION 'TR_GET_PGMID_FOR_OBJECT'
        EXPORTING
          iv_object      = l_object
        IMPORTING
          es_type        = ls_ko100
        EXCEPTIONS
          illegal_object = 1
          OTHERS         = 2.
      IF sy-subrc = 0.
        ls_e071_key-pgmid = ls_ko100-pgmid.
        ls_e071_key-object   = <ls_envi>-type.
        ls_e071_key-obj_name = <ls_envi>-object.
        collect( soft_or_hard = 'H' y = ls_e071_key ).
*        mac_collect is_e071_key 'H' '' ls_e071_key ''.
      ENDIF.
    ENDLOOP.

    et_e071_key = gt_e071_key.

  ENDMETHOD.



  METHOD get_used_objects_multi_levels.
    TYPES: BEGIN OF ty_tadir,
             pgmid    TYPE tadir-pgmid,
             object   TYPE tadir-object,
             obj_name TYPE tadir-obj_name,
             devclass TYPE tadir-devclass,
           END OF ty_tadir,
           BEGIN OF ty_tdevc,
             devclass TYPE tdevc-devclass,
             dlvunit  TYPE tdevc-dlvunit,
           END OF ty_tdevc.
    DATA: tdevc_lines TYPE HASHED TABLE OF ty_tdevc WITH UNIQUE KEY devclass.
    DATA: tadir_lines TYPE HASHED TABLE OF ty_tadir WITH UNIQUE KEY pgmid object obj_name.
    DATA: lt_e071_key     TYPE HASHED TABLE OF type_s_e071_objkey WITH UNIQUE KEY pgmid object obj_name,
          lt_e071_key_new TYPE STANDARD TABLE OF type_s_e071_objkey,
          ls_e071_rel2    TYPE type_s_e071_rel2.

    et_e071_rel2 = value #( ).

    lt_e071_key = VALUE #( ( is_e071_key ) ).

    LOOP AT lt_e071_key REFERENCE INTO DATA(e071_key).

      get_used_objects_first_level(
        EXPORTING
          is_e071_key = e071_key->*
        IMPORTING
          et_e071_key = DATA(lt_e071_rel) ).

      LOOP AT lt_e071_rel REFERENCE INTO DATA(e071_rel).
        APPEND VALUE type_s_e071_rel2(
                object       = e071_key->*
                soft_or_hard = e071_rel->soft_or_hard
                subobject    = e071_rel->subobject )
            TO et_e071_rel2.
      ENDLOOP.

      lt_e071_key_new = VALUE #( ).
      LOOP AT lt_e071_rel REFERENCE INTO e071_rel
            WHERE soft_or_hard = 'H'.
        DATA(ls_e071_key) = VALUE type_s_e071_objkey(
            pgmid    = e071_rel->subobject-pgmid
            object   = e071_rel->subobject-object
            obj_name = e071_rel->subobject-obj_name ).
        IF NOT line_exists( lt_e071_key[
            pgmid    = ls_e071_key-pgmid
            object   = ls_e071_key-object
            obj_name = ls_e071_key-obj_name ] ).
          INSERT ls_e071_key INTO TABLE lt_e071_key_new.
        ENDIF.
      ENDLOOP.

      IF lt_e071_key_new IS NOT INITIAL
            AND excluding IS NOT INITIAL.

        SELECT pgmid, object, obj_name, devclass
            FROM tadir
            FOR ALL ENTRIES IN @lt_e071_key_new
            WHERE pgmid    = @lt_e071_key_new-pgmid
              AND object   = @lt_e071_key_new-object
              AND obj_name = @lt_e071_key_new-obj_name(40)
            INTO TABLE @tadir_lines.

        IF sy-subrc = 0.

          DATA devclasses TYPE TABLE OF tadir-devclass.

          devclasses = VALUE #(
              FOR GROUPS <devclass> OF <tadir_line> IN tadir_lines
              GROUP BY <tadir_line>-devclass
              WITHOUT MEMBERS
              ( <devclass> ) ).

          SELECT devclass, dlvunit
              FROM tdevc
              FOR ALL ENTRIES IN @devclasses
              WHERE devclass = @devclasses-table_line
              INTO TABLE @tdevc_lines.

          IF sy-subrc = 0.

            LOOP AT lt_e071_key_new REFERENCE INTO DATA(e071_key_new).
              ASSIGN tadir_lines[
                        pgmid    = e071_key_new->pgmid
                        object   = e071_key_new->object
                        obj_name = e071_key_new->obj_name ]
                    TO FIELD-SYMBOL(<tadir_line2>).
              CHECK sy-subrc = 0.
              ASSIGN tdevc_lines[ devclass = <tadir_line2>-devclass ] TO FIELD-SYMBOL(<tdevc_line>).
              CHECK sy-subrc = 0.
              IF <tdevc_line>-dlvunit IN excluding-software_components.
                DELETE lt_e071_key_new USING KEY loop_key.
              ENDIF.
            ENDLOOP.

          ENDIF.
        ENDIF.
      ENDIF.

      INSERT LINES OF lt_e071_key_new INTO TABLE lt_e071_key.

    ENDLOOP.

  ENDMETHOD.



  METHOD tablstruc_field.

    DATA: ls_e071_key TYPE type_s_e071_objkey.

    IF is_dd03p-fieldname = '.APPEND_DU'.
      " .APPEND_DU indicates a recurse structure, PRECFIELD is empty, ignore it

    ELSEIF is_dd03p-fieldname = '.APPEND'
          OR is_dd03p-fieldname(6) = '.INCLU'.
      " .INCLU-*** indicates include with suffix *** for components
      ls_e071_key-pgmid    = 'R3TR'.
      ls_e071_key-object   = 'TABL'.
      ls_e071_key-obj_name = is_dd03p-precfield.
      IF is_dd03p-precfield CP 'CI_*'
            OR is_dd03p-precfield CP 'SI_*'.
        " activation won't fail if the CI_* include doesn't exist
        collect( soft_or_hard = 'S' y = ls_e071_key ).
*        mac_collect is_e071_key 'S' '' ls_e071_key ''.
      ELSE.
        " activation will fail if other includes don't exist
        collect( soft_or_hard = 'H' y = ls_e071_key ).
*        mac_collect is_e071_key 'H' '' ls_e071_key ''.
      ENDIF.
      " components with intern type / length have COMPTYPE = blank
      " (don't test ROLLNAME which sometimes is not blank)
    ELSEIF NOT is_dd03p-comptype IS INITIAL.
      ls_e071_key-pgmid    = 'R3TR'.
      DATA l_link TYPE c LENGTH 1.
      l_link = 'H'.
      CASE is_dd03p-comptype.
        WHEN 'E'.
          ls_e071_key-object   = 'DTEL'.
        WHEN 'S'.
          ls_e071_key-object   = 'TABL'.
        WHEN 'L'.
          ls_e071_key-object   = 'TTYP'.
        WHEN 'R'. "type ref to
          CASE is_dd03p-reftype.
            WHEN 'C'.
              ls_e071_key-object   = 'CLAS'.
            WHEN 'I'.
              ls_e071_key-object   = 'INTF'.
            WHEN 'E'.
              ls_e071_key-object   = 'DTEL'.
            WHEN 'S'. "structure or table
              ls_e071_key-object   = 'TABL'.
            WHEN 'L'.
              ls_e071_key-object   = 'TTYP'.
            WHEN space. "undefined
              DATA l_typename TYPE typename.
              DATA l_typekind TYPE ddtypekind.
              l_typename = is_dd03p-rollname.
              CALL FUNCTION 'DDIF_TYPEINFO_GET'
                EXPORTING
                  typename = l_typename
                IMPORTING
                  typekind = l_typekind.
              IF l_typekind IS NOT INITIAL.
                ls_e071_key-object   = l_typekind.
              ENDIF.
            WHEN 'B'.
              " reference to an internal data type, length, decimals -> IGNORE
            WHEN OTHERS.
              ASSERT is_dd03p-reftype = 'D' "data
                    OR is_dd03p-reftype = 'O'. "object
          ENDCASE.
        WHEN 'N'. "non existing structure (or inactive), i.e. CI_* or SI_*
          ls_e071_key-object = 'TABL'.
          l_link = 'S'. "include ci_* may not exist
        WHEN OTHERS.
          MESSAGE x001(00).
      ENDCASE.
      IF ls_e071_key-object IS NOT INITIAL.
        ls_e071_key-obj_name = is_dd03p-rollname.
        collect( soft_or_hard = l_link y = ls_e071_key ).
*        mac_collect is_e071_key l_link '' ls_e071_key ''.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD rs_progname_concatenate.

    " Copy of function module RS_PROGNAME_CONCATENATE and
    " + adaptation of OO part to avoid short dump if object type doesn't exist

    DATA: bool TYPE c LENGTH 1.

    CONSTANTS:
*      false LIKE bool VALUE 'F',
*      true  LIKE bool VALUE 'T',
*      yes   LIKE bool VALUE 'X',
          no    LIKE bool VALUE ' '.

    CONSTANTS:

      c_menu_progname_praefix_len      "Bereichsmenü
        TYPE i   VALUE 4,
      c_menu_progname_praefix(c_menu_progname_praefix_len)
        TYPE c   VALUE 'MENU',

      c_type_progname_praefix_len      "Type-Pools
        TYPE i   VALUE 3,
      c_type_progname_praefix(c_type_progname_praefix_len)
        TYPE c   VALUE '%_C',

      c_cntx_progname_praefix_len      "Contexte
        TYPE i   VALUE 10,
      c_cntx_progname_praefix(c_cntx_progname_praefix_len)
        TYPE c   VALUE 'CONTEXT_S_',

      c_mst_progname_praefix_len       "alte Tab-pflege
        TYPE i   VALUE 3,
      c_mst_progname_praefix(c_mst_progname_praefix_len)
        TYPE c   VALUE 'MST'.

    DATA: l_program   TYPE rs38l-include,
          l_namespace TYPE rs38l-namespace,
          l_area      TYPE rs38l-area,
          l_name      TYPE trdir-name,
*        l_obj_name  type sobj_name,
          l_ooref     TYPE REF TO if_oo_clif_incl_naming.

    l_program   = space.
    l_namespace = space.

* Programmnamen für Bestandteile von Funktionsgruppenobjekten
    CLEAR fugr_progname_group.
    CLEAR fugr_progname_include.
    IF NOT ( fugr_group IS INITIAL ).
      l_area = fugr_group.
      CALL FUNCTION 'FUNCTION_INCLUDE_CONCATENATE'
        EXPORTING
          include_number           = fugr_include_number
        IMPORTING
          include                  = fugr_progname_include
          uxx                      = fugr_progname_uxx
          top                      = fugr_progname_top
        CHANGING
          program                  = fugr_progname_group
          complete_area            = l_area
        EXCEPTIONS
          delimiter_wrong_position = 1
          OTHERS                   = 2.
      IF sy-subrc = 1.
        " TODO
*          MESSAGE e233
*            RAISING delimiter_error.
      ENDIF.
    ENDIF.


* Programmnamen für Bestandteile von log. Datenbanken
    CLEAR sldb_progname_db.
    CLEAR sldb_progname_sel.
    IF NOT ( sldb_name IS INITIAL ).
      CALL FUNCTION 'LDB_CONVERT_LDBNAME_2_DBPROG'
        EXPORTING
          ldb_name                  = sldb_name
          flag_existence_check      = no
        IMPORTING
          db_name                   = sldb_progname_db
          sel_name                  = sldb_progname_sel
        EXCEPTIONS
          wrong_position_of_slashes = 1
          OTHERS                    = 2.
      IF sy-subrc = 1.
        " TODO
*      message e233
*        raising delimiter_error.
      ENDIF.
    ENDIF.

* Programmnamen für Bereichsmenü
    IF NOT ( menu_name IS INITIAL ).
      l_name = menu_name.
      CALL FUNCTION 'RS_NAME_SPLIT_NAMESPACE'
        EXPORTING
          name_with_namespace    = l_name
        IMPORTING
          namespace              = l_namespace
          name_without_namespace = l_name
        EXCEPTIONS
          delimiter_error        = 1
          OTHERS                 = 2.
      IF sy-subrc = 1.
        " TODO
*      message e233
*        raising delimiter_error.
      ENDIF.
      CONCATENATE l_namespace
                  c_menu_progname_praefix
                  l_name
             INTO menu_progname.
    ELSE.
      CLEAR menu_progname.
    ENDIF.

* Programmnamen für Type-Pools
    IF NOT ( type_name IS INITIAL ).
      l_name = type_name.
      CALL FUNCTION 'RS_NAME_SPLIT_NAMESPACE'
        EXPORTING
          name_with_namespace    = l_name
        IMPORTING
          namespace              = l_namespace
          name_without_namespace = l_name
        EXCEPTIONS
          delimiter_error        = 1
          OTHERS                 = 2.
      IF sy-subrc = 1.
        " TODO
*      message e233
*        raising delimiter_error.
      ENDIF.
      CONCATENATE l_namespace
                  c_type_progname_praefix
                  l_name
             INTO type_progname.
    ELSE.
      CLEAR type_progname.
    ENDIF.

* Programmnamen für Contexte
    IF NOT ( cntx_name IS INITIAL ).
      l_name = cntx_name.
      CALL FUNCTION 'RS_NAME_SPLIT_NAMESPACE'
        EXPORTING
          name_with_namespace    = l_name
        IMPORTING
          namespace              = l_namespace
          name_without_namespace = l_name
        EXCEPTIONS
          delimiter_error        = 1
          OTHERS                 = 2.
      IF sy-subrc = 1.
        " TODO
*      message e233
*        raising delimiter_error.
      ENDIF.
      CONCATENATE l_namespace
                  c_cntx_progname_praefix
                  l_name
             INTO cntx_progname.
    ELSE.
      CLEAR cntx_progname.
    ENDIF.

* Programmnamen für (veraltete) Tabellenpflege
    IF NOT ( mst_name IS INITIAL ).
      l_name = mst_name.
      CALL FUNCTION 'RS_NAME_SPLIT_NAMESPACE'
        EXPORTING
          name_with_namespace    = l_name
        IMPORTING
          namespace              = l_namespace
          name_without_namespace = l_name
        EXCEPTIONS
          delimiter_error        = 1
          OTHERS                 = 2.
      IF sy-subrc = 1.
        " TODO
*      message e233
*        raising delimiter_error.
      ENDIF.
      CONCATENATE l_namespace
                  c_mst_progname_praefix
                  l_name
             INTO mst_progname.
    ELSE.
      CLEAR mst_progname.
    ENDIF.

* oo
* class
    CLEAR clas_progname.
    IF NOT clas_name IS INITIAL.
      " STANDARD CODE RAISES NO_OBJECTTYPE IF CLASS POOL DOESN'T EXIST
*    l_ooref = cl_oo_include_naming=>get_instance_by_name( clas_name ).
*    clas_progname = l_ooref->pool.
      TYPES: BEGIN OF ty_progname,
               main_part TYPE c LENGTH 30,
               extension TYPE c LENGTH 10,
             END OF ty_progname.
      clas_progname = translate(
            val  = CONV string( VALUE ty_progname( main_part = clas_name extension = 'CP' ) )
            from = ` `
            to   = '=' ).
    ENDIF.

* interface
    CLEAR intf_progname.
    IF NOT intf_name IS INITIAL.
      " STANDARD CODE RAISES NO_OBJECTTYPE IF INTERFACE POOL DOESN'T EXIST
*    l_ooref = cl_oo_include_naming=>get_instance_by_name( intf_name ).
*    intf_progname = l_ooref->pool.
      intf_progname = translate(
            val  = CONV string( VALUE ty_progname( main_part = intf_name extension = 'IP' ) )
            from = ` `
            to   = '=' ).
    ENDIF.

  ENDMETHOD.


  METHOD enhancement_implementation.

    DATA l_include TYPE include.
    DATA ls_e071_key TYPE type_s_e071_objkey.
    DATA lo_enh TYPE REF TO if_enh_tool.
    DATA lo_enh2 TYPE REF TO cl_enh_tool_hook_impl.
    DATA lo_enh99 TYPE REF TO cl_enh_tool_clif.
*        DATA l_enhname TYPE enhname.
    DATA lt_enhobj TYPE TABLE OF enhobj.
    DATA ls_enhobj TYPE enhobj.
    DATA l_tool_type TYPE enhtooltype.

*        l_enhname = is_e071_key-obj_name.

    lo_enh = cl_enh_factory=>get_enhancement( enhancement_id = name ).

    l_tool_type = lo_enh->get_tool( ).

    " For a BADI, we'll get ENHS, CLAS, INTF.
    " For a hook, we'll get the main program (FUGR or PROG or CLAS)
    " For a class enhancement, we'll get the class name (+ its interfaces + its
    "   super-classes up to top class) and the enhanced methods (new parameters)
    " For a function group enhancement, we'll get the function group and the enhanced
    "   function module (new parameters)
    SELECT * FROM enhobj INTO TABLE lt_enhobj WHERE enhname = name.
    LOOP AT lt_enhobj INTO ls_enhobj.
      ls_e071_key-pgmid    = 'R3TR'.
      ls_e071_key-object   = ls_enhobj-obj_type.
      ls_e071_key-obj_name = ls_enhobj-obj_name.
      collect( soft_or_hard = 'H' y = ls_e071_key ).
    ENDLOOP.

    CASE l_tool_type.
      WHEN 'HOOK_IMPL'.
        lo_enh2 ?= lo_enh.
        CALL METHOD lo_enh2->get_hook_impls_include
          IMPORTING
            include = l_include.
        CALL METHOD get_include_required_objects
          EXPORTING
            i_include = l_include.

      WHEN 'CLASENH'.
        lo_enh99 ?= lo_enh.
        CALL METHOD lo_enh99->get_include_name
          IMPORTING
            include = l_include.
        CALL METHOD get_include_required_objects
          EXPORTING
            i_include = l_include.

      WHEN 'FUGRENH'.
        lo_enh99 ?= lo_enh.
        CALL METHOD lo_enh99->get_include_name
          IMPORTING
            include = l_include.
        CALL METHOD get_include_required_objects
          EXPORTING
            i_include = l_include.
**** TODO
      WHEN 'BADI_IMPL'.
*            lo_enh5 ?= lo_enh.
*          DATA lt_impl TYPE enh_badi_impl_data_it.
*          DATA ls_impl TYPE LINE OF enh_badi_impl_data_it.
*          lt_impl = lo_enh5->get_implementations( ).
*          LOOP AT lt_impl INTO ls_impl.
**            ls_e071_key-pgmid    = 'R3TR'.
**            ls_e071_key-object   = 'SXCI'.
**            ls_e071_key-obj_name = ls_impl-impl_name.
**            mac_collect is_e071_key 'H' '' ls_e071_key ''.
*            ls_e071_key-pgmid    = 'R3TR'.
*            ls_e071_key-object   = 'INTF'.
*            CONCATENATE 'IF_EX_' ls_impl-impl_name INTO ls_e071_key-obj_name.
*            mac_collect is_e071_key 'H' '' ls_e071_key ''.
*            ls_e071_key-pgmid    = 'R3TR'.
*            ls_e071_key-object   = 'CLAS'.
*            ls_e071_key-obj_name = ls_impl-impl_class.
*            mac_collect is_e071_key 'H' '' ls_e071_key ''.
*          ENDLOOP.


      WHEN 'INTFENH'.
        " TODO
*            lo_enh6 ?= lo_enh.

      WHEN 'WDYENH'.
        " TODO
*            lo_enh7 ?= lo_enh.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.
