CLASS zcl_envysis DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES ty_e071_objkey TYPE seok_trkey .
    TYPES:
      soft_or_hard   TYPE c LENGTH 1 .
    TYPES:
      BEGIN OF ty_e071_rel,
        soft_or_hard TYPE soft_or_hard,
        subobject    TYPE ty_e071_objkey,
      END OF ty_e071_rel .
    TYPES:
      tt_e071_rel TYPE STANDARD TABLE OF ty_e071_rel WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_e071_rel2,
        object       TYPE ty_e071_objkey,
        soft_or_hard TYPE soft_or_hard,
        subobject    TYPE ty_e071_objkey,
      END OF ty_e071_rel2 .
    TYPES:
      tt_e071_rel2 TYPE STANDARD TABLE OF ty_e071_rel2 WITH EMPTY KEY .
    TYPES:
      type_spad(5) TYPE c .
    TYPES:
      BEGIN OF ty_wbobj_key,
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
      END OF ty_wbobj_key .
    TYPES:
      BEGIN OF ty_excluding,
        software_components TYPE RANGE OF dlvunit,
      END OF ty_excluding .
    TYPES:
      ty_packages TYPE RANGE OF tdevc-devclass .
    TYPES:
      BEGIN OF ty_tadir,
        pgmid    TYPE tadir-pgmid,
        object   TYPE tadir-object,
        obj_name TYPE tadir-obj_name,
        devclass TYPE tadir-devclass,
      END OF ty_tadir .
    TYPES:
      ty_tadir_lines TYPE HASHED TABLE OF ty_tadir WITH UNIQUE KEY pgmid object obj_name .


    METHODS constructor.

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter is_e071_key | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter et_e071_key | <p class="shorttext synchronized" lang="en"></p>
    "! @raising zcx_envysis | <p class="shorttext synchronized" lang="en"></p>
    METHODS get_used_objects_first_level
      IMPORTING
        !is_e071_key TYPE ty_e071_objkey
      EXPORTING
        !et_e071_key TYPE tt_e071_rel
      RAISING
        zcx_envysis .

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter is_e071_key | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter levels | 0 = all levels <p class="shorttext synchronized" lang="en"></p>
    "! @parameter et_e071_rel2 | <p class="shorttext synchronized" lang="en"></p>
    "! @raising zcx_envysis | <p class="shorttext synchronized" lang="en"></p>
    METHODS get_used_objects_multi_levels
      IMPORTING
        !is_e071_key  TYPE ty_e071_objkey
        !excluding    TYPE ty_excluding
        !levels       TYPE i DEFAULT 0
      EXPORTING
        !et_e071_rel2 TYPE tt_e071_rel2
      RAISING
        zcx_envysis .

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter packages | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter excluding | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter levels | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter all | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter tadir_lines_2 | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter objects_outside_packages | X
    "! @raising zcx_envysis | <p class="shorttext synchronized" lang="en"></p>
    METHODS get_package_used_objects
      IMPORTING
        !packages      TYPE ty_packages
        !excluding     TYPE ty_excluding OPTIONAL
        !levels        TYPE i DEFAULT 0
      EXPORTING
        !all           TYPE tt_e071_rel2
        !tadir_lines_2 TYPE ty_tadir_lines
        !objects_outside_packages TYPE ty_tadir_lines
      RAISING
        zcx_envysis .

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA gt_e071_key TYPE tt_e071_rel.
    DATA doc TYPE REF TO lif_doc.

    METHODS build_e071_key
      IMPORTING
        is_wbobj_key TYPE  ty_wbobj_key
      CHANGING
        es_e071_key  TYPE  seok_trkey.

    METHODS get_include_required_objects
      IMPORTING
        i_include TYPE progname.

    METHODS get_devc_required_objects
      IMPORTING
        i_package_name TYPE devclass.

    METHODS tablstruc_field
      IMPORTING
        is_dd03l    TYPE lif_doc=>ty_dd03l
        is_e071_key TYPE ty_e071_objkey.

    METHODS collect
      IMPORTING
        soft_or_hard TYPE soft_or_hard
        y            TYPE ty_e071_objkey.  "subobject

    METHODS enhancement_implementation
      IMPORTING
        name TYPE enhname
      RAISING
        zcx_envysis.

    METHODS get_objects_left_to_analyze
      IMPORTING
        i_excluding            TYPE ty_excluding
        it_e071_key            TYPE lif_doc=>tt_e071_key
        it_e071_rel2           TYPE tt_e071_rel2
      RETURNING
        VALUE(rt_e071_key_new) TYPE lif_doc=>tt_e071_key_new.

ENDCLASS.



CLASS zcl_envysis IMPLEMENTATION.


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


  METHOD collect.

    DATA ls_e071_key TYPE ty_e071_rel.

    ls_e071_key-soft_or_hard = soft_or_hard.
    ls_e071_key-subobject    = y.
    COLLECT ls_e071_key INTO gt_e071_key.

*  ENDMETHOD.
*
*
*  METHOD ddif_typeinfo_get.
*    DATA l_typename TYPE typename.
*    DATA l_typekind TYPE ddtypekind.
*
*    CLEAR e_object.
*
*    l_typename = i_tabname.
*    CALL FUNCTION 'DDIF_TYPEINFO_GET'
*      EXPORTING
*        typename = l_typename
*      IMPORTING
*        typekind = l_typekind.
*    e_object = l_typekind.

  ENDMETHOD.


  METHOD enhancement_implementation.

    DATA: l_include   TYPE include,
          ls_e071_key TYPE ty_e071_objkey,
          lo_enh      TYPE REF TO if_enh_tool,
          lo_enh_hook TYPE REF TO cl_enh_tool_hook_impl,
          lo_enh_clif TYPE REF TO cl_enh_tool_clif,
          lo_enh_fugr TYPE REF TO cl_enh_tool_fugr,
          lt_enhobj   TYPE TABLE OF enhobj,
          ls_enhobj   TYPE enhobj,
          l_tool_type TYPE enhtooltype.

    TRY.
        lo_enh = cl_enh_factory=>get_enhancement( enhancement_id = name ).
      CATCH cx_enh_io_error
          cx_enh_permission_denied
          cx_enh_canceled
          cx_enh_internal_error
          cx_enh_is_locked
          cx_enh_is_not_modifiable INTO DATA(lx).
        RAISE EXCEPTION TYPE zcx_envysis EXPORTING previous = lx.
    ENDTRY.

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

        lo_enh_hook ?= lo_enh.
        CALL METHOD lo_enh_hook->get_hook_impls_include
          IMPORTING
            include = l_include.
        CALL METHOD get_include_required_objects
          EXPORTING
            i_include = l_include.

      WHEN 'CLASENH'
        OR 'INTFENH'.

        lo_enh_clif ?= lo_enh.
        CALL METHOD lo_enh_clif->get_include_name
          IMPORTING
            include = l_include.
        CALL METHOD get_include_required_objects
          EXPORTING
            i_include = l_include.

      WHEN 'FUGRENH'.

        lo_enh_fugr ?= lo_enh.
        lo_enh_fugr->get_include_name(
          IMPORTING
            include = l_include ).
        get_include_required_objects( l_include ).

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

      WHEN 'WDYENH'.
        " TODO
*            lo_enh7 ?= lo_enh.
    ENDCASE.

  ENDMETHOD.


  METHOD get_devc_required_objects.

    DATA: ls_e071_key TYPE ty_e071_objkey.

    DATA(result) = doc->get_objects_needed_by_package( i_package_name ).

    LOOP AT result-sub_packages INTO DATA(sub_package).
      ls_e071_key-pgmid    = 'R3TR'.
      ls_e071_key-object   = 'DEVC'.
      ls_e071_key-obj_name = sub_package.
      collect( soft_or_hard = 'S' y = ls_e071_key ).
    ENDLOOP.

    ls_e071_key-pgmid    = 'R3TR'.
    ls_e071_key-object   = 'DEVC'.
    ls_e071_key-obj_name = result-super_package.
    collect( soft_or_hard = 'H' y = ls_e071_key ).

    LOOP AT result-interfaces INTO DATA(interface).
      ls_e071_key-pgmid    = 'R3TR'.
      ls_e071_key-object   = 'PINF'.
      ls_e071_key-obj_name = interface.
      collect( soft_or_hard = 'S' y = ls_e071_key ).
    ENDLOOP.

  ENDMETHOD.


  METHOD get_include_required_objects.

    DATA:
      ls_e071_key       TYPE ty_e071_objkey,
      lt_incl           TYPE TABLE OF rseuinc,
      lt_cross          TYPE TABLE OF cross,
      l_progname        TYPE syrepid,
      l_object          TYPE e071-object,
      ls_wbobj_key      TYPE ty_wbobj_key,
      is_e071_key       TYPE ty_e071_objkey,
      lt_wbcrossgt      TYPE TABLE OF wbcrossgt,
      ls_wbcrossgt      TYPE wbcrossgt,
      l_ddictype        TYPE wbcrossgt-name,
      l_tablstruc_field TYPE wbcrossgt-name.
    FIELD-SYMBOLS:
      <ls_incl>  TYPE rseuinc,
      <ls_cross> TYPE cross.

    l_progname = i_include.


    lt_incl = doc->rs_get_all_includes( l_progname ).

    " For each include, get all objects it is using.
    LOOP AT lt_incl ASSIGNING <ls_incl>.

      " Determine the type of the include
      DATA(progname_split_result) = doc->rs_progname_split( <ls_incl>-master ).
      IF progname_split_result-class_is_name = 'X'.
        " The include refers to a class or interface
        "   (section or class frame program)
        DATA lo_clif TYPE REF TO if_oo_clif_incl_naming.
        lo_clif = doc->get_oo_info_of_include( <ls_incl> ).
        ls_e071_key-pgmid    = 'R3TR'.
        ls_e071_key-object   = lo_clif->transport_key-object."CLAS or INTF
        ls_e071_key-obj_name = lo_clif->transport_key-obj_name.
        collect( soft_or_hard = 'H' y = ls_e071_key ).
      ELSEIF progname_split_result-class_method_name IS NOT INITIAL.
        " The include refers to a class method
        ls_wbobj_key-object = 'METH'.
        ls_wbobj_key-s_meth-class_name = progname_split_result-class_name.
        ls_wbobj_key-s_meth-method_name = progname_split_result-class_method_name.
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

    lt_wbcrossgt = doc->select_wbcrossgt( lt_incl ).

    LOOP AT lt_wbcrossgt INTO ls_wbcrossgt.
      SPLIT ls_wbcrossgt-name AT '\' INTO l_ddictype l_tablstruc_field.
      " Get DDIC object type
      IF l_tablstruc_field IS INITIAL.
        " TY:DDicTypeOrObjectType
        DATA l_tabname TYPE tabname.
        l_tabname = l_ddictype.
        doc->ddif_typeinfo_get(
          EXPORTING
            typename = l_tabname
          IMPORTING
            typekind  = l_object ).
        IF l_object IS NOT INITIAL.
          ls_e071_key-pgmid    = 'R3TR'.
          ls_e071_key-object   = l_object.
          ls_e071_key-obj_name = l_ddictype.
          collect( soft_or_hard = 'H' y = ls_e071_key ).
        ENDIF.
      ELSE.
        " TY:DDicType\TY:Component
        l_tablstruc_field = l_tablstruc_field+3. "remove TY:
        DATA(ls_dd03l) = doc->select_dd03l(
              tabname   = l_ddictype
              fieldname = l_tablstruc_field ).
        IF ls_dd03l IS NOT INITIAL.
          CALL METHOD tablstruc_field
            EXPORTING
              is_dd03l    = ls_dd03l
              is_e071_key = is_e071_key.
        ENDIF.
      ENDIF.
      " Note: we don't want to know if data/type comes from a type-pool
      "   because it's a little bit complex (DA and TY); instead, we use the
      "   type-pools declarations referenced in CROSS table (see below)
    ENDLOOP.

    " Note: cross-name = "?" for objects dynamically called.
    lt_cross = doc->select_cross( lt_incl ).

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
          doc->ddif_typeinfo_get(
            EXPORTING
              typename = l_typename
            IMPORTING
              typekind  = l_typekind
              gotstate  = l_gotstate ).
*          CALL FUNCTION 'DDIF_TYPEINFO_GET'
*            EXPORTING
*              typename = l_typename
*            IMPORTING
*              typekind = l_typekind
*              gotstate = l_gotstate.
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


  METHOD get_objects_left_to_analyze.

    TYPES: BEGIN OF ty_e071_to_tadir,
             e071_objkey  TYPE ty_e071_objkey,
             tadir_objkey TYPE ty_e071_objkey,
           END OF ty_e071_to_tadir,
           tt_e071_to_tadir TYPE STANDARD TABLE OF ty_e071_to_tadir WITH DEFAULT KEY.
    DATA: tdevc_lines  TYPE lif_doc=>ty_tdevc_lines,
          tadir_lines  TYPE ty_tadir_lines,
          e071_key_new TYPE REF TO zcl_envysis=>ty_e071_objkey,
          l_include    TYPE progname,
          l_fugr_group TYPE rs38l_area,
          tadir_objkey TYPE zcl_envysis=>ty_e071_objkey.


    rt_e071_key_new = VALUE #( ).
    LOOP AT it_e071_rel2 REFERENCE INTO DATA(e071_rel)
          WHERE soft_or_hard = 'H'.
      DATA(ls_e071_key) = VALUE ty_e071_objkey(
          pgmid    = e071_rel->subobject-pgmid
          object   = e071_rel->subobject-object
          obj_name = e071_rel->subobject-obj_name ).
      IF NOT line_exists( it_e071_key[
          pgmid    = ls_e071_key-pgmid
          object   = ls_e071_key-object
          obj_name = ls_e071_key-obj_name ] ).
        INSERT ls_e071_key INTO TABLE rt_e071_key_new.
      ENDIF.
    ENDLOOP.

    IF rt_e071_key_new IS NOT INITIAL
          AND i_excluding IS NOT INITIAL.

      " All R3TR PROG <include> which are part of function groups (LKKBLF01 is part of KKBL),
      " must be translated to R3TR FUGR <function-group> to find them in TADIR.
      DATA(lt_e071_key_new) = rt_e071_key_new.
      DATA(lt_e071_to_tadir) = VALUE tt_e071_to_tadir( ).
      LOOP AT lt_e071_key_new REFERENCE INTO e071_key_new.

        IF e071_key_new->pgmid = 'R3TR'
          AND e071_key_new->object = 'PROG'
          AND e071_key_new->obj_name(1) = 'L'.

          " Determine the name of its function group
          l_include = e071_key_new->obj_name.
          DATA(progname_split_result) = doc->rs_progname_split( l_include ).
          IF progname_split_result IS NOT INITIAL.
            tadir_objkey = VALUE ty_e071_objkey(
                pgmid    = 'R3TR'
                object   = 'FUGR'
                obj_name = progname_split_result-fugr_group ).
            COLLECT tadir_objkey INTO lt_e071_key_new.
            COLLECT VALUE ty_e071_to_tadir(
                e071_objkey  = e071_key_new->*
                tadir_objkey = tadir_objkey
                ) INTO lt_e071_to_tadir.
            DELETE lt_e071_key_new USING KEY loop_key.
          ENDIF.

        ELSEIF e071_key_new->pgmid = 'LIMU'.

          doc->tr_check_type(
            EXPORTING
              wi_e071  = CORRESPONDING #( e071_key_new->* )
            IMPORTING
              we_tadir = DATA(tadir) ).
          IF tadir IS NOT INITIAL.
            tadir_objkey = CORRESPONDING #( tadir ).
            COLLECT tadir_objkey INTO lt_e071_key_new.
            COLLECT VALUE ty_e071_to_tadir(
                e071_objkey  = e071_key_new->*
                tadir_objkey = tadir_objkey
                ) INTO lt_e071_to_tadir.
            DELETE lt_e071_key_new USING KEY loop_key.
          ENDIF.
        ENDIF.

      ENDLOOP.

      tadir_lines = doc->select_tadir_2( lt_e071_key_new ).

      IF tadir_lines IS NOT INITIAL.

        DATA devclasses TYPE TABLE OF tadir-devclass.
        DATA: tadir_line2 TYPE REF TO zcl_envysis=>ty_tadir.

        devclasses = VALUE #(
            FOR GROUPS <devclass> OF <tadir_line> IN tadir_lines
            GROUP BY <tadir_line>-devclass
            WITHOUT MEMBERS
            ( <devclass> ) ).

        tdevc_lines = doc->select_tdevc( devclasses ).

        IF tdevc_lines IS NOT INITIAL.

          LOOP AT rt_e071_key_new REFERENCE INTO e071_key_new.
            ASSIGN lt_e071_to_tadir[ e071_objkey = e071_key_new->* ]
                  TO FIELD-SYMBOL(<e071_to_tadir>).
            IF sy-subrc <> 0.
              tadir_line2 = REF #(  tadir_lines[
                        pgmid    = e071_key_new->pgmid
                        object   = e071_key_new->object
                        obj_name = e071_key_new->obj_name ] OPTIONAL ).
            ELSE.
              tadir_line2 = REF #( tadir_lines[
                        pgmid    = <e071_to_tadir>-tadir_objkey-pgmid
                        object   = <e071_to_tadir>-tadir_objkey-object
                        obj_name = <e071_to_tadir>-tadir_objkey-obj_name ] OPTIONAL ).
            ENDIF.
            CHECK tadir_line2 IS BOUND.
            ASSIGN tdevc_lines[ devclass = tadir_line2->devclass ] TO FIELD-SYMBOL(<tdevc_line>).
            CHECK sy-subrc = 0.
            IF <tdevc_line>-dlvunit IN i_excluding-software_components.
              DELETE rt_e071_key_new USING KEY loop_key.
            ENDIF.
          ENDLOOP.

        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_package_used_objects.

    DATA(lt_e071_key) = doc->select_tadir_1( packages ).

    LOOP AT lt_e071_key REFERENCE INTO DATA(e071_key).

      get_used_objects_multi_levels(
        EXPORTING
          is_e071_key  = e071_key->*
          excluding    = excluding
          levels       = levels
        IMPORTING
          et_e071_rel2 = DATA(used_objects) ).

*      DATA(objects_left) = get_objects_left_to_analyze(
*        EXPORTING
*          i_excluding     = excluding
*          it_e071_key     = lt_e071_key
*          it_e071_rel2    = used_objects ).

      INSERT LINES OF used_objects INTO TABLE all.

    ENDLOOP.

    SORT all BY table_line.
    DELETE ADJACENT DUPLICATES FROM all COMPARING table_line.

    " Determine the object directory entries of all used objects
    tadir_lines_2 = doc->select_tadir_2( it_e071_key_new = VALUE #(
          FOR <used_object> IN all WHERE ( subobject-object <> 'DEVC' )
          ( pgmid    = <used_object>-subobject-pgmid
            object   = <used_object>-subobject-object
            obj_name = <used_object>-subobject-obj_name(40) ) ) ).

  ENDMETHOD.


  METHOD get_used_objects_first_level.

    DATA:
      ls_e071_key  TYPE ty_e071_objkey,
      ls_wbobj_key TYPE ty_wbobj_key,
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

    et_e071_key = VALUE #( ).
    gt_e071_key = VALUE #( ).

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
    doc->tr_check_type(
      EXPORTING
        wi_e071  = ls_e071
      IMPORTING
        we_tadir = ls_tadir ).

    IF ls_tadir-object <> 'DEVC'.
      " get the package of the object
      DATA(devclass) = doc->select_tadir_3( VALUE #( pgmid     = ls_tadir-pgmid
                                                     object    = ls_tadir-object
                                                     obj_name  = ls_tadir-obj_name ) ).
      IF devclass IS NOT INITIAL.
        ls_e071_key-pgmid    = 'R3TR'.
        ls_e071_key-object   = 'DEVC'.
        ls_e071_key-obj_name = devclass.
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

    IF 0 = 1.
*    CASE ls_e071_key-object.
*
*      WHEN 'DEVC'.
*        "---------------------
*        " DEVELOPMENT CLASS
*        "---------------------
*        " Only the super-package is considered a "used object".
*        " NB 1: The sub-packages should not be considered as "used objects",
*        "   otherwise a sub-package A
*        "   having a super-package B, having a super-package C, would lead to
*        "   analyze all objects below the super-package C at all levels.
*        " NB 2: the objects contained in a package should not be
*        "   considered as "used objects", otherwise a used object of sub-package A
*        "   having a super-package B, having a super-package C, would lead to
*        "   analyze all objects below the super-package C at all levels.
*        DATA(tdevc_lines) = doc->select_tdevc( devclasses = VALUE #( ( CONV #( is_e071_key-obj_name ) ) ) ).
*        DATA(parentcl) = VALUE #( tdevc_lines[ devclass = CONV #( is_e071_key-obj_name ) ]-parentcl OPTIONAL ).
**        SELECT SINGLE parentcl
**            FROM tdevc
**            WHERE devclass = @is_e071_key-obj_name
**              AND parentcl <> @space
**            INTO @DATA(parentcl).
*        IF parentcl IS NOT INITIAL.
*          ls_e071_key-pgmid    = 'R3TR'.
*          ls_e071_key-object   = 'DEVC'.
*          ls_e071_key-obj_name = parentcl.
*          collect( soft_or_hard = 'H' y = ls_e071_key ).
*        ENDIF.
**        DATA l_package_name TYPE devclass.
**        l_package_name = is_e071_key-obj_name.
**        CALL METHOD get_devc_required_objects
**          EXPORTING
**            i_package_name = l_package_name.
*
*
*      WHEN 'TRAN'.
*        "---------------------
*        " transaction code
*        "---------------------
*
*        DATA l_tcode TYPE tcode.
*
*        l_tcode = is_e071_key-obj_name.
*
*        DATA lo_transaction TYPE REF TO zcl_tcode.
*
*
*        DATA(tcode) = doc->get_tcode_required_objects( l_tcode ).
*
*        CASE tcode-type.
*          WHEN 'O'.
*            IF tcode-s_object-local_class = 'X'.
*              ls_e071_key-pgmid    = 'R3TR'.
*              ls_e071_key-object   = 'PROG'.
*              ls_e071_key-obj_name = tcode-s_object-program_name.
*              collect( soft_or_hard = 'H' y = ls_e071_key ).
*            ELSE.
*              ls_e071_key-pgmid   = 'LIMU'.
*              ls_wbobj_key-object = 'METH'.
*              ls_wbobj_key-s_meth-method_name = tcode-s_object-method_name.
*              ls_wbobj_key-s_meth-class_name = tcode-s_object-global_class_name.
*              CALL METHOD build_e071_key
*                EXPORTING
*                  is_wbobj_key = ls_wbobj_key
*                CHANGING
*                  es_e071_key  = ls_e071_key.
*              collect( soft_or_hard = 'H' y = ls_e071_key ).
*            ENDIF.
*            IF NOT tcode-s_object-auth_object IS INITIAL.
*              ls_e071_key-pgmid    = 'R3TR'.
*              ls_e071_key-object   = 'SUSO'.
*              ls_e071_key-obj_name = tcode-s_object-auth_object.
*              collect( soft_or_hard = 'H' y = ls_e071_key ).
*            ENDIF.
*          WHEN 'D'.
*            ls_e071_key-pgmid = 'LIMU'.
*            ls_wbobj_key-object = 'DYNP'.
*            ls_wbobj_key-s_dynp-program_name = tcode-s_dialog-program_name.
*            ls_wbobj_key-s_dynp-screen_number = tcode-s_dialog-screen_number.
*            CALL METHOD build_e071_key
*              EXPORTING
*                is_wbobj_key = ls_wbobj_key
*              CHANGING
*                es_e071_key  = ls_e071_key.
*            collect( soft_or_hard = 'H' y = ls_e071_key ).
*            IF NOT tcode-s_dialog-auth_object IS INITIAL.
*              ls_e071_key-pgmid    = 'R3TR'.
*              ls_e071_key-object   = 'SUSO'.
*              ls_e071_key-obj_name = tcode-s_dialog-auth_object.
*              collect( soft_or_hard = 'H' y = ls_e071_key ).
*            ENDIF.
*          WHEN 'P'.
*            IF NOT tcode-s_parameter-called_tcode IS INITIAL.
*              ls_e071_key-pgmid    = 'R3TR'.
*              ls_e071_key-object   = 'TRAN'.
*              ls_e071_key-obj_name = tcode-s_parameter-called_tcode.
*              collect( soft_or_hard = 'H' y = ls_e071_key ).
*            ENDIF.
*            IF NOT tcode-s_parameter-program_name IS INITIAL.
*              ls_e071_key-pgmid   = 'LIMU'.
*              ls_wbobj_key-object = 'DYNP'.
*              ls_wbobj_key-s_dynp-program_name = tcode-s_parameter-program_name.
*              ls_wbobj_key-s_dynp-screen_number = tcode-s_parameter-screen_number.
*              CALL METHOD build_e071_key
*                EXPORTING
*                  is_wbobj_key = ls_wbobj_key
*                CHANGING
*                  es_e071_key  = ls_e071_key.
*              collect( soft_or_hard = 'H' y = ls_e071_key ).
*            ENDIF.
*          WHEN 'V'.
*            ls_e071_key-pgmid    = 'R3TR'.
*            ls_e071_key-object   = 'TRAN'.
*            ls_e071_key-obj_name = tcode-s_variant-called_tcode.
*            collect( soft_or_hard = 'H' y = ls_e071_key ).
*            IF NOT tcode-s_variant-transac_variant IS INITIAL.
*              ls_e071_key-object   = 'STVI'.
*              ls_e071_key-obj_name = tcode-s_variant-transac_variant.
*              collect( soft_or_hard = 'H' y = ls_e071_key ).
*            ENDIF.
*          WHEN 'R'.
*            ls_e071_key-pgmid    = 'R3TR'.
*            ls_e071_key-object   = 'PROG'.
*            ls_e071_key-obj_name = tcode-s_report-program_name.
*            collect( soft_or_hard = 'H' y = ls_e071_key ).
*            IF NOT tcode-s_report-auth_object IS INITIAL.
*              ls_e071_key-pgmid    = 'R3TR'.
*              ls_e071_key-object   = 'SUSO'.
*              ls_e071_key-obj_name = tcode-s_report-auth_object.
*              collect( soft_or_hard = 'H' y = ls_e071_key ).
*            ENDIF.
*            IF NOT tcode-s_report-program_variant IS INITIAL.
*              ls_e071_key-pgmid   = 'LIMU'.
*              " TODO : could be also VARX (&CUS...)
*              ls_wbobj_key-object = 'VARI'.
*              ls_wbobj_key-s_vari-program_name = tcode-s_report-program_name.
*              ls_wbobj_key-s_vari-variant_name = tcode-s_report-program_variant.
*              CALL METHOD build_e071_key
*                EXPORTING
*                  is_wbobj_key = ls_wbobj_key
*                CHANGING
*                  es_e071_key  = ls_e071_key.
*              collect( soft_or_hard = 'H' y = ls_e071_key ).
*            ENDIF.
*        ENDCASE.
*
*      WHEN 'ENHO'.
*        "---------------------
*        " ENHANCEMENT Implementation
*        "---------------------
*        enhancement_implementation( name = CONV enhname( is_e071_key-obj_name ) ).
*
*      WHEN 'PROG' OR 'REPS' OR 'REPO'.
*        "---------------------
*        " PROGRAM
*        "---------------------
*        CASE ls_e071_key-object.
*          WHEN 'REPS' OR 'REPO'.
*            l_subc = doc->select_trdir( CONV #( is_e071_key-obj_name ) ).
*            CASE l_subc.
*              WHEN '1' OR 'M' OR 'F' OR 'S' OR 'K'.
*                ls_e071_key-pgmid    = 'LIMU'.
*                ls_e071_key-object   = 'REPT'.
*                ls_e071_key-obj_name = is_e071_key-obj_name.
*                collect( soft_or_hard = 'H' y = ls_e071_key ).
*            ENDCASE.
*        ENDCASE.
*        l_include = is_e071_key-obj_name.
*        CALL METHOD get_include_required_objects
*          EXPORTING
*            i_include = l_include.
*
*      WHEN 'INTF' OR 'INTD'.
*        "---------------------
*        " interface pool
*        "---------------------
*        DATA l_interface_name TYPE seoclass-clsname.
*        l_interface_name = is_e071_key-obj_name.
*        doc->rs_progname_concatenate(
*          EXPORTING
*            intf_name       = l_interface_name
*          IMPORTING
*            intf_progname   = l_include ).
*        CALL METHOD get_include_required_objects
*          EXPORTING
*            i_include = l_include.
*
*      WHEN 'CLAS'.
*        "---------------------
*        " class pool
*        "---------------------
*        DATA l_class_name TYPE seoclass-clsname.
*        l_class_name = is_e071_key-obj_name.
*        doc->rs_progname_concatenate(
*          EXPORTING
*            clas_name       = l_class_name
*          IMPORTING
*            clas_progname   = l_include ).
*        CALL METHOD get_include_required_objects
*          EXPORTING
*            i_include = l_include.
*
*      WHEN 'METH'.
*        "---------------------
*        " method
*        "---------------------
*        ls_wbobj_key-s_meth = is_e071_key-obj_name.
*        cl_oo_classname_service=>get_method_include(
*          EXPORTING
*            mtdkey                = VALUE seocpdkey(
*                clsname = ls_wbobj_key-s_meth-class_name
*                cpdname = ls_wbobj_key-s_meth-method_name )
*            with_enhancements     = 'X'
*            with_alias_resolution = 'X'
*          RECEIVING
*            result                = l_include
*          EXCEPTIONS
*            class_not_existing    = 1
*            method_not_existing   = 2
*            OTHERS                = 3 ).
*        IF sy-subrc = 0.
*          CALL METHOD get_include_required_objects
*            EXPORTING
*              i_include = l_include.
*        ENDIF.
*
*      WHEN 'FUNC'.
*        "---------------------
*        " function
*        "---------------------
*        DATA l_funcname TYPE rs38l-name.
*        l_funcname = ls_e071_key-object.
*        l_include = doc->function_exists( l_funcname ).
*        IF l_include IS NOT INITIAL.
*          CALL METHOD get_include_required_objects
*            EXPORTING
*              i_include = l_include.
*        ENDIF.
*
*      WHEN 'TYPE' OR 'TYPD'.
*        "---------------------
*        " TYPE-POOL
*        "---------------------
*        DATA l_type_name TYPE trdir-name.
*        l_type_name = is_e071_key-obj_name.
*        doc->rs_progname_concatenate(
*          EXPORTING
*            type_name       = l_type_name
*          IMPORTING
*            type_progname   = l_include ).
*        CALL METHOD get_include_required_objects
*          EXPORTING
*            i_include = l_include.
*
*      WHEN 'FUGT'.
*        "---------------------
*        " FUNCTION GROUP
*        "---------------------
*        ls_e071_key-pgmid    = 'R3TR'.
*        ls_e071_key-object   = 'FUGR'.
*        ls_e071_key-obj_name = is_e071_key-obj_name.
*        collect( soft_or_hard = 'H' y = ls_e071_key ).
*
*      WHEN 'FUGR'.
*        DATA l_function_group TYPE rs38l-area.
*        l_function_group = is_e071_key-obj_name.
*        doc->rs_progname_concatenate(
*          EXPORTING
*            fugr_group          = l_function_group
*          IMPORTING
*            fugr_progname_group = l_include ).
*        CALL METHOD get_include_required_objects
*          EXPORTING
*            i_include = l_include.
*
*      WHEN 'TABT'.
*        "---------------------
*        " TABLE or STRUCTURE
*        "---------------------
*        ls_e071_key-pgmid    = 'R3TR'.
*        ls_e071_key-object   = 'TABL'.
*        ls_e071_key-obj_name = is_e071_key-obj_name.
*        collect( soft_or_hard = 'H' y = ls_e071_key ).
*
*      WHEN 'TABL' OR 'TABD'.
*        DATA l_name TYPE ddobjname.
*        l_name = is_e071_key-obj_name.
*        doc->ddif_tabl_get(
*              EXPORTING
*                name = l_name
*              IMPORTING
*                et_dd03p = lt_dd03p
*                et_dd08v = lt_dd08v
*                et_dd35v = lt_dd35v ).
*        IF sy-subrc = 0.
*          " Fields : data elements, domaines, tables de contrôles
*          LOOP AT lt_dd03p ASSIGNING <ls_dd03p>.
*            tablstruc_field(
*                is_dd03l    = CORRESPONDING #( <ls_dd03p> )
*                is_e071_key = is_e071_key ).
*
*            IF NOT <ls_dd03p>-checktable IS INITIAL.
*              ls_e071_key-pgmid    = 'R3TR'.
*              ls_e071_key-object   = 'TABL'.
*              ls_e071_key-obj_name = <ls_dd03p>-checktable.
*              collect( soft_or_hard = 'H' y = ls_e071_key ).
*            ENDIF.
*          ENDLOOP.
*          " Foreign keys
*          LOOP AT lt_dd08v ASSIGNING <ls_dd08v>.
*            ls_e071_key-pgmid    = 'R3TR'.
*            ls_e071_key-object   = 'TABL'.
*            ls_e071_key-obj_name = <ls_dd08v>-checktable.
*            collect( soft_or_hard = 'H' y = ls_e071_key ).
*          ENDLOOP.
*          " Search helps
*          LOOP AT lt_dd35v ASSIGNING <ls_dd35v>.
*            IF NOT <ls_dd35v>-shlpname IS INITIAL.
*              ls_e071_key-pgmid    = 'R3TR'.
*              ls_e071_key-object   = 'SHLP'.
*              ls_e071_key-obj_name = <ls_dd35v>-shlpname.
*              collect( soft_or_hard = 'H' y = ls_e071_key ).
*            ENDIF.
*          ENDLOOP.
*        ENDIF.
*
*      WHEN 'VIEW' OR 'VIED'.
*        "---------------------
*        " VIEW
*        "---------------------
*        l_ddobjname = is_e071_key-obj_name.
*        lt_dd26v = doc->ddif_view_get( l_ddobjname ).
*        IF sy-subrc = 0.
*          LOOP AT lt_dd26v INTO ls_dd26v.
*            ls_e071_key-pgmid    = 'R3TR'.
*            ls_e071_key-object   = 'TABL'.
*            ls_e071_key-obj_name = ls_dd26v-tabname.
*            collect( soft_or_hard = 'H' y = ls_e071_key ).
*          ENDLOOP.
*        ENDIF.
*
*      WHEN 'TTYP' OR 'TTYD'.
*        "---------------------
*        " TABLE TYPE
*        "---------------------
*        l_ddobjname = is_e071_key-obj_name.
*        DATA ls_dd40v TYPE dd40v.
*        ls_dd40v = doc->ddif_ttyp_get( l_ddobjname ).
*        IF sy-subrc = 0.
*          ls_e071_key-pgmid    = 'R3TR'.
*          doc->ddif_typeinfo_get(
*            EXPORTING
*              typename = ls_dd40v-rowtype
*            IMPORTING
*              typekind  = ls_e071_key-object ).
*          ls_e071_key-obj_name = ls_dd40v-rowtype.
*          collect( soft_or_hard = 'H' y = ls_e071_key ).
*        ENDIF.
*
*      WHEN 'DTEL' OR 'DTED'.
*        "---------------------
*        " DATA ELEMENT
*        "---------------------
*        l_rollname = is_e071_key-obj_name.
*        DATA ls_tpara TYPE tpara.
*        doc->ddif_dtel_get(
*              EXPORTING
*                rollname = l_rollname
*              IMPORTING
*                dd04v = ls_dd04v
*                tpara = ls_tpara ).
*        IF sy-subrc = 0.
*          IF NOT ls_dd04v-domname IS INITIAL.
*            ls_e071_key-pgmid    = 'R3TR'.
*            ls_e071_key-object   = 'DOMA'.
*            ls_e071_key-obj_name = ls_dd04v-domname.
*            collect( soft_or_hard = 'H' y = ls_e071_key ).
*          ENDIF.
*          IF NOT ls_dd04v-shlpname IS INITIAL.
*            ls_e071_key-pgmid    = 'R3TR'.
*            ls_e071_key-object   = 'SHLP'.
*            ls_e071_key-obj_name = ls_dd04v-shlpname.
*            collect( soft_or_hard = 'H' y = ls_e071_key ).
*          ENDIF.
*          IF NOT ls_tpara-paramid IS INITIAL.
*            ls_e071_key-pgmid    = 'R3TR'.
*            ls_e071_key-object   = 'PARA'.
*            ls_e071_key-obj_name = ls_tpara-paramid.
*            collect( soft_or_hard = 'H' y = ls_e071_key ).
*          ENDIF.
*        ENDIF.
*
*      WHEN 'DOMA' OR 'DOMD'.
*        "---------------------
*        " DOMAIN
*        "---------------------
*        l_domname = is_e071_key-obj_name.
*        ls_dd01v = doc->ddif_doma_get( l_domname ).
*        IF sy-subrc = 0.
*          IF NOT ls_dd01v-entitytab IS INITIAL.
*            ls_e071_key-pgmid    = 'R3TR'.
*            ls_e071_key-object   = 'TABL'.
*            ls_e071_key-obj_name = ls_dd01v-entitytab.
*            collect( soft_or_hard = 'H' y = ls_e071_key ).
*          ENDIF.
*        ENDIF.
*
*      WHEN 'SHLP' OR 'SHLD'.
*        "---------------------
*        " SEARCH HELP
*        "---------------------
*        l_shlpname = is_e071_key-obj_name.
*        ls_dd30v = doc->ddif_shlp_get( l_shlpname ).
*        IF sy-subrc = 0.
*          IF NOT ls_dd30v-selmethod IS INITIAL.
*            doc->ddif_typeinfo_get(
*              EXPORTING
*                typename = ls_dd30v-selmethod
*              IMPORTING
*                typekind  = l_object ).
*            ls_e071_key-object   = l_object.
*            ls_e071_key-obj_name = ls_dd30v-selmethod.
*            collect( soft_or_hard = 'H' y = ls_e071_key ).
*          ENDIF.
*          IF NOT ls_dd30v-texttab IS INITIAL.
*            ls_e071_key-pgmid    = 'R3TR'.
*            ls_e071_key-object   = 'TABL'.
*            ls_e071_key-obj_name = ls_dd30v-texttab.
*            collect( soft_or_hard = 'H' y = ls_e071_key ).
*          ENDIF.
*        ENDIF.
*
*      WHEN 'ENQU'.
*        "---------------------
*        " LOCK OBJECT
*        "---------------------
*        l_lockobject = is_e071_key-obj_name.
*        ls_dd25v = doc->ddif_enqu_get( l_lockobject ).
*        IF sy-subrc = 0.
*          ls_e071_key-pgmid    = 'R3TR'.
*          ls_e071_key-object   = 'TABL'.
*          ls_e071_key-obj_name = ls_dd25v-roottab.
*          collect( soft_or_hard = 'H' y = ls_e071_key ).
*        ENDIF.
*
*      WHEN 'IOBC' "infoObject catalog
*        OR 'IOBJ' "infoObject
*        OR 'RSPC' "process chain
*        OR 'TRFN' "transformation
*        OR 'AREA' "infoArea
*        OR 'APCO' "Application Components
*        OR 'ODSO' "Data Store Object
*        OR 'ROUT' "ABAP code (routine)
*        OR 'TRCS'."infoSource
*
*        "---------------------
*        " BI objects
*        "---------------------
*        DATA lo_awb TYPE REF TO cl_rso_repository.
*        TYPE-POOLS: rs, rsoc.
*        DATA: l_collect_grouping TYPE rso_collect_grouping,
*              lt_object          TYPE rso_t_tlogo,
*              ls_object          TYPE rso_s_tlogo,
*              l_search_levels    TYPE i,
*              lth_association    TYPE rso_th_association.
*        FIELD-SYMBOLS: <ls_association> TYPE rso_s_association,
*                       <ls_object>      TYPE rso_s_tlogo_objref_proxy.
*
*
*        lo_awb = cl_rso_repository=>get_repository( ).
*        REFRESH lt_object.
*        ls_object-tlogo = is_e071_key-object.
*        ls_object-objnm = is_e071_key-obj_name.
*        APPEND ls_object TO lt_object.
*        l_collect_grouping = rsoc_c_collect_grouping-required."Only necessary objects
*        l_search_levels = 2.
*        REFRESH lth_association.
*        CALL METHOD lo_awb->transport_wizard
*          EXPORTING
*            i_t_object         = lt_object
*            i_collect_grouping = l_collect_grouping
*            i_search_level     = l_search_levels
*          IMPORTING
*            e_th_association   = lth_association
*          EXCEPTIONS
*            object_not_found   = 0.
*        IF sy-subrc = 0.
*          READ TABLE lth_association ASSIGNING <ls_association>
*                WITH TABLE KEY
*                  tlogo   = ls_object-tlogo
*                  objnm   = ls_object-objnm
*                  objvers = 'A'.
*          IF sy-subrc = 0.
*            LOOP AT <ls_association>-objects ASSIGNING <ls_object>.
*              ls_e071_key-pgmid    = 'R3TR'.
*              ls_e071_key-object   = <ls_object>-tlogo.
*              ls_e071_key-obj_name = <ls_object>-objnm.
*              collect( soft_or_hard = 'H' y = ls_e071_key ).
*            ENDLOOP.
*          ENDIF.
*        ENDIF.
*** ajouter tous les infoObjets qu'il contient
**        DATA lt_rsdiobciobj TYPE TABLE OF rsdiobciobj.
**        FIELD-SYMBOLS <ls_rsdiobciobj> TYPE rsdiobciobj.
**        SELECT * FROM rsdiobciobj INTO TABLE lt_rsdiobciobj
**              WHERE infoobjcat = is_e071_key-obj_name
**                AND objvers    = 'A'.
**        LOOP AT lt_rsdiobciobj ASSIGNING <ls_rsdiobciobj>.
**          ls_e071_key-pgmid    = 'R3TR'.
**          ls_e071_key-object   = 'IOBJ'.
**          ls_e071_key-obj_name = <ls_rsdiobciobj>-iobjnm.
**          mac_collect is_e071_key 'H' '' ls_e071_key ''.
**        ENDLOOP.
*
*        CASE is_e071_key-object.
*          WHEN 'RSPC'.
*            "---------------------
*            " Process chain
*            "---------------------
**            DATA lt_rspcchain TYPE TABLE OF rspcchain.
**            FIELD-SYMBOLS <ls_rspcchain> TYPE rspcchain.
**            SELECT * FROM rspcchain INTO TABLE lt_rspcchain
**                  WHERE chain_id = is_e071_key-obj_name
**                    AND objvers  = 'A'.
**            LOOP AT lt_rspcchain ASSIGNING <ls_rspcchain>.
**            ENDLOOP.
**READ TABLE lt_rspcchain ASSIGNING <ls_rspcchain>
**      WITH KEY
**        type = 'TRIGGER'.
**IF SY-subrc = 0.
**DO.
** <ls_rspcchain>-event IS NOT INITIAL.
**IF <ls_rspcchain>-next IS INITIAL.
**  EXIT.
**ENDIF.
**READ TABLE lt_rspcchain ASSIGNING <ls_rspcchain>
**      WITH KEY
**        type = 'TRIGGER'.
**IF SY-subrc <> 0.
**  EXIT.
**ENDIF.
**ENDDO.
*
*          WHEN 'TRFN'.
*            "---------------------
*            " transformation
*            "---------------------
*** générer les cas d'emploi du programme généré de la transformation
**        DATA ls_rstran TYPE rstran.
*** OBJ_name could be 034QILJDF4LOEOSUXEYPWMS7K6X9K6JU
**        SELECT SINGLE * FROM rstran INTO ls_rstran
**              WHERE tranid = is_e071_key-obj_name
**                AND objvers = 'A'.
**        IF sy-subrc = 0.
*** rstran-tranprog could be 3KHXIECTOI8OJA7D9NQTQZIWB
*** prog -> GP3KHXIECTOI8OJA7D9NQTQZIWB
**          SUBMIT saprseui
**                WITH repname = l_include
**                AND RETURN.
**          CONCATENATE 'GP' ls_rstran-tranprog INTO l_include.
**          ls_e071_key-pgmid    = 'R3TR'.
**          ls_e071_key-object   = 'PROG'.
**          ls_e071_key-obj_name = l_include.
**          mac_collect is_e071_key 'H' '' ls_e071_key ''.
**        ENDIF.
*        ENDCASE.
*
*    ENDCASE.
    ENDIF.

    "---------------------
    " ENVIRONMENT ANALYSIS
    "---------------------
    DATA:
      ls_envi_types TYPE envi_types,
      lt_envi       TYPE senvi_tab,
      l_deep        TYPE i,
      l_obj_type    TYPE seu_obj,
      l_objname     TYPE sobj_name.
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
        ls_e071_key-pgmid    = ls_ko100-pgmid.
        ls_e071_key-object   = <ls_envi>-type.
        ls_e071_key-obj_name = <ls_envi>-object.
        collect( soft_or_hard = 'H' y = ls_e071_key ).
      ENDIF.
    ENDLOOP.

    " The object for which the list of used objects requested must not be returned
    " otherwise there would be a recursive endless loop.
    DELETE gt_e071_key
        WHERE subobject-pgmid    = is_e071_key-pgmid
          AND subobject-object   = is_e071_key-object
          AND subobject-obj_name = is_e071_key-obj_name.

    et_e071_key = gt_e071_key.

  ENDMETHOD.


  METHOD get_used_objects_multi_levels.

    DATA: lt_e071_key     TYPE HASHED TABLE OF ty_e071_objkey WITH UNIQUE KEY pgmid object obj_name,
          lt_e071_key_new TYPE STANDARD TABLE OF ty_e071_objkey.

    et_e071_rel2 = VALUE #( ).

    lt_e071_key = VALUE #( ( is_e071_key ) ).

    LOOP AT lt_e071_key REFERENCE INTO DATA(e071_key).

      get_used_objects_first_level(
        EXPORTING
          is_e071_key = e071_key->*
        IMPORTING
          et_e071_key = DATA(lt_e071_rel) ).

      LOOP AT lt_e071_rel REFERENCE INTO DATA(e071_rel).
        APPEND VALUE ty_e071_rel2(
                object       = e071_key->*
                soft_or_hard = e071_rel->soft_or_hard
                subobject    = e071_rel->subobject )
            TO et_e071_rel2.
      ENDLOOP.

      lt_e071_key_new = get_objects_left_to_analyze(
            i_excluding  = excluding
            it_e071_key  = lt_e071_key
            it_e071_rel2 = CORRESPONDING #( lt_e071_rel ) ).

      INSERT LINES OF lt_e071_key_new INTO TABLE lt_e071_key.

    ENDLOOP.

  ENDMETHOD.


  METHOD tablstruc_field.

    DATA: ls_e071_key TYPE ty_e071_objkey,
          l_link      TYPE c LENGTH 1,
          l_typename  TYPE typename,
          l_typekind  TYPE ddtypekind.

    IF is_dd03l-fieldname = '.APPEND_DU'.
      " .APPEND_DU indicates a recurse structure, PRECFIELD is empty, ignore it

    ELSEIF is_dd03l-fieldname = '.APPEND'
          OR is_dd03l-fieldname(6) = '.INCLU'.
      " .INCLU-*** indicates include with suffix *** for components
      ls_e071_key-pgmid    = 'R3TR'.
      ls_e071_key-object   = 'TABL'.
      ls_e071_key-obj_name = is_dd03l-precfield.
      IF is_dd03l-precfield CP 'CI_*'
            OR is_dd03l-precfield CP 'SI_*'.
        " activation won't fail if the CI_* include doesn't exist
        collect( soft_or_hard = 'S' y = ls_e071_key ).
      ELSE.
        " activation will fail if other includes don't exist
        collect( soft_or_hard = 'H' y = ls_e071_key ).
      ENDIF.
      " components with intern type / length have COMPTYPE = blank
      " (don't test ROLLNAME which sometimes is not blank)
    ELSEIF NOT is_dd03l-comptype IS INITIAL.
      ls_e071_key-pgmid    = 'R3TR'.
      l_link = 'H'.
      CASE is_dd03l-comptype.
        WHEN 'E'.
          ls_e071_key-object   = 'DTEL'.
        WHEN 'S'.
          ls_e071_key-object   = 'TABL'.
        WHEN 'L'.
          ls_e071_key-object   = 'TTYP'.
        WHEN 'R'. "type ref to
          CASE is_dd03l-reftype.
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
              l_typename = is_dd03l-rollname.
              doc->ddif_typeinfo_get(
                EXPORTING
                  typename = l_typename
                IMPORTING
                  typekind = l_typekind ).
              IF l_typekind IS NOT INITIAL.
                ls_e071_key-object   = l_typekind.
              ENDIF.
            WHEN 'B'.
              " reference to an internal data type, length, decimals -> IGNORE
            WHEN OTHERS.
              ASSERT is_dd03l-reftype = 'D' "data
                    OR is_dd03l-reftype = 'O'. "object
          ENDCASE.
        WHEN 'N'. "non existing structure (or inactive), i.e. CI_* or SI_*
          ls_e071_key-object = 'TABL'.
          l_link = 'S'. "include ci_* may not exist
        WHEN OTHERS.
          MESSAGE x001(00).
      ENDCASE.
      IF ls_e071_key-object IS NOT INITIAL.
        ls_e071_key-obj_name = is_dd03l-rollname.
        collect( soft_or_hard = l_link y = ls_e071_key ).
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD constructor.

    doc = NEW lcl_doc( ).

  ENDMETHOD.

ENDCLASS.
