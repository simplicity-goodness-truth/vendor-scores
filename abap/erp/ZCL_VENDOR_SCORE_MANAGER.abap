class ZCL_VENDOR_SCORE_MANAGER definition
  public
  final
  create public .

public section.

  types:
    begin of st_attachments_list,
        file_name   type so_text255,
        obj_type    type char20,
        document_id type sofolenti1-doc_id,
        file_size   type so_doc_siz,
        upload_date type so_cre_dat,
        uploaded_by type so_cro_nam,
      end of st_attachments_list .
  types:
    tt_attachments_list type table of st_attachments_list .
  types:
    begin of st_vendors_list,
        lifnr type lifnr,
        name1 type name1_gp,
        score type char11,
      end of st_vendors_list .
  types:
    tt_vendors_list type table of st_vendors_list .
  types:
    begin of st_vendor_details,
        lifnr     type char10,
        name1     type char35,
        sperr     type char1,
        telf1     type char16,
        stcd1     type char16,
        land1     type char3,
        regio     type char3,
        ort01     type char25,
        stras     type char30,
        pstlz     type char10,
        smtp_addr type ad_smtpadr,
        zterm     type char4,
        zwels     type char10,
        akont     type char10,
        banks     type char3,
        bankl     type char15,
        bankn     type char18,
        koinh     type koinh_fi,
        textl     type char20,
        witht     type char2,
        cnt_name  type char35,
        butxt     type char25,
        bukrs     type char4,
        kraus     type kraus_cm,
      end of st_vendor_details .
  types:
    tt_vendor_details type table of st_vendor_details .
  types:
    begin of st_companies_list,
        bukrs type bukrs,
        butxt type butxt,
      end of st_companies_list .
  types:
    tt_companies_list type table of st_companies_list .

  class-methods SET_VENDOR_SCORE
    importing
      !IP_VENDOR type LIFNR
      !IP_SCORE type KRAUS_CM
    exporting
      !EP_LOCK_STATUS type CHAR1 .
  class-methods GET_COMPANIES_LIST
    exporting
      !ET_COMPANIES_LIST type TT_COMPANIES_LIST .
  class-methods GET_VENDOR_DETAILS
    importing
      !IP_VENDOR type LIFNR
      !IP_BUKRS type BUKRS
    exporting
      !ET_EXTENDED_DETAILS type TT_VENDOR_DETAILS .
  class-methods GET_VENDORS_LIST
    importing
      !IP_BUKRS type BUKRS
    exporting
      !ET_VENDORS_LIST type TT_VENDORS_LIST .
  class-methods UPLOAD_ATTACHMENT
    importing
      !IP_VENDOR type LIFNR
      !IP_FILE_CONTENT type XSTRING
      !IP_FILE_NAME type STRING
      !IP_FILE_EXTENSION type SOODK-OBJTP .
  class-methods GET_ATTACHMENT
    importing
      value(IP_DOCUMENT_ID) type SOFOLENTI1-DOC_ID optional
    exporting
      value(ET_HEX_CONTENT) type SOLIX_TAB
      !EV_MIME_TYPE type CHAR128
      !EV_DOC_SIZE type SO_DOC_SIZ .
  class-methods GET_ATTACHMENTS_LIST
    importing
      value(IP_VENDOR) type LIFNR optional
    exporting
      value(ET_ATTACHMENTS_LIST) type TT_ATTACHMENTS_LIST .
protected section.
private section.
ENDCLASS.



CLASS ZCL_VENDOR_SCORE_MANAGER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_VENDOR_SCORE_MANAGER=>GET_ATTACHMENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_DOCUMENT_ID                 TYPE        SOFOLENTI1-DOC_ID(optional)
* | [<---] ET_HEX_CONTENT                 TYPE        SOLIX_TAB
* | [<---] EV_MIME_TYPE                   TYPE        CHAR128
* | [<---] EV_DOC_SIZE                    TYPE        SO_DOC_SIZ
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_attachment.


    data ls_document         type sofolenti1.

    data lv_extension type char20.
    data lv_mime_type type w3conttype.

    call function 'SO_DOCUMENT_READ_API1'
      exporting
        document_id                = ip_document_id
      importing
        document_data              = ls_document
      tables
        contents_hex               = et_hex_content
      exceptions
        document_id_not_exist      = 1
        operation_no_authorization = 2
        x_error                    = 3
        others                     = 4.

    lv_extension = ls_document-obj_type.
    condense lv_extension.

    call function 'SDOK_MIMETYPE_GET'
      exporting
        extension = lv_extension
      importing
        mimetype  = ev_mime_type.


     ev_doc_size = ls_document-doc_size.
*
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_VENDOR_SCORE_MANAGER=>GET_ATTACHMENTS_LIST
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_VENDOR                      TYPE        LIFNR(optional)
* | [<---] ET_ATTACHMENTS_LIST            TYPE        TT_ATTACHMENTS_LIST
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_attachments_list.

    data: ls_lpor             type sibflporb,
          ls_relst            type obl_s_relt,
          lt_relst            type obl_t_relt,
          lt_links            type obl_t_link,
          lo_root             type ref to cx_root,
          lt_header           type table of solisti1,
          lv_document_id      type sofolenti1-doc_id,
          wa_attachments_list type st_attachments_list,
          ls_document         type sofolenti1.

    ls_lpor-instid = ip_vendor.
    ls_lpor-typeid = 'LFA1'.
    ls_lpor-catid = 'BO'.

    ls_relst-sign  = 'I'.
    ls_relst-option = 'EQ'.
    ls_relst-low   = 'ATTA'.
    append ls_relst to lt_relst.

    " Read document links

    try.
        call method cl_binary_relation=>read_links
          exporting
            is_object           = ls_lpor
            it_relation_options = lt_relst
          importing
            et_links            = lt_links.

      catch cx_obl_internal_error into lo_root.
        return.

      catch cx_obl_model_error into lo_root.
        return.

    endtry.

    loop at lt_links assigning field-symbol(<ls_links>).

      lv_document_id = <ls_links>-instid_b.

      call function 'SO_DOCUMENT_READ_API1'
        exporting
          document_id                = lv_document_id
        importing
          document_data              = ls_document
        exceptions
          document_id_not_exist      = 1
          operation_no_authorization = 2
          x_error                    = 3
          others                     = 4.

      if ls_document is not initial.

       " concatenate ls_document-obj_descr '.' ls_document-obj_type into  wa_attachments_list-file_name.

        wa_attachments_list-file_name = ls_document-obj_descr.

        wa_attachments_list-document_id = lv_document_id.
        wa_attachments_list-obj_type = ls_document-obj_type.
        wa_attachments_list-file_size = ls_document-doc_size.
        wa_attachments_list-upload_date = ls_document-creat_date.
        wa_attachments_list-uploaded_by = ls_document-creat_name.

        append  wa_attachments_list to  et_attachments_list.

      endif. " if lt_header IS NOT INITIAL

    endloop.




  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_VENDOR_SCORE_MANAGER=>GET_COMPANIES_LIST
* +-------------------------------------------------------------------------------------------------+
* | [<---] ET_COMPANIES_LIST              TYPE        TT_COMPANIES_LIST
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method GET_COMPANIES_LIST.

     select bukrs butxt
        from t001
        into corresponding fields of table et_companies_list
        where xprod = 'X'.


  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_VENDOR_SCORE_MANAGER=>GET_VENDORS_LIST
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_BUKRS                       TYPE        BUKRS
* | [<---] ET_VENDORS_LIST                TYPE        TT_VENDORS_LIST
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_vendors_list.


    data lt_lfb1 type standard table of lfb1.
    data ls_lfb1 like line of lt_lfb1.

    data lt_lfa1 type standard table of lfa1.
    data ls_lfa1 type lfa1.

    data ls_vendors_list like line of et_vendors_list.

*    select o~lifnr o~name1 o~kraus
*       from lfa1 as o
*        INNER JOIN lfb1 as c on o~lifnr = c~lifnr
*       into table et_vendors_list where c~bukrs = ip_bukrs.

    select lifnr
          from lfb1
          into corresponding fields of table lt_lfb1 where bukrs = ip_bukrs and loevm <> 'X' and sperr <> 'X'.

    "if vendor is not locked for all companies or marked for deletion and is confirmed

    loop at lt_lfb1 into ls_lfb1.

      select single lifnr name1 sperr loevm sperq sperm confs kraus from lfa1 into corresponding fields of ls_lfa1 where lifnr = ls_lfb1-lifnr.

      if ls_lfa1-sperr <> 'X' and ls_lfa1-loevm <> 'X'
        and ls_lfa1-sperq <> 'X' and  ls_lfa1-sperm <> 'X'
          and ls_lfa1-confs <> '1'.

        ls_vendors_list-name1 = ls_lfa1-name1.
        ls_vendors_list-lifnr = ls_lfa1-lifnr.
        ls_vendors_list-score = ls_lfa1-kraus.

        append ls_vendors_list to et_vendors_list.

        clear ls_vendors_list.

      endif.
    endloop.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_VENDOR_SCORE_MANAGER=>GET_VENDOR_DETAILS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_VENDOR                      TYPE        LIFNR
* | [--->] IP_BUKRS                       TYPE        BUKRS
* | [<---] ET_EXTENDED_DETAILS            TYPE        TT_VENDOR_DETAILS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_vendor_details.

    data lt_lfa1 type standard table of lfa1.
    data ls_lfa1 type lfa1.
    field-symbols <ls_lfa1> like line of lt_lfa1.
    data lv_zahls type dzahls.
    data lv_tax_country type qland.

    data ls_extended_details type st_vendor_details.

    select * from lfa1 into table lt_lfa1
          up to 1 rows
      where lifnr = ip_vendor.

    loop at lt_lfa1 assigning <ls_lfa1>.

      ls_extended_details-lifnr = <ls_lfa1>-lifnr.
      ls_extended_details-name1 = <ls_lfa1>-name1.
      ls_extended_details-land1 = <ls_lfa1>-land1.
      ls_extended_details-regio = <ls_lfa1>-regio.
      ls_extended_details-ort01 = <ls_lfa1>-ort01.
      ls_extended_details-stras = <ls_lfa1>-stras.
      ls_extended_details-pstlz = <ls_lfa1>-pstlz.
      ls_extended_details-telf1 = <ls_lfa1>-telf1.
      ls_extended_details-stcd1 = <ls_lfa1>-stcd1.
      ls_extended_details-kraus = <ls_lfa1>-kraus.

      " Vendor email address

      select single smtp_addr into ls_extended_details-smtp_addr
        from adr6 where  addrnumber = <ls_lfa1>-adrnr.

      " Reconciliation account and payment details

      select single akont zwels zterm zahls qland sperr  into (ls_extended_details-akont, ls_extended_details-zwels, ls_extended_details-zterm,
        lv_zahls, lv_tax_country, ls_extended_details-sperr)
        from lfb1 where lifnr = <ls_lfa1>-lifnr and bukrs = ip_bukrs.

      " Bank details

      select single banks bankl bankn koinh into (ls_extended_details-banks, ls_extended_details-bankl, ls_extended_details-bankn, ls_extended_details-koinh)
        from lfbk where lifnr = <ls_lfa1>-lifnr.

      " Payment block text
      select single textl into ls_extended_details-textl
         from t008t where zahls = lv_zahls and spras = sy-langu.

      " Withhold tax type

      select single witht into ls_extended_details-witht
          from t059p where land1 = lv_tax_country.

      " Contact person

      select single name1 into ls_extended_details-cnt_name
          from knvk where lifnr = <ls_lfa1>-lifnr.

      " Company code and name

      if ( ip_bukrs is not initial ).

        select single butxt into ls_extended_details-butxt
            from t001 where bukrs = ip_bukrs.

      endif.

      ls_extended_details-bukrs = ip_bukrs.

      append ls_extended_details to et_extended_details.

    endloop. "  loop at lt_lfa1 assigning <ls_lfa1>



  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_VENDOR_SCORE_MANAGER=>SET_VENDOR_SCORE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_VENDOR                      TYPE        LIFNR
* | [--->] IP_SCORE                       TYPE        KRAUS_CM
* | [<---] EP_LOCK_STATUS                 TYPE        CHAR1
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method set_vendor_score.

    data lt_enqueue type table of seqg3.
    data lv_enq_args type eqegraarg.
    field-symbols <ls_enqueue> like line of lt_enqueue.


    concatenate sy-mandt ip_vendor into lv_enq_args.

    condense lv_enq_args.

    ep_lock_status = ''.

    " Checking exclusive lock

    call function 'ENQUEUE_READ'
      exporting
        gclient               = sy-mandt
        gname                 = 'LFA1'
        garg                  = lv_enq_args
        guname                = '*'
      tables
        enq                   = lt_enqueue
      exceptions
        communication_failure = 1
        system_failure        = 2
        others                = 3.

    loop at lt_enqueue assigning <ls_enqueue>.

      if ( <ls_enqueue>-gmode = 'E' ).

        ep_lock_status = 'L'.

      endif. " if ( <ls_enqueue>-gmode = 'E' )

    endloop. "  LOOP AT lt_enqueue ASSIGNING FIELD-SYMBOL(<ls_enqueue>).

    if ( ep_lock_status = '' ).

      update lfa1 set kraus  = ip_score where lifnr = ip_vendor.

    endif.  "if ( ep_lock_status = '' )


  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_VENDOR_SCORE_MANAGER=>UPLOAD_ATTACHMENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_VENDOR                      TYPE        LIFNR
* | [--->] IP_FILE_CONTENT                TYPE        XSTRING
* | [--->] IP_FILE_NAME                   TYPE        STRING
* | [--->] IP_FILE_EXTENSION              TYPE        SOODK-OBJTP
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method upload_attachment.

    data lv_xstring type xstring.
    data g_folderid type soodk.

    call function 'SO_FOLDER_ROOT_ID_GET'
      exporting
        region    = 'B'
      importing
        folder_id = g_folderid.

    data g_docdata type sodocchgi1.
    data lv_file_extension type soodk-objtp.

    data: git_objhdr    type table of solisti1,
          wa_git_objhdr type solisti1,
          git_hexcont   type table of solix,
          lt_contents   type solix_tab,
          g_docinfo     type sofolenti1.

    data lo_converter type ref to cl_bcs_convert.

    create object lo_converter.
    data: lv_row_len  type i,
          lv_filesize type i.

    data: g_bizojb     type borident,
          g_attachment type borident.


    call method cl_bcs_convert=>xstring_to_solix
      exporting
        iv_xstring = ip_file_content
      receiving
        et_solix   = lt_contents.

    describe table lt_contents.

    lv_row_len  = sy-tfill.

    lv_filesize = xstrlen( ip_file_content  ).

    g_docdata-obj_name  = ip_file_name.
    g_docdata-obj_descr = ip_file_name.
    g_docdata-obj_langu = sy-langu.
    g_docdata-doc_size = lv_filesize.

    concatenate '&SO_FILENAME=' ip_file_name into wa_git_objhdr-line.
    append wa_git_objhdr to git_objhdr.
    clear: wa_git_objhdr.
    wa_git_objhdr-line = '&SO_FORMAT=BIN'.
    append wa_git_objhdr to git_objhdr.

    lv_file_extension = ip_file_extension .
    translate lv_file_extension to upper case.

    call function 'SO_DOCUMENT_INSERT_API1'
      exporting
        folder_id                  = g_folderid
        document_data              = g_docdata
        document_type              = lv_file_extension
      importing
        document_info              = g_docinfo
      tables
        object_header              = git_objhdr
        contents_hex               = lt_contents
      exceptions
        folder_not_exist           = 1
        document_type_not_exist    = 2
        operation_no_authorization = 3
        parameter_error            = 4
        x_error                    = 5
        enqueue_error              = 6
        others                     = 7.

    if sy-subrc = 0.

      g_bizojb-objtype = 'LFA1'.
      g_bizojb-objkey  = ip_vendor.

      g_attachment-objkey  = g_docinfo-doc_id.
      g_attachment-objtype = 'MESSAGE'.

      call function 'BINARY_RELATION_CREATE'
        exporting
          obj_rolea    = g_bizojb
          obj_roleb    = g_attachment
          relationtype = 'ATTA'.

      if sy-subrc = 0.

        commit work.

      endif. " IF sy-subrc = 0.



    endif.  " IF sy-subrc = 0.



  endmethod.
ENDCLASS.
