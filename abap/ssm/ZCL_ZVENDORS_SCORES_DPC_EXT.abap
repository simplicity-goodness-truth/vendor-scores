class ZCL_ZVENDORS_SCORES_DPC_EXT definition
  public
  inheriting from ZCL_ZVENDORS_SCORES_DPC
  create public .

public section.

  types:
    begin of ty_emails_tt,
             email type string,
           end of ty_emails_tt .
  types:
    tt_emails type table of ty_emails_tt .

  class-methods GET_EMAILS_BY_ORG_UNIT_BP
    importing
      !IV_BP type REALO
    exporting
      !ET_EMAILS type TT_EMAILS .
  class-methods SEND_SCORE_UPDATE_EMAILS
    importing
      !IV_TEXT_NAME type STRING
      !IV_SUBJECT_CODE type CHAR256
      !IV_COMPANY_NAME type CHAR25
      !IV_COMPANY_CODE type CHAR10
      !IV_VENDOR_CODE type CHAR10
      !IV_VENDOR_NAME type CHAR35
      !IV_SYSTEM type CHAR3
      !IV_SCORE type CHAR6
      !IV_SCORE_TEXT type CHAR100 .

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_STREAM
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_STREAM
    redefinition .
protected section.

  methods ATTACHMENTSSET_GET_ENTITYSET
    redefinition .
  methods COMPANYSET_GET_ENTITYSET
    redefinition .
  methods SCORERANKSSET_GET_ENTITYSET
    redefinition .
  methods SYSTEMSET_GET_ENTITYSET
    redefinition .
  methods VALIDATIONDOCUME_GET_ENTITYSET
    redefinition .
  methods VENDORSCORINGLOG_GET_ENTITYSET
    redefinition .
  methods VENDORSET_GET_ENTITY
    redefinition .
  methods VENDORSET_GET_ENTITYSET
    redefinition .
  methods VENDORSET_UPDATE_ENTITY
    redefinition .
private section.

  class-methods SEND_EMAIL
    importing
      !IV_SUBJECT type STRING
      !IV_RECEIVER type STRING
      !IV_MESSAGE type STRING .
ENDCLASS.



CLASS ZCL_ZVENDORS_SCORES_DPC_EXT IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ZVENDORS_SCORES_DPC_EXT->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_STREAM
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING(optional)
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING(optional)
* | [--->] IV_SOURCE_NAME                 TYPE        STRING(optional)
* | [--->] IS_MEDIA_RESOURCE              TYPE        TY_S_MEDIA_RESOURCE
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR(optional)
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH(optional)
* | [--->] IV_SLUG                        TYPE        STRING
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITY_C(optional)
* | [<---] ER_ENTITY                      TYPE REF TO DATA
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method /iwbep/if_mgw_appl_srv_runtime~create_stream.



    data lv_vendor_code type char10.
    data lv_system type c length 3.
    data lv_company type c length 10.


    data lv_file_name_full type string.
    data lv_extension type soodk-objtp.
    data lv_file_name type string.

    data lv_destination type char258.
    data lv_destination_param type char20.

    data lv_return_code type sy-subrc.

    "DATA lt_slug_params TYPE STANDARD TABLE OF ty_slug_params_tt.
    data lt_slug_params  type table of string.
    data wa_slug_params type string.

    data wa_zvnd_score_stage type zvnd_score_stage.
    "data lv_record_id type i.

    data lv_guid type crmt_clm_guid_c.

    data lv_delayed_mode type char6.

    field-symbols: <fs_header> type /iwbep/s_mgw_name_value_pair.

    data lt_zvnd_score_stage type table of zvnd_score_stage.


    " Getting order guid and filename from iv_slug parameter

*    lv_vendor_code = substring_before( val = iv_slug sub = '|' ).
*    lv_filename = substring_after( val = iv_slug sub = '|' ).

    split iv_slug at '|' into table lt_slug_params.

    read table lt_slug_params index 1 into wa_slug_params.

    if wa_slug_params is not initial.

      lv_system = wa_slug_params.

      clear wa_slug_params.

    endif. " IF wa_slug_params IS NOT INITIAL

    " Reading vendor

    read table lt_slug_params index 2 into wa_slug_params.

    if wa_slug_params is not initial.

      lv_vendor_code = wa_slug_params.
      clear wa_slug_params.

    endif. " IF wa_slug_params IS NOT INITIAL

    " Reading file name

    read table lt_slug_params index 3 into wa_slug_params.

    if wa_slug_params is not initial.

      lv_file_name_full = wa_slug_params.
      clear wa_slug_params.

    endif. " IF wa_slug_params IS NOT INITIAL

    if ( lv_vendor_code is initial ) or ( lv_system is initial )
      or ( lv_file_name_full is initial ).

      return.

    endif.

    " Reading company


    read table lt_slug_params index 4 into wa_slug_params.

    if wa_slug_params is not initial.

      lv_company = wa_slug_params.
      clear wa_slug_params.

    endif. " IF wa_slug_params IS NOT INITIAL


    " Reading GUID


    read table lt_slug_params index 5 into wa_slug_params.

    if wa_slug_params is not initial.

      lv_guid = wa_slug_params.
      clear wa_slug_params.

    endif. " IF wa_slug_params IS NOT INITIAL


    lv_extension = substring_after( val = lv_file_name_full sub = '.' ).
    lv_file_name = substring_before( val = lv_file_name_full sub = '.' ).


    " Getting full URL
    read table me->mr_request_details->technical_request-request_header assigning <fs_header> with key name = '~request_uri'.

    "Getting action from URL

    lv_delayed_mode = substring_after( val = <fs_header>-value sub = 'delayedMode=' ).
    condense lv_delayed_mode.


    if ( lv_extension is not initial ) and ( lv_file_name is not initial ).

      " Preparing records from staging area

*      select guid sys_id company vendor file_name extension into corresponding fields of table lt_zvnd_score_stage from zvnd_score_stage
*             where sys_id = lv_system
*             and company = lv_company
*             and vendor = lv_vendor_code .


      case lv_delayed_mode.

        when 'false'. " Vendor is not locked, so we can upload all documents

          " Preparing parameter name to select destination

          concatenate lv_system '_RFC_DESTINATION' into lv_destination_param.

          select single value from zvnd_score_setup into lv_destination
            where param = lv_destination_param.

          " Uploading attachment content to GOS

          if lv_destination is not initial.
            call function 'ZVENDOR_SCORE_MANAGER_EXT' destination lv_destination
              exporting
                ip_vendor         = lv_vendor_code
                ip_method         = 'UPLOAD_ATTACHMENT'
                "ip_file_name      = lv_file_name
                ip_file_name      = lv_file_name_full
                ip_file_extension = lv_extension
                ip_file_content   = is_media_resource-value
              importing
                ep_return_code    = lv_return_code.


            " If there are files in staging are we have to remove it

*            select * into corresponding fields of table lt_zvnd_score_stage from zvnd_score_stage
*              where sys_id = lv_system
*              and company = lv_company
*              and vendor = lv_vendor_code .

*            loop at lt_zvnd_score_stage assigning field-symbol(<ls_zvnd_score_stage>).
*
*              if ( <ls_zvnd_score_stage>-file_name = lv_file_name ) and ( <ls_zvnd_score_stage>-extension = lv_extension ).
*                delete from zvnd_score_stage where guid = <ls_zvnd_score_stage>-guid.
*              endif. " if ( <ls_zvnd_score_stage>-file_name = lv_file_name ) and ( <ls_zvnd_score_stage>-extension = lv_extension )
*
*            endloop. " loop at lt_ZVND_SCORE_STAGE ASSIGNING FIELD-SYMBOL(<ls_ZVND_SCORE_STAGE>)


          endif. "  if lv_destination is not initial

        when 'true'.  " Vendor is  locked, so we put documents to a staging table

*          read table lt_zvnd_score_stage into data(ls_zvnd_score_stage) with key file_name = lv_file_name extension = lv_extension.
*
*          if sy-subrc <> 0.


            wa_zvnd_score_stage-guid = lv_guid.
            wa_zvnd_score_stage-sys_id = lv_system.
            wa_zvnd_score_stage-company = lv_company.
            wa_zvnd_score_stage-vendor = lv_vendor_code.
            wa_zvnd_score_stage-update_time = sy-uzeit.
            wa_zvnd_score_stage-update_date = sy-datum.
            wa_zvnd_score_stage-media = is_media_resource-value.
            wa_zvnd_score_stage-file_name = lv_file_name.
            wa_zvnd_score_stage-extension = lv_extension.

            insert zvnd_score_stage from wa_zvnd_score_stage.

*          else.
*
*            update zvnd_score_stage set update_date = sy-datum  update_time = sy-uzeit
*                   where guid = ls_zvnd_score_stage-guid.
*
*
*          endif.



      endcase. " case lv_delayed_mode



    endif. " IF ( lv_extension IS NOT INITIAL ) and ( lv_filename IS NOT INITIAL )

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ZVENDORS_SCORES_DPC_EXT->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_STREAM
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING(optional)
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING(optional)
* | [--->] IV_SOURCE_NAME                 TYPE        STRING(optional)
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR(optional)
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH(optional)
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITY(optional)
* | [<---] ER_STREAM                      TYPE REF TO DATA
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_ENTITY_CNTXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method /iwbep/if_mgw_appl_srv_runtime~get_stream.


    data lv_vendor_code type char10.
    data lv_system type c length 3.

    data lv_destination type char258.
    data lv_destination_param type char20.
    data lv_return_code type sy-subrc.
    data lv_document_id type so_entryid.
    data lt_hex_content type solix_tab.
    data ls_stream  type ty_s_media_resource.
    data lv_length type i.
    data lv_lines_count type i.
    data ls_scenario_key_tab like line of it_key_tab.
    data lv_mime_type type w3conttype.
    data lv_length_char type char12.

    data lv_file_name type string.
    data ls_header  type ihttpnvp.
    data lv_extension type char20.

    constants: lc_content_dispo_str type string value 'Content-Disposition', "#EC NOTEXT
               lc_content_sec_str   type string value 'Content-Security-Policy', "#EC NOTEXT
               lc_x_content_sec_str type string value 'X-Content-Security-Policy'. "#EC NOTEXT

    read table it_key_tab into ls_scenario_key_tab with key name = 'VendorCode'.

    if ls_scenario_key_tab is not initial.

      lv_vendor_code = ls_scenario_key_tab-value.
      clear ls_scenario_key_tab.

    endif.

    clear ls_scenario_key_tab.

    read table it_key_tab into ls_scenario_key_tab with key name = 'System'.

    if ls_scenario_key_tab is not initial.

      lv_system = ls_scenario_key_tab-value.
      clear ls_scenario_key_tab.

    endif.


    read table it_key_tab into ls_scenario_key_tab with key name = 'documentId'.

    if ls_scenario_key_tab is not initial.

      lv_document_id = ls_scenario_key_tab-value.
      clear ls_scenario_key_tab.

    endif.


    read table it_key_tab into ls_scenario_key_tab with key name = 'fileName'.

    if ls_scenario_key_tab is not initial.

      lv_file_name = ls_scenario_key_tab-value.
      clear ls_scenario_key_tab.

    endif.



    if ( lv_vendor_code is initial ) or ( lv_system is initial )
      or ( lv_document_id is initial ).

      return.

    endif.

    " Preparing parameter name to select destination

    concatenate lv_system '_RFC_DESTINATION' into lv_destination_param.

    select single value from zvnd_score_setup into lv_destination
      where param = lv_destination_param.

    " Receiving attachment content from GOS

    if lv_destination is not initial.
      call function 'ZVENDOR_SCORE_MANAGER_EXT' destination lv_destination
        exporting
          ip_document_id = lv_document_id
          ip_method      = 'GET_ATTACHMENT'
        importing
          ep_return_code = lv_return_code
        " ev_mime_type   = lv_mime_type
          ev_doc_size    = lv_length_char
        tables
          et_hex_content = lt_hex_content.

      if lv_return_code = 0.

        "ls_stream-mime_type = lv_mime_type.

        lv_extension = substring_after( val = lv_file_name sub = '.' ).

        condense lv_extension.

        call function 'SDOK_MIMETYPE_GET'
          exporting
            extension = lv_extension
          importing
            mimetype  = lv_mime_type.

        ls_stream-mime_type = lv_mime_type.

        " Setting headers

        ls_header-name = lc_content_dispo_str.

        lv_file_name = escape( val = lv_file_name format = cl_abap_format=>e_url ).

        concatenate 'inline; filename='  lv_file_name into ls_header-value ##NO_TEXT.

        set_header( ls_header ).




        " Preparing file length

        lv_length = lv_length_char .

*    " Transforming binary file to xstring

        call function 'SCMS_BINARY_TO_XSTRING'
          exporting
            input_length = lv_length
          importing
            buffer       = ls_stream-value
          tables
            binary_tab   = lt_hex_content
          exceptions
            failed       = 1
            others       = 2.

        if sy-subrc = 0.

          copy_data_to_ref( exporting is_data = ls_stream
                              changing  cr_data = er_stream ).




        endif. "   if sy-subrc = 0

      endif. " if lv_return_code = 0

    endif. " if lv_destination is not initial

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZVENDORS_SCORES_DPC_EXT->ATTACHMENTSSET_GET_ENTITYSET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_FILTER_SELECT_OPTIONS       TYPE        /IWBEP/T_MGW_SELECT_OPTION
* | [--->] IS_PAGING                      TYPE        /IWBEP/S_MGW_PAGING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [--->] IT_ORDER                       TYPE        /IWBEP/T_MGW_SORTING_ORDER
* | [--->] IV_FILTER_STRING               TYPE        STRING
* | [--->] IV_SEARCH_STRING               TYPE        STRING
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET(optional)
* | [<---] ET_ENTITYSET                   TYPE        ZCL_ZVENDORS_SCORES_MPC=>TT_ATTACHMENTS
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method attachmentsset_get_entityset.

    data ls_entityset  like line of et_entityset.

    data lv_vendor_code type char10.
    data lv_system type c length 3.
    data ls_scenario_key_tab like line of it_key_tab.

    data lv_destination type char258.
    data lv_destination_param type char20.
    data lv_return_code type sy-subrc.

    types: begin of ty_attachments_list_tt,
             file_name   type so_text255,
             obj_type    type char20,
             document_id type so_entryid,
             file_size   type char12,
             upload_date type dats,
             uploaded_by type char12,
           end of ty_attachments_list_tt.

    data lt_attachments_list type standard table of ty_attachments_list_tt.

    data lv_extension type char20.
    data lv_mime_type type W3CONTTYPE.

     data lv_size_mb type p DECIMALS 3.
    data lv_size_byte type int4.

    " Preparing input parameters

    read table it_key_tab into ls_scenario_key_tab with key name = 'VendorCode'.

    if ls_scenario_key_tab is not initial.

      lv_vendor_code = ls_scenario_key_tab-value.

    endif. " if ls_scenario_key_tab is not initial.

    clear ls_scenario_key_tab.

    read table it_key_tab into ls_scenario_key_tab with key name = 'System'.

    if ls_scenario_key_tab is not initial.

      lv_system = ls_scenario_key_tab-value.

    endif. "  if ls_scenario_key_tab is not initial.

    if ( lv_vendor_code is initial ) or ( lv_system is initial ).
      return.
    endif.  "if ( lv_vendor_code IS INITIAL ) or ( lv_system IS INITIAL )

    condense lv_vendor_code.
    condense lv_system.

    " Preparing parameter name to select destination

    concatenate lv_system '_RFC_DESTINATION' into lv_destination_param.

    select single value from zvnd_score_setup into lv_destination
      where param = lv_destination_param.

    if lv_destination is not initial.
      call function 'ZVENDOR_SCORE_MANAGER_EXT' destination lv_destination
        exporting
          ip_vendor           = lv_vendor_code
          ip_method           = 'GET_ATTACHMENTS_LIST'
        importing
          ep_return_code      = lv_return_code
        tables
          et_attachments_list = lt_attachments_list.
    endif.

    if lt_attachments_list is not initial.

      loop at lt_attachments_list assigning field-symbol(<ls_attachments_list>).

        ls_entityset-vendorcode = lv_vendor_code.
        ls_entityset-system = lv_system.
        ls_entityset-filename = <ls_attachments_list>-file_name.
        ls_entityset-documentid = <ls_attachments_list>-document_id.
        ls_entityset-uploaddate = <ls_attachments_list>-upload_date.
        ls_entityset-username = <ls_attachments_list>-uploaded_by.

        " Transforming size in bytes to megabytes

        lv_size_byte = <ls_attachments_list>-file_size.
        lv_size_mb = ( lv_size_byte / 1024 ) / 1024.
        ls_entityset-filesize = lv_size_mb.

        " Preparing mime type

      "  lv_extension = <ls_attachments_list>-obj_type.


        lv_extension = substring_after( val = <ls_attachments_list>-file_name sub = '.' ).


        CONDENSE lv_extension.

        call function 'SDOK_MIMETYPE_GET'
          exporting
            extension = lv_extension
          importing
            mimetype  = lv_mime_type.

        if sy-subrc = 0.
          ls_entityset-mimetype = lv_mime_type.
        endif. "  if sy-subrc = 0

        append ls_entityset to et_entityset.

      endloop. " loop at lt_attachments_list ASSIGNING FIELD-SYMBOL(<ls_attachments_list>)

    endif. "  if lt_attachments_list is not initial




  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZVENDORS_SCORES_DPC_EXT->COMPANYSET_GET_ENTITYSET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_FILTER_SELECT_OPTIONS       TYPE        /IWBEP/T_MGW_SELECT_OPTION
* | [--->] IS_PAGING                      TYPE        /IWBEP/S_MGW_PAGING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [--->] IT_ORDER                       TYPE        /IWBEP/T_MGW_SORTING_ORDER
* | [--->] IV_FILTER_STRING               TYPE        STRING
* | [--->] IV_SEARCH_STRING               TYPE        STRING
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET(optional)
* | [<---] ET_ENTITYSET                   TYPE        ZCL_ZVENDORS_SCORES_MPC=>TT_COMPANY
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method companyset_get_entityset.

    data ls_entityset  like line of et_entityset.
    data lv_system type char3.

    data lv_destination type char258.
    data lv_destination_param type char20.
    data lv_return_code type sy-subrc.

    types: begin of ty_companies_list_tt,
             company_code type char4,
             company_name type char25,
           end of ty_companies_list_tt.

    data lt_companies_list type standard table of ty_companies_list_tt.

    " Enabling filtering
    data(it_filter_so) = io_tech_request_context->get_filter( )->get_filter_select_options( ).

    if line_exists( it_filter_so[ property = 'SYSTEMID' ] ).

      data(rg_system_so) = it_filter_so[ property = 'SYSTEMID' ]-select_options.

    endif.

    " Result can be returned only if system is selected

    if rg_system_so is initial.
      clear ls_entityset.
      append ls_entityset to et_entityset.

      return.

    endif.

    loop at rg_system_so assigning field-symbol(<rs_system_so>).
      lv_system = <rs_system_so>-low.
    endloop.

    " Preparing parameter name to select destination

    concatenate lv_system '_RFC_DESTINATION' into lv_destination_param.

    select single value from zvnd_score_setup into lv_destination
      where param = lv_destination_param.


    if lv_destination is not initial.
      call function 'ZVENDOR_SCORE_MANAGER_EXT' destination lv_destination
        exporting
          ip_method         = 'GET_COMPANIES_LIST'
        importing
          ep_return_code    = lv_return_code
        tables
          et_companies_list = lt_companies_list.

    endif. " if lv_destination is not initial

    if lv_return_code = 0.

      loop at lt_companies_list assigning field-symbol(<ls_companies_list>).

        ls_entityset-companycode = <ls_companies_list>-company_code.
        ls_entityset-companyname = <ls_companies_list>-company_name.
        ls_entityset-systemid = lv_system.

        append ls_entityset to et_entityset.

      endloop. " loop at lt_vendors_list ASSIGNING FIELD-SYMBOL(<ls_vendors_list>)

    endif. " if lv_return_code = 0


  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_ZVENDORS_SCORES_DPC_EXT=>GET_EMAILS_BY_ORG_UNIT_BP
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_BP                          TYPE        REALO
* | [<---] ET_EMAILS                      TYPE        TT_EMAILS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_emails_by_org_unit_bp.

    types: begin of ty_bp_tt,
             bp type bu_partner,
           end of ty_bp_tt.

    data lt_bp type standard table of ty_bp_tt.

    data ls_emails type ty_emails_tt.

    field-symbols <ls_bp> like line of lt_bp.

    data lv_employee_uname type uname.
    data: lv_addrnumber type ad_addrnum,
          lv_persnumber type ad_persnum.


    " Receiving BP for employees of L1 ogr unit

    select partner2 into table lt_bp from but050 where partner1 = iv_bp and reltyp = 'BUR010'.

    loop at lt_bp assigning <ls_bp>.

      call function 'CRM_ERMS_FIND_USER_FOR_BP'
        exporting
          ev_bupa_no = <ls_bp>-bp
        importing
          ev_user_id = lv_employee_uname.

      select single persnumber addrnumber into (lv_persnumber, lv_addrnumber)
          from usr21 where bname eq lv_employee_uname.

      if sy-subrc eq 0.

        select single smtp_addr from adr6 into ls_emails-email  where addrnumber = lv_addrnumber and persnumber = lv_persnumber.

        append ls_emails to et_emails .

      endif. "  if sy-subrc eq 0.

    endloop. " LOOP AT lt_bp ASSIGNING <ls_bp>


  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZVENDORS_SCORES_DPC_EXT->SCORERANKSSET_GET_ENTITYSET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_FILTER_SELECT_OPTIONS       TYPE        /IWBEP/T_MGW_SELECT_OPTION
* | [--->] IS_PAGING                      TYPE        /IWBEP/S_MGW_PAGING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [--->] IT_ORDER                       TYPE        /IWBEP/T_MGW_SORTING_ORDER
* | [--->] IV_FILTER_STRING               TYPE        STRING
* | [--->] IV_SEARCH_STRING               TYPE        STRING
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET(optional)
* | [<---] ET_ENTITYSET                   TYPE        ZCL_ZVENDORS_SCORES_MPC=>TT_SCORERANKS
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method scoreranksset_get_entityset.

    data ls_entityset  like line of et_entityset.


    types: begin of ty_parameter_ranks_tt,
             param  type char258,

           end of ty_parameter_ranks_tt.

    data lt_parameter_ranks type standard table of ty_parameter_ranks_tt.
    data lv_param_mask type char20.
    data lv_rank_char type char3.

    data lv_high_bound type char6.


    select param from zvnd_score_setup into corresponding fields of table lt_parameter_ranks where param like 'SCORE_TYPE_RANK_%'.

    SORT lt_parameter_ranks by param DESCENDING.

    lv_high_bound = '100'.

    loop at lt_parameter_ranks assigning field-symbol(<ls_parameter_ranks>).

      ls_entityset-rank = substring_after( val = <ls_parameter_ranks>-param sub = 'SCORE_TYPE_RANK_' ).

      select single value from zvnd_score_setup into  ls_entityset-scorevalue
        where param eq <ls_parameter_ranks>-param.


      lv_rank_char = ls_entityset-rank.
      condense lv_rank_char.

      concatenate 'SCORE_TYPE_NAME_' lv_rank_char into lv_param_mask.

      if lv_param_mask is not initial.

        select single value from zvnd_score_setup into  ls_entityset-rankname
         where param eq  lv_param_mask.

      endif. " IF lv_param_mask IS NOT INITIAL

      CONCATENATE ls_entityset-scorevalue '%' INTO ls_entityset-SCOREVALUERANGE.
      CONCATENATE ls_entityset-SCOREVALUERANGE '-' INTO ls_entityset-SCOREVALUERANGE SEPARATED BY SPACE.
      CONCATENATE ls_entityset-SCOREVALUERANGE lv_high_bound INTO ls_entityset-SCOREVALUERANGE SEPARATED BY SPACE.
      CONCATENATE ls_entityset-SCOREVALUERANGE '%' INTO ls_entityset-SCOREVALUERANGE.

      "CONCATENATE ls_entityset-scorevalue '%' '-' lv_high_bound '%' INTO ls_entityset-SCOREVALUERANGE.

      lv_high_bound = ls_entityset-scorevalue.


      append ls_entityset to et_entityset.


    endloop. " LOOP AT lt_parameter_ranks ASSIGNING FIELD-SYMBOL(<ls_parameter_ranks>)




    SORT et_entityset by rank DESCENDING.

*
*   select single value from ZVND_SCORE_SETUP into lv_score_threshold_char
*          where param eq 'SCORE_LOW'.



*        lv_score_threshold_low = lv_score_threshold_char.
*
*        select single value from zvendunlockparam into lv_score_threshold_char
*            where param eq 'SCORE_HIGH'.
*
*        lv_score_threshold_high = lv_score_threshold_char.


  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_ZVENDORS_SCORES_DPC_EXT=>SEND_EMAIL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SUBJECT                     TYPE        STRING
* | [--->] IV_RECEIVER                    TYPE        STRING
* | [--->] IV_MESSAGE                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method send_email.

    data ls_formfield type ihttpnvp.

    data: email          type ref to zcl_aba_mail_notification,
          lv_email       type so_recname,
          lv_send_result type c.


    if email is not initial.
      free email.
    endif.

    create object email
      exporting
        i_general_mail   = abap_true
        i_subject        = iv_subject
        i_header_subject = iv_subject.

    email->set_header_logo( ).


    move iv_receiver to lv_email.

    call method email->add_force_receiver exporting email = lv_email.


    email->set_sender( i_sender = 'no-reply@sonangol.co.ao' ).

    ls_formfield = value ihttpnvp(  name = 'message'
                                   value = iv_message  ).

    email->add_formfields( value ihttpnvp(  name = 'message'
                                   value = iv_message  ) ).

    email->set_it_email_message( i_is_bsp = abap_true ).

    email->send(
        exporting
          i_with_error_screen    = 'X'    " Flag geral
          i_set_request_atts     = 'E'    " Flag geral
        receiving
          r_result               = lv_send_result
        exceptions
          failed_to_send         = 1
          others                 = 2
      ).



  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_ZVENDORS_SCORES_DPC_EXT=>SEND_SCORE_UPDATE_EMAILS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TEXT_NAME                   TYPE        STRING
* | [--->] IV_SUBJECT_CODE                TYPE        CHAR256
* | [--->] IV_COMPANY_NAME                TYPE        CHAR25
* | [--->] IV_COMPANY_CODE                TYPE        CHAR10
* | [--->] IV_VENDOR_CODE                 TYPE        CHAR10
* | [--->] IV_VENDOR_NAME                 TYPE        CHAR35
* | [--->] IV_SYSTEM                      TYPE        CHAR3
* | [--->] IV_SCORE                       TYPE        CHAR6
* | [--->] IV_SCORE_TEXT                  TYPE        CHAR100
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method send_score_update_emails.

    data: lv_subject  type string,
          lv_mail_msg type string.

    data: lt_text_name  type  thead-tdname,
          t_line        type table of tline,
          l_line        type char255,
          lv_text_token type char256..

    data lv_bp_num type realo.

    types: begin of ty_emails_tt,
             email type string,
           end of ty_emails_tt.

    data lt_emails type standard table of ty_emails_tt.
    field-symbols <ls_emails> like line of lt_emails.

    " Getting subject

    select single value from zvnd_score_setup into lv_subject where param = iv_subject_code.

    " Getting text

    lt_text_name = iv_text_name.

    call function 'READ_TEXT'
      exporting
        client                  = sy-mandt
        id                      = 'ST'
        language                = 'P'
        name                    = lt_text_name
        object                  = 'TEXT'
      tables
        lines                   = t_line
      exceptions
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        others                  = 8.

    loop at t_line into l_line.

      concatenate '<strong>' iv_vendor_code into lv_text_token.
      concatenate lv_text_token iv_vendor_name into lv_text_token separated by space.
      concatenate lv_text_token '</strong>' into lv_text_token.

      condense lv_text_token.
      replace '&1' with lv_text_token into l_line.

      concatenate '<strong>' iv_company_code into lv_text_token.
      concatenate lv_text_token iv_company_name into lv_text_token separated by space.
      concatenate lv_text_token '</strong>' into lv_text_token.

      condense lv_text_token.
      replace '&2' with lv_text_token into l_line.

      concatenate '<strong>'  iv_system '</strong>' into lv_text_token.
      condense lv_text_token.
      replace '&3' with lv_text_token into l_line.


      concatenate '<strong>'  iv_score '</strong>' into lv_text_token.
      condense lv_text_token.
      replace '&4' with lv_text_token into l_line.


      concatenate '<strong>'  iv_score_text '</strong>' into lv_text_token.
      condense lv_text_token.
      replace '&5' with lv_text_token into l_line.


*      concatenate '<strong>' iv_score into lv_text_token.
*      concatenate lv_text_token iv_score_text into lv_text_token SEPARATED BY space.
*      concatenate lv_text_token '</strong>' into lv_text_token.
*
*      condense lv_text_token.
      replace '&4' with lv_text_token into l_line.

      concatenate lv_mail_msg l_line+2 into lv_mail_msg separated by space.

    endloop. "       loop at t_line into l_line.

    " Getting receivers

    select single value from zvnd_score_setup into lv_bp_num
          where param eq 'EMAIL_NOTIF_BP'.

    if ( lv_bp_num is not initial ).

      call method zcl_zvendors_scores_dpc_ext=>get_emails_by_org_unit_bp
        exporting
          iv_bp     = lv_bp_num
        importing
          et_emails = lt_emails.

      loop at lt_emails assigning <ls_emails>.

        call method zcl_zvendors_scores_dpc_ext=>send_email
          exporting
            iv_subject  = lv_subject
            iv_receiver = <ls_emails>-email
            iv_message  = lv_mail_msg.

      endloop. " LOOP AT lt_emails ASSIGNING <ls_emails>

    endif. " if lv_bp_num is not INITIAL

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZVENDORS_SCORES_DPC_EXT->SYSTEMSET_GET_ENTITYSET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_FILTER_SELECT_OPTIONS       TYPE        /IWBEP/T_MGW_SELECT_OPTION
* | [--->] IS_PAGING                      TYPE        /IWBEP/S_MGW_PAGING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [--->] IT_ORDER                       TYPE        /IWBEP/T_MGW_SORTING_ORDER
* | [--->] IV_FILTER_STRING               TYPE        STRING
* | [--->] IV_SEARCH_STRING               TYPE        STRING
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET(optional)
* | [<---] ET_ENTITYSET                   TYPE        ZCL_ZVENDORS_SCORES_MPC=>TT_SYSTEM
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method systemset_get_entityset.


    data ls_entityset  like line of et_entityset.


    types: begin of ty_systems_list_tt,
             param type char20,
             value type char258,
           end of ty_systems_list_tt.

    data lt_systems_list type standard table of ty_systems_list_tt.


    " Adding systems from parameters

    select  param value from ZVND_SCORE_SETUP into table lt_systems_list
      where param  like '%_SYSTEM_NAME'.

    loop at lt_systems_list assigning field-symbol(<ls_systems_list>).

      ls_entityset-systemid = substring_before( val = <ls_systems_list>-param sub = '_SYSTEM_NAME' ).
      ls_entityset-systemname = <ls_systems_list>-value.

      append ls_entityset to et_entityset.

    endloop. " loop at lt_systems_list assigning field-symbol(<ls_systems_list>)




  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZVENDORS_SCORES_DPC_EXT->VALIDATIONDOCUME_GET_ENTITYSET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_FILTER_SELECT_OPTIONS       TYPE        /IWBEP/T_MGW_SELECT_OPTION
* | [--->] IS_PAGING                      TYPE        /IWBEP/S_MGW_PAGING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [--->] IT_ORDER                       TYPE        /IWBEP/T_MGW_SORTING_ORDER
* | [--->] IV_FILTER_STRING               TYPE        STRING
* | [--->] IV_SEARCH_STRING               TYPE        STRING
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET(optional)
* | [<---] ET_ENTITYSET                   TYPE        ZCL_ZVENDORS_SCORES_MPC=>TT_VALIDATIONDOCUMENTS
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method validationdocume_get_entityset.

    data:   lt_filters type  /iwbep/t_mgw_select_option,
            ls_filter  type  /iwbep/s_mgw_select_option,
            ls_so      type                   /iwbep/s_cod_select_option.
    data ls_entityset like line of et_entityset.

    data lv_guid type crmt_object_guid.

    types: begin of ty_valid_docs_tt,
             param type char20,
             value type char256,
           end of ty_valid_docs_tt.

    data lt_valid_docs type standard table of ty_valid_docs_tt.

    data lv_documents_mask_dec type c length 10.
    data lv_documents_mask_bin type string.
    data lv_docs_counter type integer.
    data lv_iterator type integer.
    data lv_mask_element type char1.
    data lv_mask_length_diff type integer.

    data lv_doc_param_mask type c length 20.
    data lv_doc_number_char type c length 3.
    data lv_weight_char type c length 2.


    data ls_scenario_key_tab like line of it_key_tab.

    data lv_vendor_code type char10.
    data lv_system type c length 3.
    data lv_company type c length 10.


    " First checking filters and then keys
*    lt_filters = io_tech_request_context->get_filter( )->get_filter_select_options( ).
*    read table lt_filters with table key property = 'GUID' into ls_filter.
*
*
*    loop at ls_filter-select_options into ls_so.
*
*      lv_guid = ls_so-low.
*
*    endloop.
*
*    if (  lv_guid is initial ).
*      read table it_key_tab into data(ls_scenario_key_tab) with key name = 'guid'.
*      lv_guid = ls_scenario_key_tab-value.
*
*    endif.
*
*    " Getting documents status mask
*
*    select single zzfld00000e from crmd_customer_h into lv_documents_mask_dec where guid = lv_guid.          .
*
*    " Unpacking documents mask to binary line
*
*    try .
*        lv_documents_mask_bin = /ui2/cl_number=>base_converter( number = lv_documents_mask_dec from = 10 to = 2 ).
*
*      catch cx_sy_move_cast_error.
*        lv_documents_mask_bin = '00000000000000'.
*    endtry.





    read table it_key_tab into ls_scenario_key_tab with key name = 'VendorCode'.

    if ls_scenario_key_tab is not initial.

      lv_vendor_code = ls_scenario_key_tab-value.

    endif. " if ls_scenario_key_tab is not initial.

    clear ls_scenario_key_tab.

    read table it_key_tab into ls_scenario_key_tab with key name = 'System'.

    if ls_scenario_key_tab is not initial.

      lv_system = ls_scenario_key_tab-value.

    endif. "  if ls_scenario_key_tab is not initial.


    read table it_key_tab into ls_scenario_key_tab with key name = 'CompanyCode'.

    if ls_scenario_key_tab is not initial.

      lv_company = ls_scenario_key_tab-value.

    endif. "  if ls_scenario_key_tab is not initial.


    if ( lv_vendor_code is initial ) or ( lv_system is initial ).
      return.
    endif.  "if ( lv_vendor_code IS INITIAL ) or ( lv_system IS INITIAL )

    condense lv_vendor_code.
    condense lv_system.
    condense lv_company.




    " Adding validation documents into the list

    select  param value from zvnd_score_setup into table lt_valid_docs
      where param  like 'VALID_DOC_NAME_%'.

    lv_docs_counter = 0.

    loop at lt_valid_docs assigning field-symbol(<ls_valid_docs>).

      ls_entityset-documentname = <ls_valid_docs>-value.
      ls_entityset-documentnumber = substring_after( val = <ls_valid_docs>-param sub = 'VALID_DOC_NAME_' ).
      ls_entityset-vendorcode = lv_vendor_code.
      ls_entityset-system = lv_system.
      ls_entityset-companycode = lv_company.

      lv_docs_counter = lv_docs_counter + 1.

      " Getting type of a document

      lv_doc_number_char = ls_entityset-documentnumber.

      condense lv_doc_number_char.

      concatenate 'VALID_DOC_TYPE_' lv_doc_number_char into lv_doc_param_mask.

      select single value from zvnd_score_setup into ls_entityset-documenttype
          where param eq lv_doc_param_mask.

      condense ls_entityset-documenttype.

      " Adding total documents in class

      ls_entityset-totaldocumentsinclass = 0.

      select count( * ) from zvnd_score_setup into ls_entityset-totaldocumentsinclass
        where param like 'VALID_DOC_TYPE_%' and value eq ls_entityset-documenttype.

      " Adding document weight

      DATA lv_document_weight type p decimals 2.

      lv_document_weight = 100 / ls_entityset-totaldocumentsinclass.

      ls_entityset-DOCUMENTWEIGHT = lv_document_weight.




      " Getting type name

      lv_doc_param_mask = ''.
      concatenate 'VALID_DOCS_CLASS_' ls_entityset-documenttype into lv_doc_param_mask.

      select single value from zvnd_score_setup into ls_entityset-documenttypename
         where param eq lv_doc_param_mask.

      " Adding class weights

      ls_entityset-classweight = 0.
      lv_doc_param_mask = ''.

      concatenate 'VALID_DOCS_WEIGHT_' ls_entityset-documenttype into lv_doc_param_mask.

      select single value from zvnd_score_setup into lv_weight_char
        where param eq lv_doc_param_mask.

      if ( lv_weight_char is not initial ).

        ls_entityset-classweight = lv_weight_char.

      endif.





      append ls_entityset to et_entityset.

    endloop. " loop at lt_valid_docs assigning field-symbol(<ls_valid_docs>).

    "Filling mask up to amount of documents

*    lv_mask_length_diff = lv_docs_counter - strlen( lv_documents_mask_bin ).
*
*    if lv_mask_length_diff > 0.
*      lv_iterator = 1.
*      while lv_iterator le lv_mask_length_diff.
*
*        concatenate '0' lv_documents_mask_bin into lv_documents_mask_bin.
*        lv_iterator = lv_iterator + 1.
*      endwhile.
*
*    endif. " if lv_mask_length_diff > 0.


    sort et_entityset by documentnumber.

*    lv_iterator = 0.
*
*    while lv_iterator < lv_docs_counter.
*
*      lv_mask_element = substring( val = lv_documents_mask_bin off = lv_iterator len = 1 ).
*
*      clear ls_entityset.
*
*      if ( lv_mask_element = '0' or sy-subrc <> 0 ).
*        ls_entityset-documentstatus = ''.
*
*      else.
*        ls_entityset-documentstatus = 'X'.
*
*      endif. " if ( lv_mask_element = '0' or sy-subrc <> 0 )
*
*      modify et_entityset from ls_entityset transporting documentstatus where documentnumber = ( lv_iterator + 1 ).
*
*      lv_iterator = lv_iterator + 1.
*
*    endwhile.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZVENDORS_SCORES_DPC_EXT->VENDORSCORINGLOG_GET_ENTITYSET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_FILTER_SELECT_OPTIONS       TYPE        /IWBEP/T_MGW_SELECT_OPTION
* | [--->] IS_PAGING                      TYPE        /IWBEP/S_MGW_PAGING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [--->] IT_ORDER                       TYPE        /IWBEP/T_MGW_SORTING_ORDER
* | [--->] IV_FILTER_STRING               TYPE        STRING
* | [--->] IV_SEARCH_STRING               TYPE        STRING
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET(optional)
* | [<---] ET_ENTITYSET                   TYPE        ZCL_ZVENDORS_SCORES_MPC=>TT_VENDORSCORINGLOGRECORD
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method vendorscoringlog_get_entityset.

    data ls_entityset  like line of et_entityset.

    data lv_vendor_code type char10.
    data lv_company_code type char10.
    data lv_system type c length 3.
    data ls_scenario_key_tab like line of it_key_tab.

    data lv_destination type char258.
    data lv_destination_param type char20.
    data lv_return_code type sy-subrc.

    data lv_update_date type sy-datum.
    data lv_update_date_char type sy-datum.
    data lv_update_time type sy-uzeit.

    data lt_zvnd_score_log type table of zvnd_score_log.

    data lv_counter type int1.
    data lv_counter_char type char2.

    types: begin of ty_docs_and_score_tt,
             doc_and_score type c length 300,
           end of ty_docs_and_score_tt.

    data lt_docs_and_score_list type standard table of ty_docs_and_score_tt.

    data lv_document      type char258.
    data lv_score         type char6.
    data lv_document_type type char50.

    types: begin of ty_parameter_ranks_tt,
             param type char20,
             value type char258,
           end of ty_parameter_ranks_tt.

    data lt_parameter_ranks type standard table of ty_parameter_ranks_tt.

    data lv_param_mask type char20.

    data lv_rank_char type char3.

    data lv_vendor_score type p decimals 2.
    data lv_rank_score type p decimals 2.


    " Preparing input parameters

    read table it_key_tab into ls_scenario_key_tab with key name = 'VendorCode'.

    if ls_scenario_key_tab is not initial.

      lv_vendor_code = ls_scenario_key_tab-value.

    endif. " if ls_scenario_key_tab is not initial.

    clear ls_scenario_key_tab.

    read table it_key_tab into ls_scenario_key_tab with key name = 'System'.

    if ls_scenario_key_tab is not initial.

      lv_system = ls_scenario_key_tab-value.

    endif. "  if ls_scenario_key_tab is not initial.


    clear ls_scenario_key_tab.

    read table it_key_tab into ls_scenario_key_tab with key name = 'CompanyCode'.

    if ls_scenario_key_tab is not initial.

      lv_company_code = ls_scenario_key_tab-value.

    endif. "  if ls_scenario_key_tab is not initial.



    if ( lv_vendor_code is initial ) or ( lv_system is initial ) or ( lv_company_code is initial ).
      return.
    endif.  "if ( lv_vendor_code IS INITIAL ) or ( lv_system IS INITIAL )

    condense lv_vendor_code.
    condense lv_system.
    condense lv_company_code.

    select * into corresponding fields of table lt_zvnd_score_log from zvnd_score_log
      where sys_id = lv_system
      and company = lv_company_code
      and vendor = lv_vendor_code
      and pending <> 'X'.

    loop at lt_zvnd_score_log assigning field-symbol(<ls_zvnd_score_log>).

      clear ls_entityset.

      ls_entityset-vendorcode = lv_vendor_code.
      ls_entityset-system = lv_system.
      ls_entityset-companycode = lv_company_code.
      ls_entityset-userid = <ls_zvnd_score_log>-user_id.
      ls_entityset-username = <ls_zvnd_score_log>-user_name.
      lv_update_date = <ls_zvnd_score_log>-update_date.
      lv_update_time = <ls_zvnd_score_log>-update_time.


      " Getting score and score text
      ls_entityset-score = <ls_zvnd_score_log>-score.

      lv_vendor_score = <ls_zvnd_score_log>-score.

      select param value from zvnd_score_setup into corresponding fields of table lt_parameter_ranks where param like 'SCORE_TYPE_RANK_%'.

      loop at lt_parameter_ranks assigning field-symbol(<ls_parameter_ranks>).


        lv_rank_score = <ls_parameter_ranks>-value.


        if ( lv_vendor_score ge  lv_rank_score ).

          lv_rank_char = substring_after( val = <ls_parameter_ranks>-param sub = 'SCORE_TYPE_RANK_' ).

          condense lv_rank_char.

          concatenate 'SCORE_TYPE_NAME_' lv_rank_char into lv_param_mask.

          if lv_param_mask is not initial.

            select single value from zvnd_score_setup into ls_entityset-scoretext
             where param eq  lv_param_mask.

          endif. " IF lv_param_mask IS NOT INITIAL

        endif. " IF ( lv_vendor_score GE  lv_rank_score )

      endloop. " LOOP AT lt_parameter_ranks ASSIGNING FIELD-SYMBOL(<ls_parameter_ranks>)

      concatenate  lv_update_time(2) ':' lv_update_time+2(2) ':' lv_update_time+4(2) into ls_entityset-updatetime.

      lv_update_date_char = lv_update_date.

      concatenate  lv_update_date_char+6(2) '.' lv_update_date_char+4(2) '.' lv_update_date_char(4) into ls_entityset-updatedate.

      " Splitting comments and documents list

      ls_entityset-comments = <ls_zvnd_score_log>-comments.


      split <ls_zvnd_score_log>-documents_list at '^' into table lt_docs_and_score_list.

      lv_counter = 1.
      loop at lt_docs_and_score_list assigning field-symbol(<ls_docs_and_score_list>).


        lv_document = substring_before( val = <ls_docs_and_score_list>-doc_and_score sub = '#' ).

        lv_score = substring_after( val = <ls_docs_and_score_list>-doc_and_score sub = '#' ).
        lv_score = substring_before( val = lv_score sub = '@' ).

        lv_document_type = substring_after( val = <ls_docs_and_score_list>-doc_and_score sub = '@' ).

        lv_counter_char = lv_counter.

         concatenate ls_entityset-documentslist lv_counter_char '.' INTO ls_entityset-documentslist.

        concatenate ls_entityset-documentslist lv_document '(' INTO ls_entityset-documentslist SEPARATED BY space.
        concatenate ls_entityset-documentslist lv_document_type '):' INTO ls_entityset-documentslist.
        concatenate ls_entityset-documentslist lv_score INTO ls_entityset-documentslist SEPARATED BY space.
         concatenate ls_entityset-documentslist '%' '\n' into ls_entityset-documentslist .

         lv_counter = lv_counter + 1.

      endloop. " LOOP AT lt_docs_and_score_list ASSIGNING FIELD-SYMBOL(<ls_docs_and_score_list>)






      append ls_entityset to et_entityset.


    endloop. " loop at lt_ZVND_SCORE_LOG ASSIGNING FIELD-SYMBOL(<ls_ZVND_SCORE_LOG>)


    SORT  et_entityset BY UPDATEDATE DESCENDING
                          UPDATETIME DESCENDING.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZVENDORS_SCORES_DPC_EXT->VENDORSET_GET_ENTITY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IO_REQUEST_OBJECT              TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITY(optional)
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITY(optional)
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [<---] ER_ENTITY                      TYPE        ZCL_ZVENDORS_SCORES_MPC=>TS_VENDOR
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_ENTITY_CNTXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method vendorset_get_entity.

    data lv_vendor_code type char10.
    data lv_system type c length 3.
    data lv_company type c length 10.
    data ls_scenario_key_tab like line of it_key_tab.
    data lv_destination type char258.
    data lv_destination_param type char20.
    data lv_return_code type sy-subrc.


    types: begin of ty_parameter_ranks_tt,
             param type char20,
             value type char258,
           end of ty_parameter_ranks_tt.

    data lt_parameter_ranks type standard table of ty_parameter_ranks_tt.

    data lv_param_mask type char20.

    data lv_rank_char type char3.

    data lv_vendor_score type p decimals 2.
    data lv_rank_score type p decimals 2.

    types: begin of ty_zvendor_extended_details_tt,
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
             koinh     type char60,
             textl     type char20,
             witht     type char2,
             cnt_name  type char35,
             butxt     type char25,
             bukrs     type char4,
             kraus     type char11,
           end of ty_zvendor_extended_details_tt.

    data lt_zvendor_extended_details type standard table of ty_zvendor_extended_details_tt.

    data lv_peniding_records type int4.

    read table it_key_tab into ls_scenario_key_tab with key name = 'VendorCode'.

    if ls_scenario_key_tab is not initial.

      lv_vendor_code = ls_scenario_key_tab-value.
      clear ls_scenario_key_tab.

    endif.

    read table it_key_tab into ls_scenario_key_tab with key name = 'System'.

    if ls_scenario_key_tab is not initial.

      lv_system = ls_scenario_key_tab-value.
      clear ls_scenario_key_tab.

    endif.


    read table it_key_tab into ls_scenario_key_tab with key name = 'CompanyCode'.

    if ls_scenario_key_tab is not initial.

      lv_company = ls_scenario_key_tab-value.
      clear ls_scenario_key_tab.

    endif.


    condense lv_vendor_code.
    condense lv_system.
    condense lv_company.


    concatenate lv_system '_RFC_DESTINATION' into lv_destination_param.

    select single value from zvnd_score_setup into lv_destination
      where param = lv_destination_param.

    " Receiving attachment content from GOS

    if ( lv_destination is not initial ) and ( lv_company is not initial ).

      call function 'ZVENDOR_SCORE_MANAGER_EXT' destination lv_destination
        exporting
          ip_vendor           = lv_vendor_code
          ip_bukrs            = lv_company
          ip_method           = 'GET_VENDOR_DETAILS'
        importing
          ep_return_code      = lv_return_code
        tables
          et_extended_details = lt_zvendor_extended_details.

      if lv_return_code = 0.

        loop at lt_zvendor_extended_details assigning field-symbol(<ls_zvendor_extended_details>).

          er_entity-vendordatareceived = abap_true.

          er_entity-vendorcode = lv_vendor_code.
          er_entity-system = lv_system.
          er_entity-companycode = lv_company.

          er_entity-vendorname = <ls_zvendor_extended_details>-name1.
          er_entity-vendorpostingblock = <ls_zvendor_extended_details>-sperr.
          er_entity-vendortelephone = <ls_zvendor_extended_details>-telf1.
          er_entity-vendortaxnumber = <ls_zvendor_extended_details>-stcd1.
          er_entity-vendorcontactname = <ls_zvendor_extended_details>-cnt_name.
          er_entity-vendorbanknumber = <ls_zvendor_extended_details>-bankl.
          er_entity-vendorbankaccount = <ls_zvendor_extended_details>-bankn.
          er_entity-vendorbankaccountholder = <ls_zvendor_extended_details>-koinh.
          er_entity-vendorpaymentblockreason = <ls_zvendor_extended_details>-textl.
          er_entity-vendorwithholdingtaxtype = <ls_zvendor_extended_details>-witht.
          er_entity-vendoremail = <ls_zvendor_extended_details>-smtp_addr.
          er_entity-vendorpaymenttermskey = <ls_zvendor_extended_details>-zterm.
          er_entity-vendorpaymentmethods = <ls_zvendor_extended_details>-zwels.
          er_entity-vendorreconcillaccount = <ls_zvendor_extended_details>-akont.
          er_entity-vendorbankcountry = <ls_zvendor_extended_details>-banks.
          er_entity-vendorcountrykey = <ls_zvendor_extended_details>-land1.
          er_entity-vendorregion = <ls_zvendor_extended_details>-regio.
          er_entity-vendorcity = <ls_zvendor_extended_details>-ort01.
          er_entity-vendorstreethouse = <ls_zvendor_extended_details>-stras.
          er_entity-vendorpostalcode = <ls_zvendor_extended_details>-pstlz.
          er_entity-companyname = <ls_zvendor_extended_details>-butxt.

          " Filling scores

          if ( <ls_zvendor_extended_details>-kraus is not initial ).
            er_entity-score = <ls_zvendor_extended_details>-kraus.
          else.
            er_entity-score = '0'.
          endif.


          lv_vendor_score = <ls_zvendor_extended_details>-kraus.


          select param value from zvnd_score_setup into corresponding fields of table lt_parameter_ranks where param like 'SCORE_TYPE_RANK_%'.

          loop at lt_parameter_ranks assigning field-symbol(<ls_parameter_ranks>).


            lv_rank_score = <ls_parameter_ranks>-value.


            if ( lv_vendor_score ge  lv_rank_score ).

              lv_rank_char = substring_after( val = <ls_parameter_ranks>-param sub = 'SCORE_TYPE_RANK_' ).

              condense lv_rank_char.

              concatenate 'SCORE_TYPE_NAME_' lv_rank_char into lv_param_mask.

              if lv_param_mask is not initial.

                select single value from zvnd_score_setup into er_entity-scoretext
                 where param eq  lv_param_mask.

              endif. " IF lv_param_mask IS NOT INITIAL

            endif. " IF ( lv_vendor_score GE  lv_rank_score )

          endloop. " LOOP AT lt_parameter_ranks ASSIGNING FIELD-SYMBOL(<ls_parameter_ranks>)



          " Checking pending situations

          select count(*) from zvnd_score_log into lv_peniding_records
            where vendor = lv_vendor_code
            and company = lv_company
            and sys_id = lv_system
            and pending = 'X'.

          if ( lv_peniding_records > 0 ).
            er_entity-pendingscoresave = 'X'.
          endif.



        endloop. " loop at lt_zvendor_extended_details assigning field-symbol(<ls_zvendor_extended_details>)

      endif. " if lv_return_code = 0


    endif. " if lv_destination lv_company is not initial

    " Checking authorizations for score weights change

    er_entity-weightchangepermitted = abap_false.

    select single value from zvnd_score_setup into lv_destination
     where param = 'NWGW_RFC_DESTINATION'.

    if sy-subrc = 0.

      call function 'AUTHORITY_CHECK' destination lv_destination
        exporting
          user                = sy-uname
          object              = 'ZWGTHACTN'
          field1              = 'ACTVT'
          value1              = '02'
        exceptions
          user_dont_exist     = 1
          user_is_authorized  = 2
          user_not_authorized = 3
          user_is_locked      = 4
          others              = 5.

      if sy-subrc = 2 .
        er_entity-weightchangepermitted = abap_true.

      endif.

    endif. " if sy-subrc = 0

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZVENDORS_SCORES_DPC_EXT->VENDORSET_GET_ENTITYSET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_FILTER_SELECT_OPTIONS       TYPE        /IWBEP/T_MGW_SELECT_OPTION
* | [--->] IS_PAGING                      TYPE        /IWBEP/S_MGW_PAGING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [--->] IT_ORDER                       TYPE        /IWBEP/T_MGW_SORTING_ORDER
* | [--->] IV_FILTER_STRING               TYPE        STRING
* | [--->] IV_SEARCH_STRING               TYPE        STRING
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET(optional)
* | [<---] ET_ENTITYSET                   TYPE        ZCL_ZVENDORS_SCORES_MPC=>TT_VENDOR
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method vendorset_get_entityset.

    data ls_entityset  like line of et_entityset.
    data lv_system type char3.
    data lv_company type char10.
    data lv_score_range type char6.

    data lv_destination type char258.
    data lv_destination_param type char20.
    data lv_return_code type sy-subrc.

    data: lt_orderby type /iwbep/t_mgw_tech_order,
          ls_orderby type /iwbep/s_mgw_tech_order.

    types: begin of ty_vendors_list_tt,
             lifnr type char10,
             name1 type char35,
             score type char11,
           end of ty_vendors_list_tt.

    data lt_vendors_list type standard table of ty_vendors_list_tt.

    data rg_filter_so type /iwbep/t_cod_select_options.

    field-symbols <rs_system_so> like line of rg_filter_so.

    data rg_vendor_code_so type /iwbep/t_cod_select_options.

    data rg_score_so type /iwbep/t_cod_select_options.

    field-symbols <ls_vendors_list> like line of lt_vendors_list.

    data lv_param_mask type c length 20.
    data lv_low_bound type p decimals 2.
    data lv_high_bound type p decimals 2.
    data lv_score      type p decimals 2.

    data lv_low_bound_char type char6.
    data lv_high_bound_char type char6.
    data lv_score_range_int type i.

    types: begin of ty_excluded_records_tt,
             lifnr type char10,
           end of ty_excluded_records_tt.

    data lt_excluded_records type standard table of ty_excluded_records_tt.
    data ls_excluded_records like line of lt_excluded_records.

    data lv_peniding_records type int4.


    " Getting system from filter
    data(it_filter_so) = io_tech_request_context->get_filter( )->get_filter_select_options( ).

    if line_exists( it_filter_so[ property = 'SYSTEM' ] ).

      " data(rg_filter_so) = it_filter_so[ property = 'SYSTEM' ]-select_options.
      rg_filter_so = it_filter_so[ property = 'SYSTEM' ]-select_options.


      loop at rg_filter_so assigning <rs_system_so>.
        lv_system = <rs_system_so>-low.
      endloop.

      condense lv_system.

    endif.

    clear rg_filter_so.
    if line_exists( it_filter_so[ property = 'COMPANYCODE' ] ).

      " data(rg_filter_so) = it_filter_so[ property = 'COMPANYCODE' ]-select_options.

      rg_filter_so = it_filter_so[ property = 'COMPANYCODE' ]-select_options.

      loop at rg_filter_so assigning <rs_system_so>.
        lv_company = <rs_system_so>-low.
      endloop.

      condense lv_company.

    endif.


    if line_exists( it_filter_so[ property = 'VENDORCODE' ] ).

      rg_vendor_code_so = it_filter_so[ property = 'VENDORCODE' ]-select_options.

    endif.

    " Result can be returned only if system and company are selected

    if ( lv_system is initial ) and ( lv_company is initial ).

      return.

    endif.

    " Additional filters
    " Score filter


    if line_exists( it_filter_so[ property = 'SCORE' ] ).

      rg_score_so = it_filter_so[ property = 'SCORE' ]-select_options.

      loop at rg_score_so assigning field-symbol(<rs_score_so>).
        lv_score_range = <rs_score_so>-low.
      endloop.

    endif.



    " Picking up sorting order

    lt_orderby = io_tech_request_context->get_orderby( ).





    " Preparing parameter name to select destination

    concatenate lv_system '_RFC_DESTINATION' into lv_destination_param.

    select single value from zvnd_score_setup into lv_destination
      where param = lv_destination_param.


    if lv_destination is not initial.
      call function 'ZVENDOR_SCORE_MANAGER_EXT' destination lv_destination
        exporting
          ip_method       = 'GET_VENDORS_LIST'
          ip_bukrs        = lv_company
        importing
          ep_return_code  = lv_return_code
        tables
          et_vendors_list = lt_vendors_list.

    endif. " if lv_destination is not initial

    if lv_return_code = 0.


      if rg_vendor_code_so is initial.
        loop at lt_vendors_list assigning <ls_vendors_list>.

          clear ls_entityset.

          ls_entityset-vendorcode  = <ls_vendors_list>-lifnr.
          ls_entityset-vendorname  = <ls_vendors_list>-name1.
          ls_entityset-companycode = lv_company.

          if <ls_vendors_list>-score is not initial.
            ls_entityset-score = <ls_vendors_list>-score.
          else.
            ls_entityset-score = '0'.

          endif.

          ls_entityset-ScoreFloatFormat = <ls_vendors_list>-score.

          " Checking pending situations

          select count(*) from zvnd_score_log into lv_peniding_records
            where vendor = <ls_vendors_list>-lifnr
            and company = lv_company
            and sys_id = lv_system
            and pending = 'X'.

          if ( lv_peniding_records > 0 ).
            ls_entityset-pendingscoresave = 'X'.
          endif.

          append ls_entityset to et_entityset.

        endloop. " loop at lt_vendors_list ASSIGNING FIELD-SYMBOL(<ls_vendors_list>)


      else.

        loop at lt_vendors_list assigning <ls_vendors_list> where lifnr in rg_vendor_code_so.

          clear ls_entityset.

          ls_entityset-vendorcode  = <ls_vendors_list>-lifnr.
          ls_entityset-vendorname  = <ls_vendors_list>-name1.
          ls_entityset-companycode = lv_company.

          if <ls_vendors_list>-score is not initial.
            ls_entityset-score = <ls_vendors_list>-score.
          else.
            ls_entityset-score = '0'.
          endif.

          ls_entityset-ScoreFloatFormat = <ls_vendors_list>-score.


          " Checking pending situations

          select count(*) from zvnd_score_log into lv_peniding_records
            where vendor = <ls_vendors_list>-lifnr
            and company = lv_company
            and sys_id = lv_system
            and pending = 'X'.

          if ( lv_peniding_records > 0 ).
            ls_entityset-pendingscoresave = 'X'.
          endif.


          append ls_entityset to et_entityset.

        endloop. " loop at lt_vendors_list ASSIGNING FIELD-SYMBOL(<ls_vendors_list>)

      endif.

    endif. " if lv_return_code = 0

    " Filter by Score Range

    if  lv_score_range is not initial.

      condense lv_score_range.

      " Taking low bound and high bound values

      concatenate 'SCORE_TYPE_RANK_' lv_score_range into lv_param_mask.

      select single value from zvnd_score_setup into lv_low_bound_char
        where param eq lv_param_mask.

      if ( sy-subrc = 0 ).

        lv_low_bound = lv_low_bound_char.
        lv_score_range_int = lv_score_range.
        lv_score_range_int = lv_score_range_int + 1.
        lv_score_range = lv_score_range_int.
        condense lv_score_range.


        concatenate 'SCORE_TYPE_RANK_' lv_score_range into lv_param_mask.

        select single value from zvnd_score_setup into lv_high_bound_char
          where param eq lv_param_mask.

        if ( sy-subrc = 0 ).
          lv_high_bound = lv_high_bound_char.
        else.

          lv_high_bound = '100.00'.
        endif.



      endif. " IF ( sy-subrc = 0 )

      loop at et_entityset assigning field-symbol(<es_entityset>).

        lv_score = <es_entityset>-score.

        if ( lv_score ge lv_low_bound ) and ( lv_score le lv_high_bound ).
          continue.
        else.
          ls_excluded_records-lifnr = <es_entityset>-vendorcode.
          append ls_excluded_records to lt_excluded_records.
        endif. " IF ( lv_score GE lv_low_bound ) AND ( lv_score LE lv_high_bound )

      endloop. " LOOP AT et_entityset ASSIGNING FIELD-SYMBOL(<es_entityset>)



      loop at lt_excluded_records assigning field-symbol(<ls_excluded_records>).

        delete et_entityset where vendorcode = <ls_excluded_records>-lifnr.
      endloop. "LOOP AT lt_excluded_records ASSIGNING FIELD-SYMBOL(<ls_excluded_records>)


    endif. " IF  lv_score_range IS NOT INITIAL



    " Sorting the output

    read table lt_orderby into ls_orderby index 1.
    if sy-subrc = 0.
      if ls_orderby-property eq 'SCORE'.


        if ls_orderby-order eq 'desc'.

          sort et_entityset by ScoreFloatFormat descending.

        endif. " if ls_orderby-order eq 'desc'


        if ls_orderby-order eq 'asc'.

          sort et_entityset by ScoreFloatFormat ascending.
        endif. " if ls_orderby-order eq 'asc'

      endif.  " IF ls_orderby-property EQ 'RATING'

    endif. " if sy-subrc = 0



  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZVENDORS_SCORES_DPC_EXT->VENDORSET_UPDATE_ENTITY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITY_U(optional)
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [--->] IO_DATA_PROVIDER               TYPE REF TO /IWBEP/IF_MGW_ENTRY_PROVIDER(optional)
* | [<---] ER_ENTITY                      TYPE        ZCL_ZVENDORS_SCORES_MPC=>TS_VENDOR
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method vendorset_update_entity.


    data ls_header type ihttpnvp.

    data lv_vendor_code type char10.
    data lv_system type c length 3.
    data lv_company type c length 10.
    data ls_scenario_key_tab like line of it_key_tab.

    data lv_destination type char258.
    data lv_destination_param type char20.
    data lv_return_code type sy-subrc.

    data lv_score type c length 6.
    data lv_comments type string.
    data lv_docs_list type string.
    data lv_records_counter type i.

    data lv_company_name type c length 25.
    data lv_vendor_name type char35.

    data lv_score_text type char100.

    data wa_zvnd_score_log type zvnd_score_log.

    data: ls_user_data type soudatai1,
          ls_user      type soudnamei1.

    data lv_record_id type i.

    data lv_lock_status  type char1.

    data lv_text_code type string.
    data lv_subject_code type char256.


    read table it_key_tab into ls_scenario_key_tab with key name = 'VendorCode'.

    if ls_scenario_key_tab is not initial.

      lv_vendor_code = ls_scenario_key_tab-value.
      clear ls_scenario_key_tab.

    endif.

    read table it_key_tab into ls_scenario_key_tab with key name = 'System'.

    if ls_scenario_key_tab is not initial.

      lv_system = ls_scenario_key_tab-value.
      clear ls_scenario_key_tab.

    endif.

    read table it_key_tab into ls_scenario_key_tab with key name = 'CompanyCode'.

    if ls_scenario_key_tab is not initial.

      lv_company = ls_scenario_key_tab-value.
      clear ls_scenario_key_tab.

    endif.

    if ( lv_system is initial ) and ( lv_company is initial ) and ( lv_vendor_code is initial ).

      return.

    endif.

    condense lv_vendor_code.
    condense lv_system.
    condense lv_company.

    " Picking update payload and calculated score and comments and docs list

    io_data_provider->read_entry_data( importing es_data = er_entity ).

    lv_score = er_entity-score.
    lv_comments = er_entity-comments.
    lv_docs_list = er_entity-documentslist.
    lv_company_name = er_entity-companyname.
    lv_vendor_name = er_entity-vendorname.
    lv_score_text = er_entity-scoretext.

    condense lv_score.

    if lv_score is not initial.

      concatenate lv_system '_RFC_DESTINATION' into lv_destination_param.

      select single value from zvnd_score_setup into lv_destination
        where param = lv_destination_param.

      call function 'ZVENDOR_SCORE_MANAGER_EXT' destination lv_destination
        exporting
          ip_vendor      = lv_vendor_code
          ip_score       = lv_score
          ip_method      = 'SET_VENDOR_SCORE'
        importing
          ep_return_code = lv_return_code
          ep_lock_status = lv_lock_status.

      case lv_return_code.

        when 0. " No technical issues occured

          " Recording status into log

          " Getting last ID and increasing by one

          lv_record_id = 0.

          select record_id into lv_record_id from zvnd_score_log up to 1 rows
            order by record_id descending.

          endselect.

          if ( sy-subrc = 0 ) .
            lv_record_id = lv_record_id + 1.
          endif.

          " Getting user full name

          ls_user-sapname = sy-uname.
          call function 'SO_USER_READ_API1'
            exporting
              user      = ls_user
            importing
              user_data = ls_user_data.

          wa_zvnd_score_log-record_id = lv_record_id.
          wa_zvnd_score_log-sys_id = lv_system.
          wa_zvnd_score_log-company = lv_company.
          wa_zvnd_score_log-vendor = lv_vendor_code.
          wa_zvnd_score_log-update_date = sy-datum.
          wa_zvnd_score_log-update_time = sy-uzeit.
          wa_zvnd_score_log-user_id = sy-uname.
          wa_zvnd_score_log-user_name = ls_user_data-fullname.
          wa_zvnd_score_log-score = lv_score.
          wa_zvnd_score_log-comments = lv_comments.
          wa_zvnd_score_log-documents_list = lv_docs_list.
          wa_zvnd_score_log-pending = ''.
          wa_zvnd_score_log-company_name = lv_company_name.
          wa_zvnd_score_log-vendor_name = lv_vendor_name.
          wa_zvnd_score_log-score_text = lv_score_text.

          " Checking if there are pending records in the log

*          select count(*) from zvnd_score_log into lv_records_counter
*            where vendor = lv_vendor_code
*            and company = lv_company
*            and pending = 'X'.

          " Delayed process with background job

          if lv_lock_status = 'L'.

            wa_zvnd_score_log-pending = 'X'.
            ls_header-name = 'ZVNDSAVE_DELAY'.
            ls_header-value = 1.

*            if ( lv_records_counter > 0 ).
*
*              update zvnd_score_log set update_date = sy-datum  update_time = sy-uzeit
*                where vendor = lv_vendor_code and company = lv_company and sys_id = lv_system and pending = 'X'.
*
*            else.

            lv_text_code = 'ZVND_SCORE_CHANGE_DELAY'.
            lv_subject_code = 'EMAIL_SCR_CHGD_DLY'.

            "     insert zvnd_score_log from wa_zvnd_score_log.

            "         endif. " if ( lv_records_counter > 0 )

          else.
            ls_header-name = 'ZVNDSAVE_OK'.
            ls_header-value = 0. "


            lv_text_code = 'ZVND_SCORE_CHANGED'.
            lv_subject_code = 'EMAIL_SCR_CHGD_SUBJ'.



            " Extra action: if vendor saving is pending since a while - we have to update it

*            if ( lv_records_counter > 0 ).
*
*              update zvnd_score_log set update_date = sy-datum  update_time = sy-uzeit  pending = ''
*                 where vendor = lv_vendor_code and company = lv_company and sys_id = lv_system and pending = 'X'.
*
*            else.

            "     insert zvnd_score_log from wa_zvnd_score_log.

            "        endif. " if ( lv_records_counter > 0 )

          endif. " IF ep_lock_status = 'L'

          insert zvnd_score_log from wa_zvnd_score_log.
          " Sending email notifications

          call method zcl_zvendors_scores_dpc_ext=>send_score_update_emails
            exporting
              iv_text_name    = lv_text_code
              iv_subject_code = lv_subject_code
              iv_company_name = lv_company_name
              iv_company_code = lv_company
              iv_vendor_code  = lv_vendor_code
              iv_vendor_name  = lv_vendor_name
              iv_system       = lv_system
              iv_score        = lv_score
              iv_score_text   = lv_score_text.

        when others. " Technical issue

          ls_header-name = 'ZVNDSAVE_FAIL'.
          ls_header-value = lv_return_code.

      endcase. " case lv_return_code

      set_header( is_header = ls_header ).

    endif. " IF lv_score IS NOT INITIAL


  endmethod.
ENDCLASS.
