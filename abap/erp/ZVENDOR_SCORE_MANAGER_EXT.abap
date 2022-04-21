function zvendor_score_manager_ext .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IP_VENDOR) TYPE  LIFNR OPTIONAL
*"     VALUE(IP_METHOD) TYPE  CHAR20
*"     VALUE(IP_DOCUMENT_ID) TYPE  SO_ENTRYID OPTIONAL
*"     VALUE(IP_FILE_CONTENT) TYPE  XSTRING OPTIONAL
*"     VALUE(IP_FILE_NAME) TYPE  STRING OPTIONAL
*"     VALUE(IP_FILE_EXTENSION) TYPE  SOODK-OBJTP OPTIONAL
*"     VALUE(IP_BUKRS) TYPE  BUKRS OPTIONAL
*"     VALUE(IP_SCORE) TYPE  KRAUS_CM OPTIONAL
*"  EXPORTING
*"     VALUE(EP_RETURN_CODE) TYPE  SY-SUBRC
*"     VALUE(EV_MIME_TYPE) TYPE  CHAR128
*"     VALUE(EP_LOCK_STATUS) TYPE  CHAR1
*"     VALUE(EV_DOC_SIZE) TYPE  SO_DOC_SIZ
*"  TABLES
*"      ET_ATTACHMENTS_LIST STRUCTURE  ZVENDOR_ATTACHMENT OPTIONAL
*"      ET_HEX_CONTENT STRUCTURE  SOLIX OPTIONAL
*"      ET_VENDORS_LIST STRUCTURE  ZVENDOR_MAIN_DETAILS_AND_SCORE
*"       OPTIONAL
*"      ET_EXTENDED_DETAILS STRUCTURE  ZVENDOR_EXTENDED_DETAILS
*"       OPTIONAL
*"      ET_COMPANIES_LIST STRUCTURE  ZCOMPANIES_BASIC_DETAILS OPTIONAL
*"----------------------------------------------------------------------

  ep_return_code = 1.

  if ( ip_method is not initial ).

    case ip_method.
      when 'GET_ATTACHMENTS_LIST'.

        if ( ip_vendor is not initial ).

          call method zcl_vendor_score_manager=>get_attachments_list
            exporting
              ip_vendor           = ip_vendor
            importing
              et_attachments_list = et_attachments_list[].

          ep_return_code = sy-subrc.

        endif. "  if ( ip_vendor is not initial )

      when 'GET_ATTACHMENT'.

        if ( ip_document_id is not initial ).
          call method zcl_vendor_score_manager=>get_attachment
            exporting
              ip_document_id = ip_document_id
            importing
              ev_mime_type   = ev_mime_type
              ev_doc_size    = ev_doc_size
           "  es_content_xstring = es_content_xstring.
              et_hex_content = et_hex_content[].

          ep_return_code = sy-subrc.

        endif. "  if ( ip_document_id is not initial )


      when 'UPLOAD_ATTACHMENT'.

        if ( ip_vendor is not initial ) and ( ip_file_content is not initial ).

          call method zcl_vendor_score_manager=>upload_attachment
            exporting
              ip_vendor         = ip_vendor
              ip_file_content   = ip_file_content
              ip_file_name      = ip_file_name
              ip_file_extension = ip_file_extension.

          ep_return_code = sy-subrc.

        endif. "  if ( ip_vendor is not initial )

      when 'GET_VENDORS_LIST'.

        if ( ip_bukrs is not initial ).

          call method zcl_vendor_score_manager=>get_vendors_list
            exporting
              ip_bukrs        = ip_bukrs
            importing
              et_vendors_list = et_vendors_list[].

          ep_return_code = sy-subrc.

        endif.

      when 'GET_COMPANIES_LIST'.

        call method zcl_vendor_score_manager=>get_companies_list
          importing
            et_companies_list = et_companies_list[].

        ep_return_code = sy-subrc.


      when 'GET_VENDOR_DETAILS'.

        if ( ip_vendor is not initial ) and ( ip_bukrs is not initial ).

          call method zcl_vendor_score_manager=>get_vendor_details
            exporting
              ip_vendor           = ip_vendor
              ip_bukrs            = ip_bukrs
            importing
              et_extended_details = et_extended_details[].

          ep_return_code = sy-subrc.

        endif. "  if ( ip_vendor is not initial )

        ep_return_code = sy-subrc.

      when 'SET_VENDOR_SCORE'.

        if ( ip_vendor is not initial ) and ( ip_score is not initial ).

          call method zcl_vendor_score_manager=>set_vendor_score
            exporting
              ip_vendor = ip_vendor
              ip_score  = ip_score
          importing
              ep_lock_status = ep_lock_status.

          ep_return_code = sy-subrc.

        endif. "  if ( ip_vendor is not initial )

        ep_return_code = sy-subrc.



    endcase. " CASE IP_METHOD.

  endif.  "if ( ip_method is not initial )

endfunction.