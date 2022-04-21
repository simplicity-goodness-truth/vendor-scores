*&---------------------------------------------------------------------*
*& Report  ZVENDOR_UNLOCK_BY_QUEUE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report zscore_vendor_by_queue.

data lt_zvnd_score_log type table of zvnd_score_log.
"data wa_zvnd_score_log type zvnd_score_log.

data lt_zvnd_score_stage type table of zvnd_score_stage.

data lv_destination type char258.
data lv_destination_param type char20.
data lv_return_code type sy-subrc.
data lv_return_code_upload type sy-subrc.

data lv_lock_status  type char1.

data lv_error_message type char258.
data lv_log_message type char258.


types: begin of ty_file_records_to_remove_tt,
         guid type crmt_clm_guid_c,
       end of ty_file_records_to_remove_tt.

data lt_file_records_to_remove type standard table of ty_file_records_to_remove_tt.

data ls_file_records_to_remove type ty_file_records_to_remove_tt.

select * into corresponding fields of table lt_zvnd_score_log from zvnd_score_log
  where pending = 'X'.


loop at lt_zvnd_score_log assigning field-symbol(<ls_zvnd_score_log>).

  concatenate <ls_zvnd_score_log>-sys_id '_RFC_DESTINATION' into lv_destination_param.

  select single value from zvnd_score_setup into lv_destination
    where param = lv_destination_param.


  call function 'ZVENDOR_SCORE_MANAGER_EXT' destination lv_destination
    exporting
      ip_vendor      = <ls_zvnd_score_log>-vendor
      ip_score       = <ls_zvnd_score_log>-score
      ip_method      = 'SET_VENDOR_SCORE'
    importing
      ep_return_code = lv_return_code
      ep_lock_status = lv_lock_status.

  case lv_return_code.

    when 0.

      if lv_lock_status = ''.

        " Score has been successfully set

        concatenate 'Vendor' <ls_zvnd_score_log>-vendor 'saved successfully on' <ls_zvnd_score_log>-sys_id 'system with score' <ls_zvnd_score_log>-score into lv_log_message separated by space.

        write: lv_log_message.

        " Releasing attachments

        select * into corresponding fields of table lt_zvnd_score_stage from zvnd_score_stage
          where sys_id = <ls_zvnd_score_log>-sys_id
          and company = <ls_zvnd_score_log>-company
          and vendor = <ls_zvnd_score_log>-vendor .

        loop at lt_zvnd_score_stage assigning field-symbol(<ls_zvnd_score_stage>).

          " Uploading attachments one by one

          if lv_destination is not initial.
            call function 'ZVENDOR_SCORE_MANAGER_EXT' destination lv_destination
              exporting
                ip_vendor         = <ls_zvnd_score_stage>-vendor
                ip_method         = 'UPLOAD_ATTACHMENT'
                ip_file_name      = <ls_zvnd_score_stage>-file_name
                ip_file_extension = <ls_zvnd_score_stage>-extension
                ip_file_content   = <ls_zvnd_score_stage>-media
              importing
                ep_return_code    = lv_return_code_upload.

            " Preparing a deletion table

            if ( lv_return_code_upload = 0 ).

              concatenate 'File' <ls_zvnd_score_stage>-file_name into lv_log_message separated by space.
              concatenate lv_log_message '.' <ls_zvnd_score_stage>-extension into lv_log_message.
              concatenate lv_log_message 'successfully uploaded to' <ls_zvnd_score_log>-sys_id 'system' into lv_log_message separated by space.

              write: lv_log_message.


              ls_file_records_to_remove-guid = <ls_zvnd_score_stage>-guid.

              append ls_file_records_to_remove to lt_file_records_to_remove.

            endif. " if ( lv_return_code_upload = 0 )

          endif. "  if lv_destination is not initial

        endloop. " loop at lt_ZVND_SCORE_STAGE ASSIGNING FIELD-SYMBOL(<ls_ZVND_SCORE_STAGE>)

        " Cleaning stage area

        loop at lt_file_records_to_remove assigning field-symbol(<ls_file_records_to_remove>).

          delete from zvnd_score_stage where guid = <ls_file_records_to_remove>-guid.

        endloop. " LOOP AT lt_file_records_to_remove ASSIGNING FIELD-SYMBOL(<ls_file_records_to_remove>)

        " Removing pending flag

        update zvnd_score_log set update_date = sy-datum  update_time = sy-uzeit  pending = ''
           where vendor = <ls_zvnd_score_log>-vendor and company = <ls_zvnd_score_log>-company and sys_id = <ls_zvnd_score_log>-sys_id.


        " Sending emails

        call method zcl_zvendors_scores_dpc_ext=>send_score_update_emails
          exporting
            iv_text_name    = 'ZVND_SCORE_CHANGED'
            iv_subject_code = 'EMAIL_SCR_CHGD_SUBJ'
            iv_company_name = <ls_zvnd_score_log>-company_name
            iv_company_code = <ls_zvnd_score_log>-company
            iv_vendor_code  = <ls_zvnd_score_log>-vendor
            iv_vendor_name  = <ls_zvnd_score_log>-vendor_name
            iv_system       = <ls_zvnd_score_log>-sys_id
            iv_score        = <ls_zvnd_score_log>-score
            iv_score_text   = <ls_zvnd_score_log>-score_text.



      else.

        " Changing update time

        update zvnd_score_log
          set update_date = sy-datum
              update_time = sy-uzeit
          where vendor = <ls_zvnd_score_log>-vendor
           and company = <ls_zvnd_score_log>-company
           and sys_id = <ls_zvnd_score_log>-sys_id.


        concatenate 'Vendor' <ls_zvnd_score_log>-vendor 'still locked on' <ls_zvnd_score_log>-sys_id 'system. No score setting or attachments uploading done.' into lv_log_message separated by space.

        write: lv_log_message.


      endif.

    when 1.

      concatenate 'Call to backend system' <ls_zvnd_score_log>-sys_id 'via' lv_destination 'failed' into lv_error_message.
      write lv_error_message.

  endcase. " case lv_return_code


endloop. "loop at lt_zvnd_score_log assigning field-symbol(<ls_zvnd_score_log>)