*&---------------------------------------------------------------------*
*& Report ZBTOCS_OLLAMA_GUI_RWS_DEMO
*&---------------------------------------------------------------------*
*& demo how to use the OLLAMA Connector
*& Repository & Docs: https://github.com/b-tocs/abap_btocs_ollama
*&---------------------------------------------------------------------*
REPORT zbtocs_ollama_gui_rws_demo.

* ------- interface
PARAMETERS: p_rfc TYPE rfcdest OBLIGATORY.                " RFC destination to libretrans API (e.g. https://libretranslate.com/)
PARAMETERS: p_prf TYPE zbtocs_rws_profile.                " B-Tocs RWS Profile
PARAMETERS: p_key TYPE zbtocs_api_key LOWER CASE.         " API key, if required
SELECTION-SCREEN: ULINE.
PARAMETERS: p_prmt TYPE zbtocs_llm_prompt LOWER CASE. " OBLIGATORY.                " user input
PARAMETERS: p_clp AS CHECKBOX TYPE zbtocs_flag_clipboard_input  DEFAULT ' '. " get the input from clipboard
SELECTION-SCREEN: ULINE.
PARAMETERS: p_modl TYPE zbtocs_llm_model  LOWER CASE DEFAULT 'llama2'.          " model to be used
PARAMETERS: p_role TYPE zbtocs_llm_role   LOWER CASE DEFAULT 'user'.            " input standard text
PARAMETERS: p_sysp TYPE zbtocs_llm_sys_prompt LOWER CASE.                       " system prompt
PARAMETERS: p_temp TYPE zbtocs_llm_template   LOWER CASE.                       " system template
PARAMETERS: p_cntx TYPE zbtocs_llm_context    LOWER CASE.                       " context
SELECTION-SCREEN: ULINE.
PARAMETERS: p_proto AS CHECKBOX TYPE zbtocs_flag_protocol         DEFAULT 'X'. " show protocol
PARAMETERS: p_trace AS CHECKBOX TYPE zbtocs_flag_display_trace    DEFAULT ' '. " show protocol with trace


INITIALIZATION.
* --------- init utils
  DATA(lo_gui_utils) = zcl_btocs_factory=>create_gui_util( ).
  DATA(lo_logger)    = lo_gui_utils->get_logger( ).

  DATA(lo_connector) = zcl_btocs_ollama_connector=>create( ).
  lo_connector->set_logger( lo_logger ).


START-OF-SELECTION.


* =============== OLLAMA Connector
* ---------- set endpoint
  IF lo_connector->set_endpoint(
    iv_rfc     = p_rfc
    iv_profile = p_prf
  ) EQ abap_true.

* --------- get input
    DATA(lv_prmt) = lo_gui_utils->get_input_with_clipboard(
        iv_current   = p_prmt
        iv_clipboard = p_clp
        iv_longtext  = abap_true
    ).

* -------- check sys prompt from input
    DATA(lv_sysp) = p_sysp.
    IF lv_prmt CS '---'.
      SPLIT lv_prmt AT '---'
        INTO lv_sysp lv_prmt.
    ENDIF.


* ---------- process options
    DATA lo_response TYPE REF TO zif_btocs_rws_response.

* ---------- API TRANSLATE
    DATA(ls_result) = VALUE zbtocs_ollama_s_generate_res( ).

    lo_response = lo_connector->api_generate(
      EXPORTING
        is_params = VALUE zbtocs_ollama_s_generate_par(
          model       = p_modl
          role        = p_role
          prompt      = lv_prmt
          sys_prompt  = lv_sysp
          template    = p_temp
          context     = p_cntx
        )
        iv_parse = abap_true
      IMPORTING
        es_result = ls_result
    ).

    IF ls_result IS NOT INITIAL.
      cl_demo_output=>begin_section( title = |Result| ).
      cl_demo_output=>write_text( text = |Response: { ls_result-response }| ).
      cl_demo_output=>end_section( ).
    ENDIF.

* ------------ check response
    IF lo_response IS INITIAL.
      lo_logger->error( |invalid response detected| ).
    ELSE.
* ------------ create status results
      DATA(lv_status) = lo_response->get_status_code( ).
      DATA(lv_reason) = lo_response->get_reason( ).

      cl_demo_output=>begin_section( title = |HTTP Request Result| ).
      cl_demo_output=>write_text( text = |HTTP Status Code: { lv_status }| ).
      cl_demo_output=>write_text( text = |HTTP Reason Text: { lv_reason }| ).
      cl_demo_output=>end_section( ).

* ------------ create content results
      DATA(lv_response) = lo_response->get_content( ).
      DATA(lv_conttype) = lo_response->get_content_type( ).

      cl_demo_output=>begin_section( title = |Response| ).
      cl_demo_output=>write_text( text = |Content-Type: { lv_conttype }| ).
      cl_demo_output=>write_html( lv_response ).
      cl_demo_output=>end_section( ).

    ENDIF.
  ENDIF.

* =============== cleanup
  DATA(lt_msg) = lo_logger->get_messages(
                   iv_no_trace      = COND #( WHEN p_trace EQ abap_true
                                              THEN abap_false
                                              ELSE abap_true )
                 ).
  lo_connector->destroy( ).


END-OF-SELECTION.


* ============== display trace
  IF p_proto = abap_true
    AND lt_msg[] IS NOT INITIAL.
    cl_demo_output=>begin_section( title = |Protocol| ).
    cl_demo_output=>write_data(
      value   = lt_msg
      name    = 'Messages'
    ).
    cl_demo_output=>end_section( ).
  ENDIF.

* ------------ display result
  cl_demo_output=>display( ).
