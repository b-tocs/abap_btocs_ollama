*&---------------------------------------------------------------------*
*& Report ZBTOCS_OLLAMA_GUI_API_TAGS
*&---------------------------------------------------------------------*
*& create embedding for prompt
*& Repository & Docs: https://github.com/b-tocs/abap_btocs_ollama
*&---------------------------------------------------------------------*
REPORT zbtocs_ollama_gui_api_embed.

* ------- interface
PARAMETERS: p_rfc TYPE rfcdest OBLIGATORY.                " RFC destination to libretrans API (e.g. https://libretranslate.com/)
PARAMETERS: p_prf TYPE zbtocs_rws_profile.                " B-Tocs RWS Profile
SELECTION-SCREEN: ULINE.
PARAMETERS: p_prmt TYPE zbtocs_llm_prompt LOWER CASE. " OBLIGATORY.                " user input
PARAMETERS: p_modl TYPE zbtocs_llm_model  LOWER CASE DEFAULT 'llama2'.          " model to be used
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


* ---------- API TRANSLATE
    DATA lt_result TYPE string_table.
    DATA lv_array  TYPE string.

    DATA(lo_response) = lo_connector->api_embeddings(
      EXPORTING
        is_params = VALUE zbtocs_ollama_s_embeddings_par(
          model = p_modl
          prompt = p_prmt
        )
        iv_parse = abap_true
      IMPORTING
        et_embedding = lt_result
        ev_embedding = lv_array
     ).

* ------------ check response
    IF lo_response IS INITIAL.
      lo_logger->error( |invalid response detected| ).
    ELSE.
* ------------ show result
      IF lv_array IS NOT INITIAL.
        cl_demo_output=>begin_section( title = |Embedding Array| ).
        cl_demo_output=>write_data( lv_array ).
        cl_demo_output=>end_section( ).
      ENDIF.

      IF lt_result IS NOT INITIAL.
        cl_demo_output=>begin_section( title = |Embeddings| ).
        cl_demo_output=>write_data( lt_result ).
        cl_demo_output=>end_section( ).
      ENDIF.
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
