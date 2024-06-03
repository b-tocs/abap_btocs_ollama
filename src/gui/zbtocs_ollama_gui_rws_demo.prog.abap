*&---------------------------------------------------------------------*
*& Report ZBTOCS_OLLAMA_GUI_RWS_DEMO
*&---------------------------------------------------------------------*
*& demo how to use the OLLAMA Connector
*& Repository & Docs: https://github.com/b-tocs/abap_btocs_ollama
*&---------------------------------------------------------------------*
REPORT zbtocs_ollama_gui_rws_demo.

* ------- interface
TABLES sscrfields.
PARAMETERS: p_rfc TYPE rfcdest OBLIGATORY.                " RFC destination to libretrans API (e.g. https://libretranslate.com/)
PARAMETERS: p_prf TYPE zbtocs_rws_profile.                " B-Tocs RWS Profile
PARAMETERS: p_key TYPE zbtocs_api_key LOWER CASE.         " API key, if required
SELECTION-SCREEN: ULINE.
PARAMETERS: p_prmt TYPE zbtocs_llm_prompt LOWER CASE. " OBLIGATORY.                " user input
PARAMETERS: p_clp AS CHECKBOX TYPE zbtocs_flag_clipboard_input  DEFAULT ' '. " get the input from clipboard
SELECTION-SCREEN: ULINE.
PARAMETERS: p_file TYPE zbtocs_filename        LOWER CASE.                       " input from file content
PARAMETERS: p_modl TYPE zbtocs_llm_model      LOWER CASE DEFAULT 'llama2'.      " model to be used
PARAMETERS: p_role TYPE zbtocs_llm_role       LOWER CASE DEFAULT 'user'.        " input standard text
PARAMETERS: p_sysp TYPE zbtocs_llm_sys_prompt LOWER CASE.                       " system prompt
PARAMETERS: p_temp TYPE zbtocs_llm_template   LOWER CASE.                       " system template
SELECTION-SCREEN: SKIP.
PARAMETERS: p_cntx TYPE zbtocs_llm_context    LOWER CASE.                       " context
SELECTION-SCREEN: PUSHBUTTON /35(18) tx_reset USER-COMMAND rctx.
SELECTION-SCREEN: PUSHBUTTON 55(18) tx_clipb USER-COMMAND iclp.
SELECTION-SCREEN: ULINE.
PARAMETERS: p_proto AS CHECKBOX TYPE zbtocs_flag_protocol         DEFAULT 'X'. " show protocol
PARAMETERS: p_trace AS CHECKBOX TYPE zbtocs_flag_display_trace    DEFAULT ' '. " show protocol with trace


INITIALIZATION.
* --------- local data
  DATA(ls_result) = VALUE zbtocs_ollama_s_generate_res( ).
  tx_reset = |Reset Context|.
  tx_clipb = |Import Clipboard|.

  PERFORM import_embedding.

* --------- init utils
  DATA(lo_gui_utils) = zcl_btocs_factory=>create_gui_util( ).
  DATA(lo_logger)    = lo_gui_utils->get_logger( ).

  DATA lo_connector TYPE REF TO zif_btocs_ollama_connector.

* --------------------- F4 Help
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  lo_gui_utils->f4_get_filename_open( CHANGING cv_filename = p_file ).

AT SELECTION-SCREEN.
* ---- process button events
  IF sscrfields-ucomm = 'RCTX'.
    CLEAR: ls_result, p_cntx.
    PERFORM export_embedding.
  ELSEIF sscrfields-ucomm = 'ICLP'.
    PERFORM context_from_clipboard.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.
  IF ls_result-context IS NOT INITIAL.
    p_cntx = ls_result-context.
  ENDIF.



* ============================== START
START-OF-SELECTION.


* =============== OLLAMA Connector
  PERFORM create_connector.
* ---------- set endpoint
  IF lo_connector IS NOT INITIAL.

* --------- get input
    DATA(lv_prmt) = lo_gui_utils->get_input_with_clipboard(
        iv_current   = p_prmt
        iv_clipboard = p_clp
        iv_longtext  = abap_true
    ).

* -------- check sys prompt from input
    DATA(lv_sysp) = p_sysp.
    DATA(lv_cntx) = p_cntx.
    IF lv_prmt CS '---'.
      SPLIT lv_prmt AT '---'
        INTO lv_sysp lv_prmt lv_cntx.
    ENDIF.

    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_sysp WITH ''.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN lv_sysp WITH ''.

    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_prmt WITH ''.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN lv_prmt WITH ''.

    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_cntx WITH ''.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN lv_cntx WITH ''.


* ---------- Output current params
    cl_demo_output=>begin_section( title = |Parameters| ).
    cl_demo_output=>write_text( text = |Model: { p_modl }| ).
    cl_demo_output=>write_text( text = |Role: { p_role }| ).

    cl_demo_output=>write_text( text = |Prompt: { lv_prmt }| ).
    cl_demo_output=>write_text( text = |System Prompt { lv_sysp }| ).

    IF p_temp IS NOT INITIAL.
      cl_demo_output=>write_text( text = |Template: { p_temp }| ).
    ENDIF.

    IF lv_cntx IS NOT INITIAL.
      cl_demo_output=>write_text( text = |Context: { lv_cntx }| ).
    ENDIF.

    cl_demo_output=>end_section( ).

* ---------- File Upload?
    DATA lv_image TYPE xstring.
    IF p_file IS NOT INITIAL.
      IF lo_gui_utils->get_upload(
      EXPORTING
        iv_filename     = p_file
      IMPORTING
        ev_binary       = lv_image
    ) EQ abap_false.
        lo_logger->error( |file upload failed| ).
      ENDIF.
    ENDIF.

* ---------- API TRANSLATE
    DATA(lo_response) = lo_connector->api_generate(
      EXPORTING
        is_params = VALUE zbtocs_ollama_s_generate_par(
          model       = p_modl
          role        = p_role
          prompt      = lv_prmt
          sys_prompt  = lv_sysp
          template    = p_temp
          context     = lv_cntx
          image       = lv_image
        )
        iv_parse = abap_true
      IMPORTING
        es_result = ls_result
    ).

    IF ls_result IS NOT INITIAL.
      cl_demo_output=>begin_section( title = |Result| ).
      cl_demo_output=>write_text( text = |Response: { ls_result-response }| ).

      IF ls_result-context IS NOT INITIAL.
        cl_demo_output=>write_text( text = |Context: { ls_result-context }| ).
        PERFORM export_embedding.
      ENDIF.

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

* ============================== END
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

* ============================== SUBROUTINES
*&---------------------------------------------------------------------*
*& Form create_connector
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_connector .

* ------- reset if open
  IF lo_connector IS NOT INITIAL.
    lo_connector->destroy( ).
    CLEAR lo_connector.
  ENDIF.

* -------- check
  IF p_rfc IS INITIAL.
    RETURN.
  ENDIF.

* -------- init
  DATA(lo_conn) = zcl_btocs_ollama_connector=>create( ).
  lo_conn->set_logger( lo_logger ).


* -------- set endpoint and result
  IF lo_conn->set_endpoint(
    iv_rfc     = p_rfc
    iv_profile = p_prf
  ) EQ abap_true.
    lo_connector = lo_conn.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form context_from_clipboard
*&---------------------------------------------------------------------*
*& import text from clipboard, create an embedding vector
*& and set context field
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM context_from_clipboard .

* ------ get text
  DATA(lv_emb_text) = lo_gui_utils->get_input_with_clipboard(
      iv_current   = ||
      iv_clipboard = abap_true
      iv_longtext  = abap_true
  ).
  IF lv_emb_text IS INITIAL.
    RETURN.
  ELSE.
    PERFORM create_embedding USING lv_emb_text.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form create_embedding
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_EMB_TEXT
*&---------------------------------------------------------------------*
FORM create_embedding  USING    p_text.

* ----- check model
  IF p_modl IS INITIAL.
    lo_logger->error( |model is required| ).
    RETURN.
  ENDIF.

  IF p_text IS INITIAL.
    lo_logger->error( |no text for embedding| ).
    RETURN.
  ENDIF.

* ----- create connection
  PERFORM create_connector.
  IF lo_connector IS INITIAL.
    lo_logger->error( |no connector available for | ).
    RETURN.
  ENDIF.

* ---- prepare
  DATA(ls_par_gen) = VALUE zbtocs_ollama_s_generate_par(
    model   = p_modl
    prompt  = p_text
    sys_prompt = 'Generate a context for further user prompts'
  ).
  CLEAR ls_result.

  DATA(lo_response) = lo_connector->api_generate(
    EXPORTING
      is_params      = ls_par_gen
      iv_parse       = abap_true
  IMPORTING
      es_result      = ls_result
  ).


* ---- create embedding
*  DATA(lv_emb_array) = ||.
*
*  DATA(ls_par) = VALUE zbtocs_ollama_s_embeddings_par(
*    model   = p_modl
*    prompt  = p_text
*  ).
*
*  DATA(lo_response) = lo_connector->api_embeddings(
*    EXPORTING
*      is_params    = ls_par
*      iv_parse     = abap_true
*    IMPORTING
**      et_embedding =                  " Table of Strings
*      ev_embedding = lv_emb_array
*  ).

** ---- check embedding
*  IF lv_emb_array IS NOT INITIAL.
*    ls_result-context = lv_emb_array.
*  ENDIF.

* --------- export context
  IF ls_result-context IS NOT INITIAL.
    PERFORM export_embedding.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form import_embedding
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM import_embedding .
  IMPORT context = ls_result-context FROM MEMORY ID 'ZBTOCS_OLLAMA_GUI_RWS_DEMO'.
  IF ls_result-context IS NOT INITIAL.
    p_cntx = ls_result-context.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form export_embedding
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM export_embedding .
  EXPORT context = ls_result-context TO MEMORY ID 'ZBTOCS_OLLAMA_GUI_RWS_DEMO'.
ENDFORM.
