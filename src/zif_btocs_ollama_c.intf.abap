INTERFACE zif_btocs_ollama_c
  PUBLIC .


  CONSTANTS version TYPE string VALUE 'V20240220' ##NO_TEXT.
  CONSTANTS release TYPE string VALUE '0.1.1' ##NO_TEXT.
  CONSTANTS homepage TYPE string VALUE 'https://b-tocs.org' ##NO_TEXT.
  CONSTANTS repository TYPE string VALUE 'https://github.com/b-tocs/abap_btocs_ollamal' ##NO_TEXT.
  CONSTANTS author TYPE string VALUE 'mdjoerg@b-tocs.org' ##NO_TEXT.
  CONSTANTS depending TYPE string VALUE 'https://github.com/b-tocs/abap_btocs_core:0.3.1' ##NO_TEXT.

  " reference https://github.com/ollama/ollama/blob/main/docs/api.md
  CONSTANTS:
    BEGIN OF api_path,
      generate   TYPE string VALUE '/api/generate',
      embeddings TYPE string VALUE '/api/embeddings',
      tags       TYPE string VALUE '/api/tags',
      show       TYPE string VALUE '/api/show',
    END OF api_path .
ENDINTERFACE.
