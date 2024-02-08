# Install OLLAMA in your B-Tocs Server farm

Create a new stack with the following compose file. If you have no B-Tocs Container Service Farm visit the (german) [Youtube Channel](https://www.youtube.com/channel/UCk4K1ZKPW4sdngJPcYeHJCA) for Step-by-Step Installation Guide.

## Compose File 

```yaml
version: "3"

networks:
  default:
    internal: true
  intern:
    external: true
  extern:
    external: true

volumes:
  ollama_def_data:

services:
  ollama_default:
    image: ollama/ollama
    networks:
      - intern
      - extern
    expose:
      - "11434"
    volumes:
      - ollama_def_data:/root/.ollama
```