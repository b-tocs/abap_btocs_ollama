# Local install with Docker Desktop

1. Install Docker Desktop
2. Load image https://hub.docker.com/r/ollama/ollama
3. Start the container with port maping 11434:11434
4. Open the containers terminal
5. Enter `ollama run YOUR_MODEL` to load your model
6. Load the ngrok binary for your platform [here](https://ngrok.com/download)
7. create a route to your service with `ngrok http 11434`
8. Copy the random hostname and use it in SM59 as host (port 443 for https)
9. Activate SSL in SM59
10. Check connection
