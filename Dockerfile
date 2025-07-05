# Instructions on how to build and run this Dockerfile and the Erlang environment:

### 1. Build the Docker image (ONLY ONCE):
    # a. Open terminal/PowerShell.
    # b. Navigate to the directory containing this Dockerfile.
    # c. Run: docker build -t erlang-dev .
# **********************************************************************
    ### 2. Run the Docker container:
    # Run: docker run -it --rm -v ${PWD}:/app -w /app erlang-dev

## Windows (other shells):
    # Command Prompt: docker run -it --rm -v %cd%:/app -w /app erlang-dev
    # Git Bash: docker run -it --rmdocker build -t erlang-dev . -v $(pwd):/app -w /app erlang-dev


FROM erlang:26

# Install build tools (optional)
RUN apt-get update && apt-get install -y git

# Set work directory
WORKDIR /app

# Default command - Erlang shell
CMD ["erl"]
