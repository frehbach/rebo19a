FROM cigroup/revolve:master

#move file inside the container
COPY getFitness.py ./revolve/experiments/examples
COPY config_new.py ./revolve/pyrevolve/config.py

#create the folder for the connection with the volume
RUN mkdir -p ./revolve/experiments/bodies



