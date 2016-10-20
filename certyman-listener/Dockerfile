FROM ubuntu

RUN apt-get update && \
    apt-get install -y libgmp-dev netbase

COPY .stack-work/install/x86_64-linux/lts-7.2/8.0.1/bin/docker-event-listener-exe /usr/local/bin/docker-event-listener

CMD "docker-event-listener"
