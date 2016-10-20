FROM ubuntu

RUN apt-get update && \
    apt-get install -y libgmp-dev netbase

COPY .stack-work/install/x86_64-linux/lts-6.5/7.10.3/bin/openresty-le-manager-exe /usr/local/bin/le-manager-ui
COPY /web/html /srv/web/html

EXPOSE 8001

WORKDIR /srv

CMD "le-manager-ui"
