FROM haskell:8.4.3 as build
WORKDIR /opt/build
RUN cabal update
RUN apt-get update && apt-get install -y \
  libpq-dev
COPY src /opt/build/src
COPY app /opt/build/app
COPY test /opt/build/test
COPY dukkhaless.cabal /opt/build/dukkhaless.cabal
COPY stack.yaml  /opt/build/stack.yaml
RUN stack build && stack test

FROM ubuntu:18.04
WORKDIR /opt/app
RUN apt-get update && apt-get install -y \
  ca-certificates \
  libgmp-dev \
  libpq-dev
COPY LICENSE /opt/app/LICENSE
COPY production.conf /opt/app/production.conf
COPY development.conf /opt/app/development.conf
COPY ./migrations /opt/app/migrations
COPY --from=build /opt/build/.stack-work/install/x86_64-linux/lts-12.11/8.4.3/bin/dukkhaless-app /opt/app/dukkhaless-app
EXPOSE 4000
CMD ["/opt/app/dukkhaless-app"]
