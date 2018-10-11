FROM haskell:8 as build
WORKDIR /opt/build
RUN cabal update
COPY ./src /opt/build/src
COPY ./app /opt/build/app
COPY ./test /opt/build/test
COPY ./dukkhaless.cabal /opt/build/dukkhaless.cabal
COPY ./stack.yaml  /opt/build/stack.yaml
RUN stack build

FROM ubuntu:18.04
RUN mkdir -p /opt/app
WORKDIR /opt/app
RUN apt-get update && apt-get install -y \
  ca-certificates \
  libgmp-dev
COPY --from=build /opt/build/.stack-work/install/x86_64-linux/lts-12.12/8.4.3/bin/dukkhaless-app /opt/app/dukkhaless-app
COPY --from=build /opt/build/.stack-work/install/x86_64-linux/lts-12.12/8.4.3/bin/dukkhaless-test /opt/app/dukkhaless-test
EXPOSE 3000
CMD ["/opt/app/dukkhaless-app"]