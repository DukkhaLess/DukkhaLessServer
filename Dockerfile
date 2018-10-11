FROM haskell:8
WORKDIR /opt/server
RUN cabal update
COPY ./src /opt/server/src
COPY ./dukkhaless.cabal /opt/server/dukkhaless.cabal
COPY ./stack.yaml  /opt/server/stack.yaml
RUN stack install
EXPOSE 3000
CMD ["dukkhaless"]