FROM haskell:8
WORKDIR /opt/server
RUN cabal update
COPY . /opt/server
RUN stack install
EXPOSE 3000
CMD ["dukkhaless"]