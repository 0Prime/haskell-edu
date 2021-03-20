FROM haskell:8.8.4

RUN apt-get update \
    && apt-get -y upgrade \
    && apt-get -y install \
    cabal-install \
    && apt-get autoremove

RUN stack install --resolver lts-16.31 hlint