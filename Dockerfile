# step 1

FROM haskell:9.4.8-slim-buster AS base

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update
RUN apt-get install libpq-dev -y

WORKDIR /app

COPY rio-cabal.cabal .

RUN cabal update && cabal build --only-dependencies

# step 2

FROM base AS builder

WORKDIR /app

COPY . .

RUN cabal build


# step 3

FROM ubuntu:22.04 AS runtime

ENV POSTGRES_CONNECT_STRING=host=localhost port=5432 user=postgres password=postgres dbname=riocabal connect_timeout=10
ENV POSTGRES_POOL_SIZE=1
# ENV JWK_STRING=

WORKDIR /root/

COPY --from=builder /app ./

RUN apt-get update

RUN apt-get install libpq-dev libncurses5 -y

EXPOSE 8080

FROM runtime as finalruntime

CMD ./dist-newstyle/build/*/ghc-*/rio-cabal-*/x/rio-cabal/build/rio-cabal/rio-cabal


