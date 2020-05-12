#
# Build the frontend.
#
FROM node:10 as dashboard-builder

WORKDIR /usr/src/app

COPY dashboard ./

RUN npm install && npm run prod

#
# Build the server.
#
FROM alpine:edge as server-builder

WORKDIR /build

RUN apk upgrade --update-cache --available && \
    apk add ghc \
            alpine-sdk \
            gmp-dev \
            libffi \
            libffi-dev \
            musl-dev \
            zlib-dev \
            ncurses-dev \
            postgresql-dev \
            cabal \
            ca-certificates && \
            cabal update

COPY UNLICENSE      ./
COPY app/           ./app
COPY README.md      ./
COPY hakatime.cabal ./
COPY src/           ./src
COPY test/          ./test

RUN cabal build -j1 -O2 hakatime   && \
    cabal install -j1 -O2 hakatime && \
    mkdir -p /app/bin              && \
    cp ~/.cabal/bin/hakatime /app/bin/hakatime

FROM alpine:edge

RUN apk add --no-cache \
        libffi-dev \
        gmp-dev \
        zlib-dev \
        ncurses-dev \
        postgresql-dev && \
    # Remove files that we don't' need.
    rm -rf /usr/lib/libLLVM* \
           /usr/lib/libclang* \
           /usr/lib/llvm10 \
           /usr/lib/clang \
           /usr/include

COPY --from=dashboard-builder /usr/src/app/dist /app/bin/dashboard
COPY --from=server-builder /app/bin/hakatime /app/bin/hakatime

EXPOSE 8080

ENV HAKA_PORT           8080
ENV HAKA_DASHBOARD_PATH /app/bin/dashboard

CMD ["/app/bin/hakatime", "run"]
