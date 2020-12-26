ARG TEXLIVE_VERSION=2020

FROM debian:buster-slim

LABEL maintainer="Hiromi ISHII <konn.jinro_at_gmail.com>"
ENV DEBIAN_FRONTEND noninteractive
ARG TEXLIVE_VERSION

RUN apt-get update \
  && apt-get install -y --no-install-recommends --no-install-suggests \
    perl wget ca-certificates

# Install TeXLive
WORKDIR /root/install
ADD ci-resources/texlive${TEXLIVE_VERSION}-frozen.profile /root/install
ADD ci-resources/install-tl-unx.tar.gz.sha512 /root/install
RUN wget -q https://ftp.kddilabs.jp/CTAN/systems/texlive/tlnet/install-tl-unx.tar.gz \
  && sha512sum -c install-tl-unx.tar.gz.sha512 \
  && tar xzvf install-tl-unx.tar.gz \
  && cd install-tl-* && ./install-tl --no-gui \
    --profile /root/install/texlive${TEXLIVE_VERSION}-frozen.profile \
    --repository https://texlive.texjp.org/${TEXLIVE_VERSION}/tlnet \
  && cd /root && rm -rf /root/install

# Introduces TEXLIVE into PATH
ENV PATH=/opt/texlive/${TEXLIVE_VERSION}/bin/x86_64-linux:$PATH

# Making sure the luaotfload to create a cache at least once.
RUN luaotfload-tool -u -v

RUN tlmgr install latexmk
