FROM gitpod/workspace-full:2022-05-25-08-50-33

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_INSTALL_STACK=1 BOOTSTRAP_HASKELL_INSTALL_HLS=1 BOOTSTRAP_HASKELL_VERBOSE=1 BOOTSTRAP_HASKELL_GHC_VERSION=9.0.2 sh

RUN sudo ln -s /home/gitpod/.ghcup/bin/* /usr/local/bin

RUN mkdir -p /workspace/.ghcup/bin && \
    sudo ls -s /home/gitpod/.ghcup/bin/* /workspace/.ghcup/bin

RUN mkdir -p /home/gitpod/.stack && \
    echo "system-ghc: true" > /home/gitpod/.stack/config.yaml
