# bin and sbin paths
HOME_PATH=$HOME/bin:$HOME/.local/bin
BIN_PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:

# Paths to various Node versions
NODE19_PATH=$HOME/.nvm/versions/node/v19.4.0/bin
NODE18_PATH=$HOME/.nvm/versions/node/v18.13.0/bin
NODE14_PATH=$HOME/.nvm/versions/node/v14.13.0/bin

COURSIER_PATH=$HOME/.local/share/coursier/bin
CONDA_PATH=$HOME/anaconda3/bin
GHC_PATH=$HOME/.ghcup/bin
OPAM_PATH=$HOME/.opam/4.08.0/bin:$HOME/.elan/bin:$HOME/gems/bin
SNAP_PATH=/snap/bin
SCALA_PATH=$HOME/opt/SCALA/scala-2.13.3/bin
SPARK_PATH=$HOME/opt/SPARK/spark-3.1.2-bin-hadoop3.2/bin

PATH=$HOME_PATH:$BIN_PATH:$NODE18_PATH:$COURSIER_PATH:$CONDA_PATH:$GHC_PATH:$OPAM_PATH:$SNAP_PATH:$SCALA_PATH:$SPARK_PATH:$PATH



# For agda-algebras:
CABAL_PATH=$HOME/.cabal/bin
export PATH=$CABAL_PATH:$PATH

# For formal-ledger-specifications: 
NIXPATH=$HOME/.nix-profile/bin:/home/williamdemeo/IOHK/ledger-agda/bin
export PATH=$NIXPATH:$PATH

# opam configuration:
test -r $HOME/.opam/opam-init/init.sh && . $HOME/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true


# no longer using this TEX path
# TEX_PATH=$HOME/.texlive2021/bin/x86_64-linux
