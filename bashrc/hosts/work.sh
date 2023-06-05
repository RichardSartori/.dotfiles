#### PROXY=http://193.56.47.20:8080 # Atos
unset http_proxy # Inria
WORKDIR=~/THESIS-gitlab/llvm_pass
SHARED_FOLDER=/mnt/c/SHARED_FOLDER
alias cds="cd $SHARED_FOLDER"
export LIBS_DIR=/home/rsartori/LIBS
export LLVM_HOME=${LIBS_DIR}/llvm-15/install
export MPI_HOME=${LIBS_DIR}/openmpi-4.1.5/install
export PGMATH_HOME=${LIBS_DIR}/libpgmath/install
export CMAKE_HOME=${LIBS_DIR}/cmake-3.26.4/install
export RUSTUP_HOME=${LIBS_DIR}/rust/rustup
export CARGO_HOME=${LIBS_DIR}/rust/cargo
alias evince="wslview"
export _INKSCAPE_GC=disable # inkscape fix
