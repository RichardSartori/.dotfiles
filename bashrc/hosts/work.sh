#### PROXY=http://193.56.47.20:8080 # Atos
unset http_proxy # Inria
WORKDIR=~/THESIS-gitlab/llvm_pass
SHARED_FOLDER=/mnt/c/SHARED_FOLDER
alias cds="cd $SHARED_FOLDER"
export LIBS_DIR=/home/rsartori/LIBS
export LLVM_HOME=${LIBS_DIR}/llvm-15/install
export MPI_HOME=${LIBS_DIR}/openmpi-4.1.4/install
export DUMPI_HOME=${LIBS_DIR}/sst-dumpi/install
export PGMATH_HOME=${LIBS_DIR}/libpgmath/install
export CMAKE_HOME=${LIBS_DIR}/cmake-3.26.3/install
export RUSTUP_HOME=${LIBS_DIR}/rust/rustup
export CARGO_HOME=${LIBS_DIR}/rust/cargo
function winopen {
	path=`realpath $1`
	rootpath=C:/Users/a800323/AppData/Local/Packages/CanonicalGroupLimited.UbuntuonWindows_79rhkp1fndgsc/LocalState/rootfs/
	cmd.exe /c start $rootpath$path
}
alias evince="winopen"
export _INKSCAPE_GC=disable # inkscape fix
