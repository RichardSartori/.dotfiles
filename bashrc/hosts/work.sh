#### PROXY=http://193.56.47.8:80
#### PROXY=http://193.56.47.8:8080
#### PROXY=proxy-fr.glb.my-it-solutions.net:84
#### unset http_proxy # is set to http://193.56.47.8:8080 for some reason
PROXY=http://193.56.47.20:8080
WORKDIR=~/THESIS-gitlab/llvm_pass
SHARED_FOLDER=/mnt/c/SHARED_FOLDER
alias cds="cd $SHARED_FOLDER"
export LIBS_DIR=/home/rsartori/LIBS
export LLVM_HOME=${LIBS_DIR}/llvm-15/install
export MPI_HOME=${LIBS_DIR}/openmpi-4.1.4/install
export DUMPI_HOME=${LIBS_DIR}/sst-dumpi/install
export PGMATH_HOME=${LIBS_DIR}/libpgmath/install
export RUST_HOME=${LIBS_DIR}/rust
function winopen {
	path=`realpath $1`
	rootpath=C:/Users/a800323/AppData/Local/Packages/CanonicalGroupLimited.UbuntuonWindows_79rhkp1fndgsc/LocalState/rootfs/
	cmd.exe /c start $rootpath$path
}
alias evince="winopen"
export _INKSCAPE_GC=disable # inkscape fix
