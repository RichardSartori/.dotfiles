WORKDIR=/home/rsartori/THESE
STOREDIR=/beegfs/rsartori
alias cds="cd $STOREDIR"
export LIBS_DIR=${STOREDIR}/LIBS
module load tools/git
#source /etc/bash_completion.d/git

case $HOSTNAME in

	devel*)
		DEFAULT_PART="routage -C bora"
		DEFAULT_TIME="12:00:00"
		;;

	miriel* | bora*)
		export LLVM_HOME=${LIBS_DIR}/llvm/install
		export MPI_HOME=${LIBS_DIR}/mpi/install
		export CMAKE_HOME=${LIBS_DIR}/cmake/install
		module load compiler/gcc/12.2.0 hardware/hwloc
		#module load build/cmake mpi/openmpi/4.1.5
		;;

esac
