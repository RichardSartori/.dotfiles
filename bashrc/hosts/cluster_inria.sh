WORKDIR=/home/rsartori/THESE
STOREDIR=/beegfs/rsartori
alias cds="cd $STOREDIR"

case $HOSTNAME in

	devel*)
		DEFAULT_PART="routage -C bora"
		DEFAULT_TIME="12:00:00"
		;;

	miriel* | bora*)
		export LLVM_HOME=${STOREDIR}/LLVM-on-PlaFRIM/install-15.0.1
		module load compiler/gcc hardware/hwloc build/cmake mpi/openmpi
		;;

esac
