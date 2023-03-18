case $HOSTNAME in

	devel*)
		DEFAULT_PART="routage -C miriel"
		DEFAULT_TIME="12:00:00"
		;;

	miriel*)
		WORKDIR=/home/rsartori/THESE
		STOREDIR=/beegfs/rsartori
		alias cds="cd $STOREDIR"
		export LLVM_HOME=${STOREDIR}/INSTALL
		module load compiler/gcc hardware/hwloc build/cmake mpi/openmpi
		;;

esac
