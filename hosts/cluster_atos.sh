case $HOSTNAME in

	nwadmin)
		PROXY=http://193.56.47.8:80
		;;

	pise*)
		PROXY=http://129.183.4.13:8080
		WORKDIR=/home_nfs/sartorir/THESE
		DEFAULT_PART="milan7513_A100"
		DEFAULT_TIME="12:00:00"
		export LIBS_DIR=/home_nfs/sartorir/LIBS
		export HWLOC_HOME=/usr/lib64
		export LLVM_HOME=${LIBS_DIR}/llvm-15/install
		export SHAMAN_HOME=${LIBS_DIR}/shaman
		export MAKE_HOME=${LIBS_DIR}/make-4.3/install
		module load openmpi/gnu/4.1.5.4
		;;

esac
