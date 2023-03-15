case $HOSTNAME in

	nwadmin)
		PROXY=http://193.56.47.8:80
		ssh pise0
		;;

	pise*)
		PROXY=http://129.183.4.13:8080
		WORKDIR=/home_nfs/sartorir/THESE
		DEFAULT_PART="rome7402" # -x pise[18-20]
		DEFAULT_TIME="12:00:00"
		export HWLOC_HOME="/usr/lib64"
		export LLVM_HOME=~/LIBS/llvm-15/install
		export OMB_HOME=~/LIBS/OMB-5.6.3/build
		export NPB_HOME=~/LIBS/NPB-3.4.1/NPB3.4-MPI/bin
		export OPEN64_HOME=~/LIBS/open64-4.2/bin
		export SHAMAN_HOME=~/LIBS/shaman
		module load openmpi/gnu/4.1.4.1
		;;

esac