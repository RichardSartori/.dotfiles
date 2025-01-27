WORKDIR=/path/to/work/directory
STOREDIR=/path/to/high/volume/storage
alias cds="cd $STOREDIR"
export LIBS_DIR=/path/to/fast/storage/with/libs
source /etc/bash_completion.d/git

case $HOSTNAME in

	front_node)
		DEFAULT_PART="partition -C constraint"
		DEFAULT_TIME="duration"
		module load compilers build_tools
		;;

	compute_node*)
		export LIB_HOME=${LIBS_DIR}/path/to/lib
		module load libraries
		;;

esac
