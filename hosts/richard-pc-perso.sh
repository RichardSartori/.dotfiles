PERSODIR=~/Documents/DEV
WORKDIR=${PERSODIR}
alias cdd="cd ~/Downloads"
export LIBS_DIR=~/Documents/DEV/Libraries
export RUSTUP_HOME=${LIBS_DIR}/rust/rustup
export CARGO_HOME=${LIBS_DIR}/rust/cargo
export BAZZITE_HOME=~/.local

case $HOSTNAME in

	bazzite)
		alias deb="distrobox enter debian"
		;;

	debian)
		alias deb="logout"
		;;

esac
