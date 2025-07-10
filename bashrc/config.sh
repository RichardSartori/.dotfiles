# source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# check interactivity
case $- in
	*i*) ;;
	*) return ;;
esac

# source plateform specific values
location=`echo ~/.bashrc | xargs realpath | xargs dirname`
if [ -f $location/host ]; then
	source $location/host
else
	echo "unknown host"
fi

# non blinking cursor
printf "\033[?12l"

# squeue aliases & functions
if ! [ -z `which 2>/dev/null squeue` ]; then
	alias squeue="squeue -o \"%8i %9P %15j %8u %8T %10M %10l %19R %S\""
	alias fr="sinfo -t IDLE"
	alias wh="squeue -u $USER"
	alias log="ssh \`\\squeue | grep $USER | head -n 1 | tr -s ' ' | cut -d ' ' -f 9\`"
	function mods { module avail 2>&1 | grep -i "$@" 2>/dev/null;}
	function req {
		part=""
		if ! [ -z "$DEFAULT_PART" ]; then part="-p $DEFAULT_PART"; fi
		time=""
		if ! [ -z "$DEFAULT_TIME" ]; then time="--time $DEFAULT_TIME"; fi
		salloc -N 1 --exclusive $part $time
	}
fi

# set proxy if defined
if ! [ -z "$PROXY" ]; then
	export http_proxy=$PROXY
	#### `echo $PROXY | sed "s/http/https/g"`
	export https_proxy=$PROXY
fi

# history control
HISTCONTROL=erasedups
HISTSIZE=2000
HISTFILESIZE=2000
HISTTIMEFORMAT="%R  "

# shell options
shopt -s histappend
shopt -s checkwinsize
#### shopt -s autocd

# frequently used directories
STARTDIR=~
if ! [ -z "$WORKDIR" ]; then
	alias cdw="cd $WORKDIR"
	STARTDIR=$WORKDIR
fi
if ! [ -z "$PERSODIR" ]; then
	STARTDIR=$PERSODIR
	alias cdp="cd $PERSODIR"
fi
cd $STARTDIR

# completion features
if ! shopt -oq posix; then
	if [ -f /usr/share/bash-completion/bash_completion ]; then
		. /usr/share/bash-completion/bash_completion
	elif [ -f /etc/bash_completion ]; then
		. /etc/bash_completion
	fi
fi

# export paths
for path in `env | grep "_HOME" | cut -d '=' -f 2`; do
	export PATH=${path}/bin:${PATH}
	export CPATH=${path}/include:${CPATH}
	export LIBRARY_PATH=${path}/lib:${LIBRARY_PATH}
	export LD_LIBRARY_PATH=${path}/lib:${LD_LIBRARY_PATH}
done

# export number of processors/threads
function set_num {
	export OMP_NUM_THREADS=$@
	export MKL_NUM_THREADS=$@
	export STARPU_NCPUS=$@
}
set_num `nproc`

# export language
export LC_ALL=en_US.UTF-8

# bash colors
bold=$(tput bold)
reset=$(tput sgr0)
red=${bold}$(tput setaf 1)
green=${bold}$(tput setaf 2)
yellow=${bold}$(tput setaf 3)
blue=${bold}$(tput setaf 4)
purple=${bold}$(tput setaf 5)
cyan=${bold}$(tput setaf 6)
white=${reset}$(tput setaf 231)
#### random_color=${bold}$(tput setaf $((RANDOM % 6 + 1)))
#### $(shuf -i 1-6 -n 1)

# export pager style
export PAGER="less -FMR"
export GROFF_NO_SGR=1 # for Konsole
export LESS_TERMCAP_md=${yellow}		# identifiers style
export LESS_TERMCAP_us=${cyan}			# parameters style
export LESS_TERMCAP_so=$'\033[30;107m'	# last line style
export LESS_TERMCAP_me=${reset}			# reset identifiers style
export LESS_TERMCAP_ue=${reset}			# reset parameters style
export LESS_TERMCAP_se=${reset}			# reset last line style

# export man style
export MANPAGER="$PAGER +Gg"

# export editor
export EMACS_SERVER="emacs_server"
emacs --bg-daemon=$EMACS_SERVER 2>/dev/null
export EDITOR="emacsclient -nw --socket-name=$EMACS_SERVER"
# use `$EDITOR -n` to open file without leaving the terminal

# ssh aliases
####alias plafrim="ssh_key_setup && ssh plafrim"
####alias dalton="ssh_key_setup && ssh dalton"
function ssh_key_setup {
	if [ -z $SSH_AGENT_PID ]; then
		eval "$(ssh-agent)"
		ssh-add
	fi
}

# scp aliases
function myscp {
	rec=""
	if [ -d $2 ] ; then rec="-r"; fi
	scp -l 1000 $rec $2 $1
}
#### alias sendtoplafrim="myscp rsartori@plafrim:/home/rsartori"
#### alias sendtodalton="myscp rsartori@dalton:/home/rsartori"

# avoid mistakes
TRASH=~/.trash
mkdir -p $TRASH
alias cp="cp -i"
alias mv="mv -i"
alias rm="\mv 2>/dev/null --backup=numbered -t $TRASH"
alias del="\rm 2>/dev/null -rf"
alias empty="del $TRASH/* $TRASH/.*"
alias cdt="cd $TRASH"

# alias for rust replacements
if ! [ -z `which 2>/dev/null exa` ]; then
	alias ls_local="exa"
	alias ls_nosort="exa"
else
	alias ls_local="\ls --color=auto -v"
	alias ls_nosort="\ls -U"
fi
if ! [ -z `which 2>/dev/null rg` ]; then
	alias grep="rg"
fi
if ! [ -z `which 2>/dev/null fd` ]; then
	alias find="fd"
fi

# ls aliases
ls_max_files_shown=100
function ls_max {
	options="" ; dirs="" ; count=0 ; force=0 ; rec=""
	while [[ $# -gt 0 ]]; do
		arg=$1
		case $arg in
			--force) shift ; force=1 ;;
			-R) shift ; options="$options $arg" ; rec="-R" ;;
			-*) shift ; options="$options $arg" ;;
			*) shift ; dirs="$dirs $arg" ; c=`ls_nosort $arg 2>/dev/null | wc -l` ; count=$((count+c)) ;;
		esac
	done
	if [ "$dirs" = "" ]; then count=`ls_nosort $rec . 2>/dev/null | wc -l`; fi
	if [[ $count -gt $ls_max_files_shown ]] && [[ $force -ne 1 ]]
	then
		echo "too many files ($count), use --force"
	else
		ls_local $options $dirs
	fi
}
alias ls="ls_max --group-directories-first"
alias ll="ls -ahl"
alias l.="ls -d .*"

# I'm a bad typer
alias {dc,vf,xs}="cd"
alias {sl,ks,ms}="ls"
alias {df,gh}="fg" ; alias dfdf="\df"
alias {maje,mkae,amke,maek}="make"
alias lire="more"

# tar functions
function compress {
	d=`basename $1 2>/dev/null`
	if [ $# -eq 0 ] || [ ! -d "$d" ]
	then
		echo "need a directory"
	else
		tar cvzf "${d}.tar.gz" $d
	fi
}
function extract {
	if [ $# -eq 1 ] && [[ "$1" =~ \.tar\.gz$ ]]
	then
		tar xvzf $1
	else
		echo "need an archive"
	fi
}

# simple functions
function mkcd { mkdir -p $@ && cd $@;}
function die { kill -9 `ps a -o pid,cmd --sort=pid | grep -i "$@" | head -n 1 | sed "s: *\([^ ].*\):\1:g" | cut -d " " -f 1` 2>/dev/null;}
function up { arg=$1; if [ $# -eq 0 ]; then arg=1; fi; dst="./"; while [ $arg -gt 0 ]; do dst="$dst../"; arg=$((arg-1)); done; cd $dst;}
function boxcheck { ack --filter --passthru --color-match=red "Overfull|Underfull";}
function mpidebug { if [ $# -eq 1 ]; then n=1; app=$1; else n=$1; app=$2; fi; mpiexec -np $n xterm -e gdb $app;}
function svgtopdf { if [ $# -eq 0 ]; then echo "missing input"; else inkscape -D -z --file=$1.svg --export-pdf=$1.pdf; fi;}
function vgdb {
	valgrind --vgdb=yes --vgdb-error=0 $1 &
	echo "gdb $1"; gdb -ex "target remote | vgdb" $1
}

# less simple functions
if ! [ -z `which 2>/dev/null rustc` ]; then
	function rust? {
		if [[ "$1" =~ "crate" ]]
		then
			firefox --new-tab "site:docs.rs $2 - Rust - Doc.rs"
		else
			firefox --new-tab "site:doc.rust-lang.org $1 - Rust"
		fi
	}
	export CLIPPY_ARGS="-W clippy::pedantic -W clippy::nursery -W clippy::unwrap_used"
	alias clippy="cargo clippy -- $CLIPPY_ARGS"
fi
if ! [ -z `which 2>/dev/null bat` ]; then
	function mybatcat {
		wc=`cat 2>/dev/null $@ | wc -l`
		wc=$((wc+4))
		max=`tput lines`
		options=""
		if [ $wc -ge $max ]; then options="+Gg"; fi
		bat --style=header,grid,numbers --paging=always --pager="less -RMF $options" $@
	}
	alias more="mybatcat"
fi

# defaults parameters
alias tree="ls -T"
alias valgrind="valgrind --leak-check=yes -v --track-origins=yes --show-reachable=yes"
alias python="python3 -q"
alias make="make --no-print-directory"

cstandard="-std=c99"
cppstandard="-std=c++20"
fstandard="-std=f2018"

gccflags="-Wall -Wextra -Werror -fmax-errors=1"
alias gcc="gcc ${cstandard} ${gccflags}"
alias gpp="g++ ${cppstandard} ${gccflags}"
alias gfortran="gfortran ${fstandard} ${gccflags}"

clangflags="-Wall -Wextra -Werror -ferror-limit=1"
alias clang="clang ${cstandard} ${clangflags}"
alias clang++="clang++ ${cppstandard} ${clangflags} ${CLANG_GCC}"
alias flang="flang ${fstandard} -Werror"

# other aliases
alias net="rm *~ .*~ \#* .\#*"
alias m="$EDITOR"
alias c="cd -"
alias ram="ps a -o cmd=COMMAND,pid=ID,stat=STATE,rss=USED,vsz=ALLOCATED --sort=-vsz | grep '\(^/\)\|\(ps a -o\)\|\(grep\)' -v"
alias hellothere="echo 'General Kenobi!'"
alias update="$location/../misc/update.sh"
alias mpimonitoring="mpiexec --mca pml_monitoring_enable 1 --mca pml_monitoring_filename comms --mca pml_monitoring_enable_output 3"

# colors for PS1
redp="\[${red}\]"
greenp="\[${green}\]"
yellowp="\[${yellow}\]"
bluep="\[${blue}\]"
purplep="\[${purple}\]"
cyanp="\[${cyan}\]"
whitep="\[${white}\]"

# bash prompt
pwd_max_length=40
function set_PS1 {
	host="$yellowp[$redp$HOSTNAME$yellowp] "
	pwd=`echo $PWD | sed s:$HOME:~:g`
	tmp=`echo $pwd | wc -c`
	if [ $tmp -gt $pwd_max_length ]
	then pwd=`echo $pwd | cut -c$((tmp-pwd_max_length+4))-`; pwd="<?> $pwd"; fi
	pwd="$greenp$pwd "
	tmp=`git branch 2>/dev/null | grep "[*]" | cut -b 3-`
	if [ `echo $tmp | wc -c` -eq 1 ]
	then git=""; else git="$cyanp($tmp) "; fi
	tmp=`date +%H:%M:%S`
	hour="$purplep$tmp "
	if [ `whoami` == root ]
	then tmp="root:"; else tmp="$"; fi
	prompt="$bluep$tmp$whitep "
	PS1="$host$pwd$git$hour$prompt"
}
export PROMPT_COMMAND="set_PS1"
