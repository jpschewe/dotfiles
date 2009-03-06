#!/bin/bash      # Hey EMACS, this should be in -*- ksh -*- mode
#
# FILE		.bashrc 
# OVERVIEW	This startup is executed every time a bash is executed.
#

#if [ -n "$BASHRCREAD" ]; then
#  return 0
#fi

#echo "Top of bashrc"

test -z "$UID"  &&  UID=`id -ur 2> /dev/null`
test -z "$EUID" && EUID=`id -u  2> /dev/null`
test -z "$USER" && USER=`id -un 2> /dev/null`
test -z "$HOST" && HOST=`hostname 2> /dev/null`
test -z "$CPU"  &&  CPU=`uname -m 2> /dev/null`
test -z "$HOSTNAME" && HOSTNAME=${HOST}
test -z "$LOGNAME"  && LOGNAME=${USER}
if [ -z "$CPU" ]; then
  case "$CPU" in
    i?86) HOSTTYPE=i386   ;;
    *)    HOSTTYPE=${CPU} ;;
  esac
fi

if [ -z "$OSTYPE" ]; then
  OSTYPE=`uname -s 2> /dev/null | tr A-Z a-z`
fi
# I added OSDIST since suse hardcoded MACHTYPE to be ${CPU}-suse-${OSTYPE}
if [ -z "$OSDIST" ]; then
  if [ -f /etc/fedora-release -o -f /etc/redhat-release ]; then
    OSDIST="redhat"
  elif [ -f /etc/SuSE-release ]; then 
    OSDIST="suse"
  elif [ -f /etc/debian_version ]; then 
    OSDIST="debian"
  else 
    OSDIST="$OSTYPE"
  fi
fi
if [ -z "$MACHTYPE" ]; then
  if [ "$OSDIST" = "$OSTYPE" ]; then 
    MACHTYPE=${CPU}-${OSTYPE}
  else
    MACHTYPE=${CPU}-${OSDIST}-${OSTYPE}
  fi
fi
# Do NOT export UID, EUID, USER, MAIL, and LOGNAME
export HOST CPU HOSTNAME HOSTTYPE OSTYPE MACHTYPE OSDIST

if [ $OSTYPE = "cygwin" ]; then
  # set in NT directly export CYGWIN="tty nosmbntsec"
  # title doesn't seem to help either, binmode will break getenv in emacs, ntea will break permissions for ls
  # set to server to run postgres 
  #alias uptime='finger @localhost | grep UpTime'
  export SHELL="/bin/bash"
  export TMP="/tmp"
  export TEMP="/tmp"
fi

#now actually build my path
# then load packages locations
source "${HOME}/.addpath"
source "${HOME}/.packages"

#------------------------------
# BASH Environment options
#------------------------------
# Set umask to group-write IFF home directory is in my default group
# This might work for places where I DO and DON'T want group-write.
if [ -G "${HOME}" ]; then
  umask 022 # u+rw, og+r
else
  umask 002 # ug+rw, o+r
fi

export IGNOREEOF=0
export FIGNORE=.o:\~:.bak:.class
export HISTFILE=~/.bash_history
export HISTSIZE=500
export command_oriented_history=1
export history_control=ignoredups

notify=
unset MAILPATH MAILCHECK
unset MAIL

export EDITOR='vim'

# turn on core files
ulimit -c unlimited

#-------------------------------
# GENERAL ENVIRONMENT VARIALBLES
#-------------------------------
#export GNU_HOST='grinch.htc.honeywell.com'
#export GNU_SECURE="${HOME}/.gnu_hosts"

export PILOTPORT=:usb
export PILOTRATE=115200


if [ -x "`type -P less`" ]; then
  PAGER="less"
else
  PAGER="more"
fi

#export DoMEPrintFormats=${HOME}/.domePrintFormats
#export DoMEUserFunctions=${HOME}/.domeUserFunctions

#------------------------------
# ALIASES & one-line FUNCTIONS
#------------------------------

export BRC="$HOME/.bashrc"	# also used for test in .profile
alias brc=". '$BRC'"
#alias brc='unset BASHRCREAD ; . "${HOME}/.bashrc"'

# other stuff here
alias restart-tomcat='(cd / && sudo /etc/init.d/tomcat restart)'
#alias BG='(exec $* &) &'
alias cwd='builtin pwd'
alias clean='/bin/rm -f *~ *.~* .*~ *% core &>/dev/null'
alias eclean='/bin/rm -f *~ *.~* .*~ *% \#* .?E?m?A?c?S?l?O?c?K?.* &>/dev/null'
alias fmclean='/bin/rm -f *.{auto,backup,lck,recover} &>/dev/null'
alias texclean='/bin/rm -f *.{aux,dvi,log} &>/dev/null'
alias h='history 15'
alias less='less -is'
#alias lf='ls -F'
#alias la='ls -Fa'
#alias ll='ls -Fl'
alias cp='cp -i'
alias mv='mv -i'
#alias w='w -s'
alias which='type'
alias where='type -all'
  
#alias rdesktop='\rdesktop -g 1024x768 -K'
#alias mn65-rsconsole='rdesktop -u getreal mn65-rsconsole'

#my aliases
alias ls='ls -F'

ls -hl / > /dev/null 2>&1
if [ $? == 0 ]; then
  alias ll='ls -hl'
else
  alias ll='ls -l'
fi
df -h / > /dev/null 2>&1
if [ $? == 0 ]; then
  alias df='df -h'
fi

alias v='ll'
alias d='ls -s'
alias bc='bc -l'
alias lsdir='ls -lA | grep "^d"'
alias lsfile='ls -lA | egrep -v "^d|^l"'
alias lslink='ls -lA | grep "^l"'
alias en='enscript -2r -b"- jschewe -" -G'
#alias en='enscript -2rG'
#alias en1='enscript -1rG'

alias faketop='ps auwx | sort -n -k 3'

#alias = 'echo $TOTAL;'
#alias == 'set TOTAL=\!$;'
#alias + '@ TOTAL+=\!$;'
#alias - '@ TOTAL-=\!$;'
#alias / '@ TOTAL/=\!$;'
#alias * '@ TOTAL*=\!$;'

# Sort on last alpha field
# - see http://www.perl.org/CPAN/doc/FMTEYEWTK/sort.html
#alias psort='perl -0000 -ne \'print ((join "\n", map { $_->[0] } sort { $a->[1] cmp $b->[1] } map {[$_, (split)[-1]] } (split /\n/, $_)) . "\n")\''

#------------------------------
# Multi-line/complex FUNCTIONS
#------------------------------

# CVS functions
cvs-stat() { cvs -n update $* 2>&1 | egrep -v '^cvs update:' ; }

# find that skips over .snapshot directories
sfind() {
  (
    set -o noglob
    shopt -s extglob
    ARGS="$*"
    DIRS="${ARGS/%-*/}"
    ARGS="${ARGS##*([^-])}"
    echo find $DIRS -path */.snapshot -prune -false -o \( $ARGS \) 1>&2
    find $DIRS -path */.snapshot -prune -false -o \( $ARGS \)
  )
}

keepalive() {
  echo "Going into keepalive mode"
  while `/bin/true`; do
    sleep 300
    echo "Keepalive :)"
  done
}

pwd()   { dirs | awk '{print $1}'; }
total() {
  perl -nle 'print; split; $sum+=$_[0]; END{ print "TOTAL:\t$sum\n"}'
}
  
#pushd() { builtin pushd $1; cd .; }
#popd() { builtin popd $1; cd .; }
  
# commands I aught to rember the name of, but don't:
# ldd           - list dynamic dependencies
# pstat -T      - 
# vmstat        -
# /usr/local/X11R5/bin/imake -DUseInstalled -I/usr/local/X11R5/lib/X11/config

#cd()
#{
#        builtin cd $1;
#       export PS1="\u@\h:\w\n\[\033]0;\u@\h\007\]>";   
#       xname $*
#}          

#fdu() {
#    perl -le 'while(<>) {chop;$f++;$b+=((stat($_))[7])};
#                    print "files=$f - bytes=$b"'
#}

psg() {
  ps $1 | tee /tmp/psg.$$ | awk 'NR == 1 {print $0}'
  cat /tmp/psg.$$ | egrep $2
  /bin/rm -f /tmp/psg.$$
}

# findp
# Author: Robert S. Sciuk
whence() {
  targlist=$*
  dirlist=`echo ${PATH} | sed 's/:/ /g'`
  for dir in ${dirlist}; do
    for target in ${targlist}; do
      if [ -x "${dir}/${target}" ]; then
          echo ${dir}/${target}
      fi
    done
  done
}

# Modified from SuSE
#startx ()  {
#  /usr/X11R6/bin/startx ${1+"$@"} 2>&1 | tee "${HOME}/.X-${HOST}.err"
#}


# directory management
pd() {
  case "$1" in
  0) ;;
  [1-9]*) pushd +$1 >/dev/null ;;
  *) pushd "$1" >/dev/null ;;
  esac
}

pdirs() {
    dirs -l -v
}

sd() {
  local num
  case "$1" in
  "") pdirs; return ;;
  [0-9]*) pd $1; pdirs; return $?;;
  esac

  num=$(pdirs | perl -e '$str = $ARGV[1]; while(<STDIN>) { /\Q$str\E$/ || next;  /^\s*(\d+)/; print $1; exit }' - $1)
  case "$num" in
  0) ;;
  "")
    if pushd "$1" >/dev/null 2>&1; then
      pdirs
      return
    fi
    echo "Directory $1 not found" >&2
    pdirs
    return 1
    ;;
  *)
    pd $num
    pdirs
    ;;
  esac
}

# figure out by which name netcat goes by
if [ -n "`type -p nc`" -a -z "`type -p netcat`" ]; then
  alias netcat=nc
elif [ -n "`type -p netcat`" -a -z "`type -p nc`" ]; then
  alias nc=netcat
fi

# aliases for rm to use trash, if available
if [ -n "`type -p trash-put`" ]; then
  alias rm=trash-put
elif [ -n "`type -p trash`" ]; then
  alias rm=trash
else 
  alias rm='rm -i'
fi

dsd() {
  case "$1" in
  "") echo "usage: dsd <dir>" >&2 ;;
  *) sd $1 >/dev/null && popd >/dev/null ;;
  esac
  pdirs
}

# ------------------
# some terminal stuff
# ------------------
if [ $TERM = "linux" ]; then
   export TERM=vt102
fi
if [ $TERM = xterm ]; then
  PS1="\u@\h:\w\n\[\033]0;\u@\h\007\]>";
elif [ $TERM = xterm-color ]; then
  PS1="\u@\h:\w\n\[\033]0;\u@\h\007\]>";
elif [ $TERM = "sun-nic" ]; then
  alias ls='ls -F'
  PS1="\u@\h:\w\n>";
elif [ $TERM = "sun" ]; then
  alias ls='ls -F'
  PS1="\u@\h:\w\n>";
else 
  PS1="\u@\h:\w\n>";
fi
export PS1

if [ -n "$EMACS" -o "$TERM" = "emacs" ]; then
  export PAGER=cat
  alias ls='ls -F'
  alias more='cat'
  alias less='cat'
  PS1="\u@\h:\w\n>";
  export EDITOR=gnuclient
fi

# perl - this may be wrong - need to play with it for a while.
if [ $OSTYPE = "cygwin" ]; then
  # could also want this to be "raw"
  export PERLIO="perlio"
fi

MANPATH1=`substpath PATH bin man`
MANPATH2=`substpath PATH bin share/man`
if [ -z "${MANPATH1}" ]; then
  MANPATH=${MANPATH2}
else
  if [ -z "${MANPATH2}" ]; then
    MANPATH=${MANPATH1}
  else
    MANPATH=${MANPATH1}:${MANPATH2}
  fi
fi

export MANPATH
case $OSTYPE in
  linux | darwin*) 
     append MANPATH /usr/share/man
     ;;
  sunos4 | solaris2)
     ### gnu-misc
     append MANPATH ${GNU_MISC_DIR}/share/man
     ;;
  *)
     ;;
esac

INFOPATH=`substpath PATH bin info`
export INFOPATH
case $OSTYPE in
  linux) 
     append INFOPATH /usr/share/info
     ;;
  *)
     ;;
esac

# X-Windows
if [ -d "$X11DIR" ]; then
  export X11INC_DIR=${X11DIR}/include
  export X11LIB_DIR=${X11DIR}/lib
  if [ -z "$DISPLAY" ]; then
    export DISPLAY="`who -m --lookup | sed -e 's/.*(\(.*\))/\1/'`:0.0"
    if [ -z "$DISPLAY" ]; then
      export DISPLAY=":0.0"
    fi
  fi
fi

if [ -f /usr/lib/ssh/gnome-ssh-askpass ]; then
  SSH_ASKPASS=/usr/lib/ssh/gnome-ssh-askpass
elif [ -f /usr/lib64/ssh/gnome-ssh-askpass ]; then
  SSH_ASKPASS=/usr/lib64/ssh/gnome-ssh-askpass
elif [ -f /usr/lib/ssh/ssh-askpass ]; then
  SSH_ASKPASS=/usr/lib/ssh/ssh-askpass
elif [ -f /usr/lib64/ssh/ssh-askpass ]; then
  SSH_ASKPASS=/usr/lib64/ssh/ssh-askpass
fi
export SSH_ASKPASS


if [ -x "${HOME}/.ssh/sssha" ]; then
  . ${HOME}/.ssh/sssha
fi

#
# Avoid loops and such
#
#BASHRCREAD=true
#export BASHRCREAD

