#!/bin/bash      # Hey EMACS, this should be in -*- ksh -*- mode
#
# FILE		.bashrc 
# OVERVIEW	This startup is executed every time a bash is executed.
#

#if [ -n "$BASHRCREAD" ]; then
#  return 0
#fi

# define location
export HOST=`/bin/hostname`
case $OSTYPE in
  solaris2* | cygwin)
            case `nslookup $HOST 2> /dev/null | /bin/grep $HOST | /bin/awk '{print $2}'` in
              *.htc.honeywell.com)
                LOCATION=htc
                ;;
              *.mn.mtu.net)
                LOCATION=home
                ;;
              *)
                LOCATION=unknown
                ;;
            esac
            ;;
  linux*)
          OSTYPE=linux
          case `hostname -f` in
            *.htc.honeywell.com)
                                 LOCATION=htc
                                 ;;
            *.treknet.net)
                           LOCATION=htc
                           ;;
            *.mn.mtu.net) 
                          LOCATION=home
                          ;;
            *) 
               LOCATION=unknown
               ;;
          esac
          ;;
  *)
     LOCATION=unknown
     ;;
esac
export LOCATION

if [ $OSTYPE = "cygwin" ]; then
  #set in NT directly export CYGWIN="tty nosmbntsec" #title doesn't seem to help either, binmode will break getenv in emacs, ntea will break permissions for ls
  alias uptime='finger @localhost | grep UpTime'
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

#-------------------------------
# GENERAL ENVIRONMENT VARIALBLES
#-------------------------------
#export GNU_HOST='grinch.htc.honeywell.com'
#export GNU_SECURE="${HOME}/.gnu_hosts"

export PILOTPORT=/dev/pilot
export PILOTRATE=115200


if [ -x "`type -p less`" ]; then
  PAGER="less -is"
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

# cvs aliases here
#alias gnu-cvs='cvs -d /net/packages/gnu/CVS-Repository'
#alias gnu-cvs-status='gnu-cvs status |& egrep -i status | egrep -v Up-to-date'
alias cvs-stat='cvs status | egrep ^File | grep -v Up-to-date'
alias cvsup='cvs update -d -R'

# other stuff here
alias restart-tomcat='(cd / && sudo /etc/init.d/tomcat restart)'
#alias BG='(exec $* &) &'
alias cwd='builtin pwd'
alias clean='/bin/rm -f *~ *.~* .*~ *% core &>/dev/null'
alias eclean='/bin/rm -f *~ *.~* .*~ *% \#* .?E?m?A?c?S?l?O?c?K?.* &>/dev/null'
alias fmclean='/bin/rm -f *.{auto,backup,lck,recover} &>/dev/null'
alias texclean='/bin/rm -f *.{aux,dvi,log} &>/dev/null'
#alias en='enscript -2rG'
#alias en1='enscript -1rG'
alias h='history 15'
alias less='less -is'
#alias lsof=/net/packages/lsof/3.67/sparc-sun-solaris2.5/lsof
#alias lf='ls -F'
#alias la='ls -Fa'
#alias ll='ls -Fl'
#alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
#alias w='w -s'
alias which='type'
alias where='type -all'
alias find-unused-unix='for h in `ypmatch HTC-HOSTS netgroup`; do ping -c 1 $h > /dev/null 2>&1 || echo $h; done'
  
alias rdesktop='\rdesktop -g 1024x768 -K'
alias mn65-rsconsole='rdesktop -u getreal mn65-rsconsole'

#my aliases
case $OSTYPE in
  cygwin)
	  alias ls='ls -F'
	  ;;
  *)
     alias ls='ls -F'
     ;;
esac
alias v='ls -l'
alias ll='ls -l'
alias d='ls -s'
alias bc='bc -l'
alias lsdir='ls -lA | grep "^d"'
alias lsfile='ls -lA | egrep -v "^d|^l"'
alias lslink='ls -lA | grep "^l"'
alias enscript='enscript -2r -b"- jschewe -" -G'
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

#psg() {
#  ps $1 | tee /tmp/psg.$$ | awk 'NR == 1 {print $0}'
#  cat /tmp/psg.$$ | egrep $2
#  /bin/rm -f /tmp/psg.$$
#}

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
startx ()  {
  /usr/X11R6/bin/startx ${1+"$@"} 2>&1 | tee "${HOME}/.X-${HOST}.err"
}


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

if [ -n "`type -p nc`" -a -z "`type -p netcat`" ]; then
  alias netcat=nc
elif [ -n "`type -p netcat`" -a -z "`type -p nc`" ]; then
  alias nc=netcat
fi

dsd() {
  case "$1" in
  "") echo "usage: dsd <dir>" >&2 ;;
  *) sd $1 >/dev/null && popd >/dev/null ;;
  esac
  pdirs
}

case $LOCATION in
  htc)
    #------------------------------
    # Project aliases
    #------------------------------
    alias check-schedinfra="find . \( -name '*.alt' -o -name '*.lib' -o -name '*.java' -o -name '*.template' -o -name '*.el' -o -name '*.html' -o -name '*.vm' -o -name '*.dm' \) -perm +222 -print | grep -v 'examples/newarch' | grep -v 'examples/distributed' | grep -v 'instrumented' | grep -v dome/tools/sched-infra/examples | grep -v blort"

    alias check-ptm="find *.properties src docs web build.xml lib matlab prj.el -type f -perm +222 -print | grep -v 'vssver.scc' | grep -v .xvpics | grep -v foo | grep -v matlab.jar | grep -v web-linux.xml | grep -v ObjectModel.bak"

    alias check-sydney="find src doc build.xml lib prj.el -type f -perm +222 -print | grep -v 'vssver.scc' | grep -v .xvpics | grep -v TAGS | grep -v PConstraintParser | grep -v PConstraintTokenTypes | grep -v PConstraintLexer"

    #------------------------------
    # Check smtp servers
    #------------------------------
    check_smtp() {
      for host in `host smtp.honeywell.com | grep 'has address' | awk '{print $NF}'`; do
        echo $host
        echo "quit" | nc $host 25
      done
    }
    
    ;;
esac

# ------------------
# some terminal stuff
# ------------------
if [ $TERM = "linux" ]; then
   export TERM=vt102
fi
if [ $TERM = xterm ]; then
  PS1="\u@\h:\w\n\[\033]0;\u@\h\007\]>";
elif [ $TERM = "emacs" ]; then
  export PAGER=cat
  alias ls='ls -F'
  alias more='cat'
  alias less='cat'
  PS1="\u@\h:\w\n>";
  export EDITOR=gnuclient
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

# perl - this may be wrong - need to play with it for a while.
if [ $OSTYPE = "cygwin" ]; then
  # could also want this to be "raw"
  export PERLIO="perlio"
fi

if [ \! $OSTYPE = "linux" ]; then
  MOZILLA_HOME=/usr/local/netscape/current/java/classes
  export MOZILLA_HOME
  
  # app-defaults stuff
  export XFILESEARCHPATH="${XFILESEARCHPATH}:${HOME}/lib/%T/%N%S:/net/users/jschewe/lib/app-defaults"
  export DTAPPSEARCHPATH=${DTAPPSEARCHPATH}:/net/users/jschewe/lib/app-defaults
fi

### for scyllarus
# socc
# FIX make add as a  function
CYBERPANEL="${HOME}/projects/argus/code/socc"
export CYBERPANEL
CYPSP="${HOME}/projects/argus/ScenarioPlayer"
export CYPSP

MANPATH=`substpath PATH bin man`
export MANPATH
case $OSTYPE in
  linux) 
     append MANPATH /usr/share/man
     ;;
  sunos4 | solaris2)
     ### procmail on sunos and solaris
     append MANPATH /net/packages/procmail/3.10/man
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

# Setup proxy server
case $LOCATION in
  home)
    export http_proxy="http://eggplant:3128"
    ;;
  htc)
    export http_proxy="http://lug.htc.honeywell.com:3128"
    ;;
  *)
    unset http_proxy
  ;;
esac

# CIRCA
case $LOCATION in
  htc|home)
       export CIRCA_BASENAME='JON'
       export CIRCA_BASEPORT='50000'
       export CIRCA_MM_HOST='localhost'
       export CIRCA_THIS_HOST=$HOST
       export CIRCA_CODE="/net/projects/cortex/code/jschewe"
       export CORTEX_ATTACKER="mn65-cygnus.htc.honeywell.com"
       export CORTEX_DEFENDER="mn65-eggplant.htc.honeywell.com"
       export CORTEX_TASTER="sulu.htc.honeywell.com"
       ;;
esac
        
if [ -f "${HOME}/.ssh/sssha" ]; then
  . "${HOME}/.ssh/sssha"
fi

#
# Avoid loops and such
#
#BASHRCREAD=true
#export BASHRCREAD

