#!/bin/sh

mypath="`echo $0 | sed -e 's,[^/]*$,,;s,/$,,;s,^$,.,'`"

ln -s "${mypath}/addpath" "${HOME}/.addpath"
ln -s "${mypath}/bash_login" "${HOME}/.bash_login"
ln -s "${mypath}/bashrc" "${HOME}/.bashrc"
ln -s "${mypath}/packages" "${HOME}/.packages"
ln -s "${mypath}/profile" "${HOME}/.profile"

# ask about ssh
#echo -n "Would you like to hae ssh-agent start up on login? "
#answer=''
#read answer
#if [ "x${answer}" "xyes
#mkdir -p "${HOME}/.ssh
#ln -s "${mypath}/sssha" "${HOME}/.ssh/sssha"


# subversion
mkdir -p "${HOME}/.subversion"
ln -s "${mypath}/subversion-config" "${HOME}/.subversion/config"
