#!/bin/sh

mypath="`echo $0 | sed -e 's,[^/]*$,,;s,/$,,;s,^$,.,'`"

ln -sf "${mypath}/addpath" "${HOME}/.addpath"
ln -sf "${mypath}/bash_login" "${HOME}/.bash_login"
ln -sf "${mypath}/bashrc" "${HOME}/.bashrc"
ln -sf "${mypath}/packages" "${HOME}/.packages"
ln -sf "${mypath}/profile" "${HOME}/.profile"

# ask about ssh
echo -n "Would you like to hae ssh-agent start up on login? (y/N) "
answer=''
read answer
if [ "x${answer}" = "xy" ]; then
  mkdir -p "${HOME}/.ssh"
  ln -sf "${mypath}/sssha" "${HOME}/.ssh/sssha"
else
  rm -f "${HOME}/.ssh/sssha"
fi

# subversion
mkdir -p "${HOME}/.subversion"
ln -fs "${mypath}/subversion-config" "${HOME}/.subversion/config"
