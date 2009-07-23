#!/bin/sh

mypath="`echo $0 | sed -e 's,[^/]*$,,;s,/$,,;s,^$,.,'`"
cd ${mypath}
mypath=$PWD

# setup bin
mkdir -p "${HOME}/bin"
for script in musicbox; do
  ln -sf "${mypath}/${script}" "${HOME}/bin"
done

if [ -e "${HOME}/music" ]; then
  echo "You should create a link in ${HOME}/music to the top of your music directory so that musicbox will know where to find music."
fi

ln -sf "${mypath}/addpath" "${HOME}/.addpath"
ln -sf "${mypath}/bash_profile" "${HOME}/.bash_profile"
ln -sf "${mypath}/bash_logout" "${HOME}/.bash_logout"
ln -sf "${mypath}/bashrc" "${HOME}/.bashrc"
ln -sf "${mypath}/packages" "${HOME}/.packages"

# ask about ssh
echo -n "Would you like to have ssh-agent start up on login? (y/N) "
answer=''
read answer
if [ "x${answer}" = "xy" ]; then
  mkdir -p "${HOME}/.ssh"
  ln -sf "${mypath}/sssha" "${HOME}/.ssh/sssha"
  ln -sf "${mypath}/sssha-helper" "${HOME}/.ssh/sssha-helper"
else
  rm -f "${HOME}/.ssh/sssha"
  rm -f "${HOME}/.ssh/sssha-helper"
fi

# subversion
mkdir -p "${HOME}/.subversion"
if [ -f "${HOME}/.subversion/config" ]; then
  /bin/mv "${HOME}/.subversion/config" "${HOME}/.subversion/config.old"
fi
ln -fs "${mypath}/subversion-config" "${HOME}/.subversion/config"
