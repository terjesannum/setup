#!/usr/bin/env bash

DIR=`cd $(dirname $0); pwd -P`
. $DIR/../lib/functions.sh

ln -fs $DIR/profile $HOME/.profile
ln -fs $DIR/bash_profile $HOME/.bash_profile
ln -fs $DIR/inputrc $HOME/.inputrc
ln -fs $DIR/bashrc $HOME/.bashrc

INSTDIR=$HOME/lib/bash/github.com

github_clone_or_pull $INSTDIR https://github.com/cykerway/complete-alias.git
github_clone_or_pull $INSTDIR https://github.com/ahmetb/kubectx.git
github_clone_or_pull $INSTDIR https://github.com/politza/ealias.git

mkdir -p $HOME/bin
ln -fs $HOME/lib/bash/github.com/kubectx/kubectx $HOME/bin/kubectx
ln -fs $HOME/lib/bash/github.com/kubectx/kubens $HOME/bin/kubens
