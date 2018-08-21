#!/usr/bin/env bash

INSTALL_DIR=$HOME
DIR=`cd $(dirname $0); pwd -P`
. $DIR/../lib/functions.sh

cp $DIR/profile $HOME/.profile
cp $DIR/bash_profile $HOME/.bash_profile
cp $DIR/bashrc $HOME/.bashrc
cp $DIR/inputrc $HOME/.inputrc

github_clone_or_pull $INSTALL_DIR/lib/bash/github.com \
                     https://github.com/cykerway/complete-alias.git \
                     https://github.com/ahmetb/kubectx.git \
                     https://github.com/politza/ealias.git

mkdir -p $HOME/bin
ln -fs $INSTALL_DIR/lib/bash/github.com/kubectx/kubectx $HOME/bin/kubectx
ln -fs $INSTALL_DIR/lib/bash/github.com/kubectx/kubens $HOME/bin/kubens
