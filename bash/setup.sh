#!/usr/bin/env bash

DIR=`cd $(dirname $0); pwd -P`
. $DIR/../lib/functions.sh

ln -fs $DIR/profile $HOME/.profile
ln -fs $DIR/bash_profile $HOME/.bash_profile
ln -fs $DIR/inputrc $HOME/.inputrc
ln -fs $DIR/bashrc $HOME/.bashrc

github_clone_or_pull $HOME/lib/bash/github.com \
                     https://github.com/cykerway/complete-alias.git \
                     https://github.com/ahmetb/kubectx.git \
                     https://github.com/politza/ealias.git

mkdir -p $HOME/bin
ln -fs $HOME/lib/bash/github.com/kubectx/kubectx $HOME/bin/kubectx
ln -fs $HOME/lib/bash/github.com/kubectx/kubens $HOME/bin/kubens
