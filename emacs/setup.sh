#!/usr/bin/env bash

DIR=`cd $(dirname $0); pwd -P`
. $DIR/../lib/functions.sh

mkdir -p $HOME/.emacs.d/github.com
ln -fs $DIR/init.el $HOME/.emacs.d/init.el

INSTDIR=$HOME/.emacs.d/github.com

github_clone_or_pull $INSTDIR https://github.com/terjesannum/org-mac-iCal.git
github_clone_or_pull $INSTDIR https://github.com/terjesannum/emacs-gcloud-mode.git
github_clone_or_pull $INSTDIR https://github.com/terjesannum/emacs-kubectx-mode.git
github_clone_or_pull $INSTDIR https://github.com/terjesannum/emacs-influxdb-mode.git
github_clone_or_pull $INSTDIR https://github.com/terjesannum/emacs-shell.git
