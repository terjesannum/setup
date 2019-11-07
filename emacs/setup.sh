#!/usr/bin/env bash

DIR=`cd $(dirname $0); pwd -P`
. $DIR/../lib/functions.sh

ln -fs $DIR/dotemacs $HOME/.emacs
mkdir -p $HOME/el
ln -fs $DIR/init.el $HOME/el/init.el

github_clone_or_pull $HOME/el/github.com \
    https://github.com/magnars/dash.el.git \
    https://github.com/spotify/dockerfile-mode.git \
    https://github.com/dominikh/go-mode.el.git \
    https://github.com/mooz/js2-mode.git \
    https://github.com/Wilfred/logstash-conf.el.git \
    https://github.com/jrblevin/markdown-mode.git \
    https://github.com/puppetlabs/puppet-syntax-emacs.git \
    https://github.com/pashky/restclient.el.git \
    https://github.com/yoshiki/yaml-mode.git \
    https://github.com/nikolas/window-number \
    https://github.com/szermatt/emacs-bash-completion.git \
    https://github.com/purcell/exec-path-from-shell.git \
    https://github.com/emacs-pe/docker-tramp.el.git \
    https://github.com/gruggiero/kubernetes-tramp.git \
    https://github.com/terjesannum/cyberpunk-theme.el.git \
    https://github.com/terjesannum/emacs-gcloud-mode-line.git \
    https://github.com/terjesannum/emacs-kubectx-mode.git \
    https://github.com/terjesannum/emacs-influxdb-mode.git \
    https://github.com/terjesannum/emacs-shell.git \
    https://github.com/antonj/Highlight-Indentation-for-Emacs.git \
    https://github.com/hbin/smart-shift.git \
    https://github.com/sshaw/copy-as-format.git \
    https://github.com/Malabarba/emacs-google-this.git \
    https://github.com/ensime/emacs-sbt-mode.git \
    https://github.com/ensime/emacs-scala-mode.git \
    https://github.com/syohex/emacs-hcl-mode.git \
    https://github.com/syohex/emacs-terraform-mode.git
