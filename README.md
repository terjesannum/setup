# setup

Misc setup

## macOS prerequisites

```sh
brew install coreutils
brew install gnu-sed
brew install gnu-tar
brew install grep
brew install bash
brew install git
sudo bash -c "echo $(brew --prefix)/bin/bash >> /private/etc/shells"
chsh -s $(brew --prefix)/bin/bash
brew install bash-completion@2
```

### Perl local lib setup

```sh
mkdir -p $HOME/perl5
PERL_MM_OPT="INSTALL_BASE=$HOME/perl5" cpan local::lib
```

## Windows prerequisites

* Install Linux subsystem
* Upgrade emacs
* Install VcXsrv
