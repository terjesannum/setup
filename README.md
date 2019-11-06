# setup

Misc setup

## macOS prerequisites

```sh
brew install gnu-sed
brew install coreutils
brew install bash
sudo bash -c "echo /usr/local/bin/bash >> /private/etc/shells"
chsh -s /usr/local/bin/bash
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
