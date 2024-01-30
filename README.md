# .dotfiles
Configuration files used in my terminal

# usage
Clone this repo in your home directory and use symbolic links with the correct name pointing to the config files.
Most configuration files will work everywhere, however some require platform-specific modifications (for exemple, one might want the .gitconfig to include different proxys depending on the platform).
In this situation, they will load a 'host' file in the same directory, that can also be a symbolic link to file in the 'hosts' directory.
In the following, replace $WHERE with where you cloned the repo (usually ~).

| Filename                          | usual location/name   |
| --------------------------------- | --------------------- |
| $WHERE/bashrc/config.sh           | ~/.bashrc             |
| $WHERE/bashrc/hosts/*             | $WHERE/bashrc/host    |
| $WHERE/gitconfig/config.gitconfig | ~/.gitconfig          |
| $WHERE/gitconfig/hosts/*          | $WHERE/gitconfig/host |
| $WHERE/emacs/config.el            | ~/.emacs              |
| $WHERE/misc/gdbinit.py            | ~/.gdbinit            |
| $WHERE/misc/ssh.config            | ~/.ssh/config         |

# examples
To use my .bashrc
```bash
ln -s ~/.dotfiles/bashrc/config.sh ~/.bashrc
```

To use the .gitconfig I use at home
```bash
ln -s ~/.dotfiles/gitconfig/config.gitconfig ~/.gitconfig
ln -s ~/.dotfiles/gitconfig/hosts/home.gitconfig ~/.dotfiles/gitconfig/host
```

To use my emacs configuration
```bash
ln -s ~/.dotfiles/emacs/config.el ~/.emacs
```

# TODOs
  1. complete bashrc/hosts/cluster*
  2. complete emacs/config.el key bindings (remove all C-c bindings)
  3. gather all host specific configuration into a single directory