# .dotfiles
Configuration files used in my terminal

# usage
Clone this repo in your home directory and use symbolic links with the correct name pointing to the config files.
Most configuration files will work everywhere, however some require platform-specific modifications (for exemple, one might want the .gitconfig to include different proxys depending on the platform).
In this situation, they will load a 'host' file in the same directory, that can also be a symbolic link to a file in the 'hosts' directory.
In the following, replace $WHERE with where you cloned the repo (in my case ~/.dotfiles).

| Filename                          | usual location/name |
| --------------------------------- | ------------------- |
| $WHERE/bashrc/config.sh           | ~/.bashrc           |
| $WHERE/gitconfig/config.gitconfig | ~/.gitconfig        |
| $WHERE/emacs/config.el            | ~/.emacs            |
| $WHERE/misc/gdbinit.py            | ~/.gdbinit          |
| $WHERE/misc/ssh.config            | ~/.ssh/config       |

# examples
To use the .bashrc I use at home
```bash
ln -s ~/.dotfiles/bashrc/config.sh ~/.bashrc
ln -s ~/.dotfiles/hosts/home.sh ~/.dotfiles/bashrc/host
```

To use my emacs configuration
```bash
ln -s ~/.dotfiles/emacs/config.el ~/.emacs
```

# TODOs
  1. complete emacs/config.el key bindings (see TODOs inside)
    * https://www.reddit.com/r/emacs/comments/8apn20
    * https://github.com/darkstego/wakib-keys
  2. complete bashrc/config.sh EDITOR export (see TODOs inside)
    * https://wikemacs.org/wiki/Emacs_server
  3. modify bashrc/config.sh update function to update everything
    * apt update + apt upgrade
    * rustup update
    * emacs M-x list-packages S-u x
    * pip install -U