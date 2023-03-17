# .config_files
Configuration files used in my terminal

# usage
I usually clone this repo in my home and use symbolic links that point to this repo.

| Filename         | usual location/name         |
| ---------------- | --------------------------- |
| bashrc/bashrc.sh | ~/.bashrc                   |
| bashrc/hosts/*   | ~/.config_files/bashrc/host |
| gitconfig/*      | ~/.git_config               |
| misc/gdbinit.py  | ~/.gdbinit                  |
| emacs/config.el  | ~/.emacs                    |

# examples
To use my .bashrc
```bash
ln -s ~/.config_files/bashrc/bashrc.sh ~/.bashrc
```

To set environment variables I use at home (require the .bashrc to be set to work properly)
```bash
ln -s ~/.config_files/bashrc/hosts/home ~/.config_files/bashrc/host
```

To use my emacs configuration
```bash
ln -s ~/.config_files/emacs/config.el ~/.emacs
```

# TODOs
  1. complete gitconfig files
  2. .ssh/config