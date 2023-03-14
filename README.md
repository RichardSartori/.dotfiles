# .config_files
Configuration files used in my terminal

# usage
I usually clone this repo in my home and use symbolic links that point to this repo.

| Filename    | usual location/name  |
| ----------- | -------------------- |
| bashrc      | ~/.bashrc            |
| hosts/*     | ~/.config_files/host |
| gitconfig/* | ~/.git_config        |
| gdbinit.py  | ~/.gdbinit           |

# examples
To use my .bashrc
```bash
ln -s ~/.config_files/bashrc ~/.bashrc
```

To set environment variables I use at home (require the .bashrc to be set to work properly)
```bash
ln -s ~/.config_files/hosts/home ~/.config_files/host
```

TODO: .emacs
TODO: .ssh/config