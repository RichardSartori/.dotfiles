# .config_files
Configuration files used in my terminal

# usage
I usually clone this repo in my home and use symbolic links that point to this repo.
All of them require the .bashrc to be set to work properly

| Filename    | usual location/name  |
| ----------- | -------------------- |
| bashrc.sh   | ~/.bashrc            |
| gitconfig/* | ~/.git_config        |
| hosts/*     | ~/.config_files/host |

# examples
To use my .bashrc
```bash
ln -s ~/.config_files/bashrc ~/.bashrc
```

To set environment variables I use at home
```bash
ln -s ~/.config_files/hosts/home ~/.config_files/host
```