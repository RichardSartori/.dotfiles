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
ln -s ~/.dotfiles/hosts/richard-pc-perso.sh ~/.dotfiles/bashrc/host
```

To use my emacs configuration
```bash
ln -s ~/.dotfiles/emacs/config.el ~/.emacs
```

# TODOs
1. complete emacs/config.el (see TODOs inside)
    * remap C-c and C-x: [method 1](https://www.reddit.com/r/emacs/comments/8apn20) or [method 2](https://github.com/darkstego/wakib-keys)
    * complete rust setup: [link](https://robert.kra.hn/posts/rust-emacs-setup/)
2. update misc/update.sh to handle upgrading rust-analyzer (if cloned from repo)

# Acknowledgements

- [Tsoding](https://github.com/tsoding) for his [dotfiles](https://github.com/rexim/dotfiles) and everything else
- [Kelvin Smith](https://github.com/oneKelvinSmith) for [monokai-emacs](https://github.com/oneKelvinSmith/monokai-emacs/blob/master/monokai-theme.el)
- The [LLVM team](https://github.com/orgs/llvm/people) for [llvm-mode](https://github.com/llvm-mirror/llvm/blob/master/utils/emacs/llvm-mode.el)
- [Andrea Cardaci](https://cardaci.xyz/) for his [gdb-dashboard](https://github.com/cyrus-and/gdb-dashboard)