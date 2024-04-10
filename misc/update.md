# One Script To Update Them All

The idea is to have one command for updating/upgrading everything

## shell

execute the following list of commands with sudo

apt update
apt list --upgradable # -a
apt -y upgrade
apt -y autoremove

```bash
alias update="sudo -- sh -c 'apt update && apt list --upgradable && apt -y upgrade && apt -y autoremove'"
```

## emacs

execute the following list of commands programmatically
M-x list-packages S-u y x q
work in progress

```lisp
(progn
	(package-list)
	(package-menu-mark-upgrades)
	(package-menu-execute))
; (package--user-selected-p t) ;; populate package-selected-packages
```

## python

```bash
pip list --format=freeze | cut -d '=' -f 1 | xargs pip install --upgrade
```

## rust

```bash
rustup update
```