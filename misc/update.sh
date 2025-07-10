#!/bin/bash

TRASH="$HOME/.trash"
delete() {
	mv 2>/dev/null --backup=numbered -t "$TRASH" "$1"
}

LOCAL_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
LOG_FILE="$LOCAL_DIR/update.log"
delete "$LOG_FILE"

echo "=== Update & Upgrade ===" | tee -a "$LOG_FILE"
echo "date: $(date)" | tee -a "$LOG_FILE"
echo "log file: $LOG_FILE" | tee -a "$LOG_FILE"

# log and execute a command
run_command() {
	echo "Running: $1" | tee -a "$LOG_FILE"
	eval "$1" 2>&1 | tee -a "$LOG_FILE"
	if [ ${PIPESTATUS[0]} -ne 0 ]; then
		echo "Error: Command failed - $1" | tee -a "$LOG_FILE"
		notify-send "Update Script Error" "Command failed: $1" -u critical
		exit 1
	fi
}

# 1. Update and upgrade apt packages
run_command "sudo -- sh -c 'apt update && apt -y full-upgrade && apt -y autoremove'"

# 2. Update Python packages
PIP_OPTIONS=""
run_command "python3 -m pip install --upgrade $PIP_OPTIONS pip"
REQUIREMENTS="$LOCAL_DIR/requirements.txt"
run_command "pip list --outdated --format=columns | awk 'NR>2 {print $1}' > $REQUIREMENTS"
if [ ! -e "$REQUIREMENTS" ]; then
	run_command "cat $REQUIREMENTS | xargs -n1 pip install --upgrade $PIP_OPTIONS"
fi
delete "$REQUIREMENTS"

# 3. Update Emacs packages
run_command "emacs --batch --eval '(progn (load-file \"~/.emacs\") (update-packages))'"

# 4. Update Rust installation
run_command "rustup update"

# 5. TODO: Update the rust analyzer alongside
####https://robert.kra.hn/posts/rust-emacs-setup/
####```bash
####cd ${LIBS_DIR}/rust/rust-analyzer
####git pull
####cargo xtask install --server
####```

# 6. Update Discord
if [ ! -z `which discord` ]; then
	DISCORD_URL="https://discord.com/api/download/stable?platform=linux&format=deb"
	DISCORD_DEB="$LOCAL_DIR/discord.deb"
	INSTALLED_VERSION=$(dpkg-query -W -f='${Version}\n' discord)
	LATEST_VERSION=$(curl -sI "$DISCORD_URL" | grep -oP "discord-([0-9\.]+)\.deb" | grep -oP "[0-9\.]+[0-9]")
	if [ -z "$LATEST_VERSION" ]; then
		run_command "Could not determine the latest Discord version | false"
	fi
	if [ "$INSTALLED_VERSION" == "$LATEST_VERSION" ]; then
		echo "Discord is already up to date" | tee -a "$LOG_FILE"
	else
		run_command "wget --no-verbose -O $DISCORD_DEB $DISCORD_URL"
		run_command "sudo apt install -y $DISCORD_DEB"
		delete "$DISCORD_DEB"
	fi
fi

#TODO: add weekly reminder and mute KDE Discover notifications

echo "Update completed successfully." | tee -a "$LOG_FILE"

exit 0
