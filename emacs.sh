#!/bin/zsh

UID=$(id -g $USER)
FILE="/tmp/emacs$UID/server"

if [ -S $FILE ]; then
else
	FILE="/tmp/$USER/emacs$UID/server"
fi

if [ -z "$INSIDE_EMACS" ]; then
	if [ -z "$@" ]; then
		TERM=xterm-256color emacsclient -nw -s "$FILE" -e "(transparency 92)"
	else
		TERM=xterm-256color emacsclient -nw -s "$FILE" -e "(progn (find-file \"$@\") (transparency 92))"
	fi
else
	emacsclient -n -s "/tmp/emacs$UID/server" "$@"
fi
