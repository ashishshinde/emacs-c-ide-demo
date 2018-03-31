#!/bin/zsh

UID=$(id -g $USER)
FILE="/tmp/emacs$UID/server"
if [ -f $FILE ]; then
else
	FILE="/tmp/$USER/emacs$UID/server"
fi

if [ -z "$INSIDE_EMACS" ]; then
	if [ -z "$@" ]; then
		emacsclient -c -s "$FILE" -e "(transparency 94)"
	else
		emacsclient -c -s "$FILE" -e "(progn (find-file \"$@\") (transparency 95))"
	fi
else
	emacsclient -n -s /tmp/emacs1000/server "$@"
fi
