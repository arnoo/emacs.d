#!/bin/bash
#LOCKFILE="/home/arno/.emacs.d/mail.lock"
#[[ -e "$LOCKFILE" ]] && echo "Lock present, exiting" && exit
#touch "$LOCKFILE"

mbsync -a
sieve-filter -C -W -e ~/.sieve/main INBOX
sieve-filter -C -W -e ~/.sieve/main Octo_INBOX # For CLI dry-run tests, remove -W and -e
sieve-filter -C -W -e ~/.sieve/mutes Octo_INBOX
pkill mu
mu index

#rm -f "$LOCKFILE"
