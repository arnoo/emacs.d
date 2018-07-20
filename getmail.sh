#!/bin/bash
#LOCKFILE="/home/arno/.emacs.d/mail.lock"
#[[ -e "$LOCKFILE" ]] && echo "Lock present, exiting" && exit
#touch "$LOCKFILE"

function archive {
  echo "====> archiving $m"
  message_id=`grep -i -m 1 '^Message-Id:' "$1" | sed -e 's/^.*\?: <\(.*\)>/\1/'`
  cmd="cmd:move msgid:$message_id  maildir:/Octo_AllMail newname:true"
  echo -e "$cmd\nquit" | mu server &> /dev/null || emacsclient -e "(mu4e~proc-send-command \"$cmd\")" &> /dev/null
}

function archive_matching {
  mu find $* --exec echo | while read m; do
    archive "$m"
  done
}

mbsync -a
pgrep mu && kill `pgrep mu`
mu index

archive_matching maildir:/Octo_INBOX AND 'subject:Reservation' AND 'from:passculture*'
archive_matching maildir:/Octo_INBOX AND 'subject:Osthéo'

sort -u -o ~/.muted-mailids ~/.muted-mailids
mu find maildir:/Octo_INBOX AND NOT to:abetremieux@octo.com AND NOT to:arb@octo.com --exec echo | while read m; do
  echo "=========="
  echo "EMAIL : $m"
  references=`grep -A 100 References: "$m" | grep -B 100 ':' -m2 | head -n -1 | sed -e 's/ /\n/g' | grep -v '^$' | tail -n +2 | sed -e 's/\(^<\|>$\)//g' | sort`
  echo "REFERENCES :"
  echo "$references"
  if [[ `comm -12 <(echo "$references") ~/.muted-mailids | wc -l` -gt 0 ]]; then
      archive $m
  fi
done
#rm -f "$LOCKFILE"
