#!/bin/bash
mbsync -a
mu index || emacsclient -e '(mu4e-update-index)'
sort -u -o ~/.muted-mailids ~/.muted-mailids
mu find maildir:/Octo_INBOX AND NOT to:abetremieux@octo.com AND NOT to arb@octo.com --exec echo | while read m; do
  echo "EMAIL : $m"
  references=`grep -A 100 References: "$m" | grep -B 100 ':' -m2 | head -n -1 | sed -e 's/ /\n/g' | grep -v '^$' | tail -n +2 | sed -e 's/\(^<\|>$\)//g' | sort`
  if [[ `comm -12 <(echo "$references") ~/.muted-mailids | wc -l` -gt 0 ]]; then
      echo "Auto archiving"
      message_id=`grep -i -m 1 '^Message-Id:' "$m" | sed -e 's/^.*\?: <\(.*\)>/\1/'`
      cmd="cmd:move msgid:$message_id  maildir:/Octo_AllMail newname:true"
      echo -e "$cmd\nquit" | mu server &> /dev/null || emacsclient -e "(mu4e~proc-send-command \"$cmd\")" &> /dev/null
  fi
done
