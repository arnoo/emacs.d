#!/bin/bash
mbsync -a
mu index || emacsclient -e '(mu4e-update-index)'
sort -o ~/.muted-mailids ~/.muted-mailids
for m in `mu find maildir:/INBOX OR maildir:/Octo_Inbox --exec echo`; do
  references=`grep -A 100 References: "$m" | grep -B 100 ':' -m2 | head -n -1 | sed -e 's/ /\n/g' | grep -v '^$' | tail -n +2 | sed -e 's/(^<|>$)//' | sort`
  if [[ `comm -12 <(echo "$references") .muted-mailids | wc -l` -gt 0 ]]; then
      echo "Auto archiving"
      break
  fi
done
mu index || emacsclient -e '(mu4e-update-index)'
