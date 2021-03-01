#!/bin/sh
	count=`curl -su USER:PASSWD https://mail.google.com/mail/feed/atom || echo "<fullcount>unknown number of</fullcount>"`
	count=`echo "$COUNT" | grep -oPm1 "(?<=<fullcount>)[^<]+" `
	echo  "$count"
