#!/bin/bash

if [[ $# -ne 2 ]] ; then
  echo 'usage: ./deploy sauce-user-name my-sql-password'
  exit 0
fi

cabal build pifuxelck-server

ssh -f $1@everythingissauce.com "sudo killall pifuxelck-server; sudo rm /srv/pifuxelck/pifuxelck-server;"

scp dist/build/pifuxelck-server/pifuxelck-server $1@everythingissauce.com:/srv/pifuxelck/pifuxelck-server

ssh -f $1@everythingissauce.com "sudo chown pifuxelck:pifuxelck /srv/pifuxelck/pifuxelck-server; sudo nohup /srv/pifuxelck/pifuxelck-server --port 3000 -mysql-host db.everythingissauce.com --mysql-port 3306 --mysql-user pifuxelck --mysql-password $2 --mysql-db pifuxelck"

exit 0
