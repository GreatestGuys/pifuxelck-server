#!/bin/bash

if [[ $# -ne 2 ]] ; then
  echo 'usage: ./deploy sauce-user-name my-sql-password'
  exit 0
fi

cabal build pifuxelck-server-deploy

ssh -f $1@everythingissauce.com "sudo killall pifuxelck-server; sudo rm /srv/pifuxelck/pifuxelck-server;"

scp dist/build/pifuxelck-server-deploy/pifuxelck-server-deploy $1@everythingissauce.com:/srv/pifuxelck/pifuxelck-server

ssh -f $1@everythingissauce.com "sudo chown pifuxelck:pifuxelck /srv/pifuxelck/pifuxelck-server; sudo nohup /srv/pifuxelck/pifuxelck-server 3000 db.everythingissauce.com 3306 pifuxelck $2 pifuxelck"

exit 0
