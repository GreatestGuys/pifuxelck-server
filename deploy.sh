#!/bin/bash

if [[ $# -ne 1 ]] ; then
  echo 'usage: ./deploy sauce-user-name'
  exit 0
fi

cabal build pifuxelck-server-deploy

ssh -f $1@everythingissauce.com 'sudo killall pifuxelck-server; sudo rm /srv/pifuxelck/pifuxelck-server;'

scp dist/build/pifuxelck-server-deploy/pifuxelck-server-deploy $1@everythingissauce.com:/srv/pifuxelck/pifuxelck-server

ssh -f $1@everythingissauce.com 'sudo chown pifuxelck:pifuxelck /srv/pifuxelck/pifuxelck-server; sudo nohup /srv/pifuxelck/pifuxelck-server db.everythingissauce.com 3306 pifuxelck yolo2013 pifuxelck'

exit 0
