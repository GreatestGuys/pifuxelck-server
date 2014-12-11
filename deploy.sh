#!/bin/bash

Color_Off='\e[0m'
BGreen='\e[1;32m'
BYellow='\e[1;33m'

if [[ $# -ne 2 ]] ; then
  echo 'usage: ./deploy sauce-user-name my-sql-password'
  exit 0
fi

echo ""
echo -e "${BYellow}Building the executable...${Color_Off}"
cabal sandbox init
cabal install --only-dependencies
cabal build pifuxelck-server

echo ""
echo -e "${BYellow}Killing the remote server...${Color_Off}"
#ssh $1@everythingissauce.com "sudo killall pifuxelck-server; sudo rm /srv/pifuxelck/pifuxelck-server;"

echo ""
echo -e "${BYellow}Deploying the executable...${Color_Off}"
#scp dist/build/pifuxelck-server/pifuxelck-server $1@everythingissauce.com:/srv/pifuxelck/pifuxelck-server

echo ""
echo -e "${BYellow}Setting permissions and running the remote server...${Color_Off}"
ssh $1@everythingissauce.com "cd /srv/pifuxelck/; sudo chown pifuxelck:pifuxelck ./pifuxelck-server; sudo su -c 'nohup ./pifuxelck-server --port 3000 --mysql-host db.everythingissauce.com --mysql-port 3306 --mysql-user pifuxelck --mysql-password $2 --mysql-db pifuxelck &' pifuxelck;"

echo ""
echo -e "${BGreen}All done!${Color_Off}"

exit 0
