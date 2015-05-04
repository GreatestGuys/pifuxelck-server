#!/bin/bash

Color_Off='\e[0m'
BGreen='\e[1;32m'
BYellow='\e[1;33m'

if [[ $# -ne 2 ]] ; then
  echo 'usage: ./deploy sauce-user-name my-sql-password'
  exit 0
fi

TMP_FILE=`mktemp /tmp/pifuxelck.XXXXXXX.sh`
cat > $TMP_FILE << EOF
echo ""
echo -e "${BYellow}Launching new instance...${Color_Off}"
cd /srv/pifuxelck/
/usr/bin/nohup ./pifuxelck-server \
  +RTS -T -RTS \
  --port 3000 \
  --mysql-host `dig db.everythingissauce.com +short | head -n 1` \
  --mysql-port 3306 \
  --mysql-user pifuxelck \
  --mysql-password $2 \
  --mysql-db pifuxelck 2>stderr > stdout < /dev/null &
exit
EOF

echo ""
echo -e "${BYellow}Building the executable...${Color_Off}"
cabal sandbox init
cabal install --only-dependencies
cabal build pifuxelck-server

echo ""
echo -e "${BYellow}Killing existing server instances...${Color_Off}"
ssh \
  $1@everythingissauce.com \
  "sudo su -c 'killall pifuxelck-server' pifuxelck"

echo ""
echo -e "${BYellow}Deploying the executable...${Color_Off}"
scp \
  dist/build/pifuxelck-server/pifuxelck-server \
  $1@everythingissauce.com:/srv/pifuxelck/pifuxelck-server

echo ""
echo -e "${BYellow}Deploying start up script...${Color_Off}"
scp $TMP_FILE $1@everythingissauce.com:${TMP_FILE}

echo ""
echo -e "${BYellow}Running start up script...${Color_Off}"
ssh \
  $1@everythingissauce.com \
  "chmod a+rxw $TMP_FILE && sudo su -c $TMP_FILE pifuxelck && rm $TMP_FILE"

echo ""
echo -e "${BGreen}All done!${Color_Off}"

rm $TMP_FILE
exit 0
