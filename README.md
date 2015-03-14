pifuxelck-server
================

The following is an incomplete guide to building, running and deploying the
pifuxelck server.

Compiling
---------

This project requires cabal and the GHC Haskell compiler. Once you have that
building the server can be done by running the following:

    cabal configure && cabal install --dependencies-only && cabal build

Local Setup
-----------

To run an instance locally you must setup a MySQL server. Once you have a server
running, you can run the following to create the required tables:

    mysql \
        -h localhost \
        -P 3306 \
        -u <YOUR MYSQL USER> \
        --password \
        pifuxelck \
        < sql/schema-002.sql

Running
-------

The following will run the server locally against a local MySQL server.

    ./dist/build/pifuxelck-server/pifuxelck-server \
        --port 4242 \
        --mysql-host localhost \
        --mysql-port 3306 \
        --mysql-user <YOUR MYSQL USER> \
        --mysql-password <YOUR MYSQL PASSWORD> \
        --mysql-db pifuxelck

Deploying
---------

Before deploying a binary to prod, the MySQL database must be migrated if there
were any changes to the schema. This is done by manually logging into the VM and
running the migration scripts in the `sql` directory.

Deploying a server binary can be accomplished with the deploy script.

    ./deploy.sh $USER <PROD MYSQL PASSWORD>
