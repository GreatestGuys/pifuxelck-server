pifuxelck-server
================

Compiling
---------

This projcet requires cabal and the GHC Haskell compiler. 

    cabal configure && cabal install --dependencies-only && cabal build
    
Running
-------

The following will run the server locally against a local MySQL server.

    ./dist/build/pifuxelck-server/pifuxelck-server --port 4242 --mysql-host localhost --mysql-port 3306 --mysql-user root --mysql-password <YOUR MYSQL PASSWORD> --mysql-db pifuxelck
