HDBC-performance
================

This is a simple performance measuring application to benchmark HDBC backend
drivers.


Installing HDBC-PostgreSQL
--------------------------

The first step is to get PostgreSQL up and running. Next we'll need to set up a
database for the ODBC test. I'll be using PostgreSQL, which can be installed
using:

    sudo aptitude install postgresql postgresql-contrib

More detailed instructions can be found on [Linode Library][1], and there's
plenty of information about how postgresql works in the [Slicehost Articles][2].

If you want to just perform a quick test, then the following should get you
making a database and creating a new user:

    $ su - postgres
    $ psql

Once in `psql`, we'll execute some queries to set things up.    
For the purposes of this preformance analysis, we'll create a user called
`hdbc`, and a database `hdbc-test`:

    CREATE ROLE hdbc WITH LOGIN ENCRYPTED PASSWORD 'password';
    CREATE DATABASE "hdbc-test" WITH OWNER hdbc ENCODING 'utf8';
    \q

Now we need to install the HDBC backend for postgresql:

    cabal install HDBC-postgresql

To test that HDBC is able to talk to postgresql, we'll just fire up a simple
ghci session:

    ghci> :m + Database.HDBC Database.HDBC.PostgreSQL
    ghci> conn <- connectPostgreSQL "host=localhost dbname=hdbc-test user=hdbc password=password"
    ghci> hdbcDriverName conn
    "postgresql"
    ghci> hdbcClientVer conn
    "8.4.8"

If that hasn't worked then I suggest you look over your configuration again.

Installing HDBC-ODBC
--------------------

Now that PostgreSQL is up and running, we can start adding a connection using
ODBC.
First make sure that you have unixodbc-dev installed on your linux machine:

    sudo aptitude install unixodbc unixodbc-dev unixodbc-bin odbc-postgresql

Getting ODBC to work with postgresql is simple when you know how: you must
make ODBC aware of your database drivers, and you need to supply some
information to allow ODBC to connect to the database.

Here's a diagram from the ODBCConfig tool that describes how the process works:

     Application
         |
    Driver Manager --- odbc.ini --- Config
         |                             |
       Driver                     odbcinst.ini
         |
    Database System

I can recommend looking at the [unixODBC documentation][3] to get things set up.
Use the `ODBCConfig` tool to get your ODBC connection set up, and use
the `DataManager` to verify that the connection works.

To get the postgresql drivers to be configured by ODBC, you'll need to
copy the configuration settings across:

    sudo sh -c "cat /usr/share/psqlodbc/odbcinst.ini.template >> /etc/odbcinst.ini"

Then use ODBCConfig to add a new User DSN using PostgreSQL Unicode and
a name of `HDBC`.

Next install HDBC-odbc from cabal:

    cabal install HDBC-odbc

Finally, we can fire up ghci to test the connection to our database via ODBC:

    ghci> :m + Database.HDBC Database.HDBC.ODBC
    ghci> conn <- connectODBC "DSN=HDBC"
    ghci> hdbcDriverName conn
    "odbc"
    ghci> hdbcClientVer conn
    "03.52"

With the basic onfiguration out of the way, we can bundle this up into an
application that connects to a database via ODBC.

[1]: http://library.linode.com/databases/postgresql
[2]: http://articles.slicehost.com/postgresql
[3]: http://www.unixodbc.org/odbcinst.html
