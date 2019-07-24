CyBy<sup>2</sup>: Chemical and Biological Data Management
=========================================================

This library provides a set of utility functions and
data types for writing purely functional client/server
applications for handling and querying chemical and
biological data. It is split into several modules
and comes with an example implementation of a HTTP-server
and web client for a fully functional chemical data management
system.

Most modules can be cross compiled to Java bytecode and JavaScript
to maximize code sharing between client and server.


Modules
-------

### util

Utility data types and functions both with an application in
cheminformatics and functional programming in general.
This is includes basic data types for defining combined queries
and calculating basic statistics for results from bioassays.


### server

Helper traits and functions used for exporting and querying data,
managing users, and validating and editing of data from user input.


### msf

This is an adaption of monadic streaming functions found
in the Haskell package [dunai](https://hackage.haskell.org/package/dunai).
Monadic streaming functions can be used as a generalization
of different styles of functional reactive programming.
They are used to implement the highly interactive parts
of the web frontend.


### msf\_js

Bindings for monadic streaming functions for interacting
with the DOM plus some utility functions for assembling
HTML documents in a typesafe manner.


### ui

Helper traits and functions for writing highly interactive
user interfaces in the browser and to reuse the same code
to also write documentation for the software. This
makes use of modules msf and msf\_js


### example/dat

Implements the data model shared between client and server
of the example application. The techniques used to abstract
over some reoccuring patterns are explained in some detail
in the code.


### example/server

Implementation of an example HTTP-server. The server loads most
data into memory at startup and therefore reacts quickly
on user requests. Data is stored incrementally on disk in
a custom JSON format (one editing instruction per line).
Linked files and user settings are stored in a SQLite database.


### example/ui

Implementation of the web front-end (together with a 
documentation page).


Setting up the Web Server
-------------------------

The example application is designed to be used together
with an HTTP server like the [Apache HTTP Server](https://httpd.apache.org/).
This web server should then be set up as a 
as a reverse proxy forwarding certain
calls to a locally running instance of the CyBy<sup>2</sup> server.
The principle together with the required modules is explained
on the [Apache webpage](https://httpd.apache.org/docs/2.4/howto/reverse_proxy.html).

After installing the web server of your choice on your system, create a
subfolder for the CyBy<sup>2</sup> page and make sure the current
user can write to it. For instance, when using Apache HTTP
server under Ubuntu as user "paul":

```
  $ sudo mkdir /var/www/html/cyby
  $ sudo chown paul /var/www/html/cyby
```

Finish setting up the configuration of your web page as
described in your web server's documentation. Make sure, that
the web server is setup as a reverse proxy and forwards
calls to "/cyby-serv/" to the locally running CyBy<sup>2</sup>-server:
For instance, with Apache HTTP Server and the CyBy<sup>2</sup>-server
listening on local port 2555, add the following
to the configuration of your virtual host:

```
  ProxyPass        /cyby/cyby-serv/ http://localhost:2555/
  ProxyPassReverse /cyby/cyby-serv/ http://localhost:2555/
```

Make sure, that the necessary modules are enabled. For instance,
under Ubuntu:

```
  $ sudo a2enmod proxy
  $ sudo a2enmod proxy_http
```

Restart your web server afterwards, if necessary.


Building and Installing the Application
---------------------------------------

In order to build the application, Java 1.8 or later is required. Install
the [simple build tool](https://www.scala-sbt.org) and
after cloning this repository from github to a local drive, run
the build script (build.sh) found in the project's root folder:

```
  $ ./build.sh /path/to/web/folder
```

"/path/to/web/folder" is the place where the content of your
web page should be put, for instance "/var/www/html/cyby".

If you do this for the first time, SBT will have to download all
needed dependencies and the Scala compilers, so this will take some time.

After the build, copy the rest of the web page using
the provided script.

```
  $ ./publish_local.sh /path/to/web/folder
```

Finally, create a directory where CyBy<sup>2</sup>'s database should
reside, and put the example data there:

```
  $ sudo mkdir /path/to/cyby/data
  $ sudo chown paul /path/to/cyby/data
  $ cp -v example/db/* /path/to/cyby/data
```


Running CyBy<sup>2</sup>
------------------------

After successfully building the application, the packaged
.jar file of the server can be found at

```
  example/server/target/scala-2.12/server.jar
```

When running it, you have to pass it the port it should
listen on and the directory where its data is stored as
command line arguments:

```
  $ java -jar server.jar 2555 /path/to/cyby/data
```

You can now give CyBy<sup>2</sup> a try by accessing its web page
in your browser. Two users have been setup for you:
An admin account with username "admin" and password "admin"
and a user account with username "user" and password "password".


About the Authors
-----------------

CyBy<sup>2</sup> was developped by the Stefan Höck
and Rainer Riedl at the Center for Organic and
Medicinal Chemistry at the Zurich University of Applied Sciences.


License
-------

CyBy<sup>2</sup> is published under version 3 of the
[GNU GENERAL PUBLIC LICENSE](http://www.gnu.org/licenses/gpl-3.0.html),
a copy of which is bundled together with the source code.

For drawing molecules in the browser,
CyBy<sup>2</sup> comes bundled with [ChemDoodle Web Components](https://web.chemdoodle.com/).
ChemDoodle Web is also licensed under the GPL v3.
