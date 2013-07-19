mbox-purge - batch email deletion based on rules
================================================

**mbox-purge** performs batch deletion of email messages from mbox
format files with appropriate locking, based on rules you specify. You
can select messages received before or after some point in time, or
whose headers or bodies match some Perl regular expression, or which are
selected based on an arbitrary Perl expression.

License
-------

Copyright (c) `Roderick Schertler`_. All rights reserved. This program
is free software; you can redistribute it and/or modify it under the
same terms as Perl itself.

Installation
------------

::

    git clone https://github.com/alonswartz/mbox-purge.git
    
    for d in $(ls deps); do
        cd deps/$d
        perl Makefile.PL
        make
        make test
        sudo make install
        cd -
    done
    
    # if you want date parsing
    cpan -i Date::Parse

Usage
-----

::

    $ ./mbox-purge.pl --help
    usage: mbox-purge.pl [switch]... mbox_file...
    switches:
     -c, --copy-to file     copy messages which are being deleted to file
         --debug            turn debugging on
         --help             show the help and die
         --newline s        use s as the line terminator rather than guessing
         --newline-native   use the native line terminator rather than guessing
     -n, --no               don't actually modify any files
         --quiet            suppress informational messages
         --verbose          output additional informational messages
         --version          show the version and exit
    criteria:
         --before       date-time   delivered before date-time
         --before-or-at date-time   delivered before or exactly at date-time
         --after        date-time   delivered after  date-time
         --after-or-at  date-time   delivered after  or exactly at date-time
         --pattern pat      head+body match   Perl regex /pat/m
         --head-pattern pat head      matches Perl regex /pat/m
         --body-pattern pat body      matches Perl regex /pat/m
         --eval code        $code->(\$head, \$body, \$msg) returns true
         --head-eval code   $code->(\$head) returns true
    
    Any messages in the given files which match all the criteria are deleted.
    
    Use `perldoc mbox-purge.pl' to see the full documentation.

Documentation
-------------

* `official documentation`_
* `mbox-purge.rst`_
* ``perldoc mbox-purge.pl``


.. _Roderick Schertler: http://www.argon.org/~roderick/
.. _official documentation: http://www.argon.org/~roderick/mbox-purge.html
.. _mbox-purge.rst: https://github.com/alonswartz/mbox-purge/blob/master/mbox-purge.rst
