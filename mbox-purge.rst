NAME
====

mbox-purge - perform batch deletion of mail messages from mbox files

SYNOPSIS
========

**mbox-purge** [**--copy-to** *file*] [**--debug**] [**--help**] [**--newline** *s*] [**--newline-native**] [**--no**] [**-n**] [**--quiet**] [**--verbose**] [**--version**] [**--before** *date-time*] [**--before-or-at** *date-time*] [**--after** *date-time*] [**--after-or-at** *date-time*] [**--pattern** *pat*] [**--head-pattern** *pat*] [**--body-pattern** *pat*] [**--eval** *code*] [**--head-eval** *code*] *file*...

DESCRIPTION
===========

**mbox-purge** performs batch deletion of email messages from mbox format files based on rules you specify. It uses *file.lock*-style locking (using **procmail**'s **lockfile** under the hood). Because of this you have to have write permission in the directory in which the *file* being processed is stored.

The file to be processed can be in mbox, mboxrd, mboxcl, mboxcl2 or buggy Elm mboxcl2 format. The data written will always be in mboxrd format. See `http://www.qmail.org/qmail-manual-html/man5/mbox.html`_ for an explanation of these terms.

OPTIONS - GENERAL
=================

**--copy-to file**

**-c file**

    Append a copy of each deleted message to *file*.

**--debug**

    Turn debugging on.

**--help**

    Show the help and die.

**--newline s**

    Use s as the line terminator rather than guessing based on the first
    line of each file.

**--newline-native**

    Use this platform's native line terminator rather than guessing
    based on the first line of each file.

**--no**

**-n**

    Don't actually modify any files, just go through the motions.

**--quiet**

    Suppress informational messages.

**--verbose**

    Output additional informational messages.

**--version**

    Show the version and exit.

OPTIONS - MESSAGE SELECTION
===========================

If multiple rules are given they all have to match for a message to
be purged.

**--before date-time**

**--before-or-at date-time**

**--after date-time**

**--after-or-at date-time**

    Purge messages which were delivered in the specified period. The
    date/time parsing is flexible, as provided by Date::Parse. I usually
    use *YYYY*-*MM*-*DD* *HH*:*MM*:*SS*. If you leave off the time it
    defaults to 00:00:00.

**--pattern pat**

    Purge messages which match *pat*. The pattern is run against the
    message after its mbox-style encoding has been unescaped. The match
    uses Perl's //m flag.

**--head-pattern pat**

    Purge messages which match *pat*. The pattern is run against the
    message after its mbox-style encoding has been unescaped. The match
    uses Perl's //m flag.

    Additionally, the headers have line continuations undone (newline
    followed by whitespace is replaced with a single space) before the
    match.

**--body-pattern pat**

    Purge messages whose bodies match *pat*. The pattern is run against
    the message after its mbox-style encoding has been unescaped. The
    match uses Perl's //m flag.

**--eval code**

    Evaluate *code* and purge the message if it returns true. *code* is
    compiled as the body of a subroutine. The subroutine receives
    references to the head, body and full text of the message as its
    arguments. The head argument has had continuation lines undone, and
    the body in both of the second arguments has had its mbox encoding
    unescaped. See also `CONVENIENCE SUBS`_.

**--head-eval code**

    Like **--eval** but the only arg passed to the *code* is a reference
    to the message header. Use this if you don't need the message body,
    to avoid having to put it in memory.

CONVENIENCE SUBS
================

Here are some subs you can use from code passed in via **-eval**:

**delivery\_time**

    Return the epoch ``time()`` when the message was delivered, as read
    from the From\_ line.

**envelope\_sender**

    Return the envelope sender, as read from the From\_ line.

**file\_name**

    Return the name of the file being processed.

**header\_all header\_pattern**

    Return the data part of all the header lines whose field names
    match *header\_pattern*. Eg::

        my @recv = header_all 'Received';

**header\_first header\_pattern**

**header\_last header\_pattern**

    These are like **header\_all**, but they only return the first or
    last matching header.

**mbox\_append file, ref-to-string**

    Append the *ref-to-string* to the mbox-format *file*, doing
    appropriate escaping and locking. Typically *ref-to-string* will be
    ``$_[2]``.

**mozilla\_expunged**

    True if the message has been marked as expunged by Mozilla. Such
    messages are effectively deleted, but haven't been removed from the
    mailbox file yet.

**msg\_num**

    Return the index of this message in the current file (starts at 1).

**parse\_date str**

    Return the epoch ``time()`` which corresponds to *str*, or **undef**.

EXAMPLES
========

::

    # Delete messages older than the given date from all your folders.

    mbox-purge --before 2000-05-01 ~/Mail/*

    # Delete messages from April 2000.

    mbox-purge --after-or-at 2000-04-01 --before 2000-05-01 file

    # Move messages delivered in the year 2000 from the listed files
    # into a single file.

    mbox-purge --after-or-at 2000-01-01 --before 2001-01-01 \
        --copy-to 2000.mbox file1 file2 file3

    # Delete a chain letter from all user's mailboxes.

    mbox-purge \
        --head-pattern '^Subject: (Re: )?GOOD LUCK TOTEM( \(fwd\))?$' \
        /var/spool/mail/*

    # Perform equivalent of Mozilla folder compaction.

    mbox-purge --eval mozilla_expunged file

    # Delete messages larger than 1M.

    mbox-purge --eval 'length ${ $_[2] } > 1_000_000' file

    # Delete messages older than 6 months from all your folders.

    mbox-purge --eval 'time - delivery_time > 60*60*24 * 31 * 6' ~/Mail/*

    # Same, but use the Date: field's date rather than the delivery date.

    mbox-purge --eval 'time - parse_date(header_first "Date")
                        > 60*60*24 * 31 * 6' ~/Mail/*

    # Move messages older than 6 months into by-year archive folders, keeping
    # separate archives for each source folder.  Eg, for ~/Mail/sent you
    # get ~/Mail/sent.2000, ~/Mail/sent.2001, etc.

    find ~/Mail ! -name '*.[0-9][0-9][0-9][0-9]' ! -name '.*' -type f \
        -print0 | xargs -0r mbox-purge \
        --eval '
            return 0 if time - delivery_time() < 60*60*24 * 31 * 6;
            require POSIX;
            my $year = POSIX::strftime("%Y", localtime delivery_time);
            mbox_append file_name . ".$year", $_[2];
            1'

BUGS
====

You can't delete from your mail spool on a system which doesn't have a
world-writable spool directory if you're a regular user, both because
**mbox-purge** doesn't know to special-case **lockfile**'s invocation
for that and because it creates the temporary file in the same directory
as the file it is processing.

CHANGES
=======

::

    $Log: mbox-purge,v $
    Revision 1.23  2007-04-11 12:32:48-04  roderick
    Typo.

    Revision 1.22  2007-04-06 16:19:54-04  roderick
    Add --copy-to, --before-or-at, --after-or-at.

    Add convenience subs:  mbox_append(), file_name(), msg_num().

    Touch lock files periodically so other processes don't think they're
    stale.

    Revision 1.21  2006-11-22 14:13:00-05  roderick
    Support differing line terminators ($/); guess the right value on a
    file-by-file basis by default.  Add --newline, --newline-native to
    override this.

    Avoid holding a whole message body in memory when possible by using the
    (new) callback-enabled version of mbox_read_body().  Add --head-eval to
    make it possible in more cases.

    Add -n as alias for --no.

    Add mozilla_expunged() convenience sub.

    Revision 1.20  2006-10-17 10:07:20-04  roderick
    Use chompexpr_fileline.

    Revision 1.19  2006-09-08 09:31:39-04  roderick
    Also check the file's size when detecting modifications.

    When you modify a mailbox remove a .msf file (Mozilla index) if present.

    Revision 1.18  2005-03-01 11:57:15-05  roderick
    Oops, set $File_name correctly.

    Revision 1.17  2004-09-02 10:49:57-04  roderick
    Important changes:

      Use the delivery date rather than the Date: header for --before and
      --after.

      Add and prefer --switch to -switch, but still allow the latter for
      old switches.

      Add convenience subs:  delivery_time(), envelope_sender(), header_all(),
      header_first(), header_last().

      Add --help, --no, --quiet, --verbose, --version.

    Less important:

      If no messages were purged from a file, leave it as is rather than
      replacing it with the new (identical) copy.

      Treat a parsed date of -1 as undef.

      Add %Message_info, $File_name, $Msg_num.

      Improve the usage message.

      For --eval, turn off strict vars, and don't let the user get at my
      lexicals.

      Don't trap signals which were ignored.

AUTHOR
======

Roderick Schertler <*roderick@argon.org*>

.. _`http://www.qmail.org/qmail-manual-html/man5/mbox.html`: http://www.qmail.org/qmail-manual-html/man5/mbox.html
.. _CONVENIENCE SUBS: #CONVENIENCE_SUBS
