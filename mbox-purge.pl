#!/usr/bin/perl -w
use strict;

# $Id: mbox-purge,v 1.23 2007-04-11 12:32:48-04 roderick Exp $
#
# Copyright (c) 1997 Roderick Schertler.  All rights reserved.  This
# program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

use sigtrap qw(die untrapped normal-signals);

use POSIX		qw(:errno_h);
use Proc::WaitStat	qw(waitstat_die);
use RS::Handy		qw(:stat $Me
			    badinvo chompexpr_fileline dstr data_dump
			    exclusive_create mbox_read_head mbox_read_body
			    mbox_escape mbox_escape_body_part_in_place xdie);

# Compile code from the user.  This comes before anything else so it
# can't access my lexicals.

sub user_eval {
    @_ == 1 || badinvo;

    local $SIG{__DIE__};
    no strict 'vars';
    return eval shift;
}

my $Usage = <<EOF;
usage: $Me [switch]... mbox_file...
switches:
 -c, --copy-to file	copy messages which are being deleted to file
     --debug		turn debugging on
     --help		show the help and die
     --newline s	use s as the line terminator rather than guessing
     --newline-native	use the native line terminator rather than guessing
 -n, --no		don't actually modify any files
     --quiet		suppress informational messages
     --verbose		output additional informational messages
     --version		show the version and exit
criteria:
     --before       date-time	delivered before date-time
     --before-or-at date-time	delivered before or exactly at date-time
     --after        date-time	delivered after  date-time
     --after-or-at  date-time	delivered after  or exactly at date-time
     --pattern pat	head+body match   Perl regex /pat/m
     --head-pattern pat	head      matches Perl regex /pat/m
     --body-pattern pat	body      matches Perl regex /pat/m
     --eval code	\$code->(\\\$head, \\\$body, \\\$msg) returns true
     --head-eval code	\$code->(\\\$head) returns true

Any messages in the given files which match all the criteria are deleted.

Use \`perldoc $Me\' to see the full documentation.
EOF

my $Copy_mbox	= undef;
my $Debug	= 0;
my $Exit	= 0;
my $Guess_line_endings = 1;
my $Lock_max_age = 60;
my $No		= 0;
my $Quiet	= 0;
my @Tmp		= ();
my $Verbose	= 0;
my $Version	 = q$Revision: 1.23 $ =~ /(\d\S+)/ ? $1 : '?';

# globals with info about current message
my $File_name	= undef;
my $Msg_num	= undef;
my %Message_info = ();

# first found is used, sub can return either undef or -1 on failure
my @Parse_date	= qw(Date::Parse::str2time Date::GetDate::getdate);

sub xwarn {
    RS::Handy::xwarn @_;
    $Exit ||= 1;
}

sub usage {
    xwarn @_ if @_;
    die $Usage;
}

sub info {
    print "$Me: ", @_, "\n" unless $Quiet;
}

sub verbose {
    print "$Me: ", @_, "\n" if $Verbose;
}

sub debug {
    print "debug: ", @_, "\n" if $Debug;
}

sub create_tmp {
    my ($orig) = @_;
    my ($base, $ext, $new, $fh);

    $base = "$orig.tmp.$$";
    $ext = 1;
    $new = $base;
    until ($fh = exclusive_create $new) {
	$! == EEXIST or xdie "can't create $new:";
	xdie "can't create a file named like $base.* in $ext tries\n"
	    if $ext == 100;
	$ext++;
	$new = "$base.$ext";
    }
    return $new, $fh;
}


{ my %locked;
sub lock_file {
    @_ == 1 || badinvo;
    my ($file) = @_;

    debug "lock $file";
    return if $locked{$file};
    my $lock_file = "$file.lock";
    system qw(lockfile -1 -r 10), $lock_file;
    waitstat_die $?, "lockfile for $lock_file";
    push @Tmp, $lock_file;
    $locked{$file} = time;
}

# Other processes can think a lock is stale if it's old (procmail does
# this by default, after about 17 minutes with my version), so touch
# the lock files periodically.

sub touch_locks {
    my $t = time;
    while (my ($file, $t0) = each %locked) {
    	my $lock = "$file.lock";
	my $age = $t - $t0;
	my $do_touch = $age > $Lock_max_age;
	next unless $do_touch;
	debug "lock $lock age $age do_touch $do_touch";
	utime $t, $t, $lock
	    or xdie "can't touch $lock:";
	$locked{$file} = $t;
    }
}

sub unlock_file {
    @_ == 1 || badinvo;
    my ($file) = @_;

    debug "unlock $file";
    $locked{$file}
	or xdie "attempty to unlock file which isn't locked: $file\n";
    my $lock_file = "$file.lock";
    unlink $lock_file
	or xdie "error unlinking $lock_file:";
    @Tmp = grep { $_ ne $lock_file } @Tmp;
    delete $locked{$file};
} }

{ my $sub;
sub parse_date {
    my ($in) = @_;

    if (!$sub) {
    	for my $full (@Parse_date) {
	    (my $mod = $full) =~ s/::[^:]+$// or die;
	    next unless eval "require $mod";
	    if (!defined &$full) {
		xwarn "$mod doesn't define $full\n";
		next;
	    }
	    debug "parse_date using $full";
	    $sub = do { no strict 'refs'; \&$full };
	    last;
	}
	$sub or xdie "no date parsing function available, I tried to load:",
	    	" @Parse_date\n";
    }

    my $t = $sub->($in);
    if (defined $t && $t == -1) {
	$t = undef;
    }

    if ($Debug) {
	my $out = defined $t ? localtime $t : undef;
	debug sprintf "%s -> %s (%s)", map { defined $_ ? $_ : 'undef' }
		$in, $t, $out;
    }

    return $t;
} }

sub parse_from_line {
    @_ == 0 || badinvo;

    return if exists $Message_info{from_line};

    if (${ $Message_info{rhead} } !~ /^(From\s+.*)/) {
	info "no From_ line in message $Msg_num of $File_name";
	$Message_info{from_line} = undef;
	return;
    }
    $Message_info{from_line} = $1;

    if ($Message_info{from_line}
	  !~ /^From \s+ (.*?) \s+ (\w\w\w \s \w\w\w \s+ \d+ \s+ \d+:\d+.*)/x) {
	info "can't parse From_ line in message $Msg_num of $File_name";
	return;
    }

    $Message_info{from_sender}	= $1;
    $Message_info{from_date}	= $2;

    $Message_info{delivery_time} = parse_date $Message_info{from_date}
	or info "invalid delivery date ($Message_info{from_date})",
		" in message $Msg_num of $File_name";
}

sub delivery_time {
    @_ == 0 || badinvo;

    parse_from_line unless exists $Message_info{from_line};
    return $Message_info{delivery_time};
}

sub envelope_sender {
    @_ == 0 || badinvo;

    parse_from_line unless exists $Message_info{from_line};
    return $Message_info{from_sender};
}

sub header_all {
    @_ == 1 || badinvo;
    my ($pat) = @_;

    return ${ $Message_info{rhead} } =~ /^$pat\s*:\s*(.*)/gim;
}

sub header_first {
    return (header_all @_)[0];
}

sub header_last {
    return (header_all @_)[-1];
}

sub mozilla_expunged {
    @_ == 0 || badinvo;

    my $ms = header_first 'X-Mozilla-Status';
    return defined $ms
    	    	&& $ms =~ /^[\da-f]+\z/
		&& hex($ms) & 8;
}

{ my %fh;
sub mbox_append {
    my ($file, $rmsg) = @_;

    lock_file $file;
    my $fh = $fh{$file} ||= do {
    	debug "open $file for appending";
    	require Symbol;
	my $fh = Symbol::gensym();
	open $fh, ">>$file"
	    or xdie "can't append to $file:";
	$fh
    };

    print $fh mbox_escape ${ $rmsg }
	or xdie "error appending to $file:";
}
END {
    while (my ($file, $fh) = each %fh) {
	debug "closing $file";
	if (!close $fh) {
	    xwarn "error closing $file:";
	    $? ||= 1;
	}
    }
} }

# user's accessors for globals

sub file_name {
    return $File_name;
}

sub msg_num {
    return $Msg_num;
}

sub main {
    my (@rule, $any_date);

    @ARGV || usage;

    while (@ARGV && $ARGV[0] =~ /^-/) {
	$_ = shift @ARGV;
	if ($_ eq '--') {
	    last;
	}
	elsif ($_ eq '--copy-to' || $_ eq '-c') {
	    @ARGV or xdie "no arg for $_\n";
	    $Copy_mbox = shift @ARGV;
	}
	elsif (/^--?debug\z/) {
	    $Debug = 1;
	}
	elsif ($_ eq '--help') {
	    usage;
	}
	elsif ($_ eq '--newline') {
	    @ARGV or xdie "no arg for $_\n";
	    $/ = shift @ARGV;
    	    $Guess_line_endings = 0;
    	}
	elsif ($_ eq '--newline-native') {
    	    $/ = "\n";
    	    $Guess_line_endings = 0;
    	}
	elsif ($_ eq '--no' || $_ eq '-n') {
	    $No = 1;
	}
	elsif (/^--?quiet\z/) {
	    $Quiet = 1;
	}
	elsif (/^--?verbose\z/) {
	    $Verbose = 1;
	}
	elsif ($_ eq '--version') {
	    print "$Me version $Version\n";
	    exit 0;
	}
	elsif (/^--?((before|after)(-or-at)?)\z/) {
	    my $rule = $1;
	    @ARGV or xdie "no arg for $rule\n";
	    my $spec = shift @ARGV;
	    my $time = parse_date $spec;
	    defined $time && $time > 0 or xdie "invalid time `$spec'\n";
	    # getdate() has ambituities (eg, 040101 = 2004-01-01), so
	    # help by choking on dates in the future.
	    if ($time > time) {
		xdie "$rule value is in the future (",
		    scalar localtime $time, ")\n";
	    }
	    push @rule, [$rule, $time];
	    $any_date = 1;
	}
	elsif (/^--?((head-|body-)?pattern)\z/) {
	    my $rule = $1;
	    @ARGV or xdie "no arg for $rule\n";
	    my $pat = shift @ARGV;
	    my $sub = eval 'sub { ${ $_[0] } =~ /$pat/om }';
	    # Validate and compile the pattern by calling the closure
	    # for the first time.
	    eval { $sub->(\ "") };		# space after \ helps emacs
	    if ($@) {
		$@ = chompexpr_fileline $@;
		xdie "invalid $_ $@\n";
	    }
	    push @rule, [$rule, $sub];
	}
	elsif (/^--?((head-)?eval)\z/) {
	    my $rule = $1;
	    @ARGV or xdie "no arg for $rule\n";
	    my $code = shift @ARGV;
	    my $sub = user_eval "sub { $code }";
	    if ($@) {
		$@ =~ s/ at .eval \d+. line \d+.\n//;
		xdie "invalid $_ code `$code': $@\n";
	    }
	    push @rule, [$rule, $sub];
	}
	else {
	    usage "invalid switch $_\n";
	}
    }
    @rule or usage "no rules specified\n";
    @ARGV or usage "no files specified\n";

    # XXX wrap the whole file loop in an eval, go to next file on a
    # failure

    for my $file_name (@ARGV) {
	my (@stat, $new_file, $new_fh, $n_kept, @stat2);

    	$File_name = $file_name;
	verbose "processing $File_name";

	lock_file $File_name;

	open FILE, $File_name or xdie "can't read $File_name:";
	@stat = stat FILE or xdie "error statting open $File_name:";

    	# Try to guess the line endings used in the file.

	if ($Guess_line_endings) {
	    my $buf = '';
	    my $nread = read FILE, $buf, 512;
	    if (!defined $nread) {
		xdie "error reading from $File_name:";
	    }
	    elsif (!$nread) {
		$/ = "\n"; # won't matter
	    }
	    elsif ($buf =~ /^([^\x0d\x0a]*(\x0d\x0a|\x0d|\x0a))/) {
		$/ = $2;
	    }
	    else {
	    	xwarn "can't intuit line endings for $File_name";
		$/ = "\n";
	    }
	    seek FILE, 0, 0
		or xdie "can't rewind $File_name:";
	    debug "line endings are ", dstr $/;
	}

	($new_file, $new_fh) = create_tmp $File_name;
	push @Tmp, $new_file;

	# XXX These are a security hole when this is run as root on user's
	# files.  I need fchmod() and fchown().
	chmod $stat[ST_MODE], $new_file
	    or xdie "can't chmod $new_file:";
	chown @stat[ST_UID, ST_GID], $new_file
	    or xdie "can't chmod $new_file:";

	$Msg_num = $n_kept = 0;
	while (my ($orig_head, $clen) = mbox_read_head *FILE) {
	    my ($head, $body, $msg, $keep, $delivery_time);
	    %Message_info = ();

	    touch_locks;

	    my $read_body = sub {
		$body = mbox_read_body *FILE, 0, $clen;
		$msg = "$orig_head$/$body";
	    };

	    ($head = $orig_head) =~ s|$/[ \t]+| |g;
	    $Msg_num++;
	    $Message_info{rhead} = \$head;

	    # I'm not using Mail::Header because it doesn't handle
	    # From_.*\n>From headers.
	    if ($any_date) {
	    	parse_from_line;
		$delivery_time = $Message_info{delivery_time};
	    }

	    $keep = 0;
	    for my $rrule (@rule) {
		my ($rule, @arg) = @$rrule;

		# This is done a little backwards.  The default is to
		# purge messages.  If a rule matches (meaning to purge
		# this message) a simple next is done.  Any rule which
		# doesn't match (meaning to keep this message) falls to
		# the bottom from whence the loop is exited (since I
		# only purge if all rules match).
		if ($rule eq 'before') {
    	    	    next if defined $delivery_time && $delivery_time < $arg[0];
		}
		elsif ($rule eq 'before-or-at') {
    	    	    next if defined $delivery_time && $delivery_time <= $arg[0];
		}
		elsif ($rule eq 'after') {
    	    	    next if defined $delivery_time && $delivery_time > $arg[0];
		}
		elsif ($rule eq 'after-or-at') {
    	    	    next if defined $delivery_time && $delivery_time >= $arg[0];
		}
		elsif ($rule eq 'pattern') {
		    $read_body->() if !defined $body;
    	    	    next if $arg[0]->(\$msg);
		}
		elsif ($rule eq 'head-pattern') {
    	    	    next if $arg[0]->(\$head);
		}
		elsif ($rule eq 'body-pattern') {
		    $read_body->() if !defined $body;
    	    	    next if $arg[0]->(\$body);
		}
		elsif ($rule eq 'eval') {
		    $read_body->() if !defined $body;
		    #print data_dump ['real args', \$head, \$body, \$msg];
		    next if $arg[0]->(\$head, \$body, \$msg);
		}
		elsif ($rule eq 'head-eval') {
		    #print data_dump ['real args', \$head];
		    next if $arg[0]->(\$head);
		}
		else {
		    xdie "bug: bad rule `$rule'\n";
		}
		# This rule didn't match, therefore keep this message.
		$keep = 1;
		last;
	    }

	    if (!$keep) {
		debug "$Msg_num purge";
	    	if (defined $Copy_mbox && !$No) {
		    # XXX it'd be better to have mbox_append be able
		    # to work with the callback-using version of
		    # mbox_read_body so you didn't have to keep the
		    # message in memory
		    $read_body->() if !defined $body;
		    mbox_append $Copy_mbox, \$msg;
		}
		else {
		    mbox_read_body *FILE, 1, $clen if !defined $body;
		}
		next;
	    }

    	    debug "$Msg_num keep";
	    if (defined $body) {
	    	debug "$Msg_num already read";
		print $new_fh mbox_escape $msg
		    or xdie "error writing to $new_file:";
	    }
	    else {
	    	debug "$Msg_num using callback";
		print $new_fh $orig_head, $/
		    or xdie "error writing to $new_file:";
		my $len = mbox_read_body *FILE, sub {
			mbox_escape_body_part_in_place $_[0];
			print $new_fh $_[0]
			    or xdie "error writing to $new_file:";
		    }, $clen;
		if ($len) {
		    print $new_fh $/
			or xdie "error writing to $new_file:";
		}
	    }
	    $n_kept++;
	}
	my $n_dropped = $Msg_num - $n_kept;

	close $new_fh
	    or xdie "error closing $new_file:";

	@stat2 = stat FILE
	    or xdie "error doing stat 2 on open $File_name:";

	$stat[ST_MTIME] == $stat2[ST_MTIME]
	    	&& $stat[ST_SIZE] == $stat2[ST_SIZE]
	    # XXX probably shouldn't remove the lock file in this case,
	    # or use the open handle on it I want to use and funlink
	    # or at least try (race!) to see if it's mine first
	    or xdie "$File_name was modified while I had it locked\n";

	close FILE
	    or xdie "error closing $File_name:";

	@Tmp = grep { $_ ne $new_file } @Tmp;
	if ($No || $n_dropped == 0) {
	    unlink $new_file
		or xdie "error unlinking $new_file:"
	}
	else {
	    rename $new_file, $File_name
		or xdie "error renaming $new_file to $File_name:";

	    # $file.msf is a Mozilla index file, it isn't valid since
	    # $file has changed.  Remove it so Mozilla will regenerate
	    # it.

	    my $msf = "$File_name.msf";
	    unlink $msf
		or $! == ENOENT
    	    	or xwarn "error unlinking $msf:";

	    unlink $File_name
		    or xdie "error unlinking $File_name:"
		if $n_kept == 0;
	}

	unlock_file $File_name;

	info sprintf "%5d kept  %5d discarded  %s",
		$n_kept, $n_dropped, $File_name;
    }

    return 0;
}

END {
    for (@Tmp) {
	unless (unlink) {
	    xwarn "error unlinking $_:";
	    $? = 1 unless $?;
	}
    }
}

$Exit = main || $Exit;
$Exit = 1 if $Exit and not $Exit % 256;
exit $Exit;

__END__

=head1 NAME

mbox-purge - perform batch deletion of mail messages from mbox files

=head1 SYNOPSIS

B<mbox-purge>
[B<--copy-to> I<file>]
[B<--debug>]
[B<--help>]
[B<--newline> I<s>]
[B<--newline-native>]
[B<--no>] [B<-n>]
[B<--quiet>]
[B<--verbose>]
[B<--version>]
[B<--before> I<date-time>]
[B<--before-or-at> I<date-time>]
[B<--after> I<date-time>]
[B<--after-or-at> I<date-time>]
[B<--pattern> I<pat>]
[B<--head-pattern> I<pat>]
[B<--body-pattern> I<pat>]
[B<--eval> I<code>]
[B<--head-eval> I<code>]
I<file>...

=head1 DESCRIPTION

B<mbox-purge> performs batch deletion of email messages from mbox format
files based on rules you specify.  It uses F<file.lock>-style locking
(using B<procmail>'s B<lockfile> under the hood).  Because of this you
have to have write permission in the directory in which the I<file>
being processed is stored.

The file to be processed can be in mbox, mboxrd, mboxcl, mboxcl2 or
buggy Elm mboxcl2 format.  The data written will always be in mboxrd
format.  See http://www.qmail.org/qmail-manual-html/man5/mbox.html for
an explanation of these terms.

=head1 OPTIONS - GENERAL

=over

=item B<--copy-to> I<file>

=item B<-c> I<file>

Append a copy of each deleted message to I<file>.

=item B<--debug>

Turn debugging on.

=item B<--help>

Show the help and die.

=item B<--newline> I<s>

Use s as the line terminator rather than guessing based on the first
line of each file.

=item B<--newline-native>

Use this platform's native line terminator rather than guessing based on
the first line of each file.

=item B<--no>

=item B<-n>

Don't actually modify any files, just go through the motions.

=item B<--quiet>

Suppress informational messages.

=item B<--verbose>

Output additional informational messages.

=item B<--version>

Show the version and exit.

=back

=head1 OPTIONS - MESSAGE SELECTION

If multiple rules are given they all have to match for a message to be
purged.

=over

=item B<--before> I<date-time>

=item B<--before-or-at> I<date-time>

=item B<--after> I<date-time>

=item B<--after-or-at> I<date-time>

Purge messages which were delivered in the specified period.  The
date/time parsing is flexible, as provided by Date::Parse.  I usually
use I<YYYY>-I<MM>-I<DD> I<HH>:I<MM>:I<SS>.  If you leave off the time
it defaults to 00:00:00.

=item B<--pattern> I<pat>

Purge messages which match I<pat>.  The pattern is run against the message
after its mbox-style encoding has been unescaped.  The match uses
Perl's //m flag.

=item B<--head-pattern> I<pat>

Purge messages which match I<pat>.  The pattern is run against the message
after its mbox-style encoding has been unescaped.  The match uses
Perl's //m flag.

Additionally, the headers have line continuations undone (newline
followed by whitespace is replaced with a single space) before the
match.

=item B<--body-pattern> I<pat>

Purge messages whose bodies match I<pat>.  The pattern is run against
the message after its mbox-style encoding has been unescaped.  The match
uses Perl's //m flag.

=item B<--eval> I<code>

Evaluate I<code> and purge the message if it returns true.  I<code> is
compiled as the body of a subroutine.  The subroutine receives references
to the head, body and full text of the message as its arguments.  The head
argument has had continuation lines undone, and the body in both of the
second arguments has had its mbox encoding unescaped.  See
also L</CONVENIENCE SUBS>.

=item B<--head-eval> I<code>

Like B<--eval> but the only arg passed to the I<code> is a reference to
the message header.  Use this if you don't need the message body, to
avoid having to put it in memory.

=back

=head1 CONVENIENCE SUBS

Here are some subs you can use from code passed in via B<-eval>:

=over

=item B<delivery_time>

Return the epoch time() when the message was delivered, as read
from the From_ line.

=item B<envelope_sender>

Return the envelope sender, as read from the From_ line.

=item B<file_name>

Return the name of the file being processed.

=item B<header_all> I<header_pattern>

Return the data part of all the header lines whose field names match
I<header_pattern>.  Eg,

    my @recv = header_all 'Received';

=item B<header_first> I<header_pattern>

=item B<header_last> I<header_pattern>

These are like B<header_all>, but they only return the first or last
matching header.

=item B<mbox_append> I<file>, I<ref-to-string>

Append the I<ref-to-string> to the mbox-format I<file>, doing appropriate
escaping and locking.  Typically I<ref-to-string> will be C<$_[2]>.

=item B<mozilla_expunged>

True if the message has been marked as expunged by Mozilla.  Such
messages are effectively deleted, but haven't been removed from the
mailbox file yet.

=item B<msg_num>

Return the index of this message in the current file (starts at 1).

=item B<parse_date> I<str>

Return the epoch time() which corresponds to I<str>, or B<undef>.

=back

=head1 EXAMPLES

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

=head1 BUGS

You can't delete from your mail spool on a system which doesn't have a
world-writable spool directory if you're a regular user, both because
B<mbox-purge> doesn't know to special-case B<lockfile>'s invocation for
that and because it creates the temporary file in the same directory as
the file it is processing.

=head1 CHANGES

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


=head1 AUTHOR

Roderick Schertler <F<roderick@argon.org>>

=cut
