#
# Copyright (c) 2000 Roderick Schertler.  All rights reserved.  This
# program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

use strict;

=head1 NAME

RS::Handy - a grab-bag of useful routines

=head1 SYNOPSIS

    use RS::Handy qw(:stat xdie);

    my @st = stat $path
    	or xdie "can't stat $path:";
    print "$path modified ", scalar localtime $st[ST_MTIME], "\n";

    # and many more, sorry for leaving them out of the synopsis

=head1 DESCRIPTION

This module provides some assorted functions I like to use in my programs.
I've tossed many of my generic routines in here, I should really have more
discipline about categorizing all these things and creating separate modules
for them.  I've split some modules out of here in the past (Proc::SyncExec,
IPC::Signal, Proc::WaitStat, String::ShellQuote, plus some which made it
into the core), if you find any of these compellingly useful let me know so
I can prioritize splitting them out, too.

Nothing is automatically exported into your namespace.

Almost all of these functions die() if they encounter any sort of
problem.

=head1 IMPORTABLE SYMBOLS

=over 4

=cut

package RS::Handy;

use Exporter ();
use Carp;

use vars qw(@ISA @EXPORT @EXPORT_OK @EXPORT_FAIL %EXPORT_TAGS $Me $VERSION);

$VERSION = q$Revision: 1.108 $ =~ /(\d\S+)/ ? $1 : '?';

@ISA	= qw(Exporter);
@EXPORT = ();
@EXPORT_OK = qw($Me
    	    	fileline chompexpr_fileline subname subcall_info badinvo
		xwarn xdie xwarn_caller xdie_caller
		xcarp xcluck xcroak xconfess
    	    	process_arg_pairs
		dirents dirents_qualified fopen_mode
		fdopen fhbits f_getfl f_setfl exclusive_create
		binary_scaled plural pluralx uncontrol uncontrol_emacs
		getopt tmpfile safe_tmp
		home home_of
		full_name_uid full_name pwuid grgid
		cat catslurp xsrand shuffle yorn prompt
		data_dump data_dump_unsorted
		untaint tainted
		rename_nounlink rename_unique
		filter_string

		mbox_read_head mbox_read_body mbox_read
		mbox_escape_body_part_in_place mbox_escape

		daemonize url_decode url_encode_c
		html_attr_encode html_attrs html_escape
		create_index_subs_pkg create_index_subs
    	    	create_constant_subs_pkg create_constant_subs
		get_win_size even_elements odd_elements define
		command_with_stdin sendmail ordinal wrap
		max min max_not_undef min_not_undef
		fuser
		make_check_digit valid_check_digit
		replace_ugids cmp_strnum cmp_strnum_transform have_prog
		rfc822_dt iso_date iso_dt
		interval_to_secs secs_to_dhms secs_to_dhms_str
		mtime flush commify
		mime_token_quote unique cache_url set_autoflush
		remove_at_exit struct_linger_pack struct_linger_unpack
		equal dstr expand_field_list_specs non_comments
		discard_zombies list_length decompressing_open_command
		eq_undef inverse_hash
		G_PER_LB KG_PER_LB LB_PER_KG lb_to_kg lb_to_g kg_to_lb g_to_lb
		longest_common_prefix longest_common_parent_directory
		create_lock_file prompt_readline center_string);

# Set up constant subs used as struct indices at compile time so they're
# available to code here.

BEGIN {
    my %struct = (
	'stat'	=> ['st', qw(dev ino mode nlink uid gid rdev size
				atime mtime ctime blksize blocks)],
	'gr'	=> ['gr', qw(name passwd gid mem)],
	'pw'	=> ['pw', qw(name passwd uid gid quota comment gcos
				dir shell expire)],
	'tm'	=> ['tm', qw(sec min hour mday mon year wday yday isdst)],
    );

    %EXPORT_TAGS = ();
    for my $tag (keys %struct) {
	my ($prefix, @field) = @{ $struct{$tag} };
	for my $i (0..$#field) {
	    my $name =  uc "${prefix}_$field[$i]";
	    push @{ $EXPORT_TAGS{$tag} }, $name;
	    no strict 'refs';
	    *$name = sub { $i };
	}
    }
}

Exporter::export_ok_tags;

$EXPORT_TAGS{all} = [@EXPORT_OK];

my %moved_syms = qw(
    gensym			Symbol
    ref_qualify			Symbol=qualify_to_ref

    fork_retry			Proc::SyncExec
    sync_exec			Proc::SyncExec
    sync_fhpopen_noshell	Proc::SyncExec
    sync_popen_noshell		Proc::SyncExec
    sync_open			Proc::SyncExec

    %sig_no			IPC::Signal=%Sig_num
    @sig_name			IPC::Signal=@Sig_name
    sig_setup			IPC::Signal=sig_translate_setup
    sig_no			IPC::Signal=sig_num
    sig_name			IPC::Signal

    exitstat			Proc::WaitStat=waitstat
    exitstat_die		Proc::WaitStat=waitstat_die
    exitstat_reuse		Proc::WaitStat=waitstat_reuse
    close_die			Proc::WaitStat

    shell_quote			String::ShellQuote
    shell_comment_quote		String::ShellQuote

    popen_noshell		Proc::SafePipe
    backtick_noshell		Proc::SafePipe
);
my @moved_syms;

push @EXPORT_FAIL, keys %moved_syms;
push @EXPORT_OK, keys %moved_syms;

sub export_fail {
    my $self = shift;
    my (@ret, $new_pkg, $sym, $new_sym, $name_changed);
    for $sym (@_) {
	if ($new_pkg = $moved_syms{$sym}) {
	    if (1) {
		push @moved_syms, $sym;
	    }
	    else {
		carp "Load $sym from $new_pkg instead of $self";
	    }
    	    if ($new_pkg =~ s/=(.*)//) {
		$new_sym = $1;
		$name_changed = 1;
	    }
	    else {
		$new_sym = $sym;
		$name_changed = 0;
	    }
	    eval "require $new_pkg";
	    die if $@;
	    $new_pkg->import($new_sym);
	    if ($name_changed) {
		$new_sym =~ s/^(?=\w)/&/;
		$sym =~ s/^\W//;
		eval "*$sym = \\$new_sym";
		die if $@;
	    }
	}
	else {
	    push @ret, $sym;
	}
    }
    return @ret;
}

END {
    if (@moved_syms) {
	local ($?, $\);
	my $subject = "moved syms for $0";
	$subject =~ s/\n.*//s;
	open MAIL, '|/usr/lib/sendmail -oeq -t';
	print MAIL "To: roderick\n";
	print MAIL "Subject: $subject\n";
	print MAIL "\n";
	print MAIL "Symbols used by $0:\n";
	print MAIL "\t", join("\n\t",
	    map { sprintf "%-14s => %s", $_, $moved_syms{$_} } @moved_syms),
	    "\n";
	print MAIL "\n";
	print MAIL -f '/dgux'
	    ? `/home/roderick/bin/share/ps-climb $$`
	    : `ps -fp $$`;
	close MAIL;
    }
}

use subs qw(badinvo);
sub dstr ($);

sub my_gensym {
    require Symbol;
    return Symbol::gensym();
}

=item B<$Me>

The basename of the currently running script.

=cut

#use File::Basename qw(basename);
#$Me = basename $0 eq '-e' ? $^X : $0;
($Me = $0 ne '-e' ? $0 : $^X) =~ s-.*/--;

=item B<fileline> [I<level>]

Return a string describing the caller's file and line number.  If
I<level> is given it's an additional number of stack frames to go
back.

=cut

sub fileline {
    @_ <= 1 || croak 'bogus invocation of ', (caller 0)[3];
    my $level = shift || 0;
    my ($package, $file, $line) = caller $level;
    return "$file line $line";
}

=item B<chompexpr_fileline> I<s>

Remove a trailing newline from I<s> and then try to remove a trailing
"at $file line $num\n" or similar from the result.  In a scalar context
return just the initial part of the string, in a list context also
include the $file and $num.

=cut

sub chompexpr_fileline {
    @_ == 1 || badinvo;
    local $_ = shift;

    chomp;
    if ($_ =~ /^(.+) at (.+) (?:chunk|line) (\d+)\.\z/s) {
    	return wantarray ? ($1, $2, $3) : $1;
    }
    else {
    	return wantarray ? ($_, undef, undef) : $_;
    }
}

=item B<subname> [I<level>]

Return the name of the current subroutine (that is, the one which invokes
B<subname>).  If I<level> is given, go back that many additional stack
frames and give the name that sub instead.

=cut

sub subname {
    @_ <= 1 || croak 'bogus invocation of ', (caller 0)[3];
    my $level = shift || 0;
    return +(caller $level + 1)[3];
}

=item B<subcall_info> [I<level>]

Return a text string which describes the invocation of the current
subroutine.  Like B<subname>, current really means B<subcall_info>'s
caller, and you can specify a I<level> to go back to a different stack
frame.

=cut

#'

sub subcall_info {
    @_ <= 1 || croak 'bogus invocation of ', (caller 0)[3];
    my $level = shift || 0;
    my $fileline = fileline $level + 2;
    # The $subname in that stack frame is the name of the sub invoked,
    # not the invoker.  So, the invoker is one frame further up.
    my $sub = subname $level + 2;
    return defined $sub ? "$sub ($fileline)" : "$fileline";
}

=item B<badinvo> [I<level> [I<message>]]

Die with a message indicating that the current subroutine was invoked
improperly.  If I<level> is given go back that many additional levels
before the current one.  If I<message> is also given include it in the
die message.

=cut

sub badinvo {
    my $level = @_ ? shift : 0;
    my $msg = @_ ? " (" . join('', @_) . ")" : '';
    die "$Me: bogus invocation of ", subname($level + 1),
        " from ", subcall_info($level + 1), "$msg\n";
}

sub badinvo_warn {
    my @arg = @_;

    # Add 1 for the invocation of this sub, 1 for the eval.
    if (@arg) {
	$arg[0] += 2;
    }
    else {
	push @arg, 2;
    }

    eval { badinvo @arg };
    warn $@;
}

=item B<xwarn> [I<message>]...

=item B<xdie> [I<message>]...

Like C<warn> and C<die> but these functions prepend the message with the
name of the script and a colon.  Further, if the last I<message> ends
with a colon the current string value of C<$!> and a newline are
appended to it.

=cut

sub xwarndie_mess {
    @_ >= 1 || badinvo;
    my $level = shift;

    if (!@_) {
	@_ = (caller 1)[3] =~ /die/ ? ('died') : ("something's wrong");
    }

    my @mess = ("$Me: ", @_);
    $mess[-1] =~ s/:\z/: $!\n/;

    if ($mess[-1] !~ /\n\z/) {
    	# Parse warn()'s output to get at last_in_gv (the most recently
    	# used filehandle), which isn't otherwise available.
    	my $last_read = '';
	{
	    local $SIG{__WARN__} = sub {
		# except the trailing ., which doesn't fit this style
		$last_read = $1
		    if shift =~ / at .*(, <.*> (chunk|line) .*?)\.?$/;
	    };
	    warn "extract file";
	}
	push @mess, " at ", fileline($level + 1), "$last_read\n"
    }

    @mess;
}

sub xwarn	{ warn xwarndie_mess 1, @_ }
sub xdie	{ die xwarndie_mess 1, @_ }

=item B<xwarn_caller> I<additional_frames>, [I<message>]...

=item B<xdie_caller> I<additional_frames>, [I<message>]...

Like C<xwarn> and C<xdie> but the given additional number of stack frames are
climbed to find the file/line from which to warn.  An I<additional_frames>
argument of 0 is equivalent to the non-C<_caller> form.

=cut

sub xwarndie_caller {
    my $type = shift;
    my ($level);

    if (@_ && $_[0] =~ /^\s*\d+$/) {
	$level = shift;
    }
    else {
	badinvo_warn 1, "additional frames not specified";
	$level = 0;
    }

    # 1 for the caller of this helper, 1 for the caller of that wrapper.
    $level += 2;

    my @mess = xwarndie_mess $level, @_;
    if ($type eq 'warn') {
	warn @mess;
    }
    else {
	die @mess;
    }
}

sub xwarn_caller {
    xwarndie_caller 'warn', @_;
}

sub xdie_caller {
    xwarndie_caller 'die', @_;
}

=item B<xcarp> I<arg>...

=item B<xcluck> I<arg>...

=item B<xcroak> I<arg>...

=item B<xconfess> I<arg>...

Like the corresponding function from the Carp module, but as with
B<xwarn> supply the script's name.

=cut

sub xcarp    { require Carp; unshift @_, "$Me: "; goto &Carp::carp;    }
sub xcluck   { require Carp; unshift @_, "$Me: "; goto &Carp::cluck; }
sub xcroak   { require Carp; unshift @_, "$Me: "; goto &Carp::croak;   }
sub xconfess { require Carp; unshift @_, "$Me: "; goto &Carp::confess; }

=item B<process_arg_pairs> I<rargs> [I<key> => I<ref to var>]...

This sub is used to translate argument lists of key/value pairs.  The
I<rargs> is a reference to the list of arguments which were given to your
sub.  The I<key>/I<ref to var> pairs are your subs arguments and the
variables they set.  If an invalid argument is present in the I<rargs>
list this sub will die.

=cut

sub process_arg_pairs {
    my ($rarg, %desc) = @_;
    my @arg = @$rarg;
    my $frames = 2;

    @arg % 2 == 0
	or xdie_caller $frames, "odd number of elements in key/value list";
    while (@arg) {
        my ($key, $val) = splice @arg, 0, 2;
        if ($desc{$key}) {
            ${ $desc{$key} } = $val;
        } else {
	    xdie_caller $frames, "invalid arg key `$key'";
        }
    }
}

=item B<dirents> I<dir>

Returns all the entries in I<dir> except dot and dotdot.

=cut

sub dirents {
    @_ == 1 or badinvo;
    my $dir = shift;
    my $dh = my_gensym;
    opendir $dh, $dir or croak "Can't opendir $dir: $!";
    my @ents = grep { $_ ne '.' and $_ ne '..' } readdir $dh;
    closedir $dh or croak "Error running closedir($dir): $!";
    @ents;
}

=item B<dirents_qualified> [I<dir>]...

Returns fully qualified pathnames of all the entries in each listed
I<dir> except dot and dotdot.  If no I<dir>s are given then an empty
list is returned.

=cut

sub dirents_qualified {
    my ($dir, $name, @ents);
    foreach $dir (@_) {
	$name = $dir eq '/' ? '' : $dir;
	push @ents, map { "$name/$_" } dirents $dir;
    }
    @ents;
}

=item B<fopen_mode> I<mode>

This function turns an fopen()-type mode string (like C<'r+'>) and
returns the Perl equivalent (C<'+E<lt>'>).

=cut

my %fopen_mode = ();
sub fopen_mode {
    @_ == 1 or badinvo;
    my $fopen_mode = shift;
    %fopen_mode = (
	'r'	=> '<',		'rb'	=> '<',
	'w'	=> '>',		'wb'	=> '>',
	'a'	=> '>>',	'ab'	=> '>>',
	'r+'	=> '+<',	'r+b'	=> '+<',	'rb+'	=> '+>',
	'w+'	=> '+>',	'w+b'	=> '+>',	'wb+'	=> '+<',
	'a+'	=> '+>>',	'a+b'	=> '+>>',	'ab+'	=> '+>>',
    ) unless %fopen_mode;
    $fopen_mode{$fopen_mode};
}

=item B<fdopen> I<fd> I<mode>

Like the system fdopen().  I dislike that POSIX.pm tells you to use
C<FileHandle-E<gt>new_from_fd>.  Further, FileHandle's new_from_fd() is
broken, it does not get the mode right if it's read/write.

=cut

sub fdopen {
    @_ == 2 or badinvo;
    my ($fd, $mode) = @_;
    my $glob = my_gensym;
    $mode = fopen_mode($mode) || croak "Invalid mode `$mode'";
    open($glob, "$mode&=$fd")
	and bless $glob, 'FileHandle';
}

=item B<fhbits> I<filehandle>...

This routine returns a bitmask which includes all the specified
filehandles.

=cut

sub fhbits {
    my @fh = @_;
    my ($fh, $bits);
    require Symbol;
    @fh = map { Symbol::qualify($_) } @fh;
    $bits = '';
    no strict 'refs';
    foreach $fh (@fh) {
	defined fileno $fh or croak "`$fh' isn't an open filehandle";
        vec($bits, fileno $fh, 1) = 1;
    }
    $bits;
}

my %binary_scaled_suffix;
my $binary_scaled_suffix_pat;
sub binary_scaled {
    my (@arg) = @_;
    my ($arg, @ret);

    unless (%binary_scaled_suffix) {
	%binary_scaled_suffix = (
	    'b'	=> 512,
	    'k' => 1024,
	    'm' => 1024**2,
	    'g' => 1024**3,
	);
	# Note, this assumes suffixes are single characters.
	$binary_scaled_suffix_pat = '[' . join('', keys %binary_scaled_suffix)
	    	    	    	    . ']';
    }

    foreach $arg (@arg) {
	$arg =~ /^(\d+)\s*($binary_scaled_suffix_pat)?$/io
	    or croak "Invalid binary-scaled number `$arg'";
	push @ret, $1 * (defined($2) ? $binary_scaled_suffix{"\L$2"} : 1);
    }
    wantarray ? @ret : $ret[$#ret];
}

sub plural {
    @_ == 1 or @_ == 2 or badinvo;
    my ($n, $p) = @_;
    $n == 1 ? '' : (defined $p ? $p : 's');
}

sub pluralx ($$$) {
    my ($n, $s, $p) = @_;
    $n == 1 ? $s : $p;
}

=item B<uncontrol> [I<string>]...

This function joins all the I<string> arguments and expands control
characters as ^A and meta characters as \200 and the like.  It returns
the concatenated result.

=cut

my @uncontrol_tab;
sub uncontrol {
    unless (@uncontrol_tab) {
	foreach (0..037) {
	    $uncontrol_tab[$_] = '^' . chr($_ + ord '@');
	}
	foreach (0177) {
	    $uncontrol_tab[$_] = '^' . chr($_ - ord '@');
	}
	foreach (040..0176) {
	    $uncontrol_tab[$_] = chr $_;
	}
	foreach (0200..0377) {
	    $uncontrol_tab[$_] = sprintf '\\%03o', $_;
	}
    }
    my $ret = '';
    foreach (split //, join('', @_)) {
	$ret .= $uncontrol_tab[ord $_];
    }
    $ret;
}

=item B<uncontrol_emacs> [I<string>]

This function joins all the I<string> arguments together and expands
control characters to C-A and meta characters to M-A and whitespace to
SPC and RET and so on in the emacs fashion.  It returns the concatenated
result.

=cut

my %emacs_xlate;
sub uncontrol_emacs {
    unless (%emacs_xlate) {
	%emacs_xlate = (
	    "\0"	=> 'NUL',
	    "\040"	=> 'SPC',
	    "\r"	=> 'RET',
	    "\n"	=> 'LFD',
	    "\t"	=> 'TAB',
	    "\f"	=> 'FF',
	    "\177"	=> 'DEL',
	    "\e"	=> 'ESC',
	);
    }
    my $ret = '';
    my ($ord, $meta, $space);
    $space = '';
    foreach (split //, join('', @_)) {
	$ord = ord $_;
	if ($ord > 0177) {
	    $meta = 'M-';
	    $ord -= 0200;
	    $_ = chr $ord;
	} else {
	    $meta = '';
	}
	if (defined $emacs_xlate{$_}) {
	    $ret .= $space . $meta . $emacs_xlate{$_} . ' ';
	    $space = '';
	} elsif (ord $_ < 040) {
	    $ret .= $space . 'C-' . $meta . chr($ord + 0140) . ' ';
	    $space = '';
	} else {
	    $ret .= $meta . $_ . ($meta ? ' ' : '');
	    $space = $meta ? '' : ' ';
	}
    }
    chop $ret if substr($ret, -1) eq ' ';
    $ret;
}

=item B<f_getfl> I<filehandle>

This performs an F_GETFL on I<filehandle> and returns the flags.

=cut

sub f_getfl {
    @_ == 1 or badinvo;
    my $fh = shift;
    require Fcntl;
    my $flags = fcntl $fh, Fcntl::F_GETFL(), 0
	or croak "Can't F_GETFL on $fh: $!";
    local $^W;
    $flags+0;
}

=item B<f_setfl> I<filehandle> I<flags>

This performs an F_SETFL using the arguments.

=cut

sub f_setfl {
    @_ == 2 or badinvo;
    my ($fh, $flags) = @_;
    require Fcntl;
    fcntl $fh, Fcntl::F_SETFL(), 0+$flags
	or croak "Can't F_SETFL on $fh: $!";
}

=item B<getopt> [B<-bundle> | B<-bundling>] [I<GetOptions-arg>...]

This is basically Getopt::Long but it has the defaults set up the way I
think they should be.

=cut

sub getopt {
    # Don't bother if there aren't any switches.  This test works because
    # I'm setting $REQUIRE_ORDER.
    return 1 unless @ARGV && substr($ARGV[0], 0, 1) eq '-';

    my $bundling = 0;
    if (@_ && ($_[0] eq -bundle || $_[0] eq -bundling)) {
	$bundling = 1;
	shift;
    }

    {
	# I'm setting this environment variable when loading Getopt::Long
	# so that the defaults for options added later (which aren't set
	# explicitly below) are more likely to match what I'd like.
	local $ENV{POSIXLY_CORRECT} = 1;
	require Getopt::Long;
    }

    Getopt::Long->VERSION(2.19);
    Getopt::Long::Configure(
	'no_auto_abbrev',
	'no_getopt_compat',
	'require_order',
	$bundling ? 'bundling' : (),
	'no_ignore_case',
	'prefix_pattern=(--|-)',
    ) if 1;

    # The getopt function puts the vars into its caller's package so
    # it's necessary to jump to it so that its caller is my caller.
    goto &Getopt::Long::GetOptions;
}

=item B<exclusive_create> I<path>

This is like a call to the system open() with flags O_CREAT, O_EXCL and
O_RDWR set.  A Perl filehandle is returned.

=cut

sub exclusive_create {
    @_ == 1 or @_ == 2 or badinvo;
    my $file = shift;
    my $mode = @_ ? shift : 0666;
    require FileHandle;
    require Fcntl;
    return new FileHandle $file,
			    Fcntl::O_CREAT()|Fcntl::O_EXCL()|Fcntl::O_RDWR(),
			    $mode
}

=item B<tmpfile>

This is just like the C function tmpfile().  I dislike that POSIX.pm
tells you to use C<FileHandle-E<gt>new_tmpfile> (though I'm unsure why I
don't just have this function call that one).

=cut

sub tmpfile {
    @_ == 0 or badinvo;
    my $base = "/tmp/$Me.$$";
    my $ext = 0;
    my $file = $base;
    my $fh;
    until ($fh = exclusive_create $file, 0666) {
	$ext++ > 100
	    and xdie "&tmpfile: can't open a file named like $base.<number>\n";
	$file = "$base.$ext";
    }
    unlink $file or xdie "&tmpfile: can't unlink $file:";
    $fh;
}

=item B<safe_tmp> [I<arg>]...

This routine safely creates temporary files and directories.  The
default is files, specify C<mkdir> as an arg to make a directory
instead.  In scalar context the return value is the name of the file
or directory created.  In array context the file-creating version
also returns a filehandle opened in read/write mode on the file.
Right now it's an error to call the mkdir version in array context.

Valid args are:

=over 4

=item B<mkdir>

This boolean option indicates that a directory rather than a file should
be created.

=item B<fh> => I<filehandle>

Create the file using the given filehandle.  If you don't specify this
a filehandle is generated for you.  You can retrieve it if you call
B<safe_tmp> in array context.

=item B<dir> => I<directory>

Place the created file in this directory.  The default is the user's
$TMPDIR or F</tmp>.  Note that both the B<dir> arg and $TMPDIR are
ignored if the C<prefix> contains a C</>.

=item B<prefix> => I<string>

Specify the part of the file name which precedes the random part.  The
default comes from $0 or (if that doesn't work out) the user's name.  If
this value contains a C</> the C<dir> argument is ignored.

=item B<loginprefix>

Use the user name, not the script name, as the preferred default B<prefix>.

=item B<postfix> => I<string>

Specify the part of the file name which comes after the random part.
There is no default, as you don't normally need one.  It's useful for
programs which require that files have a certain extension.

    $zip_file = safe_tmp postfix => '.zip'
		    or xdie "can't create temporary file:";

=item B<mode> => I<number>

Specify the file creation mode.  The default is 0600 for files and 0700
for directories.

=back

=cut

sub safe_tmp {
    my @arg = @_;
    my ($fh, $mkdir, $dir, $prefix, $loginprefix, $postfix, $mode);
    require Fcntl;

    while (@arg) {
	my $k = shift @arg;
	my $rval;
	if ($k eq 'mkdir')	{ $mkdir = 1 }
	elsif ($k eq 'fh')	{ $rval = \$fh }
	elsif ($k eq 'dir')	{ $rval = \$dir }
	elsif ($k eq 'prefix')	{ $rval = \$prefix }
	elsif ($k eq 'loginprefix') { $loginprefix = 1 }
	elsif ($k eq 'postfix')	{ $rval = \$postfix }
	elsif ($k eq 'mode')	{ $rval = \$mode }
	else {
	    croak "Invalid arg `$k' passed to safe_tmp";
	}

	if ($rval) {
	    @arg or croak "No arg specified for $k";
	    $$rval = shift @arg;
	}
    }

    if ($mkdir) {
	defined $fh and croak 'Both mkdir and fh specified';
    }
    else {
	require Symbol;
	if (defined $fh) {
	    $fh = Symbol::qualify_to_ref($fh, scalar caller);
	}
	else {
	    $fh = my_gensym;
	}
    }

    if (!defined $prefix) {
	my $cmd = $loginprefix ? '' : $0;
	if ($cmd eq '-e') {
	    $cmd = $^X;
	}
	elsif ($cmd eq '') {
	    $cmd = $ENV{LOGNAME} || $ENV{USER} || getlogin
		    || getpwuid $< || $<;
	}
	$cmd =~ s-.*/--;
	$cmd =~ s/^\.+//;
	$cmd =~ tr/a-zA-Z0-9_.-/_/cs;
	$cmd = ($cmd =~ /^([\w.\-]+)$/) ? $1 : '';	# untaint
	$cmd = 'safe_tmp' if $cmd eq '';		# can't happen
	$prefix = "$cmd.";
    }

    if ($prefix =~ m-/-) {
    	$dir = '';
    }
    elsif (!defined $dir) {
	$dir = $ENV{TMPDIR} if !tainted($ENV{TMPDIR});
	$dir = '/tmp' if !defined $dir;
    }
    $dir .= '/' if $dir ne '' && substr($dir, -1) ne '/';
    $postfix = '' if !defined $postfix;
    $mode = $mkdir ? 0700 : 0600 if !defined $mode;

    # 36^6 == 2 billion possible names
    my @alphabet = ('a'..'z', 0..9);
    xsrand();
    for (my $attempts = 0; $attempts < 10; $attempts++) {
	my $file = "$dir$prefix";
	$file .= join '', map { $alphabet[rand @alphabet] } 1..6;
	$file .= $postfix;
	if ($mkdir) {
	    wantarray
		and croak 'safe_tmp mkdir called in array context';
	    mkdir $file, $mode and return $file;
	}
	elsif (sysopen $fh, $file,
		Fcntl::O_RDWR() | Fcntl::O_CREAT() | Fcntl::O_EXCL(),
		$mode) {
	    return wantarray ? ($file, $fh) : $file;
	}
	{
	    local $!;
	    require POSIX;
	}
	$! == POSIX::EEXIST() or return;
    }
    # I couldn't find a name which didn't exist, return error with
    # existing $! (== EEXIST).
    return;
}

=item B<home>

This function returns the current home directory, preferring $HOME to
looking it up in the password file.  It doesn't cache the result.

=cut

#'

sub home {
    @_ == 0 or badinvo;
    $ENV{HOME}
	|| (getpwuid($<))[7]
	|| xdie "can't get your home directory\n";
}

=item B<home_of> I<user>

This function returns the home directory for the given user, or dies.

=cut

sub home_of {
    @_ == 1 or badinvo;
    my ($user) = @_;

    my @pw = getpwnam $user
    	or xdie_caller 1, "invalid user ", dstr $user;

    my $dir = $pw[PW_DIR];
    defined $dir && $dir ne ''
	or xdie_caller 1, "user $user has invalid home directory ", dstr $dir;

    return $dir;
}

=item B<:stat>

This tag gives you subs named ST_DEV, ST_INO, ST_MODE and so on which
return indices into a stat() or lstat() array.

=item B<:tm>

This tag gives you subs named TM_SEC, TM_MIN, and so on which return
indices into a gmtime() or localtime() array.

=item B<full_name_uid> I<uid>

Return a best guess at the full name for the user with uid I<uid>, but
always return something usable (rather than dying or returning undef).

=cut

# A cache mapping uid => full name.
my %full_name_uid;

sub full_name_uid {
    @_ == 1 or badinvo;
    my $uid = shift;
    my ($login, $name);

    return $full_name_uid{$uid}
	if exists $full_name_uid{$uid};

    if (($login, $name) = (getpwuid($uid))[0, 6]) {
	# BSD-type (?) gecos has 4 comma-separated fields (full name,
	# office location, office phone, home phone).  I'm just
	# stripping everything after a comma in case some people use a
	# different number of fields.  This loses for `Joe User, Jr.',
	# alas.
	$name =~ s/,.*//; # if ($name =~ tr/,/,/) == 3;
	# & in the full name field means to insert the capitalized
	# login name.
	$name =~ s/&/ucfirst $login/eg;
    } else {
	$name = "uid-$uid";
    }

    $full_name_uid{$uid} = $name;
    return $name;
}

=item B<full_name>

Call B<full_name_uid> with the real user id.

=cut

sub full_name {
    @_ == 0 or badinvo;
    full_name_uid $<;
}

=item B<pwuid> I<uid>

Return the user name associated with I<uid>.

=cut

my %pwuid;
sub pwuid ($) {
    my $uid = shift;
    $pwuid{$uid} ||= getpwuid($uid) || "uid$uid"
}

=item B<grgid> I<gid>

Return the group name associated with I<gid>.

=cut

my %grgid;
sub grgid ($) {
    my $gid = shift;
    $grgid{$gid} ||= getgrgid($gid) || "group$gid"
}

=item B<cat> [I<path>]...

Return the contents of each I<path>, one line per element.

=cut

sub cat (@) {
    require FileHandle;
    my $fh;
    map {
    	$fh = new FileHandle $_ or croak "Can't read $_: $!";
	$fh->getlines;
    } @_;
}

=item B<catslurp> [I<path>]...

Return the contents of each I<path>, in a list context with each file in
a separate returned value, in a scalar context all joined together.

=cut

sub catslurp (@) {
    local *FH;
    my @r;
    local $/ = undef;
    for my $file (@_) {
    	open FH, $file or xdie_caller 1, "can't read $file:";
	push @r, <FH>;
    }
    close FH;
    return wantarray ? @r : join '', @r;
}

=item B<xsrand>

A better srand().

=cut

my $xsrand_called;

sub xsrand () {
    if ($xsrand_called++) {
	# Do nothing, already called.
    }
    elsif ($] >= 5.003_90) {
	# Do nothing, automatic srand.
    }
    elsif ($] >= 5.003_01) {
	# Improved default seed.
	srand;
    }
    else {
	srand(time ^ (($$ << 15) + $$));
    }
}

=item B<shuffle> [I<item>]...

Return all the I<item>s in random order.

=cut

# Fisher/Yates shuffle from PCB.

sub shuffle {
    my @a = @_;
    my ($i, $j);

    return unless @a;
    xsrand;
    for ($i = @a; --$i; ) {
	$j = int rand $i+1;
	next if $i == $j;
	@a[$i, $j] = @a[$j, $i];
    }
    return @a;
}

=item B<yorn> I<default> I<prompt> [I<no_modify_prompt>] [I<option> =>
I<value>]...

Prompt with I<prompt> for a [YNyn] (or allow enter alone to mean the
I<default>, if one was given).  If I<no_modify_prompt> is not set then "
(y/n) [$default] " is appended to the I<prompt>.  Options are:

    iosub	sub to print prompt ($_[0]) and return response
    localize	Locale::Maketext handle

=cut

sub yorn {
    @_ < 2 and badinvo;
    my ($default, $prompt, $no_modify_prompt, @opt) = @_;
    my ($iosub, $localize);
    local $| = 1;

    $iosub = sub {
    	print shift;
    	return scalar <STDIN>;
    };

    process_arg_pairs \@opt,
	iosub		=> \$iosub,
	localize	=> \$localize;

    my ($y, $n) = qw(Y N);
    if ($localize) {
	$y = $localize->maketext('Y');
	$n = $localize->maketext('N');
    }

    if (defined $default) {
	$default = uc $default;
    	$default = { Y => $y, N => $n }->{$default};
    }

    if (!$no_modify_prompt) {
	$prompt =~ s/\s+$//;
	$prompt .= " ($y/$n) ";
	$prompt .= "[$default] " if defined $default;
	$prompt = wrap('', '', $prompt);
    }

    while (1) {
	my $ans = $iosub->($prompt);
	if (!defined $ans) {
	    xdie_caller 1, 'EOF';
	}
	chomp $ans;
	$ans = uc $ans;
	if ($ans eq '') {
	    $ans = $default;
	    next unless defined $ans;
	}
	if ($ans eq $y) { return 1 }
	if ($ans eq $n) { return 0 }
    }
}

=item B<prompt> I<prompt> I<ref_to_choices> [I<option> => I<value>]...

Display I<prompt> and prompt the user until she enters one of the
choices in the list pointed to by I<ref_to_choices>.  Options are:

    default		set default response
    downcase		downcase the input
    iosub	sub to print prompt ($_[0]) and return response
    localize		Locale::Maketext handle
    no_modify_prompt	don't add the default tot he prompt
    upcase		upcase the input
    wash		do: $input = $wash->($input);

A choice which is an RE object (C<qr//>) accepts anything which matches.

A choice of undef matches EOF from the user.  Without that this function
will die if it reads EOF.

=cut

sub prompt {
    my ($prompt, $rchoice, @opt) = @_;
    my ($allow_eof, $default, $iosub, $no_modify_prompt, $upcase, $downcase,
    	    $her_wash, $localize, $ans, %choice, @re);
    local $| = 1;

    $iosub = sub {
    	print shift;
    	return scalar <STDIN>;
    };

    process_arg_pairs \@opt,
	default			=> \$default,
	downcase		=> \$downcase,
	iosub			=> \$iosub,
	localize		=> \$localize,
	no_modify_prompt	=> \$no_modify_prompt,
	upcase			=> \$upcase,
	wash			=> \$her_wash;

    my $wash = sub {
	my $s = shift;
	if ($upcase) {
	    $s = uc $s;
	}
	elsif ($downcase) {
	    $s = lc $s;
	}
	$s = $her_wash->($s) if $her_wash;
	return $s;
    };

    for (@$rchoice) {
	if (!defined) {
	    $allow_eof = 1;
	}
	elsif (ref $_ && $_->isa('Regexp')) {
	    push @re, $_;
	}
	else {
	    $choice{$wash->($_)} = 1;
	}
    }

    if (defined $default) {
	$default = $wash->($default);
	$choice{$default}
	    or xdie_caller 1, "default `$default' isn't a valid choice";
    }

    if (!$no_modify_prompt) {
	$prompt =~ s/\s+$//;
	$prompt .= " [$default]" if defined $default;
	$prompt .= ' ';
	$prompt = wrap('', '', $prompt);
    }

  Prompt:
    while (1) {
	$ans = $iosub->($prompt);
	if (!defined $ans) {
	    return $ans if $allow_eof;
	    xdie_caller 1, 'EOF';
	}
	chomp $ans;
	$ans = $wash->($ans);
	$ans = $default if $ans eq '' && defined $default;
	last if $choice{$ans};
	for (@re) {
	    last Prompt if $ans =~ /$_/;
	}
	my $msg = "Invalid response `%s'.";
	my $qans = uncontrol $ans;
	if ($localize) {
	    print $localize->maketext(sprintf($msg, '[_1]'), $qans), "\n";
	}
	else {
	    printf "$msg\n", $qans;
	}
    }

    return $ans;
}

=item B<data_dump> I<item>...

Format some data for a human to read, for debugging.

=cut

{ my $use_yaml;
sub data_dump {
    if (!defined $use_yaml) {
	if (eval { local $SIG{__DIE__}; require YAML }) {
	    $use_yaml = 1;
	}
	else {
	    $use_yaml = 0;
	    require Data::Dumper;
	}
    }

    if ($use_yaml) {
	goto &YAML::Dump;
    }
    else {
	local $Data::Dumper::Useqq = 1;
	# Dumpxs() doesn't support $Useqq.
	goto &Data::Dumper::Dumper;
    }
} }

=item B<data_dump_unsorted> I<item>

This is like data_dump(), but it doesn't try to sorted hash keys.  This
is useful for large or Tie::IxHash hashes.

=cut

sub data_dump_unsorted {
    require YAML;
    local $YAML::SortKeys = 0;
    return &YAML::Dump;
}

=item B<untaint> [I<arg>]...

Untaint and return the args.

=cut

sub untaint {
    my @s = @_;
    my @r;
    croak "Untaint: multiple args given but called in a scalar context"
	if @s > 1 and not wantarray;
    foreach (@s) {
	/(.*)/s;
	push @r, $1;
    }
    return wantarray ? @r : $r[0];
}

=item B<tainted> [I<arg>]...

Return true if any of the args are tainted.

=cut

{ my $code;

sub tainted {
    local $^W;
    if (!$code) {
	$code = eval q{ sub {
	    return !eval {
		local $SIG{__DIE__};
		join('', @_), kill 0;
		1;
	} } };
	die if $@;
    }
    return $code->(@_);
} }

=item B<rename_nounlink> I<source> I<dest>

This is intended to be like rename(2) except that it will fail if
I<dest> exists.  It returns true if it succeeds and false (with $! set
appropriately) otherwise.  Since it's implemented with a link()/unlink()
pair it isn't atomic, alas.  If something goes strangely wrong it can
leave both the I<source> and I<dest> links on the disk, but it tries not
to let that happen.

=cut

sub rename_nounlink {
    @_ == 2 or badinvo;
    my ($src, $dest) = @_;
    my $errno;

    unless (link $src, $dest) {
	return 0;
    }
    if (unlink $src) {
	return 1;
    }
    $errno = $!+0;
    unless (unlink $dest) {
	# This shouldn't happen, so always carp about it.  It could
	# happen if somebody changed directory permissions on me.
	carp "Linked $src to $dest, but couldn't unlink $dest: $!";
    }
    $! = $errno;
    return 0;
}

=item B<rename_unique> I<source> I<dest> [I<max-tries>]

This function renames I<source> to I<dest> without overwriting an
existing I<dest>.  If I<dest> exists B<rename_unique> appends a numberic
extension and tries again, up to I<max-tries> (default 100) times.
Returns the resulting file name if all goes well, undef otherwise.  If
it returns undef you can check $! for the errno and $@ for a more
verbose description of the problem.  It uses link() so usually I<source>
and I<dest> have to be on the same filesystem, and further it isn't
atomic (there is a period during which both I<source> and I<dest> exist
on the disk).  If something goes strangely wrong it can leave both the
I<source> and I<dest> links on the disk, but it tries not to let that
happen.

=cut

#'

sub rename_unique {
    @_ == 2 || @_ == 3 or badinvo;

    my $src = shift;
    my $base = shift;
    my $max_tries = @_ ? shift : 100;
    croak "Invalid max_tries $max_tries" if $max_tries < 1;

    my ($dest, $ext, $errno);
    $dest = $base;
    $ext = 1;

    until (link ($src, $dest)) {
    	$errno = $!+0;
	require POSIX;
	unless ($errno == POSIX::EEXIST()) {
	    $! = $errno;
	    $@ = "Can't link $src to $dest: $!";
	    return undef;
	}
	$ext++;
	if ($ext > $max_tries) {
	    $! = POSIX::EEXIST();
	    $@ = "Couldn't link $src to $base.* in " . ($ext-1) . " tries";
	    return undef;
	}
	$dest = "$base.$ext";
    }
    if (unlink $src) {
	return $dest;
    }
    $errno = $!+0;
    unless (unlink $dest) {
	# This shouldn't happen, so always carp about it.  It could
	# happen if somebody changed directory permissions on me.
	carp "Linked $src to $dest, but couldn't unlink it: $!";
    }
    $! = $errno;
    $@ = "Couldn't unlink $src after linking it to $dest: $!";
    return undef;
}

=item B<filter_string> I<string> I<command> [I<arg>]...

B<filter_string> runs the I<command> with I<string> on its stdin and
returns the stdout (as a string in a scalar context or an list of lines
in an array context).  If there is a system problem B<filter_string>
croaks.  The exit status of the command is in B<$?>.

=cut

sub filter_string {
    my $s = shift;
    my @cmd = @_;
    my ($tmp_fh, $reader, $writer, $pid, @s);

    $tmp_fh = tmpfile;
    print $tmp_fh $s		or croak "Error writing to temporary file: $!";
    $tmp_fh->flush		or croak "Error flushing temporary file: $!";
    $tmp_fh->seek(0, 0)		or croak "Error rewinding temporary file: $!";

    $reader = new FileHandle;
    $writer = new FileHandle;
    pipe $reader, $writer or croak "Can't pipe: $!";
    require Proc::SyncExec;
    $pid = Proc::SyncExec::sync_exec(sub {
			open STDIN, '<&' . fileno $tmp_fh
			    and close $tmp_fh
			    and open STDOUT, '>&' . fileno $writer
			    and close $writer
			    and close $reader
		    }, @cmd)
		or croak "Error running $cmd[0]: $!";
    close $writer or croak "Error closing write pipe: $!";
    close $tmp_fh or croak "Error closing temporary file: $!";

    @s = <$reader>;

    close $reader or croak "Error closing read pipe of $cmd[0]: $!";
    waitpid($pid, 0) == $pid or croak "Error waiting for $cmd[0] pid $pid: $!";

    return wantarray ? @s : join '', @s;
}

=item B<mbox_read_head> I<fh>

This function reads an email header from from I<fh> and returns it.  In
an array context a content length indicator is also returned, you can
use it when calling mbox_read_body().

=cut

sub mbox_read_head {
    @_ == 1 or badinvo;
    my $fh = shift;
    my ($head, $clen);
    local $_;

    # Skip initial blank lines, they're not valid and they screw up the
    # following logic.
    do {
	$head = <$fh>
    } while defined $head && $head =~ m!^\s*$/!;
    defined $head or return ();

    unless ($head =~ /^From /) {
	chomp $head;
	if (length $head > 100) {
	    substr($head, 100) = '';
	    $head = dstr($head) . "...";
	}
	else {
	    $head = dstr $head;
	}
	croak "Invalid message (first line isn't From_, it's $head)";
    }

    while (<$fh>) {
	last if $_ eq $/;
	$head .= $_;
	$clen = $1 if m!^Content-Length\s*:\s*(\d+)\s*$/!i;
    }

    # Drop Content-Length.
    $head =~ s!^Content-Length\s*:.*$/(\s+.*$/)*!!mg;

    return wantarray ? ($head, $clen) : $head;
}

=item B<mbox_read_body> I<fh> [I<skip-or-callback> [I<clen>]]

This function reads an email message body from the given file handle.
The file handle is left pointing at the start of the next message, or at
EOF.  Normally the body is returned (with the mbox encodings undone).

I<skip-or-callback> has two functions:

=over 4

=item *

If it's a code ref then it's called to process parts of the body.  The
text to process is the only arg the code receives.  It is allowed to
modify this string if it wishes.  The string might contain multiple
lines, and it might contain partial lines.

If the body is empty the callback will still be called once, with an
empty string as argument.

=item *

Otherwise if I<skip-or-callback> is true then the body is just discarded.

=back

In either of these cases B<mbox_read_body> returns the length of the
body rather than the body itself.  This length is the raw length from
the start of the body to the start of the next message.

NB:  The I<clen> is the value from the Content-Length header, as returned
by B<mbox_read_head>.  It is used for parsing messages which were saved
using a Content-Length header instead of B<From_> line escaping.  If you
might have any such it's important that you include it.

=cut

sub Debug_mbox_read_body () { 0 }

sub mbox_read_body {
    @_ >= 1 && @_ <= 3 or badinvo;
    my ($fh, $skip_or_callback, $clen) = @_;
    local $_;

    my $callback	= ref $skip_or_callback eq 'CODE'
			    ? $skip_or_callback
			    : undef;
    my $skip		= defined $callback ? 0 : $skip_or_callback;

    my $called_back	= 0;		# $callback was invoked
    my $clen_ok		= 0;		# $clen was used
    my $body_pos	= tell $fh;
    my $body		= '';
    my $body_is_empty	= 1;
    my $body_has_newline= 0;

    my $process_body_data		= sub {
    	my $length = length $_[0];

	if ($length) {
	    $body_is_empty = 0;

	    # This could fail if it were possible to split the $/ across
	    # 2 invocations of this sub.  The current imlementation
	    # doesn't allow that.

	    $body_has_newline
		= $length >= length($/) && substr($_[0], -length $/) eq $/;
	}

	if ($callback) {
	    $callback->($_[0]);
	    $called_back = 1;
	}
	elsif ($skip) {
	    # do nothing
	}
	else {
	    $body .= $_[0];
	}
    };

    # Try to use the content-length if it was provided.

    if (defined $clen) {
	my $nread = undef;
	my $xbody = '';

	print "trying clen $clen\n"
	    if Debug_mbox_read_body;

	# First read/seek past $clen bytes.  For a normal mboxcl or
	# mboxcl2 message this would leave you at the trailing newline.
	# Elm's buggy implementation, though, includes the trailing
	# newline in the content length so it might leave you at the
	# next From_ line or at EOF.

	if ($skip) {
	    # I have to test the $clen against the file's actual size,
	    # because, since a seek past EOF works, a too-big $clen
	    # would look okay.

	    my $size	= -s $fh;
	    my $new_pos	= $body_pos + $clen;
	    if ($new_pos > $size) { # allows you to be exactly at EOF
		print "clen too big ($body_pos + $clen = $new_pos > $size)\n"
		    if Debug_mbox_read_body;
	    	$nread = undef;
	    }
	    else {
		print "seeking to $new_pos ($body_pos + $clen <= $size)\n"
		    if Debug_mbox_read_body;
		seek $fh, $new_pos, 0
		    or xdie_caller 1, "can't seek in mbox:";
		$nread = $clen;
	    }
	}
	else {
	    $nread = read $fh, $xbody, $clen;
	}

	if (defined $nread && $nread == $clen) {
	    $_	= <$fh>;

	    # If the next line is blank this might be a proper mboxcl
	    # encoding rather than Elm's bug, check the next line to
	    # see.

	    if (!defined $_) {
		print "nread got to EOF\n"
		    if Debug_mbox_read_body;
		# $nread goes to EOF, so figure there was a blank line
		# there and this is the buggy Elm version
	    }
	    elsif ($_ eq $/) {
		print "on blank line, checking next for From_\n"
		    if Debug_mbox_read_body;
		$nread += length $_;
		$_ = <$fh>;
	    }

	    if (!defined $_) {
		# That $clen was good, it left me at EOF.
		$clen_ok = 1;
	    }
	    elsif (/^From /) {
		# That $clen was good, it left me at a From_ line.
		$clen_ok = 1;
	    }
	    else {
		# It wasn't good, that's neither EOF nor a From_ line.
	    }
	}
	else {
	    # There weren't even that many bytes.
	}

    	if ($clen_ok) {
	    # remove trailing blank line

	    if ($xbody eq $/) {
		$xbody = '';
	    }
	    elsif (length($xbody) >= 2 * length $/
		    && substr($xbody, -2 * length $/) eq "$/$/") {
		substr($xbody, -length $/) = ''
	    }

	    $process_body_data->($xbody);
	}
	else {
	    print "clen was bad\n"
		if Debug_mbox_read_body;
	}

	# If the body was bad seek back to the start of it, if it was
	# good seek back a line to the From_ of the next message.

	print "clen_ok=$clen_ok body_pos=$body_pos nread=$nread\n"
	    if Debug_mbox_read_body;
	seek $fh, $body_pos + ($clen_ok ? $nread : 0), 0
	    or xdie_caller 1, "can't seek in mbox:";
    }

    # If the body hasn't been read yet read it in standard mbox form and
    # unescape the >From_ lines.

    if (!$clen_ok) {
	my $pending_blank = 0;
	while (defined($_ = <$fh>) || $pending_blank) {
	    if (defined $_ && /^From /) {
		# forget about any $pending_blank
		seek $fh, -length($_), 1
		    or xdie_caller 1, "can't seek in mbox:";
	    	print "found From_ at ", tell($fh), "\n"
		    if Debug_mbox_read_body;
		last;
	    }

	    if (!defined $_) {
	    	# forget about any $pending_blank
		last;
	    }

	    if ($pending_blank) {
		$pending_blank = 0;
		$process_body_data->(my $modifiable = $/);
	    }

	    if ($_ eq $/) {
	    	$pending_blank = 1;
	    }
	    else {
		s/^>(>*From )/$1/;
	    	$process_body_data->($_);
	    }

	}
    }

    if (!$body_is_empty && !$body_has_newline) {
	$process_body_data->(my $modifiable = $/);
    }

    if ($callback && !$called_back) {
	$process_body_data->(my $modifiable = "");
    }

    return $skip_or_callback ? (tell($fh) - $body_pos) : $body;
}

=item B<mbox_read> I<fh>

This function reads a mail message from I<fh> using mbox_read_head() and
mbox_read_body() (which see).  In an array context it returns the head
and the body as two elements, in a scalar context it returns them as a
single string joined with a blank line.

=cut

sub mbox_read {
    @_ == 1 or badinvo;
    my ($fh) = @_;
    my ($head, $clen, $body);

    ($head, $clen) = mbox_read_head $fh
	or return;
    $body = mbox_read_body $fh, 0, $clen;
    return wantarray ? ($head, $body) : "$head$/$body";
}


=item B<mbox_escape_body_part_in_place> I<s>

Perform mboxrd-style body escaping on I<s>, modifying the argument in
place.

=cut

sub mbox_escape_body_part_in_place {
    @_ == 1 || badinvo;

    $_[0] =~ s/^(>*From )/>$1/mg;
}

=item B<mbox_escape> I<msg>

This function escapes a mail message into mboxrd format, the result can
be written directly to a file.

=cut

sub mbox_escape {
    @_ == 1 or badinvo;
    my $msg = shift;

    length($msg) or croak "Invalid message (empty)";
    unless ($msg =~ /^From /) {
	$msg =~ m!^(.*?)($/|\z)!;
	my $head = $1;
	if (length $head > 100) {
	    substr($head, 100) = '';
	    $head = dstr($head) . "...";
	}
	else {
	    $head = dstr $head;
	}
	croak "Invalid message (doesn't start with From_, first line is $head)";
    }
    $msg .= $/
	if length($msg) < length($/) || substr($msg, -length $/) ne $/;
    my ($head, $body) = split m!$/$/!, $msg, 2;
    $head .= "$/$/";
    if (!defined $body || $body eq '') {
	$body = "";
    }
    else {
	# Escape the body.
	mbox_escape_body_part_in_place $body;
	# Add a blank line to the body.
	$body .= $/;
    }
    return $head . $body;
}

=item B<daemonize> B<keep_fh_hash_ref>

Become a daemon.  If B<keep_fh_hash_ref> is specified, STDIN/STDOUT/STDERR
are kept instead of being attached to /dev/null if the corresponding entry
in the hash is true.

=cut

sub daemonize {
    @_ == 0 || @_ == 1 || badinvo;
    my ($rkeep_fh) = @_;

    $rkeep_fh ||= {};

    require POSIX;
    require Proc::SyncExec;
    chdir '/'			or xdie "can't chdir to /:";
    open STDIN, '/dev/null'	or xdie "can't read /dev/null:"
	unless $rkeep_fh->{STDIN};
    open STDOUT, '>/dev/null'	or xdie "can't write to /dev/null:"
	unless $rkeep_fh->{STDOUT};
    exit if Proc::SyncExec::fork_retry() != 0;
    POSIX::setsid()		or xdie "can't start a new session:";
    open STDERR, '>/dev/null'	or xdie "can't write to /dev/null:"
	unless $rkeep_fh->{STDERR};
}

=item B<url_decode> I<s>

Decode URL-encoded string I<s>.

=cut

sub url_decode {
    @_ == 1 or badinvo;
    my $s = shift;
    $s =~ s/%([a-f\d]{2})/chr hex $1/ieg;
    return $s;
}

=item B<url_encode_c> I<s>

Conservatively URL-encode I<s>.  The only characters which aren't encoded
are alphanumerics, underscore, period and dash.

=cut

sub url_encode_c {
    @_ == 1 or badinvo;
    my $s = shift;
    $s =~ s/([^\w.\-])/sprintf "%%%02X", ord $1/eg;
    return $s;
}

=item B<html_attr_encode> I<s>

Encode and qutoe appropriately an HTML attribute value.

=cut

sub html_attr_encode {
    @_ == 1 || badinvo;
    my ($val) = @_;

    return '' unless defined $val;

    $val = html_escape($val);

    if ($val =~ /^[a-zA-Z0-9\-._:]*\z/) {
    	return $val;
    }

    if ($val !~ /'/) {
    	return qq{'$val'};
    }

    if ($val !~ /"/) {
    	return qq{"$val"};
    }

    $val =~ s/'/&#39;/g;
    return qq{'$val'};
}

=item B<html_attrs> [ I<name> => I<value> ]...

Take a list of HTML name, value pairs and encode them, returning a
string to use as the attribute list.  Use an undef value for a boolean
attribute.

=cut

sub html_attrs {
    my (@pair) = @_;

    my @attr;
    while (@pair) {
    	my $name = shift @pair;
    	my $val = shift @pair;
    	push @attr, defined $val ? "$name=" . html_attr_encode $val : $name;
    }

    return join ' ', @attr;
}

=item B<html_escape> I<s>

Escape I<s> so that it can be included in HTML.

=cut

sub html_escape {
    @_ == 1 || badinvo;
    my ($s) = @_;

    $s =~ s/\x8b/&#139;/g;	# this must have screwd up Windows or something
    $s =~ s/\x9b/&#155;/g;	# this must have screwd up Windows or something
    $s =~ s/&/&amp;/g;
    $s =~ s/>/&gt;/g;
    $s =~ s/</&lt;/g;
    #$s =~ s/"/&quot;/g;
    #$s =~ s/'/&#39;/g;		# for HTML tag attributes values quoted with '
    return $s;
}

=item B<create_index_subs_pkg> I<pkg>, I<to-num-pfx>, I<to-name-pfx>, I<name>...

This function sets up constant subs which are used to index into arrays.
It fills some of the same niche that pseudo hashes do, but if you have
strict subs turned on you get more typo protection.  A list of sub names
(without package) created is returned.

It is easiest to describe with an example.  If you say

    create_index_subs_pkg __PACKAGE__, 'F', 'FNAME', qw(foo bar baz);

you get

    sub F_FOO		() { 0 }
    sub F_BAR		() { 1 }
    sub F_BAZ		() { 2 }

    sub FNAME_FOO	() { 'foo' }
    sub FNAME_BAR	() { 'bar' }
    sub FNAME_BAZ	() { 'baz' }

You can specify either the I<to-num-prefix> or the I<to-name-prefix>
as undef in order to skip those subs.  If either prefix is C<''> the
underscore will also be skipped when creating the corresponding sub names.

Most people will want to use create_index_subs() instead, which calls
this function with the package argument filled in.

=cut

sub create_index_subs_pkg {
    @_ >= 4 || badinvo;
    my ($pkg, $pfx_num, $pfx_name, @item) = @_;
    my (@created);

    for (\$pfx_num, \$pfx_name) {
	$$_ .= '_' if defined $$_ && $$_ ne '';
    }

    for my $ix (0..$#item) {
	next unless defined $item[$ix];
	for ([$pfx_num, $ix], [$pfx_name, $item[$ix]]) {
	    my ($pfx, $data) = @$_;

	    next unless defined $pfx;
	    my $base = "${pfx}\U$item[$ix]";
	    $base =~ tr/a-zA-Z0-9/_/c;
	    push @created, $base;
	    no strict 'refs';
	    *{ "${pkg}::$base" } = sub () { $data };
	}
    }

    return @created;
}

=item B<create_index_subs> I<to-num-prefix>, I<to-name-prefix>, I<name>...

This calls B<create_index_subs_pkg> but sets the package argument for
you.

=cut

sub create_index_subs {
    @_ >= 3 || badinvo;
    return create_index_subs_pkg scalar caller, @_;
}

=item B<create_constant_subs_pkg> I<pkg>, I<prefix>, { I<key> => I<value> }...

This function sets up constant subroutines.  It returns the names of
the subs (without package) created.  A sub is named with the I<prefix>,
and underscore, and upcased I<key>.  If the I<prefix> is C<''> the
underscore will also be skipped when creating the corresponding sub
name.  For example:

    create_constant_subs_pkg __PACKAGE__, 'PF', foo => 'F', bar => 'B';

will create

    sub PF_FOO () { 'F' }
    sub PF_BAR () { 'B' }

Most peole will want to use create_constant_subs() instead, which calls
this function with the package argument filled in.

=cut

sub create_constant_subs_pkg {
    @_ >= 3 || badinvo;
    my ($pkg, $pfx, @pair) = @_;
    @pair % 2 == 0 || badinvo 0, 'odd number of pairs specified';
    my (@created);

    $pfx .= '_' if $pfx ne '';

    while (@pair) {
    	my ($k, $v) = splice @pair, 0, 2;

    	my $base = "${pfx}\U$k";
	$base =~ tr/-/_/;
	push @created, $base;
	no strict 'refs';
    	*{ "${pkg}::$base" } = sub () { $v };
    }

    return @created;
}

=item B<create_constant_subs> I<prefix>, { I<key> => I<value> }...

This calls B<create_constant_subs_pkg> but sets the package argument for
you.

=cut

sub create_constant_subs {
    @_ >= 2 || badinvo;
    return create_constant_subs_pkg scalar caller, @_;
}

=item B<get_win_size>

Return the kernel's idea of the current TTY's terminal size.

=cut

sub get_win_size {
    require Term::ReadKey;
    my @size = Term::ReadKey::GetTerminalSize();
    $size[0] ||= 80;
    $size[1] ||= 24;
    return ($size[0], $size[1]);
}

=item B<even_elements> LIST

=item B<odd_elements> LIST

Return only the even or odd elements of LIST, numbering from 0.

=cut

sub even_elements {
    my $i;
    return grep { ++$i % 2 } @_;
}

sub odd_elements {
    my $i;
    return grep { $i++ % 2 } @_;
}

=item B<define> LIST

Map undefined elements to the empty string.

=cut

sub define {
    @_ or return;
    if (!wantarray) {
	@_ == 1 || badinvo 'multiple args in non-array context';
	return defined $_[0] ? $_[0] : '';
    }
    return map { defined $_ ? $_ : '' } @_;
}

=item B<command_with_stdin> I<s>, I<cmd>, I<arg>...

Run B<cmd> with I<args> and pipe I<s> to it on stdin.  Croaking if
something unusual happens, otherwise the exit value is in $?.

If the I<s> is sub ref it's called repeatedly to supply the input.

=cut

sub command_with_stdin {
    @_ >= 2 || badinvo;
    my ($s, @cmd) = @_;

    require Proc::SafePipe;
    my ($fh, $pid) = Proc::SafePipe::popen_noshell('w', @cmd);

    local $SIG{PIPE} = 'IGNORE';
    if (ref $s eq 'CODE') {
	while (defined(my $l = $s->())) {
	    print $fh $l or xdie "error writing to $cmd[0]:";
	}
    }
    else {
	print $fh $s or xdie "error writing to $cmd[0]:";
    }
    close $fh
	or $! == 0
	or xdie "error closing $cmd[0]:";
}

=item B<sendmail> I<message>, I<sendmail arg>...

Run B<sendmail> with I<message> on its input and the given args, croaking
if something unusual happens.  Its return is in $?.

If the I<message> is sub ref it's called repeatedly to supply the message.

=cut

sub sendmail {
    @_ >= 2 || badinvo;
    my ($msg, @arg) = @_;

    command_with_stdin $msg, '/usr/lib/sendmail', @arg;
}

=item B<ordinal> I<number>

Convert from a number to mixed numeric/English ordinal (1st, 2nd, etc.).

=cut

sub ordinal {
    @_ == 1 || badinvo;
    my ($n) = @_;

    # Drop leading zeroes, spaces, and such.
    $n += 0;

    if ($n =~ /1.$/)	{ return "${n}th" }
    elsif ($n =~ /1$/)	{ return "${n}st" }
    elsif ($n =~ /2$/)	{ return "${n}nd" }
    elsif ($n =~ /3$/)	{ return "${n}rd" }
    else		{ return "${n}th" }
}

=item B<wrap> I<initial tab>, I<subsequent tab>, I<text>...

This is like Text::Wrap::wrap() (which it uses) but it won't die, it
joins strings with C<$,> rather than C<' '>, and it sets the width based
on the terminal size.

=cut

{ my $columns;
sub wrap {
    @_ >= 3 || badinvo;
    my ($t1, $t2, @text) = @_;

    if (!defined $columns) {
	($columns, undef) = get_win_size;
	require Text::Wrap;
    }

    local $Text::Wrap::columns = $columns - 4;
    my $j = defined $, ? $, : '';
    my $wrapped = eval { Text::Wrap::wrap($t1, $t2, join $j, @text) };
    $wrapped = join $j, @text if $@; # XXX simple line breaks
    return $wrapped;
} }

=item B<max> I<num>...

Return the greatest I<num>.

=cut

sub max {
    @_ >= 1 || badinvo;
    my $n = shift;
    for (@_) {
	$n = $_ if $_ > $n;
    }
    return $n;
}

=item B<min> I<num>...

Return the least I<num>.

=cut

sub min {
    @_ >= 1 || badinvo;
    my $n = shift;
    for (@_) {
	$n = $_ if $_ < $n;
    }
    return $n;
}

=item B<max_not_undef> I<num or undef>...

Return the greatest arg, treating undef as smaller than any number.

=cut

sub max_not_undef {
    @_ >= 1 || badinvo;
    my @v = grep { defined } @_;
    return @v ? max @v : undef;
}

=item B<min_not_undef> I<num or undef>...

Return the least arg, treating undef as larger than any number.

=cut

sub min_not_undef {
    @_ >= 1 || badinvo;
    my @v = grep { defined } @_;
    return @v ? min @v : undef;
}

=item B<fuser> I<file>...

Return a list of PIDs which are using the given I<file>s.

=cut

sub fuser {
    @_ >=1 || badinvo;
    my @arg = @_;
    my ($cmd, @pid, %pid);

    if ($^O eq 'dgux') {
    	$cmd = 'fuser 2>/dev/null';
    }
    else {
    	xdie "fuser command for OS $^O unknown";
    }

    require String::ShellQuote;
    $cmd .= ' ' . String::ShellQuote::shell_quote(@arg);

    require Proc::WaitStat;
    my $output = `$cmd`;
    Proc::WaitStat::waitstat_die($?, $cmd);

    for (split ' ', $output) {
	if (/^\d+$/) {
	    push @pid, $_ unless $pid{$_}++;
	}
	else {
	    xdie "invalid output from fuser: `$_'";
	}
    }

    return @pid;
}

sub make_check_digit {
    @_ == 2 || badinvo;
    my ($num, $mod) = @_;

    defined $num && $num ne '' && $num !~ /\D/
	or xdie_caller 1, "inavlid number `$num'";
    $mod > 0 && $mod <= 10
	or xdie_caller 1, "invalid mod value `$mod'";

    # Get the $num into integer range before doing mod on it.  Any
    # number < 10 * 200,000,000 is under 2,000,000,000 which will be
    # sufficient to loop quickly and yet not go over the integer limit.

    my $sub = $mod * 200_000_000;
    $num -= $sub while $num >= 2147483646;

    return $num % $mod;
}

=item B<valid_check_digit> I<num> [I<mod>]

Return a boolean indicating whether the given I<num> has a valid
mod-I<mod> (default 7) check digit.

=cut

sub valid_check_digit {
    @_ == 1 || @_ == 2 || badinvo;
    my $num = shift;
    my $mod = @_ ? shift : 7;

    length($num) > 1 or return;

    # Don't use 4-arg substr, for 5.004 compatibility.
    my $existing = substr $num, -1, 1;
    substr($num, -1, 1) = '';

    return $existing == make_check_digit $num, $mod;
}

=item B<replace_ugids> I<uid>, I<gid>...

Replace both the real and effective user and group IDs of the current
process, or die trying.

=cut

sub replace_ugids {
    @_ >= 2 || badinvo;
    my ($uid, $gid, @extra_gid) = @_;
    my ($gid_list, %want_effective, %got_effective);

    $gid_list = join ' ', $gid, @extra_gid;
    $( = $gid;
    $) = "$gid $gid_list";
    if ($( != $gid || $) != $gid) {
	croak "Can't set real/effective gid to $gid (currently $(/$))";
    }
    for ($gid, @extra_gid) {
	$want_effective{$_} = 1;
    }
    for (split ' ', $)) {
	$got_effective{$_}  = 1;
    }
    if (grep(!$want_effective{$_}, split ' ', $))
    	    || grep(!$got_effective{$_}, @extra_gid)) {
	croak "Can't set effective supplementary group list to $gid_list",
    	    " (got $) instead)";
    }

    $< = $uid;
    $> = $uid;
    if ($> != $uid || $< != $uid) {
	croak "Can't set real/effective uid to $uid (currently $</$>)";
    }
}

=item B<cmp_strnum> I<a>, I<b>

This is like cmp but it tries to do the Right Thing for scalars which
contain both numbers and non-numbers, like software version numbers.

=item B<cmp_strnum_transform> I<s>

This is the helper function it uses to do that.  The result is a scalar
which can be B<cmp>ed directly against similar scalars.

=cut

{ my %cache;
sub cmp_strnum_transform {
    my $key = shift;

    return undef if !defined $key;

    my $val = $cache{$key};
    if (!defined $val) {
	($val = $key) =~ s/(\d+)/sprintf '%016d', $1/eg;
	$cache{$key} = $val;
    }

    return $val;
} }

sub cmp_strnum {
    @_ == 2 || badinvo;
    return cmp_strnum_transform(shift) cmp cmp_strnum_transform(shift);
}

=item B<have_prog> I<program>...

Return the first I<program> which is an executable file in the $PATH (or
which is an executable file given with absolute or relative path).

=cut

sub have_prog {
    @_ || badinvo;
    my @prog = @_;

    # Punt on a missing $PATH, as the behavior in that case depends on
    # your kernel.

    $ENV{PATH} = '/usr/bin:/bin' if !exists $ENV{PATH};

    my @path = map { $_ eq '' ? '.' : $_ } split /:/, $ENV{PATH}, -1;

    # An empty $PATH is interpreted as a single empty element and so
    # becomes '.'.

    @path = ('.') if !@path;

    for my $prog (@prog) {
    	if (index($prog, '/') >= 0) {
	    return $prog if -f $prog && -x _;
	}
	for my $dir (@path) {
	    return $prog if -f "$dir/$prog" && -x _;
	}
    }

    return;
}

=item rfc822_dt [I<time> [I<use_utc>]]

Return an RFC 2822-style date string (like C<Mon, 21 Jan 2002 02:33:56
-0500>) for the given time.  If the time isn't specified the current
local time is used.  If I<use_utc> is true the output is in UTC rather
than the local time zone.

=cut

{

my @wday = qw(Sun Mon Tue Wed Thu Fri Sat);
my @mon  = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);

sub rfc822_dt {
    @_ <= 2 || badinvo;
    my ($time, $use_utc) = @_;
    $time ||= time;

    my (@tm, $offset);

    if ($use_utc) {
	@tm = gmtime $time;
	$offset = 0;
    }
    else {
	@tm = localtime $time;
	my @gm = gmtime $time;
	$offset = 3600 * ($gm[TM_HOUR] - $tm[TM_HOUR])
    	    	  + 60 * ($gm[TM_MIN ] - $tm[TM_MIN ])
    	    	  +      ($gm[TM_SEC ] - $tm[TM_SEC ])
		  + 24 * 60 * 60 * (
			$gm[TM_YEAR] < $tm[TM_YEAR]	? -1
			: $gm[TM_YEAR] > $tm[TM_YEAR]	? +1
			: $gm[TM_YDAY] < $tm[TM_YDAY]	? -1
			: $gm[TM_YDAY] > $tm[TM_YDAY]	? +1
			: 0
    	    	    );
    }

    my $sign = '-';
    if ($offset <= 0) {
	$sign = '+';
	$offset *= -1;
    }
    my $offset_hour	= int($offset / 3600);	$offset -= $offset_hour * 3600;
    my $offset_min	= int($offset / 60);	$offset -= $offset_min  * 60;

    # Mon, 21 Jan 2002 02:33:56 -0500

    return sprintf "%s, %2d %s %d %02d:%02d:%02d %s%02d%02d",
	$wday[$tm[TM_WDAY]],
    	$tm[TM_MDAY],
	$mon[$tm[TM_MON]],
	$tm[TM_YEAR] + 1900,
	$tm[TM_HOUR],
	$tm[TM_MIN],
	$tm[TM_SEC],
	$sign, $offset_hour, $offset_min;
} }

=item iso_date [I<time> [I<use_utc>]]

Return the date in ISO 8601 format.

=cut

sub iso_date {
    @_ <= 2 || badinvo;
    my ($time, $use_utc) = @_;
    $time ||= time;

    my @tm = $use_utc ? gmtime $time : localtime $time;
    return sprintf "%4d-%02d-%02d",
    	$tm[TM_YEAR] + 1900,
	$tm[TM_MON] + 1,
	$tm[TM_MDAY];
}

=item iso_dt [I<time> [I<use_utc>]]

Return the date/time in ISO 8601 format.

=cut

sub iso_dt {
    @_ <= 2 || badinvo;
    my ($time, $use_utc) = @_;
    $time ||= time;

    my @tm = $use_utc ? gmtime $time : localtime $time;
    return sprintf "%4d-%02d-%02d %02d:%02d:%02d",
    	$tm[TM_YEAR] + 1900,
	$tm[TM_MON] + 1,
	@tm[TM_MDAY, TM_HOUR, TM_MIN, TM_SEC];
}

=item interval_to_secs I<interval>

Convert a string to a number of seconds.  An interval is currently a
decimal number followed by a suffix.  Suffixes and examples:

    s = second		-30s = -30 seconds
    m = minute		  5m = 5 minutes
    h = hour		1.5h = 1.5 hours
    d = day		  1d = 1 day

If you give me invalid input I'll croak.

=cut

{

my %mult = (
    's' => 1,
    'm' => 60,
    'h' => 60 * 60,
    'd' => 60 * 60 * 24,
);

sub interval_to_secs {
    @_ == 1 || badinvo;
    my ($orig) = @_;

    local $_ = $orig;

    defined or croak "Undefined interval";

    s/^([+\-]?\d+\.?\d*)//
	or croak "No number at start of interval `$orig'";
    my $n = $1;

    my $m = $mult{$_};
    defined $m
	or croak "Invalid multiplier in interval `$orig'";

    return $n * $m;
} }

=item secs_to_dhms I<seconds>

Convert a number of seconds into a number of days, hours, minutes, and
seconds.

    secs_to_dhms 90061
    	-> (1, 1, 1, 1)

=cut

sub secs_to_dhms {
    @_ == 1 || badinvo;
    my ($s) = @_;
    my (@r);

    $s >= 0
	or croak "Invalid number of seconds ", dstr $s;

    for my $duration (60*60*24, 60*60, 60) {
	my $this = int($s / $duration);
	push @r, $this;
	$s -= $this * $duration;
    }
    push @r, $s;

    return @r;
}

=item secs_to_dhms_str I<seconds>

Convert a number of seconds into a human-readable representation of the
number of days, hours, minutes, and seconds.

    secs_to_dhms 90061
    	-> '1d+1:01:01'

=cut

sub secs_to_dhms_str {
    my ($d, $m, $h, $sf) = secs_to_dhms @_;

    # XXX still subject to rounding problems
    my ($s, $frac) = split /\./, $sf, 2;

    my $r = sprintf "%d:%02d:%02s", $m, $h, $s;
    if (defined $frac) {
	$r .= ".$frac";
    }

    if ($d) {
	$r = "${d}d+$r";
    }
    return $r;
}

=item mtime [I<file>]

Return the mtime of I<file>, or undef with $! set.  If I<file> is
ommitted, returns the mtime from the current saved stat buffer C<_>.

=cut

sub mtime {
    @_ == 0 || @_ == 1 || badinvo;
    my @s = @_ ? stat shift : stat _;
    return @s ? $s[ST_MTIME] : undef;
}

=item flush I<fh>

Flush the output on the given filehandle, or return an error.  This does not
turn on autoflush for the filehandle.

=cut

sub flush {
    @_ == 1 || badinvo;
    require Symbol;
    my $fh = Symbol::qualify_to_ref(shift);

    my $old_fh = select $fh;
    my $err = do {
	local ($|, $\) = (1, '');
	print $fh ''
    };
    select $old_fh;
    return $err;
}

=item commify [I<num>]...

Add commas to a number to each given number.  Eg, C<23456> is returned
as C<23,456>.

=cut

sub commify {
    my @in = @_
	or return;
    my @out;
    for (@in) {
	1 while s/^([-+]?\d+)(\d{3})/$1,$2/;
	push @out, $_;
    }
    return wantarray ? @out : shift @out;
}

=item mime_token_quote I<s>

Quote I<s> as a MIME token and return the result.

=cut

sub mime_token_quote {
    @_ == 1 || badinvo;
    my ($s) = @_;
    # XXX Characters \200-\377 aren't actually valid.
    if ($s =~ /[\040\000-\037\177\200-\377()<>@,;:\\\"\/\[\]?=]/) {
	$s =~ s/([\"\015\\])/\\$1/g;
	$s = qq["$s"];
    }
    return $s;
}

=item unique I<s>...

Return the unique args.

=cut

sub unique {
    my %seen;
    return grep { !$seen{$_}++ } @_;
}

=item cache_url I<url>, I<secs>

Fetch and return the data from I<url>, and cache the result.  Use the
cached value unless it was cached more than I<secs> seconds ago.

=cut

{

# XXX store the modification time, use that to do an If-Modified-Since
# query when refreshing
#
# XXX support refreshing per Expires:

my %cache;	# [time fetched, content]
my $ua;
my $debug = 0;

sub cache_url {
    @_ == 2 || badinvo;
    my ($url, $refresh) = @_;

    if (!$ua) {
	require HTTP::Date;
	require HTTP::Request;
	require LWP::UserAgent;

	if ($debug) {
	    require LWP::Debug;
	    LWP::Debug->import('+');
	}

	$ua = LWP::UserAgent->new;
	$ua->agent("$Me/cache_url/$VERSION");
	$ua->env_proxy;
    }

    my $cache = $cache{$url};
    if ($cache && time - $cache->[0] < $refresh) {
	warn subname, ": using cached $url\n" if $debug;
	return $cache->[1];
    }

    warn subname, ": fetching     $url\n" if $debug;
    my $req = HTTP::Request->new(GET => $url);
    $req->header('Cache-Control' => 'no-cache');
    my $resp = $ua->request($req);
    if (!$resp->is_success) {
    	xwarn "error fetching $url: ", $resp->status_line;
	return;
    }

    $cache{$url} = [time, $resp->content];
    return $resp->content;
} }

=item set_autoflush I<fh> [I<value>]

Set $| for the given I<fh> to I<value> (default on).  The previous $|
for the filehandle is returned.

=cut

sub set_autoflush {
    @_ == 1 || @_ == 2 || badinvo;
    require Symbol;
    my $fh = Symbol::qualify_to_ref(shift);
    my $val = @_ ? shift : 1;

    my $old_fh = select $fh;
    my $old_val = $|;
    $| = $val;
    select $old_fh;
    return $old_val;
}

=item remove_at_exit I<path>...

Arrange for each I<path> to be removed when the program exits (even if
it's killled by a (normal, per L<sigtrap>) signal).  If any can't be
removed an error will be printed and the program will exit with a
non-zero status.

This doesn't support removing directories yet, because File::Path is
unsafe and doesn't properly report errors.

=cut

{

my $pid;
my @ent;

sub remove_at_exit {
    @_ || badinvo;
    if (!@ent) {
	$pid = $$;
	require 'sigtrap.pm';
	'sigtrap'->import(qw(die untrapped normal-signals));
    }
    push @ent, @_;
}

END {
    return unless defined $pid && $$ == $pid;
    for (@ent) {
	#print "$$ remove $_\n";
    	next if unlink;
	my $err = $!;
	#print "$$ didn't remove: $!\n";

	next unless lstat;
	xwarn "error unlinking $_: $err\n";
	$? = 1 unless $?;
    }
}

}

=item struct_linger_pack I<linger_onoff>, I<linger_time>

Pack a struct linger as used by a SO_LINGER setsockopt().

=cut

{

my $template = 'II';

sub struct_linger_pack {
    @_ == 2 || badinvo;
    return pack $template, @_;
}

=item (I<linger_onoff>, I<linger_time>) = struct_linger_unpack I<packed>

Unpack a struct linger as used by a SO_LINGER setsockopt().

=cut

sub struct_linger_unpack {
    @_ == 1 || badinvo;
    return unpack $template, shift;
}

}

=item B<equal> I<thing_1>, I<thing_2>

This predicate is true if I<thing_1> and I<thing_2> have similar structure
and contents.

XXX The current implementation fails on code references, treating them
all as equal.

=cut

sub equal {
    @_ == 2 || badinvo;
    my @t = @_;

    require YAML;
    # I'd like to use YAML's DumpCode method to fix code serializing
    # (just using the stringified reference would do just what I want),
    # but it doesn't work.
    my @y = map { YAML::Dump($_) } @t;
    return $y[0] eq $y[1];
}

=item B<dstr> I<s>

Return I<s> suitably formatted for unambiguous, warning-free display,
for use in debugging.   This is prototyped to take a single scalar arg.

=cut

sub dstr ($) {
    local $_ = shift;
    return 'undef' if !defined;
    return '[]' if $_ eq '';
    return $_ if !/[\[\]%\x00-\x20\x7f-\xff]/ && $_ ne 'undef';
    return "[$_]" if !/[\[\]\x00-\x1f\x7f-\xff]/;
    return url_encode_c $_;
}

=item expand_field_list_specs I<num-fields>, I<spec>...

Given a field count and a a string like B<cut>'s C<-f> switch (except
negative numbers count from the end), return a corresponding list of
fields.  Eg,

    expand_field_list_specs 12, '5,8 - -3', '2-4', '10-''
    	=> (5, 8, 9, 10, 2, 3, 4, 10, 11, 12)

A too-negative number for the current I<num-fields> is treated as 1.  An
invalid I<spec> causes a die().

=cut

sub expand_field_list_specs {
    @_ >= 2 || badinvo;
    my ($field_count, @spec) = @_;
    my @r;

    for my $spec (@spec) {
    	my $orig = $spec;
	$spec =~ s/\s+//g;
    	for my $sub (split /,/, $spec) {
	    $sub =~ /^(-?\d+)(-(-?\d+)?)?\z/
	    	or xdie subname, ": invalid spec `$orig'";
	    my $lo = $1;
	    my $hi = defined $3 ? $3			# a-b
			: defined $2 ? $field_count	# a-
			: $lo;				# a
	    for ($lo, $hi) {
	    	if ($_ == 0) {
		    xdie subname, ": 0 value in spec `$orig'";
		}
		elsif ($_ < 0) {
		    $_ = $field_count + 1 + $_;
		    $_ = 1 if $_ < 1;
		}
	    }
	    for ($lo..$hi) {
	    	push @r, $_ if $_ <= $field_count;
	    }
	}
    }

    return @r;
}

=item non_commments [I<comment-re>,] [I<s>]...

Return all the I<s> args which aren't empty, blank, or comments.  By
default a comment is a line which matches C</^\s*#/>.  The I<comment-re>
arg must be a Regexp object (as with C<qr//>) so I can tell it's there.

=cut

sub non_comments {
    return unless @_;
    my $comment_re = (ref $_[0] && do {
			    require UNIVERSAL;
			    UNIVERSAL::isa($_[0], 'Regexp') })
			? shift
			: qr/^\s*#/;
    return grep { defined $_ && /\S/ && !/$comment_re/ } @_;
}

=item discard_zombies

Get rid of current zombies (processes which have exited but haven't
been reaped).

=cut

sub discard_zombies {
    @_ == 0 || badinvo;

    require POSIX;
    1 while waitpid(-1, POSIX::WNOHANG()) > 0;
    return;
}

=item list_length [I<arg>]...

Return the number of arguments.  This is useful to count how many things
another function returns.

=cut

sub list_length {
    return scalar @_;
}

=item decompressing_open_command I<file>

If I<file> is a compressed file, return a command which will decompress
it.  Otherwise return it as-is.

=cut

sub decompressing_open_command {
    @_ == 1 || badinvo;
    my ($path) = @_;
    my (@cmd);

    if ($path =~ /\.(gz|Z|z)\z/) {
    	@cmd = qw(gzip --decompress --stdout);
    }
    elsif ($path =~ /\.bz2?\z/) {
	@cmd = qw(bzip2 --decompress --stdout);
    }
    else {
	return $path;
    }

    # XXX make function out of path-search, die if the decompress command
    # is missing?

    require String::ShellQuote;
    return String::ShellQuote::shell_quote(@cmd, $path) . " |";
}

=item eq_undef I<a>, I<b>

This is like eq but it won't warn if either arg is undef, and undef
is only equal to another undef.

=cut

sub eq_undef {
    my ($a, $b) = @_;

    if (!defined $a) {
	if (!defined $b) {
	    return 1;
	}
	else {
	    return 0;
	}
    }
    elsif (!defined $b) {
	return 0;
    }
    else {
	return $a eq $b;
    }
}

=item inverse_hash [I<key> => I<value>]...

Reverse and return the key/value pairs.  If there's a duplicate value,
croak.  The returned values will be in the same order you gave them to
me.

=cut

sub inverse_hash {
    my (%r, @r);

    while (@_) {
    	my ($k, $v) = splice @_, 0, 2;
	if (exists $r{$v}) {
	    xcroak "hash contains duplicate value ", dstr $v,
		" for keys ", dstr $r{$v}, " and ", dstr $k;
	}
	$r{$v} = $k;
	push @r, $v, $k;
    }

    return @r;
}

=item G_PER_LB

=item KG_PER_LB

=item LB_PER_KG

=item lb_to_kg I<val>

=item lb_to_g I<val>

=item kg_to_lb I<val>

=item g_to_lb I<val>

These are simple weight conversion functions.  Nothing special is done
about the number of returned significant digits.

=cut

sub G_PER_LB  () { 453.59237       } # by definition
sub KG_PER_LB () { 0.45359237      }
sub LB_PER_KG () { 1000 / G_PER_LB }
sub lb_to_kg ($) { @_ == 1 || badinvo; return $_[0] / LB_PER_KG        }
sub lb_to_g  ($) { @_ == 1 || badinvo; return $_[0] / LB_PER_KG * 1000 }
sub kg_to_lb ($) { @_ == 1 || badinvo; return $_[0] * LB_PER_KG }
sub  g_to_lb ($) { @_ == 1 || badinvo; return $_[0] * LB_PER_KG / 1000 }

=item longest_common_prefix I<string>...

XXX

=cut

sub longest_common_prefix {
    @_ >= 1 || badinvo;

    my $prefix = shift;
    for (@_) {
	chop $prefix while substr($_, 0, length $prefix) ne $prefix;
    }
    return $prefix;
}

sub longest_common_parent_directory {
    my $p = longest_common_prefix @_;

    if (!defined $p || $p eq '') {
	return undef;
    }

    $p =~ s|/[^/]*\z||
	if $p ne '/';
    $p = '/' if $p eq '';
    return $p
}

sub create_lock_file {
    @_ || badinvo;
    my ($path, @opt) = @_;

    process_arg_pairs \@opt, (
    	debug		=> \(my $debug         = 0),
    	die_sub		=> \(my $die_sub       = \&xdie),
	retry_sleep	=> \(my $retry_sleep   = 1),
	retry_timeout	=> \(my $retry_timeout = 0),
    	warn_age	=> \(my $warn_age      = undef),
    	warn_sub	=> \(my $warn_sub      = \&xwarn),
    );

    my $start = time;
    my $retries_done = 0;
    while (1) {
	my $fh = exclusive_create $path;
	if ($fh) {
	    set_autoflush $fh;
	    print $fh "$$\n"
		or $die_sub->("error writing to $path:");
	    remove_at_exit $path;
	    return $fh;
	}

	{
	    local $!;
	    require POSIX;
	}
	$! == POSIX::EEXIST()
	    or $die_sub->("can't create $path:");

    	if ($retry_timeout && time - $start < $retry_timeout) {
	    print "sleep $retry_sleep\n"
		if $debug;
	    sleep $retry_sleep;
	    next;
	}

	last;
    }

    if (!defined $warn_age) {
	return undef;
    }

    my @st = stat $path
	or return undef;
    my $age = time - $st[ST_MTIME];
    if ($age > $warn_age) {
	$warn_sub->(
	    "lock file $path is old ($age second", plural($age), ", ",
	    POSIX::strftime("%Y-%m-%d %H:%M:%S", localtime $st[ST_MTIME]),
	    ")\n"
	);
    }

    return undef;
}

# XXX combine with prompt()?
# XXX optionally line-wrap prompt

{ my ($interactive, $use_readline, $rl);
sub prompt_readline {
    @_ == 1 || badinvo;
    my ($prompt) = @_;
    my ($s);

    if (!defined $interactive) {
	$interactive = -t STDIN;
    }

    if (!defined $use_readline) {
    	$use_readline = $interactive;
	if ($use_readline) {
	    require Term::ReadLine;
	    $rl = Term::ReadLine->new($Me)
		or xdie "can't initialize Term::ReadLine\n";
	    $rl->ornaments(0);
	}
    }

    if ($use_readline) {
	$s = $rl->readline($prompt);
    }
    else {
    	if ($interactive) {
	    print $prompt;
	    flush STDOUT;
	}
	$s = <STDIN>;
	chomp $s if defined $s;
    }

    if (!defined $s && $interactive) {
	print "(eof)\n";
    }

    return $s;
} }

sub center_string {
    @_ == 2 || @_ == 3 || badinvo;
    my ($field_size, $s, $truncate) = @_;

    my $l = length $s;
    $l <= $field_size
	or $truncate
	or badinvo 0, "string too large ($l >$ field_size, ", dstr $s, ")";

    return sprintf "%-${field_size}.${field_size}s",
	" " x (($field_size - $l) / 2) . $s;
}

#=item B<ps>
#
#This is a simple interface to B<ps>.  Right now you can't specify the
#selection criteria, I don't know what minimal subset I want to support.
#You get back an array of arrays.  The sub arrays give info for one
#process, use the PS_* subs to indext into them.
#
#    user name	PS_USER
#    pid		PS_PID
#    pppid	PS_PPID
#    tty		PS_TTY
#    cmd		PS_CMD		argv, not accounting name
#
#Import the :ps tag to get both ps() and the PS_* contstants.
#
#=cut
#
#sub ps {
#    @_ == 0 || badinvo;
#    my (@r);
#    local *PS;
#
#    if ($^O ne 'dgux') {
#	xdie "ps() unimplemented for this OS ($^O)";
#    }
#
#    open PS, 'ps -eo uname=,pid=,ppid=,tty=,args= |'
#    	or xdie "can't fork:";
#    while (<PS>) {
#	# DG's ps outputs arg lists, and even command names, containing
#	# newlines.  There's no really safe way to deal with this yet
#	# still see the commands.  I do a best effort at handling these,
#	# but a malicious person can trick me.  XXX This stuff is used
#	# by root tasks and that's probably enough reason not to do this
#	# at all, but
#    	if (!/^(\S+) \s+ (\d+) \s+ (\d+) \s+ (\S+) \s+ (.*)/x) {
#
#	or next;
#	    or xdie "unparsable outpuk

1;

__END__

=back

=head1 AUTHOR

Roderick Schertler <F<roderick@argon.org>>

=cut
