#!/usr/bin/perl

#use strict;
#use warnings;

# declarations
sub declarations;
sub check_defined;
sub myname;		# a list operator
sub declarations;
sub comments;
sub simple_statements;
sub truth_and_falsehood;
sub statement_modifiers;

&declarations();



&truth_and_falsehood();

&statement_modifiers();

# definitions
sub declarations {
	$a;			# $a is declared but undefined
	check_defined($a);
	use warnings;
	$a = "*" . $a . "*";	# warning: uninitialized value as a string ("")
	no warnings;
	check_defined($a);
	undef $a;		# now $a is undefined agin
	use warnings;
	$a = $a + 1;		# warning: uninitialized value as a number (0)
	no warnings;
	check_defined($a);

	# TODO: when used as a reference that isn't being assigned to, it is treated as an error

	my $me;
	$me = myname $0 or die "can't get myname";
}
sub check_defined {
	my ($p) = @_;
	if ($p) {	# if $p is defined
		print "The scalar variable is defined and the value is $p\n"
	} else {
		print "The scalar variable is undefined\n"
		# the scalar variable remains undefined
	}
}

sub myname {
	return "Fabio"
}

sub truth_and_falsehood() {
  print "The numer 0 is false in a boolean context\n" unless 0; 
  print "The string \"\" is false in a boolean context\n" unless "";
  print "The empty list () is false in a boolean context\n" unless ();
  my $foo = "defined";
  unless(undef($foo) || undef) {
    print "The undef function is false in a boolean context\n";
  }
  my $false_value = (1 == 0);
  print("False value evaluated as a string:", $false_value, "\n");
  print("False value evaluated as a number:", ($false_value + 0) , "\n");
}

sub go_outside() {
  print "Going outside\n";
}

sub play() {
  print "Playing\n";
}

sub statement_modifiers() {

  # if EXPR
  my $ear = "very long ears";
  print "Basset hounds got long ears\n" if length $ear >= 10;

  # unless EXPR
  my $is_raining = 0;
  go_outside() and play() unless $is_raining;

  # for EXPR
  # $_ is aliased to each item in turn
  print "Hello $_!\n" for qw(world Dolly nurse);

  # while EXPR
  my $i = 0;
  print $i++ while $i <= 10; print "\n";

  # until EXPR
  my $j = 0;
  print $j++ until $j > 10; print "\n";

  # do BLOCK until EXPR
  my $line;
  print "Type . <ENTER> to continue...\n";
  do {
    $line = <STDIN>;
  } until !defined($line) || $line eq ".\n";

  # do { LOOP-BLOCK-WITH-NEXT } until EXPR
  my $x = 0; my $y = 1; my $z = 2;
  do 
  {  # do-BLOCK
    {   # LOOP_BLOCK
      next if $x == $y;
      print "$x $y $z next not executed\n";
    } 
  } until $x++ > $z;

  $x = 0; $y = 2; $z = 6;
  # do { LOOP-BLOCK-WITH-LAST } until EXPR
  LOOP:
  {   # LOOP BLOCK
    do 
    {   #do-BLOCK
      last if $x == $y**2;
      print "$x $y $z last not executed\n";
    } while $x++ <= $z;
  }

}
