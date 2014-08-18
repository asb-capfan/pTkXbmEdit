#!perl

use strict;
use warnings;
use File::Spec;
use FindBin qw/$Bin/;
use lib File::Spec->catdir($Bin, '..', 'lib');
use My::XBMEditor;

my $app = My::XBMEditor->new;
$app->run;
exit(0);