package My::XBMEditor;

use 5.008008;
use strict;
use warnings;
use Moose;
use Moose::Util::TypeConstraints;
use curry;
use Tk 804;
use Tk::Menu;
use Tk::Text;
use Tk::Image;
use Tk::Dialog;
use Tk::StatusBar;
use Tk::ToolBar;
use Image::Xbm;
use Data::Dumper qw/Dumper/;

our $VERSION = '3.0';
$VERSION = eval $VERSION;

# use coerce to glue Moose's attributes on Tk's textvariables
subtype 'TkRef' => as 'ScalarRef';
coerce 'TkRef', from 'Str', via { my $r = $_; return \$r };

=head1 NAME

pTkXbmEdit - an XBM editor written in Perl/Tk

=head1 SYNOPSIS

  use My::XBMEditor;
  my $app = My::XBMEditor->new;
  $app->run;

=head1 DESCRIPTION

Source: http://www.perlmonks.org/?node_id=163230
    downloaded 2017-04-27 11:04

=head1 ATTRIBUTES

=head2 mw

The Tk::MainWindow object for the GUI.

=cut

has 'mw' => (is => 'ro', isa => 'Tk::MainWindow', default => sub{
    my $mw = Tk::MainWindow->new(
        -background  => 'ghostwhite',
        -borderwidth => 1,
        -relief      => 'groove',
        -width       => 500,
        -height      => 500,
        -title       => 'pTkXbmEdit - a Xbm Editor written in Perl/Tk by crazyinsomniac',
    );
    return $mw;
});

=head2 canvas

This is the canvas where the grid is drawn.

=cut

has 'canvas' => (is => 'rw', isa => 'Tk::Canvas');

has 'about_dialog' => (is => 'ro', isa => 'Tk::Dialog', lazy => 1, default => sub{
    my $self = shift;
    my $mw = $self->mw;
    my $d = $mw->Dialog(
        -title => 'About pTkXbmEdit',
        -text => "This is pTkXbmEdit v$VERSION",
        -bitmap => 'info',
        -buttons => ['Dismiss'],
    );
    return $d;
});


=head2 status_sticky

Private attribute. Will hold some text that is sticky in the status bar.
Set to empty string if you want to clear the sticky text.

=cut

has 'status_sticky' => (is => 'rw', isa => 'Str', default => '');


=head2 status

Holds a reference to the text displayed in the status bar.

Note: dereference to set a new value, e.g. C<${$self->status} = 'new value';>

=cut

has 'status' => (is => 'rw', isa => 'TkRef', coerce => 1, default => '');


=head2 grid_width_x

Holds a reference to the width of the grid.

=cut

has 'grid_width_x' => (is => 'rw', isa => 'TkRef', coerce => 1, default => '');


=head2 grid_height_y

Holds a reference to the height of the grid.

=cut

has 'grid_height_y' => (is => 'rw', isa => 'TkRef', coerce => 1, default => '');


=head2 grid

=cut

has 'grid' => (is => 'rw', isa => 'ArrayRef[ArrayRef[Int]]', default => sub{ [[]] });


=head2 crazy0naCAMEL

Not sure about this. It's used when the XBM grisd is drawn for the very first time.

=cut

has 'crazy0naCAMEL' => (is => 'ro', isa => 'Str', default => "---------#-----------#--------
---------##------####---------
-----######-----#---#---------
---##---###-----##--#---------
--##########----#--#----------
---##--#####-----##-----------
---#---####------#-##---------
---#---####---####-#----------
---#--####---#-----#----------
---#--####----##-##-----------
---#-#########--######--------
---#-####---##--#######-------
---#-#######-#-#########------
---#--####################----
----#-#####################---
-----#######################--
-------#####################--
--------###################---
--------######------#######---
---------####-------######----
---------###---------#####----
---------###---------###-#----
---------###---------###-#----
---------###----------##-#----
---------###----------####----
--------##-----------###------
",);


=head2 crazy0naCAMEL

Not sure about this. It's used when clearing the canvas.

=cut

has 'MirrorCamel' => (is => 'ro', isa => 'Str', default => "----####-----##-----------------
---######---###-----------------
---######--##-##-##-------------
---######--#---#####------------
---######----####--#-------##---
----###-----##-##----######-###-
-------------#-#-#--##--------##
-#####---------#---#------------
##----#####----#-##-------------
----------#######---------------
-#-------------#----------------
-#---#--#-----------#-----##----
###-#-#-#----------###---####---
#--------#-----#--#############-
-#--------##------------######--
--#------#---------------#---#--
###-----#----------------#---#--
-#------#-----------------------
--------------------------------
--------------------------------
--------#---#-###--##-##----###-
--------#####-#--#-#-#-#---##-#-
###-------#---###--#-#-#--##--#-
-#-#-#--#####-#--#-#---#---#--#-
-#-##---#---#-#-#--#---#---#-##-
-#-#-#--#---#-##---#---#---#-#--
---------------------------##---
----------###-----#-#--#----#---
---------##-#--####---###-------
--------##-#---#-##-#--#----###-
---------###-#-####-#--#---##-#-
----------####-##-#-#--##---###-
",);


=head1 METHODS

=head2 run()

Initializes ad starts the application (GUI event loop).

=cut

sub run {
    my $self = shift;
    
    $self->set_us_up_the_gui();
    $self->set_us_up_the_grid();

    $self->mw->MainLoop;

    return;
} # /run




=head2 duck( $canvas )

Canvas click handler.

=cut

sub duck {
    my $self = shift;
    my $widget = shift or die('Missing canvas');
    
    my $Ev = $widget->XEvent;
    my $s = $Ev->s;

    if ($s =~ m{^B1}ix) {    # button1 down (left button)
        $self->flip_square($widget, 0);
    } elsif ($s =~ m{^B3}ix) {    # button3 down (right button)
        $self->flip_square($widget, 1);
    }
    
    return;
} # /duck




=head2 set_us_up_the_menu_bar()

Defines the menu bar and applies toolar button bindings.

=cut

sub set_us_up_the_menu_bar {
    my $self = shift;
    my $mw = $self->mw;
    
    my $menuitems = [
        [Cascade => "~File", -tearoff => 0, -menuitems =>
            [
                [Button => "L~oad", -accelerator => 'Ctrl+o', -command => $self->curry::f_open,],
                [Button => "Save ~As", -accelerator => 'Ctrl+a', -command => $self->curry::f_saveas,],
                [Separator => ""],
                [Button => "E~xit", -command => $self->curry::exit_program(),],
            ],
        ],
        [Cascade => "~Help", -tearoff => 0, -menuitems =>
            [
                [Button => "~About pTkXbmEdit", -command => sub{ $self->about_dialog->Show; },],
            ],
        ],
    ];
    
    my $menu = $mw->Menu(-menuitems => $menuitems);
    $mw->configure(-menu => $menu);

    # -- add bindings for global hotkeys as the Tk::Menu accellerators don't do anything.
    $mw->bind($mw, "<Control-o>" => $self->curry::f_open);
    $mw->bind($mw, "<Control-a>" => $self->curry::f_saveas);

    return;
} # /set_us_up_the_menu_bar




=head2 exit_program()

Quits the XBM editor.

=cut

sub exit_program {
    my $self = shift;
    my $mw = $self->mw;
    
    # TODO: add one of those annoying "do you really want to exit" dialogs
    # with a picture of a sad looking cat
    
    $mw->destroy;
    exit(0);
} # /exit_program




=head2 f_open()

Asks for an XBM image file.
If provided, draws it on the canvas.

=cut

sub f_open {
    my $self = shift;
    my $mw = $self->mw;
    
    my @ext = (
        ["X BitMap",	[qw/.xbm/]],
        ["All files",	[qw/*/]],
    );
    
    my $file = $mw->getOpenFile(
        -filetypes => \@ext,
        -defaultextension => 'xbm',
        -initialdir       => '.',
        -title            => "Open File:",
    );
    
    if ($file) {
        $self->Statuss("loading $file ...\n", "[$file]");
        my $xbm_image = undef;
        eval{
            $xbm_image = Image::Xbm->new(-file => $file);
        };
        if ( $@ ) {
            my $d = $mw->Dialog(
                -title => 'Invalid XBM File!',
                -text => 'Error reading file: ' . $@,
                -bitmap => 'error',
                -buttons => ['Ok'],
                -default_button => 'Ok',
            );
            $d->Show;
            $self->Statuss("Invalid XBM file", "[$file]");
        }else{
            $self->draw_us_up_a_picture($xbm_image->as_string());
            $self->Statuss("", "[$file]");
        }
        
    } else {
        $self->Statuss("no file to load\n");
    }

    return;
} # /f_open




=head2 Statuss( $message, $sticky )

Displays a message in the program's status bar.

Provide empty message to clear the status bar.

C<$sticky> is an additional text in front of the current status bar
message.

=cut

sub Statuss {
    my $self = shift;
    my ($msg, $sticky) = @_;
    
    $self->status_sticky($sticky) if defined $sticky; # "" will be used to clear
    
    ${$self->status} = $self->status_sticky . ' ' . $msg;
    return;
} # /Statuss




=head2 f_saveas()

Shows a dialog to save the file. Saves the canvas content to the file
if one is provided by the dialog. Aborting the dialog does not save the file.

=cut

sub f_saveas {
    my $self = shift;
    my $mw = $self->mw;
    
    my @ext = (
        ['XBM Image Files',	[qw/.xbm/]],
        ['All files',	        [qw/*/]],
    );
    
    my $file = $mw->getSaveFile(
        -defaultextension => ".xbm",
        -initialdir       => "./*.xbm",
        -title            => "Save File:",
        -filetypes        => \@ext,
    );
    
    if ($file and length $file > 0) {
        my $picXBM = $self->make_us_up_the_picture();
        if ( !$picXBM ) {
            $self->Statuss("no picture, nothing to save\n");
            return;
        }
        eval{
            $picXBM->save($file)
        };
        if ( $@ ) {
            my $d = $mw->Dialog(
                -title => 'Error Saving File!',
                -text => "Error saving file: $@",
                -bitmap => 'error',
                -buttons => ['Ok'],
                -default_button => 'Ok',
            );
            $d->Show;
            $self->Statuss("Error saving $file\n");
        }else{
            $self->Statuss("saved $file, looks great\n");
        }
        
    } else {
        $self->Statuss("no filename, can't save shit\n");
    }
    return;
} # /f_saveas




=head2 set_us_up_the_gui()

Builds / defines the GUI.

=cut

sub set_us_up_the_gui {
    my $self = shift;
    my $mw = $self->mw;
    
    $self->set_us_up_the_menu_bar;
    
    my $tb = $mw->ToolBar(
        -movable => 0,
        -side => 'top',
    );
    
    $tb->ToolButton (
        -text => 'Flip All Squares',
        -command => sub { $self->flip_em_all(); },
    );
    
    $tb->ToolButton (
        -text => 'White All Squares',
        -command => sub { $self->wipe_em_all('white'); },
    );
    
    $tb->ToolButton (
        -text => 'Black All Squares',
        -command => sub { $self->wipe_em_all('black'); },
    );
    
    $tb->separator;
    
    $tb->ToolLabEntry(
        -label => 'X:',
        -labelPack => [-side => "left", -anchor => "w"],
        -bg => 'white',
        -textvariable => $self->grid_width_x,
    );
    
    $tb->ToolLabEntry(
        -label => 'Y:',
        -labelPack => [-side => "left", -anchor => "w"],
        -bg => 'white',
        -textvariable => $self->grid_height_y,
    );
    
    $tb->separator;
    
    $tb->ToolButton (
        -text => 'Make the grid',
        -command => sub { $self->set_us_up_the_grid(); },
    );
    
    $tb->ToolButton (
        -text => 'Clear the grid',
        -command => sub { $self->clear_us_up_the_grid(); },
    );
    
    ${$self->status} = "status";
    
    my $sb = $mw->StatusBar();
    $sb->addLabel(
        -relief => 'flat',
        -textvariable => \$self->status,
    );

    my $canvas = $mw->Canvas(
        -width      => 0,
        -height     => 0,
        -background => "#AFFAAF",
    )->pack;
    $self->canvas( $canvas );
    
    $canvas->CanvasBind('<ButtonPress>' => [$self->curry::flip_square()]);
    $canvas->CanvasBind('<Motion>' => [$self->curry::duck()]);
    
    return;
} # /set_us_up_the_gui




=head2 clear_us_up_the_grid( $canvas )

Clears the canvas and removes the grid.

XXX: removing the grid resizes the window. This is ugly and should
be improved.

=cut

sub clear_us_up_the_grid {
    my $self = shift;
    my $canvas = $self->canvas;
    $self->Statuss(" ... clearing up the grid ... ", "");

    # if there isn't a grid, draw one
    if ( $#{$self->grid->[0]} == -1 ) {
        $self->draw_us_up_a_picture($self->MirrorCamel);
        return;
    }
    
    # if there is a grid, remove it

    $canvas->delete('all');
    $canvas->configure(-width => 0, -height => 0);

    $self->grid([[]]);

    $self->Statuss(" ... done clearing up the grid.");

    return;
} # /clear_us_up_the_grid




=head2 make_us_up_the_picture()

Creates an C<Image::XBM> object from the canvas - if there is
something on the canvas.

=cut

sub make_us_up_the_picture {
    my $self = shift;
    my $canvas = $self->canvas;
    $self->Statuss(' ... making up the picture ... ');

    my $picture = '';

    if ( $#{$self->grid->[0]} > -1 ) {
        for my $row (@{ $self->grid }) {
            for my $id (@{$row}) {
                $picture .= ($canvas->itemcget($id, -fill) eq 'white') ? '-' : '#';
            }
            $picture .= "\n";
        }
        $self->Statuss(' ... done making up the picture.');

        return Image::Xbm->new_from_string($picture);
    }

    $self->Statuss(' there was no grid to make a picture from.');

    return;
} # /make_us_up_the_picture




=head2 set_us_up_the_grid( $x?, $y?, $scale? )

Creates the grid where the XBM can be drawn.
When the grid is initialized from scratch, some example pircture
is inserted.

=cut

sub set_us_up_the_grid {
    my $self = shift;
    my ($x, $y, $scale) = @_;
    my $mw = $self->mw;
    
    $self->Statuss(" ... setting up the grid ...", "");

    $scale = 8 unless $scale;

    unless ($x and $y) {
        $x = ${$self->grid_width_x};
        $y = ${$self->grid_height_y};
    }

    ${$self->grid_width_x} = '';
    ${$self->grid_height_y} = '';

    $self->draw_us_up_a_picture($self->crazy0naCAMEL) unless $x and $y;

    $x =~ s/\D//;
    $y =~ s/\D//;

    $x =~ s/(.{2}).*$/$1/;
    $y =~ s/(.{2}).*$/$1/;

    return unless $x and $y;

    ${$self->grid_width_x} = $x if $x;
    ${$self->grid_height_y} = $y if $y;

    $self->clear_us_up_the_grid() if $#{$self->grid->[0]} > -1;
    
    my $canvas = $self->canvas;
    $canvas->configure(
        -width      => ($x + 2) * $scale,
        -height     => ($y + 2) * $scale,
        -background => "#AFFAAF",
    );

    my $grid = $self->layout_canvas_grid($self->canvas, $x, $y, $scale);
    $self->grid($grid);

    $self->Statuss(" ... done setting up the grid.");

    return;
} # /set_us_up_the_grid




=head2 layout_canvas_grid( $canvas, $colsX, $rowsY, $scale )

Creates a set of rectangles on the C<$canvas>.
C<$colsX> is the number of columns, C<$rowsY> is the number of rows. 
C<$scale> is the width of a single rectabgle.

=cut

sub layout_canvas_grid {
    my $self = shift;
    my ($widget, $colsX, $rowsY, $scale) = @_;

    $scale = 10 unless $scale;

    my @danums = ();

    for my $iy (1 .. $rowsY) {
        my @dabums = ();
        for my $ix (1 .. $colsX) {
            my $id = $widget->create(
                'rectangle',
                $ix * $scale,
                $iy * $scale,
                $ix * $scale + $scale,
                $iy * $scale + $scale,
                -fill => 'white',
            );

            $widget->itemconfigure($id);
            push @dabums, $id;
        }
        push @danums, \@dabums;
    }

    return \@danums;
} # /layout_canvas_grid




=head2 wipe_em_all( $color )

Fills all squares with color C<$color>.

C<$color> is either black or white.

=cut

sub wipe_em_all {
    my $self = shift;
    my $color = shift or die('Missing color');
    die('Color must be one of: black, white') if $color !~ m/^black|white$/;
    my $canvas = $self->canvas;
    
    # assumtion: there is nothing else on the canvas
    $canvas->itemconfigure('all', -fill => $color,);
    
    return;
} # /wipe_em_all




=head2 flip_em_all()

Flips all squares on the canvas.
All white squares will be black, all black squares will be white.

=cut

sub flip_em_all {
    my $self = shift;
    my $canvas = $self->canvas;
    
    for my $row (@{ $self->grid }) {
        for my $id (@{$row}) {
            my $color = $canvas->itemcget($id, -fill);
            $color = ($color eq 'white') ? 'black' : 'white';
            $canvas->itemconfigure($id, -fill => $color,);
        }
    }
} # /flip_em_all




=head2 flip_square( $canvas, $draw_black_or_white? )

Modifies the color of a square.
This methods looks far more complex than it has to be.
Complexity might be due to refactoring

This method evalues where the user clicked on the C<$canvas>.

C<$draw_black_or_white> should be C<undef>, C<0>, or C<1>.

TODO: improve this method.

=cut

sub flip_square {
    my $self = shift;
    my $canvas = shift or die('Missing canvas');
    my $draw_black_or_white = shift; # may be undef
    
    my $Ev = $canvas->XEvent;
    my $x = $Ev->x;
    my $y = $Ev->y;
    
    my $id = $canvas->find('overlapping', $x, $y, $x + 1, $y + 1);

    if ($id) {
        my $color = $canvas->itemcget($id, -fill);
        if (defined $draw_black_or_white) {
            if ($draw_black_or_white) {
                $color = 'white';
            } else {
                $color = 'black';
            }
        } else {
            $color = (defined($color) && $color eq 'white') ? 'black' : 'white';
        }

        $canvas->itemconfigure($id, -fill => $color,);
    }
    return;
} # /flip_square




=head2 draw_us_up_a_picture( $string )

Draws a pircture from C<$string>.

=cut

sub draw_us_up_a_picture {
    my $self = shift;
    my $string = shift or die('Missing string to draw');
    
    my $xbm = Image::Xbm->new_from_string($string);
    my @picrow = map { [ split '', $_ ] } split "\n", $xbm->as_string();

    return unless @picrow;

    my ($xbm_height, $xbm_width) = $xbm->get( '-height', '-width' );
    
    $self->set_us_up_the_grid($xbm_width, $xbm_height);

    my $canvas = $self->canvas;
    for (my $iy = 0 ; $iy < $xbm_height ; $iy++) {
        for (my $ix = 0 ; $ix < $xbm_width ; $ix++) {
            $canvas->itemconfigure(
                $self->grid->[$iy][$ix],
                -fill => 'black',
            ) unless $picrow[$iy][$ix] ne '#';
        }
    }

    return;
} # /draw_us_up_a_picture




=head1 TODOs / Plans

I<I can't wait to do this in Wx already.> (crazyinsomniac)

rewrite in Wx eventually
(ought to speed everything up, makes adding scrollbars easier)

but first add undo/redo, and

the behaviour so button_down/button_up does a bind/unbind of

Motion (rather than listening all the time)
and add for configurable [SQUARE] sizes ('em little boxes which flip)
that way you can cram a 500 x 500 xbm on screen ;)

add Ctrl-s (save buffer to file) Ctrl-a ( save as)

=head1 SEE ALSO

Source: L<http://www.perlmonks.org/?node_id=163230> downloaded 2017-04-27 11:04

=head1 AUTHOR

Original version written by crazyinsomniac. 

=head1 CONTRIBUTORS

Alex Becker, E<lt>c a p f a n -at- g m x -dot- d eE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2002 by crazyinsomniac

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.18.2 or,
at your option, any later version of Perl 5 you may have available.

=cut

1; # /My::XBMEditor