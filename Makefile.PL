use 5.008008;
use ExtUtils::MakeMaker;
WriteMakefile(
    NAME              => 'My::XBMEditor',
    VERSION_FROM      => 'lib/My/XBMEditor.pm', # finds $VERSION
    ABSTRACT_FROM  => 'lib/My/XBMEditor.pm', # retrieve abstract from module
    PREREQ_PM         => {
        # e.g., Module::Name => 1.1
        'Tk' => '804',
        'ExtUtils::MakeMaker' => '6.64',
        'Moose' => 0,
        'Moose::Util::TypeConstraints' => 0,
        'curry' => '1.0',
        'Tk::StatusBar' => 0,
        'Tk::ToolBar' => 0,
        'Image::Xbm' => 0,
    },
    TEST_REQUIRES => {
        # modules only required for testing
        'Test::More' => '0.98',
        'Test::Pod' => '1.0',
        'Test::Pod::Coverage' => '1.08',
    },
    AUTHOR         => 'Alex Becker <c a p f a n -4t- g m x -d0t- d e>',
    LICENSE        => 'perl_5',
    dist           => { COMPRESS => "gzip", SUFFIX => "gz" },
    META_MERGE     => {
        resources => {
            repository => 'https://github.com/asb-capfan/pTkXbmEdit',
        },
    },
    MIN_PERL_VERSION => 5.008008,
);
