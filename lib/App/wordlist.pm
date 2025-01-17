## no critic: InputOutput::ProhibitInteractiveTest
package App::wordlist;

use 5.010001;
use strict;
use warnings;
use Log::ger;

use List::Util qw(shuffle);
use Perinci::Sub::Util qw(gen_modified_sub);

# AUTHORITY
# DATE
# DIST
# VERSION

our %SPEC;

our %argspecopt_wordlists = (
    wordlists => {
        summary => 'Select one or more wordlist modules',
        'x.name.is_plural' => 1,
        schema => ['array*' => {
            of => 'str*', # for the moment we need to use 'str' instead of 'perl::modname' due to Perinci::Sub::GetArgs::Argv limitation
            'x.perl.coerce_rules'=>[ ['From_str_or_array::expand_perl_modname_wildcard'=>{ns_prefix=>"WordList"}] ],
        }],
        cmdline_aliases => {w=>{}},
        element_completion => sub {
            require Complete::Util;

            my %args = @_;
            Complete::Util::complete_array_elem(
                word  => $args{word},
                array => [map {$_->{name}} @{ _list_installed() }],
            );
        },
        tags => ['category:module-selection'],
    },
);

our %argspecsopt_exclude_wordlist = (
    exclude_wordlists => {
        'x.name.is_plural' => 1,
        'x.name.singular' => 'exclude_wordlist',
        summary => 'Exclude wordlist modules',
        schema => ['array*' => {
            of => 'str*', # for the moment we need to use 'str' instead of 'perl::modname' due to Perinci::Sub::GetArgs::Argv limitation
            'x.perl.coerce_rules'=>[ ['From_str_or_array::expand_perl_modname_wildcard'=>{ns_prefix=>"WordList"}] ],
        }],
        element_completion => sub {
            require Complete::Util;

            my %args = @_;
            Complete::Util::complete_array_elem(
                word  => $args{word},
                array => [map {$_->{name}} @{ _list_installed() }],
            );
        },
        cmdline_aliases => {X=>{}},
        tags => ['category:module-selection'],
    },
    exclude_wordlist_pattern => {
        schema => 're_from_str*',
        cmdline_aliases => {P=>{}},
        tags => ['category:module-selection'],
    },
    exclude_dynamic_wordlists => {
        schema => 'bool*',
        cmdline_aliases => {D=>{}},
        tags => ['category:module-selection'],
    },
);

our %argspecopt_wordlist_bundles = (
    wordlist_bundles => {
        'x.name.is_plural' => 1,
        'x.name.singular' => 'wordlist_bundle',
        schema => ['array*' => {
            of => 'str*', # for the moment we need to use 'str' instead of 'perl::modname' due to Perinci::Sub::GetArgs::Argv limitation
            'x.perl.coerce_rules'=>[ ['From_str_or_array::expand_perl_modname_wildcard'=>{ns_prefix=>"WordList"}] ],
        }],
        summary => 'Select one or more wordlist bundle (Acme::CPANModules::WordListBundle::*) modules',
        cmdline_aliases => {b=>{}},
        element_completion => sub {
            require Complete::Util;

            my %args = @_;
            Complete::Util::complete_array_elem(
                word  => $args{word},
                array => [map {$_->{name}} @{ _list_installed_bundles() }],
            );
        },
        tags => ['category:module-selection'],
    },
);

sub _length_in_graphemes {
    my $length = () = $_[0] =~ m/\X/g;
    return $length;
}

sub _list_installed {
    require Module::List;
    my $mods = Module::List::list_modules(
        "WordList::",
        {
            list_modules  => 1,
            list_pod      => 0,
            recurse       => 1,
            return_path   => 1,
        });
    my @res;
    for my $wl0 (sort keys %$mods) {
        (my $wl = $wl0) =~ s/\AWordList:://;

        my $lang = '';
        if ($wl =~ /^(\w\w)::/) {
            $lang = $1;
        }

        push @res, {
            name => $wl,
            lang => $lang,
            path => $mods->{$wl0}{module_path},
        };
     }
    \@res;
}

sub _list_installed_bundles {
    require Module::List;
    my $mods = Module::List::list_modules(
        "Acme::CPANModules::WordListBundle::",
        {
            list_modules  => 1,
            list_pod      => 0,
            recurse       => 1,
            return_path   => 1,
        });
    my @res;
    for my $wlb0 (sort keys %$mods) {
        (my $wlb = $wlb0) =~ s/\AAcme::CPANModules::WordListBundle:://;

        my $lang = '';
        if ($wlb =~ /^(\w\w)::/) {
            $lang = $1;
        }

        push @res, {
            name => $wlb,
            lang => $lang,
            path => $mods->{$wlb0}{module_path},
        };
     }
    \@res;
}

sub _word_has_chars_unordered {
    my ($word, $chars, $ci) = @_;

    if ($ci) {
        $word = lc $word;
        $chars = lc $chars;
    }

    for my $i (0..length($chars)-1) {
        my $char = substr($chars, $i, 1);
        my $index = index($word, $char);
        return 0 if $index < 0;
    }
    1;
}

sub _word_has_chars_ordered {
    my ($word, $chars, $ci) = @_;

    if ($ci) {
        $word = lc $word;
        $chars = lc $chars;
    }

    my $last_index;
    for my $i (0..length($chars)-1) {
        my $char = substr($chars, $i, 1);
        my $index = index($word, $char);
        return 0 if $index < 0;
        return 0 if defined $last_index && $index < $last_index;
        $last_index = $index;
        $word =~ s/\Q$char\E//;
    }
    1;
}

$SPEC{wordlist} = {
    v => 1.1,
    summary => 'Grep words from (or test them against) WordList::*',
    args => {
        arg => {
            schema => ['array*' => of => 'str*'],
            pos => 0,
            greedy => 1,
            tags => ['category:word-filtering'],
        },
        ignore_case => {
            schema  => 'bool',
            default => 1,
            cmdline_aliases => {i=>{}},
            tags => ['category:word-filtering'],
        },
        len => {
            schema  => 'int*',
            tags => ['category:word-filtering'],
        },
        min_len => {
            schema  => 'int*',
            tags => ['category:word-filtering'],
        },
        max_len => {
            schema  => 'int*',
            tags => ['category:word-filtering'],
        },
        num => {
            summary => 'Return (at most) this number of words (0 = unlimited)',
            schema  => ['int*', min=>0, max=>9999],
            default => 0,
            cmdline_aliases => {n=>{}},
        },
        random => {
            summary => 'Pick random words',
            description => <<'_',

If set to true, then streaming will be turned off. All words will be gathered
first, then words will be chosen randomly from the gathered list.

_
            schema  => 'bool*',
            cmdline_aliases => {r=>{}},
        },

        %argspecopt_wordlists,
        %argspecopt_wordlist_bundles,

        %argspecsopt_exclude_wordlist,

        or => {
            summary => 'Instead of printing words that must match all queries (the default), print words that match any query',
            schema  => 'bool',
            tags => ['category:word-filtering'],
        },
        action => {
            schema  => ['str*', {
                in=>[
                    'list_cpan',
                    'list_installed',
                    'list_selected',
                    'grep',
                    'stat',
                    'test',
                ],
                'x.in.summaries' => [
                    'List WordList::* modules on CPAN',
                    'List WordList::* modules installed locally',
                    'List WordList::* that are selected for use',
                    'Grep words from selected WordList::* modules',
                    'Show statistics for each selected WordList::* modules',
                    'Test words against selected WordList::* modules',
                ],
            }],
            default => 'grep',
            cmdline_aliases => {
                l => {
                    summary=>'List installed WordList::* modules',
                    is_flag => 1,
                    code => sub { my $args=shift; $args->{action} = 'list_installed' },
                },
                L => {
                    summary=>'List WordList::* modules on CPAN',
                    is_flag => 1,
                    code => sub { my $args=shift; $args->{action} = 'list_cpan' },
                },
                s => {
                    summary=>'Show statistics contained in the wordlist modules',
                    is_flag => 1,
                    code => sub { my $args=shift; $args->{action} = 'stat' },
                },
                t => {
                    summary=>'Test whether words exists in wordlist',
                    is_flag => 1,
                    code => sub { my $args=shift; $args->{action} = 'test' },
                },
            },
            description => <<'_',

Action `list_installed` (shortcut option `-l`) will list WordList::* modules
installed on the local system.

Action `list_cpan` (shortcut option `-L`) will list available WordList::*
modules on CPAN, either by querying the MetaCPAN site or by querying a local
mini CPAN using <pm:App::lcpan>.

Action `list_selected` (option `--action=list_selected`) will list the selected
WordList::* modules (e.g. via `-w` or `-b`).

Action `grep` (the default action) will filter the words from each selected
wordlists and print them.

Action `stat` (shortcut option `-s`) will show statistics about all the selected
wordlists.

Action `test` (shortcut option `-t`) will check whether words are in one of the
wordlist, using `word_exists()` method on each wordlist.


_
        },
        lcpan => {
            schema => 'bool',
            summary => 'Use local CPAN mirror first when available (for -L)',
        },
        detail => {
            summary => 'Display more information when listing modules/result',
            description => <<'_',

When listing installed modules (`-l`), this means also returning a wordlist's
language.

When returning grep result, this means also returning wordlist name.

_
            schema  => 'bool',
        },
        chars_unordered => {
            summary => 'Specify possible characters for the word (unordered)',
            schema => 'str*',
            tags => ['category:word-filtering'],
        },
        chars_ordered => {
            summary => 'Specify possible characters for the word (ordered)',
            schema => 'str*',
            tags => ['category:word-filtering'],
        },
        langs => {
            'x.name.is_plural' => 1,
            summary => 'Only include wordlists of certain language(s)',
            description => <<'_',

By convention, language code is the first subnamespace of a wordlist module,
e.g. WordList::EN::* for English, WordList::FR::* for French, and so on.
Wordlist modules which do not follow this convention (e.g. WordList::Password::*
or WordList::PersonName::*) are not included.

_
            schema => ['array*', of => ['str*', match => '\A\w\w\z']],
            element_completion => sub {
                my %args = @_;
                my @langs;
                for my $rec (@{ _list_installed() }) {
                    next unless length $rec->{lang};
                    push @langs, $rec->{lang}
                        unless grep {$_ eq $rec->{name}} @langs;
                }
                require Complete::Util;
                Complete::Util::complete_array_elem(
                    word => $args{word}, array => \@langs);
            },
            tags => ['category:module-selection'],
        },
        color => {
            summary => 'When to highlight search string/matching pattern with color',
            schema => ['str*', {
                in=>[
                    'never',
                    'always',
                    'auto',
                ],
                'x.in.summaries' => [
                    'Never show color',
                    'Show color if program is run interactively (i.e. not piped)',
                    'Always show color, regardless of whether program is run through a pipe',
                ],
            }],
            default => 'auto',
        },
    },
    examples => [
        {
            argv => [],
            summary => 'By default print all words from all wordlists',
            test => 0,
            'x.doc.show_result' => 0,
        },
        {
            argv => [qw/-D/],
            summary => 'Print all words from all static wordlists (dynamic ones are excluded with --exclude-dynamic-wordlists a.k.a. -D)',
            test => 0,
            'x.doc.show_result' => 0,
        },
        {
            argv => [qw/foo bar/],
            summary => 'Print all words matching /foo/ and /bar/',
            test => 0,
            'x.doc.show_result' => 0,
        },
        {
            argv => [qw/--or foo bar/],
            summary => 'Print all words matching /foo/ or /bar/',
            test => 0,
            'x.doc.show_result' => 0,
        },
        {
            argv => [qw/--detail foo/],
            summary => 'Print wordlist name for each matching words',
            test => 0,
            'x.doc.show_result' => 0,
        },
        {
            argv => [qw/-w ID::KBBI foo/],
            summary => 'Select a specific wordlist (multiple -w allowed)',
            test => 0,
            'x.doc.show_result' => 0,
        },
        {
            argv => [qw/-w ID::** foo/],
            summary => 'Select all ID::* wordlists (wildcard will be expanded)',
            test => 0,
            'x.doc.show_result' => 0,
        },
        {
            argv => [qw/-b Proverbs foo/],
            summary => 'Select a bunch of wordlists via wordlist bundle (Acme::CPANModules::WordListBundle::* module)',
            test => 0,
            'x.doc.show_result' => 0,
        },
        {
            argv => [qw/-w Phrase::**::Proverb::** foo/],
            summary => 'An alternative to select all proverb wordlists',
            test => 0,
            'x.doc.show_result' => 0,
        },
        {
            argv => [qw/-w Phrase::**::Proverb::** --action=list_selected/],
            summary => 'Check to see which wordlists we are selecting via `-w` with wildcards or via `-b`',
            test => 0,
            'x.doc.show_result' => 0,
        },
        {
            argv => [qw/-w EN::Enable --len 6 -i --chars-unordered bobleg/],
            summary => 'Print all words from EN::Enable wordlist that are 6 characters long and have the letters BOBLEG (in no particular order); great for cheats in word forming games',
            test => 0,
            'x.doc.show_result' => 0,
        },
        {
            argv => [qw/-w EN::Enable --len 6 -i --chars-ordered BGL/],
            summary => 'Print all words from EN::Enable wordlist that are 6 characters long and have the letters B,G,L (in that order); great for finding crossword puzzle answers',
            test => 0,
            'x.doc.show_result' => 0,
        },

        {
            argv => [qw/--lang FR foo/],
            summary => 'Select French wordlists (multiple --lang allowed)',
            test => 0,
            'x.doc.show_result' => 0,
        },
        {
            argv => [qw|/fof[aeiou]/|],
            summary => 'Filter by regex',
            test => 0,
            'x.doc.show_result' => 0,
        },
        {
            argv => ['-w', 'MetaSyntactic=theme,dangdut'],
            summary => 'Select a wordlist with parameters',
            test => 0,
            'x.doc.show_result' => 0,
        },
        {
            argv => [qw/-l/],
            summary => 'List installed wordlist modules',
            test => 0,
            'x.doc.show_result' => 0,
            tags => ['category:action-list-installed'],
        },
        {
            argv => [qw/-L/],
            summary => 'List wordlist modules available on CPAN',
            test => 0,
            'x.doc.show_result' => 0,
            tags => ['category:action-list-cpan'],
        },
        {
            argv => [qw/-w ID::KBBI -s/],
            summary => 'Show statistics of a wordlist module',
            test => 0,
            'x.doc.show_result' => 0,
            tags => ['category:action-stat'],
        },
        {
            argv => [qw/-w Password::RockYou::BloomOnly -t foobar 123456 someGoodPass923/],
            summary => 'Check some passwords against a password wordlist',
            test => 0,
            'x.doc.show_result' => 0,
            tags => ['category:action-test'],
        },
        {
            argv => [qw/-w Password::** -X Password::RockYou -t foobar 123456 someGoodPass923/],
            summary => 'Check some passwords against all Password wordlists except Password::RockYou (because it is slow to check)',
            test => 0,
            'x.doc.show_result' => 0,
            tags => ['category:action-test'],
        },
        {
            argv => [qw/-w Password::** -P RockYou -t foobar 123456 someGoodPass923/],
            summary => 'Check some passwords against all Password wordlists except those matching /RockYou/ regex',
            test => 0,
            'x.doc.show_result' => 0,
            tags => ['category:action-test'],
        },
    ],
    'cmdline.default_format' => 'text-simple',
    'x.doc.faq' => <<'_',

## How to select multiple wordlists? It's cumbersome having to -w WORDLIST1 -w WORDLIST2 and so on!

You can specify wildcard in `-w` option, e.g. if you want to include all English
wordlists you can use `-w EN::*` or `-w EN::**` (`**` recurses while `*` only
matches one level deep).

Or you can also use `-b` option. Some people bundle wordlists together and put
them up on CPAN in the `Acme::CPANModules::WordListBundle::*` namespace. You can
install those modules first then use the wordlist bundle.

## Can `wordlist` help me solve Wordle?

Yes, using regex or the `--chars-ordered` and `--chars-unordered` options. For
example, if you have:

    T W _ S _

(3 letters with the correct position), you can use:

    % wordlist -w EN::Wordle '/^tw.s./' --len 5
    twist

or:

    % wordlist -w EN::Wordle --chars-ordered tws --len 5
    tawse
    thaws
    ...
    twist
    twits

Another example, if you have:

    W* T* _ S _

(2 letters with the incorrect position and 1 letter in the correct position),
you can use:

    % wordlist -w EN::Wordle --chars-unordered wts --len 5 '/^...s.$/'

Included in the distribution is the <prog:wordlist-wordle> script for
convenience. This CLI defaults to grepping the "EN::Wordle" wordlist and you
specify something like `wt_S_` for the pattern (lowercase for letter in
incorrect position, uppercase for letter in correct position, underscore for
unguessed):

    % wordlist-wordle 'wt_S_'

_
};
sub wordlist {
    require Encode;
    require Module::Load::Util;

    my %args = @_;

    my $action = $args{action} // 'grep';
    my $list_installed = _list_installed();
    my $ci = $args{ignore_case} // 1;
    my $or = $args{or};
    my $arg = $args{arg} // [];
    my $detail = $args{detail};
    my $num = $args{num} // 0;
    my $random = $args{random};
    my $color = $args{color} // 'auto';

    my $use_color = ($color eq 'always' ? 1 : $color eq 'never' ? 0 : undef)
        // $ENV{COLOR} // (-t STDOUT);

    if ($action eq 'grep' || $action eq 'stat' || $action eq 'list_selected' || $action eq 'test') {
        # convert /.../ in arg to regex
        for (@$arg) {
            $_ = Encode::decode('UTF-8', $_);
            if (m!\A/(.*)/\z!) {
                $_ = $ci ? qr/$1/i : qr/$1/;
            } else {
                $_ = lc($_) if $ci;
            }
        }

        my @res;
        my $wordlists = [];
        my $has_specified_list;

        if ($args{wordlist_bundles}) {
            $has_specified_list++;
            for my $wb (@{ $args{wordlist_bundles} }) {
                my $wbmod = "Acme::CPANModules::WordListBundle::$wb";
                (my $wbmodpm = "$wbmod.pm") =~ s!::!/!g;
                require $wbmodpm;

                my $list;
                {
                    no strict 'refs'; ## no critic: TestingAndDebugging::ProhibitNoStrict
                    $list = ${"$wbmod\::LIST"};
                }

                my $i = -1;
                for my $entry (@{ $list->{entries} }) {
                    $i++;
                    my $mod = $entry->{module};
                    $mod =~ s/\AWordList::// or do {
                        warn "Wordlist bundle module $wbmod: entry[$i]: module is not WordList::, skipped";
                        next;
                    };
                    push @$wordlists, $mod;
                }
            } # for $wb
        }

        if ($args{wordlists}) {
            $has_specified_list++;
            push @{ $wordlists }, @{ $args{wordlists} };
        }

        unless ($has_specified_list) {
            $wordlists = [];
            for my $rec (@$list_installed) {
                if ($args{langs} && @{ $args{langs} }) {
                    next unless grep { $rec->{lang} eq uc($_) } @{$args{langs}};
                }
                push @$wordlists, $rec->{name};
            }
        }

      EXCLUDE_WORDLISTS: {
            if ($args{exclude_wordlists} && @{ $args{exclude_wordlists} }) {
                my $filtered_wordlists = [];
              WORDLIST:
                for my $wl (@$wordlists) {
                    for my $exwl (@{ $args{exclude_wordlists} }) {
                        do { log_trace "Excluding wordlist $wl (--excluded-wordlist)"; next WORDLIST } if $wl eq $exwl;
                    }
                    push @$filtered_wordlists, $wl;
                }
                $wordlists = $filtered_wordlists;
            }

            if (defined $args{exclude_wordlist_pattern}) {
                my $filtered_wordlists = [];
              WORDLIST:
                for my $wl (@$wordlists) {
                    do { log_trace "Excluding wordlist $wl (--excluded-wordlist-pattern)"; next WORDLIST } if $wl =~ $args{exclude_wordlist_pattern};
                    push @$filtered_wordlists, $wl;
                }
                $wordlists = $filtered_wordlists;
            }

            if ($args{exclude_dynamic_wordlists}) {
                no strict 'refs'; ## no critic: TestingAndDebugging::ProhibitNoStrict
                my $filtered_wordlists = [];
              WORDLIST:
                for my $wl (@$wordlists) {
                    my $mod = "WordList::$wl"; $mod =~ s/=.*//;
                    (my $modpm = "$mod.pm") =~ s!::!/!g;
                    require $modpm;
                    do { log_trace "Excluding wordlist $wl (--excluded-dynamic-wordlists)"; next WORDLIST } if ${"$mod\::DYNAMIC"};
                    push @$filtered_wordlists, $wl;
                }
                $wordlists = $filtered_wordlists;
            }
        }

        $wordlists = [shuffle @$wordlists] if $random;
        log_trace "Wordlist(s) to use: %s", $wordlists;

        if ($action eq 'list_selected') {
            return [200, "OK", $wordlists];
        }

        if ($action eq 'stat') {
            no strict 'refs'; ## no critic: TestingAndDebugging::ProhibitNoStrict
            return [200] unless @$wordlists;
            my %all_stats;
          WORDLIST:
            for my $wl (@$wordlists) {
                my $mod = "WordList::$wl"; $mod =~ s/=.*//;
                (my $modpm = "$mod.pm") =~ s!::!/!g;
                require $modpm;
                if (@$wordlists == 1) {
                    return [200, "OK", \%{"$mod\::STATS"}];
                } else {
                    $all_stats{$wl} = \%{"$mod\::STATS"};
                }
            }
            return [200, "OK", \%all_stats];
        }

        if ($action eq 'test') {
            my @words = @{ $args{arg} };
            my $res = [200, "OK", [], {'table.fields'=>[qw/word found_in/]}];

          WORDLIST:
            for my $wl (@$wordlists) {
                last unless @words;
                log_debug "Testing against wordlist $wl ...";
                my $wl_obj;
                eval {
                    $wl_obj = Module::Load::Util::instantiate_class_with_optional_args(
                        {ns_prefix=>"WordList"}, $wl);
                };
                if ($@) {
                    warn;
                    next WORDLIST;
                }
                for my $i (reverse (0 .. $#words)) {
                    if ($wl_obj->word_exists($words[$i])) {
                        push @{$res->[2]}, {word=>$words[$i], found_in=>$wl};
                    }
                }
            } # for WORDLIST

            return $res;
        }

        # optimize random picking when there's only one wordlist to pick from
        if ($random && @$wordlists == 1 && $num > 0 && $num <= 100) {
            my $wl_obj = Module::Load::Util::instantiate_class_with_optional_args(
                {ns_prefix=>"WordList"}, $wordlists->[0]);
            return [200, "OK", [$wl_obj->pick($num)]];
        }

        my $n = 0;

        my $code_format_word = sub {
            my ($wl, $word, $highlight_str, $ci) = @_;
            #use DD; dd \@_;
            if (defined $highlight_str) {
                if (ref $highlight_str eq 'Regexp') {
                    $word =~ s/($highlight_str)/\e[1;31m$1\e[0m/g;
                } else {
                    if ($ci) {
                        $word =~ s/(\Q$highlight_str\E)/\e[1;31m$1\e[0m/gi;
                    } else {
                        $word =~ s/(\Q$highlight_str\E)/\e[1;31m$1\e[0m/g;
                    }
                }
            }
            $detail ? [$wl, $word] : $word;
        };

        my $i_wordlist = 0;
        my $nth_word;
        my $wl_obj;
        my $code_return_word = sub {
          REDO:
            return if $i_wordlist >= @$wordlists;
            my $wl = $wordlists->[$i_wordlist];
            unless ($wl_obj) {
                log_trace "Instantiating wordlist $wl ...";
                eval {
                    $wl_obj = Module::Load::Util::instantiate_class_with_optional_args(
                        {ns_prefix=>"WordList"}, $wl);
                };
                warn if $@;
                unless ($wl_obj) {
                    $i_wordlist++;
                    goto REDO;
                }
                $nth_word = 0;
            }
            my $word = $nth_word++ ? $wl_obj->next_word : $wl_obj->first_word;
            unless (defined $word) {
                undef $wl_obj;
                $i_wordlist++;
                goto REDO;
            }

            goto REDO if !$random && $num > 0 && $n >= $num;
            goto REDO if defined($args{len}) &&
                _length_in_graphemes($word) != $args{len};
            goto REDO if defined($args{min_len}) &&
                _length_in_graphemes($word) < $args{min_len};
            goto REDO if defined($args{max_len}) &&
                _length_in_graphemes($word) > $args{max_len};

            goto REDO if defined $args{chars_unordered} &&
                !_word_has_chars_unordered($word, $args{chars_unordered}, $ci);
            goto REDO if defined $args{chars_ordered} &&
                !_word_has_chars_ordered($word, $args{chars_ordered}, $ci);

            my $cmpword = $ci ? lc($word) : $word;
            my $match_arg;
            for (@$arg) {
                my $match =
                    ref($_) eq 'Regexp' ? $cmpword =~ $_ :
                    index($cmpword, $_) >= 0;
                if ($or) {
                    # succeed early when --or
                    if ($match) {
                        $n++;
                        return $code_format_word->(
                            $wl, $word, $use_color ? $_ : undef, $ci);
                    }
                } else {
                    # fail early when and (the default)
                    if (!$match) {
                        goto REDO;
                    }
                }
                $match_arg = $_;
            }
            if (!$or || !@$arg) {
                $n++;
                return $code_format_word->(
                    $wl, $word, $use_color ? $match_arg : undef, $ci);
            }
        };
        my $res = [200, "OK", $code_return_word, {stream=>1}];

      RANDOMIZE: {
            last unless $random;
            require Array::Pick::Scan;

            my @words;
            if ($num > 0) {
                @words = Array::Pick::Scan::random_item($res->[2], $num);
            } else {
                while (defined(my $word = $res->[2]->())) { push @words, $word }
                @words = shuffle @words;
            }
            $res = [200, "OK", \@words];
        }

      RETURN_RES:
        $res;

    } elsif ($action eq 'list_installed') {

        my @res;
        for (@$list_installed) {
            if ($detail) {
                push @res, $_;
            } else {
                push @res, $_->{name};
            }
        }
        [200, "OK", \@res,
         {('cmdline.default_format' => 'text') x !!$detail}];

    } elsif ($action eq 'list_cpan') {

        my @methods = $args{lcpan} ?
            ('lcpan', 'metacpan') : ('metacpan', 'lcpan');

      METHOD:
        for my $method (@methods) {
            if ($method eq 'lcpan') {
                unless (eval { require App::lcpan::Call; 1 }) {
                    warn "App::lcpan::Call is not installed, skipped listing ".
                        "modules from local CPAN mirror\n";
                    next METHOD;
                }
                my $res = App::lcpan::Call::call_lcpan_script(
                    argv => [qw/mods --namespace WordList/],
                );
                return $res if $res->[0] != 200;
                return [200, "OK",
                        [map {my $w = $_; $w =~ s/\AWordList:://; $w }
                             grep {/WordList::/} sort @{$res->[2]}]];
            } elsif ($method eq 'metacpan') {
                unless (eval { require MetaCPAN::Client; 1 }) {
                    warn "MetaCPAN::Client is not installed, skipped listing ".
                        "modules from MetaCPAN\n";
                    next METHOD;
                }
                my $mcpan = MetaCPAN::Client->new;
                my $rs = $mcpan->module({
                        'module.name'=>'WordList::*',
                    });
                my @res;
                while (my $row = $rs->next) {
                    my $mod = $row->module->[0]{name};
                    say "D: mod=$mod" if $ENV{DEBUG};
                    $mod =~ s/\AWordList:://;
                    push @res, $mod unless grep {$mod eq $_} @res;
                }
                warn "Empty result from MetaCPAN\n" unless @res;
                return [200, "OK", \@res];
            }
        }
        return [412, "Can't find a way to list CPAN mirrors"];

    } else {

        [400, "Unknown action '$action'"];

    }
}

gen_modified_sub(
    base_name => 'wordlist',
    output_name => 'wordlist_wordle',
    modify_args => {
        wordlists => sub {
            $_[0]{default} = ['EN::Wordle'];
        },
        len => sub {
            $_[0]{default} = 5;
        },
    },
    remove_args => [
        'action',
        'lcpan',
        'chars_unordered',
        'chars_ordered',
    ],
    modify_meta => sub {
        $_[0]{summary} = 'Help solve Wordle';
        delete $_[0]{'x.doc.faq'};
        $_[0]{description} = <<'_';

This is a wrapper to <prog:wordlist> designed to be a convenient helper to solve
Wordle puzzle. By default it greps from the `EN::Wordle` wordlist. It accepts
a series of guesses in a format like the following:

    A^R^isE^
    Pound
    might
    blA^ck
    PR^ivY^

where lowercase means wrong guess, uppercase means correct letter and position,
while (uppercase) letter followed by a caret (`^`) means the letter exists in
another position. It will convert these guesses to regex patterns and the
`--chars-unordered` option and pass it to `wordlist`.

_
        $_[0]{examples} = [
            {
                argv => ['cR^eEk'],
                summary => 'One guess',
                test => 0,
                'x.doc.show_result' => 0,
            },
            {
                argv => ['A^R^isE^', 'Pound', 'might', 'blA^ck', 'PR^ivY^'],
                summary => 'Five guesses',
                test => 0,
                'x.doc.show_result' => 0,
            },
        ];
    },
    output_code => sub {
        my %args = @_;

        $args{arg} //= [];

        my $chars_unordered = '';
        my $possible_letters = join '', "a".."z";
        my @new_arg;
        for my $arg (@{ $args{arg} }) {
            my @chars = split //, $arg;
            my $re = '';
            my %letter_exists;
            while (@chars) {
                my $char = shift @chars;
                return [400, "Invalid letter '$char' in guess '$arg'"] unless $char =~ /[A-Za-z]/;
                my $caret = @chars && $chars[0] eq '^' ? shift(@chars) : '';
                my $uc = $char eq uc $char;
                $char = lc $char;

                if ($caret) { # letter is in another position
                    my $letters = $possible_letters;
                    $letters =~ s/$char//;
                    $re .= "[$letters]";
                    $letter_exists{$char}++;
                    $chars_unordered .= $char unless index($chars_unordered, $char) >= 0;
                } elsif ($uc) { # correct guess
                    $re .= $char;
                    $letter_exists{$char}++;
                    $chars_unordered .= $char unless index($chars_unordered, $char) >= 0;
                } else { # wrong guess
                    my $letters = $possible_letters;
                    $letters =~ s/$char//;
                    $possible_letters =~ s/$char// unless $letter_exists{$char};
                    $re .= "[$letters]";
                }
            }
            $re = "/\\A$re\\z/";
            push @new_arg, $re;
        }

        $args{arg} = \@new_arg;
        $args{chars_unordered} = $chars_unordered if length $chars_unordered;

        log_trace "Arguments passed to wordlist(): %s", \%args;
        wordlist(%args);
    },
);

1;
# ABSTRACT:

=head1 SYNOPSIS

See the included script L<wordlist>.


=head1 ENVIRONMENT

=head2 DEBUG => bool

=head2 COLOR => bool

Set color on/off when --color=auto (the default).


=head1 SEE ALSO

L<App::WordListUtils>

L<App::GamesWordlist> (L<games-wordlist>) which greps from
C<Games::Word::Wordlist::*> instead.

L<WordList> and C<WordList::*> modules.

L<arraydata> from L<App::arraydata>, L<hashdata> from L<App::hashdata>, and
L<tabledata> from L<App::tabledata>. These are newer projects that will
supersede WordList one day.
