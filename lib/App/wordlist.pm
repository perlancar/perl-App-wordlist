package App::wordlist;

# DATE
# VERSION

use 5.010001;
use strict;
use warnings;

use List::Util qw(shuffle);

our %SPEC;

our %arg_wordlists = (
    wordlists => {
        'x.name.is_plural' => 1,
        schema => ['array*' => of => 'str*'],
        summary => 'Select one or more wordlist modules',
        cmdline_aliases => {w=>{}},
        element_completion => sub {
            require Complete::Util;

            my %args = @_;
            Complete::Util::complete_array_elem(
                word  => $args{word},
                array => [map {$_->{name}} @{ _list_installed() }],
                ci    => 1,
            );
        },
    },
);

sub _list_installed {
    require Module::List;
    my $mods = Module::List::list_modules(
        "WordList::",
        {
            list_modules  => 1,
            list_pod      => 0,
            recurse       => 1,
        });
    my @res;
    for my $wl0 (sort keys %$mods) {
        $wl0 =~ s/\AWordList:://;
        my $wl = $wl0;

        my $type;
        if ($wl =~ /^(Base|MetaSyntactic)\z/) {
            # just a base class
            next;
        } elsif ($wl =~ s/^Base:://) {
            $type = 'Base';
        } elsif ($wl =~ s/^MetaSyntactic:://) {
            $type = 'MetaSyntactic';
        } elsif ($wl =~ s/^Char:://) {
            $type = 'Char';
        } elsif ($wl =~ s/^Phrase:://) {
            $type = 'Phrase';
        } elsif ($wl =~ s/^Test:://) {
            $type = 'Test';
        } else {
            $type = 'Word';
        }

        my $lang = '';
        if ($wl =~ /^(\w\w)::/) {
            $lang = $1;
        }

        push @res, {
            name => $wl0,
            lang => $lang,
            type => $type,
        };
     }
    \@res;
}

$SPEC{wordlist} = {
    v => 1.1,
    summary => 'Grep words from WordList::*',
    args => {
        arg => {
            schema => ['array*' => of => 'str*'],
            pos => 0,
            greedy => 1,
        },
        ignore_case => {
            schema  => 'bool',
            default => 1,
        },
        len => {
            schema  => 'int*',
        },
        min_len => {
            schema  => 'int*',
        },
        max_len => {
            schema  => 'int*',
        },
        num => {
            summary => 'Return (at most) this number of words (0 = unlimited)',
            schema  => ['int*', min=>0, max=>9999],
            default => 0,
            cmdline_aliases => {n=>{}},
        },
        random => {
            summary => 'Pick random words (must set --num to positive number)',
            schema  => 'bool*',
            cmdline_aliases => {r=>{}},
        },
        %arg_wordlists,
        or => {
            summary => 'Match any word in query instead of the default "all"',
            schema  => 'bool',
        },
        action => {
            schema  => ['str*', in=>[
                'list_cpan', 'list_installed',
                'grep',
            ]],
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
            },
        },
        lcpan => {
            schema => 'bool',
            summary => 'Use local CPAN mirror first when available (for -L)',
        },
        detail => {
            summary => 'Display more information when listing modules/result',
            description => <<'_',

When listing installed modules (`-l`), this means also returning a wordlist's
type and language.

When returning grep result, this means also returning wordlist name.

_
            schema  => 'bool',
        },
        types => {
            summary => 'Only include wordlists of certain type(s)',
            description => <<'_',

By convention, type information is encoded in the wordlist's name. `Char` means
to only include wordlists with names matching `Char::*`. `Phrase` means to only
include wordlists with names matching `Phrase::*`. `Word` means to only include
wordlists that are not of type `Char` and `Phrase`. `Test` means to only include
wordlists with names matching `Test::*`.

_
            schema => ['array*', of => ['str*', in=>['Base', 'MetaSyntactic', 'Char', 'Phrase', 'Word', 'Test']]],
            description => <<'_',

By convention, language information is encoded in the wordlist's name. For
example, English wordlists have names matching `EN::*` or `Word::EN::*` or
`Char::EN::*` or `Phrase::EN::*`.

_
            cmdline_aliases => {t=>{}},
        },
        langs => {
            'x.name.is_plural' => 1,
            summary => 'Only include wordlists of certain language(s)',
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
            argv => [qw/-t Phrase foo/],
            summary => 'Select phrase wordlists (multiple -t allowed)',
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
            argv => [qw/-l/],
            summary => 'List installed wordlist modules',
            test => 0,
            'x.doc.show_result' => 0,
        },
        {
            argv => [qw/-L/],
            summary => 'List wordlist modules available on CPAN',
            test => 0,
            'x.doc.show_result' => 0,
        },
    ],
    'cmdline.default_format' => 'text-simple',
};
sub wordlist {
    require Encode;

    my %args = @_;

    my $action = $args{action} // 'grep';
    my $list_installed = _list_installed();
    my $ci = $args{ignore_case} // 1;
    my $or = $args{or};
    my $arg = $args{arg} // [];
    my $detail = $args{detail};
    my $num = $args{num} // 0;
    my $random = $args{random};

    return [412, "Must set --num to positive number when --random"]
        if $random && !$num;

    if ($action eq 'grep') {
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
        my $wordlists;
        if ($args{wordlists}) {
            $wordlists = $args{wordlists};
        } else {
            $wordlists = [];
            for my $rec (@$list_installed) {
                if ($args{types} && @{ $args{types} }) {
                    next unless grep { $rec->{type} eq $_ } @{$args{types}};
                }
                if ($args{langs} && @{ $args{langs} }) {
                    next unless grep { $rec->{lang} eq uc($_) } @{$args{langs}};
                }
                push @$wordlists, $rec->{name};
            }
        }
        $wordlists = [shuffle @$wordlists] if $random;

        my $n = 0;

        my $code_add_word = sub {
            my ($wl, $word) = @_;
            if ($random) {
                if (@res < $num) {
                    splice @res, rand(@res+1), 0,
                        $detail ? [$wl, $word] : $word;
                } else {
                    rand($n) < @res and splice @res, rand(@res), 1,
                        $detail ? [$wl, $word] : $word;
                }
            } else {
                push @res, $detail ? [$wl, $word] : $word;
            }
        };

        for my $wl (@$wordlists) {
            my $mod = "WordList::$wl";
            (my $modpm = $mod . ".pm") =~ s!::!/!g;
            require $modpm;
            my $obj = $mod->new;
            $obj->each_word(
                sub {
                    my $word = shift;

                    return if !$random && $num > 0 && $n >= $num;
                    return if defined($args{len}) &&
                        length($word) != $args{len};
                    return if defined($args{min_len}) &&
                        length($word) < $args{min_len};
                    return if defined($args{max_len}) &&
                        length($word) > $args{max_len};

                    my $cmpword = $ci ? lc($word) : $word;
                    for (@$arg) {
                        my $match =
                            ref($_) eq 'Regexp' ? $cmpword =~ $_ :
                                index($cmpword, $_) >= 0;
                        if ($or) {
                            # succeed early when --or
                            if ($match) {
                                $n++;
                                $code_add_word->($wl, $word);
                                return;
                            }
                        } else {
                            # fail early when and (the default)
                            if (!$match) {
                                return;
                            }
                        }
                    }
                    if (!$or || !@$arg) {
                        $n++;
                        $code_add_word->($wl, $word);
                    }
                }
            );
        }
        [200, "OK", \@res];

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
                return [200, "OK", [$random ? shuffle(@res) : sort(@res)]];
            }
        }
        return [412, "Can't find a way to list CPAN mirrors"];

    } else {

        [400, "Unknown action '$action'"];

    }
}

1;
# ABSTRACT:

=head1 SYNOPSIS

See the included script L<wordlist>.


=head1 ENVIRONMENT

=head2 DEBUG => bool


=head1 SEE ALSO

L<App::GamesWordlist> (L<games-wordlist>) which greps from
C<Games::Word::Wordlist::*> instead.

L<WordList> and C<WordList::*> modules.
