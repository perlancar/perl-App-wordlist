package App::wordlist;

# AUTHORITY
# DATE
# DIST
# VERSION

use 5.010001;
use strict;
use warnings;

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
language.

When returning grep result, this means also returning wordlist name.

_
            schema  => 'bool',
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
        },
        color => {
            summary => 'When to highlight search string/matching pattern with color',
            schema => ['str*', in=>['never', 'always', 'auto']],
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
        #{
        #    argv => [qw/-t Phrase foo/],
        #    summary => 'Select phrase wordlists (multiple -t allowed)',
        #    test => 0,
        #    'x.doc.show_result' => 0,
        #},
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
    my $color = $args{color} // 'auto';

    my $use_color = ($color eq 'always' ? 1 : $color eq 'never' ? 0 : undef)
        // $ENV{COLOR} // (-t STDOUT);

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
                if ($args{langs} && @{ $args{langs} }) {
                    next unless grep { $rec->{lang} eq uc($_) } @{$args{langs}};
                }
                push @$wordlists, $rec->{name};
            }
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
        my $wl_obj;
        my $code_return_word = sub {
          REDO:
            return if $i_wordlist >= @$wordlists;
            my $wl = $wordlists->[$i_wordlist];
            unless ($wl_obj) {
                my $mod = "WordList::$wl";
                (my $modpm = "$mod.pm") =~ s!::!/!g;
                eval { require $modpm; 1 };
                if ($@) {
                    warn;
                    $i_wordlist++;
                    goto REDO;
                }
                $wl_obj = $mod->new;
                $wl_obj->reset_iterator;
            }
            my $word = $wl_obj->next_word;
            unless (defined $word) {
                undef $wl_obj;
                $i_wordlist++;
                goto REDO;
            }

            goto REDO if $num > 0 && $n >= $num;
            goto REDO if defined($args{len}) &&
                _length_in_graphemes($word) != $args{len};
            goto REDO if defined($args{min_len}) &&
                _length_in_graphemes($word) < $args{min_len};
            goto REDO if defined($args{max_len}) &&
                _length_in_graphemes($word) > $args{max_len};

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
        [200, "OK", $code_return_word, {stream=>1}];

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

1;
# ABSTRACT:

=head1 SYNOPSIS

See the included script L<wordlist>.


=head1 ENVIRONMENT

=head2 DEBUG => bool

=head2 COLOR => bool

Set color on/off when --color=auto (the default).


=head1 FAQ

=head2 How to make wordlist return words in random order?

The C<--random> (C<-r>) option was removed in v0.268. To return random words,
you can pipe the output of C<wordlist> to C<shuf> or other similar utility.


=head1 SEE ALSO

L<App::GamesWordlist> (L<games-wordlist>) which greps from
C<Games::Word::Wordlist::*> instead.

L<WordList> and C<WordList::*> modules.
