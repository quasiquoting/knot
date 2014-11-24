========
Globoweb
========

Globoweb is a literate programming tool. You may wish to take a look at `other
tools`_; `nuweb`_ and `noweb`_ are particularly significant. There is also
`lit`_, which uses Markdown for its syntax.

Globoweb only tangles; it does not weave.

Rationale
=========

I think literate programming is awesome, but I don't like that it's LaTeX-based
because I'm going to publish this stuff on the web and the LaTeX document has
to go through too many transformations to get to HTML. Also, I would like to
customize the output more. Also, maybe I want to use one of the markup
languages supported by GitHub's READMEs. This is an attempt at a literate
programming tool that is both markup and code agnostic.

After reading `Joe Armstrong's literate program`_, I decided that I could
tackle this task -- a strong recommendation for literate programming!

.. _other tools: https://en.wikipedia.org/wiki/Literate_programming#Tools
.. _nuweb: http://nuweb.sourceforge.net/
.. _noweb: http://www.cs.tufts.edu/~nr/noweb/
.. _lit: https://github.com/cdosborn/lit
.. _Joe Armstrong's literate program: https://www.sics.se/~joe/ericsson/literate/literate.html


The Program
===========

This is the layout of the module.

.. code:: erlang
   :class: file:src/globoweb.erl

    -module(globoweb).
    -compile(export_all).
    -ifdef(TEST).
    -include_lib("eunit/include/eunit.hrl").
    -endif.

    <<functions>>

    -ifdef(TEST).
    <<tests>>
    -endif.


Finding Code Blocks
-------------------

The parse function will be the entry point to the parser. It accepts the parser
config. The parser config is a map with two keys: ``code_start`` and
``code_end``.

code_start
    This is regex that contains two capture groups ``consume`` and ``tag``. The
    ``consume`` group will be removed before the code block is parsed. The
    ``tag`` group captures the name of the macro.

code_end
    When collecting the code block, the parser checks for ``code_end``. The
    ``consume`` group is removed and the parser starts looking for the next
    code block with ``code_start``.

When ``parse`` finds a code block, it delegates to ``parse_code`` and throws
away all markup blocks.

.. code:: erlang
   :class: functions

    parse(Document, Config) ->
        parse(Document, Config, []).

    parse("", _Config, Acc) ->
        lists:reverse(Acc);

    parse(Input, #{code_start := Start} = Config, Acc) ->
        case re:run(Input, Start, [{capture, [consume, tag], list}]) of
            nomatch ->
                [_Char | Rest] = Input,
                parse(Rest, Config, Acc);
            {match, [Consume, Tag]} ->
                Consumed_input = re:replace(Input, Consume, "", [{return, list}]),
                {Code, Rest} = parse_code(Consumed_input, Config),
                parse(Rest, Config, [{Tag, Code} | Acc])
        end.


When a code block is found, ``parse_code`` grabs returns section of the
document by checking for the ``code_end``.

.. code:: erlang
   :class: functions

    parse_code(Input, Config) ->
        parse_code(Input, Config, []).

    parse_code("", _Config, Acc) ->
        {lists:reverse(Acc), ""};

    parse_code(Input, #{code_end := End} = Config, Acc) ->
        case re:run(Input, End) of
            nomatch ->
                [Char | Rest] = Input,
                parse_code(Rest, Config, [Char | Acc]);
            {match, _} ->
                {lists:reverse(Acc), Input}
        end.

    parse_test() ->
        Document = "This is some documentation.\n"
                   "\n"
                   ".. code:: erlang\n"
                   "   :class: file:test_files/test_document.erl\n"
                   "\n"
                   "    This is code.\n"
                   "\n"
                   "    More code.\n"
                   "\n"
                   "\n"
                   "More documentation.\n",

        Parser_config = #{code_start => "^(?<consume>\n.. code::( | [^\n]+)?\n   :class: (?<tag>[^\n]+)\n)",
                          code_end => "^(?<consume>\n)[\\S]"},

        [{"file:test_files/test_document.erl", "\n    This is code.\n\n    More code.\n\n"}] = parse(Document, Parser_config),


        Larger_document = string:join([Document, Document], "\n"),
        Expected_output = [
            {"file:test_files/test_document.erl", "\n    This is code.\n\n    More code.\n\n"},
            {"file:test_files/test_document.erl", "\n    This is code.\n\n    More code.\n\n"}
        ],
        Expected_output = parse(Larger_document, Parser_config).


Indentation
-----------

The code blocks might contain indentation. With Markdown and reStructuredText,
indentation is required, but because we will maintain the indentation in the
macro expansion it's safe to always strip the leading white space.

The ``find_indentation`` function returns the leading white space from the
first line with non-white space on it.

.. code:: erlang
   :class: functions

    find_indentation([]) -> "";

    find_indentation([Line | Lines]) ->
        case re:run(Line, "\\S") of
            {match, _} ->
                {match, [Indentation]} = re:run(Line, "^(?<indentation>[\s]*)", [{capture, [indentation], list}]),
                Indentation;
            _ ->
                find_indentation(Lines)
        end.

.. code:: erlang
   :class: tests

    find_indentation_test() ->
        "    " = find_indentation(["", "  \t", "    ", "    foo() -> ok.", "\t\tbar() -> ok."]),
        "" = find_indentation(["", "", "foo() -> ok.", "", "bar() -> ok."]).


Now we need to trim the indentation by creating a regex pattern with ``^`` and
the indentation returned from ``find_indentation`` and applying it to every
line.

.. code:: erlang
   :class: functions

    trim_indentation(Lines, Indentation) ->
        Regex = string:join(["^", Indentation], ""),
        trim_indentation(Lines, Regex, []).

    trim_indentation([], _Regex, Acc) ->
        lists:reverse(Acc);

    trim_indentation([Line | Rest], Regex, Acc) ->
        trim_indentation(Rest, Regex, [re:replace(Line, Regex, "", [{return, list}]) | Acc]).

.. code:: erlang
   :class: tests

    trim_indentation_test() ->
        Input = [" ", "\t   ", "    foo() -> ok.", "    ", "    bar() -> ok."],
        % Leaves inconsisten input alone.
        Expected = [" ", "\t   ", "foo() -> ok.", "", "bar() -> ok."],
        Expected = trim_indentation(Input, "    ").


And we'll need to apply it to every line in code blocks.

.. code:: erlang
   :class: functions

    trim_code(Code) ->
        Lines = re:split(Code, "\n", [{return, list}]),
        Indentation = find_indentation(Lines),
        New_lines = trim_indentation(Lines, Indentation),
        string:join(New_lines, "\n").

.. code:: erlang
   :class: tests

    trim_code_test() ->
        Input = "\n"
                "\n"
                "    foo() ->\n"
                "        ok.\n"
                "\n"
                "    bar() -> ok.\n"
                "\n",
        Expected = "\n"
                   "\n"
                   "foo() ->\n"
                   "    ok.\n"
                   "\n"
                   "bar() -> ok.\n"
                   "\n",

        Expected = trim_code(Input).


Concatenation of Macros
-----------------------

Now we've got a list of ``{Tag, Code}``. Some of the tags will be duplicated.
Those should be concatenated. After this function exits, we'll be working with
a map of the code that will be used in macro expansion.


.. code:: erlang
   :class: functions

    concat_code(Blocks) ->
        concat_code(Blocks, #{}).

    concat_code([], Map) ->
        Map;

    concat_code([{Tag, Code} | Rest], Map) ->
        case maps:is_key(Tag, Map) of
            true ->
                concat_code(Rest, maps:update(Tag, string:join([maps:get(Tag, Map), Code], "\n"), Map));
            _ ->
                concat_code(Rest, maps:put(Tag, Code, Map))
        end.

.. code:: erlang
   :class: tests

    concat_code_test() ->
        Input = [{"a", "one"}, {"b", "two"}, {"a", "three"}],
        Expected = #{"a" => "one\nthree",
                     "b" => "two"},
        Expected = concat_code(Input).


Macro Expansion
---------------

The code blocks may contain macros. We need to expand them. Macro's can
reference other, unevaluated macros, so I'm just going to expand them multiple
times. This creates a problem when the macros collide with language syntax. For
instance, macros denoted as ``<<macro>>`` are typical, but Erlang's bit syntax
is also denoted by ``<<`` and ``>>``. So, after we expand all the macros we'll
do a final pass and unescape.

This means that we'll augment the parser config with

macro
    A pattern that must have a capture group for the macro ``name``. For
    example: ``^<<(?<name>[^>]+)>>``.

In the other literate programming tools I've looked at, macros must be on their
own line. They can be preceded by any amount of white space and when the macro
is expanded, all the lines are preceded by that white space. I want to do
something a little cooler. I would like this::

    <<some file>>=
    This is HTML and here is a list.
    <ul>
        <li><_<list elements>></li>
    </ul>
    >>

    <_<list elements>>=
    one
    two
    three
    >>

...to expand to::

    This is HTML and here is a list.
    <ul>
        <li>one</li>
        <li>two</li>
        <li>three</li>
    </ul>

The only difference is that we need to wrap the macro lines in the preceding
and trailing white space.

I think the easiest way to do that is to have a function that checks a line for
the macro pattern and returns either ``none`` or a ``{Tag_name, Line_prefix,
Line_suffix}``.

.. code:: erlang
   :class: functions

    find_macro(Line, #{macro := Pattern} = _Config) ->
        find_macro(Line, Pattern, "").

    find_macro("", _Pattern, _Acc) ->
        % No macro was found.
        none;

    find_macro(Line, Pattern, Acc) ->
        case re:run(Line, Pattern, [{capture, all, list}]) of
            nomatch ->
                [Char | Rest] = Line,
                find_macro(Rest, Pattern, [Char | Acc]);

            {match, [Entire_match, Name]} ->
                Prefix = lists:reverse(Acc),
                Suffix = string:substr(Line, string:len(Entire_match) + 1),
                {Name, Prefix, Suffix}
        end.

.. code:: erlang
   :class: tests

    find_macro_test() ->
        Input = "    <li><_<list elements>></li>",
        Expected = {"list elements", "    <li>", "</li>"},
        Expected = find_macro(Input, #{macro => "^<_<(?<name>[^>]+)>>"}).

Now we need to expand all the macros in a block using ``find_macro``.

.. code:: erlang
   :class: functions

    expand_macros(Block, Macros, Config) ->
        string:join(expand_macros(re:split(Block, "\n", [{return, list}]), Macros, Config, []),
                    "\n").

    expand_macros([], _Macros, _Config, Acc) ->
        lists:reverse(Acc);

    expand_macros([Line | Rest], Macros, Config, Acc) ->
        case find_macro(Line, Config) of
            none ->
                expand_macros(Rest, Macros, Config, [Line | Acc]);

            {Name, Prefix, Suffix} ->
                Value = maps:get(Name, Macros),
                Value_lines = re:split(Value, "\n", [{return, list}]),
                Expanded_lines = lists:map(fun (L) -> Prefix ++ L ++ Suffix end, Value_lines),
                New_acc = lists:foldl(fun (X, List) -> [X | List] end, Acc, Expanded_lines),
                expand_macros(Rest, Macros, Config, New_acc)
        end.

.. code:: erlang
   :class: tests

    expand_macros_test() ->
        Input = "<ul>\n"
                "    <li><_<items>></li>\n"
                "</ul>",
        Expected = "<ul>\n"
                   "    <li>one</li>\n"
                   "    <li>two</li>\n"
                   "    <li>three</li>\n"
                   "</ul>",
        Macros = #{"items" => "one\ntwo\nthree"},
        Config = #{macro => "^<_<(?<name>[^>]+)>>"},

        Expected = expand_macros(Input, Macros, Config).


Now, since we can expand the macros in one block, we only need to do it for all
blocks.

Expand blocks runs expand_macros on every block. In the test it requires two
passes because A and C are nested in ALL. At some point I'll have to decide how
many passes to execute. That depends on expected user behavior, but, I suspect
I can just do it four times, chosen arbitrarily.

.. code:: erlang
   :class: functions

    expand_all_blocks(Macros, Config) ->
        maps:map(fun (_K, V) -> expand_macros(V, Macros, Config) end, Macros).

.. code:: erlang
   :class: tests

    expand_all_blocks_test() ->
        Config = #{macro => "^<_<(?<name>[^>]+)>>"},

        Input = #{"A" => "a:\n"
                         "  <_<B>>",
                  "B" => "b\nb",
                  "C" => "c:\n"
                         "  <_<D>>",
                  "D" => "d\nd",
                  "ALL" => "<_<A>>\n"
                           "    || <_<C>> ||\n"},

        Expected = #{"A" => "a:\n"
                            "  b\n"
                            "  b",
                     "B" => "b\nb",
                     "C" => "c:\n"
                            "  d\n"
                            "  d",
                     "D" => "d\nd",
                     "ALL" => "a:\n"
                              "  b\n"
                              "  b\n"
                              "    || c: ||\n"
                              "    ||   d ||\n"
                              "    ||   d ||\n"},

        Expected = expand_all_blocks(expand_all_blocks(Input, Config), Config).

After expanding the macros, we'll need to unescape escaped macros augmenting
the parser config with an ``escaped_macro``.

escaped_macro
    This must be chosen carefully. Since the parser scans over the input one
    character at a time, ``\\<<`` is a bad choice with the example macro
    pattern above.  A better choice would be ``<\\<`` but on the other hand,
    you could make ``macro`` be ``[^\\]?<<(?<name>[^>]+)>>``. Anyway -- it
    takes consideration. Especially as regards syntax highlighting.

    The full matched string will be replaced by a concatenation of the groups
    in the pattern. I'm not sure this is nice at all, but consider the
    following.

.. code:: erlang
   :class: tests

    escaped_macro_example_test() ->
        Input = "<_<notamacro>> ...",
        Pattern = "(<)\\|(<[^>]+>>)",
        Expected = "<_<notamacro>> ...",

        Expected = unescape(Input, #{escaped_macro => Pattern}).

The two groups in ``Pattern`` are concatenated together to yield
``<<notamacro>>``.


.. code:: erlang
   :class: functions

    unescape(Line, Config) ->
        unescape(Line, Config, "").

    unescape("", _Config, Acc) ->
        lists:reverse(Acc);

    unescape(Line, #{escaped_macro := Pattern} = Config, Acc) ->
        case re:run(Line, Pattern, [{capture, all}]) of
            nomatch ->
                [Char | Rest] = Line,
                unescape(Rest, Config, [Char | Acc]);

            {match, [{0, Match_end} | Groups]} ->
                Replacement_string = lists:foldl(
                    fun (X, A) ->
                        lists:concat([A, substring(Line, X)])
                    end,
                    "",
                    Groups),
                Rest_of_line = string:substr(Line, Match_end + 1),
                unescape(Rest_of_line, Config, lists:concat([lists:reverse(Replacement_string), Acc]))
        end.

    substring(String, {Start, Length}) ->
        string:substr(String, Start + 1, Length).

    lines(String) ->
        re:split(String, "\n", [{return, list}]).

    map_lines(Fun, String) ->
        Altered_lines = lists:map(Fun, lines(String)),
        string:join(Altered_lines, "\n").

.. code:: erlang
   :class: tests

    substring_test() ->
        "123" = substring("12345", {0, 3}).

    lines_test() ->
        ["foo", "bar", "baz"] = lines("foo\nbar\nbaz").

    map_lines_test() ->
        "foo\nbar\nbaz\nbuzz" = map_lines(fun (X) -> X end, "foo\nbar\nbaz\nbuzz").

    unescape_test_again() ->
        Config = #{escaped_macro => "^(<)\\(<([^>]+>>)"},
        "    No macro." = unescape("    No macro.", Config),
        "    <_<notamacro>>" = unescape("    <\\<notamacro>>", Config).


This will unescape the ``escaped_macro`` (TODO: which I now realize is badly
named).

The ``substring`` function takes the range output of ``re:run`` to grab that
segment of the string.

The ``lines`` function is probably something I should be using all over the
place, it being a utility function. I'll have to refactor this program again,
of course; same with ``map_lines``.


Writing Files
-------------

The software must perform work. This is how it outputs files.

.. code:: erlang
   :class: functions

    get_output_files(Blocks) ->
        get_output_files(maps:to_list(Blocks), []).

    get_output_files([], Acc) ->
        lists:reverse(Acc);

    get_output_files([{Tag, Block} | Rest], Acc) ->
        case Tag of
            [$f, $i, $l, $e, $: | File_name] ->
                get_output_files(Rest, [{File_name, Block} | Acc]);
            _ ->
                get_output_files(Rest, Acc)
        end.

.. code:: erlang
   :class: tests

    get_output_files_test() ->
        [] = get_output_files(#{"A" => "a", "B" => "b"}),
        [{"globoweb.erl", "TODO"}] = get_output_files(#{"A" => "a", "file:globoweb.erl" => "TODO", "B" => "b"}),
        Files = get_output_files(#{"file:globoweb.erl" => "TODO", "file:src/globoweb.erl" => "TODO", "file:../../why.txt" => "?"}),
        "TODO" = proplists:get_value("globoweb.erl", Files),
        "TODO" = proplists:get_value("src/globoweb.erl", Files),
        "?" = proplists:get_value("../../why.txt", Files).


The get_output_files checks that map I've been calling 'blocks' or 'macros' for
tags that start with "file:". Since we're only concerned with the output of
files, no other blocks are returned. They're probably nested in one of the
output file blocks.

Also, it returns a proplist which means we went from a three-tuple to a map to
two-tuples. I'm kind of annoyed with myself but I still think it's best to keep
going. I'm almost ready to complete a first pass at this program and then it
can self-host.

.. code:: erlang
   :class: functions

    file_name(Base_directory, File_name) ->
        filename:nativename(filename:absname_join(Base_directory, File_name)).

.. code:: erlang
   :class: tests

    file_name_test() ->
        "test_files/foobar.txt" = file_name("test_files", "foobar.txt"),
        "/path/to/repository/src/globoweb.erl" = file_name("/path/to/repository", "src/globoweb.erl").

The file_name function will just concatenate paths.

.. code:: erlang
   :class: functions

    write_file(Base_directory, File_name, Contents) ->
        Fn = file_name(Base_directory, File_name),
        ok = file:write_file(Fn, Contents),
        Fn.

.. code:: erlang
   :class: tests

    write_file_test() ->
        "test_files/test.txt" = write_file("test_files", "test.txt", "write_file_test\n"),
        {ok, <_<"write_file_test\n">>} = file:read_file(file_name("test_files", "test.txt")),
        file:delete(file_name("test_files", "test.txt")).

The write_file function just wraps out file naming requirements around
``file:write_file``. The test for this function basically tests that the Erlang
file module works, which is a stupid thing to do. I just wanted to make sure I
understood it -- it's a test for me, not it.

.. code:: erlang
   :class: functions

    read_file(File_name) ->
        {ok, Binary} = file:read_file(File_name),
        binary_to_list(Binary).

.. code:: erlang
   :class: tests

    read_file_test() ->
        "test_files/read_file_test.txt" = write_file("test_files", "read_file_test.txt", "read_file_test\n"),
        Fn = file_name("test_files", "read_file_test.txt"),
        "read_file_test\n" = read_file(Fn),
        file:delete(Fn).

We're working with lists, not binaries, so read_file just indicates that.


Putting it all Together
-----------------------

Given an input file, ``process_file`` will write the contents out to the file
indicated in every tag that starts with "file:". It returns a list of the files
written.

The actual output of the script has a couple things that annoy me. The first is
that the line prefix is applied to empty lines (i.e, "    \n"). The second is
that it doesn't end with a line break but vim puts one in my test file.  It's
probably find to just ignore this or I could also add a line break at the end
of all files. I'm not sure what I'll do.

.. code:: erlang
   :class: functions

    process_file(File_name, Config) ->
        Contents = read_file(File_name),
        Blocks = parse(Contents, Config),
        Unindented_blocks = lists:map(fun ({Tag, Code}) -> {Tag, trim_code(Code)} end, Blocks),
        Macros = concat_code(Unindented_blocks),
        Expanded_macros = expand_all_blocks(Macros, Config),
        Unescaped_macros = maps:map(fun (_K, V) ->
                                        map_lines(fun (Line) ->
                                                      unescape(Line, Config)
                                                  end,
                                                  V)
                                    end,
                                    Expanded_macros),
        Output_files = get_output_files(Unescaped_macros),
        Base_directory = filename:dirname(File_name),
        lists:map(fun ({Output_file_name, File_contents}) ->
                      write_file(Base_directory, Output_file_name, File_contents)
                  end,
                  Output_files).

.. code:: erlang
   :class: tests

    process_file_test() ->
        Config = #{code_start => "^(?<consume><_<(?<tag>[^>]+)>>=\n)",
                   code_end => "^\n>>",
                   macro => "^<_<(?<name>[^>]+)>>",
                   escaped_macro => "^(<)\\\\\\\\(<)"},

        Output_file = "test_files/process_file_test.js",

        [Output_file] = process_file("test_files/process_file_test.lit.txt", Config),
        Expected_output = read_file("test_files/process_file_test.js.expected_output"),
        Actual_output = read_file(Output_file),

        Expected_output = Actual_output ++ "\n",
        file:delete(Output_file).

We'll need to process multiple files.

.. code:: erlang
   :class: functions

    process_files(Files, Config) ->
        process_files(Files, Config, []).

    process_files([], _Config, Acc) ->
        lists:reverse(Acc);

    process_files([File | Rest], Config, Acc) ->
        process_files(Rest, Config, [process_file(File, Config) | Acc]).

.. code:: erlang
   :class: tests

    process_files_test() ->
        Config = #{code_start => "^(?<consume>\n<_<(?<tag>[^>]+)>>=\n)",
                   code_end => "^(?<consume>^\n>>)\n",
                   macro => "^<_<(?<name>[^>]+)>>",
                   escaped_macro => "^(<)|(<)"},

        Expected = [["test_files/process_files_1_a.txt", "test_files/process_files_1_b.txt"],
                    ["test_files/process_files_2_a.txt", "test_files/process_files_2_b.txt"],
                    ["test_files/process_files_3_a.txt", "test_files/process_files_3_b.txt"]],

        Expected = process_files(["test_files/process_files_1.lit.txt",
                                  "test_files/process_files_2.lit.txt",
                                  "test_files/process_files_3.lit.txt"],
                                 Config),
        lists:foreach(fun (Files) ->
                          lists:foreach(fun (File) -> file:delete(File) end, Files)
                      end,
                      Expected).


The End
-------

.. code:: erlang
   :class: functions

    start(Files) ->
        Config = #{code_end => "^\n[\\S]",
                   code_start => "^(?<consume>\n.. code::.*\n   :class: (?<tag>.+)\n\n)",
                   macro => "^<<(?<name>[^>]+)>>",
                   escaped_macro => "^(<)_(<)"},
        Output = process_files(Files, Config),
        lists:foreach(fun (Some_files) ->
                          lists:foreach(fun (File) ->
                                            io:format("~s written.~n", [File])
                                        end,
                                        Some_files)
                      end,
                      Output).

When we self-host, I want to do it in reStructuredText. For now we'll just hard
code the config to our reStructuredText usage.

.. code:: erlang
   :class: tests

    process_rst_test() ->
        Config = #{code_end => "^\n[\\S]",
                   code_start => "^(?<consume>\n.. code::.*\n   :class: (?<tag>.+)\n\n)",
                   macro => "^<<(?<name>[^>]+)>>",
                   escaped_macro => "^(<)_(<)"},

        Output_files = process_file("test_files/test.lit.rst", Config),

        lists:foreach(fun (File) ->
                          io:format("Deleting: ~p~n", [File]),
                          file:delete(File)
                      end,
                      Output_files).
