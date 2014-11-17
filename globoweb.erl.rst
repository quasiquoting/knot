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

This is the layout of the module and the app.src (which is a package
description).

.. code:: erlang
   :class: file:src/globoweb.erl

    -module(globoweb).
    -compile(export_all).

    -ifdef(TEST).
    -include_lib("eunit/include/eunit.hrl").
    -endif.

    __functions__

    -ifdef(TEST).
    __tests__
    -endif.
.. end code

.. code:: erlang
   :class: file:src/globoweb.app.src

    {application, globoweb,
     [
      {description, "A literate programming tool."},
      {vsn, "1"},
      {modules, [
                 globoweb
                ]},
      {registered, []},
      {applications, [
                      kernel,
                      stdlib
                     ]},
      {env, []}
     ]}.
.. end code


grab_until
----------

We'll have to divide input strings into blocks of content, so the first thing
we'll need is a way to collect a string up until a parameterized value.

.. code:: erlang
   :class: functions

    grab_until(Input, Sentry) ->
        grab_until(Input, Sentry, []).

    grab_until("", _Sentry, Acc) ->
        {lists:reverse(Acc), ""};
    grab_until(Input, Sentry, Acc) ->
        Sentry_length = string:len(Sentry),
        Input_window = string:substr(Input, 1, Sentry_length),
        case string:equal(Input_window, Sentry) of
            true ->
                {lists:reverse(Acc), string:substr(Input, Sentry_length + 1)};
            false ->
                [Char | Rest_of_input] = Input,
                grab_until(Rest_of_input, Sentry, [Char | Acc])
        end.
.. end code

.. code:: erlang
   :class: tests

    split_test() ->
        {"abcd", "efgh"} = grab_until("abcd<efgh", "<"),
        % Extract a documentation chunk.
        {"foobar\n", "tag>>=\ncode chunk\n>>bazbuzz"} = grab_until("foobar\n<<tag>>=\ncode chunk\n>>bazbuzz", "<<"),
        % Extract a code chunk.
        {"tag>>=\ncode chunk", "bazbuzz"} = grab_until("tag>>=\ncode chunk\n>>bazbuzz", "\n>>"),
        % Extract next documentation chunk.
        {"bazbuzz", ""} = grab_until("bazbuzz", "<<").
.. end code


The grab_until implementation takes an input string, and a sentry. It returns a
two-element tuple with the section of the input up until the sentry (or "" when
it's the end of the input) and the rest of the input after the sentry. The
sentry is consumed.


get_blocks
----------

A literate program is alternating documentation blocks and code blocks. Let us
convert the input into this most basic division.

.. code:: erlang
   :class: functions

    get_blocks(Input, Code_tag_start, Code_tag_end, Code_end) ->
        % Start off with a markup block.
        {Block, Rest} = get_markup_block(Input, Code_tag_start),
        get_blocks(Rest, Code_tag_start, Code_tag_end, Code_end, [Block]).


    get_blocks("", _Code_tag_start, _Code_tag_end, _Code_end, Acc) ->
        lists:reverse(Acc);
    get_blocks(Input, Code_tag_start, Code_tag_end, Code_end, [{markup, Markup_block} | Acc]) ->
        % The most recent block was a markup block, so get a code block.
        {Code_block, Rest} = get_code_block(Input, Code_tag_end, Code_end),
        get_blocks(Rest, Code_tag_start, Code_tag_end, Code_end, [Code_block, {markup, Markup_block} | Acc]);
    get_blocks(Input, Code_tag_start, Code_tag_end, Code_end, [{code, Code_tag, Code_block} | Acc]) ->
        % The most recent block was a code block, so get a markup block.
        {Markup_block, Rest} = get_markup_block(Input, Code_tag_start),
        get_blocks(Rest, Code_tag_start, Code_tag_end, Code_end, [Markup_block, {code, Code_tag, Code_block} | Acc]).


    get_markup_block(Input, Code_tag_start) ->
        {Block, Rest} = grab_until(Input, Code_tag_start),
        {{markup, Block}, Rest}.


    get_code_block(Input, Code_tag_end, Code_end) ->
        {Tag, Rest} = grab_until(Input, Code_tag_end),
        {Code, Rest1} = grab_until(Rest, Code_end),
        Trimmed_tag = re:replace(Tag, "^\\s+|\\s+$", "", [global, {return, list}]),
        {{code, Trimmed_tag, Code}, Rest1}.
.. end code

.. code:: erlang
   :class: tests

    get_markup_block_test() ->
        {{markup, "This document only has markup."}, ""} = get_markup_block("This document only has markup.", "<<"),
        {{markup, "This has a little more.\n"}, "tag>>=\ncode\n>>"} = get_markup_block("This has a little more.\n<<tag>>=\ncode\n>>", "<<"),
        {{markup, ""}, "tag>>=\ncode\n>>"} = get_markup_block("<<tag>>=\ncode\n>>", "<<").

    get_code_block_test() ->
        {{code, "tag", "only code"}, ""} = get_code_block("tag>>=\nonly code\n>>", ">>=\n", "\n>>"),
        {{code, "tag", "more code"}, "\nThat's some code."} = get_code_block("tag>>=\nmore code\n>>\nThat's some code.", ">>=\n", "\n>>").

    get_blocks_test() ->
        get_blocks("Goodbye, world.\n", "\n<<", ">>=\n", "\n>>\n", [{code,"mycode","print(\"Hello, world.\")"},{markup,"Hello, world."}]),
        Input = "Hello, world.\n"
                "<<mycode>>=\n"
                "print(\"Hello, world.\")\n"
                ">>\n"
                "Goodbye, world.\n",
        [{markup, "Hello, world."},
         {code, "mycode", "print(\"Hello, world.\")"},
         {markup, "Goodbye, world.\n"}] = get_blocks(Input, "\n<<", ">>=\n", "\n>>\n").
.. end code

The get_blocks implemenataion has two utility functions that grab markup and
code blocks. The initialization in get_blocks/3 puts a potential requirement on
the format of our literate documents -- they must start with a markup block.
However, I think this depends on what is used as a sentry for code blocks. This
might be crappy.

This function emits a list of {markup, Markup} and {code, Tag, Code}.

    - Markup = string()
    - Tag = string()
    - Code = string()


strip_markup
------------

In `Joe Armstrong's EWEB implementation`_, he goes through several passes over
the code blocks to provide various things (like line numbering). In it he
increments the atom for every pass over the code blocks (code1, code2, etc).
That way, if there's an error, it's easier to find the cause.

The whole point of my implementation is that the documentation or 'tangled'
file is the source file, so we won't actually need these markup blocks. The
code1 atom will signify stripped markup.

.. _Joe Armstrong's EWEB implementation: https://www.sics.se/~joe/ericsson/literate/literate.html

.. code:: erlang
   :class: functions

    strip_markup(Blocks) ->
        strip_markup(Blocks, []).

    strip_markup([], Acc) ->
        lists:reverse(Acc);

    strip_markup([{markup, _Text} | Rest], Acc) ->
        strip_markup(Rest, Acc);

    strip_markup([{code, Tag, Text} | Rest], Acc) ->
        strip_markup(Rest, [{code1, Tag, Text} | Acc]).
.. end code

.. code:: erlang
   :class: tests

    strip_markup_test() ->
        [{code1, "tag", "1"}, {code1, "tag", "2"}] = strip_markup([
            {markup, "a"},
            {code, "tag", "1"},
            {markup, "b"},
            {code, "tag", "2"}]).
.. end code


concat_code
-----------

Code with the same tag should be concatenated.

.. code:: erlang
   :class: functions

    concat_code(Blocks) ->
        concat_code(Blocks, #{}).

    concat_code([], Map) ->
        Map;

    concat_code([{code1, Tag, Code} | Rest], Map) ->
        case maps:is_key(Tag, Map) of
            true ->
                concat_code(Rest, maps:update(Tag, string:join([maps:get(Tag, Map), Code], "\n"), Map));
            _ ->
                concat_code(Rest, maps:put(Tag, Code, Map))
        end.
.. end code

.. code:: erlang
   :class: tests

    concat_code_test() ->
        #{"a" := "one\nthree",
          "b" := "two"} = concat_code([{code1, "a", "one"}, {code1, "b", "two"}, {code1, "a", "three"}]).
.. end code

Wait, but if we're now dealing with a map, are we losing the benefit of atoms
to signify a place in the code where an error is thrown? I feel like I might
get bogged down with this question. I'll just keep going until I understand the
problem.


Macro Expansion
---------------

Now I'll try and expand these macros. In the other literate programming tools
I've looked at, macros must be on their own line. They can be preceded by any
amount of white space and when the macro is expanded, all the lines are
preceded by that white space. I want to do something a little cooler. I would
like this::

    <<some file>>=
    This is HTML and here is a list.
    <ul>
        <li><<list elements>></li>
    </ul>
    >>

    <<list elements>>=
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

.. code:: erlang
   :class: functions

    grab_macro(Line, Macro_start, Macro_end) ->
        {Possible_prefix, Rest} = grab_until(Line, Macro_start),
        case Rest of
            "" -> none;
            _ ->
                Prefix = Possible_prefix,
                {Macro_name, Suffix} = grab_until(Rest, Macro_end),
                {Macro_name, Prefix, Suffix}
        end.
.. end code

.. code:: erlang
   :class: tests

    grab_macro_test() ->
        none = grab_macro("Nothing doing.", "<<", ">>"),
        {"foobar", "    ", ""} = grab_macro("    <<foobar>>", "<<", ">>"),
        {"list elements", "    <li>", "</li>"} = grab_macro("    <li><<list elements>></li>", "<<", ">>").
.. end code

Given a line, grab_macro will either return the atom none or a three-tuple with
the macro name, line prefix, and line suffix.

.. code:: erlang
   :class: functions

    expand_macros(Block, Macros, Macro_start, Macro_end) ->
        expand_macros(re:split(Block, "\n", [{return, list}]), Macros, Macro_start, Macro_end, []).

    expand_macros([], _Macros, _Macro_start, _Macro_end, Acc) ->
        string:join(lists:reverse(Acc), "\n");

    expand_macros([Line | Rest], Macros, Macro_start, Macro_end, Acc) ->
        case grab_macro(Line, Macro_start, Macro_end) of
            none ->
                expand_macros(Rest, Macros, Macro_start, Macro_end, [Line | Acc]);
            {Name, Prefix, Suffix} ->
                Macro_value = maps:get(Name, Macros),
                Macro_lines = re:split(Macro_value, "\n", [{return, list}]),
                Expanded_lines = Prefix ++ string:join(Macro_lines, Suffix ++ "\n" ++ Prefix) ++ Suffix,
                expand_macros(Rest, Macros, Macro_start, Macro_end, [Expanded_lines | Acc])
        end.
.. end code

.. code:: erlang
   :class: tests

    expand_macros_test() ->
        Macros = #{"list elements" => "one\ntwo\nthree",
                   "parent" => "This is HTML and here is a list.\n"
                               "<ul>\n"
                               "    <li><<list elements>></li>\n"
                               "</ul>"},
        Expected_output = "This is HTML and here is a list.\n"
                          "<ul>\n"
                          "    <li>one</li>\n"
                          "    <li>two</li>\n"
                          "    <li>three</li>\n"
                          "</ul>",
        Expected_output = expand_macros(maps:get("parent", Macros), Macros, "<<", ">>").
.. end code

The expand_macros function takes a block and replaces macro instances with the
given map (which is a map of all the blocks) and returns the new block value.

.. CAUTION::
   This function doesn't currently escape macros. If the macro delimeters are
   << and >>, then they will conflict with Erlang binaries! I must fix that,
   but I'm not sure how yet. One option is to pick delimeters that aren't used
   in any languages in the source document. I think that's a cop out. I'll come
   back to this later. (TODO)

.. code:: erlang
   :class: functions

    expand_blocks(Blocks, Macro_start, Macro_end) ->
        expand_blocks(maps:to_list(Blocks), Blocks, Macro_start, Macro_end, #{}).

    expand_blocks([], _Macros, _Macro_start, _Macro_end, Output_blocks) ->
        Output_blocks;

    expand_blocks([{Name, Block} | Rest], Macros, Macro_start, Macro_end, Output_blocks) ->
        New_block = expand_macros(Block, Macros, Macro_start, Macro_end),
        expand_blocks(Rest, Macros, Macro_start, Macro_end, maps:put(Name, New_block, Output_blocks)).
.. end code

.. code:: erlang
   :class: tests

    expand_blocks_test() ->
        Blocks = #{"A" => "b:\n"
                          "  <<B>>",
                   "B" => "b\nb",
                   "C" => "d:\n"
                          "  <<D>>",
                   "D" => "d\nd",
                   "ALL" => "<<A>>\n"
                            "    || <<C>> ||\n"},

        Expected_output = #{"A" => "b:\n"
                                   "  b\n"
                                   "  b",
                            "B" => "b\nb",
                            "C" => "d:\n"
                                   "  d\n"
                                   "  d",
                            "D" => "d\nd",
                            "ALL" => "b:\n"
                                     "  b\n"
                                     "  b\n"
                                     "    || d: ||\n"
                                     "    ||   d ||\n"
                                     "    ||   d ||\n"},

        Expected_output = expand_blocks(expand_blocks(Blocks, "<<", ">>"), "<<", ">>").
.. end code

Expand blocks runs expand_macros on every block. In the test it requires two
passes because A and C are nested in ALL. At some point I'll have to decide how
many passes to execute. That depends on expected user behavior, but, I suspect
I can just do it a bunch of times.

.. CAUTION::
   There's some problems.

   1. I think I'm starting to confuse the terms 'block' and 'macro'.
   2. Each level has to buy into Macro_start, Macro_end, etc. I think I need to
      define a data structure to represent the parser config. It might also be
      good to have another structure for the parser state -- like the lines
      traversed, the current column. Possibly other things I can't think of
      right now.
   3. I really don't like how I went from three-tuples to maps. I think I
      should use one or the other.


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
.. end code

.. code:: erlang
   :class: tests

    get_output_files_test() ->
        [] = get_output_files(#{"A" => "a", "B" => "b"}),
        [{"globoweb.erl", "TODO"}] = get_output_files(#{"A" => "a", "file:globoweb.erl" => "TODO", "B" => "b"}),
        Files = get_output_files(#{"file:globoweb.erl" => "TODO", "file:src/globoweb.erl" => "TODO", "file:../../why.txt" => "?"}),
        "TODO" = proplists:get_value("globoweb.erl", Files),
        "TODO" = proplists:get_value("src/globoweb.erl", Files),
        "?" = proplists:get_value("../../why.txt", Files).
.. end code


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
.. end code

.. code:: erlang
   :class: tests

    file_name_test() ->
        "test_files/foobar.txt" = file_name("test_files", "foobar.txt"),
        "/path/to/repository/src/globoweb.erl" = file_name("/path/to/repository", "src/globoweb.erl").
.. end code

The file_name function will just concatenate paths.

.. code:: erlang
   :class: functions

    write_file(Base_directory, File_name, Contents) ->
        Fn = file_name(Base_directory, File_name),
        ok = file:write_file(Fn, Contents),
        Fn.
.. end code
.. code:: erlang
   :class: tests

    write_file_test() ->
        "test_files/test.txt" = write_file("test_files", "test.txt", "write_file_test\n"),
        {ok, <<"write_file_test\n">>} = file:read_file(file_name("test_files", "test.txt")),
        file:delete(file_name("test_files", "test.txt")).
.. end code

The write_file function just wraps out file naming requirements around
``file:write_file``. The test for this function basically tests that the Erlang
file module works, which is a stupid thing to do. I just wanted to make sure I
understood it -- it's a test for me, not it.

.. code:: erlang
   :class: functions

    read_file(File_name) ->
        {ok, Binary} = file:read_file(File_name),
        binary_to_list(Binary).
.. end code

.. code:: erlang
   :class: tests

    read_file_test() ->
        "test_files/read_file_test.txt" = write_file("test_files", "read_file_test.txt", "read_file_test\n"),
        Fn = file_name("test_files", "read_file_test.txt"),
        "read_file_test\n" = read_file(Fn),
        file:delete(Fn).
.. end code

We're working with lists, not binaries, so read_file just indicates that.

.. code:: erlang
   :class: functions

    process_file(File_name, Code_tag_start, Code_tag_end, Code_end, Macro_start, Macro_end) ->
        Contents = read_file(File_name),
        Blocks = get_blocks(Contents, Code_tag_start, Code_tag_end, Code_end),
        Code_blocks = strip_markup(Blocks),
        Macros = concat_code(Code_blocks),
        % Definitely confusing macros and blocks.
        Expanded_macros = lists:foldl(fun (_, M) -> expand_blocks(M, Macro_start, Macro_end) end,
                                      Macros,
                                      lists:seq(1, 4)),
        Output_files = get_output_files(Expanded_macros),
        % If the file is in the current directory it may not have a slash. If it
        % doesn't filename:basename returns the file name and we want it to be ".".
        Base_directory = filename:dirname(File_name),
        lists:map(fun ({Output_file_name, File_contents}) ->
                      write_file(Base_directory, Output_file_name, File_contents)
                  end,
                  Output_files).
.. end code

.. code:: erlang
   :class: tests

    process_file_test() ->
        Output_file = "test_files/process_file_test.js",
        [Output_file] = process_file("test_files/process_file_test.lit.txt",
                                              "\n<<", ">>=\n", "\n>>", "<<", ">>"),
        Expected_output = read_file("test_files/process_file_test.js.expected_output"),
        Actual_output = read_file(Output_file),

        Expected_output = Actual_output ++ "\n",
        file:delete(Output_file).
.. end code


Given an input file, process_file will write the contents out to the file
indicated in every tag that starts with "file:". It returns a list of the files
written.

The actual output of the script has a couple things that annoy me. The first is
that the line prefix is applied to empty lines (i.e, "    \n"). The second is
that it doesn't end with a line break but vim puts one in my test file.  It's
probably find to just ignore this or I could also add a line break at the end
of all files. I'm not sure what I'll do.

.. code:: erlang
   :class: functions

    process_files(Files, Code_tag_start, Code_tag_end, Code_end, Macro_start, Macro_end) ->
        process_files(Files, Code_tag_start, Code_tag_end, Code_end, Macro_start, Macro_end, []).

    process_files([], _Code_tag_start, _Code_tag_end, _Code_end, _Macro_start, _Macro_end, Acc) ->
        lists:reverse(Acc);

    process_files([File | Rest], Code_tag_start, Code_tag_end, Code_end, Macro_start, Macro_end, Acc) ->
        process_files(Rest, Code_tag_start, Code_tag_end, Code_end, Macro_start, Macro_end, [process_file(File, Code_tag_start, Code_tag_end, Code_end, Macro_start, Macro_end) | Acc]).
.. end code

.. code:: erlang
   :class: tests

    process_files_test() ->
        Output_files = [["test_files/process_files_1_a.txt", "test_files/process_files_1_b.txt"],
                        ["test_files/process_files_2_a.txt", "test_files/process_files_2_b.txt"],
                        ["test_files/process_files_3_a.txt", "test_files/process_files_3_b.txt"]],
        Output_files = process_files(["test_files/process_files_1.lit.txt",
                                      "test_files/process_files_2.lit.txt",
                                      "test_files/process_files_3.lit.txt"],
                                     "\n<<", ">>=\n", "\n>>", "<<", ">>"),
        lists:foreach(fun (Files) ->
                          lists:foreach(fun (File) -> file:delete(File) end, Files)
                      end,
                      Output_files).
.. end code


process_files run process_file on multiple files. Now we just need to start this thing.


.. code:: erlang
   :class: functions

    start(Files) ->
        Output = process_files(Files, "\n   :class: ", "\n", "\n.. end code", "::", "::"),
        lists:foreach(fun (Some_files) ->
                          lists:foreach(fun (File) ->
                                            io:format("~s written.~n", [File])
                                        end,
                                        Some_files)
                      end,
                      Output).
.. end code


When we self-host, I want to do it in reStructuredText. For now we'll just hard
code the sentries to our reStructuredText usage.

.. code:: erlang
   :class: tests

    process_rst_test() ->
        Output_files = process_file("test_files/test.lit.rst",
                                    "\n   :class: ", "\n", "\n.. end code", "::", "::"),
        lists:foreach(fun (File) ->
                          io:format("Deleting: ~p~n", [File]),
                          file:delete(File)
                      end,
                      Output_files).
.. end code
