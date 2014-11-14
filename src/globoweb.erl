-module(globoweb).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").


% I think literate programming is awesome, but I don't like that it's
% LaTeX-based because I'm going to publish this stuff on the web and the LaTeX
% document has to go through too many transformations to get to HTML. Also, I
% would like to customize the output more. Also, maybe I want to use one of the
% markup languages supported by GitHub's READMEs. This is an attempt at a
% literate programming tool that is both markup and code agnostic.


% We'll have to divide input strings into blocks of content, so the first thing
% we'll need is a way to collect a string up until a parameterized value.


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

-ifdef(TEST).
split_test() ->
    {"abcd", "efgh"} = grab_until("abcd<efgh", "<"),
    % Extract a documentation chunk.
    {"foobar\n", "tag>>=\ncode chunk\n>>bazbuzz"} = grab_until("foobar\n<<tag>>=\ncode chunk\n>>bazbuzz", "<<"),
    % Extract a code chunk.
    {"tag>>=\ncode chunk", "bazbuzz"} = grab_until("tag>>=\ncode chunk\n>>bazbuzz", "\n>>"),
    % Extract next documentation chunk.
    {"bazbuzz", ""} = grab_until("bazbuzz", "<<").
-endif.


% The grab_until implementation takes an input string, and a sentry. It returns
% a two-element tuple with the section of the input up until the sentry (or
% "" when it's the end of the input) and the rest of the input after the
% sentry. The sentry is consumed.


% A literate program is alternating documentation blocks and code blocks. Let
% us convert the input into this most basic division.


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
    {{code, Tag, Code}, Rest1}.


-ifdef(TEST).
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
-endif.


% The get_blocks implemenataion has two utility functions that grab markup and
% code blocks. The initialization in get_blocks/3 puts a potential requirement
% on the format of our literate documents -- they must start with a markup
% block. However, I think this depends on what is used as a sentry for code
% blocks. This might be crappy.
%
% This function emits a list of {markup, Markup} and {code, Tag, Code}.
%     Markup = string()
%     Tag = string()
%     Code = string()

% In Joe Armstrong's EWEB implementation, he goes through several passes over
% the code blocks to provide various things (like line numbering). In it he
% increments the atom for every pass over the code blocks (code1, code2, etc).
% That way, if there's an error, it's easier to find the cause.
%
% https://www.sics.se/~joe/ericsson/literate/literate.html

% The whole point of my implementation is that the documentation or 'tangled'
% file is the source file, so we won't actually need these markup blocks. The
% code1 atom will signify stripped markup.


strip_markup(Blocks) ->
    strip_markup(Blocks, []).

strip_markup([], Acc) ->
    lists:reverse(Acc);

strip_markup([{markup, _Text} | Rest], Acc) ->
    strip_markup(Rest, Acc);

strip_markup([{code, Tag, Text} | Rest], Acc) ->
    strip_markup(Rest, [{code1, Tag, Text} | Acc]).

-ifdef(TEST).
strip_markup_test() ->
    [{code1, "tag", "1"}, {code1, "tag", "2"}] = strip_markup([
        {markup, "a"},
        {code, "tag", "1"},
        {markup, "b"},
        {code, "tag", "2"}]).
-endif.


% Code with the same tag should be concatenated.

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

-ifdef(TEST).
concat_code_test() ->
    #{"a" := "one\nthree",
      "b" := "two"} = concat_code([{code1, "a", "one"}, {code1, "b", "two"}, {code1, "a", "three"}]).
-endif.


% Wait, but if we're now dealing with a map, are we losing the benefit of atoms
% to signify a place in the code where an error is thrown? I feel like I might
% get bogged down with this question. I'll just keep going until I understand
% the problem.


% Now I'll try and expand these macros. In the other literate programming tools
% I've looked at, macros must be on their own line. They can be preceded by any
% amount of white space and when the macro is expanded, all the lines are
% preceded by that white space. I want to do something a little cooler. I would
% like this:
%
% <<some file>>=
% This is HTML and here is a list.
% <ul>
%     <li><<list elements>></li>
% </ul>
% >>
%
% <<list elements>>=
% one
% two
% three
% >>
%
% ...to expand to:
%
% This is HTML and here is a list.
% <ul>
%     <li>one</li>
%     <li>two</li>
%     <li>three</li>
% </ul>
%
% The only difference is that we need to wrap the macro lines in the preceding
% and trailing white space.

grab_macro(Line, Macro_start, Macro_end) ->
    {Possible_prefix, Rest} = grab_until(Line, Macro_start),
    case Rest of
        "" -> none;
        _ ->
            Prefix = Possible_prefix,
            {Macro_name, Suffix} = grab_until(Rest, Macro_end),
            {Macro_name, Prefix, Suffix}
    end.

-ifdef(TEST).
grab_macro_test() ->
    none = grab_macro("Nothing doing.", "<<", ">>"),
    {"foobar", "    ", ""} = grab_macro("    <<foobar>>", "<<", ">>"),
    {"list elements", "    <li>", "</li>"} = grab_macro("    <li><<list elements>></li>", "<<", ">>").
-endif.


% Given a line, grab_macro will either return the atom none or a three-tuple
% with the macro name, line prefix, and line suffix.


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

-ifdef(TEST).
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
-endif.


% The expand_macros function takes a block and replaces macro instances with
% the given map (which is a map of all the blocks) and returns the new block
% value. This function doesn't currently escape macros. If the macro delimeters
% are << and >>, then they will conflict with Erlang binaries! I must fix that,
% but I'm not sure how yet. One option is to pick delimeters that aren't used
% in any languages in the source document. I think that's a cop out. I'll come
% back to this later. (TODO)


expand_blocks(Blocks, Macro_start, Macro_end) ->
    expand_blocks(maps:to_list(Blocks), Blocks, Macro_start, Macro_end, #{}).

expand_blocks([], _Macros, _Macro_start, _Macro_end, Output_blocks) ->
    Output_blocks;

expand_blocks([{Name, Block} | Rest], Macros, Macro_start, Macro_end, Output_blocks) ->
    New_block = expand_macros(Block, Macros, Macro_start, Macro_end),
    expand_blocks(Rest, Macros, Macro_start, Macro_end, maps:put(Name, New_block, Output_blocks)).

-ifdef(TEST).
expand_blocks_test() ->
    Blocks = #{"A" => "b:\n"
                      "  <<B>>",
               "B" => "b\nb",
               "C" => "d:\n"
                      "  <<D>>",
               "D" => "d\nd",
               "ALL" => "<<A>>\n"
                        "<<C>>\n"},

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
                                 "d:\n"
                                 "  d\n"
                                 "  d\n"},

    Expected_output = expand_blocks(expand_blocks(Blocks, "<<", ">>"), "<<", ">>").
-endif.


% Expand blocks runs expand_macros on every block. In the test it requires two
% passes because A and C are nested in ALL. At some point I'll have to decide
% how many passes to execute. That depends on expected user behavior, but, I
% suspect I can just do it a bunch of times.
%
% There's some problems.
%
% 1. I think I'm starting to confuse the terms 'block' and 'macro'.
% 2. Each level has to buy into Macro_start, Macro_end, etc. I think I need to
%    define a data structure to represent the parser config. It might also be
%    good to have another structure for the parser state -- like the lines
%    traversed, the current column. Possibly other things I can't think of
%    right now.
% 3. I really don't like how I went from three-tuples to maps. I think I should
%    use one or the other.
