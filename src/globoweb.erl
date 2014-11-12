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
