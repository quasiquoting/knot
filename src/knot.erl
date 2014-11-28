-module(knot).
-compile(export_all).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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


find_indentation([]) -> "";

find_indentation([Line | Lines]) ->
    case re:run(Line, "\\S") of
        {match, _} ->
            {match, [Indentation]} = re:run(Line, "^(?<indentation>[\s]*)", [{capture, [indentation], list}]),
            Indentation;
        _ ->
            find_indentation(Lines)
    end.

trim_indentation(Lines, Indentation) ->
    Regex = string:join(["^", Indentation], ""),
    trim_indentation(Lines, Regex, []).

trim_indentation([], _Regex, Acc) ->
    lists:reverse(Acc);

trim_indentation([Line | Rest], Regex, Acc) ->
    trim_indentation(Rest, Regex, [re:replace(Line, Regex, "", [{return, list}]) | Acc]).

trim_code(Code) ->
    Lines = re:split(Code, "\n", [{return, list}]),
    Indentation = find_indentation(Lines),
    New_lines = trim_indentation(Lines, Indentation),
    string:join(New_lines, "\n").

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

expand_all_blocks(Macros, Config) ->
    maps:map(fun (_K, V) -> expand_macros(V, Macros, Config) end, Macros).

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

file_name(Base_directory, File_name) ->
    filename:nativename(filename:absname_join(Base_directory, File_name)).

write_file(Base_directory, File_name, Contents) ->
    Fn = file_name(Base_directory, File_name),
    ok = file:write_file(Fn, Contents),
    Fn.

read_file(File_name) ->
    {ok, Binary} = file:read_file(File_name),
    binary_to_list(Binary).

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

process_files(Files, Config) ->
    process_files(Files, Config, []).

process_files([], _Config, Acc) ->
    lists:reverse(Acc);

process_files([File | Rest], Config, Acc) ->
    process_files(Rest, Config, [process_file(File, Config) | Acc]).

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


-ifdef(TEST).
find_indentation_test() ->
    "    " = find_indentation(["", "  \t", "    ", "    foo() -> ok.", "\t\tbar() -> ok."]),
    "" = find_indentation(["", "", "foo() -> ok.", "", "bar() -> ok."]).


trim_indentation_test() ->
    Input = [" ", "\t   ", "    foo() -> ok.", "    ", "    bar() -> ok."],
    % Leaves inconsisten input alone.
    Expected = [" ", "\t   ", "foo() -> ok.", "", "bar() -> ok."],
    Expected = trim_indentation(Input, "    ").


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


concat_code_test() ->
    Input = [{"a", "one"}, {"b", "two"}, {"a", "three"}],
    Expected = #{"a" => "one\nthree",
                 "b" => "two"},
    Expected = concat_code(Input).


find_macro_test() ->
    Input = "    <li><<list elements>></li>",
    Expected = {"list elements", "    <li>", "</li>"},
    Expected = find_macro(Input, #{macro => "^<<(?<name>[^>]+)>>"}).

expand_macros_test() ->
    Input = "<ul>\n"
            "    <li><<items>></li>\n"
            "</ul>",
    Expected = "<ul>\n"
               "    <li>one</li>\n"
               "    <li>two</li>\n"
               "    <li>three</li>\n"
               "</ul>",
    Macros = #{"items" => "one\ntwo\nthree"},
    Config = #{macro => "^<<(?<name>[^>]+)>>"},

    Expected = expand_macros(Input, Macros, Config).


expand_all_blocks_test() ->
    Config = #{macro => "^<<(?<name>[^>]+)>>"},

    Input = #{"A" => "a:\n"
                     "  <<B>>",
              "B" => "b\nb",
              "C" => "c:\n"
                     "  <<D>>",
              "D" => "d\nd",
              "ALL" => "<<A>>\n"
                       "    || <<C>> ||\n"},

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

escaped_macro_example_test() ->
    Input = "<<notamacro>> ...",
    Pattern = "(<)\\|(<[^>]+>>)",
    Expected = "<<notamacro>> ...",

    Expected = unescape(Input, #{escaped_macro => Pattern}).

substring_test() ->
    "123" = substring("12345", {0, 3}).

lines_test() ->
    ["foo", "bar", "baz"] = lines("foo\nbar\nbaz").

map_lines_test() ->
    "foo\nbar\nbaz\nbuzz" = map_lines(fun (X) -> X end, "foo\nbar\nbaz\nbuzz").

unescape_test_again() ->
    Config = #{escaped_macro => "^(<)\\(<([^>]+>>)"},
    "    No macro." = unescape("    No macro.", Config),
    "    <<notamacro>>" = unescape("    <\\<notamacro>>", Config).


get_output_files_test() ->
    [] = get_output_files(#{"A" => "a", "B" => "b"}),
    [{"knot.erl", "TODO"}] = get_output_files(#{"A" => "a", "file:knot.erl" => "TODO", "B" => "b"}),
    Files = get_output_files(#{"file:knot.erl" => "TODO", "file:src/knot.erl" => "TODO", "file:../../why.txt" => "?"}),
    "TODO" = proplists:get_value("knot.erl", Files),
    "TODO" = proplists:get_value("src/knot.erl", Files),
    "?" = proplists:get_value("../../why.txt", Files).


file_name_test() ->
    "test_files/foobar.txt" = file_name("test_files", "foobar.txt"),
    "/path/to/repository/src/knot.erl" = file_name("/path/to/repository", "src/knot.erl").

write_file_test() ->
    "test_files/test.txt" = write_file("test_files", "test.txt", "write_file_test\n"),
    {ok, <<"write_file_test\n">>} = file:read_file(file_name("test_files", "test.txt")),
    file:delete(file_name("test_files", "test.txt")).

read_file_test() ->
    "test_files/read_file_test.txt" = write_file("test_files", "read_file_test.txt", "read_file_test\n"),
    Fn = file_name("test_files", "read_file_test.txt"),
    "read_file_test\n" = read_file(Fn),
    file:delete(Fn).

process_file_test() ->
    Config = #{code_start => "^(?<consume><<(?<tag>[^>]+)>>=\n)",
               code_end => "^\n>>",
               macro => "^<<(?<name>[^>]+)>>",
               escaped_macro => "^(<)\\\\\\\\(<)"},

    Output_file = "test_files/process_file_test.js",

    [Output_file] = process_file("test_files/process_file_test.lit.txt", Config),
    Expected_output = read_file("test_files/process_file_test.js.expected_output"),
    Actual_output = read_file(Output_file),

    Expected_output = Actual_output ++ "\n",
    file:delete(Output_file).

process_files_test() ->
    Config = #{code_start => "^(?<consume>\n<<(?<tag>[^>]+)>>=\n)",
               code_end => "^(?<consume>^\n>>)\n",
               macro => "^<<(?<name>[^>]+)>>",
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

-endif.
