
    -module(globoweb).
    -compile(export_all).

    -ifdef(TEST).
    -include_lib("eunit/include/eunit.hrl").
    -endif.

    
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
    
        strip_markup(Blocks) ->
            strip_markup(Blocks, []).
    
        strip_markup([], Acc) ->
            lists:reverse(Acc);
    
        strip_markup([{markup, _Text} | Rest], Acc) ->
            strip_markup(Rest, Acc);
    
        strip_markup([{code, Tag, Text} | Rest], Acc) ->
            strip_markup(Rest, [{code1, Tag, Text} | Acc]).
    
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
    
        grab_macro(Line, Macro_start, Macro_end) ->
            {Possible_prefix, Rest} = grab_until(Line, Macro_start),
            case Rest of
                "" -> none;
                _ ->
                    Prefix = Possible_prefix,
                    {Macro_name, Suffix} = grab_until(Rest, Macro_end),
                    {Macro_name, Prefix, Suffix}
            end.
    
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
    
        expand_blocks(Blocks, Macro_start, Macro_end) ->
            expand_blocks(maps:to_list(Blocks), Blocks, Macro_start, Macro_end, #{}).
    
        expand_blocks([], _Macros, _Macro_start, _Macro_end, Output_blocks) ->
            Output_blocks;
    
        expand_blocks([{Name, Block} | Rest], Macros, Macro_start, Macro_end, Output_blocks) ->
            New_block = expand_macros(Block, Macros, Macro_start, Macro_end),
            expand_blocks(Rest, Macros, Macro_start, Macro_end, maps:put(Name, New_block, Output_blocks)).
    
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
    
        process_files(Files, Code_tag_start, Code_tag_end, Code_end, Macro_start, Macro_end) ->
            process_files(Files, Code_tag_start, Code_tag_end, Code_end, Macro_start, Macro_end, []).
    
        process_files([], _Code_tag_start, _Code_tag_end, _Code_end, _Macro_start, _Macro_end, Acc) ->
            lists:reverse(Acc);
    
        process_files([File | Rest], Code_tag_start, Code_tag_end, Code_end, Macro_start, Macro_end, Acc) ->
            process_files(Rest, Code_tag_start, Code_tag_end, Code_end, Macro_start, Macro_end, [process_file(File, Code_tag_start, Code_tag_end, Code_end, Macro_start, Macro_end) | Acc]).
    
        start(Files) ->
            Output = process_files(Files, "\n   :class: ", "\n", "\n.. end code", "::", "::"),
            lists:foreach(fun (Some_files) ->
                              lists:foreach(fun (File) ->
                                                io:format("~s written.~n", [File])
                                            end,
                                            Some_files)
                          end,
                          Output).

    -ifdef(TEST).
    
        split_test() ->
            {"abcd", "efgh"} = grab_until("abcd<efgh", "<"),
            % Extract a documentation chunk.
            {"foobar\n", "tag>>=\ncode chunk\n>>bazbuzz"} = grab_until("foobar\n<<tag>>=\ncode chunk\n>>bazbuzz", "<<"),
            % Extract a code chunk.
            {"tag>>=\ncode chunk", "bazbuzz"} = grab_until("tag>>=\ncode chunk\n>>bazbuzz", "\n>>"),
            % Extract next documentation chunk.
            {"bazbuzz", ""} = grab_until("bazbuzz", "<<").
    
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
    
        strip_markup_test() ->
            [{code1, "tag", "1"}, {code1, "tag", "2"}] = strip_markup([
                {markup, "a"},
                {code, "tag", "1"},
                {markup, "b"},
                {code, "tag", "2"}]).
    
        concat_code_test() ->
            #{"a" := "one\nthree",
              "b" := "two"} = concat_code([{code1, "a", "one"}, {code1, "b", "two"}, {code1, "a", "three"}]).
    
        grab_macro_test() ->
            none = grab_macro("Nothing doing.", "<<", ">>"),
            {"foobar", "    ", ""} = grab_macro("    <<foobar>>", "<<", ">>"),
            {"list elements", "    <li>", "</li>"} = grab_macro("    <li><<list elements>></li>", "<<", ">>").
    
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
    
        get_output_files_test() ->
            [] = get_output_files(#{"A" => "a", "B" => "b"}),
            [{"globoweb.erl", "TODO"}] = get_output_files(#{"A" => "a", "file:globoweb.erl" => "TODO", "B" => "b"}),
            Files = get_output_files(#{"file:globoweb.erl" => "TODO", "file:src/globoweb.erl" => "TODO", "file:../../why.txt" => "?"}),
            "TODO" = proplists:get_value("globoweb.erl", Files),
            "TODO" = proplists:get_value("src/globoweb.erl", Files),
            "?" = proplists:get_value("../../why.txt", Files).
    
        file_name_test() ->
            "test_files/foobar.txt" = file_name("test_files", "foobar.txt"),
            "/path/to/repository/src/globoweb.erl" = file_name("/path/to/repository", "src/globoweb.erl").
    
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
            Output_file = "test_files/process_file_test.js",
            [Output_file] = process_file("test_files/process_file_test.lit.txt",
                                                  "\n<<", ">>=\n", "\n>>", "<<", ">>"),
            Expected_output = read_file("test_files/process_file_test.js.expected_output"),
            Actual_output = read_file(Output_file),
    
            Expected_output = Actual_output ++ "\n",
            file:delete(Output_file).
    
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
    
        process_rst_test() ->
            Output_files = process_file("test_files/test.lit.rst",
                                        "\n   :class: ", "\n", "\n.. end code", "::", "::"),
            lists:foreach(fun (File) ->
                              io:format("Deleting: ~p~n", [File]),
                              file:delete(File)
                          end,
                          Output_files).
    -endif.