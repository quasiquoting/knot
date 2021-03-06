%% This file was generated by knot.md using the literate program contained
%% therein.

-module(knot).
-compile(export_all).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/file.hrl").

collect_to_eol(Input) ->
    collect_to_eol(Input, "").

collect_to_eol("", Acc) ->
    {lists:reverse(Acc), ""};

collect_to_eol([$\n | Rest], Acc) ->
    {lists:reverse(Acc), Rest};

collect_to_eol([Char | Rest], Acc) ->
    collect_to_eol(Rest, [Char | Acc]).
collect_to_fence(Input) ->
    collect_to_fence(Input, "").

collect_to_fence("", Acc) ->
    {lists:reverse(Acc), ""};

collect_to_fence([$\n, $`, $`, $` | Rest], Acc) ->
    {lists:reverse(Acc), Rest};

collect_to_fence([Char | Rest], Acc) ->
    collect_to_fence(Rest, [Char | Acc]).
all_code(Input) ->
    all_code(Input, []).

all_code("", Acc) ->
    lists:reverse(Acc);

all_code([$\n, $`, $`, $` | Rest], Acc) ->
    {Attributes, Rest1} = collect_to_eol(Rest),
    {match, [Name]} = re:run(Attributes,
                             "name=\"(?<name>[^\"]+)\"",
                             [{capture, [name], list}]),
    {Code, Rest2} = collect_to_fence(Rest1),
    all_code(Rest2, [{Name, Code} | Acc]);

all_code([_ | Rest], Acc) ->
    all_code(Rest, Acc).
concat_sections(Sections) ->
    Join_sections = fun (Key, Acc) ->
        Values = proplists:get_all_values(Key, Sections),
        Joined = string:join(Values, "\n"),
        [{Key, Joined} | Acc]
    end,

    lists:foldr(Join_sections, [], proplists:get_keys(Sections)).
collect_to_replacement_open(Line) ->
    collect_to_replacement_open(Line, "").

collect_to_replacement_open("", Acc) ->
    {lists:reverse(Acc), ""};

% Ignores escaped delimeters.
collect_to_replacement_open([$\\, $<, $< | Rest], Acc) ->
    collect_to_replacement_open(Rest, [$<, $<, $\\ | Acc]);

collect_to_replacement_open([$<, $< | Rest], Acc) ->
    {lists:reverse(Acc), Rest};

collect_to_replacement_open([Char | Rest], Acc) ->
    collect_to_replacement_open(Rest, [Char | Acc]).
split_section(Line) ->
    case collect_to_replacement_open(Line) of
        {_, ""} ->
            % No section in this line.
            nil;

        {Prefix, Rest} ->
            % Rest contains the section name and the closing delimiter.
            {Padded_name, Suffix} = collect_to_replacement_close(Rest),
            {string:strip(Padded_name), Prefix, Suffix}
    end.
collect_to_replacement_close(Input) ->
    collect_to_replacement_close(Input, []).

collect_to_replacement_close("", Acc) ->
    {lists:reverse(Acc), ""};

collect_to_replacement_close([$>, $> | Rest], Acc) ->
    {lists:reverse(Acc), Rest};

collect_to_replacement_close([Char | Rest], Acc) ->
    collect_to_replacement_close(Rest, [Char | Acc]).
expand_sections(Code, Sections) ->
    expand_sections(Code, Sections, []).

expand_sections("", _Sections, Acc) ->
    string:join(lists:reverse(Acc), "\n");

expand_sections(Code, Sections, Acc) ->
    {Line, Rest} = collect_to_eol(Code),
    case split_section(Line) of
        nil ->
            expand_sections(Rest, Sections, [Line | Acc]);

        {Name, Prefix, Suffix} ->
            case proplists:get_value(Name, Sections) of
                undefined ->
                    io:format("Warning: code section named ~p not found.~n", [Name]),
                    expand_sections(Rest, Sections, [Prefix ++ Suffix| Acc]);

                Code_to_insert ->
                    New_lines = re:split(Code_to_insert, "\n", [{return, list}]),
                    Wrapped = lists:map(fun (X) -> Prefix ++ X ++ Suffix end, New_lines),
                    expand_sections(Rest, Sections, [string:join(Wrapped, "\n") | Acc])
            end
    end.
expand_all_sections(Sections) ->
    expand_all_sections(Sections, Sections, []).

expand_all_sections([], _Sections, Acc) ->
    lists:reverse(Acc);

expand_all_sections([{Name, Code} | Rest], Sections, Acc) ->
    expand_all_sections(Rest, Sections, [{Name, expand_sections(Code, Sections)} | Acc]).
unescape(Code) ->
    re:replace(Code, "\\\\<<", "<<", [global, {return, list}]).
unescape_sections(Sections) ->
    unescape_sections(Sections, []).

unescape_sections([], Acc) ->
    lists:reverse(Acc);

unescape_sections([{Name, Code} | Rest], Acc) ->
    unescape_sections(Rest, [{Name, unescape(Code)} | Acc]).
file_sections(Sections) ->
    file_sections(Sections, []).

file_sections([], Acc) ->
    lists:reverse(Acc);

file_sections([{[$f, $i, $l, $e, $: | _] = Name, Code} | Rest], Acc) ->
    file_sections(Rest, [{Name, Code} | Acc]);

file_sections([_ | Rest], Acc) ->
    file_sections(Rest, Acc).
file_name(Base_directory, File_name) ->
    filename:nativename(filename:absname_join(Base_directory, File_name)).

write_file(Base_directory, File_name, Contents) ->
    Fn = file_name(Base_directory, File_name),
    case file:write_file(Fn, Contents) of
        ok -> Fn;
        {error, Reason} ->
            io:format("Error: Failed to write file (~s): ~s. "
                      "(Knot doesn't create directories, so you may need to "
                      "create one.)~n", [Fn, Reason])
    end.
process_file(File_name) ->
    Base_directory = filename:dirname(File_name),
    Concatenated_code = concat_sections(
                            all_code(
                                read_file(File_name))),
    Expanded_code = expand_all_sections(
                        expand_all_sections(
                            expand_all_sections(
                                expand_all_sections(Concatenated_code)))),
    Files = file_sections(
                unescape_sections(Expanded_code)),

    write_file_sections(Base_directory, Files, []).

write_file_sections(_Base_directory, [], Files_written) ->
    lists:reverse(Files_written);

write_file_sections(Base_directory, [{[$f, $i, $l, $e, $: | File_name], Contents} | Rest], Files_written) ->
    New_file = write_file(Base_directory, File_name, Contents),
    write_file_sections(Base_directory, Rest, [New_file | Files_written]).
process_files(Files) ->
    process_files(Files, []).

process_files([], Files_written) ->
    % Files_written is a list of lists of strings. Strings are lists, too,
    % so lists:flatten can't be used becuause it turns into a single
    % string.
    lists:concat(lists:reverse(Files_written));

process_files([File | Files], Files_written) ->
    process_files(Files, [process_file(File) | Files_written]).
file_modified_time(File_name) ->
    {ok, Info} = file:read_file_info(File_name),
    Info#file_info.mtime.
modified_times(Files) ->
    Fun = fun (X, A) ->
        [{X, file_modified_time(X)} | A]
    end,
    lists:foldl(Fun, [], Files).
watch(Files, Fun) ->
    watch(Files, Fun, []).

watch(Files, Fun, State) ->
    Modified_times = modified_times(existing_files(Files)),
    Changed_files = changed_files(Modified_times, State),

    case length(Changed_files) > 0 of
        true ->
            apply(Fun, [Changed_files]);
        _ -> noop
    end,

    % wait
    timer:sleep(timer:seconds(1)),

    % loop
    watch(Files, Fun, Modified_times).
changed_files(A, B) ->
    Fun = fun (X, Acc) ->
        case proplists:get_value(X, A) =:= proplists:get_value(X, B) of
            false ->
                [X | Acc];
            _ -> Acc
        end
    end,
    lists:foldl(Fun, [], proplists:get_keys(A)).
existing_files(Files) ->
    lists:filter(fun filelib:is_file/1, Files).
read_file(File_name) ->
    case file:read_file(File_name) of
        {ok, Binary} ->
            binary_to_list(Binary);
        {error, Reason} ->
            io:format("Failed to read file (~s): ~s~n", [File_name, Reason]),
            error({read_file, File_name, Reason})
    end.

print_sections(Sections) ->
    lists:foreach(fun ({Name, Code}) ->
                      io:format("~s~n-----~n~s~n-----~n~n",
                                [Name, Code])
                  end,
                  Sections).
print_code(File_name) ->
    print_sections(
        all_code(
            read_file(File_name))).

print_unindented_code(File_name) ->
    print_sections(
        all_code(
            read_file(File_name))).

print_concatenated_code(File_name) ->
    print_sections(
        concat_sections(
            all_code(
                read_file(File_name)))).

print_expanded_code(File_name) ->
    print_sections(
        expand_all_sections(
            concat_sections(
                all_code(
                    read_file(File_name))))).

print_unescaped_code(File_name) ->
    print_sections(
        unescape_sections(
            expand_all_sections(
                concat_sections(
                    all_code(
                        read_file(File_name)))))).
print_file_sections(File_name) ->
    print_sections(
        file_sections(
            unescape_sections(
                expand_all_sections(
                    concat_sections(
                        all_code(
                            read_file(File_name))))))).

-ifdef(TEST).
collect_to_eol_test() ->
    {"", ""} = collect_to_eol(""),
    {"foo", "bar\nbaz"} = collect_to_eol("foo\nbar\nbaz"),
    {"foo", ""} = collect_to_eol("foo\n").
collect_to_fence_test() ->
    {"foobar", ""} = collect_to_fence("foobar"),
    {"my\ncode\nhere", "\nmore input"} = collect_to_fence("my\ncode\nhere\n```\nmore input").
all_code_test() ->
    Input = "A sample document.\n"
            "\n"
            "``` {.erlang name=\"section 1\"}\n"
            "Code 1, line 1.\n"
            "Code 1, line 2.\n"
            "```\n"
            "\n"
            "More documentation.\n"
            "\n"
            "```{name=\"section 2\"}\n"
            "Code 2, line 1.\n"
            "Code 2, line 2.\n"
            "```\n"
            "\n"
            "End of sample document.\n",

    Expected = [{"section 1", "Code 1, line 1.\nCode 1, line 2."},
                {"section 2", "Code 2, line 1.\nCode 2, line 2."}],

    Expected = all_code(Input).

all_code_no_intermediate_documentation_test() ->
    Input = "A sample document.\n"
            "\n"
            "``` {.fake name=\"section 1\"}\n"
            "\n"
            "Code 1, line 1.\n"
            "Code 1, line 2.\n"
            "```\n"
            "``` {.fake name=\"section 2\"}\n"
            "Code 2, line 1.\n"
            "Code 2, line 2.\n"
            "```\n"
            "The end.\n",

    Expected = [{"section 1", "\nCode 1, line 1.\nCode 1, line 2."},
                {"section 2", "Code 2, line 1.\nCode 2, line 2."}],

    Expected = all_code(Input).
concat_sections_test() ->
    Input = [{"foo", "FOO"},
             {"bar", "BAR"},
             {"foo", "FOO"}],

    Expected = [{"foo", "FOO\nFOO"},
                {"bar", "BAR"}],

    Expected = concat_sections(Input).
collect_to_replacement_open_test() ->
    {"foobar", ""} = collect_to_replacement_open("foobar"),
    {"    ", "my replacement>>"} = collect_to_replacement_open("    <<my replacement>>"),
    {"- ", "my replacement>> -"} = collect_to_replacement_open("- <<my replacement>> -"),
    {"\\<<not a replacement>>", ""} = collect_to_replacement_open("\\<<not a replacement>>").
split_section_test() ->
    nil = split_section("foobar"),
    {"my section", "    ", ""} = split_section("    <<my section>>"),
    {"my section", "    <li>", "</li>"} = split_section("    <li><<my section>></li>").
collect_to_replacement_close_test() ->
    {"foobar", ""} = collect_to_replacement_close("foobar"),
    {"my replacement", "..."} = collect_to_replacement_close("my replacement>>...").
expand_sections_test() ->
    Input_code = "\n"
                 "start\n"
                 "<< things >>\n"
                 "- <<things>> -\n"
                 "    <<unused>>\n"
                 "end\n",
    Input_sections= [{"things", "one\ntwo"}],
    Expected = "\nstart\none\ntwo\n- one -\n- two -\n    \nend",
    Expected = expand_sections(Input_code, Input_sections).
expand_all_sections_test() ->
    Input = [{"first one", "First.\n<<list of things>>"},
             {"second one", "This...\n-<<list of things>>-\nis the second."},
             {"All the things!", "<<first one>>\n* <<second one>> *\nDone."},
             {"list of things", "one\ntwo"}],

    Expected = [{"first one", "First.\none\ntwo"},
                {"second one", "This...\n-one-\n-two-\nis the second."},
                {"All the things!", "First.\none\ntwo\n* This... *\n* -one- *\n* -two- *\n* is the second. *\nDone."},
                {"list of things", "one\ntwo"}],

    Expected = expand_all_sections(expand_all_sections(Input)).
unescape_test() ->
    "foo\n    <<not a section>>\nbar" = unescape("foo\n    \\<<not a section>>\nbar"),
    "- \\<< really not a section>> -" = unescape("- \\\\<< really not a section>> -").
unescape_sections_test() ->
    Input = [{"foo", "\\<<"},
             {"bar", "bar"}],

    Expected = [{"foo", "<<"},
                {"bar", "bar"}],

    Expected = unescape_sections(Input).
file_sections_test() ->
    Input = [{"file:a", "a"},
             {"not a file", "not a file"},
             {"file:b", "b"}],
    Expected = [{"file:a", "a"},
                {"file:b", "b"}],
    Expected = file_sections(Input).
file_name_test() ->
    "test_files/foobar.txt" = file_name("test_files", "foobar.txt"),
    "/path/to/repository/src/knot.erl" = file_name("/path/to/repository", "src/knot.erl").

write_file_test() ->
    "test_files/test.txt" = write_file("test_files", "test.txt", "write_file_test\n"),
    {ok, <<"write_file_test\n">>} = file:read_file(file_name("test_files", "test.txt")),
    file:delete(file_name("test_files", "test.txt")).
process_file_test() ->
    ["test_files/process_file_test.js"] = process_file("test_files/process_file_test.md"),
    Expected = read_file("test_files/process_file_test.js.expected_output"),
    Actual = read_file("test_files/process_file_test.js") ++ "\n",
    %?debugVal(Expected),
    %?debugVal(Actual),
    Expected = Actual,
    file:delete("test_files/process_file_test.js").
process_files_test() ->
    Output_files = ["test_files/process_files_1_a.txt",
                    "test_files/process_files_1_b.txt",
                    "test_files/process_files_2_a.txt",
                    "test_files/process_files_2_b.txt",
                    "test_files/process_files_3_a.txt",
                    "test_files/process_files_3_b.txt"],
    Actual = process_files(["test_files/process_files_1.md",
                            "test_files/process_files_2.md",
                            "test_files/process_files_3.md"]),

    true = lists:all(fun (X) ->
                        lists:member(X, Output_files)
                     end,
                     Actual),

    6 = length(Actual),

    lists:map(fun file:delete/1, Actual).
file_modified_time_test() ->
    {Day, _} = calendar:local_time(),
    {Day, _} = file_modified_time("knot.erl").
modified_times_test() ->
    Files = ["knot.beam", "knot.erl"],
    Modified_times = modified_times(Files),

    {Today, _} = calendar:local_time(),
    {Today, _} = proplists:get_value("knot.beam", Modified_times),
    {Today, _} = proplists:get_value("knot.erl", Modified_times).
existing_files_test() ->
    Input = ["../knot.md", "i_will_never_exist.txt"],
    Expected = ["../knot.md"],
    Expected = existing_files(Input).
-endif.