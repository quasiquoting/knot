This is a very basic literate file as an initial test.

###### file:process_file_test.js
```{.javascript name="file:process_file_test.js"}
(function () {
    <<shared state>>
    <<functions>>
    <<api>>
    // This shouldn't be detected as a macro:
    // \<<not a macro>>
    <<layer 1>>
}());
```


This seems pretty typical of JavaScript.

###### shared state
```{.javascript name="shared state"}
var _state = 'off';
```

###### functions
```{.javascript name="functions"}

function toggle_state() {
    if (_state == 'off') {
        _state == 'on';
    } else {
        _state == 'off';
    }
}

function get_state() {
    return _state;
}

```

This is an example of why a literate program is really nice. I can add to the
shared state at the same time as I add a function to manage it.

###### shared state
```{.javascript name="shared state"}
var _message = '';
```

###### functions
```{.javascript name="functions"}
function set_message(message) {
    _message = message;
}

function get_message() {
    return _message;
}
```


Now we can provide an API to users of this library.

###### api
```{.javascript name="api"}
window.mylibrary = {
    message: get_message,
    state: get_state,
    toggle_state: toggle_state};
```


###### layer 1
```{name="layer 1"}
// <<layer 2>> //
```

###### layer 2
```{name="layer 2"}
<<layer 3>>
```

###### layer 3
```{name="layer 3"}
deep macro expansion
```
