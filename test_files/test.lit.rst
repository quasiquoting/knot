A reStucturedText File
======================

This was kind of the whole point of this library. I want to write literate
programs in rST because it is the greatest markup in the Universe.


.. code:: javascript
   :class: file:test_rst.js

    (function () {
        alert('Hello, world.');
    }());
.. end code


We would explain that JavaScript file and write some Erlang code.

.. code:: erlang
   :class: file:test_rst.erl

    -module(test_rst).
    -compile(export_all).

    ::functions::

    -ifdef(TEST).
    ::tests::
    -endif.
.. end code

.. code:: erlang
   :class: functions

    some_function() -> ok.
.. end code

.. code:: erlang
   :class: tests

    some_function_test() ->
        ok = some_function().
.. end code
