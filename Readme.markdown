Let's Build a Compiler... in F#!
================================

F# translation of Jack Crenshaw's Pascal code from his article, "[Let's Build A Compiler](http://compilers.iecc.com/crenshaw/)".

This code is not idiomatic F#. That may change in the future, but for the time being I'm translating Crenshaw's Pascal in a fairly literal way.

Future plans include finishing the series and emitting .NET IL instead of 68000 asm.

Have fun!

Running the Application
=======================

Start by running the tests. Note because Visual Studio's support for tests in F# is lousy, **you must do a build (F6) before running the tests!** Running the tests won't implicitly update the assembly used by the test runner!

So do a build and then run all tests in the solution.

You can also use the compiler interactively. Run the project Lbac.Compiler and it will compile as you type.

License
=======

Crenshaw's Pascal and text is Copyright (C) 1988 by Jack W. Crenshaw. All rights reserved. I haven't included any of his material here directly, but it was the basis for this series.

My F# code and comments are Copyright 2011 by Craig Stuntz.
You may use this code under terms of the MIT license. See License.txt.

