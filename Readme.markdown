Let's Build a Compiler... in F#!
================================

F# translation of Jack Crenshaw's Pascal code from his article, "[Let's Build A Compiler](http://compilers.iecc.com/crenshaw/)".

This code is not idiomatic F#. I'm in the process of changing this.

Have fun!

Running the Application
=======================

Start by running the tests. 

You can also use the compiler interactively. Run the project Lbac.Compiler and it will compile as you type.

Finally, you can run non-interactively, using files:

	> copy con > Foo.txt
	  1+2
	  ^C
	> Lbac.Compiler -i Foo.txt -o Bar.exe

This produces a console application which returns the result (3!).

License
=======

Crenshaw's Pascal and text is Copyright (C) 1988 by Jack W. Crenshaw. All rights reserved. I haven't included any of his material here directly, but it was the basis for this series.

My F# code and comments are Copyright 2011 by Craig Stuntz.
You may use this code under terms of the MIT license. See License.txt.

