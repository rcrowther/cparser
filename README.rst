======================
C Parser for Parboiled
======================

A `c` parser (Scala)
--------------------
A `c` parser written in Scala. For Parboiled, the Java/Scala PEG parser.

You only need one file from here, /src/parsers/c99. All the rest is support framework I used on the way.


Which dialect?
--------------
Saying you can parse 'c' is like saying you speak English. Which?

This file was based on online BISON parsers, dated 1995. That may make them the informal 'C95' standard. They have been heavily revised in light of a 2005 draft I found. That could be anything, I'll never know, but it does include important features such as "inline". I called it 'C99' as it likely is, in some form.


How close is it?
----------------
Not standard. The biggest hack is forcing specifications of type to come before declarations. This keeps the parser within the scope of what PEG and Parboiled do well. Then again, most 'c' code is written like this.


How do I make it work?
----------------------
With Parboiled. You'll need parboiled-core and parboiled-scala jars. Look at the documentation on the sites below. With appropriate support code, code something like ::

    val parser = new C99 { override val buildParseTree = false }
    val runner = TracingParseRunner(parser.Root)
    val result = runner.run(in)
    traceError("Parsing errors:")
    println( ErrorUtils.printParseErrors(result))


Does it work?
-------------
I do not have thorough tests, and niether have I worked towards line-by-line verification. I know it can scan GLib files, which seems to me a reasonable rough test.

 
References
----------------

An original BISON parser,
http://www.lysator.liu.se/c/ANSI-C-grammar-l.html#CONSTANT

The 2005 Draft 'c' specification,
http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1124.pdf

Parboiled GitHub
https://github.com/sirthias/parboiled


Parboiled Home page
http://parboiled.org

