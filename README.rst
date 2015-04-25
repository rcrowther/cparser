======================
C Parser for Parboiled
======================

A `c` parser (Scala)
--------------------
A `c` parser written in Scala. For Parboiled, the Java/Scala PEG parser.

You only need one file from here, /src/parsers/c99.scala. All the rest is support framework.


Which dialect?
--------------
Saying you can parse 'c' is like saying you speak English. Which?

This file was based on BISON parsers available online, dated 1995. That may be the informal 'C95' standard. The rule tree was heavily revised in light of a 2005 draft 'c' spec. The draft could be any version, I'll never know, but it does include notable features such as "inline". I called it 'C99'. Likely it is, in some form.


How close is it?
----------------
Not standard. The biggest hack is forcing specifications of type to come before declarations. This keeps the parser within scope of what PEG and Parboiled do well. Then again, most 'c' code is written like this.


How do I make it work?
----------------------
With Parboiled. You'll need 'parboiled-core' and 'parboiled-scala' jars. Look at the documentation on the sites below. With appropriate support code, code something like ::

    val parser = new C99 { override val buildParseTree = false }
    val runner = TracingParseRunner(parser.Root)
    val result = runner.run(in)
    traceError("Parsing errors:")
    println( ErrorUtils.printParseErrors(result))


Does it work?
-------------
I do not have thorough tests. Niether have I worked towards line-by-line verification. I know it can scan GLib files, which is... working 'c' code?

 
References
----------------

An original BISON parser,
http://www.lysator.liu.se/c/ANSI-C-grammar-l.html

The 2005 draft 'c' specification,
http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1124.pdf

Parboiled GitHub,
https://github.com/sirthias/parboiled

Parboiled Home page,
http://parboiled.org

