Mini-C Compiler
===============

This is a compiler for a subset of the C programming language. The compiler is written
in F# and targets Microsoft Intermediate Language (MSIL). It was written so that I could
teach myself F#. If you're interested in doing the same thing, you may find it useful.

GUI
---

I wrote a simple GUI to visualize the compilation process. The Abstract Syntax Tree (AST)
and Microsoft Intermediate Language (MSIL) panels update in real-time when you change the source code.

![Screenshot](https://github.com/tgjones/mini-c/raw/master/docs/screenshot.png)

Acknowledgements
----------------

* I got the grammar for this particular subset of C from 
  [this paper](http://jamesvanboxtel.com/projects/minic-compiler/minic.pdf), 
  which looks like a university course assignment.
* Tim Robinson's [blog series on writing a Lisp compiler in F#](http://www.partario.com/blog/2009/05/lisp-compiler-in-f-introduction.html)
  was very useful. Although the source languages are different, I was still able 
  to use many of his ideas for my implementation.

License
-------

Mini-C is released under the [MIT License](http://www.opensource.org/licenses/MIT).