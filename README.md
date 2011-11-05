Phase Zero
==========

About
----------

Phase Zero is a system for quickstarting projects using a template
file tree.  Template variables are set interactively when a project is
created using prompts stored in the package.

Template Package
----------------

A template package contains a directory named `src` and a file named
`variables.conf`.  The project file tree goes in `src`, and
`variables.conf` contains a list of variables and prompts.  Variables
have to be in a section, or they will not be set.  An example file

    [common]
    
    # Each row is
    # variableName = Interactive prompt
    
    thing = A thing
    segment = A directory segment
    file = A file name

Templates
----------

Template files use the [HStringTemplate][] library.  Variables in a
template are surrounded by dollar signs.  See the library
documentation for more details.

File names and segments in the file path can also contain template
variables.  They follow the same rules.  Other than that, the file
structure in the template package will be replicated exactly.

[HStringTemplate]: http://hackage.haskell.org/packages/archive/HStringTemplate/0.6.6/doc/html/Text-StringTemplate.html
