Most of the text here should be known and is *classic*.

To get started, consult the video guides 
  [http://user.math.uzh.ch/furrer/download/www/videos.shtml]
We assume the content of the videos as known. Here is a summary in a nutshell.

  

** Directory Structure **

Create a simple directory structure, for example:
```
git/MSc_Thesis_nn/
        |- report
        |- r
        |- data
        |- documents
        |...
```

Do not use spaces in path and file names. It is best to use only the alphabet [a-z,A-Z,1-9] and one '.' with appropriate extension.
 

** R **
 
Always work with relative paths!
Always work reproducibly!

Provide `readme` files. Benchmark and test your functions.  

If you use extensive datasets, provide a link to these to the involved members. These files should be placed in data as well, but not pushed to git!

The page [https://google.github.io/styleguide/Rguide.xml] gives some guidelines on how to structure code as well as further style guides for R.
          

** LaTeX **

Put all figures in a directory called `figure`. Files created by `pdflatex` do not need to be on git. These files should be excluded by the file `.gitignore`. It is possible that these files are not properly ignored see [http://stackoverflow.com/questions/11451535/gitignore-is-not-working].

Use macros. The file `header.sty` is an example.

Consult `guide4LaTeX.txt`,



** GIT ** 

System dependent files (`*.so`, `*.exe`) or files created through a compilation process should not be on git (`*.log`). Use the provided `.gitignore` file.  

Extensive data files, including RData, should not be pushed.

Push tex/Rmd/Rnw files only if they properly compile. 



================================================================================
Reinhard Furrer, Spring 2018
================================================================================
