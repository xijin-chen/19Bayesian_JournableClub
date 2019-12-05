911:

GIT
===

- Pull whenever you start working

- Push only after your modifications produce a letigimate PDF, i.e., if all compiles properly.

- System dependent files (`*.so`, `*.exe`) or files created through a compilation process should not be on git. Do not overrule the provided `.gitignore` file. 

- Extensive data files should not be pushed but deposited and directly accessed in R. 

- If errors at pushing occur:
   - properly committed/
   - is another pull necessary? 

LaTeX
=====

- Have you opened the appropriate 'project'?
    (Errors like:  "Line 11: Undefined control sequence"/
                      "Missing \begin{document}" )
 
- Are additional R packages required?

- Check the output log to find the bug.

- Consult 'rules4latex.txt'.

knitr
=====

- No one knows all options: https://yihui.name/knitr/options/

- Keep very long computations offline.

- Always work with relative paths! Always work reproducibly!



