(TeX-add-style-hook
 "main"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("acmart" "acmsmall" "screen")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1")))
   (add-to-list 'LaTeX-verbatim-environments-local "Verbatim")
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-environments-local "minted")
   (TeX-run-style-hooks
    "latex2e"
    "acmart"
    "acmart10"
    "booktabs"
    "subcaption"
    "bookmark"
    "inputenc"
    "fontenc"
    "xspace"
    "fancyhdr"
    "minted")
   (TeX-add-symbols
    '("todo" 2)
    '("q" 2)
    '("blu" 1)
    '("blk" 1)
    '("std" 1)
    '("cmd" 1)
    "hs"
    "teq"
    "ghci"
    "defeq"
    "dollar")
   (LaTeX-add-bibliographies
    "refs"))
 :latex)

