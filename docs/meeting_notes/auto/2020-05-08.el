(TeX-add-style-hook
 "2020-05-08"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem") ("geometry" "a4paper" "total={7in, 9in}")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art11"
    "inputenc"
    "fontenc"
    "graphicx"
    "grffile"
    "longtable"
    "wrapfig"
    "rotating"
    "ulem"
    "amsmath"
    "textcomp"
    "amssymb"
    "capt-of"
    "hyperref"
    "float"
    "geometry")
   (LaTeX-add-labels
    "sec:org920b62b"
    "sec:orgf156ee3"
    "sec:org2d52582"
    "sec:orgff4f63e"
    "sec:org5293e88"
    "sec:org6833ba4"
    "sec:org86f2887"
    "sec:orga4b4c97"
    "sec:orgccf2714"
    "sec:org49cfda1"
    "sec:orgf005aea"
    "sec:org45e7abb"
    "sec:org3a9b6b4"
    "sec:org1c06ad1"
    "sec:org58322c6"
    "sec:org75d7dea"))
 :latex)

