(TeX-add-style-hook
 "report"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem") ("geometry" "a4paper" "total={7in, 9in}") ("algorithm2e" "ruled" "procnumbered") ("xcolor" "dvipsnames") ("enumitem" "shortlabels")))
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
    "geometry"
    "algorithm2e"
    "xcolor"
    "pifont"
    "subcaption"
    "enumitem"
    "newfloat"
    "todonotes"
    "kantlipsum"
    "rugscriptie")
   (TeX-add-symbols
    "cmark"
    "xmark"
    "altasterism")
   (LaTeX-add-labels
    "sec:orgd4eeed8"
    "sec:org8f60ae0"
    "sec:org6dc882d"
    "sec:org7f1de23"
    "section:background"
    "sec:org56453c0"
    "sec:org491f2b4"
    "sec:org23e634a"
    "sec:org1bee90d"
    "subsection:chaos_game"
    "chaosGame"
    "sec:orgffab345"
    "sec:org7be3eab"
    "subsection:chaos_game_gpu"
    "sec:orgbf76a80"
    "sec:orgf5f0256"
    "sec:orgeee00e8"
    "sec:orgc2c5418"
    "sec:org15a778d"
    "sec:org0070bb1"
    "sec:org479085d"
    "sec:orge9609da"
    "subsection:self_similarity"
    "sec:orgec6ca05"
    "sec:org177fbb3"
    "sec:org6197cdf"
    "sec:orga0cdbc8"
    "sec:org834aaaa"
    "sec:orgcf7a295"
    "sec:org4ae5fb8"
    "sec:org1d3588c"
    "sec:org05f1ce5"
    "sec:org2e7bb93"
    "sec:org4c00ae2"
    "sec:orged143d7"
    "sec:org73c54ea"
    "sec:org3899552")
   (LaTeX-add-bibliographies
    "bibliography")
   (LaTeX-add-newfloat-DeclareFloatingEnvironments
    '("lsystem" "")))
 :latex)

