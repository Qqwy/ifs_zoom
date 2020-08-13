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
    "sec:org530208c"
    "sec:org50a1c6a"
    "sec:orgbc0722e"
    "sec:orge6880d4"
    "section:background"
    "sec:org2480645"
    "sec:orgd5249dc"
    "sec:org54a6c9c"
    "sec:orgcf7d05e"
    "subsection:chaos_game"
    "chaosGame"
    "sec:orgf102595"
    "sec:org5f6b0ab"
    "subsection:chaos_game_gpu"
    "sec:org45b5eeb"
    "subsection:deterministic_gpu"
    "sec:orgd6f1ab0"
    "sec:org7621a10"
    "sec:org6a9da71"
    "sec:org040787b"
    "sec:org50383e6"
    "subsection:point_cloud_optimizations"
    "sec:org1f7469f"
    "subsection:self_similarity"
    "sec:org7116b74"
    "sec:org092d7a6"
    "sec:org32ebe1f"
    "sec:orgff98e72"
    "sec:orgd545f4d"
    "sec:org2ebdd16"
    "sec:org8fbf79c"
    "sec:org84dd1ac"
    "sec:org625e189"
    "sec:orge8567ee"
    "sec:org7a68ea1"
    "sec:org63c4e91"
    "sec:org186c58e"
    "sec:orgf7f8276"
    "sec:org155d165")
   (LaTeX-add-bibliographies
    "bibliography")
   (LaTeX-add-newfloat-DeclareFloatingEnvironments
    '("lsystem" "")))
 :latex)

