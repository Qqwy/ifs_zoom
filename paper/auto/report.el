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
    "sec:org6c020a4"
    "sec:orgaf9ca23"
    "sec:org80fbb4e"
    "sec:org3aadd34"
    "section:background"
    "sec:orgee658f7"
    "sec:org00b805d"
    "sec:orgafc99c3"
    "sec:org448db85"
    "subsection:viewport_transformation"
    "sec:org3e92a09"
    "sec:org0392e27"
    "sec:org6824b9c"
    "subsection:chaos_game"
    "chaosGame"
    "sec:orge2a65d9"
    "sec:org27fb09e"
    "subsection:chaos_game_gpu"
    "sec:orgbbb02a0"
    "subsection:deterministic_gpu"
    "sec:org3184d25"
    "sec:org01456ad"
    "sec:org1d634cb"
    "sec:org85e6b69"
    "sec:org10cd42b"
    "subsection:point_cloud_optimizations"
    "sec:org10f68ba"
    "subsection:self_similarity"
    "sec:orgb873968"
    "sec:org8c89bd1"
    "sec:orgaa58ee4"
    "sec:org59e7717"
    "sec:orgb8ffe93"
    "sec:org67e7edc"
    "sec:org3252c46"
    "sec:org59b2415"
    "sec:org0429ae8"
    "sec:org2c2571e"
    "sec:org38d82a8"
    "sec:org90ebb89"
    "sec:orgb2b82d5"
    "sec:org09753ed"
    "sec:org03541c9"
    "sec:org7e5c291")
   (LaTeX-add-bibliographies
    "bibliography")
   (LaTeX-add-newfloat-DeclareFloatingEnvironments
    '("lsystem" "")))
 :latex)

