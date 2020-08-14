(TeX-add-style-hook
 "report"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem") ("geometry" "a4paper" "total={7in, 9in}") ("algorithm2e" "ruled" "procnumbered") ("xcolor" "dvipsnames") ("enumitem" "shortlabels")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
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
    "sec:orgc5b263f"
    "sec:orge115b1a"
    "sec:org7147e0d"
    "sec:org32880ce"
    "section:background"
    "sec:org6a0c6d4"
    "sec:org15d6dda"
    "sec:org8ae51b8"
    "sec:orgf4dbce6"
    "subsection:viewport_transformation"
    "sec:org5bdd311"
    "sec:orgb8298b8"
    "sec:org80c999f"
    "subsection:chaos_game"
    "chaosGame"
    "sec:org668f987"
    "sec:org4b1ea41"
    "subsection:chaos_game_gpu"
    "sec:org2dbddc6"
    "subsection:deterministic_gpu"
    "sec:org59cb855"
    "sec:org3703620"
    "sec:orgd1553e1"
    "sec:org14a72e9"
    "sec:org748d9ef"
    "subsection:point_cloud_optimizations"
    "sec:orgcf27e48"
    "subsection:self_similarity"
    "sec:org61f20fb"
    "sec:org351a067"
    "sec:org092a439"
    "sec:orgd1ce23b"
    "sec:org9636adb"
    "sec:orgb25c7f1"
    "sec:orgdfc92ae"
    "sec:org827203d"
    "sec:org67a4ce7"
    "sec:org3017905"
    "subsection:jumping_restrictions"
    "sec:orgee8e6c2"
    "sec:org7181b6e"
    "figigure:dragon_curve_a"
    "figigure:dragon_curve_b"
    "fig:five over x"
    "figigure:dragon_curve_d"
    "figure:dragon_curve_overlaps"
    "sec:org6314dd9"
    "subsection:coloring"
    "sec:orge1e3bd7"
    "sec:org96b225b"
    "sec:orge863faf"
    "sec:orgbcc6e9b")
   (LaTeX-add-bibliographies
    "bibliography")
   (LaTeX-add-newfloat-DeclareFloatingEnvironments
    '("lsystem" "")))
 :latex)

