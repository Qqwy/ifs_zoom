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
    "sec:orgc6d8fce"
    "sec:orga8afed3"
    "sec:org27169ba"
    "sec:org1dcb8fc"
    "section:background"
    "sec:org7ba5904"
    "figure:sierpinsky_iterations"
    "sec:org24e6850"
    "sec:org3bb0e71"
    "sec:orga7d85b7"
    "subsection:viewport_transformation"
    "sec:orgb3f1037"
    "sec:org9b05243"
    "sec:org33fd212"
    "subsection:chaos_game"
    "figure:barnsley_mil"
    "figure:barnsley_ten_mil"
    "figure:barnsley_chaos_game_points"
    "chaosGame"
    "sec:orge982078"
    "sec:org084431d"
    "subsection:chaos_game_gpu"
    "sec:orgb1f39b4"
    "subsection:deterministic_gpu"
    "sec:org2b50a11"
    "section:research_question"
    "sec:orgb20be57"
    "section:approach"
    "sec:org1734547"
    "sec:org0350409"
    "sec:org0e745e2"
    "subsection:point_cloud_optimizations"
    "sec:org5b5a6f5"
    "subsection:self_similarity"
    "sec:orgb08e2cf"
    "subsection:coloring"
    "sec:org906b022"
    "figure:program_flow"
    "sec:org6e09222"
    "sec:org06daff4"
    "sec:orgb12d0ae"
    "sec:orgf881a18"
    "sec:org5e758da"
    "sec:orgcd56b9c"
    "sec:orge5d5076"
    "figure:barnsley_guides"
    "figure:barnsley_guides_and_points"
    "figure:barnsley_guides_vs_points"
    "sec:org7531238"
    "section:findings"
    "sec:orgd3d8613"
    "subsection:jumping_restrictions"
    "sec:org934c1ba"
    "sec:org1aebb40"
    "figure:dragon_curve_a"
    "figure:dragon_curve_b"
    "figure:dragon_curve_c"
    "figure:dragon_curve_d"
    "figure:dragon_curve_overlaps"
    "sec:orga481eb4"
    "sec:org382d045"
    "section:conclusion"
    "sec:org2f5b781"
    "section:further_work"
    "sec:orgef3f2cf"
    "ifs:sierpinsky"
    "ifs:barnsley_fern"
    "ifs:dragon_curve")
   (LaTeX-add-bibliographies
    "bibliography")
   (LaTeX-add-newfloat-DeclareFloatingEnvironments
    '("lsystem" "")))
 :latex)

