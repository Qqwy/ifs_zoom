(TeX-add-style-hook
 "report"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem") ("geometry" "a4paper" "total={7in, 9in}") ("algorithm2e" "ruled" "procnumbered") ("xcolor" "dvipsnames") ("enumitem" "shortlabels")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
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
    "listings"
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
    "sec:orgf1a4fc4"
    "sec:org3a01a6a"
    "sec:org7628d8a"
    "sec:orge520937"
    "section:background"
    "sec:org14fb05b"
    "figure:sierpinsky_iterations"
    "sec:org3e564f1"
    "sec:org5c55f60"
    "sec:org37b682c"
    "subsection:viewport_transformation"
    "sec:org9c0046c"
    "sec:org09257de"
    "sec:orga1103be"
    "subsection:chaos_game"
    "figure:barnsley_mil"
    "figure:barnsley_ten_mil"
    "figure:barnsley_chaos_game_points"
    "chaosGame"
    "sec:orgfd24cfe"
    "sec:orgbfb695c"
    "subsection:chaos_game_gpu"
    "sec:org2a9f800"
    "subsection:deterministic_gpu"
    "sec:org226c34e"
    "section:research_question"
    "sec:orged310ea"
    "section:approach"
    "sec:org4811b68"
    "sec:org8d83c24"
    "sec:org1377317"
    "subsection:point_cloud_optimizations"
    "sec:org697e525"
    "subsection:self_similarity"
    "figure:sierpinsky_jump"
    "algorithm:self_similarity_jump_up"
    "algorithm:self_similarity_jump_down"
    "sec:orgae40080"
    "subsection:coloring"
    "sec:org8fb5d2f"
    "figure:program_flow"
    "sec:org650a312"
    "sec:org197a1c5"
    "sec:orgb7f1b2d"
    "listing:barnsley_fern_ifs_file"
    "sec:org0b8df97"
    "sec:orge3d9b9e"
    "sec:orgaa4ba2b"
    "sec:org20074ea"
    "figure:barnsley_guides"
    "figure:barnsley_guides_and_points"
    "figure:barnsley_guides_vs_points"
    "sec:org0ca7f19"
    "section:findings"
    "sec:org60f2bac"
    "subsection:jumping_restrictions"
    "sec:orgd7482bf"
    "figure:sierpinsky_transformation_borders"
    "sec:org90d10f1"
    "figure:barnsley_jump_a"
    "figure:barnsley_jump_b"
    "figure:barnsley_jump"
    "figure:dragon_curve_a"
    "figure:dragon_curve_b"
    "figure:dragon_curve_c"
    "figure:dragon_curve_d"
    "figure:dragon_curve_overlaps"
    "sec:orge8ec75e"
    "sec:orgde192ca"
    "section:conclusion"
    "sec:orgb4cf905"
    "section:further_work"
    "sec:org9b21927"
    "ifs:sierpinsky"
    "ifs:dragon_curve"
    "ifs:barnsley_fern")
   (LaTeX-add-bibliographies
    "bibliography")
   (LaTeX-add-newfloat-DeclareFloatingEnvironments
    '("ifs" "")))
 :latex)

