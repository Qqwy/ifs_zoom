(TeX-add-style-hook
 "report"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem") ("geometry" "a4paper" "total={7in, 9in}") ("caption" "font={small, it}" "labelfont=bf") ("algorithm2e" "ruled" "procnumbered") ("xcolor" "dvipsnames") ("enumitem" "shortlabels")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
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
    "caption"
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
    "sec:orgfee208a"
    "sec:orgfa7e82a"
    "sec:org6c4d6bc"
    "sec:org3b4b690"
    "section:background"
    "sec:org75f0239"
    "figure:sierpinsky_iterations"
    "sec:orgb0012b2"
    "sec:org0a93b42"
    "sec:org28a3a19"
    "subsection:viewport_transformation"
    "sec:org66d5d21"
    "sec:orga0aa444"
    "sec:org7ff9fbd"
    "sec:org1202c96"
    "sec:orgd50a544"
    "subsection:chaos_game"
    "figure:barnsley_mil"
    "figure:barnsley_ten_mil"
    "figure:barnsley_chaos_game_points"
    "chaosGame"
    "sec:orgaba79a4"
    "sec:orgdbcdfb4"
    "subsection:chaos_game_gpu"
    "sec:org8530ebf"
    "subsection:deterministic_gpu"
    "sec:orgcd35dcb"
    "section:research_question"
    "sec:org674aab3"
    "section:approach"
    "sec:org1f92692"
    "sec:org950fda9"
    "sec:org56fe4d5"
    "subsection:point_cloud_optimizations"
    "sec:orgb6a65a4"
    "subsection:self_similarity"
    "figure:sierpinsky_jump"
    "algorithm:self_similarity_jump_up"
    "algorithm:self_similarity_jump_down"
    "sec:orgfa5676f"
    "subsection:coloring"
    "sec:org328574a"
    "figure:program_flow"
    "sec:orgce3c643"
    "sec:org3bf8231"
    "sec:org06ac1df"
    "listing:barnsley_fern_ifs_file"
    "sec:org9670a41"
    "sec:orgacf59da"
    "sec:orgbc25dde"
    "sec:org2e48682"
    "figure:barnsley_guides"
    "figure:barnsley_guides_and_points"
    "figure:barnsley_guides_vs_points"
    "sec:orgc22ceff"
    "section:findings"
    "sec:orge992bee"
    "subsection:jumping_restrictions"
    "sec:org49c4b88"
    "figure:sierpinsky_transformation_borders"
    "sec:orgc0713be"
    "figure:barnsley_jump_a"
    "figure:barnsley_jump_b"
    "figure:barnsley_jump"
    "figure:dragon_curve_a"
    "figure:dragon_curve_b"
    "figure:dragon_curve_c"
    "figure:dragon_curve_d"
    "figure:dragon_curve_overlaps"
    "sec:org0b6489a"
    "sec:org5942d2a"
    "section:conclusion"
    "sec:org7fbfc0f"
    "section:further_work"
    "sec:orge17e93c"
    "ifs:sierpinsky"
    "ifs:dragon_curve"
    "ifs:barnsley_fern")
   (LaTeX-add-bibliographies
    "bibliography")
   (LaTeX-add-newfloat-DeclareFloatingEnvironments
    '("ifs" "")))
 :latex)

