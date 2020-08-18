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
    "sec:org763597a"
    "sec:org946c58c"
    "sec:orgd52b125"
    "sec:org7230d60"
    "section:background"
    "sec:org20c8eec"
    "subsection:informal_description"
    "figure:sierpinsky_iterations"
    "sec:orgc098f4d"
    "sec:org58199b0"
    "sec:org0778fa0"
    "subsection:viewport_transformation"
    "sec:orga6f9d53"
    "sec:org9a48ab1"
    "sec:org1f11ea1"
    "sec:org6067193"
    "sec:orgfe80f03"
    "subsection:chaos_game"
    "figure:barnsley_mil"
    "figure:barnsley_ten_mil"
    "figure:barnsley_chaos_game_points"
    "chaosGame"
    "sec:orga14efe2"
    "sec:org45ea8fd"
    "subsection:chaos_game_gpu"
    "sec:org98ed973"
    "subsection:deterministic_gpu"
    "sec:orgac6a772"
    "section:research_question"
    "sec:org320bdad"
    "section:approach"
    "sec:orgc3de48d"
    "sec:orgca1614e"
    "sec:org1587f39"
    "subsection:point_cloud_optimizations"
    "sec:orgb8c91fe"
    "subsection:self_similarity"
    "figure:sierpinsky_jump"
    "algorithm:self_similarity_jump_up"
    "algorithm:self_similarity_jump_down"
    "sec:orga9f7edc"
    "subsection:coloring"
    "sec:org899ef91"
    "figure:program_flow"
    "sec:org668be05"
    "sec:orge870605"
    "sec:org52d56f3"
    "listing:barnsley_fern_ifs_file"
    "sec:orgc7d68e2"
    "sec:orgcb7cc87"
    "sec:orga7b3bba"
    "sec:orgd473176"
    "figure:barnsley_guides"
    "figure:barnsley_guides_and_points"
    "figure:barnsley_guides_vs_points"
    "sec:org5aa40bd"
    "section:findings"
    "sec:org56bd208"
    "subsection:jumping_restrictions"
    "sec:org0cf9535"
    "figure:sierpinsky_transformation_borders"
    "sec:org7ae2f0a"
    "figure:barnsley_jump_a"
    "figure:barnsley_jump_b"
    "figure:barnsley_jump"
    "figure:dragon_curve_a"
    "figure:dragon_curve_b"
    "figure:dragon_curve_c"
    "figure:dragon_curve_d"
    "figure:dragon_curve_overlaps"
    "sec:orgdffee33"
    "sec:org8e531bc"
    "section:conclusion"
    "sec:org617cbfc"
    "section:further_work"
    "sec:orga0ec685"
    "ifs:sierpinsky"
    "ifs:dragon_curve"
    "ifs:barnsley_fern")
   (LaTeX-add-bibliographies
    "bibliography")
   (LaTeX-add-newfloat-DeclareFloatingEnvironments
    '("ifs" "")))
 :latex)

