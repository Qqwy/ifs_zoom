(TeX-add-style-hook
 "report"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem") ("geometry" "a4paper" "total={7in, 9in}") ("caption" "font={small, it}" "labelfont=bf") ("algorithm2e" "ruled" "procnumbered") ("xcolor" "dvipsnames") ("enumitem" "shortlabels")))
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
    "sec:org04c15a0"
    "sec:org75d2474"
    "sec:org3b0cfb1"
    "sec:orgec09bfb"
    "section:background"
    "sec:org72376eb"
    "subsection:informal_description"
    "figure:sierpinsky_iterations"
    "sec:org8b3b4e5"
    "sec:orgfc0fde5"
    "sec:org7497b89"
    "subsection:viewport_transformation"
    "sec:org1b436cc"
    "sec:org0d39927"
    "sec:org80e9b2d"
    "sec:org612a065"
    "sec:org9211c90"
    "subsection:chaos_game"
    "figure:barnsley_mil"
    "figure:barnsley_ten_mil"
    "figure:barnsley_chaos_game_points"
    "chaosGame"
    "sec:org1e2a1d9"
    "sec:org8ff5dd7"
    "subsection:chaos_game_gpu"
    "sec:orgc8e276c"
    "subsection:deterministic_gpu"
    "sec:orgad0a8e7"
    "section:research_question"
    "sec:orgb30e6c0"
    "section:approach"
    "sec:orgfceb12e"
    "sec:org36966c2"
    "sec:orga206bf4"
    "subsection:point_cloud_optimizations"
    "sec:org3d23f27"
    "subsection:self_similarity"
    "figure:sierpinsky_jump"
    "algorithm:self_similarity_jump_up"
    "algorithm:self_similarity_jump_down"
    "sec:org4d3e635"
    "subsection:coloring"
    "sec:org7da9814"
    "figure:program_flow"
    "sec:orgb49babf"
    "sec:org92fce80"
    "sec:org5a6f9df"
    "listing:barnsley_fern_ifs_file"
    "sec:org4a4c4fa"
    "sec:orgee448b6"
    "sec:orgc2f9f7f"
    "sec:org085f210"
    "figure:barnsley_guides"
    "figure:barnsley_guides_and_points"
    "figure:barnsley_guides_vs_points"
    "sec:org2746c6d"
    "section:findings"
    "sec:org0060cc1"
    "subsection:jumping_restrictions"
    "sec:orge7fea29"
    "figure:sierpinsky_transformation_borders"
    "sec:org25cdafb"
    "figure:barnsley_jump_a"
    "figure:barnsley_jump_b"
    "figure:barnsley_jump"
    "figure:dragon_curve_a"
    "figure:dragon_curve_b"
    "figure:dragon_curve_c"
    "figure:dragon_curve_d"
    "figure:dragon_curve_overlaps"
    "sec:org025a89e"
    "sec:org6376d4b"
    "section:conclusion"
    "sec:orge62c610"
    "section:further_work"
    "sec:org3cd5143"
    "ifs:sierpinsky"
    "ifs:dragon_curve"
    "ifs:barnsley_fern")
   (LaTeX-add-bibliographies
    "bibliography")
   (LaTeX-add-newfloat-DeclareFloatingEnvironments
    '("ifs" "")))
 :latex)

