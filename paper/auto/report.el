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
    "sec:orge12556f"
    "sec:orgd2693d1"
    "sec:org6879d8e"
    "sec:org4991c46"
    "section:background"
    "sec:org914d586"
    "subsection:informal_description"
    "figure:sierpinsky_iterations"
    "sec:orga284b25"
    "sec:orgf0d640c"
    "sec:org149b231"
    "subsection:viewport_transformation"
    "sec:org2112530"
    "sec:org6318365"
    "sec:orgf3d0261"
    "sec:orgb4de8f6"
    "sec:orgf584cbb"
    "subsection:chaos_game"
    "figure:barnsley_mil"
    "figure:barnsley_ten_mil"
    "figure:barnsley_chaos_game_points"
    "chaosGame"
    "sec:org1b78490"
    "sec:org2d00920"
    "subsection:chaos_game_gpu"
    "sec:orge5387a9"
    "subsection:deterministic_gpu"
    "sec:org37fcf8e"
    "section:research_question"
    "sec:org944dc5f"
    "section:approach"
    "sec:orgf1bc0a4"
    "sec:org83c2c6e"
    "sec:orgb27e365"
    "subsection:point_cloud_optimizations"
    "sec:orgd39aa92"
    "subsection:self_similarity"
    "figure:sierpinsky_jump"
    "algorithm:self_similarity_jump_up"
    "algorithm:self_similarity_jump_down"
    "sec:org13ce70e"
    "subsection:coloring"
    "sec:org91d8b4f"
    "figure:program_flow"
    "sec:orgc8abaac"
    "sec:orgb1919ba"
    "sec:org65bcf51"
    "listing:barnsley_fern_ifs_file"
    "sec:org4c4def2"
    "sec:org12fa5f0"
    "sec:org70e1097"
    "sec:orgc10e4b4"
    "figure:barnsley_guides"
    "figure:barnsley_guides_and_points"
    "figure:barnsley_guides_vs_points"
    "sec:org5e7d69e"
    "section:findings"
    "sec:orge0d5f51"
    "subsection:jumping_restrictions"
    "sec:orgd0a6dd0"
    "figure:sierpinsky_transformation_borders"
    "sec:org021dace"
    "figure:barnsley_jump_a"
    "figure:barnsley_jump_b"
    "figure:barnsley_jump"
    "figure:dragon_curve_a"
    "figure:dragon_curve_b"
    "figure:dragon_curve_c"
    "figure:dragon_curve_d"
    "figure:dragon_curve_overlaps"
    "sec:org565f000"
    "sec:orgd36d221"
    "section:conclusion"
    "sec:org3e0097f"
    "section:further_work"
    "sec:org18e0326"
    "ifs:sierpinsky"
    "ifs:dragon_curve"
    "ifs:barnsley_fern")
   (LaTeX-add-bibliographies
    "bibliography")
   (LaTeX-add-newfloat-DeclareFloatingEnvironments
    '("ifs" "")))
 :latex)

