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
    "sec:org2712a28"
    "sec:org3235ce9"
    "sec:orgeb78942"
    "sec:org46a937f"
    "section:background"
    "sec:org0ef95f4"
    "subsection:informal_description"
    "figure:sierpinsky_iterations"
    "sec:org8a8d0be"
    "sec:orgb040e35"
    "sec:org8b2b574"
    "subsection:viewport_transformation"
    "sec:org6928c59"
    "sec:orgde90512"
    "sec:org89ff80f"
    "sec:org9b190b6"
    "sec:orga58ca6e"
    "subsection:chaos_game"
    "figure:barnsley_mil"
    "figure:barnsley_ten_mil"
    "figure:barnsley_chaos_game_points"
    "chaosGame"
    "sec:org0b92ca0"
    "sec:org8486d22"
    "subsection:chaos_game_gpu"
    "sec:org16f9088"
    "subsection:deterministic_gpu"
    "sec:org575d501"
    "section:research_question"
    "sec:orga42fad4"
    "section:approach"
    "sec:org584834f"
    "sec:org517f5e8"
    "sec:org11cf15f"
    "subsection:point_cloud_optimizations"
    "sec:org577a6a4"
    "subsection:self_similarity"
    "figure:sierpinsky_jump"
    "algorithm:self_similarity_jump_up"
    "algorithm:self_similarity_jump_down"
    "sec:org9d35f79"
    "subsection:coloring"
    "sec:org4b6676e"
    "figure:program_flow"
    "sec:org96125f5"
    "sec:orgda288ec"
    "sec:orgce371a3"
    "listing:barnsley_fern_ifs_file"
    "sec:org8efef64"
    "sec:org0c796e6"
    "sec:org9d673fb"
    "sec:orge49fb67"
    "figure:barnsley_guides"
    "figure:barnsley_guides_and_points"
    "figure:barnsley_guides_vs_points"
    "sec:org7d3b420"
    "section:findings"
    "sec:orgc8a3b66"
    "subsection:jumping_restrictions"
    "sec:org36c880e"
    "figure:sierpinsky_transformation_borders"
    "sec:orga3a4183"
    "figure:barnsley_jump_a"
    "figure:barnsley_jump_b"
    "figure:barnsley_jump"
    "figure:dragon_curve_a"
    "figure:dragon_curve_b"
    "figure:dragon_curve_c"
    "figure:dragon_curve_d"
    "figure:dragon_curve_overlaps"
    "sec:org4d0e382"
    "sec:orgcce44a9"
    "section:conclusion"
    "sec:org0f8c541"
    "section:further_work"
    "sec:org25f629d"
    "ifs:sierpinsky"
    "ifs:dragon_curve"
    "ifs:barnsley_fern")
   (LaTeX-add-bibliographies
    "bibliography")
   (LaTeX-add-newfloat-DeclareFloatingEnvironments
    '("ifs" "")))
 :latex)

