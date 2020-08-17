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
    "sec:orgfc268e0"
    "sec:orgc7e62ce"
    "sec:org87bdeb7"
    "sec:org2a53c12"
    "section:background"
    "sec:org7d6652e"
    "subsection:informal_description"
    "figure:sierpinsky_iterations"
    "sec:org1dc2d4e"
    "sec:org58fc020"
    "sec:orgd746a05"
    "subsection:viewport_transformation"
    "sec:org784e20e"
    "sec:org48e378f"
    "sec:orgf2adbb5"
    "sec:org0a99a97"
    "sec:orgaf5e985"
    "subsection:chaos_game"
    "figure:barnsley_mil"
    "figure:barnsley_ten_mil"
    "figure:barnsley_chaos_game_points"
    "chaosGame"
    "sec:org5358822"
    "sec:orgd3f02ce"
    "subsection:chaos_game_gpu"
    "sec:orge3055ee"
    "subsection:deterministic_gpu"
    "sec:orgd180160"
    "section:research_question"
    "sec:org5ac34a7"
    "section:approach"
    "sec:orgde05689"
    "sec:orgabae298"
    "sec:org3ef1011"
    "subsection:point_cloud_optimizations"
    "sec:org9d7f8a7"
    "subsection:self_similarity"
    "figure:sierpinsky_jump"
    "algorithm:self_similarity_jump_up"
    "algorithm:self_similarity_jump_down"
    "sec:orgf478668"
    "subsection:coloring"
    "sec:org3fa8acf"
    "figure:program_flow"
    "sec:org300ad60"
    "sec:org64ef018"
    "sec:orgfd1de64"
    "listing:barnsley_fern_ifs_file"
    "sec:orgdac84ff"
    "sec:orgae588bd"
    "sec:orgf8fc749"
    "sec:org0caf9bb"
    "figure:barnsley_guides"
    "figure:barnsley_guides_and_points"
    "figure:barnsley_guides_vs_points"
    "sec:orgd9bc11d"
    "section:findings"
    "sec:org0e87124"
    "subsection:jumping_restrictions"
    "sec:org28da5da"
    "figure:sierpinsky_transformation_borders"
    "sec:org12590be"
    "figure:barnsley_jump_a"
    "figure:barnsley_jump_b"
    "figure:barnsley_jump"
    "figure:dragon_curve_a"
    "figure:dragon_curve_b"
    "figure:dragon_curve_c"
    "figure:dragon_curve_d"
    "figure:dragon_curve_overlaps"
    "sec:org46ee366"
    "sec:orgcdf44f8"
    "section:conclusion"
    "sec:orgb855850"
    "section:further_work"
    "sec:orgc3d479d"
    "ifs:sierpinsky"
    "ifs:dragon_curve"
    "ifs:barnsley_fern")
   (LaTeX-add-bibliographies
    "bibliography")
   (LaTeX-add-newfloat-DeclareFloatingEnvironments
    '("ifs" "")))
 :latex)

