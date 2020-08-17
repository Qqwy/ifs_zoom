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
    "sec:orgcb8b41f"
    "sec:org51dc968"
    "sec:orgff721bc"
    "sec:org031b741"
    "section:background"
    "sec:org18d16e4"
    "subsection:informal_description"
    "figure:sierpinsky_iterations"
    "sec:orgdff02a2"
    "sec:orgec4087d"
    "sec:org9f223c8"
    "subsection:viewport_transformation"
    "sec:orgf5c8cf7"
    "sec:org511cb61"
    "sec:orge354be0"
    "sec:orgee08ec2"
    "sec:org1366fb1"
    "subsection:chaos_game"
    "figure:barnsley_mil"
    "figure:barnsley_ten_mil"
    "figure:barnsley_chaos_game_points"
    "chaosGame"
    "sec:orgdd971c3"
    "sec:org7561620"
    "subsection:chaos_game_gpu"
    "sec:orgf04a355"
    "subsection:deterministic_gpu"
    "sec:org1766f4f"
    "section:research_question"
    "sec:org6dbd2ad"
    "section:approach"
    "sec:org397c997"
    "sec:org605c5bc"
    "sec:orgc78854d"
    "subsection:point_cloud_optimizations"
    "sec:org72f5c1b"
    "subsection:self_similarity"
    "figure:sierpinsky_jump"
    "algorithm:self_similarity_jump_up"
    "algorithm:self_similarity_jump_down"
    "sec:orgf57dac9"
    "subsection:coloring"
    "sec:org7f0e21e"
    "figure:program_flow"
    "sec:org4109ae0"
    "sec:orgf88544e"
    "sec:org63e56c1"
    "listing:barnsley_fern_ifs_file"
    "sec:org7f8e85a"
    "sec:org7f46520"
    "sec:org61c50dd"
    "sec:org6c9a373"
    "figure:barnsley_guides"
    "figure:barnsley_guides_and_points"
    "figure:barnsley_guides_vs_points"
    "sec:org35b596f"
    "section:findings"
    "sec:org2c2af45"
    "subsection:jumping_restrictions"
    "sec:orgecc5213"
    "figure:sierpinsky_transformation_borders"
    "sec:orgf39a2b2"
    "figure:barnsley_jump_a"
    "figure:barnsley_jump_b"
    "figure:barnsley_jump"
    "figure:dragon_curve_a"
    "figure:dragon_curve_b"
    "figure:dragon_curve_c"
    "figure:dragon_curve_d"
    "figure:dragon_curve_overlaps"
    "sec:orgf418747"
    "sec:orgdd553dc"
    "section:conclusion"
    "sec:orgfd0d708"
    "section:further_work"
    "sec:org89ef2ee"
    "ifs:sierpinsky"
    "ifs:dragon_curve"
    "ifs:barnsley_fern")
   (LaTeX-add-bibliographies
    "bibliography")
   (LaTeX-add-newfloat-DeclareFloatingEnvironments
    '("ifs" "")))
 :latex)

