;;; DO NOT MODIFY THIS FILE

;;; cogre-loaddefs.el --- Auto-generated CEDET autoloads

;;;### (autoloads (cogre-mode) "cogre-mode" "cogre/cogre-mode.el")

(autoload 'cogre-mode "cogre-mode" "\
Connected Graph Editor Mode.
\\{cogre-mode-map}" t nil)

;;;***

;;;### (autoloads (cogre-load-graph cogre) "cogre" "cogre/cogre.el")

(autoload 'cogre "cogre" "\
Create a new graph with the Connected Graph Editor.
The new graph will be given NAME.  See `cogre-mode' for details.
Optional argument GRAPH-CLASS indicates the type of graph to create." t nil)

(autoload 'cogre-load-graph "cogre" "\
Load a graph from FILE into a new graph buffer." t nil)

;;;***

;;;### (autoloads (cogre-uml-create cogre-uml-quick-class) "uml-create" "cogre/uml-create.el")

(autoload 'cogre-uml-quick-class "uml-create" "\
Create a new UML diagram based on CLASS showing only immediate lineage.
The parent to CLASS, CLASS, and all of CLASSes children will be shown." t nil)

(autoload 'cogre-uml-create "uml-create" "\
Create a new UML diagram, with CLASS as the root node.
CLASS must be a type in the current project." t nil)

;;;***

;;;### (autoloads (wisent-dot-setup-parser) "wisent-dot" "cogre/wisent-dot.el")

(autoload 'wisent-dot-setup-parser "wisent-dot" "\
Setup buffer for parse." nil nil)

(add-hook 'graphviz-dot-mode-hook 'wisent-dot-setup-parser)

;;;***


