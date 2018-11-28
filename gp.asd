(defsystem "gp"
  :description "Genetic Programming"
  :version "0.0.1"
  :author "Guillaume MICHEL"
  :mailto "contact@orilla.fr"
  :homepage "http://orilla.fr"
  :license  "MIT License (see COPYING)"
  :depends-on ("alexandria")
  :components ((:static-file "COPYING")
               (:static-file "README.md")
               (:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "random")
                                     (:file "kernel")
                                     ;;(:file "fast_eval")
                                     (:file "simplify")
                                     (:file "simplify_rules")))))
