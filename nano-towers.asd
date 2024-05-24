#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

(defsystem nano-towers
  :version "1.0.0"
  :author "Bohong Huang <1281299809@qq.com>"
  :maintainer "Bohong Huang <1281299809@qq.com>"
  :license "Apache-2.0"
  :homepage "https://github.com/bohonghuang/spring-lisp-game-jam-2024"
  :bug-tracker "https://github.com/bohonghuang/spring-lisp-game-jam-2024/issues"
  :source-control (:git "https://github.com/bohonghuang/spring-lisp-game-jam-2024.git")
  :entry-point "nano-towers:main"
  :build-operation program-op
  :depends-on (#:alexandria #:eon)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "constants")
                             (:file "utils")
                             (:file "scene")
                             (:module "game"
                              :serial t
                              :components ((:file "screen")
                                           (:file "tower")
                                           (:file "enemy")
                                           (:file "attack")
                                           (:file "logic")))
                             (:file "menu")))))
