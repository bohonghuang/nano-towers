#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

(defsystem nano-towers
  :version "1.0.0"
  :author "Bohong Huang <1281299809@qq.com>"
  :maintainer "Bohong Huang <1281299809@qq.com>"
  :license "Apache-2.0"
  :homepage "https://github.com/bohonghuang/nano-towers"
  :bug-tracker "https://github.com/bohonghuang/nano-towers/issues"
  :source-control (:git "https://github.com/bohonghuang/nano-towers.git")
  :entry-point "nano-towers:main"
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "nano-towers"
  :depends-on (#:alexandria #:eon)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "constants")
                             (:file "utils")
                             (:file "ui")
                             (:file "scene")
                             (:module "game"
                              :serial t
                              :components ((:file "screen")
                                           (:file "tower")
                                           (:file "enemy")
                                           (:file "attack")
                                           (:file "logic")))
                             (:file "menu")))))
