(defsystem spring-lisp-game-jam-2024
  :version "1.0.0"
  :author "Bohong Huang <1281299809@qq.com>"
  :maintainer "Bohong Huang <1281299809@qq.com>"
  :license "Apache-2.0"
  :homepage "https://github.com/bohonghuang/spring-lisp-game-jam-2024"
  :bug-tracker "https://github.com/bohonghuang/spring-lisp-game-jam-2024/issues"
  :source-control (:git "https://github.com/bohonghuang/spring-lisp-game-jam-2024.git")
  :depends-on (#:alexandria #:eon)
  :components ((:module "src"
                :components ((:file "package")))))
