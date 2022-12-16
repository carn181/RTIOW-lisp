(asdf:defsystem ray-tracing
  :author "snuck"
  :license "GNU"
  :depends-on (:3d-vectors)
  :components ((:file "package")
               (:file "ray")
               (:file "main")))
