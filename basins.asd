;;;; basins.asd

(asdf:defsystem #:basins
  :description "Describe basins here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :depends-on (#:cepl #:rtg-math.vari #:cepl.sdl2 #:swank #:livesupport #:cepl.skitter.sdl2)
  :components ((:module "src"
                :components ((:file "package")
                             (:file "parameters" :depends-on ("package"))
                             (:file "functions" :depends-on ("package"))
                             (:file "state" :depends-on ("package"
                                                         "functions"))
                             (:file "shaders" :depends-on ("package"))
                             (:file "camera" :depends-on ("package"
                                                          "parameters"))
                             (:file "geometry" :depends-on ("package"
                                                            "parameters"
                                                            "functions"))
                             (:file "balls" :depends-on ("package"
                                                         "parameters"))
                             (:file "drawing" :depends-on ("package"
                                                           "parameters"
                                                           "state"
                                                           "shaders"))
                             (:file "setup-cleanup" :depends-on ("package"
                                                                 "state"
                                                                 "geometry"))
                             (:file "basins" :depends-on ("package"
                                                          "parameters"
                                                          "shaders"
                                                          "camera"
                                                          "state"
                                                          "geometry"
                                                          "balls"
                                                          "drawing"
                                                          "setup-cleanup"))))))
