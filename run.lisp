(push :prod *features*)

(push (merge-pathnames "../ann/") asdf:*central-registry*)

(ql:quickload :ann)

#+dev(hunch:start-web 7001)
