(in-package #:ann)
(named-readtables:in-readtable rutilsx-readtable)


;;; files

(let ((base-dir (asdf:component-pathname (asdf:find-system :ann))))
  (defun local-file (&rest parts)
    "Compose PARTS into a pathname relevant to projects' base dir."
    (merge-pathnames (strjoin "/" parts)
                     base-dir)))

(defun find-meta-file (dir)
  "Find .ann.yaml file in local DIR."
  (do ((dir (strcat "data/" dir)
            (slice dir 0 (position #\/ dir :from-end t))))
      ((null (find #\/ dir))
       (local-file "data" ".ann.yaml"))
    (let ((file (local-file dir ".ann.yaml")))
      (when (probe-file file)
        (return file)))))
    
(defun read-file-with-anns (file)
  "Read and process anns from FILE."
  (with ((dir (slice file 0 (position #\/ file :from-end t)))
         (meta (read-meta (find-meta-file dir)))
         (ann-file (local-file "data" (strcat file ".ann"))))
    (unless (probe-file ann-file)
      (with-out-file (out ann-file)
        (write-line "" out)))
    (list (read-file (local-file "data" file))
          meta
          (decode-anns @meta.format (read-file ann-file)))))

;;; ann struct

(defstruct ann
  id beg end tag txt repr)

(defgeneric decode-anns (format str)
  (:documentation
   "Create ann structs from annotation in STR according to FORMAT."))

(defgeneric encode-anns (format anns &optional stream)
  (:documentation
   "Write ANNS to STREAM according to FORMAT."))

(defun anned-text (txt anns)
  "Format TXT with annotations from ANNS list."
  (re:regex-replace-all
   (fmt "~%")
   (with-output-to-string (out)
     (let ((off 0))
       (dolist (ann (sort anns '< :key 'ann-beg))
         (format out "~A<span class=\"ann-span ann-tag-~A\" id=\"ann-~A\" ~
                             beg=\"~A\" end=\"~A\">~A</span>"
                 (who:escape-string (slice txt off @ann.beg))
                 @ann.tag @ann.id @ann.beg @ann.end
                 (who:escape-string (slice txt @ann.beg @ann.end)))
         (:= off @ann.end))
       (write-string (who:escape-string (slice txt off)) out)))
   " <br>"))

(defun ann-htm (meta anns)
  "Format annotations from ANNS to html snippets according to their META spec."
  (who:with-html-output-to-string (out)
    (:ul (loop :for ann :in anns
               :for repr :in (split #\Newline
                                    (with-output-to-string (out)
                                      (encode-anns @meta.format anns out)))
               :do (who:htm
                    (:li :class "ann-li"
                         (:nobr
                          (:a :href "#" :onclick (fmt "rem_ann(~A)" @ann.id)
                              "✕")
                          " "
                          (:a :href "#" :onclick (fmt "ann_dialog(~A, \"~A\")"
                                                      @ann.id @ann.tag)
                              (who:str (if (> (length repr) 30)
                                           (strcat (slice repr 0 25) "<...>")
                                           repr))))))))))

;;; meta info

(defstruct meta
  format schema highlight (ext "txt"))

(defun yaml->struct (struct file)
  "Read meta yaml description from FILE an fill STRUCT."
  (dotable (k v (yaml:parse (read-file file)))
    (:= (? struct (mksym k)) (mkeyw v)))
  struct)

(let ((schema-cache #h()))
  (defun read-meta (file)
    "Read meta spec from FILE and the relevant schema from schemas/."
    (if (probe-file file)
        (with ((meta (yaml->struct (make-meta) file))
               (schema (getset# @meta.schema schema-cache
                                (yaml:parse (local-file (fmt "schemas/~(~A~).yaml"
                                                             @meta.schema))))))
          (:= (? schema :name) @meta.schema
              @meta.schema schema)
          meta)
        (error "No meta file (.ann.yaml)")))
  (defun clear-schema-cache ()
    "Clear schema cache."
    (clrhash schema-cache)))

(defun write-schema-stylesheet (meta)
  "Generate and write to the site/ directory a stylesheet for the META spec."
  (let ((file (local-file "site" (fmt "~A.css" (? @meta.schema :name)))))
    (with-out-file (out file)
      (dotable (tag info @meta.schema)
        (unless (keywordp tag)
          (format out "SPAN.ann-tag-~A { ~A: ~A;~@[ ~A;~] }~%"
                  tag (ecase @meta.highlight
                        (:font "color")
                        (:background "background-color")
                        (:underline "text-decoration-color"))
                  (case @meta.highlight
                    (:background (color->rgba (? info "color")))
                    (otherwise (? info "color")))
                  (when (eql @meta.highlight :underline)
                    "text-decoration: underline")))))
    file))

(defun color->rgba (color)
  "RGBA Values for main COLORs (total - 10)."
  (fmt "rgba(~A,0.3)"
       (ecase (mkeyw color)
         (:red "255,0,0")
         (:green "0,255,0")
         (:blue "0,0,255")
         (:aqua "0,255,255")
         (:brown "165,42,42")
         (:purple "128,0,128")
         (:orange "255,94,0")
         (:yellow "255,217,0")
         (:pink "214,37,152")
         (:grey "200,200,200"))))    
