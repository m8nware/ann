(in-package #:ann)
(named-readtables:in-readtable rutilsx-readtable)


(defmacro std-page ((&key title style script) &body body)
  "Standard page template."
  `(who:with-html-output-to-string (out)
     (:html
      (:head
       (:title (who:str ,title))
       (:link :rel "stylesheet" :type "text/css" :href "/static/main.css")
       (:link :rel "icon" :type "image/png" :href "/static/favico.png")
       (when ',style
         (who:htm (:link :rel "stylesheet" :type "text/css"
                         :href (fmt "/static/~A.css" ,style))))
       (:script :type "text/javascript" :src "/static/jquery-1.11.3.js" "")
       (:script :type "text/javascript" :src "/static/main.js" "")
       (when ',script
         (who:htm (:script :type "text/javascript"
                           :src (fmt "/static/~A.js" ,script) ""))))
      (:body
       (:div :class "page"
             ,@body)
       (:div :class "footer"
        (:span :class "tagline"
         (who:fmt "&nbsp; ~A (m8n) &ldquo;Ann assistant at your service&rdquo; &nbsp;"
                  (nth-value 5 (decode-universal-time (get-universal-time))))))))))

(defmacro file-nav (prefix files pos &optional suffix)
  (with-gensyms (count)
    `(let ((,count (length ,files)))
       (who:with-html-output-to-string (out)
         (:div :class "file-nav"
               (when (> ,count 1)
                 (who:htm
                  (:a :class "nav"
                      :href (fmt "/~A/~A~@[/~A~]" ,prefix
                                 (who:escape-string
                                  (? ,files (1- (if (plusp ,pos)
                                                    ,pos
                                                    ,count))))
                                 ,suffix)
                      "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;◄&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")))
               (:nobr "     ")
               (who:fmt "~A (~A/~A)" file (1+ ,pos) ,count)
               (when (> ,count 1)
                 (who:htm
                  (:a :class "nav"
                      :href (fmt "/~A/~A~@[/~A~]" ,prefix
                                 (who:escape-string
                                  (? ,files (if (= ,pos (1- ,count))
                                                0
                                                (1+ ,pos))))
                                 ,suffix)
                      "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;►&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"))))))))

(url "/static/:file" (file)
  (htt:handle-static-file (local-file "site" file)))


;;; annotation

(url "/" (dir)
  (unless (or (null dir)
              (ends-with "/" dir))
    (:= dir (strcat dir "/")))
  (std-page (:title "Ann welcomes you")
    (:div (:img :src "/static/ann.jpg" :style "float:left; margin-right:20px;")
          "Welcome to Ann, an annotation assistant."
          :br "Please, select file to annotate:")
    (if-it (let ((dir (strcat "data" (if dir (strcat "/" dir) ""))))
             (set-difference (directory (local-file dir "*.*"))
                             (cons (local-file dir ".ann.yaml")
                                   (directory (local-file dir "*.ann")))
                             :test 'equal))
           (who:htm
            (:div :class "columns"
             (dolist (file (sort (mapcar ^(if-it (pathname-name %)
                                                 (fmt "~A.~A"
                                                      it (pathname-type %))
                                                 (strcat
                                                  (last1 (pathname-directory %))
                                                  "/"))
                                         it)
                                 'string< :key ^(if (ends-with "/" %)
                                                    (strcat #\Nul %)
                                                    %)))
               (who:htm
                (:a :href (who:escape-string (strcat "/ann/" dir file))
                    (who:str (if (> (length file) 30)
                                 (fmt "~A..~A"
                                      (slice file 0 14)
                                      (substr file -15))
                                 file))
                    (:br))))))
           (who:htm (:blockquote
                     ":( Nothing. You need to add something to /data/ dir.")))))

(defun list-ann-files (file meta)
  (sort (mapcar ^(fmt "~{~A/~}~A.~A"
                      (subseq (pathname-directory %)
                              (1+ (position "data" (pathname-directory %)
                                            :test 'string=)))
                      (pathname-name %)
                      (pathname-type %))
                (uiop:directory*
                 (local-file "data"
                             (fmt "~A/*.~(~A~)"
                                  (slice file 0
                                         (position #\/ file
                                                   :from-end t))
                                  @meta.ext))))
        'string<))

(url "/ann/:file" (file)
  (require-auth file)
  (if-it (probe-file (local-file "data" file))
         (if (uiop:directory-pathname-p it)
             (handle-/ :dir file)
             (with (((raw meta anns) (read-file-with-anns file))
                    (all-files (list-ann-files file meta))
                    (file-pos (position file all-files :test 'string=)))
               (unless file-pos
                 (htt:abort-request-handler
                  (princ-to-string (:= (htt:return-code*) htt:+http-not-found+))))
               (std-page (:title (fmt "Ann processing: ~A" file)
                          :style (? @meta.schema :name)
                          :script "ann")
                 (:div :id "modal" :class "modal"
                  (:div :class "modal-content"
                   (:span :class "close" "✕")
                   (:form
                    (:input :type "hidden" :id "modal-hid" :value "")
                    (:radiogroup
                     (let (start)
                       (dotable (k v @meta.schema)
                         (unless (keywordp k)
                           (who:htm
                            (:label
                             (:input :type "radio" :class "radio"
                                     :style "display: none"
                                     :onclick "return post_ann()"
                                     :id (fmt "radio-~A" k) :value k
                                     (:span :style (fmt "color: ~A"
                                                        (? v "color"))
                                            (who:fmt "~A (~A)" k
                                                     (slice (? v "desc") 0
                                                            (min 30 (length
                                                                     (? v "desc"))))))))
                            (:br)))
                         (void start))))
                    (:br)
                    (:input :type "button" :id "modal-del"
                            :value "Remove" :disabled "disabled"))))
                 (who:str (file-nav "ann" all-files file-pos))
                 (:table
                  (:tr
                   (:th "Text")
                   (:th :style "min-width: 400px;"
                        "Annotations"))
                  (:tr
                   (:td
                    (:div :id "txt-data"
                          :contenteditable t :onmouseup "ann_dialog()"
                          (who:str (anned-text raw anns))))
                   (:td
                    (:div :id "ann-data"
                          (who:str (ann-htm meta anns)))))))))
         (htt:abort-request-handler
          (princ-to-string (:= (htt:return-code*) htt:+http-not-found+)))))
             
(url "/post/:file" (file)
  (require-auth file)
  (when (eql :POST (mkeyw (htt:request-method*)))
    (with ((data (yason:parse (htt:raw-post-data :force-text t)))
           ((txt meta anns) (read-file-with-anns file)))
      (ecase (mkeyw (? data "action"))
        (:add (with (((beg end) (mapcar 'parse-integer
                                        (split #\, (? data "span")))))
                (when (> beg end)
                  (:= end (length txt)))
                (do ((i beg (1+ i)))
                    ((or (>= i end)
                         (not (char= #\Space (char txt i)))))
                  (:+ beg))
                (do ((i end (1- i)))
                    ((or (< i beg)
                         (not (char= #\Space (char txt (1- i))))))
                  (:- end))
                (unless (= beg end)
                  (:= anns (remove-if ^(and (= beg (ann-beg %))
                                            (= end (ann-end %)))
                                      anns))
                  (push (make-ann :beg beg :end end
                                  :tag (mkeyw (? data "tag"))
                                  :txt (slice txt beg end))
                        anns))
                (:= anns (sort anns '< :key 'ann-beg))
                (doindex (i ann anns)
                  (:= @ann.id (1+ i)))))
        (:rem (:= anns (remove-if ^(= @%.id (? data "id"))
                                  anns))))
      (with-out-file (ann-file (local-file "data" (strcat file ".ann")))
        (write-string (with-output-to-string (out)
                        (encode-anns @meta.format anns out))
                      ann-file))
      (json:encode-json-to-string
       #h("txt" (anned-text txt anns)
          "ann" (ann-htm meta anns))))))


;;; diffing

#+Nil
(url "/" (dir)
  (unless (or (null dir)
              (ends-with "/" dir))
    (:= dir (strcat dir "/")))
  (std-page (:title "Ann welcomes you")
    (:div (:img :src "/static/ann.jpg" :style "float:left; margin-right:20px;")
          "Welcome to Ann, an annotation assistant."
          :br "Please, select file to review:")
    (if-it (let ((dir (strcat "data" (if dir (strcat "/" dir) ""))))
             (append (directory (local-file dir "*.txt"))
                     (directory (local-file dir "*/"))))
           (who:htm
            (:div :class "columns"
                  (dolist (file (sort (mapcar ^(if-it (pathname-name %)
                                                      (fmt "~A.~A"
                                                           it (pathname-type %))
                                                      (strcat
                                                       (last1 (pathname-directory %))
                                                       "/"))
                                              it)
                                      'string< :key ^(if (ends-with "/" %)
                                                         (strcat #\Nul %)
                                                         %)))
                    (who:htm
                     (:a :href (who:escape-string (strcat "/diff/" dir file))
                         (who:str (if (> (length file) 30)
                                      (fmt "~A..~A"
                                           (slice file 0 14)
                                           (substr file -15))
                                      file))
                         (:br))))))
           (who:htm (:blockquote
                     ":( Nothing. You need to add something to /data/ dir.")))))

(defun diff-page (file ann1 ann2 &optional ann3)
  (with ((ann/file (fmt "~A/~A" ann1 file))
         ((raw meta _) (read-file-with-anns ann/file))
         (all-files (list-ann-files ann/file meta)))
    (std-page (:title "Ann review mode on"
               :style (? @meta.schema :name)
               :script "diff")
      (who:str (file-nav "diff"
                         all-files
                         (position ann/file all-files :test 'string=)
                         ann2))
      (:table
       (:tr
        (:th "Text")
        (:th :style "min-width: 400px;"
             "Annotations"))
       (:tr
        (:td
         (:div :id "txt-data" :contenteditable t
               (who:str raw)))
        (:td
         (:div :id "ann-data"
               (who:str (diff-htm meta
                                  (local-file "data" ann1 file)
                                  (local-file "data" ann2 file)
                                  (when ann3
                                    (local-file "data" ann3 file))))
               (:form :method "POST" :style "text-align: center;"
                      :onsubmit
                      (fmt "return post_diff(\"~A/~A/~A~@[/~A~]\");"
                           ann1 file ann2 ann3)
                      (:button :type "submit" :style "font-size: large;"
                               "Record diff")))))))))

(url "/diff/:ann1/:file/:ann2" (ann1 file ann2)
  (require-auth "admin")
  (with (((ann2 &optional ann3) (split #\/ ann2)))
    (if (and (probe-file (local-file "data" ann1 file))
             (probe-file (local-file "data" ann2 file))
             (or (not ann3)
                 (probe-file (local-file "data" ann3 file))))
        (ecase (mkeyw (htt:request-method*))
          (:GET (diff-page file ann1 ann2 ann3))
          (:POST
           (with ((diff-dir (local-file "data" "diff" (fmt "~A-~A~@[-~A~]/"
                                                           ann1 ann2 ann3)))
                  (to-rem #h())
                  (ann1-t 0) (ann2-t 0) (ann3-t 0)
                  (ann1-f 0) (ann2-f 0) (ann3-f 0)
                  (ann1=ann2 0) (ann1=ann3 0) (ann2=ann3 0)
                  ((to-rem-raw cat-chgs) (yason:parse
                                          (htt:raw-post-data :force-text t)))
                  (i 0))
             (ensure-directories-exist diff-dir)
             (loop :for (id i) :in to-rem-raw :do
               (:+ (get# id to-rem 0) i))
             (with-output-to-string (stats)
               (format stats "~%~%# ~A~%~%SIZE: ~A" file
                       (length (read-file (local-file "data" ann1 file))))
               (with-out-file (out (fmt "~A~A.ann" diff-dir file))
                 (dolist (diff (diff-files (local-file "data" ann1
                                                       (strcat file ".ann"))
                                           (local-file "data" ann2
                                                       (strcat file ".ann"))
                                           (when ann3
                                             (local-file "data" ann3
                                                         (strcat file ".ann")))))
                   (when-it (? cat-chgs (princ-to-string @diff.id))
                     (when @diff.l1 (:= @diff.l1.cat it))
                     (when @diff.l2 (:= @diff.l2.cat it))
                     (when @diff.l3 (:= @diff.l3.cat it)))
                   (case (? to-rem @diff.id)
                     (1 (:+ ann1-f)
                        (when-it (or @diff.l2 @diff.l3)
                          (format-ann-line out (:+ i) it)
                          (when @diff.l2 (:+ ann2-t))
                          (when @diff.l3 (:+ ann3-t))))
                     (2 (:+ ann2-f)
                        (when-it (or @diff.l1 @diff.l3)
                          (format-ann-line out (:+ i) it)
                          (when @diff.l1 (:+ ann1-t))
                          (when @diff.l3 (:+ ann3-t))))
                     (3 (:+ ann1-f)
                        (:+ ann2-f)
                        (when-it @diff.l3
                          (format-ann-line out (:+ i) it)
                          (:+ ann3-t)))
                     (4 (:+ ann3-f)
                        (when-it (or @diff.l1 @diff.l2)
                          (format-ann-line out (:+ i) it)
                          (when @diff.l1 (:+ ann1-t))
                          (when @diff.l2 (:+ ann2-t))))
                     (5 (:+ ann1-f)
                        (:+ ann3-f)
                        (when-it @diff.l2
                          (format-ann-line out (:+ i) it)
                          (:+ ann2-t)))
                     (6 (:+ ann2-f)
                        (:+ ann3-f)
                        (when-it @diff.l1
                          (format-ann-line out (:+ i) it)
                          (:+ ann1-t)))
                     (7 (:+ ann1-f)
                        (:+ ann2-f)
                        (:+ ann3-f))
                     (otherwise
                      (ecase @diff.status
                        (:ok (:+ ann1-t)
                             (:+ ann2-t)
                             (:+ ann3-t)
                             (:+ ann1=ann2)
                             (:+ ann1=ann3)
                             (:+ ann2=ann3))
                        (:l1-l2 (:+ ann1-t)
                                (:+ ann2-t)
                                (:+ ann1=ann2))
                        (:l1-l3 (:+ ann1-t)
                                (:+ ann3-t)
                                (:+ ann1=ann3))
                        (:l2-l3 (:+ ann2-t)
                                (:+ ann2=ann3)
                                (:+ ann3-t))
                        (:l1 (:+ ann1-t))
                        (:l2 (:+ ann2-t))
                        (:l3 (:+ ann3-t)))
                      (format-ann-line out (:+ i)
                                       (or @diff.l1 @diff.l2 @diff.l3))))))
               (if ann3
                   (format stats
                           "~%~A: ~A/~A~%~A: ~A/~A~%~A: ~A/~A~%~
AGREEMENT ~A-~A: ~$~%AGREEMENT ~A-~A: ~$~%AGREEMENT ~A-~A: ~$~%"
                           ann1 ann1-t (+ ann1-t ann1-f) 
                           ann2 ann2-t (+ ann2-t ann2-f)
                           ann3 ann3-t (+ ann3-t ann3-f)
                           ann1 ann2
                           (/ ann1=ann2
                              (/ (+ ann1-t ann1-f ann2-t ann2-f)
                                 2))
                           ann1 ann3
                           (/ ann1=ann3
                              (/ (+ ann1-t ann1-f ann3-t ann3-f)
                                 2))
                           ann2 ann3
                           (/ ann2=ann3
                              (/ (+ ann2-t ann2-f ann3-t ann3-f)
                                 2)))
                   (format stats "~%~A: ~A/~A~%~A: ~A/~A~%AGREEMENT ~A-~A: ~$~%"
                           ann1 ann1-t (+ ann1-t ann1-f) 
                           ann2 ann2-t (+ ann2-t ann2-f)
                           ann1 ann2
                           (/ ann1=ann2
                              (/ (+ ann1-t ann1-f ann2-t ann2-f)
                                 2))))
               (with-open-file (log (local-file "data" "diff.log")
                                    :direction :output
                                    :if-does-not-exist :create :if-exists :append)
                 (write-string (get-output-stream-string stats) log))))
           "ok"))
          (htt:abort-request-handler
           (princ-to-string (:= (htt:return-code*) htt:+http-not-found+))))))


;;; auth (primitive)

(defvar *users* #h(equalp)
        "Stores user-pass pairs in plain text.")
(defvar *auth* #h()
        "Store auth cookies.")

(eval-always
 (defun load-auth (&optional (users-file
                              (merge-pathnames "users.txt"
                                               (asdf:component-pathname
                                                (asdf:find-system '#:ann)))))
   "Load auth data from USERS-FILE."
   (let ((users #h(equalp)))
     (if (probe-file users-file)
         (dolines (line users-file)
                  (with (((user pass) (split #\Space line :count 2)))
                        (:= (? users user) pass)))
         (format *debug-io* "No users file at: ~A – continuing without auth" users-file))
     users))

 (:= *users* (load-auth)))

(defun auth (user pass)
  "Given a USER and PASS return USER if it is authenticated against *users*.
   Note: Password is in plain text here."
  (when (and-it (? *users* user)
                (string= pass it))
    (let ((token (random 10000000)))
      (:= (? *auth* token) user)
      (htt:set-cookie "ann-auth" :value token))
    user))

(defun require-auth (file)
  "Ensures that before accessing the FILE authentication is performed and
   the username is either 'admin'
   or the same as FILE's first dir in the relative path."
  (or (zerop (ht-count *users*))
      (member (cond-it
                ((htt::header-in* "Authorization")
                 (apply 'auth
                        (split #\: (base64:base64-string-to-string
                                    (slice it (1+ (or (position #\Space it)
                                                      -1)))))))
                ((htt:cookie-in "ann-auth")
                 (? *auth* (parse-integer it)))
                (t (apply 'auth (htt:require-authorization))))
              (list "admin"
                    (slice file 0 (position #\/ file)))
              :test 'equal)
      (progn (:= (htt:return-code*) htt:+http-forbidden+)
             (htt:abort-request-handler))))
