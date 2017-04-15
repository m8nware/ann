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

(url "/" (dir)
  (unless (or (null dir)
              (ends-with "/" dir))
    (:= dir (strcat dir "/")))
  (std-page (:title "Ann welcomes you")
    (:div (:img :src "/static/ann.jpg" :style "float:left; margin-right:20px;")
          "Welcome to Ann, an annotation assistant."
          :br "Please, select file to annotate:")
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
                (:a :href (who:escape-string (strcat "/ann/" dir file))
                    (who:str file)
                    (:br))))))
           (who:htm (:blockquote
                     ":( Nothing. You need to add something to /data/ dir.")))))

(url "/ann/:file" (file)
  (require-auth file)
  (if-it (probe-file (local-file "data" file))
         (if (uiop:directory-pathname-p it)
             (handle-/ :dir file)
             (with (((raw meta anns) (read-file-with-anns file))
                    (all-files 
                     (sort (mapcar ^(fmt "~{~A/~}~A.~A"
                                         (subseq (pathname-directory %)
                                                 (1+ (position
                                                      "data"
                                                      (pathname-directory %)
                                                      :test 'string=)))
                                         (pathname-name %)
                                         (pathname-type %))
                                   (uiop:directory*
                                    (local-file "data"
                                                (fmt "~A/*.~A"
                                                     (slice file 0
                                                            (position #\/ file
                                                                      :from-end t))
                                                     @meta.ext))))
                           'string<))
                    (file-count (length all-files))
                    (file-pos (position file all-files :test 'string=)))
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
                                                (who:fmt
                                                 "~A (~A)" k
                                                 (slice (? v "desc") 0
                                                        (min 30
                                                             (length (? v "desc"))))))))
                                (:br)))
                             (void start))))
                        (:br)
                        (:input :type "button" :id "modal-del"
                                :value "Remove" :disabled "disabled"))))
                     (:div :class "file-nav"
                      (when (> file-count 1)
                        (who:htm
                         (:a :class "nav"
                             :href (fmt "/ann/~A"
                                        (who:escape-string
                                         (? all-files (1- (if (plusp file-pos)
                                                              file-pos
                                                              file-count)))))
                             "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;◄&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")))
                      (:nobr "     ")
                      (who:fmt "~A (~A/~A)" file (1+ file-pos) file-count)
                      (when (> file-count 1)
                        (who:htm
                         (:a :class "nav"
                             :href (fmt "/ann/~A"
                                        (who:escape-string
                                         (? all-files
                                            (if (= file-pos (1- file-count))
                                                0
                                                (1+ file-pos)))))
                             "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;►&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"))))
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
         (progn (:= (htt:return-code*) htt:+http-not-found+)
                (htt:abort-request-handler))))
             
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
         
(url "/static/:file" (file)
  (htt:handle-static-file (local-file "site" file)))


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
         (warn "No users file at: ~A" users-file))
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
