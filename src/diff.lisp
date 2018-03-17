(in-package #:ann)
(named-readtables:in-readtable rutilsx-readtable)


(defstruct span
  cat beg end text)

(defun parse-ann-line (line)
  (with (((_ span text) (split #\Tab line))
         ((cat beg _) (split #\Space span))
         (beg (parse-integer beg))
         (text (string-trim (list #\. #\Space #\Tab #\Newline) text)))
    (make-span :cat (mkeyw cat)
               :beg beg
               :end (+ beg (length text))
               :text text)))

(defun format-ann-line (out id span)
  (format out "~@[T~A	~]~A ~A ~A	~A~%"
          id @span.cat @span.beg @span.end @span.text))


;;; diff

(defstruct diff
  id status l1 l2 l3)

(defun diff-dirs (dir1 dir2)
  (dolist (file1 (directory (strcat dir1 "*.ann")))
    (format t "~A: " (pathname-name file1))
    (if-it (probe-file (strcat dir2 (pathname-name file1) ".ann"))
           (if-it (diff-files file1 it)
                  (progn
                    (format t "~A diffs~%" (length it))
                    (with-out-file (out (strcat (pathname-name file1) ".diff"))
                      (dolist (d it)
                        (format out "~A~%" d))))
                  (format t "ok~%"))
           (format t "Not Found!%"))))

(defun diff-files (f1 f2 &optional f3)
  (with (((ls1 ls2 ls3) (mapcar ^(when %
                                   (-> (read-file %)
                                       (split #\Newline %
                                              :remove-empty-subseqs t)
                                       (mapcar 'parse-ann-line %)
                                       (sort % '< :key 'span-beg)))
                                (list f1 f2 f3)))
         (diffs nil)
         (id 0))
    (loop :while (and ls1 ls2 (or (not f3) ls3)) :do
      (with (((l1 l2 l3) (mapcar 'first
                                 (list ls1 ls2 ls3))))
        (cond ((< @l1.beg (min @l2.beg (if l3 @l3.beg @l2.beg)))
               (push (make-diff :id (:+ id) :status :l1 :l1 l1) diffs)
               (:= ls1 (rest ls1)))
              ((< @l2.beg (min @l1.beg (if l3 @l3.beg @l1.beg)))
               (push (make-diff :id (:+ id) :status :l2 :l2 l2) diffs)
               (:= ls2 (rest ls2)))
              (l3
               (cond ((< @l3.beg (min @l1.beg @l2.beg))
                      (push (make-diff :id (:+ id) :status :l3 :l3 l3) diffs)
                      (:= ls3 (rest ls3)))
                     ((> (max @l1.beg @l2.beg @l3.beg)
                         (min @l1.beg @l2.beg @l3.beg))
                      (let ((mismatch (cond ((= @l1.beg @l2.beg)
                                             (if (> @l3.beg @l1.beg) 3 :l3))
                                            ((= @l1.beg @l3.beg)
                                             (if (> @l2.beg @l1.beg) 2 :l2))
                                            (t
                                             (if (> @l1.beg @l2.beg) 1 :l1)))))
                        (push (if (numberp mismatch)
                                  (apply 'make-diff
                                         :id (:+ id)
                                         :status (ecase mismatch
                                                   (1 (if (eql @l2.cat @l3.cat)
                                                          :l2-l3
                                                          (list @l2.cat @l3.cat)))
                                                   (2 (if (eql @l1.cat @l3.cat)
                                                          :l1-l3
                                                          (list @l1.cat @l3.cat)))
                                                   (3 (if (eql @l1.cat @l2.cat)
                                                          :l1-l2
                                                          (list @l2.cat @l2.cat))))
                                         (let ((ls (list (pair :l1 l1)
                                                         (pair :l2 l2)
                                                         (pair :l3 l3))))
                                           (flatten
                                            (if (numberp mismatch)
                                                (remove (lt (? ls (1- mismatch)))
                                                        ls :key 'lt)
                                                (keep mismatch ls :key 'lt)))))
                                  (make-diff :id (:+ id) :status mismatch
                                             mismatch (ecase mismatch
                                                        (:l1 l1)
                                                        (:l2 l2)
                                                        (:l3 l3))))
                              diffs)
                        (ecase mismatch
                          (:l1 (:= ls1 (rest ls1)))
                          (:l2 (:= ls2 (rest ls2)))
                          (:l3 (:= ls3 (rest ls3)))
                          (1 (:= ls2 (rest ls2)
                                 ls3 (rest ls3)))
                          (2 (:= ls1 (rest ls1)
                                 ls3 (rest ls3)))
                          (3 (:= ls1 (rest ls1)
                                 ls2 (rest ls2))))))
                     (t
                      (push (cond ((/= @l1.end @l2.end @l3.end)
                                   (make-diff :id (:+ id) :status :ends
                                              :l1 l1 :l2 l2 :l3 l3))
                                  ((reduce 'eql (list @l1.cat @l2.cat @l3.cat))
                                   (make-diff :id (:+ id) :status :ok :l1 l1))
                                  ((eql @l1.cat @l2.cat)
                                   (make-diff :id (:+ id) :status :l1-l2 :l1 l1))
                                  ((eql @l1.cat @l3.cat)
                                   (make-diff :id (:+ id) :status :l1-l3 :l1 l1))
                                  ((eql @l2.cat @l3.cat)
                                   (make-diff :id (:+ id) :status :l2-l3 :l1 l2))
                                  (t
                                   (make-diff :id (:+ id) :status (list @l1.cat
                                                                        @l2.cat
                                                                        @l3.cat)
                                              :l1 l1 :l2 l2 :l3 l3)))
                            diffs)
                      (:= ls1 (rest ls1)
                          ls2 (rest ls2)
                          ls3 (rest ls3)))))
              (t
               (push (cond ((/= @l1.end @l2.end)
                            (make-diff :id (:+ id) :status :ends
                                       :l1 l1 :l2 l2))
                           ((eql @l1.cat @l2.cat)
                            (make-diff :id (:+ id) :status :ok :l1 l1))
                           (t
                            (make-diff :id (:+ id) :status (list @l1.cat
                                                                 @l2.cat)
                                       :l1 l1 :l2 l2)))
                     diffs)
               (:= ls1 (rest ls1)
                   ls2 (rest ls2))))))
    (dolist (l1 ls1)
      (push (make-diff :id (:+ id) :status :l1 :l1 l1) diffs))
    (dolist (l2 ls2)
      (push (make-diff :id (:+ id) :status :l2 :l2 l2) diffs))
    (dolist (l3 ls3)
      (push (make-diff :id (:+ id) :status :l3 :l3 l3) diffs))
    (reverse diffs)))

(defun ann-line-htm (id span)
  (fmt "<input id=\"cat-~A\" size=\"5\" onclick=\"return false;\"~
               onblur=\"edit_cat(~A,'~A')\" value=\"~A\" /> ~A ~A	~A~%"
       id id @span.cat @span.cat @span.beg @span.end @span.text))


(defun diff-htm (meta ann-file1 ann-file2 &optional ann-file3)
  ""
  (who:with-html-output-to-string (out)
    (:ul :style "max-width: 500px; max-height: 600px; overflow-y: scroll;"
         (loop :for diff :in (diff-files (strcat ann-file1 ".ann")
                                          (strcat ann-file2 ".ann")
                                          (when ann-file3
                                            (strcat ann-file3 ".ann")))
               :for i :from 1 :do
           (if (eql :ok @diff.status)
               (who:htm
                (:li :class "ann-li green"
                     (:a :href "#" :onclick (fmt "return rem_diff(~A, 0)"
                                                 @diff.id)
                         "✕")
                     " "
                     (:a :href "#" :onclick (fmt "return highlight(~A, ~A)"
                                                 @diff.l1.beg @diff.l1.end)
                         (who:str (ann-line-htm @diff.id @diff.l1)))))
               (progn
                 (when @diff.l1
                   (who:htm
                    (:li :class "ann-li yellow"
                         (:a :href "#" :onclick (fmt "return rem_diff(~A, 1)"
                                                     @diff.id)
                             "✕")
                         " "
                         (:a :href "#" :onclick (fmt "return highlight(~A, ~A)"
                                                     @diff.l1.beg @diff.l1.end)
                             (who:str (ann-line-htm @diff.id @diff.l1))))))
                 (when @diff.l2
                   (who:htm
                    (:li :class "ann-li blue"
                         (:a :href "#" :onclick (fmt "return rem_diff(~A, 2)"
                                                     @diff.id)
                             "✕")
                         " "
                         (:a :href "#" :onclick (fmt "return highlight(~A, ~A)"
                                                     @diff.l2.beg @diff.l2.end)
                             (who:str (ann-line-htm @diff.id @diff.l2))))))
                 (when @diff.l3
                   (who:htm
                    (:li :class "ann-li pink"
                         (:a :href "#" :onclick (fmt "return rem_diff(~A, 4)"
                                                     @diff.id)
                             "✕")
                         " "
                         (:a :href "#" :onclick (fmt "return highlight(~A, ~A)"
                                                     @diff.l3.beg @diff.l3.end)
                             (who:str (ann-line-htm @diff.id @diff.l3))))))))
           (who:htm (:br))))))
