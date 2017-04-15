(in-package #:ann)
(named-readtables:in-readtable rutilsx-readtable)


(defmethod decode-anns ((format (eql :bsf)) str)
  (let ((i 0))
    (mapcar (lambda (line)
              (with (((_ tag beg end &rest txt)
                      (split-if ^(member % '(#\Space #\Tab))
                                line)))
                (make-ann
                 :id (:+ i) :txt (strjoin " " txt) :tag (mkeyw tag) :repr line
                 :beg (parse-integer beg) :end (parse-integer end))))
            (split #\Newline str :remove-empty-subseqs t))))

(defmethod encode-anns ((format (eql :bsf)) anns
                        &optional (stream *standard-output*))
  (doindex (i ann anns)
    (format stream "T~A	~A ~A ~A	~A~%"
            (1+ i) @ann.tag @ann.beg @ann.end
            (substitute #\Return #\Newline @ann.txt)))
  anns)
