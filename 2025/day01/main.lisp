(defparameter *pointer* 50)
(defparameter *test-input*
  '(("L68" . 82)
    ("L30" . 52)
    ("R48" . 0)
    ("L5"  . 95)
    ("R60" . 55)
    ("L55" . 0)
    ("L1"  . 99)
    ("L99" . 0)
    ("R14" . 14)
    ("L82" . 32)))

(defparameter *test-input-extra*
  '("L150"
   "L50"
   "L150"
   "R50"))

(defun parse-rotation (rot-string)
  (let* ((head (char rot-string 0))
         (tail (subseq rot-string 1))
         (number (parse-integer tail)))
    (case head
     (#\L (- number))
     (#\R number))))


(defun rotate (rotation pointer)
  (let ((r (+ rotation pointer)))
    (values (mod r 100)
            (if (>= rotation 0)
                (- (floor r 100) (floor pointer 100))
                (- (floor (1- pointer) 100) (floor (1- r) 100))))))

(defun apply-rotation (rot-strings initial-position)
  (loop :with p = initial-position
        :for rot-string in rot-strings
        :do (setq p (rotate (parse-rotation rot-string) p))
        :collect p))

(defun apply-rotation-with-interlap (rot-strings initial-position)
  (loop :with p = initial-position
        :for rot-string in rot-strings
        :for (new-p rotations) = (multiple-value-list (rotate (parse-rotation rot-string) p))
        :do (setq p new-p)
        :collect rotations))

(defun sum (l)
  (reduce #'+ l :initial-value 0))

(defun count-rotation-with-interlap (rot-strings initial-position)
  (sum (apply-rotation-with-interlap rot-strings initial-position)))


(defun run-tests ()
  (let* ((rot-strings (mapcar 'car *test-input*))
         (expected (mapcar 'cdr *test-input*))
         (rotations (apply-rotation rot-strings *pointer*))
         (count-zeros (count 0 rotations))
         (interlap-zeros (count-rotation-with-interlap rot-strings *pointer*))
         (extra-test (count-rotation-with-interlap *test-input-extra* *pointer*))
         (check-rotations (loop :for rotation in rotations
                                :for rotation-expected in expected
                                :always (= rotation rotation-expected))))
    (assert check-rotations (rotations) "Rotations: ~a" rotations)
    (assert (eq count-zeros 3) (count-zeros) "Expected 3 zeros, but found ~a" count-zeros)
    (assert (eq interlap-zeros 6) (interlap-zeros) "Expected 6 interlap-zeros, but found ~a" interlap-zeros)
    (assert (eq extra-test 4) (extra-test) "Expected 4 in extra-test, but found ~a" extra-test)))

(defun solve-part-a (lines)
  (let ((zeros (count 0 (apply-rotation lines *pointer*))))
    (format t "Part A: ~a~%" zeros)))

(defun solve-part-b (lines)
  (let ((zeros (count-rotation-with-interlap lines *pointer*)))
    (format t "Part B: ~a~%" zeros)))

(defun main ()
  (run-tests)
  (let* ((lines (uiop:read-file-lines "input.txt")))
    (solve-part-a lines)
    (solve-part-b lines)))

(eval-when (:execute)
  (main))
