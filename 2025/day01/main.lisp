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

(defun parse-rotation (rot-string)
  (let* ((head (char rot-string 0))
         (tail (subseq rot-string 1))
         (number (parse-integer tail)))
    (case head
     (#\L (- number))
     (#\R number))))


(defun rotate (rotation pointer)
  (let ((r (+ rotation pointer)))
    (cond
      ((< r 0) (mod (+ r 100) 100))
      ((>= r 100) (mod r 100))
      (t r))))

(defun apply-rotation (rot-strings initial-position)
  (loop :with p = initial-position
        :for rot-string in rot-strings
        :do (setq p (rotate (parse-rotation rot-string) p))
        :collect p))

(defun run-tests ()
  (let* ((rot-strings (mapcar 'car *test-input*))
         (expected (mapcar 'cdr *test-input*))
         (rotations (apply-rotation rot-strings *pointer*))
         (check-rotations (loop :for rotation in rotations
                                :for rotation-expected in expected
                                :always (= rotation rotation-expected)))
         (check-count (eq (count 0 rotations) 3)))
    (assert check-rotations)
    (assert check-count)))

(defun solve-part-a (lines)
  (let ((zeros (count 0 (apply-rotation lines *pointer*))))
    (format t "Part A: ~a" zeros)))

(defun main ()
  (run-tests)
  (let* ((lines (uiop:read-file-lines "input.txt")))
    (solve-part-a lines)))

(eval-when (:execute)
  (main))
