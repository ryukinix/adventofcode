(ql:quickload "str")
(require 'uiop)

(defun substringp (sub s &key (test 'char=))
  (search (string sub)
          (string s)
          :test test))

(defun filter-digits (line)
  (remove-if-not #'digit-char-p line))

(defun lastcar (list)
  (car (last list)))

(defun get-calibration-value (line)
  (let ((digits (concatenate 'list (filter-digits line))))
    (parse-integer
     (concatenate 'string
                  (list
                   (first digits)
                   (lastcar digits))))))

(defun resolve (lines)
  (loop for line in lines
        sum (get-calibration-value line)))

(defun preprocess-spelled-digits (line)
  ;; not quiet right, the substitution should be made in order
  ;; eightwothree -> 8wo3, not eigh23
  (let ((spelled-digits '(("one" . "1e")
                          ("two" . "2o")
                          ("three" . "3e")
                          ("four" . "4r")
                          ("five" . "5e")
                          ("six" .  "6x")
                          ("seven" . "7n")
                          ("eight" . "8t")
                          ("nine" . "9e"))))

    (loop with line-changing = line
          with line-size = (length line)
          for (spelled . digit) in (sort (copy-seq spelled-digits)
                                         (lambda (a b)
                                           (< (or (substringp a line) line-size)
                                              (or (substringp b line) line-size)))
                                         :key #'car)
          do (setq line-changing
                   (str:replace-all spelled digit line-changing))
          finally (return line-changing))))

(defun preprocess-lines (lines)
  ;; not quiet right, the substitution should be made in order
  ;; eightwothree -> 8wo3, not eigh23

  (loop for line in lines
        collect (preprocess-spelled-digits line) ))

(defun main ()
  (let ((lines (uiop:read-file-lines "input.txt")))
    ;; part1
    (format t "~S~%" (resolve lines))
    ;; part2
    (format t "~S~%" (resolve (preprocess-lines lines)))))

(main)
