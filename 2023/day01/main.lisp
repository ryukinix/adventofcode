(ql:quickload "str")
(require 'uiop)

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

(defun preprocess-spelled-digits (lines)
  ;; not quiet right, the substitution should be made in order
  ;; eightwothree -> 8wo3, not eigh23
  (let ((spelled-digits '(("one" . "1")
                          ("two" . "2")
                          ("three" . "3")
                          ("four" . "4")
                          ("five" . "5")
                          ("six" .  "6")
                          ("seven" . "7")
                          ("eight" . "8")
                          ("nine" . "9"))))

    (loop for line in lines
          collect (loop with line-changing = line
                        for (spelled . digit) in (reverse spelled-digits)
                        do (setq line-changing
                                 (str:replace-all spelled digit line-changing))
                        finally (return line-changing)
                    ))))

(defun main ()
  (let ((lines (uiop:read-file-lines "input.txt")))
    ;; part1
    (format t "~S~%" (resolve lines))
    ;; part2
    (format t "~S~%" (resolve (preprocess-spelled-digits lines)))
    ))

(main)
