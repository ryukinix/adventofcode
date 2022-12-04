(require 'uiop)
(setf (fdefinition 'split) #'uiop:split-string) ;; create alias

(defun contains (r1 r2)
  (destructuring-bind ((min1 max1) (min2 max2)) (list r1 r2)
    (and (<= min1 min2) (>= max1 max2))))

(defun overlaps (r1 r2)
  (or (contains r1 r2)
      (contains r2 r1)))

(defun overlaps-partial (r1 r2)
  (destructuring-bind ((min1 max1) (min2 max2)) (list r1 r2)
    (and (<= min1 max2) (<= min2 max1))))

(defun parse-pair (pair)
  (let* ((ranges (split pair :separator ","))
         (range1 (split (nth 0 ranges) :separator "-"))
         (range2 (split (nth 1 ranges) :separator "-")))
    (list (mapcar #'parse-integer range1)
          (mapcar #'parse-integer range2))))

(defun resolve (pairs overlapfn)
  (loop with ranges = (mapcar #'parse-pair pairs)
        for range in ranges
        when (apply overlapfn range)
          counting 1))

(defun main ()
  (let ((pairs (uiop:read-file-lines "input.txt")))
    (print (resolve pairs #'overlaps))
    (print (resolve pairs #'overlaps-partial))))

(main)
