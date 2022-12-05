(require 'uiop)
(setf (fdefinition 'split) #'uiop:split-string) ;; create alias

;; Stacks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; [T]     [D]         [L]             ;;
;; [R]     [S] [G]     [P]         [H] ;;
;; [G]     [H] [W]     [R] [L]     [P] ;;
;; [W]     [G] [F] [H] [S] [M]     [L] ;;
;; [Q]     [V] [B] [J] [H] [N] [R] [N] ;;
;; [M] [R] [R] [P] [M] [T] [H] [Q] [C] ;;
;; [F] [F] [Z] [H] [S] [Z] [T] [D] [S] ;;
;; [P] [H] [P] [Q] [P] [M] [P] [F] [D] ;;
;; 1   2   3   4   5   6   7   8   9   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *stacks*
  '((T R G W Q M F P)
    (R F H)
    (D S H G V R Z P)
    (G W F B P H Q)
    (H J M S P)
    (L R R S H T Z M)
    (L M N H T P)
    (R Q D F)
    (H P L N C S D)))

(defun parse-line (line)
  (let* ((tokens (split line :separator " "))
         (move (parse-integer (nth 1 tokens)))
         (from (parse-integer (nth 3 tokens)))
         (to (parse-integer (nth 5 tokens))))
    (list move (1- from) (1- to))))

(defun update-part1 (stacks move from to)
  (loop repeat move
        do (push (pop (nth from stacks))
                 (nth to stacks))))

(defun update-part2 (stacks move from to)
  (let ((crates (subseq (nth from stacks) 0 move)))
    (setf (nth to stacks)
          (append crates (nth to stacks)))
    (setf (nth from stacks)
          (subseq (nth from stacks) move))))


(defun resolve (lines updatefn)
  (let ((stacks (copy-list *stacks*)))
    (loop for statement in (mapcar #'parse-line lines)
          do (apply updatefn (cons stacks statement))
          finally (return (mapcar #'car stacks)))))

(defun main ()
  (let ((lines (uiop:read-file-lines "input.txt")))
    ;; part1
    (format t "~{~S~}~%" (resolve lines #'update-part1))
    ;; part2:
    (format t "~{~S~}~%" (resolve lines #'update-part2))))

(main)
