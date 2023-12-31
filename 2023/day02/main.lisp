(eval-when (:load-toplevel :compile-toplevel :execute)
  (ql:quickload "str"))

(defun extract-game-id (game-token)
  ;; INPUT: "Game 1"
  (cadr (str:split " " game-token)))

(defun extract-games (games)
  ;; INPUT: "10 red, 7 green, 3 blue; 5 blue, 3 red, 10 green; 4 blue, 14 green, 7 red;"
  (str:split ";" games :omit-nulls t))

(defun convert-game-in-assoc (game)
  ;; INPUT: "10 red, 7 green, 3 blue"
  (loop for cubes in (str:split "," game)
        for (value color) = (str:split " " cubes :omit-nulls t)
        collect (cons (intern (str:upcase color) )
                      (parse-integer value))))

(defun get-values-of-color (color alist)
  (mapcar (lambda (x)
            (or (cdr (assoc color x))
                0))
          alist))

(defun reduce-max-values (parsed-game)
  ;; INPUT:
  ;; (((RED . 10) (GREEN . 7) (BLUE . 3)) ((BLUE . 5) (RED . 3) (GREEN . 10))
  ;;  ((BLUE . 4) (GREEN . 14) (RED . 7)) ((RED . 1) (GREEN . 11))
  ;;  ((BLUE . 6) (GREEN . 17) (RED . 15)) ((GREEN . 18) (RED . 7) (BLUE . 5)))
  ;; OUTPUT:
  ;; ((RED . 15) (BLUE . 6) (GREEN . 18))


  (list
   (cons 'RED (reduce #'max (get-values-of-color 'RED parsed-game)))
   (cons 'BLUE (reduce #'max (get-values-of-color 'BLUE parsed-game)))
   (cons 'GREEN (reduce #'max (get-values-of-color 'GREEN parsed-game)))))

(defun parse (line)
  ;; INPUT:
  ;; Game 1: 10 red, 7 green, 3 blue; 5 blue, 3 red, 10 green; 4 blue, 14 green, 7 red; 1 red, 11 green; 6 blue, 17 green, 15 red; 18 green, 7 red, 5 blue
  ;;
  ;; OUTPUT:
  ;; (((RED . 10) (GREEN . 7) (BLUE . 3)) ((BLUE . 5) (RED . 3) (GREEN . 10))
  ;;  ((BLUE . 4) (GREEN . 14) (RED . 7)) ((RED . 1) (GREEN . 11))
  ;;  ((BLUE . 6) (GREEN . 17) (RED . 15)) ((GREEN . 18) (RED . 7) (BLUE . 5)))


  (let* ((game-id-and-games (str:split ":" line))
         (game-id (extract-game-id (car game-id-and-games)))
         (games (extract-games (cadr game-id-and-games))))
    (loop for game in games
          collect (convert-game-in-assoc game))))


(defun check-restriction (max-game restriction)
  (let* ((colors '(RED GREEN BLUE)))
    (every (lambda (c)
             (>= (cdr (assoc c restriction))
                 (cdr (assoc c max-game))
                 ))
           colors)))

(defun resolve-part1 (max-games)
  (let ((restriction '((RED . 12) (GREEN . 13) (BLUE . 14))))
   (loop for x from 1 to (length max-games)
         for game in max-games
         if (check-restriction game restriction)
           sum x)))

(defun resolve-part2 (max-games)
  (loop for game in max-games
        for game-values = (mapcar #'(lambda (x) (cdr x)) game)
        for power-cube = (reduce #'* game-values)
        sum power-cube))

(defun main ()
  (let* ((lines (uiop:read-file-lines "input.txt"))
         (parsed-input (mapcar (lambda (line) (reduce-max-values (parse line)))
                               lines)))
    ;; NOTE: print the solution using lines of input.txt
    ;; (format t "Parsing: ")
    ;; (pprint (parse (car lines)))
    ;; (format t "~%~%Transformation max: ")
    ;; (pprint (reduce-max-values (parse (car lines))))
    ;; (format t "~%~%Part 1:  ")
    (print (resolve-part1 parsed-input))
    (print (resolve-part2 parsed-input))))

(eval-when (:execute)
  (main))
