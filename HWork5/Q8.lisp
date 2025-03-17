;;; Recursive Descent Parser with Error Handling

(defvar *tokens* '() "List of tokens to be parsed.")
(defvar *position* 0 "Tracks current position in input tokens.")

(defun match (expected)
  "Matches the expected token with the current token. Generates detailed error messages."
  (if (null *tokens*)
      (error "Unexpected end of input at position ~A. Expected ~A." *position* expected)
      (let ((token (car *tokens*)))
        (if (equal token expected)
            (progn
              (setf *tokens* (cdr *tokens*))
              (incf *position*))
            (error "Syntax error at position ~A: Expected ~A but found ~A." *position* expected token)))))

(defun parse-G ()
  "Parses the G production."
  (let ((token (car *tokens*)))
    (if (member token '(x y z w))
        (match token)
        (error "Syntax error at position ~A: Expected 'x', 'y', 'z', or 'w' but found ~A." *position* token))))

(defun parse-E-Prime ()
  "Handles E' (E-prime) to remove left recursion."
  (when (and (not (null *tokens*)) (equal (car *tokens*) 'o))
    (match 'o)
    (parse-G)
    (parse-E-Prime)))

(defun parse-E ()
  "Parses the E production."
  (parse-G)
  (parse-E-Prime))

(defun parse-L-Prime ()
  "Handles L' (L-prime) to remove left recursion."
  (when (and (not (null *tokens*)) (equal (car *tokens*) 's))
    (match 's)
    (parse-L-Prime)))

(defun parse-L ()
  "Parses the L production."
  (match 's)
  (parse-L-Prime))

(defun parse-S ()
  "Parses the S production."
  (let ((token (car *tokens*)))
    (cond
      ((equal token 's) (match 's))
      ((equal token 'd) (match 'd) (parse-L))
      (t (error "Syntax error at position ~A: Expected 's' or 'd' but found ~A." *position* token)))))

(defun parse-I ()
  "Parses the I production."
  (match 'i)
  (parse-E)
  (parse-S))

(defun parse (token-list)
  "Main parsing function. Initializes tokens and position."
  (setf *tokens* token-list)
  (setf *position* 0)
  (parse-I)
  (if (null *tokens*)
      (format t "Parsing successful!~%")
      (format t "Unexpected tokens remaining at position ~A: ~A~%" *position* *tokens*)))