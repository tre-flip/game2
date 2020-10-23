;;;; input.lisp

(in-package :game2)

(defun bind-up (key thunk)
  (bind-button :w key thunk)
  (bind-button :up key thunk))

(defun bind-down (key thunk)
  (bind-button :s key thunk)
  (bind-button :down key thunk))

(defun bind-left (key thunk)
  (bind-button :a key thunk)
  (bind-button :left key thunk))

(defun bind-right (key thunk)
  (bind-button :d key thunk)
  (bind-button :right key thunk))
