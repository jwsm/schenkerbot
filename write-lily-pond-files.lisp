#!/usr/local/bin/sbcl --script
(write-line "Writing Out Text Files...")

(defun write-key-file ()
  (format t "Writing Key File")
  (with-open-file (outfile "/Users/jwsm/Desktop/WACM/project/pdf_output/tmp/key.txt"
                         :direction :output :if-exists :supersede)
    (let ((*standard-output* outfile))
    (format t "F"))))

(defun write-soprano-file ()
  (format t "Writing Soprano File")
  (with-open-file (outfile "/Users/jwsm/Desktop/WACM/project/pdf_output/tmp/soprano.txt"
                         :direction :output :if-exists :supersede)
    (let ((*standard-output* outfile))
    (format t "<a b c> s <a b c> s <a b c> s <a b c> s <a b c> s <a b c> s <a b c> <a b c>"))))

(defun write-bass-file ()
  (format t "Writing Bass File")
  (with-open-file (outfile "/Users/jwsm/Desktop/WACM/project/pdf_output/tmp/bass.txt"
                         :direction :output :if-exists :supersede)
    (let ((*standard-output* outfile))
    (format t "<d e f> s <d e f> s <d e f> s <d e f> s <d e f> s <d e f> s <d e f> <d e f>"))))

(defun write-chords-file ()
  (format t "Writing Bass File")
  (with-open-file (outfile "/Users/jwsm/Desktop/WACM/project/pdf_output/tmp/chords.txt"
                         :direction :output :if-exists :supersede)
    (let ((*standard-output* outfile))
    (format t "\\markup \\rN {V 6 4}  \\markup \\rN {vi 6 4}  \\markup \\rN {ii 6 5}"))))

(write-key-file)
(write-soprano-file)
(write-bass-file)
(write-chords-file)