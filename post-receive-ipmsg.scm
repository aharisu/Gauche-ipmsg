;;;
;;; post-receive-ipmsg
;;;
;;; MIT License
;;; Copyright 2012 aharisu
;;; All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;;
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.
;;;
;;;
;;; aharisu
;;; foo.yobina@gmail.com
;;;

(use srfi-1)
(use srfi-13)
(use file.util)
(use gauche.process)

(define-constant config-dir "~/.devst")
(define-constant ipmsg-send-dir "/ram/gprog/ipmsg/")

(define-constant user "push-broadcast")
(define-constant host "git-server")


(define (read-target-notify path)
  (with-input-from-file
    path
    (pa$ port-map
         string-trim-both
         read-line)))

(define (setup-target-notify :optional (file "target-notify"))
  (read-target-notify (expand-path (build-path config-dir file))))


(define (get-description description)
  (if (rxmatch #/Unnamed repository.*$/ description)
    "UNNAMED PROJECT"
    description))

(define (get-tag show-list rx)
  (string-join
    (filter-map
      (lambda (line)
        (let1 m (rxmatch rx line)
          (if m
            (m 1)
            #f)))
      show-list)
    "\n"))

(define (get-date show-msg-list) (get-tag show-msg-list #/Author:\s+(\S+)\s+.*$/))
(define (get-author show-msg-list) (get-tag show-msg-list #/Date:\s+(.*)$/))
(define (get-message show-msg-list) (get-tag show-msg-list #/    (.*)$/))

(define (get-commit-message)
  (let* ([git-dir (process-output->string "git rev-parse --git-dir")]
         [description (get-description (call-with-input-file 
                                         (string-append git-dir "/description")
                                         port->string))]
         [args (string-split (read-line (current-input-port)) #[\s])]
         [show-msg-list (process-output->string-list 
                          (string-append "git show --no-color " (cadr args)))])
#`"Author: ,(get-date show-msg-list)
Date: ,(get-author show-msg-list)
Description: ,description
,(get-message show-msg-list)"))


(define (main args) 
  (let ([targets (setup-target-notify)]
        [message (get-commit-message)])
    (process-output->string 
      #`"gosh -I/,ipmsg-send-dir ,|ipmsg-send-dir|ipmsg-send.scm -m=\",message\" -u=,user -h=,host ,(string-join targets \" -t=\" 'prefix)")
    0))

