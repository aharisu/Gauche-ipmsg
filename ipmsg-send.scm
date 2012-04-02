;;;
;;; ipmsg-send
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

(use srfi-13)
(use gauche.parseopt)
(use gauche.net)

(add-load-path ".")
(use ipmsg)

(define (usage)
  (print "Usage: gosh ipmsg-send.scm [-t|--target=<target>] [-m|--message=<msg>] [-u|--user=<sender-user>] [-h|--host=<sender-host>]")
  (print "Example: gosh ipmsg-send.scm -t=192.168.1.2 -m=\"hoge hoge\"")
  (print "         gosh ipmsg-send.scm -t=192.168.1.2 -t=192.168.1.3 -m=\"message multi cast\"")
  (print "         gosh ipmsg-send.scm -u=sender-user -h=sender-host -t=192.168.1.2 -m=\"specify sender user and host\"")
  )

(define (send-message user host msg target-list)
  (let* ([mgr (make <ipmsg-manager>
                    :user user
                    :host host)]
         [buf (make-ipmsg-uvector mgr msg)]
         [sock (make-socket AF_INET SOCK_DGRAM)])
    (unwind-protect
      (for-each
        (lambda (target) 
        (socket-sendto sock buf
                       (make <sockaddr-in> :host target :port 2425)))
        target-list)
      (socket-close sock))))

(define (main args)
  (let ([send-target '()])
    (let-args (cdr args)
      ([#f "t|target=s"
        => (lambda (opt)
             (let1 opt (string-trim-both opt)
               (unless (string-null? opt)
                 (set! send-target (cons opt send-target)))))]
       [message "m|message=s"]
       [user "u|user=s"]
       [host "h|host=s"]
       [#f "help" => usage]
       [else (opt . _) (print "Unknown option : " opt) (usage)]
       . args)
      (if (and message (not (null? send-target)))
        (send-message user host message send-target)
        (usage))))
  0)


