;;;
;;; ipmsg
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

(define-module ipmsg
  (use gauche.uvector)
  (use gauche.vport)
  (export
    <ipmsg-manager> make-ipmsg-uvector
    ))

(select-module ipmsg)

(define (get-mode cmd) (logand #x000000ff))
(define (get-opt cmd) (logand #xffffff00))

(define-constant ipmsg-version #x001)
(define-constant ipmsg-default-port #x0979)

(define-constant ipmsg-nooperation #x00000000)
(define-constant ipmsg-br-entry #x00000001)
(define-constant ipmsg-br-exit #x00000002)
(define-constant ipmsg-ansentry #x00000003)
(define-constant ipmsg-br-absence #x00000004)
(define-constant ipmsg-br-isgetlist #x00000010)
(define-constant ipmsg-okgetlist #x00000011)
(define-constant ipmsg-getlist #x00000012)
(define-constant ipmsg-anslist #x00000013)
(define-constant ipmsg-br-listgetlist2 #x00000018)

(define-constant ipmsg-sendmsg #x00000020)
(define-constant ipmsg-recvmsg #x00000021)
(define-constant ipmsg-readmsg #x00000030)
(define-constant ipmsg-delmsg #x00000031)
(define-constant ipmsg-ansreadmsg #x00000032)

(define-constant ipmsg-getinfo #x00000040)
(define-constant ipmsg-sendinfo #x00000041)

(define-constant ipmsg-getabssenceinfo #x00000050)
(define-constant ipmsg-sendabsenceinfo #x00000051)

(define-constant ipmsg-getfiledata #x00000060)
(define-constant ipmsg-releasefiles #x00000061)
(define-constant ipmsg-getdirfiles #x00000062)

(define-constant ipmsg-getpubkey #x00000072)
(define-constant ipmsg-anspubkey #x00000073)

;; option for all command
(define-constant ipmsg-opt-absence    #x00000100)
(define-constant ipmsg-opt-server     #x00000200)
(define-constant ipmsg-opt-dialup     #x00010000)
(define-constant ipmsg-opt-fileattach #x00200000)
(define-constant ipmsg-opt-encrypt    #x00400000)
(define-constant ipmsg-opt-utf8       #x00800000)
(define-constant ipmsg-opt-caputf8    #x01000000)
(define-constant ipmsg-opt-encextmsg  #x04000000)
(define-constant ipmsg-opt-clipboard  #x08000000)

;; option for send command
(define-constant ipmsg-opt-sendcheck #x00000100)
(define-constant ipmsg-opt-secret    #x00000200)
(define-constant ipmsg-opt-broadcast #x00000400)
(define-constant ipmsg-opt-multicast #x00000800)
(define-constant ipmsg-opt-autoret   #x00002000)
(define-constant ipmsg-opt-retry     #x00004000)
(define-constant ipmsg-opt-password  #x00008000)
(define-constant ipmsg-opt-nolog     #x00020000)
(define-constant ipmsg-opt-noaddlist #x00080000)
(define-constant ipmsg-opt-readcheck #x00100000)
(define-constant ipmsg-opt-secretex  (logior ipmsg-opt-readcheck ipmsg-opt-secret))

(define-constant max-udpbuf 32768)

(define-constant def-user-name "ipmsg-user")
(define-constant def-host-name "ipmsg-host")

(define (conv-newline-windows->unix msg) (regexp-replace-all #/\n\r/ msg "\n"))

(define (string->u8vector-bytes! target tstart string bytes-count)
  (let* ([str-buf (string->u8vector string)]
         [copy-bytes (min (u8vector-length str-buf) bytes-count)])
    (u8vector-copy! target tstart str-buf 0 copy-bytes)
    copy-bytes))

(define (make-msg user host packet-no cmd msg :optional (ex-msg ""))
  (let ([buf (make-u8vector max-udpbuf)]
        [max-len max-udpbuf]
        [broadcast-cmd? (let1 mode (get-mode cmd)
                   (or (eq? mode ipmsg-br-entry)
                     (eq? mode ipmsg-br-exit)
                     (eq? mode ipmsg-br-absence)
                     (eq? mode ipmsg-nooperation)))]) 
     ;; copy text until command number
    (let* ([pkt-len (string->u8vector-bytes! buf 0  
                                             (format #f "~D:~D:~A:~A:~D:" 
                                                     ipmsg-version packet-no 
                                                     (or user (sys-uid->user-name (sys-getuid)) def-user-name)
                                                     (or host (sys-gethostname) host-name)
                                                     (logior cmd ipmsg-opt-utf8)) ; force utf8
                                             max-len)]
           [ex-len (if (>= (+ pkt-len (string-size ex-msg)) 2) 0 (string-size ex-msg))])
      (set! max-len (- max-len ex-len))
      (set! pkt-len (+ pkt-len
                       ;;copy message
                       (string->u8vector-bytes! buf pkt-len (conv-newline-windows->unix msg) (- max-len pkt-len))
                       1))
      (unless (zero? ex-len)
        (set! pkt-len (+ pkt-len
                         ;;copy ex message
                         (string->u8vector-bytes! buf pkt-len ex-msg ex-len))))
      (when broadcast-cmd?
        (set! pkt-len (+ pkt-len 1
                         (string->u8vector-bytes! buf (+ pkt-len 1)
                                                  (format #f "\nUN:~A\nHN:~A\nNN:~A\nGN:~A"
                                                          user-name host-name msg ex-msg)
                                                  (- max-len pkt-len)))))
      (u8vector-copy buf 0 (min pkt-len (u8vector-length buf))))))

(define (u8vector->string-until buf start until-char-or-num)
  (let ([len (u8vector-length buf)]
        [until-num (if (char? until-char-or-num)
                     (char->integer until-char-or-num)
                     until-char-or-num)])
    (if (< start len)
      (let loop ([i start])
        (cond 
          [(>= i len) (values #f (u8vector->string buf start))]
          [(eq? (u8vector-ref buf i) until-num)
           (values i (u8vector->string buf start i))]
          [else (loop (+ i 1))]))
      (values #f ""))))

(define (make-u8vector-in-port buf)
  (let1 pos 0
    (make <virtual-input-port>
          :getb (lambda ()
                  (if (< pos (u8vector-length buf))
                    (begin0 (u8vector-ref buf pos)
                      (inc! pos))
                    (eof-object))))))

(define (port-filter fn reader)
  (let loop ([item (reader)]
             [r '()])
    (if (eof-object? item)
      (reverse! r)
      (let1 x (fn item)
        (loop (reader) (if x (cons x r) r))))))

(define (read-until until-char-or-num :optional (port (current-input-port)))
  (let ([until-num (if (char? until-char-or-num)
                     (char->integer until-char-or-num)
                     until-char-or-num)]
        [out (open-output-string)])
    (let loop ([b (read-byte port)])
      (cond
        [(eof-object? b) 
         (let1 ret (get-output-string out)
           (if (string-null? ret)
             (eof-object)
             ret))]
        [(eq? b until-num) (get-output-string out)]
        [else 
          (write-byte b out)
          (loop (read-byte port))]))))

(define (constract-ret-obj cmd ex1 ex2) 
  `((pkt-no . ,(string->number (car cmd)))
    (user . ,(cadr cmd))
    (host . ,(caddr cmd))
    (cmd . ,(cadddr cmd))
    (msg . ,(cadr (cddddr cmd)))
    (ex1 . ,ex1)
    (ex2 . ,ex2)))

(define (resolve-broadcast port)
  (if (let1 b (read-byte port) (or (eof-object? b) (not (eq? b #\cr))))
    #f
    (let1 l (port-filter
              (lambda (token)
                (let1 kind (substring token 0 3)
                  (cond
                    [(string=? kind "UN:") ('un . (substring kind 3 (string-length)))]
                    [(string=? kind "HN:") ('hn . (substring kind 3 (string-length)))]
                    [(string=? kind "NN:") ('nn . (substring kind 3 (string-length)))]
                    [(string=? kind "GN:") ('gn . (substring kind 3 (string-length)))]
                    [else #f])))
              (lambda () (read-until #\cr port)))
      (if (null? l) #f l))))

(define (resolve-msg buf)
  (let* ([port (make-u8vector-in-port buf)]
         [cmd (read-until 0 port)])
    (if (eof-object? cmd)
      #f
      (let1 cmd (string-split cmd #\:)
        (if (eq? (length cmd) 6)
          (let1 b (peek-byte port)
            (cond 
              [(eof-object? b) (constract-ret-obj cmd #f #f)]
              [(eq? b #\cr) (constract-ret-obj cmd #f (resolve-broadcast port))]
              [else (constract-ret cmd (read-until 0 port) (resolve-broadcast port))]))
          #f)))))

(define-class <ipmsg-manager> ()
  (
   (pkt-no :init-value 0)
   (user :init-keyword :user :init-value #f)
   (host :init-keyword :host :init-value #f)
   )
  )

(define (get-next-packet-no mgr)
  (let* ([now (sys-time)]
         [pkt-no (slot-ref mgr 'pkt-no)]
         [pkt-no (if (> now pkt-no) now pkt-no)])
    (slot-set! mgr 'pkt-no pkt-no)
    (+ pkt-no 1)))

(define-method make-ipmsg-uvector ((mgr <ipmsg-manager>) msg)
  (make-msg (slot-ref mgr 'user) (slot-ref mgr 'host)
            (get-next-packet-no mgr) ipmsg-sendmsg (x->string msg)))

