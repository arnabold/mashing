#lang racket

(define (serve port-no)
  (define main-cust (make-custodian)) ;; main-cust owns listener, main
  ;; server thread loop
  (parameterize ([current-custodian main-cust])
    (define listener (tcp-listen port-no 5 #t))
    (define (loop)
      (accept-and-handle listener)
      (loop))
    ;; Server thread
    (thread loop))
  (lambda ()
    (custodian-shutdown-all main-cust)))

(define (accept-and-handle listener)
  (define cust (make-custodian)) ;; creates a new custodians
  (parameterize ([current-custodian cust]) ;; A parameter that
    ;; determines a custodian that assumes responsibility for newly
    ;; created threads, file-stream ports, TCP ports, TCP listeners,
    ;; UDP sockets, and byte converters.
    (define-values (in out) (tcp-accept listener))  
    ;; Connection Thread
    (thread (lambda ()
              (handle in out)
              (close-input-port in)
              (close-output-port out))))
  ;; Watcher Thread
  (thread (lambda ()
            (sleep 10)
            (custodian-shutdown-all cust))) ;; With this
    ;; implementation, in, out, and the thread that calls handle all
    ;; belong to cust. In addition, if we later change handle so that
    ;; it, say, opens a file, then the file handles will also belong
    ;; to cust, so they will be reliably closed when cust is shut
    ;; down.
  )
  
(define (handle in out)
  ;; Discard the request header (up to blank line):
  (regexp-match #rx"(\r\n|^)\r\n" in)
  ;; Send reply:
  (display "HTTP/1.0 200 Okay\r\n" out)
  (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
  (display "<html><body>Hello, world!</body></html>" out))

;; A malicious client:
;; (enter! "serve.rkt")
;; (define stop (serve 8081))
;; (define-values (cin cout) (tcp-connect "localhost" 8081))
;; (read-line cin)
;; after 10 seconds ... #<eof>

;; Spikes
;; Run with $ ~/Applications/racket/bin/raco test serve.rkt
(module+ test
  (require rackunit)
  (check-equal? (+ 1 1) 2)
  ;; Dynamic Binding parameterize
  (define location (make-parameter "here")) ;; a parameter initialized
  ;; to value "here"
  (check-equal? (location) "here")
  (check-equal? (parameterize ([location "there"])
                  (location)) "there")
  (check-equal? (location) "here") ;; When control leaves the
  ;; parameterize form—either through a normal return, an exception, or
  ;; some other escape—the parameter reverts to its earlier value
  (check-equal? (parameterize ([location "in a house"])
                  (list (location)
                        (parameterize ([location "with a mouse"])
                          (location))
                        (location)))
                '("in a house" "with a mouse" "in a house"))
  (define (would-you-could-you?)
    (and (not (equal? (location) "here"))
         (not (equal? (location) "there"))))
  (check-false (would-you-could-you?))
  (check-true (parameterize ([location "on a bus"])
                (would-you-could-you?)))
  (check-equal?
   (let
       ([get (parameterize ([location "with a fox"])
               (lambda () (location)))])
     (get)) ;; localtion evaluated outside parameterize
   "here")
  (define (try-again! where)
    (location where))
  (check-equal? (location) "here")
  (check-equal?
   (parameterize ([location "on a train"])
     (list (location)
           (begin (try-again! "in a boat")
                  (location))))
   '("on a train" "in a boat")) 
  ) 



