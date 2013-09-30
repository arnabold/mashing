#lang racket

;; just to check if (enter! "serve.rkt") works.
(define (go)
  'yep-it-works)

;; port-no: ip port number for client connections
(define (serve port-no)
  ;; define a listener to accept TCP connections. tcp-listen creates a
  ;; listening server on the local machine at the port number
  ;; specified by port-no. The max-allow-wait argument determines the
  ;; maximum number of client connections that can be waiting for
  ;; acceptance. Here is set to 5. The reuse? argument is true: it
  ;; will create a listener even if the port is involved in a
  ;; TIME-WAIT state.
  (define listener (tcp-listen port-no 5 #t))
  ;; The server must loop to accept connections fro the listener
  (define (loop)
    (accept-and-handle listener)
    (loop))
  ;; put the listener loop in its own thread and have serve to run
  ;; immediately.
  (define t (thread loop))
  ;; Have serve return an anonymous function that can be used to
  ;; shut down the server thread and tcp listener
  (lambda ()
    (kill-thread t)
    (tcp-close listener)))

;; accept-and-handle
;; accepts a client connection using tcp-accept, which returns two
;; values: a stream for for input from the client and a stream of
;; output to the client.
(define (accept-and-handle listener)
  (define-values (in out) (tcp-accept listener))
  ;; we can put each individual connection in its own thread
  (thread
   (lambda ()
     ;; just to show the effect of different threads
     (sleep (random 10))
     (handle in out)
     (close-input-port in)
     (close-output-port out))))
  
;; handle
;; for now reads and discards the request header and the write "Hello,
;; world!" web page as the result
(define (handle in out)
  ;; Discard the request header (up to blank line):
  (regexp-match #rx"(\r\n|^)\r\n" in)
  ;; Send reply:
  (display "HTTP/1.0 200 Okay\r\n" out)
  (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
  (display "<html><body>Hello, world!</body></html>" out))

;; Note: inside geiser use C-c C-c to stop the evaluation of (serve 8080)
