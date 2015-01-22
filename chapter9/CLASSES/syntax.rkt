#lang eopl
(require racket)
(provide (all-defined-out))

;;; Syntax
(define-datatype program program?
   (a-program
    (class-decls (list-of class-decl?))
    (body expression?)))

(define-datatype class-decl class-decl?
  (a-class-decl  
    (class-name symbol?)
    (super-name symbol?)
    (field-names (list-of symbol?))
    (method-decls (list-of method-decl?))))

(define-datatype method-decl method-decl?
   (a-method-decl
    (method-name symbol?)
    (vars (list-of symbol?))
    (body expression?)))

(define-datatype expression expression?
  (const-exp (num number?))
  (diff-exp (exp1 expression?) (exp2 expression?))
  (zero?-exp (exp1 expression?))
  (if-exp
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?))
  (var-exp (var symbol?))
  (let-exp
   (vars (list-of symbol?))
   (exps (list-of expression?))
   (body expression?))
  (proc-exp
   (vars (list-of symbol?))
   (body expression?))  
  (call-exp
   (rator expression?)
   (rand (list-of expression?)))
  (letrec-exp
   (p-name (list-of symbol?))
   (b-vars (list-of (list-of symbol?)))
   (p-body (list-of expression?))
   (letrec-body expression?))
  (begin-exp (exp1 expression?) (exps (list-of expression?)))
  (assign-exp
   (var symbol?)
   (exp expression?))  
  (sum-exp (exp1 expression?) (exp2 expression?))
  (list-exp (exp (list-of expression?)))
  (new-object-exp
   (class-name symbol?)
   (rands (list-of expression?)))
  (self-exp)
  (method-call-exp
   (obj-exp expression?)
   (method-name symbol?)
   (rands (list-of expression?)))
  (super-call-exp
   (method-name symbol?)
   (rands (list-of expression?))))

(define (program->string pgm)
  (cases program pgm
    (a-program (class-decls exp1)
               (string-append
                (class-decls->string class-decls) "\n"
               (exp->string exp1 )))))

(define (class-decls->string clss)
  (string-join (map class-decl->string clss) "\n"))

(define (class-decl->string cls)
  (cases class-decl cls
    (a-class-decl (class-name super-name field-names method-decls)
                  (string-append "class " (symbol->string class-name) " extends " (symbol->string super-name) 
                                 (string-join (map (lambda (f) (string-append "\nfield " (symbol->string f))) field-names))
                                 (string-join (map method-decl->string method-decls)))
                  )
    ))

(define (method-decl->string md)
  (cases method-decl md
    (a-method-decl (method-name vars body)
                   (string-append "\nmethod " (symbol->string method-name) 
                                  "(" (string-join (map symbol->string vars) ",") ")\n"
                                  (exp->string body)))))

(define (exp->string exp)
  (cases expression exp
    (const-exp (num) (number->string num))
    (var-exp (var) (symbol->string var))
    (diff-exp (exp1 exp2)
              (let ((val1 (exp->string exp1))
                    (val2 (exp->string exp2)))
                (string-append "(" val1 " - " val2 ")")))
    (sum-exp (exp1 exp2)
              (let ((val1 (exp->string exp1))
                    (val2 (exp->string exp2)))
                (string-append "(" val1 " + " val2 ")")))    
    (zero?-exp (exp1)
               (let ((val1 (exp->string exp1 )))
                 (string-append "zero? " val1)))
    (if-exp (exp1 exp2 exp3)
            (let ((val1 (exp->string exp1 ))
                  (val2 (exp->string exp2 ))
                  (val3 (exp->string exp3 )))
              (string-append "if " val1 " then " val2 " else " val3 )))
    (let-exp (vars exps body)       
             (let ((vals1 (map exp->string exps))
                   (val2 (exp->string body)))
               (string-append "let " 
                              (string-join
                               (map (lambda (v e) (string-append (symbol->string v) " = " e "\n")) vars vals1)) 
                              " in " val2) ))
    (proc-exp (vars body)
              (let ((val (exp->string body)))
                (string-append "proc(" (string-join (map symbol->string vars) ",") ")" val)))
    (call-exp (rator rands)
              (let ((val1 (exp->string rator))
                    (vals2 (map exp->string rands)))
                (string-append "(" val1 " " (string-join vals2) ")")))
    (letrec-exp (p-names b-varss p-bodys letrec-body)
                (string-append "letrec\n" 
                               (string-join (map stringify p-names b-varss p-bodys)) 
                               "in\n" 
                               (exp->string letrec-body)))
    (assign-exp (var exp)
                (let ((val1 (symbol->string var ))
                      (val2 (exp->string exp )))
                  (string-append val1 " := " val2  )))
    (begin-exp (exp exps)
               (string-append "begin " (exp->string exp) ";" (string-join  (map exp->string exps) ";") " end"))
    (list-exp (exps)
              (string-append "list(" (string-join (map exp->string exps) ",") ")"))
    (new-object-exp (class-name rands)
                    (string-append "new " (symbol->string class-name) "("
                                   (string-join (map exp->string rands) ",") ")"))
    (self-exp () "self")
    (method-call-exp (obj-exp method-name rands)
                     (string-append "send " (exp->string obj-exp) " " (symbol->string method-name) "(" (string-join (map exp->string rands) ",") ")"))
    (super-call-exp (method-name rands)
                    (string-append "super " (symbol->string method-name) (string-join (map exp->string rands) ",")))
    ))

(define (stringify p-name b-vars p-body)
  (string-append (symbol->string p-name) "(" (string-join (map symbol->string b-vars) ",") ") = " (exp->string p-body) "\n"))
