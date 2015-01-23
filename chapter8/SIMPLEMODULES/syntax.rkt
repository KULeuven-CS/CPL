#lang eopl
(provide (all-defined-out))

;;; Syntax
(define-datatype program program?
   (a-program
    (m-defs (list-of module-definition?))
    (body expression?)))

(define-datatype module-definition module-definition?
  (a-module-definition
   (m-name symbol?)
   (expected-iface interface?)
   (m-body module-body?)))

(define-datatype interface interface?
  (simple-iface 
   (decls (list-of declaration?))))

(define-datatype declaration declaration?
  (val-decl 
   (var-name symbol?) 
   (ty type?)))

(define-datatype module-body module-body?
  (defns-module-body
    (defns (list-of definition?))))

(define-datatype definition definition?
  (val-defn (var-name symbol?) (exp expression?)))

(define-datatype expression expression?
  (qualified-var-exp
   (m-name symbol?)
   (var-name symbol?))
  (const-exp (num number?))
  (diff-exp (exp1 expression?) (exp2 expression?))
  (zero?-exp (exp1 expression?))
  (if-exp
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?))
  (var-exp (var symbol?))
  (let-exp
   (var symbol?)
   (exp1 expression?)
   (body expression?))
  (proc-exp
   (var symbol?)
   (ty type?)
   (body expression?))
  (call-exp
   (rator expression?)
   (rand expression?))
  (letrec-exp
   (result-ty type?)
   (p-name symbol?)
   (b-var symbol?)
   (b-var-ty type?)
   (p-body expression?)
   (letrec-body expression?))
  )

(define-datatype type type?
   ;(named-type (name symbol?))
   ;(qualified-type
   ; (m-name symbol?)
   ; (t-name symbol?))
   (int-type)
   (bool-type)
   (proc-type (arg-type type?) (result-type type?)))


(define (program->string pgm)
  (cases program pgm
    (a-program (defs exp1)
               (string-append
                (moduledefs->string defs)
                "\n"
                (exp->string exp1 )))))

(define (moduledefs->string defs)
  (apply string-append (map moduledef->string defs)))

(define (moduledef->string def)
  (cases module-definition def
    (a-module-definition (m-name expected-iface m-body)
                         (string-append "module "
                                        (symbol->string m-name)
                                        "\n interface\n"
                                        (interface->string expected-iface)
                                        "\n body\n"
                                        (mbody->string m-body)))))

(define (interface->string iface)
  (cases interface iface
    (simple-iface (decls)
                  (apply
                   string-append
                   (append '("[") (map decl->string decls) '("]"))))))

(define (decl->string decl)
  (cases declaration decl
    (val-decl (var-name ty)
              (string-append
               " "
               (symbol->string var-name)
               " : "
               (type->string ty)
               "\n"))))

(define (mbody->string m-body)
  (cases module-body m-body
    (defns-module-body (defns)
      (apply
       string-append
       (append '("[") (map defn->string defns) '("]"))))))

(define (defn->string defn)
  (cases definition defn
    (val-defn (var-name exp)
              (string-append
               " "
               (symbol->string var-name)
               " = "
               (exp->string exp)
               "\n"))))

(define (type->string ty)
  (cases type ty
    ;(named-type (name) (symbol->string name)) 
    ;(qualified-type (m-name t-name) (string-append
    ;                                 "from "
    ;                                 (symbol->string m-name)
    ;                                 " take "
    ;                                 (symbol->string t-name)))
    (int-type () "int")
    (bool-type () "bool")
    (proc-type (arg-type result-type ) (string-append "(" (type->string arg-type) " -> " (type->string result-type) ")"))
    ))

(define (exp->string exp)
  (cases expression exp
    (const-exp (num) (number->string num))
    (var-exp (var) (symbol->string var))
    (qualified-var-exp (m-name var-name) (string-append
                                     "(from "
                                     (symbol->string m-name)
                                     " take "
                                     (symbol->string var-name)
                                     ")"))
    (diff-exp (exp1 exp2)
              (let ((val1 (exp->string exp1))
                    (val2 (exp->string exp2)))
                (string-append "(" val1 " - " val2 ")")))
    (zero?-exp (exp1)
               (let ((val1 (exp->string exp1 )))
                 (string-append "zero? " val1)))
    (if-exp (exp1 exp2 exp3)
            (let ((val1 (exp->string exp1 ))
                  (val2 (exp->string exp2 ))
                  (val3 (exp->string exp3 )))
              (string-append "if " val1 " then " val2 " else " val3 )))
    
    (let-exp (var exp1 body)       
             (let ((val1 (exp->string exp1))
                   (val2 (exp->string body)))
               (string-append "let " (symbol->string var) " = " val1 " in " val2) ))
    (proc-exp (var ty body)
              (let ((val (exp->string body)))
                (string-append "proc(" (symbol->string var) ":" (type->string ty) ")" val)))
    (call-exp (rator rand)
              (let ((val1 (exp->string rator))
                    (val2 (exp->string rand)))
                (string-append "(" val1 " " val2 ")")))
    (letrec-exp (p-result-type p-name b-var b-var-type p-body letrec-body)
                (let ((ty1 (type->string p-result-type))
                      (val1 (exp->string p-body))
                      (ty2 (type->string b-var-type))
                      (val2 (exp->string letrec-body)))
                  (string-append "letrec " ty1 " " (symbol->string p-name) "(" (symbol->string b-var) ":" ty2 ") = " val1 " in " val2)))
    ))

  ;;;;;;;;;;;;;;;; syntactic tests and observers ;;;;;;;;;;;;;;;;

  ;;;; for types

  (define atomic-type?
    (lambda (ty)
      (cases type ty
        (proc-type (ty1 ty2) #f)
        (else #t))))

  (define proc-type?
    (lambda (ty)
      (cases type ty
        (proc-type (t1 t2) #t)
        (else #f))))

  (define proc-type->arg-type
    (lambda (ty)
      (cases type ty
        (proc-type (arg-type result-type) arg-type)
        (else (eopl:error 'proc-type->arg-type
                "Not a proc type: ~s" ty)))))

  (define proc-type->result-type
    (lambda (ty)
      (cases type ty
        (proc-type (arg-type result-type) result-type)
        (else (eopl:error 'proc-type->result-types
                "Not a proc type: ~s" ty)))))

  ;(define type-to-external-form
    (lambda (ty)
      (cases type ty
        (int-type () 'int)
        (bool-type () 'bool)
        (proc-type (arg-type result-type)
          (list
            (type-to-external-form arg-type)
            '->
            (type-to-external-form result-type)))
       (named-type (name) name)
        (qualified-type (modname varname)
          (list 'from modname 'take varname))
        )))
  

;...
  ;;;; for module definitions

  ;; maybe-lookup-module-in-list : Sym * Listof(Defn) -> Maybe(Defn)
  (define maybe-lookup-module-in-list
    (lambda (name module-defs)
      (if (null? module-defs)
        #f
        (let ((name1 (module-definition->name (car module-defs))))
          (if (eqv? name1 name)
            (car module-defs)
            (maybe-lookup-module-in-list name (cdr module-defs)))))))

  ;; maybe-lookup-module-in-list : Sym * Listof(Defn) -> Defn OR Error
  (define lookup-module-in-list
    (lambda (name module-defs)
      (cond
        ((maybe-lookup-module-in-list name module-defs)
         => (lambda (mdef) mdef))
        (else 
          (eopl:error 'lookup-module-in-list
            "unknown module ~s"
            name)))))

  (define module-definition->name
    (lambda (m-defn)
      (cases module-definition m-defn
        (a-module-definition (m-name m-type m-body)
          m-name))))

 (define module-definition->interface
    (lambda (m-defn)
      (cases module-definition m-defn
        (a-module-definition (m-name m-type m-body)
          m-type))))

  (define module-definition->body
    (lambda (m-defn)
      (cases module-definition m-defn
        (a-module-definition (m-name m-type m-body)
          m-body))))

 (define val-decl?
   (lambda (decl)
     (cases declaration decl
       (val-decl (name ty) #t))))

 (define decl->name
   (lambda (decl)
     (cases declaration decl
       (val-decl (name ty) name))))

 (define decl->type
   (lambda (decl)
     (cases declaration decl
       (val-decl (name ty) ty))))
