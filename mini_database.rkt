#lang racket
(define NULL 'null)

;====================================
;=            Cerința 1             =
;= Definirea elementelor de control =
;=          20 de puncte            =
;====================================

;= Funcțiile de acces
(define init-database
  (λ ()
    '()))

(define create-table
  (λ (table columns-name)
    (list table (map (λ (column) (list column)) columns-name))))

(define get-name
  (λ (table)
    (car table)))

(define get-columns
  (λ (table)
    (map (λ (column) (car column)) (car (cdr table)))))

(define get-tables
  (λ (db)
    db))

(define get-table
  (λ (db table-name)
    (car (filter (λ (table) (equal? (car table) table-name)) db))))

(define add-table
  (λ (db table)
    (append db (list table))))

(define remove-table
  (λ (db table-name)
    (filter (λ (table) (not (equal? (car table) table-name))) db)))

;= Pentru testare, va trebui să definiți o bază de date (având identificatorul db) cu următoarele tabele

;============================================================================================
;=                         Tabela Studenți                                                   =
;= +----------------+-----------+---------+-------+-------+                                  =
;= | Număr matricol |   Nume    | Prenume | Grupă | Medie |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;= |            123 | Ionescu   | Gigel   | 321CA |  9.82 |                                  =
;= |            124 | Popescu   | Maria   | 321CB |  9.91 |                                  =
;= |            125 | Popa      | Ionel   | 321CC |  9.99 |                                  =
;= |            126 | Georgescu | Ioana   | 321CD |  9.87 |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;=                                                                                           =
;=                                         Tabela Cursuri                                    =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | Anul | Semestru |            Disciplină             | Număr credite | Număr teme |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | I    | I        | Programarea calculatoarelor       |             5 |          2 |      =
;= | II   | II       | Paradigme de programare           |             6 |          3 |      =
;= | III  | I        | Algoritmi paraleli și distribuiți |             5 |          3 |      =
;= | IV   | I        | Inteligență artificială           |             6 |          3 |      =
;= | I    | II       | Structuri de date                 |             5 |          3 |      =
;= | III  | II       | Baze de date                      |             5 |          0 |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;============================================================================================
(define db
  (add-table (add-table (init-database) (create-table "Studenți" '("Număr matricol" "Nume" "Prenume" "Grupă" "Medie")))
             (create-table "Cursuri" '("Anul" "Semestru" "Disciplină" "Număr credite" "Număr teme"))))
            

;====================================
;=            Cerința 2             =
;=         Operația insert          =
;=            10 puncte             =
;====================================
(define insert
  (λ (db table-name record)
    (map (λ (table)
           (if (equal? (car table) table-name)
               (cons (car table) (list (map (λ (column)
                      (append column (list (let ([found (filter (λ (record-pair)
                                                                  (equal? (car record-pair) (car column))) record)])
                                             (if (> (length found) 0)
                                                 (cdr (car found))
                                                 NULL))))) (car (cdr table)))))
               table)) db)))


;====================================
;=            Cerința 3 a)          =
;=     Operația simple-select       =
;=             10 puncte            =
;====================================
(define simple-select
  (λ (db table-name columns)
    (map (λ (column)
           (cdr (car (filter (λ (table-column) (equal? column (car table-column))) (car (cdr (get-table db table-name))))))) columns)))
                       
           

;====================================
;=            Cerința 3 b)          =
;=           Operația select        =
;=            30 de puncte          =
;====================================
(define average
  (λ (nums)
    (/ (apply + nums) (length nums))))
(define column-operations (list
                           (cons 'min (λ (arr) (apply min arr)))
                           (cons 'max (λ (arr) (apply max arr)))
                           (cons 'count (λ (arr) (length (remove-duplicates arr))))
                           (cons 'sum (λ (arr) (apply + arr)))
                           (cons 'avg (λ (arr) (apply average (list arr))))
                           (cons 'sort-asc (λ (arr) (apply sort (list arr <))))
                           (cons 'sort-desc (λ (arr) (apply sort (list arr >))))))
(define column-operation
  (λ (func params)
    (apply (cdr (car (filter (λ (name-func)
                          (eq? (car name-func) func)) column-operations))) (list params))))

(define make-column-value-pairs
  (λ (table)
    (cdr (apply map (append (list (λ args
                                    (map (λ (column value) (cons column value)) (get-columns table) args))) (cadr table))))))
(define filter-column-value-pairs
  (λ (table conditions)
    (filter (λ (row)
              (andmap (λ (condition)
                     (let ([c-comparator (car condition)] [c-column (cadr condition)] [c-value (caddr condition)])
                       (apply c-comparator (list (cdar (filter (λ (one-pair)
                                                                 (eq? (car one-pair) c-column)) row)) c-value)))) conditions)) table)))

(define remake-table-columns
  (λ (rows)
    (map (λ (column-name column-values)
           (cons column-name column-values)) (map (λ (name-value-pair)
                                                    (car name-value-pair)) (car rows)) (apply map (append (list (λ args (map (λ (one-pair)
                                                                                                                               (cdr one-pair)) args))) rows)))))
(define select-table-columns
  (λ (columns operations)
    (map (λ (operation)
           (let ([column (if (pair? operation) (cdr operation) operation)] [operation-name (if (pair? operation) (car operation) #f)])
             (let ([selected-column (car (filter (λ (full-column)
                                                   (eq? (car full-column) column)) columns))])
               (if operation-name
                   (column-operation operation-name (cdr selected-column))
                   (cdr selected-column))))) operations)))

(define select
  (λ (db table-name columns conditions)
    (select-table-columns (remake-table-columns (filter-column-value-pairs (make-column-value-pairs (get-table db table-name)) conditions)) columns)))

;====================================
;=             Cerința 4            =
;=           Operația update        =
;=            20 de puncte          =
;====================================
(define update
  (λ (db table-name values conditions)
    'your-code-here))

;====================================
;=             Cerința 5            =
;=           Operația remove        =
;=              10 puncte           =
;====================================
(define delete
  (λ (db table-name conditions)
    'your-code-here))

;====================================
;=               Bonus              =
;=            Natural Join          =
;=            20 de puncte          =
;====================================
(define natural-join
  (λ (db tables columns conditions)
    'your-code-here))

;====================================
;=           Fill database          =
;====================================
(set! db (insert db "Studenți" (list '("Nume" . "Ionescu")
                            '("Prenume" . "Gigel")
                            '("Număr matricol" . 123)
                            '("Grupă" . "321CA")
                            '("Medie" . 9.82))))
(set! db (insert db "Studenți" (list '("Nume" . "Popescu")
                            '("Prenume" . "Maria")
                            '("Număr matricol" . 124)
                            '("Grupă" . "321CB")
                            '("Medie" . 9.91))))
(set! db (insert db "Studenți" (list '("Nume" . "Popa")
                            '("Prenume" . "Ionel")
                            '("Număr matricol" . 125)
                            '("Grupă" . "321CC")
                            '("Medie" . 9.99))))
(set! db (insert db "Studenți" (list '("Nume" . "Georgescu")
                            '("Prenume" . "Ioana")
                            '("Număr matricol" . 126)
                            '("Grupă" . "321CD")
                            '("Medie" . 9.87))))
(set! db (insert db "Cursuri" (list '("Anul" . "I")
                            '("Semestru" . "I")
                            '("Disciplină" . "Programarea calculatoarelor")
                            '("Număr credite" . 5)
                            '("Număr teme" . 2))))
(set! db (insert db "Cursuri" (list '("Anul" . "II")
                            '("Semestru" . "II")
                            '("Disciplină" . "Paradigme de programare")
                            '("Număr credite" . 6)
                            '("Număr teme" . 3))))
(set! db (insert db "Cursuri" (list '("Anul" . "III")
                            '("Semestru" . "I")
                            '("Disciplină" . "Algoritmi paraleli și distribuiți")
                            '("Număr credite" . 5)
                            '("Număr teme" . 3))))
(set! db (insert db "Cursuri" (list '("Anul" . "IV")
                            '("Semestru" . "I")
                            '("Disciplină" . "Inteligență artificială")
                            '("Număr credite" . 6)
                            '("Număr teme" . 3))))
(set! db (insert db "Cursuri" (list '("Anul" . "I")
                            '("Semestru" . "II")
                            '("Disciplină" . "Structuri de date")
                            '("Număr credite" . 5)
                            '("Număr teme" . 3))))
(set! db (insert db "Cursuri" (list '("Anul" . "III")
                            '("Semestru" . "II")
                            '("Disciplină" . "Baze de date")
                            '("Număr credite" . 5)
                            '("Număr teme" . 0))))