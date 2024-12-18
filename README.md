<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 3</b><br/>
"Конструктивний і деструктивний підходи до роботи зі списками"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>: Гармаш Дмитро Олегович КВ-13</p>
<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання

Реалізуйте алгоритм сортування чисел у списку двома способами: функціонально і імперативно.

1. Функціональний варіант реалізації має базуватись на використанні рекурсії і
   конструюванні нових списків щоразу, коли необхідно виконати зміну вхідного
   списку. Не допускається використання: деструктивних операцій, циклів, функцій
   вищого порядку або функцій для роботи зі списками/послідовностями, що
   використовуються як функції вищого порядку. Також реалізована функція не має
   бути функціоналом (тобто приймати на вхід функції в якості аргументів).

2. Імперативний варіант реалізації має базуватись на використанні циклів і
   деструктивних функцій (псевдофункцій). Не допускається використання функцій
   вищого порядку або функцій для роботи зі списками/послідовностями, що
   використовуються як функції вищого порядку. Тим не менш, оригінальний список
   цей варіант реалізації також не має змінювати, тому перед виконанням
   деструктивних змін варто застосувати функцію copy-list (в разі необхідності).
   Також реалізована функція не має бути функціоналом (тобто приймати на вхід
   функції в якості аргументів).
   Алгоритм, який необхідно реалізувати, задається варіантом (п. 3.1.1). Зміст і шаблон
   звіту наведені в п. 3.2.
   Кожна реалізована функція має бути протестована для різних тестових наборів. Тести
   мають бути оформленні у вигляді модульних тестів (наприклад, як наведено у п. 2.3).

## Варіант 1

Алгоритм сортування вибором за незменшенням.

## Лістинг функції з використанням конструктивного підходу

;; Функція знаходження мінімального елемента в списку
(defun find-min (lst)
  "Знаходить мінімальний елемент у списку lst."
  (if (null (cdr lst))            ; Якщо список містить лише один елемент
      (car lst)                   ; Повертаємо цей елемент
      (let ((min-rest (find-min (cdr lst)))) ; Рекурсивно шукаємо мінімум у хвості
        (if (< (car lst) min-rest) ; Порівнюємо перший елемент із мінімумом хвоста
            (car lst)              ; Повертаємо мінімальний елемент
            min-rest))))           ; Або залишаємо мінімум із хвоста

;; Функція видалення першого входження елемента
(defun remove-first (elem lst)
  "Видаляє перше входження elem у списку lst."
  (cond
    ((null lst) nil)              ; Якщо список порожній, повертаємо nil
    ((eql (car lst) elem) (cdr lst)) ; Якщо перший елемент збігається, пропускаємо його
    (t (cons (car lst)            ; Інакше додаємо перший елемент до нового списку
             (remove-first elem (cdr lst)))))) ; Рекурсивно обробляємо хвіст

;; Функція сортування вибором (функціональний підхід)
(defun selection-sort-functional (lst)
  "Сортує список lst за допомогою сортування вибором (функціональний підхід)."
  (if (null lst)                  ; Якщо список порожній
      nil                         ; Повертаємо порожній список
      (let ((min (find-min lst))) ; Знаходимо мінімальний елемент
        (cons min                  ; Додаємо мінімум до нового списку
              (selection-sort-functional (remove-first min lst)))))) ; Рекурсивно сортуємо решту
```

### Тестові набори

```lisp
(defun test-selection-sort-functional ()
  "Тести для функції selection-sort-functional."
  (check-sort "Test 1: simple list" 'selection-sort-functional '(3 1 4 1 5 9 2) '(1 1 2 3 4 5 9))
  (check-sort "Test 2: empty list" 'selection-sort-functional '() '())
  (check-sort "Test 3: single element" 'selection-sort-functional '(42) '(42))
  (check-sort "Test 4: already sorted" 'selection-sort-functional '(1 2 3 4) '(1 2 3 4))
  (check-sort "Test 5: reverse sorted" 'selection-sort-functional '(5 4 3 2 1) '(1 2 3 4 5)))
```

### Тестування

```lisp
passed... Test 1: simple list

Expected: (1 1 2 3 4 5 9)
Got: (1 1 2 3 4 5 9)

passed... Test 2: empty list

Expected: NIL
Got: NIL

passed... Test 3: single element

Expected: (42)
Got: (42)

passed... Test 4: already sorted

Expected: (1 2 3 4)
Got: (1 2 3 4)

passed... Test 5: reverse sorted

Expected: (1 2 3 4 5)
Got: (1 2 3 4 5)
```

## Лістинг функції з використанням деструктивного підходу

```lisp
;; Функція знаходження індексу мінімального елемента
(defun find-min-destructive (lst)
  "Знаходить індекс мінімального елемента у списку lst."
  (let ((min (car lst)) (index 0) (min-index 0))
    (dolist (elem (cdr lst) min-index) ; Перебираємо елементи хвоста списку
      (incf index)                     ; Збільшуємо індекс
      (when (< elem min)               ; Якщо поточний елемент менший за мінімальний
        (setf min elem                 ; Оновлюємо мінімальний елемент
              min-index index)))))     ; Оновлюємо індекс мінімального елемента

;; Функція для обміну елементів у списку
(defun swap (lst i j)
  "Міняє місцями елементи lst з індексами i та j."
  (let ((temp (nth i lst)))           ; Зберігаємо значення i-го елемента
    (setf (nth i lst) (nth j lst))    ; Замінюємо i-й елемент j-м
    (setf (nth j lst) temp)))         ; Замінюємо j-й елемент i-м

;; Функція сортування вибором (деструктивний підхід)
(defun selection-sort-imperative (lst)
  "Сортує список lst за допомогою сортування вибором (деструктивний підхід)."
  (let ((copy (copy-list lst)))       ; Створюємо копію списку
    (dotimes (i (1- (length copy)))   ; Ітерація до передостаннього елемента
      (let ((min-index (find-min-destructive (subseq copy i)))) ; Знаходимо мінімум у хвості
        (swap copy i (+ i min-index)))) ; Міняємо місцями поточний і мінімальний елементи
    copy))                           ; Повертаємо відсортований список
```

### Тестові набори

```lisp
(defun test-selection-sort-imperative ()
  "Тести для функції selection-sort-imperative."
  (check-sort "Test 1: simple list" 'selection-sort-imperative '(3 1 4 1 5 9 2) '(1 1 2 3 4 5 9))
  (check-sort "Test 2: empty list" 'selection-sort-imperative '() '())
  (check-sort "Test 3: single element" 'selection-sort-imperative '(42) '(42))
  (check-sort "Test 4: already sorted" 'selection-sort-imperative '(1 2 3 4) '(1 2 3 4))
  (check-sort "Test 5: reverse sorted" 'selection-sort-imperative '(5 4 3 2 1) '(1 2 3 4 5)))
```

### Тестування

```lisp
passed... Test 1: simple list

Expected: (1 1 2 3 4 5 9)
Got: (1 1 2 3 4 5 9)

passed... Test 2: empty list

Expected: NIL
Got: NIL

passed... Test 3: single element

Expected: (42)
Got: (42)

passed... Test 4: already sorted

Expected: (1 2 3 4)
Got: (1 2 3 4)

passed... Test 5: reverse sorted

Expected: (1 2 3 4 5)
Got: (1 2 3 4 5)
```
