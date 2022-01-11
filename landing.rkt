#!/usr/bin/env racket
#lang racket

;;; Игра "Посадка на луну". Реализована при помощи 2htdp/universe.
;;; Управление:
;;; * Cтрелка влево/вправо - поворот влево/вправо
;;; * Стрелка вверх - ускорение
;;;
;;; Н. И. Подгрный / Nikon Podgorny <podgorny.nikon@yandex.ru>, 2021, 2022

(require 2htdp/universe
         2htdp/image
         lang/posn)

;; Размеры игрового пространства (сцены)
(define width 1600)
(define height 900)

;; Позиция для оторажения "земли"
(define earth-x (- width 200))
(define earth-y 200)

;; Цвет частиц выхлопов двигателя.
(define particle-color (color 240 149 96 255))

;; Основной зеленый цвет для отображения всего.
(define green-main (color 53 222 53 255))

;; Вторичный зеленый цвет для теней и т.п..
(define green-secondary (color 180 209 151 100))

;; Основной красный цвет.
(define red-main (color 222 78 53 255))

;; Расстояние до поверхности для приземления.
(define landing-distance 40)

;; Угол (в градусах) максимального наклона для безопасного приземления.
(define landing-safe-angle 30)

;; Угол (в градусах) максимального отклонения от поверхности при посадке.
(define landing-max-diff 10)

;; Максимальная скорость для безопасного приземления.
(define landing-safe-speed 7)

;; Объем бака с топливом.
(define fuel-full 500)

;; Потребление топливая за один тик работы двигателя.
(define fuel-consumption-per-tick 1)

;; Модуль ускорения при нажатии на кнопку "вперед".
(define key-accel 1.2)

;; Модуль угловой скорости при нажатии на кнопки поворота.
(define key-ω (/ pi 50))

;; Множитель для гашения скорости.
(define damp 0.90)

;; Стили для отображения
;; Поверхрности
(define land-pen (pen green-main 5 "solid" "round" "round"))
;; Вектора скорости
(define vector-pen (pen "blue" 5 "long-dash" "round" "round"))

;; Состояние "мира".
(struct world
  (x y           ; координаты корабля
   vx vy         ; вектор скорости
   accel         ; модуль ускорения
   α             ; угол поворота относительно Ox
   ω             ; угловая скорость
   particles     ; список частиц
   land-points   ; отрезки поверхности
   ship-speed    ; скорость корабля
   nearest-land  ; ближайшая поверхность
   fuel-left)    ; запас топлива
  #:transparent)

;; Отрезок поверхности
(struct land-part 
  (x1 y1  ; координаты начала отрезка 
   x2 y2) ; координаты конца отрезка
  #:transparent)

;; Приближение к поверхности
(struct landing
  (dist
   land-angle)
  #:transparent)

;; Добавление элемента в список.
(define (append-element lst elem)
  (append lst (list elem)))

;; Генерирует n случайных точек для отрезков поверхности
;; (соответсвенно генерируется n-1 отрезок) 
;; Позиции точек по оси X равномерно распределяется на отрезке [from, to]
;; Позиции точек по оси Y выбираюися случайно в диапозоне [miny, maxy]
;; Возвращает координаты в списке вида ((x1 x2 ... xn) (y1 y2 ... yn))
;; Пример:
;; (random-land-posns 5 0 12 0 2) ---> ((0 3 6 9 12) (0 0 1 1 0))
;;
;;1       *--*
;;       /    \
;;      /      \
;;0 *--*        *
;;  0  3  6  9  12
;;
(define (random-land-posns n from to miny maxy)
  (define (step-len) (/ (- to from) (- n 1)))
  (define (random-list Xarr Yarr count)
    (if (= count n)
      (list Xarr Yarr)
      (random-list 
        (append-element Xarr (+ from (* (step-len) count))) 
        (append-element Yarr (random miny maxy)) 
        (+ count 1))))

  (random-list '() '() 0))

;; По заданному списку точек points и нелевому уровню Yzero
;; строит список отрезков (land-part) поверхности
(define (generate-land points Yzero)
  (define (generate-loop arr Xcords Ycords)
    (if (< (length Xcords) 2)
        arr
        (generate-loop 
          (append-element arr 
            (land-part 
              (car Xcords) 
              (- Yzero (car Ycords))
              (cadr Xcords)
              (- Yzero (cadr Ycords))))
          (cdr Xcords)
          (cdr Ycords))))
  (generate-loop '() (car points) (cadr points)))

;; Исходное состояние
(define world0 (world 
  (/ width 2) 100    ; x, y
  0 0                ; vx vy 
  0                  ; accel 
  (* pi -1/2)        ; α
  0                  ; ω 
  '()                ; particles
  (generate-land     ; land-points
    (random-land-posns 10 0 width -100 100) 
    (- height 100))
  0                  ; ship-speed 
  (landing +inf.f 0) ; nearest-land 
  fuel-full))        ; fuel-left

;; Частица.
(struct particle
  (x y        ; координаты
   vx vy      ; скорость
   ttl)       ; время жизни
  #:transparent)

;; Обработка нажатий и отпусканий кнопок.
;; press? — истина, если клавиша нажата, ложь — отпущена.
;; w — состояние игрового мира,
;; k — клавиша.
(define ((control press?) w k)
  (let ((new-a (if press? key-accel 0.0))
        (new-ω (if press? key-ω 0.0)))
    (cond
      ((key=? k "up")
       (struct-copy world w (accel new-a)))
      ((key=? k "left")
       (struct-copy world w (ω (- new-ω))))
      ((key=? k "right")
       (struct-copy world w (ω new-ω)))
      (else w))))

;; Создает одну частицу.
(define (emit-particle w)
  ;; Захват нужных полей структуры world.
  (match-define
    (struct* world ((x x) (y y) (vx vx) (vy vy) (α α)))
    w)
  ;; Вылетает в направлении противоположном направлению ракеты со
  ;; случайной скоростью и случайным отклонением.
  (let* ((pα (+ α (* 0.9 (- (random) 0.5)))) ; угол направления
         (pv (+ 5 (* (random) 10)))          ; значение скорости
         (pvx (- (* (cos pα) pv)))           ; вектор скорости
         (pvy (- (* (sin pα) pv))))
    ;; Координаты и скорость были вычислены относительно ракеты,
    ;; получаем их относительно игрового пространства.
    (particle (+ x (* (cos α) -30))
              (+ y (* (sin α) -30))
              (+ pvx vx)         
              (+ pvy vy)
              ;; Случайное время жизни.
              (random 3 8))))

;; Возвращает список новых частиц.
(define (emit-particles w)
  (build-list 10 (λ _ (emit-particle w))))

;; Обновляет состояние частицы p.
(define (update-particle p)
  (match-define (particle x y vx vy ttl) p)
  (particle (+ x vx)
            (+ y vy)
            vx
            vy
            (sub1 ttl)))

;; Обновляет состояния частиц в списке particles и удаляет частицы с
;; истекшим временем жизни.
(define (update-particles particles)
  (map update-particle
       (filter (λ (p) (positive? (particle-ttl p)))
               particles)))

;; Возвращает новое состояние мира по прошествии единицы времени.
(define (tick w)
  (match-define (world x y vx vy accel α ω particles land speed nearest-land fuel-left) w)
  (let* (;; Поворот (применяем угловую скорость).
         (α (clamp (+ α ω) (* pi -1) 0))

         ;; Вектор ускорения.
         ;; Если топливо закончилось, то ускорение = 0
         (actual-accel (if (> fuel-left 0) accel 0))
         (ax (* (cos α) actual-accel))
         (ay (+ (* (sin α) actual-accel) 0.8))

         ;; Сколько топлива останется
         (new-fuel-left (clamp (if (> actual-accel 0 ) 
                                   (- fuel-left fuel-consumption-per-tick) 
                                   fuel-left) 0 fuel-full))

         ;; Вектор скорости.
         (vx (+ vx ax))
         (vy (+ vy ay))

         ;; Перемещаем на значение скорости и переносим на
         ;; противоположную сторону при выходе за границы игрового
         ;; пространства по оси X, ограничиваем перемещени по оси Y
         ;; #------x------#
         ;; |      |      |
         ;; <------|------>
         ;; |      |      |
         ;; #------x------#
         (x (modulo (round (+ x vx)) width))
         (y (clamp (round (+ y vy)) 0 height))

         ;; Находим ближайщую к кораблю поверхность.
         (nearest (find-nearest-land x y land))

         ;; Обновляем частицы.
         (particles (update-particles particles))
         
         ;; Добавляем новые частицы, если двигатель работает.
         (particles (if (zero? actual-accel)
                        particles
                        (append (emit-particles w) particles))))
    
    (world x y 
           (* vx damp) (* vy damp) 
           actual-accel α ω 
           particles land 
           (calc-speed vx vy) 
           nearest 
           new-fuel-left)))

;; Изображение корабля
(define lunar-module (bitmap "assets/ship.png"))
(define ship-pic
  (let* ((the-ship lunar-module)
         (w (image-width the-ship))
         (h (image-height the-ship))
         (d (max w h)))
    (place-image/align the-ship
                       (- d (/ w 2))
                       (- d (/ h 2))
                       'left 'top
                       (circle d 'solid (color 0 0 0 0)))))

;; Изображение земли
(define earth (bitmap "assets/earth.png"))

;; Изображение частицы.
(define particle-pic (star 8 "solid" particle-color))

;; Добавляет частицу p к сцене scene.
(define (place-particle p scene)
  (place-image particle-pic
               (particle-x p)
               (particle-y p)
               scene))

;; Отображаем отрезки поверхности с точками points
;; На сцене scene
(define (draw-land scene points)
  (if (empty? points)
    scene
    (draw-land 
      (let ((point (car points)))
        (scene+line scene
          (land-part-x1 point) 
          (land-part-y1 point) 
          (land-part-x2 point) 
          (land-part-y2 point)
          land-pen))
      (cdr points))))

;; Отображаем вектор устроения (vx, vy) корабля в точке (x, y)
;; На сцене scene
(define (draw-accel-vector scene x y vx vy)
  (scene+line scene
    x y
    (+ x (* vx 10))
    (+ y (* vy 10))
    vector-pen))

;; Ограничение значения x отрезком [minx, maxx]
(define (clamp x minx maxx)
  (max (min maxx x) minx))

;; Вычисление расстояния между двумя точками
(define (calc-dist-2-points x1 y1 x2 y2)
  (sqrt (+ (sqr (- x2 x1)) (sqr (- y2 y1)))))

;; Вычисление угла наклона отрезка типа land-part.
;;          
;;          y
;;          |   @ (x2, y2)
;;          |  /
;;          | /
;;          |/ искомый угол
;; (x1, y1) @---------------x
;;
(define (calc-angle-of-segment segment)
  (atan (- (land-part-y2 segment) (land-part-y1 segment)) 
        (- (land-part-x2 segment) (land-part-x1 segment))))

;; Расчёт скорости корабля (длина вектора скорости)
(define (calc-speed vx vy)
  (calc-dist-2-points 0 0 vx vy))

;; Расчёт минимального расстояния от точки x) до отрезка ab
;;    x
;;    | dist
;;    |
;; a---------b
;;
;;              x
;;             / dist
;;            /
;; a---------b
;; 
(define (calc-dist-to-segment x y segment)
  ; a, b, c - коэфициенты прямой
  (let* ((ax (land-part-x1 segment)) ; координаты начала
         (ay (land-part-y1 segment))
         (bx (land-part-x2 segment)) ; координаты конца
         (by (land-part-y2 segment))

         ;; Коэфиценты прямой
         (a (- ay by))
         (b (- bx ax))
         (c (- (* ax by) (* bx ay)))

         ;; Скалярное произведение ba и bx
         (scal-prod1 (+ (* (- ax bx) (- x bx)) (* (- ay by) (- y by))))
         ;; Скалярное произведение ab и ax
         (scal-prod2 (+ (* (- bx ax) (- x ax)) (* (- by ay) (- y ay))))

         ;; Длина перпендикуляра из точки x на прямую (не отрезок) ab
         (normal-dist (/ (abs (+ (* a x) (* b y) c)) (sqrt (+ (sqr a) (sqr b)))))

         ;; Расстояние до начала отрезка ab
         (dist-to-begin (calc-dist-2-points x y ax ay))
         ;; Расстояние до конца отрезка ab
         (dist-to-end (calc-dist-2-points x y bx by)))
       (cond 
         ((and (= a 0) (= b 0)) dist-to-begin)          ; отрезок ab - точка
         ((>= (* scal-prod1 scal-prod2) 0) normal-dist) ; перпендикуляр из точки x падает на ab
         (else (min dist-to-begin dist-to-end)))))      ; перпендикуляр из точки x НЕ падает на ab

;; Ищет ближайщую поверхность из списка segments.
;; Возвращает структуру landing.
(define (find-nearest-land shipX shipY segments)
  (define (find-min-loop min-dist min-part-angle rest-of-segments)
    (if (empty? rest-of-segments)
      (landing min-dist (radians->degrees min-part-angle))
      (let* ((segment (car rest-of-segments))
             (dist (calc-dist-to-segment shipX shipY segment))
             (new-rest (cdr rest-of-segments)))
             (if (< dist min-dist)
               (find-min-loop dist (calc-angle-of-segment segment) new-rest)
               (find-min-loop min-dist min-part-angle new-rest)))))
  (find-min-loop +inf.f 0 segments))

;; Прогрес-бар оставшегося топлива.
(define (draw-fuel-bar fuel)
  (overlay/align "left" "bottom"
    (rectangle (* width (/ fuel fuel-full)) 20 "solid" green-main)
    (rectangle width 20 "solid" green-secondary)))

(define (draw w)
  (let* ((α (radians->degrees(+ (world-α w) (* pi 1/2))))
         (speed (world-ship-speed w))
         (fuel (world-fuel-left w))
         (α-string (string-join (list "Угол наклона:" (number->string (round α)) "°")))
         (speed-string (string-join (list "Скорость:" (number->string (round speed)) "уе/c")))
         (fuel-string (string-join (list "Топливо:" (number->string fuel) "уе")))
         (angle-text (text α-string 24 (if (< (abs α) landing-safe-angle) green-main red-main)))
         (speed-text (text speed-string 24 (if (< speed landing-safe-speed) green-main red-main)))
         (fuel-text (text fuel-string 24 green-main))
         (s (empty-scene width height "black"))
         (s (place-image earth
                         earth-x earth-y
                         s))
         (s (foldl place-particle s (world-particles w)))
         (s (draw-accel-vector s (world-x w) (world-y w) (world-vx w) (world-vy w)))
         (s (draw-land s (world-land-points w)))
         (s (place-image (rotate (radians->degrees
                                  (- (* pi 3/2)
                                     (world-α w)))
                                 ship-pic)
                         (world-x w) (world-y w)
                         s))
         (s (place-image/align angle-text
                         20 20
                         "left" "top"
                         s))
         (s (place-image/align speed-text
                         20 50
                         "left" "top"
                         s))
         (s (place-image/align fuel-text
                         20 80
                         "left" "top"
                         s))
         (s (place-image/align (draw-fuel-bar fuel)
                               0 height
                               "left" "bottom"
                               s)))
    s))

;; Проверка завершения посадки.
(define (landing-done? dist-to-land)
  (< dist-to-land landing-distance))

;; Проверка на успех посадки.
(define (landing-success? ship-angle ship-speed land-angle)
  (and (<= ship-angle landing-safe-angle) 
       (<= ship-speed landing-safe-speed)
       (<= (- ship-angle land-angle) landing-max-diff)))

;; Условие завершения игры.
(define (done? w)
  (let* ((nearest (world-nearest-land w))
         (dist (landing-dist nearest)))
        (landing-done? dist)))

;; Экран результата.
(define (result-screen w str color)
  (underlay (draw w)              ; Последний кадр
            (text str 24 color))) ; Текст

(define (last-picture w)
  (let* ((nearest (world-nearest-land w))
         (ship-angle (radians->degrees(+ (world-α w) (* pi 1/2))))
         (ship-speed (world-ship-speed w))
         (land-angle (landing-land-angle nearest))
         (angle-ok (<= ship-angle landing-safe-angle))
         (speed-ok (<= ship-speed landing-safe-speed))
         (angle-diff-ok (<= (abs (- ship-angle land-angle)) landing-max-diff)))
        (cond 
          ((not angle-ok) 
           (result-screen w "Корабль разбился! Слишком большой угол наклона!" "red"))
          ((not speed-ok)
           (result-screen w "Корабль разбился! Слишком большая скорость!" "red"))
          ((not angle-diff-ok)
           (result-screen w "Корабль разбился! Слишком большое отклонение от поверхности!" "red"))
          (else
           (result-screen w "Успешная посадка!" "green")))))

(define (start)
  (big-bang world0
          (on-tick tick)
          (on-key (control #t))
          (on-release (control #f))
          (to-draw draw)
          (stop-when done? last-picture)))


(module+ main
  (start))