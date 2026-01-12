# Лабораторная работа №3

## Использованные алгоритмы интерполяции

---
**Выполнила:** Гаврилович Вероника Вячеславовна 
**Группа:** Р3331  
**Преподаватель:** Пенской Александр Владимирович  
**Язык:** Clojure
---


### Задача
Реализовать потоковую интерполяцию с поддержкой нескольких алгоритмов:
- Линейная интерполяция отрезками
- Интерполяция Ньютона

### Линейная интерполяция
Аппроксимация между узлами осуществляется линейной функцией:

$$y = y_1 + \frac{x - x_1}{x_2 - x_1} \cdot (y_2 - y_1)$$

Используется для приближённого вычисления значения функции между двумя известными точками.
Формула:

$$t = \frac{x - x_1}{x_2 - x_1}$$

$$y = y_1 + t (y_2 - y_1)$$

---

### **2. Интерполяция полиномом Ньютона**

Используется полином в форме Ньютона:

$$P(x)=a_0 + a_1(x-x_0) + a_2(x-x_0)(x-x_1) + \ldots$$

Коэффициенты вычисляются через таблицу разделённых разностей:

$f[x_i] = y_i$

$f[x_i, x_{i+1}] = \frac{f[x_{i+1}] - f[x_i]}{x_{i+1}-x_i}$

$f[x_i, x_{i+1}, x_{i+2}] =
\frac{f[x_{i+1},x_{i+2}] - f[x_i,x_{i+1}]}
{x_{i+2} - x_i}$

Последовательность первых элементов каждой строки таблицы разностей формирует коэффициенты
([a_0, a_1, a_2, …]).

Вычисление значения полинома выполняется по схеме Горнера для формы Ньютона:

$P(x) = a_0 + (x - x_0)\big[a_1 + (x - x_1)[a_2 + \cdots]\big]$

---


## Ключевые элементы реализации

## Модуль `interpolation`

### Единый полиморфный интерфейс

Вся функциональность реализована через **один мультиметод** с диспетчеризацией по кортежу `[операция алгоритм]`:

``` Clojure
(defmulti interpolate
  "Главный полиморфный интерфейс для интерполяции"
  (fn [operation alg & _] [operation alg]))
```

### Поддерживаемые операции

**`:compute`** — вычисление значения интерполяции
``` Clojure
(interpolate :compute :linear points x)
(interpolate :compute :newton points n x)
```

**`:process`** — обработка входящей точки
``` Clojure
(interpolate :process :linear opts state point)
(interpolate :process :newton opts state point)
```

**`:ready?`** — проверка готовности алгоритма
``` Clojure
(interpolate :ready? :linear state opts)
(interpolate :ready? :newton state opts)
```

**`:max-points`** — получение максимального размера окна
``` Clojure
(interpolate :max-points :linear opts)  ; => 2
(interpolate :max-points :newton opts)  ; => (inc n)
```

### Реализации для линейной интерполяции

``` Clojure
(defmethod interpolate [:compute :linear]
  [_ _ points x]
  ;; находит сегмент и вычисляет y = y1 + t*(y2-y1)
  ...)

(defmethod interpolate [:process :linear]
  [_ alg opts state point]
  ;; добавляет точку, ограничивает размер, генерирует выходы
  ...)
```

### Реализации для метода Ньютона

``` Clojure
(defmethod interpolate [:compute :newton]
  [_ _ points n x]
  ;; выбирает окно, строит разделённые разности, вычисляет полином
  ...)

(defmethod interpolate [:process :newton]
  [_ alg opts state point]
  ;; добавляет точку с параметром n, ограничивает размер, генерирует выходы
  ...)
```

### Вспомогательные функции

**Для линейной интерполяции:**
- `find-segment` — находит пару соседних точек [p1 p2], таких что x1 ≤ x ≤ x2

**Для метода Ньютона:**
- `choose-window` — выбирает окно из N ближайших точек
- `calc-coefficients` — строит таблицу разделённых разностей
- `newton-eval` — вычисляет полином по схеме Горнера

### Потоковая обработка
- `handle-datapoint` — координирует обработку для всех активных алгоритмов через единый интерфейс `interpolate`
- `produce-outputs-for-alg` — генерирует результаты на промежутке с заданным шагом
- `init-state` — инициализация начального состояния

---

## Модуль `core`

- parse-dbl/int - обертки для красоты для парсинга строки в double/int.
- parse-args - Парсер аргументов командной строки.
``` Clojure
(defn parse-args
  "Парсер аргументов командной строки"
  [args]
  (loop [m    {:linear? false
               :newton? false
               :step    1.0
               :n       4}
         args args]
    (if (empty? args)
      (do
        (when (and (not (:linear? m))
                   (not (:newton? m)))
          (binding [*out* *err*]
            (println "Error: at least one of --linear or --newton must be given"))
          (System/exit 1))
        (when (<= (:step m) 0)
          (binding [*out* *err*]
            (println "Error: --step must be > 0"))
          (System/exit 1))
        m)

      (let [[a & rest-args] args]
        (cond
          (= a "--linear")
          (recur (assoc m :linear? true) rest-args)

          (= a "--newton")
          (recur (assoc m :newton? true) rest-args)

          (= a "--step")
          (recur (assoc m :step (parse-dbl (first rest-args)))
                 (rest rest-args))

          (or (= a "-n") (= a "--n"))
          (recur (assoc m :n (parse-int (first rest-args)))
                 (rest rest-args))

          :else
          (throw (ex-info (str "Unexpected argument: " a) {})))))))
```

Поддерживает параметры:
* `--linear` — включить линейную интерполяцию,
* `--newton` — включить интерполяцию Ньютона,
* `--step <число>` — задать шаг интерполяции,
* `-n <число>` или `--n <число>` — количество точек для Ньютона.


- parse-point - Парсит строку вида:

```
x y
x;y
x,y
```

Возвращает структуру `{:x ..., :y ...}` или `nil`.

- fmt3 - Форматирует число с 3 знаками после запятой, убирает лишние нули.

- -main
``` Clojure
(defn -main [& args]
  (let [opts (parse-args args)]
    (loop [state (interp/init-state)]
      (when-some [line (read-line)]
        (if-let [p (parse-point line)]
          (let [{:keys [state outputs]}
                (interp/handle-datapoint opts state p)]
            (doseq [{:keys [alg x y]} outputs]
              (println (format "%s: %s %s"
                               (name alg) (fmt3 x) (fmt3 y))))
            (recur state))
          (recur state))))))
```
Выполняет:

1. Парсинг аргументов.
2. Инициализацию состояния интерполятора.
3. Чтение строк из stdin.
4. Парсинг входных точек.
5. Вызов `handle-datapoint` для каждой новой точки.
6. Вывод всех вычисленных значений интерполяции.

Работает в потоковом режиме.

---

## Тесты
Были реализованы:
- property-based тесты (проверка монотонности и совпадения линейной интерполяции, как самой простой, с аналитическим значением);
- юнит тесты для каждого алгоритма на совпадение с ожидаемым результатом (interpolation-test)
- core тесты (проверяют корректность парсинга, обработку валидных/невалидных входных данных, корректность вывода и т.д.)
