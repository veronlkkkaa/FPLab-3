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

### Архитектура на основе полиморфизма

Реализация использует **мультиметоды** для полиморфной обработки различных типов интерполяторов.

### Структура состояния

Состояние представляет собой **вектор интерполяторов**, где каждый интерполятор — это map с полями:
- `:type` — тип интерполятора (`:linear` или `:newton`)
- `:points` — очередь точек (PersistentQueue)
- `:next-x` — следующая координата для вывода
- `:n` — параметр n (только для Newton)

``` Clojure
;; Пример состояния с двумя активными интерполяторами:
[{:type :linear
  :points #queue [...]
  :next-x 5.0}
 {:type :newton
  :points #queue [...]
  :next-x 5.0
  :n 4}]
```

### Инициализация состояния

Функция `init-state` создает вектор интерполяторов на основе опций, используя `cond->`:

``` Clojure
(defn init-state [opts]
  (cond-> []
    (:linear? opts)
    (conj {:type :linear
           :points clojure.lang.PersistentQueue/EMPTY
           :next-x nil})

    (:newton? opts)
    (conj {:type :newton
           :points clojure.lang.PersistentQueue/EMPTY
           :next-x nil
           :n (:n opts)})))
```

Это позволяет добавлять только нужные интерполяторы при старте.

### Мультиметод `process-interpolator`

Главный полиморфный интерфейс с диспетчеризацией по типу интерполятора:

``` Clojure
(defmulti process-interpolator
  (fn [interpolator _ _ _] (:type interpolator)))

(defmethod process-interpolator :linear
  [interpolator new-point step max-x]
  ;; обрабатывает точку для линейной интерполяции
  {:interpolator обновленный-интерполятор
   :outputs [{:alg :linear :x ... :y ...}]})

(defmethod process-interpolator :newton
  [interpolator new-point step max-x]
  ;; обрабатывает точку для интерполяции Ньютона
  {:interpolator обновленный-интерполятор
   :outputs [{:alg :newton :x ... :y ...}]})
```

### Обработка точек без дублирования кода

Функция `handle-datapoint` использует `map` для обработки всех интерполяторов:

``` Clojure
(defn handle-datapoint [opts state point]
  (let [step (:step opts)
        max-x (:x point)

        ;; Обрабатываем все интерполяторы через map
        results (map (fn [interpolator]
                       (process-interpolator interpolator point step max-x))
                     state)

        ;; Извлекаем новые интерполяторы и выходы
        new-interpolators (mapv :interpolator results)
        all-outputs (mapcat :outputs results)]

    {:state new-interpolators
     :outputs (vec all-outputs)}))
```

Нет условных конструкций `if` — все алгоритмы обрабатываются единообразно через полиморфизм.

### Финализация выходов

Мультиметод `finalize-interpolator` генерирует оставшиеся выходы после EOF:

``` Clojure
(defmulti finalize-interpolator
  (fn [interpolator _step] (:type interpolator)))

(defmethod finalize-interpolator :linear [interpolator step]
  ;; генерирует оставшиеся точки для линейной интерполяции
  ...)

(defmethod finalize-interpolator :newton [interpolator step]
  ;; генерирует оставшиеся точки для интерполяции Ньютона
  ...)

(defn finalize-outputs [opts state]
  (let [step (:step opts)
        all-outputs (mapcat #(finalize-interpolator % step) state)]
    (vec all-outputs)))
```

### Вспомогательные функции

**Для линейной интерполяции:**
- `find-segment` — находит пару соседних точек [p1 p2], таких что x1 ≤ x ≤ x2
- `linear-interpolate` — вычисляет линейную интерполяцию в точке x

**Для метода Ньютона:**
- `choose-window` — выбирает окно из N ближайших точек
- `calc-coefficients` — строит таблицу разделённых разностей
- `newton-eval` — вычисляет полином по схеме Горнера
- `newton-interpolate` — полная интерполяция в точке x

### Преимущества архитектуры

1. **Нет дублирования кода** — обработка всех алгоритмов выполняется через `map`
2. **Расширяемость** — добавление нового алгоритма требует только:
   - Добавить новый `defmethod` для `process-interpolator`
   - Добавить новый `defmethod` для `finalize-interpolator`
   - Обновить `init-state` для создания нового типа интерполятора
3. **Чистый функциональный стиль** — использование `cond->`, `map`, `mapcat` вместо условных конструкций

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
    (loop [state (interp/init-state opts)]
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
