(ns alphabet-cipher.coder)

(defn rotate [n l] (concat (drop n l) (take n l)))

(def alpha-start (int \a))

(def substitution-chart
  (let [alphabet (map char (range alpha-start (inc (int \z))))]
  (for [row alphabet]
    (rotate
      (- (int row) alpha-start)
      alphabet))))

(defn encode-char [x y]
  (nth (nth substitution-chart (- (int x) alpha-start))
    (- (int y) alpha-start)))

(defn normalise-keyword [keyword len]
  (let [repetitions (inc (quot len (count keyword)))]
    (apply str
           (take len
                 (apply str (repeat repetitions keyword))))))

(defn encode [keyword message]
  (apply str (map encode-char
                  (normalise-keyword keyword (count message))
                  message)))

(defn decode [keyword message]
  "decodeme")

(defn decipher [cipher message]
  "decypherme")
