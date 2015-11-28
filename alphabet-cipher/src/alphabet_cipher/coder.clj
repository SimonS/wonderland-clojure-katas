(ns alphabet-cipher.coder)

;;;;; This encodes and decodes correctly, but I don't feel it's as idiomatic /
;;;;; sensible as it should be. I feel that the decoding should be almost a
;;;;; direct reversal of the encoding using negative rotation. But my
;;;;; girlfriend is in labour, so a naïve lookup will do for now :D

(defn rotate [n l] (concat (drop n l) (take n l)))

(def alpha-start (int \a))
(def alpha-end (inc (int \z)))

(def substitution-chart
  (let [alphabet (apply str (map char (range alpha-start alpha-end)))]
  (for [row alphabet]
    (rotate
      (- (int row) alpha-start)
      alphabet))))

(defn encode-char [x y]
  (nth (nth substitution-chart (- (int x) alpha-start))
    (- (int y) alpha-start)))

(defn decode-char [key encoded]
  (char (let [index (.indexOf
                      (nth substitution-chart (- (int encoded) alpha-start))
                      key)]
          (if (= index 0) \a ; HACKS. -rotation would get rid of condition.
              (- alpha-end index)))))

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
  (apply str (map decode-char
                  (normalise-keyword keyword (count message))
                  message)))

(defn decipher [cipher message]
  "decypherme")
