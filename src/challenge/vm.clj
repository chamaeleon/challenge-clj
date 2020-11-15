(ns challenge.vm
  (:require [clojure.java.io :refer [input-stream]])
  (:require [gloss.core :refer [compile-frame]])
  (:require [gloss.io :refer [decode-all]])
  (:gen-class))

(def bin-frame (compile-frame :uint16-le))

(def pc (atom 0))
(def memory (make-array Integer/TYPE 65536))
(def running (atom true))
(def input (atom nil))
(def stack (atom ()))

(def instructions
  { 0 :halt,  1 :set,   2 :push,  3 :pop,  4 :eq,   5 :gt,  6 :jmp,  7 :jt,
    8 :jf,    9 :add,  10 :mult, 11 :mod, 12 :and, 13 :or, 14 :not, 15 :rmem,
   16 :wmem, 17 :call, 18 :ret,  19 :out, 20 :in,  21 :noop})

(defn get-char []
  (if-let [[ch & rest] @input]
    (do (reset! input rest)
        ch)
    (do (reset! input (str (read-line) (char 10)))
        (recur))))

(defn raw-read [addr]
  (aget ^ints memory addr))

(defn mem-read [addr]
  (let [value (raw-read addr)]
    (if (>= value 32768)
      (raw-read (raw-read addr))
      value)))

(defn mem-write [addr val]
  (aset ^ints memory addr (int val)))

(defn math-op
  ([op addr]
   (mod (op (mem-read addr)) 32768))
  
  ([op addr1 addr2]
   (mod (op (mem-read addr1) (mem-read addr2)) 32768)))

(defmulti execute instructions)

(defmethod execute :halt [_]
  (reset! running false))

(defmethod execute :set [_]
  (mem-write (raw-read (inc @pc)) (mem-read (+ @pc 2)))
  (swap! pc + 3))

(defmethod execute :push [_]
  (swap! stack conj (mem-read (inc @pc)))
  (swap! pc + 2))

(defmethod execute :pop [_]
  (if (> (count @stack) 0)
    (do
      (mem-write (raw-read (inc @pc)) (first @stack))
      (swap! stack rest)
      (swap! pc + 2))
    (do
      (println "pop instruction with empty stack!")
      (reset! running false))))

(defmethod execute :eq [_]
  (mem-write (raw-read (inc @pc))
             (if (= (mem-read (+ @pc 2))
                    (mem-read (+ @pc 3)))
               1 0))
  (swap! pc + 4))

(defmethod execute :gt [_]
  (mem-write (raw-read (inc @pc))
             (if (> (mem-read (+ @pc 2))
                    (mem-read (+ @pc 3)))
               1 0))
  (swap! pc + 4))

(defmethod execute :jmp [_]
  (reset! pc (mem-read (inc @pc))))

(defmethod execute :jt [_]
  (if (not= (mem-read (inc @pc)) 0)
    (reset! pc (mem-read (+ @pc 2)))
    (swap! pc + 3)))

(defmethod execute :jf [_]
  (if (= (mem-read (inc @pc)) 0)
    (reset! pc (mem-read (+ @pc 2)))
    (swap! pc + 3)))

(defmethod execute :add [_]
  (mem-write (raw-read (inc @pc)) (math-op + (+ @pc 2) (+ @pc 3)))
  (swap! pc + 4))

(defmethod execute :mult [_]
  (mem-write (raw-read (inc @pc)) (math-op * (+ @pc 2) (+ @pc 3)))
  (swap! pc + 4))

(defmethod execute :mod [_]
  (mem-write (raw-read (inc @pc)) (math-op mod (+ @pc 2) (+ @pc 3)))
  (swap! pc + 4))

(defmethod execute :and [_]
  (mem-write (raw-read (inc @pc)) (math-op bit-and (+ @pc 2) (+ @pc 3)))
  (swap! pc + 4))

(defmethod execute :or [_]
  (mem-write (raw-read (inc @pc)) (math-op bit-or (+ @pc 2) (+ @pc 3)))
  (swap! pc + 4))

(defmethod execute :not [_]
  (mem-write (raw-read (inc @pc)) (math-op bit-not (+ @pc 2)))
  (swap! pc + 3))

(defmethod execute :rmem [_]
  (mem-write (raw-read (inc @pc)) (raw-read (mem-read (+ @pc 2))))
  (swap! pc + 3))

(defmethod execute :wmem [_]
  (mem-write (mem-read (inc @pc)) (mem-read (+ @pc 2)))
  (swap! pc + 3))

(defmethod execute :call [_]
  (swap! stack conj (+ @pc 2))
  (reset! pc (mem-read (inc @pc))))

(defmethod execute :ret [_]
  (if (> (count @stack) 0)
    (do
      (reset! pc (first @stack))
      (swap! stack rest))
    (do
      (println "ret instruction with empty stack!")
      (reset! running false))))

(defmethod execute :out [_]
  (print (char (mem-read (inc @pc))))
  (flush)
  (swap! pc + 2))

(defmethod execute :in [_]
  (mem-write (raw-read (inc @pc)) (int (get-char)))
  (swap! pc + 2))

(defmethod execute :noop [_]
  (swap! pc inc))

(defmethod execute :default [_]
  (println (format "Unimplemented instruction at address %d" @pc))
  (reset! running false))

(defn load-memory [filename]
  (let [handle (input-stream filename)]
    (if-let [content (decode-all bin-frame handle)]
      (do
        (doseq [[idx val] (map-indexed (fn [a b] [a b]) content)]
          (mem-write idx val))
        true)
      false)))

(defn run [filename]
  (if (load-memory filename)
    (while  @running
      (execute (mem-read @pc)))
    (println "Failed to load" filename)))