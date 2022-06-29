
(import '(javax.sound.midi MidiSystem Synthesizer))

(def synth (doto (MidiSystem/getSynthesizer) .open))

(defn note-on [note]
  (let [channel (aget (.getChannels synth) 0)]
    (.noteOn channel note 127)))

(defn note-off [note]
  (let [channel (aget (.getChannels synth) 0)]
    (.noteOff channel note 127)))

(defn get-frequency [note]
  (* 8.175805469120409 (Math/pow 1.059463 note)))

(defn get-note [frequency]
  (int (Math/round (/ (Math/log (/ frequency 8.1758))
                      (Math/log 1.059463)))))
