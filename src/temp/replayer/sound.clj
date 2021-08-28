
(import javax.sound.sampled.AudioSystem)

(def sound-clip (atom nil))
(declare replaying)

(defn play-sound! [sound]
  ;; (if (and @replaying
  ;;          (float= (:replay-speed @world) 1.0))
  ;;   (let [file (new File (str "res/sounds/" (dekeyword sound) ".wav"))
  ;;         clip (AudioSystem/getClip)
  ;;         audio-input-stream (AudioSystem/getAudioInputStream file)]
  ;;     (reset! sound-clip clip)
  ;;     (.open clip audio-input-stream)
  ;;     (.loop clip 0)))
  )

(defn stop-sound! []
  (if (not-nil? @sound-clip)
    (.stop @sound-clip)))

