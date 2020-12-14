
[rocker probe move-chip reset-chip swap]

(fn [part-name]
  (while (on? rocker)
    (activate move-chip)
    (activate test-chip)
    (if (on? greater-than-lamp)
      (activate swap))
    (if (on? end-probe)
      (activate reset-chip))))

