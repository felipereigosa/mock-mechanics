
[block]

(fn [part-name]
  (if (on? part-name)
    (jump {:address "level2",
           :block :ground,
           :relative-position [-2.2957842 0.1 2.3279579],
           :relative-direction [0.87433845 0.0 -0.4853167],
           :cameraman-position [-28.525906646925872 0.2537400744592097 16.887440629811255],
           :x-angle 26.104880924449375,
           :y-angle 60.96677821710444,
           :pivot [-2.2957842350006104 0.3 2.32795786857605]})
    ))
