{:parts {:block11906 {:type :block, :color :white, :value 0, :layer 1, :scale (0.5 1.6 0.5), :children {:probe11908 {:position [0.3 0.55 0.0], :rotation [0.0 0.0 -1.0 90.00000250447816]}, :chip11902 {:position [0.0 -0.55 0.2850001], :rotation [1.0 0.0 0.0 90.00000250447816]}, :chip11903 {:position [0.0 -0.19999999 0.2850001], :rotation [1.0 0.0 0.0 90.00000250447816]}, :cpu11905 {:position [0.0 0.14999998 0.2850001], :rotation [1.0 0.0 0.0 90.00000250447816]}, :lamp11936 {:position [0.0 0.90000004 0.0], :rotation [0.0 1.0 0.0 0.0]}, :cpu11938 {:position [0.0 0.49999994 -0.285], :rotation [-1.0 0.0 0.0 90.00000250447816]}}}, :lamp11936 {:type :lamp, :color [0 255 0], :value 1, :layer 1, :scale [0.4 0.2 0.4], :dark-color [0 76 0], :children nil}, :probe11907 {:type :probe, :color :purple, :value 1, :layer 1, :scale [0.1 0.1 0.1], :children nil}, :block11899 {:type :block, :color :white, :value 0, :layer 1, :scale [0.5 0.5 0.5], :children {:track11900 {:position [0.0 1.25 0.0], :rotation [0.0 1.0 0.0 0.0]}}}, :button11904 {:type :button, :color :red, :value 0, :layer 1, :scale [0.5 0.2 0.5], :children nil}, :ground-part {:type :ground, :color :dark-gray, :scale [12 0.2 12], :children {:block11899 {:position [1.25 0.35 0.75], :rotation [0.0 1.0 0.0 0.0]}, :button11904 {:position [2.25 0.2 0.75], :rotation [0.0 1.0 0.0 0.0]}, :block11906 {:position [-0.35 0.90000004 0.75], :rotation [0.0 1.0 0.0 0.0]}}}, :cpu11938 {:children nil, :color :blue, :pins {:lamp11936 {:x 250, :trigger false, :value 1}, :probe11907 {:x 140, :trigger true, :value 1}}, :scale [0.3 0.07 0.3], :value 0, :type :cpu, :layer 2, :connections {:connection11940 {:points [:probe11907 :lamp11936], :tab 0}}, :tab 0}, :block11901 {:type :block, :color [255 255 0], :value 0, :layer 1, :scale (1.6 0.1 0.5), :children {:probe11907 {:position [0.74999994 -0.10000014 -5.9604645E-8], :rotation [1.0 -4.371139E-8 0.0 180.00000500895632]}}}, :cpu11905 {:children nil, :color :blue, :pins {:button11904 {:x 80, :trigger true, :value 0}, :chip11902 {:x 420, :trigger false, :value 0}, :chip11903 {:x 300, :trigger false, :value 0}, :probe11907 {:x 170, :trigger false, :value 1}}, :scale [0.3 0.07 0.3], :gates {:gate-and11909 {:type :and, :x 170, :y 550, :tab 0}, :gate-not11913 {:type :not, :x 170, :y 510, :tab 1}, :gate-and11914 {:type :and, :x 170, :y 560, :tab 1}}, :value 0, :type :cpu, :layer 2, :connections {:connection11910 {:points [:probe11907 :gate-and11909], :tab 0}, :connection11911 {:points [:button11904 [80 550] :gate-and11909], :tab 0}, :connection11912 {:points [:gate-and11909 [300 550] :chip11903], :tab 0}, :connection11915 {:points [:button11904 [80 560] :gate-and11914], :tab 1}, :connection11916 {:points [:probe11907 :gate-not11913], :tab 1}, :connection11917 {:points [:gate-not11913 :gate-and11914], :tab 1}, :connection11918 {:points [:gate-and11914 [420 560] :chip11902], :tab 1}}, :tab 2}, :probe11908 {:type :probe, :color :purple, :value 1, :layer 1, :scale [0.1 0.1 0.1], :children nil}, :chip11903 {:children nil, :color :dark-gray, :scale [0.3 0.07 0.3], :value 0, :time 1.0, :functions {:track11900 {:points [[0 0.5] [1.0 1.0]], :relative false, :final-points [[0 0.5] [1.0 1.0]]}}, :type :chip, :layer 2, :view {:offset [0 0], :zoom 1}, :final-time 1.0}, :track11900 {:type :track, :color :red, :value 0.5, :layer 1, :scale [0.1 1 0.1], :children {:block11901 {:position [0.54999995 0.049999952 0.0], :rotation [0.0 1.0 0.0 0.0]}}}, :chip11902 {:children nil, :color :dark-gray, :scale [0.3 0.07 0.3], :value 0, :time 1.0, :functions {:track11900 {:points [[0 0] [1.0 0.5]], :relative false, :final-points [[0 0] [1.0 0.5]]}}, :type :chip, :layer 2, :view {:offset [0 0], :zoom 1}, :final-time 1.0}}, :camera {:vector [0 0 1], :distance 31.458287234608427, :x-angle 33.800000000000075, :y-angle -27.00000000000001, :pivot [0.14716345293844135 0.0 0.5463383355532763], :eye [12.015087189510707 17.500106811523438 23.838450686017143]}}