{:parts {:block11906 {:children {:probe11908 {:position [0.3 0.55 0.0], :rotation [0.0 0.0 -1.0 90.00000250447816]}, :chip11902 {:position [0.0 -0.55 0.2850001], :rotation [1.0 0.0 0.0 90.00000250447816]}, :chip11903 {:position [0.0 -0.19999999 0.2850001], :rotation [1.0 0.0 0.0 90.00000250447816]}, :cpu11905 {:position [0.0 0.14999998 0.2850001], :rotation [1.0 0.0 0.0 90.00000250447816]}}, :color :white, :pins nil, :scale (0.5 1.6 0.5), :value 0, :functions nil, :type :block, :layer 1, :connections nil}, :probe11907 {:children nil, :color :purple, :pins nil, :scale [0.1 0.1 0.1], :value 1, :functions nil, :type :probe, :layer 1, :connections nil}, :block11899 {:children {:track11900 {:position [0.0 1.25 0.0], :rotation [0.0 1.0 0.0 0.0]}}, :color :white, :pins nil, :scale [0.5 0.5 0.5], :value 0, :functions nil, :type :block, :layer 1, :connections nil}, :button11904 {:children nil, :color :red, :pins nil, :scale [0.5 0.2 0.5], :value 0, :functions nil, :type :button, :layer 1, :connections nil}, :ground-part {:type :ground, :color :dark-gray, :scale [12 0.2 12], :children {:block11899 {:position [1.25 0.35 0.75], :rotation [0.0 1.0 0.0 0.0]}, :button11904 {:position [2.25 0.2 0.75], :rotation [0.0 1.0 0.0 0.0]}, :block11906 {:position [-0.35 0.90000004 0.75], :rotation [0.0 1.0 0.0 0.0]}}, :pins nil, :connections nil, :functions nil}, :block11901 {:children {:probe11907 {:position [0.74999994 -0.10000014 -5.9604645E-8], :rotation [1.0 -4.371139E-8 0.0 180.00000500895632]}}, :color [255 255 0], :pins nil, :scale (1.6 0.1 0.5), :value 0, :functions nil, :type :block, :layer 1, :connections nil}, :cpu11905 {:children nil, :color :blue, :pins {:button11904 {:x 80, :trigger true, :value 0}, :chip11902 {:x 420, :trigger false, :value 0}, :chip11903 {:x 300, :trigger false, :value 0}, :probe11907 {:x 170, :trigger false, :value 1}}, :scale [0.3 0.07 0.3], :gates {:gate-and11909 {:type :and, :x 170, :y 550, :tab 0}, :gate-not11913 {:type :not, :x 170, :y 510, :tab 1}, :gate-and11914 {:type :and, :x 170, :y 560, :tab 1}}, :value 0, :functions nil, :type :cpu, :layer 2, :connections {:connection11910 {:points [:probe11907 :gate-and11909], :tab 0}, :connection11911 {:points [:button11904 [80 550] :gate-and11909], :tab 0}, :connection11912 {:points [:gate-and11909 [300 550] :chip11903], :tab 0}, :connection11915 {:points [:button11904 [80 560] :gate-and11914], :tab 1}, :connection11916 {:points [:probe11907 :gate-not11913], :tab 1}, :connection11917 {:points [:gate-not11913 :gate-and11914], :tab 1}, :connection11918 {:points [:gate-and11914 [420 560] :chip11902], :tab 1}}, :tab 2}, :probe11908 {:children nil, :color :purple, :pins nil, :scale [0.1 0.1 0.1], :value 1, :functions nil, :type :probe, :layer 1, :connections nil}, :chip11903 {:children nil, :color :dark-gray, :pins nil, :scale [0.3 0.07 0.3], :value 0, :time 1.0, :functions {:track11900 {:points [[0 0.5] [1.0 1.0]], :relative false, :final-points [[0 0.5] [1.0 1.0]]}}, :type :chip, :layer 2, :connections nil, :view {:offset [0 0], :zoom 1}, :final-time 1.0}, :track11900 {:children {:block11901 {:position [0.54999995 0.049999952 0.0], :rotation [0.0 1.0 0.0 0.0]}}, :color :red, :pins nil, :scale [0.1 1 0.1], :value 0.5, :functions nil, :type :track, :layer 1, :connections nil}, :chip11902 {:children nil, :color :dark-gray, :pins nil, :scale [0.3 0.07 0.3], :value 0, :time 1.0, :functions {:track11900 {:points [[0 0] [1.0 0.5]], :relative false, :final-points [[0 0] [1.0 0.5]]}}, :type :chip, :layer 2, :connections nil, :view {:offset [0 0], :zoom 1}, :final-time 1.0}}, :camera {:vector [0 0 1], :distance 31.458287234608427, :x-angle 33.000000000000206, :y-angle -36.600000000000136, :pivot [0.47011568783269375 0.0 0.747194875789063], :eye [16.200398471340996 17.133411407470703 21.92804013640918]}, :visible-layers [2 1]}