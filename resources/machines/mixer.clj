{:parts {:probe11907 {:children nil, :saved-value 0, :color :purple, :pins nil, :scale [0.1 0.1 0.1], :value 0, :functions nil, :type :probe, :layer 1, :connections nil}, :block11899 {:children {:track11900 {:position [0.0 1.25 0.0], :rotation [0.0 1.0 0.0 0.0]}}, :saved-value 0, :color :white, :pins nil, :scale [0.5 0.5 0.5], :value 0, :functions nil, :type :block, :layer 1, :connections nil}, :button11904 {:children nil, :saved-value 0, :color :red, :pins nil, :scale [0.5 0.2 0.5], :value 0, :functions nil, :type :button, :layer 1, :connections nil}, :ground-part {:children {:block11899 {:position [1.25 0.35 0.75], :rotation [0.0 1.0 0.0 0.0]}, :button11904 {:position [3.75 0.2 1.75], :rotation [0.0 1.0 0.0 0.0]}}, :saved-value nil, :color :dark-gray, :pins nil, :scale [12 0.2 12], :functions nil, :type :ground, :connections nil}, :block11901 {:children {:probe11907 {:position [0.74999994 -0.10000014 -5.9604645E-8], :rotation [1.0 -4.371139E-8 0.0 180.00000500895632]}, :block13509 {:position [0.44999993 -0.8000001 5.9604645E-8], :rotation [1.0 3.821371E-15 8.742278E-8 180.00000500895632]}}, :saved-value 0, :color [255 255 0], :pins nil, :scale (1.6 0.1 0.5), :value 0, :functions nil, :type :block, :layer 1, :connections nil}, :cpu11905 {:children nil, :saved-value 0, :color :blue, :pins {:button11904 {:x 90, :trigger true, :value 0}, :chip11902 {:x 190, :trigger false, :value 0}}, :scale [0.3 0.07 0.3], :gates {:gate-and18130 {:type :and, :x 160, :y 100, :tab 0}}, :value 0, :functions nil, :type :cpu, :layer 2, :connections {:connection18131 {:points [:button11904 [90 100] :gate-and18130], :tab 0}, :connection13511 {:points [:button11904 :chip11902], :tab 1}}, :tab 1}, :block13509 {:children {:cpu11905 {:position [0.0 -0.4500001 -0.28500003], :rotation [-1.0 0.0 0.0 90.00000250447816]}, :chip11902 {:position [0.0 -0.05000013 -0.28500003], :rotation [-1.0 0.0 0.0 90.00000250447816]}}, :saved-value 0, :color :white, :pins nil, :scale (0.5 1.5 0.5), :value 0, :functions nil, :type :block, :physics true, :layer 1, :connections nil}, :track11900 {:children {:block11901 {:position [0.54999995 0.049999952 0.0], :rotation [0.0 1.0 0.0 0.0]}}, :saved-value 1.0, :color :red, :pins nil, :scale [0.1 1 0.1], :value 1.0, :functions nil, :type :track, :layer 1, :connections nil}, :chip11902 {:children nil, :saved-value 0, :color :dark-gray, :pins nil, :scale [0.3 0.07 0.3], :value 0, :time 1.0, :functions {:track11900 {:points [[0 0] [1.0 1.0]], :relative false, :final-points [[0 0] [1.0 1.0]]}}, :type :chip, :layer 2, :connections nil, :view {:offset [0 0], :zoom-x 1, :zoom-y 1}, :final-time 1.0}}, :camera {:vector [0 0 1], :distance 43.49611793198034, :x-angle 35.000000000000085, :y-angle -16.99999999999978, :pivot [2.3078506397905976 0.25 2.1151055342605964], :eye [12.725034515828195 25.198348999023438 36.188179875080905]}, :visible-layers [2 1]}