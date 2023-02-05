<div>
    <h1>MockMechanics</h1>
    ![](https://mockmechanics.com/mechanics/home/banner.png)

    MockMechanics is a program that allows the creation of complex and interactive mechanisms using only simple building blocks and a graphical and intuitive form of programming. By focusing on simplicity it encourages experimentation and thinkering: you can develop your ideas as you build them instead of carefully planning ahead. The project is still evolving but already some pretty interesting things can be made, including playable musical instruments, prototypes for real machines, visual representations of algorithms, games and many other things. It is very flexible and easy to use so who knows what else people will come up with. The best way to get to know what it is and what it can do is to watch the first video on the companion YouTube channel.

    [![Introductory video](https://mockmechanics.com/mechanics/thumbnail-yt.png)](https://www.youtube.com/watch?v=HrwxbQj5mj0)

    <h1>Getting Started</h1>
    <p>Simply install leiningen, download the project, and on the root directory do```lein run``` (``` lein osx -- run```on macOs). Make sure you have the Java Runtime Environment version 11 or newer installed. You can get it <a style="color:#00283f;" href="https://adoptopenjdk.net/">here.</a>
        On Ubuntu you can use this instead on the command line:```sudo apt install openjdk-11-jre```
        <h1>Gallery</h1>
        ![](https://mockmechanics.com/mechanics/gallery/avatar0.png)
        <br>
        ![](https://mockmechanics.com/mechanics/gallery/avatar1.png)
        <br>
        ![](https://mockmechanics.com/mechanics/gallery/avatar2.png)
        <br>
        ![](https://mockmechanics.com/mechanics/gallery/seven-segment.png)
        <br>
        ![](https://mockmechanics.com/mechanics/gallery/gears.png)
        <br>
        ![](https://mockmechanics.com/mechanics/gallery/rack.png)
        <br>
        ![](https://mockmechanics.com/mechanics/gallery/tetris.png)
        <br>
        ![](https://mockmechanics.com/mechanics/gallery/clock.png)
        <br>
        ![](https://mockmechanics.com/mechanics/gallery/piano.png)
        <br>
        ![](https://mockmechanics.com/mechanics/gallery/safe.png)
        <br>
        ![](https://mockmechanics.com/mechanics/gallery/mixer.png)
        <br>
        ![](https://mockmechanics.com/mechanics/gallery/simon.png)
        <br>
        ![](https://mockmechanics.com/mechanics/gallery/printer.png)
        <br>
        ![](https://mockmechanics.com/mechanics/gallery/sort.png)
        <br>
        ![](https://mockmechanics.com/mechanics/gallery/row-tester.png)
        <br>
        ![](https://mockmechanics.com/mechanics/gallery/decider.png)
        <br>
        ![](https://mockmechanics.com/mechanics/gallery/copier.png)
        <br>
        ![](https://mockmechanics.com/mechanics/gallery/do.png)
        <br>
        ![](https://mockmechanics.com/mechanics/gallery/bridge.png)
        <br>
        ![](https://mockmechanics.com/mechanics/gallery/machine2machine.png)
        <br>
        ![](https://mockmechanics.com/mechanics/gallery/two-buttons.png)
        <br>
        ![](https://mockmechanics.com/mechanics/gallery/vertical.png)
        <br>
        ![](https://mockmechanics.com/mechanics/gallery/hammer.png)
        <br>
        ![](https://mockmechanics.com/mechanics/gallery/dalek.png)
        <br>
        ![](https://mockmechanics.com/mechanics/gallery/smooth.png)
        <br>
        ![](https://mockmechanics.com/mechanics/gallery/graph.png)
        <br>
        ![](https://mockmechanics.com/mechanics/gallery/speaker.png)

        <h1>Documentation</h1>
        <div id="docs_content">
            <div id="docs_sidebar">
                <a href="#concepts">Concepts</a>
                <a href="#interface">Interface</a>
                <a href="#library">Library machines</a>
                <a href="#add">Add mode</a>
                <a href="#edit">Edit mode</a>
                <a href="#graph">Graph mode</a>
                <a href="#motherboard">Motherboard mode</a>
                <a href="#color">Color mode</a>
                <a href="#property">Property mode</a>
                <a href="#toggle">Toggle mode</a>
                <a href="#layer">Layer mode</a>
                <a href="#simulation">Simulation mode</a>
                <a href="#physics">Physics mode</a>
            </div>
            <h2 id="concepts">Concepts</h2>
            <div class="news_item">
                <p>Mock Mechanics is a program that lets you create interactive machines in a very intuitive way, even programming these machines is mostly a question of connecting things visually in a logical way.</p>
                <p><u>Tree structure:</u> Machines are 3d tree structures of parts, so each part has a parent. You add a part to the ground then add parts to that part and so on to create the structure you want. When you move a part, all it's children move with it. </p>
                <p><u>Modes:</u> The program has different modes (selected on the right buttons bar), each view the same 3d environment but allows you to interact with it in a different way, add mode for instance allows adding objects, edit mode allows scaling and moving them and so on.</p>
                <p><u>Values:</u> Each part has a value associated with it, and what this value means depends on the part. The track value represents its rotation from 0 to 1, 1 being 360 degrees. The wagon value is the distance from its starting point on the track path it's on, the button (and block) value is 1 when pressed and 0 otherwise. The lamp value is 1 when on and 0 when off. You can run a chip by setting it's value to 1 and play the sound of a speaker in the same way. The probe has a value of 1 when it's touching another probe and 0 otherwise.</p>
                <p><u>Behaviour:</u> To make things move (or press buttons, light lamps and so on) you add it to a graph chip part and run the graph. The graph has time (in seconds) on the x axis, i.e the horizontal and the value in the y axis. A graph function that goes from (0,0) to (1,1) for a track for instance would when run rotate that track 360 degrees in 1 second.</p>
                <p><u>Decisions:</u> To connect the behaviour of one part with the behaviour of another (say running a graph when a button is pressed) you use a motherboard part to connect the values of those parts. You can use logical gates to make the values of the outputs any function of the value of the inputs. When a motherboard is called (when one of it's trigger inputs changes) it computes and sets the value of its outputs to zero or one. For instance suppose you connect a button pin directly to a lamp and set the button as a trigger (which it is by default). When you press the button, the lamp will go on and when you release it will go off. The button press and release runs the motherboard (since the button is a trigger) and since the lamp is connected to the button, it's value become the same as that button, 1 (button pressed, lamp on) or 0 (button released, lamp off). Or you could use a probe pin to check a condition in the world, for instance run that chip when the button is pressed and that block is close to that one.</p>
                <p><u>Settings:</u> You can set a few preferences by modifying the settings.clj file in the root directory. This file will grow as I publish new versions of the program to allow fine tuning of the program by the users.</p>
            </div>
            <h2 id="interface">Interface</h2>
            <div class="news_item">
                <p>You can rotate the view by clicking and dragging the right mouse button. You can also set the pivot of rotation to a part or a point in the ground by right clicking it. You can pan with the middle mouse button. You can zoom in and out with the mouse wheel. The left mouse button click behaviour is specific for each mode, on simulation mode you can apply a force to a part (which is free or has a parent that is free) to move it, on color mode you can apply the current color to the clicked part and so on.</p>
                <p><u>Left buttons bar</u>: new file, undo and redo are self explanatory.</p>
                <p><u>Right buttons bar</u>: Select mode.</p>
                <p><u>Save:</u> The first time it will prompt for a root name, you could type say clock and press enter and it will save the file machines/clock_000.mch. After that, saving will not ask for a name and only save a new version clock_001, clock_002 etc.</p>
                <p><u>Open:</u> will prompt for the root name and load the latest version.</p>
                <p><u>Reset Camera:</u> Place the camera back at the original position is was on startup.</p>
                <p><u>Cancel action:</u> almost self explanatory, useful when you start an action like opening a file but give up, also to erase the shortcuts buffer. Mock mechanics accepts compound shortcuts like emacs, for instance to open a file just press "Ctrl+o", to import a file (for instance load a submachine to the machine you are working on) press "Ctrl+x" followed by "o".</p>
            </div>
            <h2 id="library">Library Machines</h2>
            <div class="news_item">
                <p>The three machines: copier, proximity-test and lever are the first "library" machines (you could have build them yourself but I already built them for you). You can import them with "Ctrl-x o" to use as submachines of your own machines.</p>
                <p>The lever is the simplest, it can be pulled on or off, there is a hidden probe on layer 2 that lets you check which state it's in.</p>
                <p>The proximity-test is for checking if there is a part in a given direction. Just place (transfer) the probe anywhere and press the button. If there is an object in the probe's line of sight (anywhere "on top" of it, the bottom of an object is always sitting on a surface, place a probe on a wall and its top will be facing in the horizontal, try to place a cone on the ground and then on a wall and you'll see what I mean) then the light will be turned on, if not it will be turned off and you can use the light value itself as an input to let a motherboard know if there was a part there or not. I used it for instance on the row tester at the bottom of the tetris game to check each square to see if there was a block there. You can also open the script proximity-test.clj to set the max distance from the probe that counts.</p>
                <p>The copier uses the same idea of probes as pointers, point the green probe at anything and point the red probe to where you want to copy it and press the button. The yellow toggle is to decide whether to copy something to the exact place you point or copy in place, but to the parent you point.</p>
                <p>Play with the machine library-test to understand all these machines.</p>
                <p>* As a side note, the safe machine is broken at the moment due to a problem with the physics engine, so it doesn't actually stop at the wall and locking has no effect, I'm working on it. For the same reason, the tetris script (tetris-loop.clj) has an extra function that won't be necessary when I fix the physics engine issue.</p>
            </div>
            <h2 id="add">Add mode</h2>
            <div class="news_item">
                <p>Select an object from the bottom bar then click on some surface (including the ground) to add the object there, while the mouse is pressed you can move it (just like move mode on edit mode).</p>
                <p>You can only add to a track at the end, just move the mouse close to the end and click on the appropriate face of the helper cube, clicking anywhere else just adds to the "top" face, which is what you want most of the time when the object you're adding to the track is not another track.</p>
                <p>Wagons can only be added to tracks and you can click anywhere on the track. The wagon will move on the circuit created by its parent track and any adjacent tracks (to experiment create an L shape with two tracks, add a wagon to one of them and then on property mode drag the wagon around and note the value, which is the distance from the endpoint of the track path).</p>
            </div>
            <h2 id="edit">Edit mode</h2>
            <div class="news_item">
                <p><u>Move:</u> move objects on the surface it's on (the surface of its parent) (the info at the bottom of the screen gives the relative coordinates, (0,0) is the center of the surface). You can also press shift to change the snap distance.</p>
                <p><u>Change height:</u> change vertical distance from the surface, ideally you want a child sitting on the surface of it's parent but sometimes it's necessary to tweak the alignment of things.</p>
                <p><u>Rotate:</u> click and drag away from object to rotate one way, back towards object to rotate another.</p>
                <p><u>Scale:</u> scale works differently for different objects, some can't be scalled at all, for others you click and drag a surface to make the object grow in that direction (like the block, cylinder). For the sphere and track click anywhere and drag (can only scale in one way). For the cone click near the base to increase diameter, near the top to increase height.</p>
                <p><u>Copy:</u> Ctrl click on object to make a selection (it will flash a different color) then normal click to add a copy of it (and all its children) somewhere else.</p>
                <p><u>Transfer:</u> move object to new parent/new surface on the same parent. Same deal as copy, ctrl click to select, normal click to place.</p>
                <p><u>Delete:</u> self explanatory (all children deleted too), and if there are any references to any of the deleted objects (inside a graph or motherboard), they disappear as well.</p>
            </div>
            <h2 id="graph">Graph mode</h2>
            <div class="news_item">
                <p>If you click on a graph chip you open it for editing, if you click anything else, you add/remove it from the current graph. You can click the button on the bottom right to show/hide the possible commands (use use the shortcuts in red). You can pan the graph by dragging with the middle mouse button. You can zoom in and out with the mouse wheel. Zooming with shift pressed scales only the y direction so you can control the aspect ratio of the graph.
                <p><u>Move submode:</u> move nodes around (shift to change snapping).</p>
                <p><u>Set x:</u> white band shows (it's expecting you to click on a node) then enter the precise x value.</p>
                <p><u>Set y:</u> ditto for y.</p>
                <p><u>Add:</u> click at any point of a function line segment to add a node there.</p>
                <p><u>Delete:</u> click on node to delete it.</p>
                <p><u>Run:</u> run the graph as a test.</p>
                <p><u>Toggle:</u> click on function to toggle between relative and absolute. An absolute function (with circles for nodes) will set the value of the part directly, for instance if it has a node at (1,1) then at 1 second the part it's controlling will be set to the value 1. A relative function (with square nodes will take the part starting value in consideration. For instance if a wagon has value 0 and you have a relative function which is a single segment from (0,2) to (0.25,2), then when you run the graph the wagon will move from 0 to 0.5 (increase at a rate of 2 for 0.25 of a second), if run again it will move from 0.5 to 1 and so on. You can also think of these two functions as a position function, giving the position directly and a velocity function, setting the rate at which the position is changing.</p>
                <p><u>View:</u> reset view of graph.</p>
                <p><u>Lengths:</u> when creating graphs for wagons, its often useful to know what the lengths of each leg of its circuit (and the total) are, click on wagon function to find out.</p>
            </div>
            <h2 id="motherboard">Motherboard mode</h2>
            <div class="news_item">
                <p>Similar to graph mode, click on motherboards to view them or click on other parts to add or remove them as pins (outputs and inputs) to the motherboard.
                <p><u>Move submode:</u> move nodes and pins around</p>
                <p><u>And, or, not:</u> add the corresponding gate to the motherboard</p>
                <p><u>Delete:</u> delete nodes or connections</p>
                <p><u>Connect:</u> connect two elements (pins or nodes) in a input to output direction, can also click on the background to create turns to make the wiring more organized visually. Click on a pin or node, then click on zero or more points in the background and finally click on another pin or node.</p>
                <p><u>Trigger:</u> click on a pin to toggle it as a trigger or not (blue circle indicates a trigger pin). The motherboard is called when one of it's trigger pins changes value.</p>
                <p><u>Run:</u> click on a chip pin to run it, useful when you have multiple chips as pins and want to find out what one does.</p>
                <p><u>Script:</u> toggle script or connections, if using a script the motherboard will actually run the code in the machines folder with the name. For example if the name "test" is given, you have to create a file machines/test.clj and name all the pins in a vector so you can use them and the last thing should be a fn that accepts a part name, which will be the part triggered. You can also have helper functions before the fn. Scripts are often unecessary unless creating library parts, the circuits using logical gates are often enough, but if you do need to use one, look at the machine scripted-decider for an example.</p>
            </div>
            <h2 id="color">Color mode</h2>
            <div class="news_item">
                <p>Select color, click on part to change it (some part's colors can't be changed like the graph part). The colors have shortcuts too, at least the basic ones (r for red, y for yellow etc).</p>
            </div>
            <h2 id="property">Property mode</h2>
            <div class="news_item">
                <p>Set the value of the part (the angle of the track, from 0 to 1, the distance of the wagon from 0 to the maximum distance on the path, etc) and any other properties. Click on the part to view its properties then click on the property to enter a value.</p>
                <p>frequency: Set the speaker's frequency in hertz to alter the pitch.</p>
                <p>max-angle: if not nil and the the track is free, it will only be able to moved in the range 0, max-angle.</p>
                <p>snap: if not nil (or 0), the track or wagon will snap to multiples of this value, for example if snap is set to 0.25, when released by the mouse a track will go to the angles 0, 90, 180 or 270, whichever is closest.</p>
            </div>
            <h2 id="toggle">Toggle mode</h2>
            <div class="news_item">
                <p>It's similar to property mode but for binary properties, you select which property you want to see, then click an object to toggle its value to true or false (red is true, white is false). For instance if you set the "free" property of a track or wagon, on simulation mode you'll be able to move it by dragging it or any of its children (rotate the track or move the wagon on its path), the lever machine has a free track (it will also snap to the snap value if you set it on property mode). The "solid" property allows a block to interact physically with other blocks and physical objects like the balls and the avatar.</p>
            </div>
            <h2 id="layer">Layer mode</h2>
            <div class="news_item">
                <p>Click on a layer square to view objects in it, shift click to add/remove the layer from current visible ones, drag objects to the desired layer (shift drag to move it's children there too).</p>
            </div>
            <h2 id="simulation">Simulation mode</h2>
            <div class="news_item">
                <p>Where the magic happens, buttons will be pressable, motherboards will respond to changes, physical objects (spheres) will move and you can drag any free wagon/track with the mouse.</p>
            </div>
            <h2 id="physics">Physics mode (experimental and hidden), "Alt-x p"</h2>
            <div class="news_item">
                <p>Add/remove spheres to interact with parts of your machine. You have to make blocks "solid" on toggle mode for them to interact with the balls. Click on a surface to add sphere there, click on a sphere to remove it.</p>
            </div>
        </div>
        <h1>Contact</h1>
        <div id="text_content">
            <p>Have any queries, bugs, features or anything else you want to tell me? You can contact me at <span style="color:#00283f;">mock.mechanics@gmail.com</span>. You can also follow MockMechanics on all the usual places:
                <a href="https://www.youtube.com/c/MockMechanics">Youtube</a>
                <a href="https://twitter.com/mockmechanics">Twitter</a>
                <a href="https://www.instagram.com/mockmechanics/">Instagram</a>
                If you like this project and want it to continue, you can also support it on Patreon:
                <a href="https://www.patreon.com/mockmechanics">Patreon</a></p>
        </div>
</div>
