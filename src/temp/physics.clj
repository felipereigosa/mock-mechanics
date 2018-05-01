(ns temp.core)

(import [com.bulletphysics.collision.dispatch DefaultCollisionConfiguration
         CollisionDispatcher])
(import com.bulletphysics.collision.broadphase.AxisSweep3)
(import [com.bulletphysics.collision.shapes CollisionShape
         StaticPlaneShape BoxShape
         SphereShape CylinderShape])
(import [com.bulletphysics.linearmath Transform DefaultMotionState])

(import [com.bulletphysics.dynamics
         constraintsolver.SequentialImpulseConstraintSolver
         DiscreteDynamicsWorld RigidBodyConstructionInfo RigidBody])
(import com.bulletphysics.dynamics.constraintsolver.Point2PointConstraint)
(import com.bulletphysics.dynamics.constraintsolver.HingeConstraint)
(import com.bulletphysics.dynamics.constraintsolver.SliderConstraint)
(import com.bulletphysics.dynamics.constraintsolver.Generic6DofConstraint)

(def planet (atom nil))

(defn create-planet! []
  (let [collision-configuration (new DefaultCollisionConfiguration)
        dispatcher (new CollisionDispatcher collision-configuration)
        gravity (new Vector3f 0 -1 0)
        world-aabb-min (new Vector3f -10000 -10000 -10000)
        world-aabb-max (new Vector3f 10000 10000 10000)
        overlapping-pair-cache (new AxisSweep3 world-aabb-min world-aabb-max)
        solver (new SequentialImpulseConstraintSolver)
        my-planet (new DiscreteDynamicsWorld dispatcher overlapping-pair-cache
                       solver collision-configuration)]
    (.setGravity my-planet gravity)
    (set! (.-allowedCcdPenetration (.getDispatchInfo my-planet)) 0)
    (reset! planet my-planet)))
  
(defn create-static-plane [[nx ny nz] d]
  (let [shape (new StaticPlaneShape (new Vector3f nx ny nz) d)
        transform (let [t (new Transform)]
                           (.setIdentity t)
                           t)
        motion-state (new DefaultMotionState transform)
        construction-info (new RigidBodyConstructionInfo 0 motion-state
                                      shape (new Vector3f 0 0 0))
        rigid-body (new RigidBody construction-info)]
    (.addRigidBody @planet rigid-body 1 0xffffffff)))

(defn create-ground! []
  (create-static-plane [0 1 0] 0)
  (create-static-plane [0 0 1] -30)
  (create-static-plane [0 0 -1] -30)
  (create-static-plane [1 0 0] -30)
  (create-static-plane [-1 0 0] -30))

(defn create-body [shape mass transform group collides]
  (let [motion-state (new DefaultMotionState transform)
        local-inertia (let [vec (new Vector3f)]
                        (.calculateLocalInertia shape mass vec)
                        vec)
        construction-info (new RigidBodyConstructionInfo mass motion-state
                               shape local-inertia)
        rigid-body (new RigidBody construction-info)]
    (.forceActivationState rigid-body RigidBody/DISABLE_DEACTIVATION)
    (.addRigidBody @planet rigid-body group collides)
    rigid-body))

(declare make-transform)

(defn create-cube-body
  ([[w h d] mass transform group collides]
   (let [shape (new BoxShape (new Vector3f (/ w 2) (/ h 2) (/ d 2)))]
     (create-body shape mass transform group collides)))
  ([scale position rotation]
   (create-cube-body scale 1 (make-transform position rotation) 1 1)))

;; (defn create-cube-object [mass position rotation scale skin group collides]
;;   {:mesh (create-cube-mesh [0 0 0] [0 1 0 0] scale skin)
;;    :body (create-cube-body scale mass (make-transform position rotation) ;;#######
;;                           group collides)})

;; (defn create-sphere-body [r mass transform group collides]
;;   (create-body (new SphereShape r) mass transform group collides))

;; (defn create-sphere-object [mass position rotation scale skin group collides]
;;   {:mesh (create-sphere-mesh [0 0 0] [0 0 1 0] scale skin)
;;    :body (create-sphere-body (first scale) mass (make-transform position rotation)
;;                              group collides)})

;; (defn create-cylinder-body [r h mass transform group collides]
;;   (create-body (new CylinderShape (new Vector3f r (/ h 2) r)) mass transform group collides))

;; (defn create-cylinder-object [mass position rotation scale skin group collides]
;;   {:mesh (create-cylinder-mesh [0 0 0] [0 0 1 0] scale skin)
;;    :body (create-cylinder-body (first scale) (second scale)
;;                              mass (make-transform position rotation)
;;                              group collides)})

(defn get-body-transform [body]
  (let [transform (new Transform)]
    (.getWorldTransform (.getMotionState body) transform)))

(defn step-simulation! [elapsed]
  (.stepSimulation @planet elapsed 7 (/ 1 60.0)))

;; (defn apply-torque [object torque]
;;   (let [body (:body object)]
;;     (.applyTorque body (make-vector3f torque))
;;     object))

(defn apply-force [body force point]
  (let [transform (get-body-transform body)
        body-position (get-transform-position transform)
        point (vector-subtract point body-position)]
    (.applyForce body (make-vector3f force) (make-vector3f point))
    body))

;; (defn get-angular-velocity [object]
;;   (let [body (:body object)
;;         v (new Vector3f)]
;;     (.getAngularVelocity body v)
;;     [(.-x v) (.-y v) (.-z v)]))

;; (defn apply-angular-friction [object strenght]
;;   (let [angular-velocity (get-angular-velocity object)
;;         torque (vector-multiply angular-velocity (* strenght -1))]
;;     (apply-torque object torque)))

;; (defn get-linear-velocity [object]
;;   (let [body (:body object)
;;         v (new Vector3f)]
;;     (.getLinearVelocity body v)
;;     [(.-x v) (.-y v) (.-z v)]))

;; (defn get-object-position [object]
;;   (let [body (:body object)
;;         transform (get-body-transform body)]
;;     (get-transform-position transform)))

;; (defn apply-linear-friction [object strenght]
;;   (let [linear-velocity (get-linear-velocity object)
;;         force (vector-multiply linear-velocity (* strenght -1))]
;;     (apply-force object force (get-object-position object))))

;; (defn create-hinge-constraint [world object-a-name object-b-name
;;                                pivot-a axis-a
;;                                pivot-b axis-b]
;;   (let [body-a (get-in world [:objects object-a-name :body])
;;         body-b (get-in world [:objects object-b-name :body])
;;         pivot-a (make-vector3f pivot-a)
;;         axis-a (make-vector3f axis-a)
;;         pivot-b (make-vector3f pivot-b)
;;         axis-b (make-vector3f axis-b)
;;         constraint (new HingeConstraint body-a body-b pivot-a pivot-b axis-a axis-b)]
;;     (.addConstraint @planet constraint)
;;     constraint))

;; (defn create-weld-constraint [world object-a-name object-b-name
;;                                pivot-a axis-a
;;                                pivot-b axis-b]
;;   (let [body-a (get-in world [:objects object-a-name :body])
;;         body-b (get-in world [:objects object-b-name :body])
;;         frame-a (make-transform pivot-a (conj axis-a 0))
;;         frame-b (make-transform pivot-b (conj axis-b 0))
;;         constraint (new Generic6DofConstraint body-a body-b frame-a frame-b true)]
;;     (.setLimit constraint 0 0 0)
;;     (.setLimit constraint 1 0 0)
;;     (.setLimit constraint 2 0 0)
;;     (.setLimit constraint 3 0 0)
;;     (.setLimit constraint 4 0 0)
;;     (.setLimit constraint 5 0 0)
;;     (.addConstraint @planet constraint)
;;     constraint))

;; (defn create-slider-constraint [world object-a-name object-b-name
;;                                 pivot-a axis-a
;;                                 pivot-b axis-b]
;;   (let [body-a (get-in world [:objects object-a-name :body])
;;         body-b (get-in world [:objects object-b-name :body])
;;         frame-a (make-transform pivot-a (conj axis-a 0))
;;         frame-b (make-transform pivot-b (conj axis-b 0))
;;         constraint (new SliderConstraint body-a body-b frame-a frame-b true)]
;;     (.addConstraint @planet constraint)
;;     constraint))

;; ;;-----------------------------------------------------------------------------------;;
;; ;; mouse force

;; (defn local-to-world-coordinates [transform [x y z]]
;;   (let [vector (into-array Float/TYPE [x y z 1.0])
;;         matrix (get-transform-matrix transform)
;;         [x y z _] (into [] (multiply-matrix-vector matrix vector))]
;;     [x y z]))

;; (defn world-to-local-coordinates [transform [x y z]]
;;   (let [vector (into-array Float/TYPE [x y z 1.0])
;;         matrix (get-inverse-matrix (get-transform-matrix transform))
;;         [x y z _] (into [] (multiply-matrix-vector matrix vector))]
;;     [x y z]))

;; (defn get-force-start-point [world]
;;   (let [local-point (get-in world [:force :local-start])
;;         object-name (get-in world [:force :object-name])
;;         body (get-in world [:objects object-name :body])
;;         transform (get-body-transform body)]
;;     (local-to-world-coordinates transform local-point)))

;; (defn get-force-end-point [world]
;;   (let [force (:force world)
;;         screen-point (:screen-end force)
;;         line (unproject-point world screen-point)]
;;     (line-plane-intersection line (:plane force))))

;; (defn get-force-plane [world]
;;   (let [start-point (get-force-start-point world)
;;         eye (get-in world [:camera :eye])
;;         pivot (get-in world [:camera :pivot])
;;         n (vector-normalize (vector-subtract eye pivot))
;;         cx (vector-cross-product n [1 0 0]) ;;###################### may not work
;;         px (vector-add start-point cx)
;;         cy (vector-cross-product n [0 1 0])
;;         py (vector-add start-point cy)]
;;     [start-point px py]))

;; (defn get-object-collision [world object-name x y]
;;   (let [mesh (get-in world [:objects object-name :mesh])
;;         body (get-in world [:objects object-name :body])
;;         transform (get-body-transform body)
;;         [distance point] (get-mesh-collision world x y mesh transform)]
;;     {:d distance :p point :o object-name}))

;; (defn get-front-object-collision [world x y]
;;   (let [collisions (map (fn [object-name]
;;                           (get-object-collision world object-name x y))
;;                         (keys (:objects world)))
;;         collisions (sort-by :d distance-comparator collisions)
;;         collision (first collisions)]
;;     (if (nil? (:d collision))
;;       [nil nil]
;;       [(:o collision) (:p collision)])))

;; (defn force-pressed [world event]
;;   (let [[object-name point] (get-front-object-collision world (:x event) (:y event))]
;;     (if (nil? object-name)
;;       world
;;       (let [body (get-in world [:objects object-name :body])
;;             transform (get-body-transform body)]
;;         (-> world
;;             (assoc-in [:force :local-start]
;;                       (world-to-local-coordinates transform point))
;;             (assoc-in [:force :object-name] object-name)
;;             ((fn [w]
;;                (assoc-in w [:force :plane] (get-force-plane w))))
;;             (assoc-in [:force :active] true))))))

;; (defn force-released [world event]
;;   (-> world
;;       (assoc-in [:force :active] false)
;;       (assoc-in [:force :screen-end] nil)))

;; (defn make-vector3f [[x y z]]
;;   (new Vector3f x y z))

;; (defn update-mouse-force [world]
;;   (let [force (:force world)]
;;     (when (:active force)
;;       (let [object-name (:object-name force)
;;             object (get-in world [:objects object-name])
;;             body (:body object)
;;             transform (get-body-transform body)
;;             start (get-force-start-point world)
;;             end (get-force-end-point world)
;;             force (vector-multiply (vector-subtract end start) 0.7)
;;             rel-pos (vector-subtract start (get-transform-position transform))]
;;         (.applyForce body (make-vector3f force) (make-vector3f rel-pos)))))
;;       world)

;; (defn force-moved [world event]
;;   (if (get-in world [:force :active])
;;     (assoc-in world [:force :screen-end] [(:x event) (:y event)])
;;     world))

;; (defn set-body-transform [body transform]
;;   (.clearForces body)
;;   (.setLinearVelocity body (new Vector3f 0 0 0))
;;   (.setAngularVelocity body (new Vector3f 0 0 0))
;;   (.setCenterOfMassTransform body transform)
;;   (.stepSimulation @planet 1 7 (/ 1 60.0)) ;;######### update without stepping
;;   body)

