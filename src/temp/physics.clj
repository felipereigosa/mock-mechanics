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
    (.addRigidBody @planet rigid-body 1 0xffffffff)
    rigid-body))

(defn create-ground! []
  (create-static-plane [0 1 0] 0)
  ;; (create-static-plane [0 0 1] -30)
  ;; (create-static-plane [0 0 -1] -30)
  ;; (create-static-plane [1 0 0] -30)
  ;; (create-static-plane [-1 0 0] -30)
  )

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

(declare get-transform-matrix)
(declare get-transform-position)
(declare get-transform-rotation)
(declare make-vector3f)

(defn get-body-transform [body]
  (let [transform (new Transform)]
    (.getWorldTransform (.getMotionState body) transform)))

(defn step-simulation! [elapsed]
  (.stepSimulation @planet elapsed 7 (/ 1 60.0)))

(defn apply-local-force [body force point]
  (.applyForce body (make-vector3f force) (make-vector3f point))
  body)

(defn apply-force [body force point]
  (let [transform (get-body-transform body)
        body-position (get-transform-position transform)
        point (vector-subtract point body-position)]
    (.applyForce body (make-vector3f force) (make-vector3f point))
    body))


(defn body-local-point [body point]
  (let [[x y z] point
        vector (into-array Float/TYPE [x y z 1.0])
        transform (get-body-transform body)
        matrix (get-inverse-matrix (get-transform-matrix transform))
        [x y z _] (into [] (multiply-matrix-vector matrix vector))]
    [x y z]))

(defn body-local-axis [body axis]
  (let [[x y z] axis
        vector (into-array Float/TYPE [x y z 1.0])
        rotation (get-transform-rotation (get-body-transform body))
        transform (make-transform [0 0 0] rotation)
        matrix (get-inverse-matrix (get-transform-matrix transform))
        [x y z _] (into [] (multiply-matrix-vector matrix vector))]    
    [x y z]))

(defn create-weld-constraint [body-a body-b]
  (let [point (get-transform-position (get-body-transform body-a))
        axis [0 1 0]
        pivot-a (make-vector3f (body-local-point body-a point))
        axis-a (make-vector3f (body-local-axis body-a axis))
        pivot-b (make-vector3f (body-local-point body-b point))
        axis-b (make-vector3f (body-local-axis body-b axis))
        constraint (new HingeConstraint body-a body-b
                        pivot-a pivot-b axis-a axis-b)]
    (.setLimit constraint 0 0.01)
    (.addConstraint @planet constraint)
    constraint))

(defn create-hinge-constraint [body-a body-b point axis]
  (let [pivot-a (make-vector3f (body-local-point body-a point))
        axis-a (make-vector3f (body-local-axis body-a axis))
        pivot-b (make-vector3f (body-local-point body-b point))
        axis-b (make-vector3f (body-local-axis body-b axis))
        constraint (new HingeConstraint body-a body-b
                        pivot-a pivot-b axis-a axis-b)]
    (.addConstraint @planet constraint)
    constraint))

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
