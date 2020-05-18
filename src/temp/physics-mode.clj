
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
(import com.bulletphysics.collision.dispatch.CollisionFlags)
(import javax.vecmath.Vector3f)

(defn create-planet []
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
    my-planet))
  
(defn create-static-plane [[nx ny nz] d]
  (let [shape (new StaticPlaneShape (new Vector3f nx ny nz) d)
        transform (let [t (new Transform)]
                           (.setIdentity t)
                           t)
        motion-state (new DefaultMotionState transform)
        construction-info (new RigidBodyConstructionInfo 0 motion-state
                                      shape (new Vector3f 0 0 0))
        rigid-body (new RigidBody construction-info)]
    rigid-body))

(defn add-body-to-planet [planet body]
  (.addRigidBody planet body 1 1))

(defn remove-body [planet body]
  (.removeRigidBody planet body))

(defn create-ground [planet]
  (add-body-to-planet planet (create-static-plane [0 1 0] 0))
  (add-body-to-planet planet (create-static-plane [1 0 0] -6))
  (add-body-to-planet planet (create-static-plane [-1 0 0] -6))
  (add-body-to-planet planet (create-static-plane [0 0 1] -6))
  (add-body-to-planet planet (create-static-plane [0 0 -1] -6))
  planet)

(defn create-body [shape mass transform]
  (let [motion-state (new DefaultMotionState transform)
        local-inertia (let [vec (new Vector3f)]
                        (.calculateLocalInertia shape mass vec)
                        vec)
        construction-info (new RigidBodyConstructionInfo mass motion-state
                               shape local-inertia)
        rigid-body (new RigidBody construction-info)]
    (.forceActivationState rigid-body RigidBody/DISABLE_DEACTIVATION)
    rigid-body))

(declare make-transform)

(defn create-kinematic-body [position rotation scale]
  (let [[w h d] scale
        shape (new BoxShape (new Vector3f (/ w 2) (/ h 2) (/ d 2)))
        transform (make-transform position rotation)
        motion-state (new DefaultMotionState transform)
        mass 1.0
        local-inertia (let [vec (new Vector3f)]
                        (.calculateLocalInertia shape mass vec)
                        vec)
        construction-info (new RigidBodyConstructionInfo mass motion-state
                               shape local-inertia)
        body (new RigidBody construction-info)]
    (.setCollisionFlags body (bit-or (.getCollisionFlags body)
                                     CollisionFlags/KINEMATIC_OBJECT))
    (.forceActivationState body RigidBody/DISABLE_DEACTIVATION)
    body))

(defn create-sphere-body [r mass transform]
  (create-body (new SphereShape r) mass transform))

(defn set-body-transform [body transform]
  (.setWorldTransform (.getMotionState body) transform))

(defn get-body-transform [body]
  (let [transform (new Transform)]
    (.getWorldTransform (.getMotionState body) transform)))

(defn step-simulation! [planet elapsed]
  (.stepSimulation planet elapsed 7 (/ 1 60.0)))

(defn create-physics-world [world]
  (let [r 0.2
        d (* r 2)
        mesh (create-model-mesh "resources/physical-sphere.obj"
                                [0 0 0] [1 0 0 0]
                                [d d d] nil)
        world (-> world
                  (assoc-in [:planet] (create-planet))
                  (assoc-in [:sphere-radius] r)
                  (assoc-in [:sphere-mesh] mesh))]
    (create-ground (:planet world))
    world))

(defn make-sphere [world position rotation]
  (let [body (create-sphere-body
              (:sphere-radius world) 1.0
              (make-transform position rotation))]
    (add-body-to-planet (:planet world) body)
    body))

(defn create-sphere [world position]
  (let [body (create-sphere-body
              (:sphere-radius world) 1.0
              (make-transform position [1 0 0 0]))]
    (add-body-to-planet (:planet world) body)
    (update-in world [:spheres] (partial cons body))))

(defn delete-sphere [world sphere]
  (remove-body (:planet world) sphere)
  (update-in world [:spheres]
             (fn [spheres]
               (remove #(= % sphere) spheres))))

(defn get-sphere-at [world x y]
  (let [line (unproject-point world [x y])
        radius (:sphere-radius world)
        spheres (filter (fn [sphere]
                          (let [transform (get-body-transform sphere)
                                position (get-transform-position transform)]
                            (< (point-line-distance position line) radius)))
                        (:spheres world))
        eye (get-in world [:camera :eye])]
    (first (sort-by (fn [sphere]
                      (let [transform (get-body-transform sphere)
                            position (get-transform-position transform)]
                        (distance position eye)))
                    spheres))))

(defn draw-spheres! [world]
  (let [mesh (:sphere-mesh world)]
    (doseq [body (:spheres world)]
      (let [transform (get-body-transform body)
            mesh (assoc-in mesh [:transform] transform)]
        (draw-mesh! world mesh)))))

(defn is-physical-part? [[name part]]
  (and
   (in? (:type part) [:block :wagon])
   (:physics part)))

(defn compute-kinematic-body [part-name parts groups]
  (let [part (get-in parts [part-name])
        position (get-transform-position (:transform part))
        rotation (get-transform-rotation (:transform part))
        scale (:scale part)
        body (create-kinematic-body position rotation scale)
        root-name (first (find-if #(in? part-name %) groups))
        root (get-in parts [root-name])
        part-transform (:transform part)
        root-transform (:transform root)
        relative-transform (remove-transform part-transform
                                             root-transform)]
    {:body body
     :transform relative-transform
     :root root-name}))

(defn compute-kinematic-bodies [parts groups]
  (let [physical-part-names (map first (filter is-physical-part? parts))]
    (map #(compute-kinematic-body % parts groups)
         physical-part-names)))

(defn remove-all-bodies [world]
  (doseq [{:keys [body]} (:bodies world)]
    (remove-body (:planet world) body))
  (assoc-in world [:bodies] []))

(defn create-kinematic-bodies [world parts groups]
  (let [world (remove-all-bodies world)
        kinematic-bodies (compute-kinematic-bodies parts groups)]
    (doseq [{:keys [body]} kinematic-bodies]
      (add-body-to-planet (:planet world) body))
    (assoc-in world [:bodies] kinematic-bodies)))

(defn recompute-body-transforms! [world]
  (doseq [b (:bodies world)]
    (let [body (:body b)
          parent (get-in world [:weld-groups (:root b)])
          relative-transform (:transform b)
          parent-transform (:transform parent)
          transform (combine-transforms relative-transform
                                        parent-transform)]
      (set-body-transform body transform))))

(defn add-sphere [world x y]
  (if-let [collision (get-part-collision world x y)]
    (let [local-normal (get-collision-normal world collision)
          part (get-in world [:parts (:part-name collision)])
          rotation (get-rotation-component (:transform part))
          normal (apply-transform rotation local-normal)
          offset (vector-multiply (vector-normalize normal)
                                  (:sphere-radius world))
          position (vector-add (:point collision) offset)]
      (create-sphere world position))
    (let [line (unproject-point world [x y])
          ground-plane [[0 0 0] [1 0 0] [0 0 1]]
          offset [0 (:sphere-radius world) 0]
          point (line-plane-intersection line ground-plane)
          position (vector-add point offset)]
      (create-sphere world position))))

(defn delete-sphere [world sphere]
  (remove-body (:planet world) sphere)
  (update-in world [:spheres]
             (fn [spheres]
               (remove #(= % sphere) spheres))))

(defn delete-all-spheres [world]
  (doseq [sphere (:spheres world)]
    (remove-body (:planet world) sphere))
  (assoc-in world [:spheres] []))

(defn physics-mode-pressed [world {:keys [x y]}]
  (add-sphere world x y))

