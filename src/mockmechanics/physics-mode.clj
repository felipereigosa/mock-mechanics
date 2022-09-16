(ns mockmechanics.core
  (:require [mockmechanics.library.vector :as vector])
  (:import [com.bulletphysics.collision.dispatch
            DefaultCollisionConfiguration
            CollisionDispatcher]
           [com.bulletphysics.collision.shapes
            CollisionShape
            StaticPlaneShape
            BoxShape
            SphereShape
            CylinderShape]
           [com.bulletphysics.linearmath
            Transform
            DefaultMotionState]
           [com.bulletphysics.dynamics
            constraintsolver.SequentialImpulseConstraintSolver
            DiscreteDynamicsWorld
            RigidBodyConstructionInfo
            RigidBody]
           com.bulletphysics.collision.broadphase.AxisSweep3
           com.bulletphysics.dynamics.constraintsolver.Point2PointConstraint
           com.bulletphysics.dynamics.constraintsolver.HingeConstraint
           com.bulletphysics.dynamics.constraintsolver.SliderConstraint
           com.bulletphysics.dynamics.constraintsolver.Generic6DofConstraint
           com.bulletphysics.collision.dispatch.CollisionFlags
           javax.vecmath.Vector3f))

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
  (.addRigidBody planet body 1 3))

(defn remove-body [planet body]
  (.removeRigidBody planet body))

(defn create-ground [planet]
  (.addRigidBody planet (create-static-plane [0 1 0] 0) 2 1)
  (.addRigidBody planet (create-static-plane [1 0 0] -6) 2 1)
  (.addRigidBody planet (create-static-plane [-1 0 0] -6) 2 1)
  (.addRigidBody planet (create-static-plane [0 0 1] -6) 2 1)
  (.addRigidBody planet (create-static-plane [0 0 -1] -6) 2 1)
  planet)

(defn create-body [shape mass transform kinematic?]
  (let [motion-state (new DefaultMotionState transform)
        local-inertia (let [vec (new Vector3f)]
                        (.calculateLocalInertia shape mass vec)
                        vec)
        construction-info (new RigidBodyConstructionInfo mass motion-state
                               shape local-inertia)
        rigid-body (new RigidBody construction-info)]
    (.forceActivationState rigid-body RigidBody/DISABLE_DEACTIVATION)

    ;; (when kinematic?
    ;;   (.setCollisionFlags rigid-body
    ;;                       (bit-or
    ;;                         (.getCollisionFlags rigid-body)
    ;;                         CollisionFlags/KINEMATIC_OBJECT)))
    rigid-body))

(declare make-transform)

(defn create-sphere-body [r mass transform]
  (create-body (new SphereShape r) mass transform false))

(defn get-body-transform [body]
  (let [transform (new Transform)]
    (.getWorldTransform (.getMotionState body) transform)))

(defn step-simulation! [planet elapsed]
  (.stepSimulation planet elapsed 7 (/ 1 60.0)))

(defn create-physics-world [world]
  (let [r 0.2
        d (* r 2)
        mesh (create-model-mesh "res/physical-sphere.obj"
                                [0 0 0] [1 0 0 0]
                                [d d d] nil)
        world (-> world
                  (assoc-in [:planet] (create-planet))
                  (assoc-in [:sphere-radius] r)
                  (assoc-in [:sphere-mesh] mesh)
                  (assoc-in [:bodies] [])
                  (assoc-in [:spheres] []))]
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

(defn get-sphere-at [world spec]
  (let [line (get-spec-line world spec)
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
                        (vector/distance position eye)))
                    spheres))))

(defn draw-spheres! [world]
  (let [mesh (:sphere-mesh world)]
    (doseq [body (:spheres world)]
      (let [transform (get-body-transform body)
            mesh (assoc-in mesh [:transform] transform)]
        (draw-mesh! world mesh)))))

(defn is-solid-part? [[name part]]
  ;; (and
  ;;  (in? (:type part) [:block :wagon])
  ;;  (:solid part))
  (= (:type part) :block)
  )

(defn remove-all-bodies [world]
  (doseq [{:keys [body]} (:bodies world)]
    (remove-body (:planet world) body))
  (assoc-in world [:bodies] []))

(defn create-part-body [part-name parts groups]
  (let [part (get-in parts [part-name])
        [w h d] (:scale part)
        shape (new BoxShape (new Vector3f (/ w 2) (/ h 2) (/ d 2)))
        body (create-body shape 100 (make-transform [0 0 0] [1 0 0 0]) true)
        root-name (first (find-if #(in? part-name %) groups))
        root (get-in parts [root-name])
        part-transform (:transform part)
        root-transform (:transform root)
        relative-transform (remove-transform part-transform
                                             root-transform)]
    {:body body
     :transform relative-transform
     :root root-name}))

(defn create-part-bodies [world parts groups]
  (let [world (remove-all-bodies world)
        solid-part-names (map first (filter is-solid-part? parts))
        bodies (map #(create-part-body % parts groups)
                    solid-part-names)]
    (doseq [{:keys [body]} bodies]
      (add-body-to-planet (:planet world) body))
    (assoc-in world [:bodies] bodies)))

(defn recompute-body-transforms! [world]
  (doseq [b (:bodies world)]
    (let [body (:body b)
          parent (get-in world [:weld-groups (:root b)])
          relative-transform (:transform b)
          parent-transform (:transform parent)
          transform (combine-transforms relative-transform
                                        parent-transform)]
      (.setCenterOfMassTransform body transform)
      (.setAngularVelocity body (new Vector3f 0 0 0))
      (.setLinearVelocity body (new Vector3f 0 0 0))))
  world)

(defn add-sphere [world event]
  (if-let [collision (get-part-collision world event)]
    (let [local-normal (get-collision-normal world collision)
          part (get-in world [:parts (:part-name collision)])
          rotation (get-rotation-component (:transform part))
          normal (apply-transform rotation local-normal)
          offset (vector/multiply (vector/normalize normal)
                                  (:sphere-radius world))
          position (vector/add (:point collision) offset)]
      (create-sphere world position))
    (let [line (get-spec-line world event)
          ground-plane [[0 0 0] [1 0 0] [0 0 1]]
          offset [0 (:sphere-radius world) 0]
          point (line-plane-intersection line ground-plane)
          position (vector/add point offset)]
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

(defn physics-mode-pressed [world event]
  (if-let [sphere (get-sphere-at world event)]
    (delete-sphere world sphere)
    (add-sphere world event)))

(def v-in (atom (new Vector3f 0 0 0)))
(def v-out (atom (new Vector3f)))

(defn spheres-moving? [world]
  (let [velocities (map (fn [sphere]
                          (.getVelocityInLocalPoint sphere @v-in @v-out)
                          (.length @v-out))
                        (:spheres world))]
    (some #(> % 0.1) velocities)))
