(ns set-invaders.core
  (:require [quil
             [core :as quil]
             [middleware]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Index
;;  1. Constants
;;  2. Creating Game Objects
;;  3. Drawing Routines
;;  4. Particle Effects
;;  5. Gameplay Mechanics
;;  6. Initial Game State
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 1. Constants
;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(def colors
  "Quil/Processings internal integer representations for the colors."
  {:red   -65536
   :green -16711936
   :blue  -16776961})


(def base-width
  "Used as a base unit length. Everything is proportional to this"
  20)

(def stroke-weight
  "Line thickness for all drawing"
  4)


(def pair-overlap
  "Half the number of pixels of overlap the pair of sprites that make up one enemy"
  10)

(def pair-swap-time
  "A constant proportional to the amount of time it takes for an enemy pair to rotate"
  0.02)


(def default-enemy-dx
  "Proportional to the speed that a row of enemies moves from left to right"
  2)

(def default-enemy-dy
  "Proportional to the speed that a row of enemies moves from top to bottom"
  2)

(def next-on-axis
  "This established the order to cycle through colors, fill-types, and shapes"
  {;; colors
   :red :green
   :green :blue
   :blue :red
   ;; shapes
   :square :circle
   :circle :triangle
   :triangle :square
   ;;fill-types
   :fill :crossed
   :crossed :empty
   :empty :fill})


(def triangle-points
  "A vector of the form [x1 y1 x2 y2 x3 y3] for an equilateral triangle proportional to base-width"
  (vec
   (flatten
    (for [angle [30 150 270]]
      [(* base-width (quil/sin (quil/radians angle))) (* base-width (quil/cos (quil/radians angle)))]))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 2. Creating Game Elements
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sprite
  "A Square, Circle or Triangle, with an associated color and fill type.
  This is always part of a pair (one enemy), a bullet, or a particle."
  [color shape fill-type]
  {:color color :shape shape :fill-type fill-type :alpha 255})

(defn random-sprite
  "Creates a random sprite."
  []
  (sprite (rand-nth [:red :green :blue])
          (rand-nth [:square :circle :triangle])
          (rand-nth [:fill :crossed :empty])))


(defn pair
  "Returns a pair of sprites. This is used in game to represent _one_ enemy."
  [left right]
  {:block-type :pair
   :swap-t (quil/random 0 3)
   :left left
   :right right})

(defn row
  "Creates a row of enemies. x is initial leftmost enemy of the row, y the height of all the enemies.
  dx and dy are the speed at which the row moves."
  [sprites x y dx dy]
  {:block-type :row :x x :y y :dx dx :dy dy
   :columns (into {} (map-indexed vector sprites))})

(defn particle
  "Creates a particle. This will have no effect on game play, but will be pretty."
  [sprite x y dx dy dr scale ds dalpha]
  {:block-type :particle
   :x x :y y :dx dx :dy dy :dr dr :ds ds :dalpha dalpha :scale scale :rotation 0.0
   :contents sprite})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 3. Drawing routines
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn set-colors
  "Extracted from Sprite drawing. Called from render."
  [{:keys [color fill-type alpha]}]
  (quil/stroke (colors color) alpha)
  (if (= :fill fill-type)
    (quil/fill (colors color) alpha)
    (quil/fill (colors color) 0)))

(defmulti render
  "Render a sprite to the screen."
  (fn [sprite]
    (or (get sprite :shape)
        (get sprite :block-type))))

(defmethod render :square
  [{:keys [fill-type] :as sprite}]
  (set-colors sprite)
  (quil/rect (- base-width) (- base-width) (* 2 base-width) (* 2 base-width))
  (when (= fill-type :crossed)
    (do
      (quil/line (- base-width) (- base-width) base-width base-width)
      (quil/line (- base-width) base-width base-width (- base-width)))))

(defmethod render :triangle
  [{:keys [fill-type] :as sprite}]
  (set-colors sprite)
  (apply quil/triangle triangle-points)
  (when (= fill-type :crossed)
    (do
      (quil/line (triangle-points 0) (triangle-points 1) 0 0)
      (quil/line (triangle-points 2) (triangle-points 3) 0 0)
      (quil/line (triangle-points 4) (triangle-points 5) 0 0))))

(defmethod render :circle
  [{:keys [fill-type] :as sprite}]
  (set-colors sprite)
  (quil/ellipse 0 0 (* 2 base-width) (* 2 base-width))
  (when (= fill-type :crossed)
    (do
      (quil/line (- base-width) 0 base-width 0)
      (quil/line 0 base-width 0 (- base-width)))))

(defmethod render :pair
  [{:keys [left right swap-t] :as pair}]
  (let [offset (quil/constrain (* (quil/cos swap-t) base-width) (- pair-overlap base-width) (- base-width pair-overlap))
        render-left (fn []
                      (quil/push-matrix)
                      (quil/translate offset 0)
                      (render left)
                      (quil/pop-matrix))
        render-right (fn []
                       (quil/push-matrix)
                       (quil/translate (- offset) 0)
                       (render right)
                       (quil/pop-matrix)
                       )]
    (case (:fill-type left)
      :fill (do (render-left) (render-right))
      :empty (do (render-right) (render-left))
      :crossed (do (render-right) (render-left)))
    ))

(defmethod render :row
  [{:keys [x y columns] :as row}]
  (doseq [[index sprite] columns]
    (quil/push-matrix)
    (quil/translate (+ x (* 1.5 base-width) (* index base-width 5)) y)
    (render sprite)
    (quil/pop-matrix)
    ))

(defmethod render :player
  [{:keys [x bullet]}]
  (let [height (- (quil/height) 40)]
    (quil/fill 255 255 255 255)
    (quil/stroke 255 255 255 255)
    (quil/rect (- x (* 1.5 base-width)) (- height 2) (* 3 base-width) 4)
    (quil/push-matrix)
    (quil/translate x (- height base-width 15))
    (quil/scale 0.60)
    (render bullet)
    (quil/pop-matrix)
  ))

(defmethod render :particle
  [{:keys [x y rotation scale contents]}]
  (quil/push-matrix)
  (quil/translate x y)
  (quil/scale scale)
  (quil/rotate rotation)
  (render contents)
  (quil/pop-matrix))

(defn render-score
  "Renders the score in the lower right corner of the screen"
  [score]
  (quil/text-align :right)
  (quil/fill 255 255 255 255)
  (quil/text (str score) (- (quil/width) 10) (- (quil/height) 14)))

(defn render-hitbox
  "Draws a rectangle around the provided box"
  [[l t r b]]
  (let [width (- r l)
        height (- b t)]
    (quil/stroke 255 255 255 100)
    (quil/fill 0 0 0 0)
    (quil/rect l t width height)))

(defn draw-state
  "Draws every element on the screen (in Z-Index order):
  1. The Particles.
  2. The Player.
  3. The Enemies.
  4. The Score."
  [{:keys [rows player score particles]}]
  (quil/background 0)
  (quil/stroke-weight stroke-weight)
  (doall (map render particles))
  (render player)
  (doall (map render rows))
  (render-score score))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  4. Particle Effects (because fun)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn particle-burst
  "Spawn a burst of #[size] particles at origin-x,origin-y"
  [origin-x origin-y size]
  (repeatedly size (fn [] (particle (random-sprite)
                                    origin-x
                                    origin-y
                                    (quil/random -5 5)
                                    (quil/random -5 5)
                                    (quil/random -1.5 1.5)
                                    (quil/random 0.01 0.5)
                                    (quil/random -0.1 0)
                                    (quil/random -4.0 0)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  5. Gameplay Mechanics
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti hitbox
  "Returns a vector of for [left top right bottom] that describe the bounds of the provided sprite."
  (fn [sprite]
    (or (get sprite :shape)
        (get sprite :block-type))))

(defmethod hitbox :square
  [sprite]
  [(- base-width) (- base-width) base-width base-width])

(defmethod hitbox :circle
  [sprite]
  [(- base-width) (- base-width) base-width base-width])

(defmethod hitbox :triangle
  [sprite]
  [(- base-width) (- base-width) base-width base-width])

(defmethod hitbox :pair
  [pair]
  [(- pair-overlap base-width base-width) (- base-width) (+ (- base-width pair-overlap) base-width) base-width])

(defmethod hitbox :row
  [{:keys [x y columns]}]
  (let [min-ind (apply min (keys columns))
        max-ind (apply max (keys columns))
        left (+ x
                (* min-ind base-width 5))
        right (+ x
                 (* 4.5 base-width)
                 (* max-ind base-width 5)
                 (* 0.5 (- pair-overlap)))
        ]
     [left (- y base-width) right (+ base-width y)]))


(defn cycle-axis
  "Given a sprite and an axis (one of :color, :shape, or :fill-type) returns a new sprite that differs on that axis."
  [sprite axis]
  (update sprite axis next-on-axis))



(defn animate-pair
  "Updates the animation timing to rotate a pair of sprites"
  [pair]
  (update pair :swap-t + pair-swap-time))

(defn animate-row-pairs
  "Animates all the pairs in a row"
  [row]
  (update row :columns
          (fn [pairs-map] (reduce-kv
                           (fn [new-pairs-map pair-index pair]
                             (assoc new-pairs-map pair-index (animate-pair pair)))
                           nil
                           pairs-map))))

(defn update-row-location
  "Advances a row of enemies towards the player"
  [{:keys [columns x dx dy] :as row}]
  (when columns
    (let [[left top right bottom] (hitbox row)]
      (if (< 0 left right (quil/width))
        (update row :x + dx)
        (-> row
            (update :dx -)
            (update :x - dx)
            (update :y + dy))))))

(defn update-rows
  "Updates all the rows of enemies by:
  1. Moving them left/right and towards the player.
  2. Animating the enemies."
  [state]
  (update-in state [:rows]
             (fn
               [row]
               (vec (filter identity (map (comp animate-row-pairs update-row-location) row))))))

(defn update-player-location
  "Updates the player location based on the last direction they moved."
  [state]
  (let [direction (-> state :player :move)
        right-cap (- (quil/width) (* 1.75 base-width))
        delta 4]
    (case direction
      :left (update-in state [:player :x] (fn [x] (max (* 1.75 base-width) (- x delta))))
      :right (update-in state [:player :x] (fn [x] (min right-cap (+ x delta))))
      state)))

(defn bounds-check-particle
  "Returns nil is a particle is:
  1. No longer on the visible screen.
  2. Faded (alpha low).
  3. Shrunk (scale low)."
  [{:keys [x y scale] {alpha :alpha} :contents :as particle}]
  (cond
    (or (< x 0) (> x (quil/width))) nil
    (or (< y 0) (> y (quil/height))) nil
    (< alpha 0) nil
    (< scale 0) nil
    :else particle))

(defn animate-particle
  "Animates a particle based on it's rotation, dx,dx, scale change, and alpha change rates.
  If that particle isn't visible, returns nil."
  [{:keys [dx dy dr ds dalpha] :as particle}]
  (-> particle
      (update :x + dx)
      (update :y + dy)
      (update :scale + ds)
      (update :rotation + dr)
      (update-in [:contents :alpha] + dalpha)
      bounds-check-particle))

(defn animate-particles
  "Animates all particles. Removes particles that are no longer visible."
  [{:keys [particles] :as state}]
  (let [new-particles (vec (filter identity (map animate-particle particles)))]
    (assoc state :particles new-particles)))

(defn update-state [state]
  "Function to update the game state:
1. Updates the player location based on last movement direction.
2. Moves the enemies towards the player.
3. Animates particles."
  (-> state
      update-player-location
      update-rows
      animate-particles
      ))

(defn key-pressed
  "Updates state based on keys for bullet type rotation, player movement, and a secret key that does something pretty."
  [state {pressed :raw-key :as event}]
  (assoc
   (case pressed
     \i (update-in state [:player :bullet] cycle-axis :shape)
     \o (update-in state [:player :bullet] cycle-axis :color)
     \p (update-in state [:player :bullet] cycle-axis :fill-type)
     \a (assoc-in state  [:player :move] :left)
     \d (assoc-in state  [:player :move] :right)
     \t (update state :particles into (particle-burst (get-in state [:player :x]) (quil/random 100 500) 180))
     state)
   :last-key event))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  6. Initial Game State
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def initial-row-count
  "The number of rows of enemies."
  4)

(def initial-column-count
  "The number of enemies per row"
  6)

(def gutter
  "Space between the edge of the screen and randomized placements."
  20)

(defn setup []
  "Returns an initial game state (some parts randomized)"
  (quil/frame-rate 30)
  (quil/color-mode :rgb)
  {:score 1
   :player {:block-type :player
            :move :none
            :x (quil/random (+ base-width gutter) (- (quil/width) base-width gutter))
            :bullet (random-sprite)}
   :particles []
   :rows (vec
          (for [y-offset (take initial-row-count (iterate (partial + base-width base-width gutter) 50))]
            (row (repeatedly initial-column-count (fn [] (pair (random-sprite) (random-sprite))))
                 gutter y-offset
                 default-enemy-dx default-enemy-dy)
            ))
            })

(quil/defsketch set-invaders
  :title "Set Invaders!"
  :size [1000 800]
  :setup setup
  :update update-state
  :draw draw-state
  :key-pressed key-pressed
  :middleware [quil.middleware/fun-mode quil.middleware/pause-on-error])

