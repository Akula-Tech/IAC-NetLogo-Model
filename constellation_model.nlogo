patches-own [
  event-intensity   ; event strength [0,1]
  event-end-time    ; when event will end in ticks
  event-type        ; type of event (1 = orange, 2 = blue, 3 = green)
  is-static-zone    ; boolean to indicate if the patch is part of a static monitoring zone
]

globals [
  orbit-y-coords    ; list that stores the y-coordinates of each orbit
  event-colors      ; list of colors for different event types
  event-priorities  ; list of priorities for different event types
  static-zone-color ; color for static monitoring zones
  task-id-counter  ; Counter for generating unique task IDs

]

breed [ground-stations ground-station]
breed [coverage-circles coverage-circle]
breed [satellites satellite]
breed [tasks task]


satellites-own [
  orbit-index       ; which orbit a satellite belongs to (0 to num-orbits - 1)
;  event-detected    ; 0 or 1 indicating if a satellite sensor is withing range of an event patch
  event-priority    ; Priority of the detected event
  gs-visibility     ; 0 or 1 indicating if the satellite is connected to a ground station
  distances-to-gs    ; list to store distances to each ground station
  closest-gs         ; the closest ground station
  distance-to-closest-gs  ; distance to the closest ground station
  ;links-to-closest-gs  ; New variable to store num links to closest ground station

  ; New variables for enhanced event detection
  events-detected           ; list of event types detected
  event-coverage-percentages ; list of coverage percentages for each event type
  total-sensor-patches      ; total number of patches within sensor coverage
  event-centers             ; list of center coordinates for each event type
  event-distances           ; list of distances to each event center
  event-directions          ; list of directions to each event center
  interpreted-event-directions
  event-exposure-times      ; list of exposure times for each event type
  north-link        ; link to the satellite directly north
  south-link        ; link to the satellite directly south
  east-link         ; link to the satellite directly east
  west-link         ; link to the satellite directly west
  northeast-link    ; link to the satellite in the northeast
  northwest-link    ; link to the satellite in the northwest
  southeast-link    ; link to the satellite in the southeast
  southwest-link    ; link to the satellite in the southwest
  over-static-zone  ; 0 or 1 indicating if the satellite is over a static monitoring zone
  outline-color     ; color of the satellite's outline
  dp-unit-tops      ; TOPS measure for the data processing unit

  power                     ; Current power level of the satellite
  max-power                 ; Maximum power capacity
  ;power-generation-rate     ; Rate at which the satellite generates power (per tick)
  remote-sensing-payload    ; available remote sensing payload
  current-task              ; The task currently being executed by the satellite
  current-tasks             ; List of tasks currently being performed by the satellite
  task-queue                ; List of tasks assigned to this satellite but not yet executed
  completed-tasks           ; List of tasks completed by this satellite
  total-score               ; Cumulative score from completed tasks
  task-success-rate
  ; Properties for contract-network protocol
  available-for-tasks       ; Boolean indicating if the satellite can take on new tasks
  bid-in-progress           ; Boolean indicating if the satellite is currently bidding on a task
  task-indicators

]

tasks-own [
  task-id               ; Unique identifier for the task
  task-type             ; Type of task (e.g., "low-power-detection", "medium-power-detection", etc.)
  task-priority         ; Priority of the task (higher number = higher priority)
  time-required         ; Time required to complete the task (in ticks)
  power-required        ; Power required to complete the task
  score                 ; Score given if the task is completed
  ;sensor-coverage-req   ; Required sensor coverage (as a percentage)
  ;payload-req           ; Required remote-sensing payload/sensor

  ; Event-related properties
  event-xcor            ; X-coordinate of the associated event
  event-ycor            ; Y-coordinate of the associated event

  ; Task status properties
  status                ; Current status of the task (e.g., "unassigned", "assigned", "in-progress", "completed")
  assigned-to           ; The satellite assigned to this task (if any)
  progress              ; Current progress of the task (0-100%)

  ; For relay and sub-contracting tasks
  origin-satellite      ; The satellite that originated the task
  destination-satellite ; The intended recipient satellite or ground station
]



ground-stations-own [
  coverage-area     ; radius of the ground station's coverage area
  sat-visibility     ; 0 or 1 indicating if the ground station is connected to a satellite
]

to setup
  clear-all
  setup-orbits
  setup-event-properties
  setup-patches

  create-ground-stations-with-coverage
  draw-ground-station-coverage
  create-satellites-with-coverage
  draw-satcom-coverage
  draw-sensor-rectangles
  setup-inter-satellite-links
  setup-static-zones
  ask patches [
    set event-intensity 0
    set event-end-time 0
    set event-type 0
  ]
  set task-id-counter 0

  reset-ticks
  manage-events
end

to generate-task [task-type-input priority-input time-req score-input coverage-req payload-input event-x event-y]
  create-tasks 1 [
    set task-id task-id-counter
    set task-type task-type-input
    set task-priority priority-input
    set score score-input
    set event-xcor event-x
    set event-ycor event-y
    set status "unassigned"
    set assigned-to nobody
    set progress 0
    set origin-satellite nobody
    set destination-satellite nobody

    ; Set power-required based on task-type
    set power-required (
      ifelse-value
      task-type = "isl" [ power-req-isl ]
      task-type = "gs" [ power-req-gs ]
      task-type = "rs" [ power-req-rs ]
      task-type = "dp" [ power-req-dp ]
      task-type = "ai-low" [ power-req-ai-low ]
      task-type = "ai-mid" [ power-req-ai-mid ]
      task-type = "ai-high" [ power-req-ai-high ]
      [ 0 ]  ; Default case, you might want to handle this differently
    )

    ; Set time-required based on task-type
    set time-required (
      ifelse-value
      task-type = "isl" [ time-req-isl ]
      task-type = "gs" [ time-req-gs ]
      task-type = "rs" [ time-req-rs ]
      task-type = "dp" [ time-req-dp ]
      task-type = "ai-low" [ time-req-ai-low ]
      task-type = "ai-mid" [ time-req-ai-mid ]
      task-type = "ai-high" [ time-req-ai-high ]
      [ 0 ]  ; Default case, you might want to handle this differently
    )

    ; Position the task at the event location
    setxy event-x event-y
    hide-turtle  ; Hide the task "turtle" as we don't need to visualize it
  ]
  set task-id-counter task-id-counter + 1
end


; New procedure to initialize patches
to setup-patches
  ask patches [
    set event-intensity 0
    set event-end-time 0
    set event-type 0
    set is-static-zone false  ; Initialize is-static-zone for all patches
  ]
end

to setup-event-properties
  set event-colors [orange blue green]
  set event-priorities [1 0.7 0.2]
  set static-zone-color 44
end

; New procedure to set up static monitoring zones
to setup-static-zones
  let num-zones random (max-static-zones + 1)
  repeat num-zones [
    let zone-width random max-static-zone-size + 1
    let zone-height random max-static-zone-size + 1
    let zone-x random-xcor
    let zone-y random-ycor
    ask patches with [
      abs (pxcor - zone-x) <= zone-width / 2 and
      abs (pycor - zone-y) <= zone-height / 2
    ] [
      set is-static-zone true
      set pcolor static-zone-color
    ]
  ]
end

; Calculate the y-coordinates for each orbit
to setup-orbits

  let orbit-spacing world-height / (num-orbits + 1)  ; calculating space between orbits

  ; create list of y-coordinates for each orbit, evenly spaced
  set orbit-y-coords n-values num-orbits [i -> min-pycor + ((i + 1) * orbit-spacing)]

end

; Create satellites and position them in their orbits
to create-satellites-with-coverage
  foreach (range num-orbits) [ i ->  ; for each orbit
    let y-coord item i orbit-y-coords  ; get the y-coordinate for this orbit
    let x-spacing world-width / num-sats-per-orbit  ; calculate space between satellites
    foreach (range num-sats-per-orbit) [ j ->  ; for each satellite in this orbit
      create-satellites 1 [  ; create a new satellite
        setxy (min-pxcor + (j * x-spacing)) y-coord  ; initial position of the satellite
        set heading 90  ; make it face right
        set color white
        set outline-color black
        set size 1.5
        set orbit-index i  ; assign it to this orbit
;        set event-detected 0  ; initialize event detection status
        set event-priority 0
        ; Initialize inter-satellite link variables
        set north-link nobody
        set south-link nobody
        set east-link nobody
        set west-link nobody
        set northeast-link nobody
        set northwest-link nobody
        set southeast-link nobody
        set southwest-link nobody
        set max-power 100            ; Set maximum power capacity
        set power max-power          ; Start with full power
        set current-tasks []
        set events-detected []
        set task-indicators []

      ]
    ]
  ]
end

;to manage-power
;  ask satellites [
;    ; Deplete power based on AI usage
;    set power power - power-req-ai-low
;
;    ; Regenerate power if in the first half of the world (sunlight)
;    if xcor < 0 [
;      set power power + power-regen-rate
;    ]
;
;    ; Ensure power stays within bounds
;    set power max (list 0 (min (list power max-power)))
;  ]
;end

to manage-power
  ask satellites [
    ; Deplete power based on AI usage and current tasks
    set power power - power-req-ai-low
    foreach current-tasks [ task_ ->
      if power >= [power-required] of task_ [
        set power power - [power-required] of task_
      ]
    ]

    ; Regenerate power if in the first half of the world (sunlight)
    if xcor < 0 [
      set power power + power-regen-rate
    ]

    ; Ensure power stays within bounds
    set power max (list 0 (min (list power max-power)))
  ]
end



to create-ground-stations-with-coverage
  ifelse even-gs-distribution [
    create-evenly-distributed-ground-stations
  ] [
    create-randomly-distributed-ground-stations
  ]
end

to create-randomly-distributed-ground-stations
  create-ground-stations num-ground-stations [
    setxy random-xcor random-ycor
    set-ground-station-properties
  ]
end

to create-evenly-distributed-ground-stations
  let num-rows round sqrt num-ground-stations
  let num-cols ceiling (num-ground-stations / num-rows)

  let row-spacing world-height / (num-rows + 1)
  let col-spacing world-width / (num-cols + 1)

  let ground-stations-created 0
  let row 1
  let col 1

  while [ground-stations-created < num-ground-stations] [
    create-ground-stations 1 [
      setxy (min-pxcor + col * col-spacing) (min-pycor + row * row-spacing)
      set-ground-station-properties
    ]

    set ground-stations-created ground-stations-created + 1

    set col col + 1
    if col > num-cols [
      set col 1
      set row row + 1
    ]
  ]
end

to set-ground-station-properties
  set shape "house"
  set color 9
  set size 2
  set heading 0  ; make it face right
  set coverage-area ground-station-coverage
end

; New procedure to set up inter-satellite links
to setup-inter-satellite-links
  ask satellites [
    let current-satellite self
    let current-orbit orbit-index

    ; Find east and west links (same orbit)
    set east-link min-one-of (other satellites with [orbit-index = current-orbit and xcor > [xcor] of current-satellite]) [xcor]
    set west-link max-one-of (other satellites with [orbit-index = current-orbit and xcor < [xcor] of current-satellite]) [xcor]

    ; Handle wrap-around for east and west
    if east-link = nobody [
      set east-link min-one-of (satellites with [orbit-index = current-orbit]) [xcor]
    ]
    if west-link = nobody [
      set west-link max-one-of (satellites with [orbit-index = current-orbit]) [xcor]
    ]

    ; Find north and south links (adjacent orbits)
    if current-orbit > 0 [
      set south-link min-one-of (satellites with [orbit-index = current-orbit - 1]) [distance current-satellite]
    ]
    if current-orbit < (num-orbits - 1) [

      set north-link min-one-of (satellites with [orbit-index = current-orbit + 1]) [distance current-satellite]
    ]

    ; Find diagonal links
    if north-link != nobody [
      set northeast-link [east-link] of north-link
      set northwest-link [west-link] of north-link
    ]
    if south-link != nobody [
      set southeast-link [east-link] of south-link
      set southwest-link [west-link] of south-link
    ]
  ]
end

to go
  clear-drawing
  draw-ground-station-coverage
  move-satellites
  calculate-distances-to-ground-stations
  maintain-spacing
  update-inter-satellite-links
  manage-events
  manage-power
  manage-tasks
  draw-satcom-coverage
  draw-sensor-rectangles
  if ticks mod (sensor-width / agent-speed) = 0 [
    print (word "Detecting events at tick " ticks)
    detect-events
  ]
  detect-gs-coverage
  update-colors
  update-satellite-visuals

  tick
end

to draw-event-direction-arrows
  ask satellites [
    foreach event-directions [ dir ->
      let arrow-end (list (xcor + sin dir) (ycor + cos dir))
      draw-line-to arrow-end red
    ]
  ]
end

to draw-line-to [end-point line-color]
  let start-xcor xcor
  let start-ycor ycor
  pen-up
  setxy item 0 end-point item 1 end-point
  set color line-color
  pen-down
  setxy start-xcor start-ycor
  pen-up
end

to calculate-distances-to-ground-stations
  ask satellites [
    let this-satellite self
    set distances-to-gs []
    let sat-x xcor
    let sat-y ycor
    ask ground-stations [
      let gs-x xcor
      let gs-y ycor
      let dx_ sat-x - gs-x
      let dy_ sat-y - gs-y
      let euclidean-dist sqrt (dx_ * dx_ + dy_ * dy_)
      ask this-satellite [
        set distances-to-gs lput (list myself euclidean-dist) distances-to-gs
      ]
    ]
    set distances-to-gs sort-by [[d1 d2] -> item 1 d1 < item 1 d2] distances-to-gs
    if not empty? distances-to-gs [
      set closest-gs item 0 first distances-to-gs
      set distance-to-closest-gs item 1 first distances-to-gs
    ]
  ]
end


; Move all satellites forward
to move-satellites
  ask satellites [
    fd agent-speed  ; move forward by the specified speed
    if xcor >= max-pxcor [  ; if a satellite reaches the right edge of the world
      setxy min-pxcor ycor  ; wrap it around to the left edge
    ]
  ]
end

; Draw sensor coverage rectangles around satellites
to draw-sensor-rectangles
  ask satellites [
    let original-color color
    let original-heading heading
    pen-up
    set color white

    ; Store the original position
    let center-x xcor
    let center-y ycor

    ; Calculate rectangle corners
    let half-length sensor-length / 2
    let half-width sensor-width / 2
    let x1 (center-x - half-width)
    let y1 (center-y + half-length)
    let x2 (center-x + half-width)
    let y2 (center-y - half-length)

    ; Draw the rectangle
    pen-down
    setxy x1 y1
    setxy x2 y1
    setxy x2 y2
    setxy x1 y2
    setxy x1 y1

    ; Return to the original position and reset properties
    pen-up
    setxy center-x center-y
    set color original-color
    set heading original-heading
  ]
end



; Draw satcom coverage circles around satellites
to draw-satcom-coverage
  ask satellites [
    let original-color color
    let original-heading heading
    pen-up
    set color red

    ; Store the original position
    let center-x xcor
    let center-y ycor

    ; Draw the circle
    pen-down
    repeat 360 [
      ; Calculate the next point on the circle
      let next-x (center-x + (satcom-coverage * sin heading))
      let next-y (center-y + (satcom-coverage * cos heading))
      ; Move to the next point
      setxy next-x next-y
      rt 1
    ]

    ; Return to the original position and reset properties
    pen-up
    setxy center-x center-y
    set color original-color
    set heading original-heading
  ]
end

to draw-ground-station-coverage
  ask ground-stations [
    let original-color color
    let original-heading 0
    pen-up
    set color red

    let center-x xcor
    let center-y ycor

    pen-down
    repeat 360 [
      let next-x (center-x + (coverage-area * sin heading))
      let next-y (center-y + (coverage-area * cos heading))
      setxy next-x next-y
      rt 1
    ]

    pen-up
    setxy center-x center-y
    set color original-color
    set heading original-heading
  ]
end

; Maintain proper spacing between satellites in each orbit
; had to write this function since satellites were not maintaining distance between each other
; after a certain point - either a float point precision error that accumulates or something
; to do with the world/patch size and wrap-around.
to maintain-spacing
  foreach (range num-orbits) [ i ->  ; for each orbit
    let orbit-satellites satellites with [orbit-index = i]  ; get all satellites in this orbit
    let ideal-spacing world-width / num-sats-per-orbit  ; Cclculate ideal space between satellites
    let first-satellite min-one-of orbit-satellites [xcor]  ; find the leftmost satellite
    let current-satellite first-satellite

    repeat (num-sats-per-orbit - 1) [  ; for each pair of adjacent satellites
      ; find the next satellite to the right (or wrap around to the leftmost)
      let next-satellite min-one-of (orbit-satellites with [xcor > [xcor] of current-satellite]) [xcor]
      if next-satellite = nobody [
        set next-satellite first-satellite
      ]

      ; calculate the actual distance between the satellites (considering world wrap-around)
      let actual-distance (([xcor] of next-satellite - [xcor] of current-satellite) + world-width) mod world-width
      let adjustment (ideal-spacing - actual-distance) / 2  ; calculate how much to adjust

      ; adjust the position of both satellites
      ask current-satellite [
        set xcor (xcor - adjustment + world-width) mod world-width
      ]
      ask next-satellite [
        set xcor (xcor + adjustment + world-width) mod world-width
      ]

      set current-satellite next-satellite  ; Move to the next pair of satellites
    ]
  ]
end


to manage-events
  ask patches with [event-intensity > 0] [
    if ticks >= event-end-time [
      set event-intensity event-intensity - (event-dissipation-rate / 2)
      if event-intensity <= 0 [
        set event-intensity 0
        set event-end-time 0
        set event-type 0
      ]
      update-patch-color
    ]
  ]

  if count patches with [event-intensity > 0] < max-events [
    if random-float 1 < 1 [
      create-event
    ]
  ]

  ask patches with [event-type > 0] [
    update-patch-color
  ]
end

to update-patch-color
  ifelse event-type > 0 [
    set pcolor scale-color (item (event-type - 1) event-colors) event-intensity 0 1
  ] [
    ifelse is-static-zone [
      set pcolor static-zone-color
    ] [
      set pcolor black  ; or whatever your default patch color is
    ]
  ]
end

; Create a new event at a random location
to create-event
  let event-center one-of patches with [event-end-time = 0]  ; choose a random empty patch
  if event-center != nobody [
    let event-size min-event-size + random max-event-size  ; random size within parameters
    let event-duration min-event-duration + random max-event-duration  ; random duration within parameters
    let new-event-type 1 + random length event-colors  ; randomly choose an event type
    ask event-center [
      set event-end-time ticks + event-duration
      set event-type new-event-type
      ; Set intensity for all patches within the event radius
      ask patches in-radius event-size [
        let distance-factor 1 - (distance myself / event-size)
        if distance-factor > event-intensity [
          set event-intensity distance-factor
          set event-end-time [event-end-time] of myself
          set event-type new-event-type
        ]
        update-patch-color  ; Update the patch color
      ]
    ]
  ]
end

to-report get-event-priority [evt-type]
  report item (evt-type - 1) event-priorities
end



to-report highest-priority-event [events]
  let max-priority 0
  let highest-priority-event-type 0
  foreach events [evt-type ->
    let priority get-event-priority evt-type
    if priority > max-priority [
      set max-priority priority
      set highest-priority-event-type evt-type
    ]
  ]
  report highest-priority-event-type
end


to-report patches-in-sensor-range [sat]
  let half-width [sensor-width] of sat / 2
  let half-length [sensor-length] of sat / 2
  report patches with [
    abs (pxcor - [xcor] of sat) <= half-width and
    abs (pycor - [ycor] of sat) <= half-length
  ]
end


to-report request-task-generation [task-type-input priority-input event-x event-y]
  report (list task-type-input priority-input event-x event-y)
end



to detect-events

  if ticks mod (sensor-width / agent-speed) != 0 [
    print (word "Warning: detect-events called unexpectedly at tick " ticks)
  ]

  let all-task-requests []  ; Initialize this outside the ask satellites block

  ask satellites [
    ; Reset detection variables
    set events-detected []
    set event-coverage-percentages []
    set event-centers []
    set event-distances []
    set event-directions []
    set interpreted-event-directions []
    set event-exposure-times []

    ; Get all patches within sensor coverage
;    let covered-patches patches in-radius sensor-coverage
;    set total-sensor-patches count covered-patches

    ;let covered-patches patches in-rectangle sensor-width sensor-length
    let covered-patches patches-in-sensor-range self
    set total-sensor-patches count covered-patches

    ; Detect events
    let detected-event-patches covered-patches with [event-intensity > 0]
    let event-types remove-duplicates [event-type] of detected-event-patches

    foreach event-types [ evt-type ->
      let type-patches detected-event-patches with [event-type = evt-type]
      let coverage-percentage (count type-patches / total-sensor-patches) * 100

      ; Calculate event center based on the chosen method
      let event-center nobody
      ifelse use-true-event-centers? [
        ; Calculate the true event center
        let all-event-patches patches with [event-type = evt-type]
        let total-intensity sum [event-intensity] of all-event-patches
        let weighted-x sum [pxcor * event-intensity] of all-event-patches
        let weighted-y sum [pycor * event-intensity] of all-event-patches
        let center-x weighted-x / total-intensity
        let center-y weighted-y / total-intensity
        set event-center patch center-x center-y
      ] [
        ; Estimate event center (existing logic)
        let center-x mean [pxcor] of type-patches
        let center-y mean [pycor] of type-patches
        set event-center patch center-x center-y
      ]

      ; Calculate distance and direction to event center
      let dist distance event-center
      let dir towards event-center
      let interpreted-dir interpret-direction dir

      ; Update event detection variables
      set events-detected lput evt-type events-detected
      set event-coverage-percentages lput coverage-percentage event-coverage-percentages
      set event-centers lput event-center event-centers
      set event-distances lput dist event-distances
      set event-directions lput dir event-directions
      set interpreted-event-directions lput interpreted-dir interpreted-event-directions
      set event-exposure-times lput 1 event-exposure-times
    ]

    ; Update event priority
    ifelse not empty? events-detected [
      set event-priority max (map get-event-priority events-detected)
    ] [
      set event-priority 0
    ]

    ; Update over-static-zone
    set over-static-zone ifelse-value (any? covered-patches with [is-static-zone = true]) [1] [0]


    let satellite-task-requests []  ; Initialize this for each satellite

    ; Generate task requests for each detected event
    foreach events-detected [ evt-type ->
      let event-center item (position evt-type events-detected) event-centers

      ; Request data processing task
      set satellite-task-requests lput (request-task-generation "dp" evt-type [pxcor] of event-center [pycor] of event-center) satellite-task-requests

      ; Request medium-power detection task
      set satellite-task-requests lput (request-task-generation "ai-mid" evt-type [pxcor] of event-center [pycor] of event-center) satellite-task-requests

      ; Request high-power detection task
      set satellite-task-requests lput (request-task-generation "ai-high" evt-type [pxcor] of event-center [pycor] of event-center) satellite-task-requests
    ]

    ; Add this satellite's requests to the overall list
    set all-task-requests (sentence all-task-requests satellite-task-requests)
  ]

  ; Generate tasks based on requests
  foreach all-task-requests [ request ->
    let task-type_ item 0 request
    let priority item 1 request
    let event-x item 2 request
    let event-y item 3 request

    generate-task task-type_ priority 0 0 0 0 event-x event-y
  ]

  ; Assign new tasks to satellites
  let new-tasks tasks with [assigned-to = nobody]
  ask new-tasks [
    let closest-satellite min-one-of satellites [distance myself]
    set assigned-to closest-satellite
    set status "in-progress"
    set origin-satellite closest-satellite
    set destination-satellite closest-satellite
    ask closest-satellite [
      set current-tasks lput myself current-tasks
    ]
  ]
end


to manage-tasks
  ask satellites [
    if not empty? current-tasks [
      let tasks-to-keep []
      foreach current-tasks [ task_ ->
        ; Check if the satellite has enough power for the task
        ifelse power >= [power-required] of task_ [
          ; Deduct power
          set power power - [power-required] of task_

          ; Increase progress
          ask task_ [
            set progress progress + 1
            if progress >= time-required [
              set status "completed"
            ]
          ]

          ; Keep the task if it's not completed
          if [status] of task_ != "completed" [
            set tasks-to-keep lput task_ tasks-to-keep
          ]
        ]
        [
          ; If not enough power, keep the task for later
          set tasks-to-keep lput task_ tasks-to-keep
        ]
      ]

      ; Update current-tasks with only the tasks to keep
      set current-tasks tasks-to-keep
    ]
  ]
end


; Helper function to interpret direction
to-report interpret-direction [direction]
  if direction >= 0 and direction < 22.5 or direction >= 337.5 and direction < 360 [
    report "North"
  ]
  if direction >= 22.5 and direction < 67.5 [
    report "Northeast"
  ]
  if direction >= 67.5 and direction < 112.5 [
    report "East"
  ]
  if direction >= 112.5 and direction < 157.5 [
    report "Southeast"
  ]
  if direction >= 157.5 and direction < 202.5 [
    report "South"
  ]
  if direction >= 202.5 and direction < 247.5 [
    report "Southwest"
  ]
  if direction >= 247.5 and direction < 292.5 [
    report "West"
  ]
  if direction >= 292.5 and direction < 337.5 [
    report "Northwest"
  ]
end



; procedure to detect ground station coverage
to detect-gs-coverage
  ask satellites [
    set gs-visibility 0
  ]
  ask ground-stations [
    set sat-visibility 0
  ]

  ask satellites [
    let this-satellite self
    ask ground-stations [
      if distance this-satellite <= (satcom-coverage + coverage-area) [
      ;if member? patch-here patches-in-sensor-range this-satellite or distance this-satellite <= coverage-area [

        ask this-satellite [
          set gs-visibility 1
        ]
        set sat-visibility 1
      ]
    ]
  ]
end



to update-colors
  let all-distances [distance-to-closest-gs] of satellites
  ask satellites [
    ifelse not empty? events-detected [
      ; Color based on the highest priority event detected
      let highest-priority-event-type highest-priority-event events-detected
      set color item (highest-priority-event-type - 1) event-colors
    ] [
      ifelse over-static-zone = 1 [
        set color static-zone-color
      ] [
        ifelse gs-visibility = 1 [
          set color red
        ] [
          if not empty? all-distances [
            let min-distance-to-gs min all-distances
            let max-distance-to-gs max all-distances
            set color scale-color red distance-to-closest-gs (-1 * (min-distance-to-gs) - 6) (max-distance-to-gs - 6)
          ]
        ]
      ]
    ]
    draw-satellite-with-outline
  ]

  ask ground-stations [
    ifelse sat-visibility = 1 [
      set color red
    ] [
      set color 9  ; Original color (light blue)
    ]
  ]
end

; New procedure to draw satellite with outline
to draw-satellite-with-outline
  let original-color color

  ; Draw the outline
  set color outline-color
  pen-up
  set size size + 0.5  ; Slightly larger for the outline
  stamp
  set size size - 0.5  ; Reset to original size

  ; Draw the main body
  set color original-color
  stamp
end

to update-satellite-visuals
  ask satellites [
    ; Remove old indicators
    foreach task-indicators [ indicator ->
      ask indicator [ die ]
    ]
    set task-indicators []

    ; Create new indicators for current tasks
    let num-tasks length current-tasks
    if num-tasks > 0 [
      let angle-step 360 / num-tasks
      let indicator-distance 2  ; Distance from satellite center

      let angle 0
      foreach current-tasks [ task_ ->
        let task-type_ [task-type] of task_
        hatch-coverage-circles 1 [
          set shape task-type-to-shape task-type_
          set color task-type-to-color task-type_
          set size 1
          set heading angle
          fd indicator-distance
          set hidden? false
          create-link-with myself [
            tie
            hide-link
          ]
          ask myself [ set task-indicators lput myself task-indicators ]
        ]
        set angle angle + angle-step
      ]
    ]
  ]
end

to-report task-type-to-shape [task-type_]
  report (
    ifelse-value
    task-type_ = "dp" [ "square" ]
    task-type_ = "ai-mid" [ "triangle" ]
    task-type_ = "ai-high" [ "star" ]
    [ "circle" ]  ; default
  )
end

to-report task-type-to-color [task-type_]
  report (
    ifelse-value
    task-type_ = "dp" [ yellow ]
    task-type_ = "ai-mid" [ yellow ]
    task-type_ = "ai-high" [ yellow ]
    [ white ]  ; default
  )
end

; New procedure to update inter-satellite links after movement
to update-inter-satellite-links
  ask satellites [
    let current-satellite self
    let current-orbit orbit-index

    ; Update east and west links
    set east-link min-one-of (other satellites with [orbit-index = current-orbit and xcor > [xcor] of current-satellite]) [xcor]
    set west-link max-one-of (other satellites with [orbit-index = current-orbit and xcor < [xcor] of current-satellite]) [xcor]

    ; Handle wrap-around for east and west
    if east-link = nobody [
      set east-link min-one-of (satellites with [orbit-index = current-orbit]) [xcor]
    ]
    if west-link = nobody [
      set west-link max-one-of (satellites with [orbit-index = current-orbit]) [xcor]
    ]

    ; Update north and south links
    if current-orbit > 0 [
     set south-link min-one-of (satellites with [orbit-index = current-orbit - 1]) [distance current-satellite]
    ]
    if current-orbit < (num-orbits - 1) [
      set north-link min-one-of (satellites with [orbit-index = current-orbit + 1]) [distance current-satellite]
    ]

    ; Update diagonal links
    if north-link != nobody [
      set northeast-link [east-link] of north-link
      set northwest-link [west-link] of north-link
    ]
    if south-link != nobody [
      set southeast-link [east-link] of south-link
      set southwest-link [west-link] of south-link
    ]
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
10
10
753
754
-1
-1
10.352113
1
10
1
1
1
0
1
1
1
-35
35
-35
35
1
1
1
ticks
30.0

BUTTON
1205
45
1280
78
NIL
setup\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1205
90
1268
123
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
990
130
1159
163
min-event-duration
min-event-duration
0
1000
880.0
10
1
NIL
HORIZONTAL

SLIDER
989
222
1119
255
max-events
max-events
0
5000
1210.0
10
1
NIL
HORIZONTAL

SLIDER
764
129
917
162
agent-speed
agent-speed
0.01
1
0.0125
0.01
1
NIL
HORIZONTAL

SLIDER
990
177
1140
210
max-event-duration
max-event-duration
0
1000
570.0
10
1
NIL
HORIZONTAL

SLIDER
766
38
918
71
num-orbits
num-orbits
0
10
7.0
1
1
NIL
HORIZONTAL

SLIDER
764
84
917
117
num-sats-per-orbit
num-sats-per-orbit
0
10
7.0
1
1
NIL
HORIZONTAL

SLIDER
991
37
1131
70
min-event-size
min-event-size
0
10
6.0
1
1
NIL
HORIZONTAL

SLIDER
991
82
1121
115
max-event-size
max-event-size
0
100
6.0
1
1
NIL
HORIZONTAL

SLIDER
764
174
917
207
sensor-coverage
sensor-coverage
0
10
2.4
0.1
1
NIL
HORIZONTAL

BUTTON
1205
135
1277
168
go once
go\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
989
271
1179
304
event-dissipation-rate
event-dissipation-rate
0
1
0.01
0.01
1
NIL
HORIZONTAL

SLIDER
760
510
935
543
num-ground-stations
num-ground-stations
0
10
2.0
1
1
NIL
HORIZONTAL

SLIDER
758
596
938
629
ground-station-coverage
ground-station-coverage
0
10
3.1
0.1
1
NIL
HORIZONTAL

SLIDER
764
299
918
332
satcom-coverage
satcom-coverage
0
10
3.1
0.1
1
NIL
HORIZONTAL

SWITCH
763
344
938
377
enable-north-south?
enable-north-south?
0
1
-1000

SWITCH
763
389
938
422
enable-east-west?
enable-east-west?
0
1
-1000

SWITCH
763
434
938
467
enable-diagonals?
enable-diagonals?
0
1
-1000

SWITCH
760
554
938
587
even-gs-distribution
even-gs-distribution
0
1
-1000

SLIDER
785
710
925
743
max-static-zones
max-static-zones
0
100
8.0
1
1
NIL
HORIZONTAL

SLIDER
770
665
940
698
max-static-zone-size
max-static-zone-size
0
100
19.0
1
1
NIL
HORIZONTAL

SWITCH
988
321
1192
354
use-true-event-centers?
use-true-event-centers?
1
1
-1000

INPUTBOX
975
390
1075
450
power-req-isl
0.0
1
0
Number

INPUTBOX
1085
390
1180
450
power-req-gs
0.0
1
0
Number

INPUTBOX
1300
390
1400
450
power-req-dp
0.01
1
0
Number

INPUTBOX
975
460
1075
520
power-req-ai-low
0.01
1
0
Number

INPUTBOX
1085
460
1180
520
power-req-ai-mid
0.02
1
0
Number

INPUTBOX
1190
460
1290
520
power-req-ai-high
0.04
1
0
Number

TEXTBOX
788
13
891
32
Satellite Setup
13
0.0
0

TEXTBOX
1021
12
1111
32
Events Setup\n
13
0.0
0

TEXTBOX
778
482
916
502
Ground Station Setup
13
0.0
0

TEXTBOX
803
639
920
659
Static Zone Setup
13
0.0
0

TEXTBOX
1085
365
1252
385
Power Requirements
13
0.0
0

INPUTBOX
1190
390
1290
450
power-req-rs
0.0
1
0
Number

INPUTBOX
1300
460
1400
520
power-regen-rate
0.1
1
0
Number

SLIDER
765
215
915
248
sensor-length
sensor-length
0
10
2.0
1
1
NIL
HORIZONTAL

SLIDER
765
255
915
288
sensor-width
sensor-width
0
10
6.0
1
1
NIL
HORIZONTAL

INPUTBOX
975
565
1075
625
time-req-isl
1.0
1
0
Number

INPUTBOX
1085
565
1180
625
time-req-gs
1.0
1
0
Number

INPUTBOX
1190
565
1290
625
time-req-rs
1.0
1
0
Number

INPUTBOX
1300
565
1400
625
time-req-dp
10.0
1
0
Number

INPUTBOX
975
635
1075
695
time-req-ai-low
1.0
1
0
Number

INPUTBOX
1085
635
1180
695
time-req-ai-mid
20.0
1
0
Number

INPUTBOX
1190
635
1290
695
time-req-ai-high
30.0
1
0
Number

TEXTBOX
1090
540
1240
558
Time Requirements
13
0.0
1

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
1
@#$#@#$#@
