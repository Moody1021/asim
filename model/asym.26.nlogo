extensions [profiler shell csv vid matrix]
breed [actors actor]
breed [cogs cog]
breed [gcogs gcog]
breed [forces force]
breed [groupies groupie]
breed [groups group]
breed [cus cu]
directed-link-breed [ldts ldt]
directed-link-breed [fvecs fvec]

forces-own [
  mag    ;; size of vector
]

actors-own [
  angle   ;; angle of vector. Calculated for each iteration.
  acount  ;; number of agents
  dcn     ;; distance to closest neighbor calculate for each iteration.
  dxc     ;; delta x coord change
  dyc     ;; delta y coord change
  fx     ;; x-component of force vector
  fy     ;; y-component of force vector
  gpd    ;; group polarization distance
  gmr    ;; group mass ratio
  gx     ;; gravitational metric for x
  gy     ;; grav metric for y
  mag     ;; size of vector. Calculated for each iteration.
  massP  ;; passive mass
  mass   ;; the actor's active mass
  mygroup ;; group number
  tribe   ;; which tribe I belong to
  xc     ;; real x-coordinate (in case agent leaves world). these are used for all calculations and then used to update xcor, ycor.
  xorig  ;; initial x coord
  yc     ;; real y-coordinate (in case agent leaves world)
  yorig  ;; initial y coord
]

groupies-own [
  angle   ;; angle of vector. Calculated for each iteration.
  acount ;; number of agents
  dcn     ;; distance to closest neighbor calculate for each iteration.
  dxc     ;; delta x coord change
  dyc     ;; delta y coord change
  fx     ;; x-component of force vector
  fy     ;; y-component of force vector
  gx     ;; gravitational metric for x
  gy     ;; grav metric for y
  mag     ;; size of vector. Calculated for each iteration.
  massP  ;; passive mass
  mass   ;; the actor's active mass
  mygroup ;; group number
  tribe   ;; my tribe
  xc     ;; real x-coordinate (in case agent leaves world). these are used for all calculations and then used to update xcor, ycor.
  xorig  ;; initial x coord
  yc     ;; real y-coordinate (in case agent leaves world)
  yorig  ;; initial y coord
]
groups-own [
  angle   ;; angle of vector. Calculated for each iteration.
  acount ;; number of agents
  dcn     ;; distance to closest neighbor calculate for each iteration.
  dxc     ;; delta x coord change
  dyc     ;; delta y coord change
  fx     ;; x-component of force vector
  fy     ;; y-component of force vector
  gpd    ;; group polarization distance
  gmr    ;; group mass ratio
  gx     ;; gravitational metric for x
  gy     ;; grav metric for y
  mag     ;; size of vector. Calculated for each iteration.
  massP  ;; passive mass
  mass   ;; the actor's active mass
  myactors ;; all the group members
  tribe   ;; my tribe
  tribemix ;; NOTE: this must be the same length as the tribecolors list global variable
  xc     ;; real x-coordinate (in case agent leaves world). these are used for all calculations and then used to update xcor, ycor.
  xorig  ;; initial x coord
  yc     ;; real y-coordinate (in case agent leaves world)
  yorig  ;; initial y coord
]

cogs-own [
  xorig   ;; original cog
  yorig   ;; original
]

patches-own [
  xgrad    ;; x force gradient
  ygrad    ;; y force gradient
]

globals
[ activeForIter     ;; actives for an iteration
  agentcount        ;; count of actors
  attractive        ;; total attractive force
  breed-change      ;; a breed change has occurred when true
  center-of-mass-xc ;; x-coordinate of the center of mass
  center-of-mass-yc ;; y-coordinate of the center of mass. Used to recenter all agents around the center of mass of the whole population.
  ctrmassv          ;; center of mass movement
  ctrmassvr         ;; center of mass movement in radians
  dcnsum            ;; sum of all distances to closest neighbors
  dcnsd             ;; standard deviation of distance to closest neighbor
  elapsed-time      ;; time elapsed is sum of dts
  euclidflag        ;; is space show euclidean or warped
  finaldt           ;; final dt at end of simulation
  floc              ;; list of final agent locations
  forcescale        ;; how much to scale force vector size
  groupcount        ;; number of groups
  grppol            ;; group polarization
  grprad            ;; group radicalization
  grpradm           ;; group rad moment
  grppolm           ;; group polarization moment
  itcounter         ;; used to iterate past probability based endpoint to make sure everything is in a final stop condition
  iterations        ;; number of simulation iterations
  masserror         ;; tracks if a mass less than zero is generated.
  massratio         ;; ratio of largest mass to totmass
  masscale          ;; sise of agent scaled by mass (.1 for scale 1)
  maxdist           ;; max distance traveled in an iteration
  maxforce          ;; max force on each iteration. Used to determine when to stop the simulation - when maxforce < min force cutoff
  meandistclosestneighbor   ;; mean distance to closest neighbor
  meandist          ;; mean distance between all agents
  meandistorigin    ;; eman distance to the origin
  meanxloc          ;; mass weighted x location for all agents
  meanxsd           ;; mass weighted x location standard deviation for all agents
  meanyloc
  meanysd
  mergeCount        ;; number of mergers in an iteration
  mergeTotMass      ;; merge mass for each iteration
  mominertia        ;; moment of inertia
  oobcount          ;; count of agents that fall outside universe during setup
  nspv              ;; normalized system polarization
  nspvr             ;; normalized system polarization in r, theta mode
  pax               ;; a parameter for x coord for social pressure
  pay               ;; a parameter for y coord for social pressure
  pbx               ;; b parameter for x coord for social pressure
  pby               ;; b parameter for y coord for social pressure
  peakdist          ;; peak dist for an iteration. When dynamic dt is on: the max dist traveled is =< than the coalescence radius.
  phasecount        ;; how many iterations have happened during phase transition
  ;phaseiteration    ;; how many iterations to skip grouping check during a phase transition
  phaseneg          ;; g < 0
  phasepos          ;; g is >0
  phasetrans        ;; system  in phase transition mode

  radicalization    ;; metric to measure development of extremism
  radmax            ;; max radicalization metric
  record-interface  ;; make movie of interface
  record-view       ;; make movie of view
  repulsive         ;; total repulsive force across all agents for each iteration
  rseedused         ;; random seed used for the run
  scale             ;; scale multiplier factor from a -1,1 world.
  stopflag          ;; when set, stop simulation
  stopreason        ;; why the simulation was stopped
  timeend
  timestart         ;; start and end times for a run
  tribecolors       ;; colors for each tribe
  tribecolorname    ;; name of color. Must besame size as tribecolors
  tribecount        ;; number of tribes at the end of simulation
  tribenum          ;; number of tribes in simulation o = 1
  tribefinalmass    ;; final mass of each tribe - must be same size as tribecolors
  tribemassmean     ;; active mass mean for tribe for behavior space experiments only
  tribefinalmassP   ;; must be same size as tribecolors
  tribeinitmass     ;; initial mass of each tribe must be same size as tribecolors
  tribeinitmassP    ;; must be same size as tribecolors
  tribemassratio    ;; given a mass-mean, what is the ratio from the most to the least tribe mass
  tribematrix       ;; n x n matrix of tribe interaction for n tribes

  tglobal           ;; temporary global used for behavior space variations with tribes
  tmat11            ;; these vars are only for changing tribe interaction matrix values for experiments
  tmat12
  tmat13
  tmat14
  tmat21
  tmat22
  tmat23
  tmat24
  tmat31
  tmat32
  tmat33
  tmat34
  tmat41
  tmat42
  tmat43
  tmat44
  totmass           ;; total mass of agents used for debugging only
  valchg            ;; maximum value change ratio
  valchgmax         ;; max value chg distance
  valchgmin
]


; Basic Model Design:
;   Setup initializes the model for a simulation run.
;     The spatial and mass distributions selected for the agents are set up
;     All necessary globals are initialized
;     Note: randomseed is initialized for repeatable runs


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;
to setup
;  if behaviorspace-run-number = 0 [
;    clear-all
;  ]
  clear-all
  set tribecolors [15 105 65 25 45 125]
  set tribecolorname ["red" "blue" "green" "orange" "yellow" "magenta"]
  set tribecount 0
  set tribenum 0
  set tribeinitmass [0 0 0 0 0 0]   ; NOTE: these lists need to have the same number of elements as tribecolors list
  set tribefinalmass [0 0 0 0 0 0]
  set tribeinitmassP [0 0 0 0 0 0]
  set tribefinalmassP [0 0 0 0 0 0]
  set tribemassmean 5000
  set tribematrix matrix:make-identity 2
  ;;set tribemassratio .2
  set grppol []
  set grppolm []
  set grprad []
  set grpradm []
  set ctrmassv []
  set ctrmassvr []
  set floc []
  set nspv [0 0]
  set nspvr [0 0]
  set timestart read-from-string remove "\n" (shell:exec "date" "+%s")
  set itcounter 0
  set oobcount 0
  set forcescale 40
  set scale max-pxcor
  set stopflag false
  set masscale .02
  set peakdist 0
  set dt 1
  set elapsed-time 0
  set iterations 0
  set masserror 0
  set phaseneg false
  set phasepos true
  set phasetrans false
  set-default-shape actors "circle"
  set record-view False
  set record-interface False
  set euclidflag False
  ;; social pressure init
  set pax fxcenter * 10 / ( fxwidth / 2 )
  set pbx 10 / ( fxwidth / 2 )
  set pay fycenter * 10 / ( fywidth / 2 )
  set pby 10 / ( fywidth / 2 )
  setgradients

  ifelse randomseed != 0 [
    random-seed randomseed
    set rseedused randomseed
  ] [
   set rseedused new-seed
   random-seed rseedused
  ]
  set totmass 0
  create-actors number [
    initagent
  ]
  ask patches [
    set pcolor white
  ]

  if Geometry [
    doGeometry
  ]

  inject
  if number != 0 [
    meanlocs
    set tribenum tribenum + 1
  ]
  ; center of gravity display
  create-cogs 1 [
    set color 15
    set pen-size 3
  ]
  cogset
  ;; create identifier agent for center of universe
  create-cus 1 [
    set xcor 0
    set ycor 0
    set color yellow
    set shape "target"
    set size 3
  ]
  if xOrigin != 0 or yOrigin != 0 [
    ask actors [
      pen-up
      ifelse (xcor + xOrigin) > max-pxcor [
        set xcor max-pxcor - 1
      ][
        set xcor xcor + xOrigin
      ]
      ifelse (ycor + yOrigin) > max-pycor [
        set ycor max-pycor - 1
      ][
        set ycor ycor + yOrigin
      ]
      pen-down
    ]
  ]

  reset-ticks
end

to setmore
  ;;user-message (word "n: " number " xMean  " xMean " yMean " yMean)
  create-actors number [
    initagent
  ]
  if tribes [
    let tnum tribenum + 1
    set tribematrix matrix:make-identity tnum
    if tmat11 != 0 [
      let tl (list (list tmat11 tmat12) (list tmat21 tmat22))
      set tribematrix matrix:from-row-list tl
    ]
    inject
  ]
  meanlocs
  ; center of gravity display
  cogset

  set tribenum tribenum + 1
  setup-plots
  initmeasure
end

to cogset
  ask cogs [
    pen-up
    set xcor meanxloc
    set ycor meanyloc
    set xorig meanxloc
    set yorig meanyloc
    pen-down
  ]
end

to inject
  if NumberInjected > 0 [
    create-actors NumberInjected [
      initagent
      if injMassx != 0 or injMassy != 0 [
        pen-up
        coord-set injMassx injMassy
        pen-down
      ]
      set totmass totmass + InjectionMaxMass - mass
      set mass InjectionMaxMass
      set massP InjectionMaxMass

      set size masstosize mass
    ]
  ]
end

to setmatrix [ ml ]
  set tribematrix matrix:from-row-list ml
end

to inittemps
  set tglobal 0
  set tmat11 0
  set tmat12 0
  set tmat13 0
  set tmat14 0
  set tmat21 0
  set tmat22 0
  set tmat23 0
  set tmat24 0
  set tmat31 0
  set tmat32 0
  set tmat33 0
  set tmat34 0
  set tmat41 0
  set tmat42 0
  set tmat43 0
  set tmat44 0
end

to space-set
  if xSpatialDist = "Random" [
     set xc random-float (2 * scale) - scale
  ]
  if ySpatialDist = "Random" [
     set yc random-float (2 * scale) - scale
  ]
  if xSpatialDist = "Normal" [
    ;set xc random-normal-in-bounds xMean (xStd) (- scale) scale
    set xc random-normalu xMean (xStd)
  ]
  if ySpatialDist = "Normal" [
    ;set yc random-normal-in-bounds yMean (yStd) (- scale) scale
    set yc random-normalu yMean (yStd)
  ]
  if xSpatialDist = "Exponential" [
     set xc random-exponentialu xMean
  ]
  if ySpatialDist = "Exponential" [
     set yc random-exponentialu yMean
  ]
  ifelse ( abs xc ) > max-pxcor [
    set shape "ghost"
    set oobcount oobcount + 1
    ifelse xc < 0 [ set xcor min-pxcor ]
    [ set xcor max-pxcor ]
  ][
    set xcor xc
  ]
  ifelse ( abs yc ) > max-pycor [
    set shape "ghost"
    set oobcount oobcount + 1
    ifelse yc < 0 [ set ycor min-pycor ]
    [ set ycor max-pycor ]
  ][
    set ycor yc
  ]
  if ShowForces [
    hatch-forces 1 [
      create-fvec-from myself [
        set thickness 1
        set color red
      ]
      hide-turtle
    ]
  ]
end

to mass-set
  let locmass 0
  let locmassp 0
  if active-mass-distribution = "Random"
    [ set locmass ( random-float maxmass ) ]
  if active-mass-distribution = "Normal"
    [ set locmass random-normalu mass-mean mass-stddev ]
  if active-mass-distribution = "Exponential"
    [ set locmass random-exponentialu (mass-mean )]
  if passive-mass-distribution = "Random"
    [ set locmassp ( random-float maxmass ) ]
  if passive-mass-distribution = "Normal"
    [ set locmassp random-normalu pmass-mean pmass-stddev ]
  if passive-mass-distribution = "Exponential"
    [ set locmassp random-exponentialu pmass-mean ]
  ifelse PassiveMass [
    set mass locmass
    set massP locmassp
    if locmassp < 0 [
      set massP pmass-mean
    ]
  ][
    set mass locmass
    set massP locmass
  ]
  if mass < 0 [
    ;;user-message (word "who " who " mass " mass " mass-mea " mass-mean " breed " breed " rseedused " rseedused " yStd " yStd " yMean " yMean " xStd " xStd) ]
    set mass mass-mean
    set massP mass-mean
    set masserror masserror + 1
  ]
  set size masstosize mass
end

to initagent
  ifelse tribes [
    set color item tribenum tribecolors
  ][
    set color (  random 9  ) + (( random  14 ) * 10)
  ]
  set mygroup nobody
  space-set
  mass-set
  set tribe tribenum
  set totmass totmass + mass
  set acount 1
  set xorig xc
  set yorig yc
  set gx agx
  set gy agy
  if track [ pen-down ]
end


to create-agent
  if mouse-down?
  [ let mx mouse-xcor
    let my mouse-ycor
    let mz 0
    if (not any? actors-on patch mx my )
    [
      create-actors 1 [
        initagent
        set xc mx ;initial-position-x
        set yc my ;initial-position-y
        setxy xc yc
        set mass initial-mass
        set massp initial-mass
        set size masstosize mass
        set totmass totmass + mass
      ]
      display
    ]
  ]
  while [mouse-down?]
  []
end

; convert mass to size parameter for displaying agents proportional to their size.
; Note: this is done on a log scale
to-report masstosize [m]
  report scale * masscale * log m 10
end

; generate a random distribution within the min max
; parameters given with the given mean and standard deviation
to-report random-normalu [ mid dev ]
  report random-normal mid dev
end

; generate an exponential distribution within the min / max
; range given with the given mean.
to-report random-exponentialu [ distmean ]
  report random-exponential distmean
end

; generate an exponential distribution within the min / max
; range given with the given mean.
to-report random-exponential-in-bounds [ distmean mmin mmax ]
  let result random-exponential distmean
  if result < mmin or result > mmax [
    set oobcount oobcount + 1
    report random-exponential-in-bounds distmean mmin mmax
  ]
  report result
end

;; setup social pressure feature gradients
to setgradients
  if temp [
    let xmax fycenter + ywidth / 2 ; calculate the y range for force field
    let xmin fycenter + ( - ywidth / 2 ) ; the force formula takes care of the x range
    let ymax fxcenter + xwidth / 2  ; calculate x range for force field
    let ymin fxcenter + ( - xwidth / 2 ) ; the force formula takes care of the y range
    if gradtype = "linear" [
      ask patches [
        if ( pxcor < (( fxwidth / 2 ) + fxcenter )) and ( pxcor > (( fxcenter -  fxwidth / 2 ) ))
          and pycor < xmax and pycor > xmin [
          set xgrad Kx
        ]
        if ( pycor < (( fywidth / 2 ) + fycenter )) and ( pycor > (( fycenter -  fywidth / 2 ) ))
          and pxcor < ymax and pxcor > ymin [
          set ygrad Ky
        ]
      ]
    ]
    if gradtype = "expo" [

      ask patches [
        set xgrad 0
        set ygrad 0
        if pycor > xmin and pycor < xmax [
          if abs ( pax - pbx * pxcor ) < 10 [
            set xgrad Kx * pbx * exp ( pax - ( pbx * pxcor ) ) / (( 1 + exp ( pax - ( pbx * pxcor )) ^ 2))
          ]
        ]
        if pxcor > ymin and pxcor < ymax [
          if abs ( pay - pby * pycor ) < 10  [
            set ygrad Ky * pby * exp ( pay - ( pby * pycor ) ) / (( 1 + exp ( pay - ( pby * pycor )) ^ 2))
          ]
        ]
      ]
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Runtime Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

; Runtime Design
;  There are several controls that determine what happens at runtime:
;    tickstop - set to a nonzero value, will stop the simulation when the number of iterations selected is reached
;    record-view or record-interface - start a movie creation for the run
;    AIB and MaxDT settings can both stop a simulation if those  values are reached. If this happens, a stopflag and
;      stopreason is set to indicate the end of the simulation. The go function checks this flag at the end of each iteration
;      and stops the simulation.
;
;  NOTE: the coordinates used for the simulation are defined agent variables xc, xy. The netlogo agent coordinates
;        are used to set xc, xy at the beginning of each iteration. Then xc, xy are used for all the movement calculations.
;        Agent xcor, ycor are set by function adjust-position after all the force and movement calculations are done.
;        This is done near the end of each iteration before plots are updated.
;  NOTE; for clarity of design and modularity, the ask actors functions is invoked for collision / merger checks,
;        force calculations, max movement calculations (for stopping conditions and dynamic dt setting),
;
to go
  ;; Check for stop conditions
  set stopflag false
  if tickstop != 0 [
    if iterations > tickstop [
      simdone "ItStop"
      stop
    ]
  ]
  if AgentStop != 0 [
    if AgentStop >= count actors [
      simdone "AgentCount"
      stop
    ]
  ]
  if DTStop != 0 and dt >= DTStop [
    set finaldt dt
    simdone "DTStop"
    stop
  ]
  ;; feature to record simulation into a movie
  if record-view or record-interface [
    if vid:recorder-status = "inactive" [
      vid:start-recorder
    ]
    movie-init
  ]
  ;show (word "On Iteration: " iterations " g " g " phaseneg " phaseneg)
  if gravitycycle [
    set g gcycle
  ]
  set attractive 0
  set repulsive 0
  set mergeCount 0
  set mergeTotMass 0
  set breed-change false
  set maxforce 0
  set maxdist 0
  set peakdist 0
  initactives
  ;; must do all of these steps separately to get correct results
  ;; since all actors interact with one another
  ifelse phasetrans [
    set phasecount phasecount + 1
    if phasecount > phaseiteration [
      set phasecount 0
      set phasetrans false
    ]
  ] [
    collision-check
  ]
  measure  ;; collect measurements
  force-set

  if peakdist = 0 [
    ifelse Infprob != 100 or ProbDist != "None" [
      set itcounter itcounter + 1
      if itcounter > 5000 [
        set stopflag true
        set stopreason "No movement"
        update-plots
      ]
      set peakdist 1
    ] [
        set stopflag true
        set stopreason "No movement"
        update-plots
    ]
  ]
  if stopflag [
    simdone stopreason
    stop
  ]
  if AdaptiveDT [
    set dt sqrt ( CoalescenseRadius / peakdist )
  ]

  ask groups [ update-position ]
  ask actors [ update-position ]
  ask groupies [ group-follow ]
  update-plots
  set elapsed-time elapsed-time + dt
  set iterations iterations + 1
  tick-advance dt
  set radicalization radmetric
end

to simdone [ reason ]
  set timeend read-from-string remove "\n" (shell:exec "date" "+%s")
  set stopreason reason
  movie
  finalmeasure
end

to initactives
  ask actors [
    set fx 0
    set fy 0
    set label int mass
    set label-color black
  ]
  ask groups [
    set fx 0
    set fy 0
    set label int mass
    set label-color black
  ]
end

to collision-check
  set activeForIter actives
  ask activeForIter [
    my-collisions
  ]
  if breed-change = true [
     ask actors [
       if mygroup != nobody [
         set breed groupies
       ]
     ]
     set breed-change false
   ]
end

; if any agents are within coalescence radius, merge into one agent
to my-collisions
  ask other activeForIter [
     if realdist self myself <= CoalescenseRadius [
      ;show (word "a " [who] of myself " " [breed] of myself " self " who " " breed)
      grouping self myself
    ]
  ]
end

to grouping [ a b ]
  ifelse [breed] of a = groups and [breed] of b = groups [
    mergegroup a b
  ][
    ifelse [breed] of a = groups or [breed] of b = groups [
      if [breed] of a = groups [
        joingroup a b
      ]
      if [breed] of b = groups [
        joingroup b a
      ]
    ][
      ifelse ([mygroup] of a = nobody) and  ([mygroup] of b = nobody) [
        makegroup a b
      ][
        if ((not [hidden?] of a) or (not [hidden?] of b)) [
          if not ([hidden?] of a) [
            joingroup [mygroup] of b a
          ]
          if not ([hidden?] of b) [
            joingroup [mygroup] of a b
          ]
        ]
      ]
    ]
  ]
end

to force-set
  let av actives
  ask av [
    forces-on-me other av with [ (realdist self myself) <= AIB ]
    update-socialforce
  ]
  ask av [ peakdist-calc ]
  if ShowForces [
    show-force
  ]
end

to update-socialforce
  if temp [
    if debug [
      show (word "socF fx bef " fx " fy bef " fy)
    ]
    set fx fx + txgrad xc yc
    set fy fy + tygrad xc yc
    if debug [
      show (word "socF fx aft " fx " fy aft " fy)
    ]
  ]
end

to-report fcalcx [a b]
  let ab adotb b a
  let dist realdist b a
  set ab forceadjustment ab
  ifelse tribes [
    ;;show (word "who it dist fxt fxy" "me " who " it " b " dist "dist " --- "b " --- " fyt)
    report (matrix:get tribematrix [tribe] of b  [tribe] of a) * g * ([gx] of b) * ( (( [xc] of b - [xc] of a ) / dist) *  ( [mass] of b ) * ab ) / ( (dist ^ DistanceExponent)  )

  ][
    report g * ([gx] of b) * ( (( [xc] of b - [xc] of a ) / dist) *  ( [mass] of b ) * ab ) / ( (dist ^ DistanceExponent)  )
  ]
end

to-report fcalcy [a b]
  ;show (word "a " [who] of a " B " [who] of b)
  let ab adotb b a
  let dist realdist b a
  set ab forceadjustment ab
  ifelse tribes [
    ;;show (word "who it dist fxt fxy" "me " who " it " it " dist "dist " --- " fxt " --- " fyt)
    report (matrix:get tribematrix [tribe] of b  [tribe] of a) * g * ([gy] of b) * ( (( [yc] of b - [yc] of a ) / dist) *  ( [mass] of b ) * ab ) / ( (dist ^ DistanceExponent)  )
  ][
    report g * ([gy] of b) * ( (( [yc] of b - [yc] of a ) / dist) *  ( [mass] of b ) * ab ) / ( (dist ^ DistanceExponent)  )
  ]
end

to forces-on-me [inrange]
  set fx sum map [ a -> fcalcx self a] [self] of inrange
  set fy sum map [ a -> fcalcy self a] [self] of inrange
end

to-report forceadjustment [ab]
  if forceAdj != "Normal" [
      ifelse forceAdj = "AttractOnly" [
        set ab abs ab
      ][ ifelse forceAdj = "Repulse" [
        set ab ( - abs ab )
      ][ ifelse forceAdj = "NoRepulse" [
            if ab < 0 [
              set ab 0
            ]
          ][ ifelse forceAdj = "NoAttract" [
              if ab > 0 [
                set ab 0
              ]
            ][
              set ab ( - ab )
            ]
    ]]]]
  report ab
end

; The force calculations are done. 'it' is the asking agent.
; Note: The force adjustment control determines the force direction.
to sum-its-force-on-me [it] ;; Turtle Procedure
  let dist realdist it self
  let ab adotb it self
  let fxt 0
  let fyt 0
  if forceAdj != "Normal" [
    ifelse forceAdj = "AttractOnly" [
      set ab abs ab
    ][ ifelse forceAdj = "Repulse" [
      set ab ( - abs ab )
    ][ ifelse forceAdj = "NoRepulse" [
          if ab < 0 [
            set ab 0
          ]
        ][ ifelse forceAdj = "NoAttract" [
            if ab > 0 [
              set ab 0
            ]
          ][
            set ab ( - ab )
          ]
  ]]]]
  ifelse tribes [
    set fxt (matrix:get tribematrix [tribe] of it  tribe) * g * ([gx] of it) * ( (( [xc] of it - xc ) / dist) *  ( [mass] of it ) * ab ) / ( (dist ^ DistanceExponent)  )
    set fyt (matrix:get tribematrix [tribe] of it  tribe) * g * ([gy] of it) * ( (( [yc] of it - yc ) / dist) *  ( [mass] of it ) * ab ) / ( (dist ^ DistanceExponent)  )
    show (word "who it dist fxt fxy" "me " who " it " it " dist "dist " --- " fxt " --- " fyt)
  ][
    set fxt g * ([gx] of it) * ( (( [xc] of it - xc ) / dist) *  ( [mass] of it ) * ab ) / ( (dist ^ DistanceExponent)  )
    set fyt g * ([gy] of it) * ( (( [yc] of it - yc ) / dist) *  ( [mass] of it ) * ab ) / ( (dist ^ DistanceExponent)  )
  ]
  ;
  set fx fx + fxt
  set fy fy + fyt
  ifelse ab >= 0 [
    set attractive attractive + sqrt ( fxt ^ 2 + fyt ^ 2)
  ] [
    set repulsive repulsive + sqrt ( fxt ^ 2 + fyt ^ 2)
  ]
;  if debug [
;    show (word "a " [who] of myself " " [breed] of myself " self " who " " breed " fx fy " fx fy)
;  ]
   ;;show (word "who " who " it= " it " adotb " ab  " fx " fx " fy " fy )
end

to free-groupies
  ask groupies [
    ;show (word "fg: " count groupies)
    pen-up
    set breed actors
    set mygroup nobody
    let tx xc
    let ty yc
    if (abs xc) > max-pxcor[
      ifelse xc > max-pycor [
        set tx max-pycor - .1
      ][
        set tx min-pycor + .1
      ]
    ]
    if (abs yc) > max-pycor [
      ifelse yc > max-pycor [
        set ty max-pycor - .1
      ][
        set ty min-pycor + .1
      ]
    ]
    setxy tx ty
    show-turtle
    pen-down
  ]
end

to group-extinction
  ask groups [
    die
  ]
end

to check-group-departure
  let membership False
  if mygroup != nobody [
    ask other [myactors] of mygroup [
      if distance myself <= CoalescenseRadius [
        set membership True
      ]
    ]
    if not membership [
      let myexbuddies other [myactors] of mygroup
      ask mygroup [
        set myactors myexbuddies
      ]
    ]
  ]
end

to update-group
  let mytotmass 0
  let myxloc 0
  let myyloc 0
  ask myactors [
    set myxloc myxloc + ( xc * mass )
    set myyloc myyloc + ( yc + mass )
    set mytotmass mytotmass + mass
  ]
  set xc myxloc / mytotmass
  set yc myyloc / mytotmass
  setxy xc yc
end


; Make a group two agents into one.
; Note: the position of the merged agent is the position of the agent with the larger mass.
;       the color of the merged agent is the color of the agent with the larger mass.
to makegroup [ a b ] ;; a merges with with b
  ;show (word "makegroup a " [who] of a " " [breed] of a " b " [who] of b " " [breed] of b)
  ;show (word "Caller who: " who)
  set breed-change true
  hatch-groups 1 [
    set myactors (turtle-set a b)
    set mass ([mass] of a) + ([mass] of b)
    set massP ([massP] of a) + ([massP] of b)
    set acount ([acount] of a) + ([acount] of b)
    ifelse [mass] of a > [mass] of b [
      coord-set [xc] of a [yc] of a
    ][
      coord-set [xc] of b [yc] of b
    ]
    if track [ pen-down ]

    ask a [
      set mygroup myself
      hide-turtle
    ]
    ask b [
      set mygroup myself
      hide-turtle
    ]
    set shape "star"
    set size masstosize mass
    if tribes [
      set tribemix [0 0 0 0 0 0]
      tribecolor-set a b
      tribejoin a
      tribejoin b
    ]
    ;show (word "mg: a " [who] of a " " [breed] of a " b " [who] of b " " [breed] of b " who of grp " who)
  ]
  ;show (word "makegroup a " [who] of a " " [breed] of a " b " [who] of b " " [breed] of b " GROUP " [mygroup] of a)
end

; Set mass contribution of this agent to group
to tribejoin [a]
  let tcp (position ([color] of a) tribecolors)
  let tm item tcp tribemix
  set tm tm + [mass] of a
  set tribemix replace-item tcp  tribemix tm
  ;show (word "makegroup a " [who] of a " color " [color] of a " tcp " tcp " mass " [mass] of a " tribemix " tribemix)
end

; a is the group, b is the actor
to joingroup [a b]
;  if [breed] of a != groups [
;    user-message (word "jg: rseedused " rseedused " a " [who] of a " " [breed] of a " b " [who] of b " " [breed] of b)
;  ]
  if not [hidden?] of b [
    ask a [
      set mass mass + [mass] of b
      set massP massP + [massP] of b
      set acount acount + [acount] of b
      set myactors (turtle-set myactors b)
      set size masstosize mass
      if tribes [
        tribecolor-set a b
        tribejoin b
      ]
    ]
    set breed-change true

    ask b [
      set mygroup a
      hide-turtle
    ]
  ]
end

to mergegroup [a b]
;  if ([breed] of a != groups) or  ([breed] of b != groups) [
;    user-message (word "jg: rseedused " rseedused " a " [who] of a " " [breed] of a " b " [who] of b " " [breed] of b)
;  ]
  ask [myactors] of a [
    set mygroup b
  ]
  ask b [
    if [mass] of a > mass [
      coord-set [xc] of a [yc] of a
      set tribe [tribe] of a
    ]
    set mass mass + [mass] of a
    set massP massP + [massP] of a
    set acount acount + [acount] of a
    set size masstosize mass
    set myactors (turtle-set myactors [myactors] of a)
    if tribes [
      tribecolor-set a b
      tribejoin a
    ]
  ]
  die
end

to tribecolor-set [a b]
  ifelse ([mass] of a) > ([mass] of b) [
    set color item ([tribe] of a) tribecolors
    set tribe [tribe] of a
  ][
    set color item ([tribe] of b) tribecolors
    set tribe [tribe] of b
  ]
end

to coord-set [x y]
  set xc x
  set yc y
  ifelse ( abs x ) > max-pxcor or ( abs y ) > max-pycor [
    set shape "ghost"
  ][
    setxy x y
  ]

end

to-report actives
  report (turtle-set actors groups)
end

; The position is update for the agent depending on the forces on the agent.
; Note: the force is divided by the mass of the agent being updated. The mass is considered to be
;       its passive mass / inertia to the movement.
to update-position ;; Turtle Procedure
  ;; As our system is closed, we can safely recenter the center of mass to the origin.

  set dxc fx * ( dt ^ 2) / massP
  set dyc fy * ( dt ^ 2) / massP

  let dist sqrt ( dxc ^ 2 + dyc ^ 2)
  if dist > maxdist [
    set maxdist dist
  ]
  let newxc (xc + dxc)
  let newyc (yc + dyc)
  if abs(newxc) > max-pxcor [
    set shape "ghost"
  ]
  if abs(newyc) > max-pycor [
    set shape "ghost"
  ]
  set xc newxc
  set yc newyc
;;  if debug [
;;    if newxc < min-pxcor or newxc > max-pxcor or newyc < min-pycor or newyc > max-pycor [
;;      user-message (word "up who " who " dxc dxy " dxc " " dyc " newxc newrxy " xc " " yc " xcor ycor " xcor " " ycor)
;;    ]
;;  ]
  if debug [
    show (word  "maxdist " maxdist " deltat " dt)
  ]
  adjust-position
end

; set agent coordinates from coordinate variables used to do all the force calculations.
to adjust-position ;; Turtle Procedure
  ifelse shape != "ghost" [
    setxy xc yc
  ] [
    if (abs(xc) < max-pxcor) and (abs(yc) < max-pycor) [
      if breed = actors [
        set shape "circle"
      ]
      if breed = groups [
        set shape "star"
      ]
      setxy xc yc
    ]
  ]
end

to-report txgrad [ x y ] ; x y coord
  if patch x y = nobody [
    report 0
  ]
  report [xgrad] of patch x y
end

to-report tygrad [ x y ]
  if patch x y = nobody [
    report 0
  ]
  report [ygrad] of patch x y
end

to heatmap
  let mf max [xgrad] of patches + max [ygrad] of patches
  let pf 0
  ask patches [
    set pf xgrad + ygrad
    if pf != 0 [
      set pcolor heatval mf pf
    ]
  ]
end

to-report heatval [ m v ] ; m is max and v is value for heat
  let iv 0
  let hg m / 2
  let lc 0
  let ac 8
  while [ lc < 4 ] [
    if v >= hg [
      set iv iv + ac
      set v v - hg
      set hg m - v
    ]
    set hg hg / 2
    set ac ac / 2
    set lc lc + 1
  ]
  report item iv [ 135 138 137 136 126 116 106 96 86 76 66 56 46 36 26 16 ]
end

to group-follow
  set xc xc + [dxc] of mygroup
  set yc yc + [dyc] of mygroup
end

to show-force
  if maxforce != 0 [
      let poshift forcescale / maxforce
      let tx 0
      let ty 0
      ask actors [
        ;;user-message (word "maxforce poshift who " maxforce " " poshift " " who " xcor ycor " xcor " " ycor )

        ask out-fvec-neighbors [
          ;;user-message (word "fvecnbr" who " xcor ycor " xcor " " ycor "myself xcor ycor " [xcor] of myself " " [ycor] of myself " who of myself " [who] of myself)
            set tx ( [xcor] of myself ) + ( [fx] of myself ) * poshift
            set ty ( [ycor] of myself ) + ( [fy] of myself ) * poshift
            ifelse ( abs tx ) < max-pxcor [
              set xcor tx
            ] [
              ifelse xcor < 0 [
                set xcor min-pxcor
              ] [
                set xcor max-pxcor
              ]
            ]
            ifelse ( abs ty ) < max-pycor [
              set ycor ty
            ] [
              ifelse xcor < 0 [
                set ycor min-pycor
              ] [
                set ycor max-pycor
              ]
            ]
        ]
      ]
    ]
    let showflag 0
    ask fvecs [
      set showflag 0
      ask both-ends [
        if hidden? [ set showflag showflag + 1 ]
      ]
      ifelse showflag < 2 [  show-link ]
      [ hide-link ]
    ]
end

; calculate the a dot b value of two agents
to-report adotb [a b]
   if infprob < random 100 [
;    probdebug "infprob" a b
    report 0
  ]
  if ProbDist != "None" [
    if ProbDist = "AttributeDiff" [
      let cosval abs (( ([xc] of a * [xc] of b) + [yc] of a * [yc] of b )/( vecmag a * vecmag b))
      if cosval < random-float 1 [
;        probdebug "AttrDiff" a b
        report 0
      ]
    ]
    if ProbDist = "DistanceProb" [
      let dist realdist a b
      let prob abs (AIB - dist) / AIB
      if prob < random-float 1 [
 ;       probdebug "ProbDist" a b
        report 0
      ]
    ]
  ]
  report ( ([gx] of a * [gx] of b * [xc] of a * [xc] of b) + [gy] of a * [gy] of b * [yc] of a * [yc] of b )
end


; How who is out of the loop for this iteration
to probdebug [ r a b ]
  if count actors <= 3 [
    print ( word r " Zero FaOnb for iteration " iterations " a " [who] of a " b " [who] of b)
  ]
end


; calculate distance between two agents
to-report realdist [a b]
  report sqrt (((([gx] of a ) * ([gx] of b)) * ( [xc] of a - [xc] of b) ^ 2) + ((([gy] of a) * ([gy] of b)) * ([yc] of a - [yc] of b) ^ 2))
end

; calculate the force on an agent
to-report force-on-me [it]
  report sqrt ( [fx] of it ^ 2 + [fy] of it  ^ 2  )
end



; calculate the magnitude of the given vector / agent
to-report vecmag [vec]
  report sqrt ( ([xc] of vec) ^ 2  + ([yc] of vec) ^ 2)
end

; calculate the distance to be traveled by the agent calling
; with a dt of 1. set the global max if distance exceeds the current maximum
; also set the global maxforce for this iteration
to peakdist-calc
  if force-on-me self > maxforce [
    set maxforce force-on-me self
  ]
  set dxc  fx / massP
  set dyc  fy / massP
  let dist sqrt ( dxc ^ 2 + dyc ^ 2 )
  if dist > peakdist [
    set peakdist dist
  ]
end

to euclid
  ifelse not euclidflag [
     ask actives [
      pen-up
      coord-set (xc * gx) (yc * gy)
      pen-down
    ]
    set euclidflag True
  ][
    ask actives [
      pen-up
      coord-set (xc / gx) (yc / gy)
      pen-down
    ]
    set euclidflag False
  ]
  show (word "Euclidean space is " euclidflag)
end

;; Geometry feature of model
to doGeometry
  let rval Pradius
  while [ rval <= Prange ] [
    let n 0
    repeat Nsides [
      let txc nextVx Pxcenter rval Nsides n
      let tyc nextVy Pycenter rval Nsides n
      if abs(txc) < max-pxcor and abs(tyc) < max-pycor [
        create-actors 1 [
          initagent
          set xc txc
          set yc tyc
          setxy txc tyc
        ]
        if fill [
          dofill txc tyc rval n
        ]
      ]
      ;; user-message ( word "x y side cos sin" txc " " tyc " "  n  " " cos (360 * n / Nsides) )
      set n n + 1


    ]
    set rval rval + Pstep
    if Pstep = 0 [
      set rval Prange + 1
    ]
  ]
end

to dofill [txc tyc rval nth]
  let f filldistance
  loop [
    let ntxc nextVx Pxcenter rval Nsides (nth + 1)
    let ntyc nextVy Pycenter rval Nsides (nth + 1)
    let nx txc + f * sin ( atan (ntxc - txc) (ntyc - tyc))
    let ny tyc + f * sin ( atan (ntyc - tyc) (ntxc - txc))
    if abs nx > max (list abs ntxc abs txc) or abs ny > max (list abs ntyc abs tyc) [ stop ]
    create-actors 1 [
      initagent
      set xc nx
      set yc ny
      setxy nx ny
      set f f + filldistance
    ]
  ]
 end

to-report nextVx [v rv sides nth]
  report v + rv * precision cos (360 * nth / Nsides) 12
end

to-report nextVy [v rv sides nth]
  report v + rv * precision sin (360 * nth / Nsides) 12
end

;; Gravity cycling feature of model
to-report gcycle
  let pg g
  ifelse elapsedT [
    set g (gamp * (sin ( ( elapsed-time - gshift ) / gperiod))) + glevel
  ][
    set g (gamp * (sin ( ( iterations - gshift ) / gperiod))) + glevel
  ]
  if g = 0 [
    ifelse pg < 0 [
      set g (  - .001 )
    ][
      set g .001
    ]
  ]
  if (g < 0) and (phaseneg = false)  [
    free-groupies
    group-extinction
    set phaseneg true
    set phasepos false
    set phasecount 0
    set phasetrans true
  ]
  if (phasepos = false) and (g > 0) [
    set phaseneg false
    set phasepos true
    set phasetrans true
    set phasecount 0
  ]
  report g
end

;; generate metrics for model
to meanlocs
  set meandistorigin 0
  set meanxloc 0
  set meanyloc 0
  set agentcount (count actors) + (count groups)
  set mominertia 0
;  Calculate the mass weighted mean x and y locations of all the agents
;  Calculate distance to the origin for each agent for the mean distance to the origin calc.
;  Calculate the total mass of all the agents to get the mean x,y location divisor.
;  Set mag (size), angle of each agent
  let av actives
  ask av [
    set mag vecmag self
    set angle atan xc yc
  ]
  set meanxloc ( sum map [a -> [xc] of a * [mass] of a] [self] of av ) / totmass
  set meanyloc ( sum map [a -> [yc] of a * [mass] of a] [self] of av ) / totmass
  set mominertia sum (map [ a -> [mass] of a * ([mag] of a) ^ 2 ] [self] of av)
  set meandistorigin ( sum [mag] of av ) / agentcount
end

to-report coordchg
  let xt xc - xorig
  let yt yc - yorig
  report sqrt ( xt ^ 2 + yt ^ 2 )
end

to-report vectheta
  report atan (xc - xorig) (yc - yorig)
end


to nspvadd [ xv yv ]
  set nspv replace-item 0 nspv ( xv + item 0 nspv )
  set nspv replace-item 1 nspv ( yv + item 1 nspv )
end

to nspvcalc [ av ]
  if count av > 1 [
    let tx 0
    let ty 0
    let tr 0
    let act one-of av
    let rl 0
    ;user-message (word "grps " av " nspv " nspv)
    let theta 1
    ask act [
      set rl other av
      ask rl [
        if mass > [mass] of myself [
          set theta (- 1)
        ]
        set tx xc - [xc] of myself
        set ty yc - [yc] of myself
        set tr realdist self myself
        set tx ( mass * [mass] of myself ) * tx * theta / (totmass ^ 2)
        set ty ( mass * [mass] of myself ) * ty * theta / (totmass ^ 2)

        nspvadd tx ty
        ;user-message (word "nspv " nspv " dist " realdist self myself)
      ]
    ]
    nspvcalc rl
  ]
end

to initmeasure
  ;; take metrics after setup and before go
  let tnum 0
  let tmass 0
  let tmassP 0
  ;show (word "im 1 tribenum " tribenum " tnum " tnum)
  ; initialize the tribes passive and active masses list
  if tribes [
    repeat tribenum [
      ;show (word "im 2 tribenum " tribenum " tnum " tnum)
      ask actors with [ color = item tnum tribecolors] [
        set tmass tmass + mass
        set tmassp tmassp + massP
      ]
      set tribeinitmass replace-item tnum tribeinitmass tmass
      set tribeinitmassP replace-item tnum tribeinitmassP tmassP
      set tmass 0
      set tmassP 0
      set tnum tnum + 1
    ]
  ]
end


to finalmeasure
  ;; change in values of each group and ratio min / max
  show (word "finalmeasure called " count groups)
  set valchgmax max-val-chg
  set valchgmin min-val-chg
  set valchg  valchgmin / valchgmax
  ;; calculate group polarization and moment for each group
  let av actives
  let pg 0
  ; calculate starting center of mass of each group
  let tmass 0
  ask av [
    if breed = groups [
      set tmass sum [mass] of myactors
      set xorig sum (map [ [a  b] -> a * b ] [mass] of myactors [xorig] of myactors) / tmass
      set yorig sum (map [ [a  b] -> a * b ] [mass] of myactors [yorig] of myactors) / tmass
    ]
    set pg coordchg
    set grppol lput pg grppol
    set grppolm lput ( pg * mass ) grppolm
    set floc lput xc floc
    set floc lput yc floc
    set grprad lput gpd grprad
    set grpradm lput ( gmr / totmass ) grpradm
  ]
  let cmchg 0
  let theta 0
  let ttribe 0
  let xt 0
  let yt 0
  ask cogs [
    ;; Create vector for movement of center of total center of mass
    create-link 0
    set cmchg sqrt ( (xcor - xorig) ^ 2 + (ycor - yorig) ^ 2 )
    set theta atan (xcor - xorig) (ycor - yorig)
    set ctrmassv (list xcor xorig ycor yorig)
    set ctrmassvr (list cmchg theta)
    ;; Create vectors for each final groups change in center of mass
    ask av [
      set ttribe tribe
      create-link item ttribe tribecolors
    ]
  ]
  if count av > 1 [
    nspvcalc av
    let rd sqrt ( ( item 0 nspv ) ^ 2 + ( item 1 nspv ) ^ 2 )
    set theta atan ( item 0 nspv ) ( item 1 nspv )
    set nspvr replace-item 0 nspvr rd
    set nspvr replace-item 1 nspvr theta
  ]
  ;; take metrics after setup and before go
  let tnum 0
  set tmass 0
  let tmassP 0
  let tflag false
  ;show (word "fm 1 tribenum " tribenum " tnum " tnum)
  ifelse tribes [
    set tribecount 0
    repeat tribenum [
      ;show (word "fm 2 tribenum " tribenum " tnum " tnum)
      ask actives with [ color = item tnum tribecolors] [
        set tmass tmass + mass
        set tmassp tmassp + massP
        set tflag true
      ]
      if tflag [
        set tribecount tribecount + 1
        set tflag false
      ]
      set tribefinalmass replace-item tnum tribefinalmass tmass
      set tribefinalmassP replace-item tnum tribefinalmassP tmassP
      set tmass 0
      set tmassP 0
      set tnum tnum + 1
    ]
    let tl ""
    let tclr ""
    let tcount 0
    let tpm 0
    let tgm 0
    let tgmax 0
    let tcolor 0
    ask groups [
      set tgmax 0
      set tcount 0
      set tclr ""
      repeat length tribemix [
        if item tcount tribemix != 0 [
          tribecalc
          set tgm precision ((item tcount tribemix) / mass) 2
          set tpm tgm
          if tgm > tgmax [
            set tcolor item tcount tribecolors
            set tgmax tgm
          ]
          ;show (word "tgm tgmax " tgm " " tgmax " tcolor " tcolor " tmix " tribemix)
          set tclr (word tclr " " (item tcount tribecolorname) "=" tpm)
        ]
        set tcount tcount + 1
      ]
      set color tcolor
      show (word "who mass " who " " mass)
      set label (word "mass=" (round mass) tclr)
    ]
  ] [
    ask groups [
      set label (word round mass)
    ]
  ]
end

to create-link [ linkcolor ]
  hatch-forces 1 [
        pen-up
        set xcor [xorig] of myself
        set ycor [yorig] of myself
        create-fvec-from myself [
          set thickness 1
          set color linkcolor
          show-link
        ]
        set hidden? true
      ]
end

to-report edgeval [v m]
  ifelse v < 0 [
    set v (1 - m)
  ] [
    set v m - 1
  ]
  report v
end

to tribecalc
  let tcp 0
  let tm 0
  let tmix [0 0 0 0 0 0]
  ask myactors [
    set tcp (position color tribecolors)
    set tm mass + item tcp tmix
    set tmix replace-item tcp  tmix tm
    ;show (word "who " who " mass " mass " tm " tm " tmix " tmix)
  ]
  set tribemix tmix
end


to-report radmetric
  let xsum 0
  let ysum 0
  let tcount 0
  let rsum 0
  let curx 0
  let cury 0
  let gsum 0
  let tsum 0
  ask actives [
    if breed = actors [
      set tsum  sqrt (((xc - xorig) ^ 2) + ((yc - yorig) ^ 2 ))
      set rsum rsum + tsum
      set gpd tsum
      set gmr mass / totmass
      set tcount tcount + 1
    ]
    if breed = groups [
      set curx xc
      set cury yc
      set tsum 0
      ask myactors [
        set tsum tsum + sqrt (((curx - xorig) ^ 2) + ((cury - yorig) ^ 2 ))
        set tcount tcount + 1
      ]
      set rsum rsum + tsum
      set gpd tsum / acount
      set gmr mass / totmass
    ]
  ]
  let rmetric rsum / tcount
  if rmetric > radmax [
    ;show (word "it: " iterations " rmax=" radmax " rmetric=" rmetric " diff=" (radmax - rmetric) " rsum " rsum " tc " tcount)
    set radmax rmetric
  ]
  report rmetric
end

to-report triberad [ tnum ]
  let xsum 0
  let ysum 0
  let tcount 0
  let rsum 0
  let curx 0
  let cury 0
  let gsum 0
  let tsum 0
  ask actives [
    if tribe = tnum [
      if breed = actors [
        set tsum  sqrt (((xc - xorig) ^ 2) + ((yc - yorig) ^ 2 ))
        set rsum rsum + tsum
        set gpd tsum
        set gmr mass / totmass
        set tcount tcount + 1
      ]
      if breed = groups [
        set curx xc
        set cury yc
        set tsum 0
        ask myactors [
          set tsum tsum + sqrt (((curx - xorig) ^ 2) + ((cury - yorig) ^ 2 ))
          set tcount tcount + 1
        ]
        set rsum rsum + tsum
        set gpd tsum / acount
        set gmr mass / totmass
      ]
    ]
  ]
  let rmetric rsum / tcount
  if rmetric > radmax [
    ;show (word "it: " iterations " rmax=" radmax " rmetric=" rmetric " diff=" (radmax - rmetric) " rsum " rsum " tc " tcount)
    set radmax rmetric
  ]
  report rmetric
end

; Calculate means, standard deviations, etc. for display.
to measure
  if metrics [
    set groupcount count groups

    meanlocs
    ask cogs [
      ifelse abs meanxloc >= max-pxcor or abs meanyloc >= max-pycor [
        set color 15 + ( 10 * int ( iterations / 400 ))
      ] [
        set xcor meanxloc
        set ycor meanyloc
      ]
    ]
    set massratio ( max [mass] of actives ) / totmass

  ; Calculate mean distance to neighbors for all the agents
  ; Calculate mean distance to closest neighbor for all agents
  ; For each agent, store the distance to closest neighbor
    set dcnsum 0
    set meandist 0
    ;meandist-update actors groups
    ;meandist-update groups actors
    meandist-update actives
    ; Get the means across all agents.
    set meandist meandist / agentcount  ; get mean distance to neighbor across all agents
    set meandistclosestneighbor dcnsum / agentcount

    ; Calculate the standard deviations for x, y, and dist to closest neighbor
    set dcnsd 0
    set meanxsd 0
    set meanysd 0
    std-update actors
    std-update groups
    set meanxsd sqrt ( meanxsd / agentcount )
    set meanysd sqrt ( meanysd / agentcount )
    set dcnsd sqrt ( dcnsd / agentcount )
  ]
end

to std-update [ brd ]
  ask brd [
    set meanxsd meanxsd + ( xc - meanxloc ) ^ 2
    set meanysd meanysd + ( yc - meanyloc ) ^ 2
    set dcnsd dcnsd + ( meandistclosestneighbor - dcn ) ^ 2
  ]
end

to meandist-update [ av ]
  let mydist 0
  let mymeandist 0
  ask av [
    set mydist map [tog -> realdist self tog] [self] of other av
    ifelse empty? mydist [
      set mydist list 0 0
    ][
      set dcn min mydist
    ]
    set dcnsum dcnsum + dcn
    set mymeandist sum mydist

    ; Then divide by agentcount -1 to not include self) to gat the mean dist to all agents
    ifelse agentcount = 1 [
      set meandist 0
      set dcnsum 0
    ][
      ; Note that distance to self is 0 and is not counted in calculating the mean, hence agent count - 1
      set meandist meandist + ( mymeandist / ( agentcount - 1 ) )  ; sum each agents mean distance to neighbors
    ]
  ]
end

;; Center of Mass
to recenter
  find-center-of-mass
  ask actors
  [ set xc (xc - center-of-mass-xc)
    set yc (yc - center-of-mass-yc)
    adjust-position
  ]
end

to show-group-start
  let av actives
  let tmass 0
  let ttribe 0
  ask gcogs [ die ]
  ask av [
    set ttribe tribe
    if breed = groups [
      set tmass sum [mass] of myactors
      set xorig sum (map [ [a  b] -> a * b ] [mass] of myactors [xorig] of myactors) / tmass
      set yorig sum (map [ [a  b] -> a * b ] [mass] of myactors [yorig] of myactors) / tmass
    ]
    hatch-gcogs 1 [
      pen-up
      set size 3
      set shape "house"
      set xcor [xorig] of myself
      set ycor [yorig] of myself
      create-fvec-to myself [
        if tribes [
          set color item ttribe tribecolors
        ]
        set thickness 2
        show-link
      ]
      ;set color 66
    ]
  ]
end

to-report min-val-chg
  let rlist []
  let cmgx 0
  let cmgy 0
  ask groups [
    set cmgx sum [mass * xorig] of myactors / sum [mass] of myactors
    set cmgy sum [mass * yorig] of myactors / sum [mass] of myactors
    set rlist lput sqrt (((xcor - cmgx) ^ 2 + (ycor - cmgy) ^ 2)) rlist
  ]
  ask actors [
    set cmgx mass * xorig / mass
    set cmgy mass * yorig / mass
    set rlist lput sqrt (((xcor - cmgx) ^ 2 + (ycor - cmgy) ^ 2)) rlist
  ]
  report min rlist
end

to-report max-val-chg
  let rlist []
  let cmgx 0
  let cmgy 0
  ask groups [
    set cmgx sum [mass * xorig] of myactors / sum [mass] of myactors
    set cmgy sum [mass * yorig] of myactors / sum [mass] of myactors
    set rlist lput sqrt (((xcor - cmgx) ^ 2 + (ycor - cmgy) ^ 2)) rlist
  ]
  ask actors [
    set cmgx mass * xorig / mass
    set cmgy mass * yorig / mass
    set rlist lput sqrt (((xcor - cmgx) ^ 2 + (ycor - cmgy) ^ 2)) rlist
  ]
  report max rlist
end


to find-center-of-mass
  if any? actors
  [ set center-of-mass-xc sum [mass * xc] of actors / sum [mass] of actors
    set center-of-mass-yc sum [mass * yc] of actors / sum [mass] of actors
  ]
end

; Record the view as a movie
to movie
  if record-view [
    vid:save-recording "view.mp4"
    vid:stop
  ]
  if record-interface [
    vid:save-recording "interface.mp4"
    vid:stop
  ]
end

; Record the interface as a movie
to movie-init
  if record-view [
    vid:record-view
  ]
  if record-interface [
    vid:record-interface
  ]
end

to readexp
  foreach csv:from-file expfile [
    [x] ->
      let expfx first x
      if not member? "#" expfx [
        run expfx
      ]
  ]
end

to-report covariance
  let ac count actors
  let xbar sum [xcor] of actors / ac
  let ybar sum [ycor] of actors / ac
  let cov  sum (map [ [xa ya] -> (xa - xbar) * (ya - ybar)] [xcor] of actors [ycor] of actors) / ac
  ;;show (word "xbar ybar cov  " xbar " " ybar " " cov)
  report cov
end


;Copyright 2020 Moody Ahmad, G Jordan Maclay.
;Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International Public License
;License details at https://creativecommons.org/licenses/by-nc-sa/4.0/legalcode
;Note: For full disclosure: This model base was the N-Bodies model in the Netlogo Sample Models library under Chemistry and Physics -> Mechanics -> unverified -> N-Bodies. That
;was a starting point. The model has been modified almost completely from the starting point.
@#$#@#$#@
GRAPHICS-WINDOW
761
12
1544
796
-1
-1
3.86
1
10
1
1
1
0
0
0
1
-100
100
-100
100
0
0
1
Elapsed Time
100.0

BUTTON
14
46
74
79
Go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
192
47
364
80
number
number
0
1000
50.0
1
1
Agents
HORIZONTAL

BUTTON
15
10
187
43
Setup
setup
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
2085
544
2257
577
Create Agent
create-agent
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
2130
585
2273
618
initial-mass
initial-mass
1
5000
50.0
100
1
NIL
HORIZONTAL

SLIDER
2128
629
2273
662
agent-color
agent-color
5
135
0.0
10
1
NIL
HORIZONTAL

BUTTON
102
47
175
80
Go Once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
374
47
477
80
g
g
.00001
.0005
7.0E-5
.00002
1
NIL
HORIZONTAL

PLOT
224
225
743
345
mean distances
iterations
distance
0.0
10.0
0.0
0.2
true
true
"" ""
PENS
"meandist" 1.0 0 -14730904 true "" "plot meandist"
"meandistclosestneighbo" 1.0 0 -14439633 true "" "plot meandistclosestneighbor"
"dcnsd" 1.0 0 -2674135 true "" "plot dcnsd"

PLOT
231
89
741
209
GroupCounter
iterations
number
0.0
10.0
0.0
20.0
true
true
";;set-plot-y-range 0 .20\nif tribes [\n  let ttc tribenum - 1\n  create-temporary-plot-pen item ttc tribecolorname\n]" "if tribes [\n  let ttc 0\n  let tactors 0\n  let tgroups 0\n  repeat tribenum [\n    set-current-plot-pen item ttc tribecolorname\n    set-plot-pen-color item ttc tribecolors\n    set tactors count actors with [tribe = ttc]\n    set tgroups count groups with [tribe = ttc]\n    plot tgroups\n    set ttc ttc + 1\n  ]\n]\n"
PENS
"groups" 1.0 0 -7500403 true "" "plot count groups"

SWITCH
2086
670
2176
703
debug
debug
1
1
-1000

SLIDER
397
10
590
43
CoalescenseRadius
CoalescenseRadius
0
10
0.2
.002
1
NIL
HORIZONTAL

SLIDER
192
10
393
43
AIB
AIB
0
280
80.0
1
1
NIL
HORIZONTAL

PLOT
1547
523
2065
651
Forces
NIL
NIL
0.0
10.0
0.01
0.01
true
true
"" ""
PENS
"attract" 1.0 0 -13791810 true "" "plot attractive"
"repulse" 1.0 0 -2674135 true "" "plot ( - repulsive )"
"net force" 1.0 0 -7500403 true "" "plot attractive - repulsive"
"mcount" 1.0 0 -955883 true "" "plot mergeCount"
"mmass" 1.0 0 -6459832 true "" "plot mergeTotMass"

BUTTON
2025
430
2108
463
OutputData
export-all-plots \"counter.csv\"
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

CHOOSER
18
197
175
242
xSpatialDist
xSpatialDist
"Normal" "Exponential" "Random"
0

SLIDER
36
242
179
275
xMean
xMean
min-pxcor
max-pxcor
0.0
1
1
NIL
HORIZONTAL

SLIDER
38
278
179
311
xStd
xStd
0
100
15.0
1
1
NIL
HORIZONTAL

SLIDER
592
11
735
44
DistanceExponent
DistanceExponent
0
7
2.0
.1
1
NIL
HORIZONTAL

SLIDER
10
699
182
732
maxmass
maxmass
0
12000
12000.0
1
1
NIL
HORIZONTAL

CHOOSER
13
431
209
476
active-mass-distribution
active-mass-distribution
"Normal" "Exponential" "Random"
0

SLIDER
71
477
213
510
mass-mean
mass-mean
1
1000
60.0
1
1
NIL
HORIZONTAL

SLIDER
70
513
215
546
mass-stddev
mass-stddev
0
20
10.0
.1
1
NIL
HORIZONTAL

PLOT
225
348
745
472
meanx
iterations
meanxy
0.0
10.0
0.0
0.3
true
true
"set-plot-y-range -.2 .2\nif tribes [\n  let ttc tribenum - 1\n  create-temporary-plot-pen item ttc tribecolorname\n]\n" "if tribes [\n  let ttc 0\n  let txloc 0\n  repeat tribenum [\n    set-current-plot-pen item ttc tribecolorname\n    set-plot-pen-color item ttc tribecolors\n    set txloc mean [xc] of actives with [tribe = ttc]\n    plot txloc\n    set ttc ttc + 1\n  ]\n]\n"
PENS
"meanx" 1.0 0 -16777216 true "" "plot meanxloc"

PLOT
1551
932
1932
1052
mass distribution
mass
agents
0.0
100.0
0.0
5.0
true
true
";set-plot-x-range 0 int ( (max [mass] of actors) / 10 )\nset-histogram-num-bars 10" ""
PENS
"agents" 1.0 1 -2674135 true "" "histogram [mass] of actives"

PLOT
227
607
755
727
radicalization
time
rad
0.0
10.0
0.0
10.0
true
true
"if tribes [\n  let ttc tribenum - 1\n  create-temporary-plot-pen item ttc tribecolorname\n  ;show (word \"created pen \" item ttc tribecolorname \" tnum \" tribenum)\n]\n;show (word \"setup plot\")" "if tribes [\n  let ttc 0\n  repeat tribenum [\n    set-current-plot-pen item ttc tribecolorname\n    set-plot-pen-color item ttc tribecolors\n    plot triberad ttc\n    ;show (word \"tribe color \" item ttc tribecolorname)\n    set ttc ttc + 1\n  ]\n]\n;show (word \"updated radplot\")\n"
PENS
"rad" 1.0 0 -16777216 true "" "plot radicalization"

MONITOR
366
861
416
906
Agents
(count actors) + (count groups)
0
1
11

BUTTON
2187
670
2267
703
Center
recenter
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
19
89
109
122
track
track
0
1
-1000

PLOT
1552
654
2074
804
peakdist
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"peakdist" 1.0 0 -16777216 true "" "plot peakdist"

BUTTON
1943
430
2015
463
World
export-world \"world.csv\"
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
492
47
626
80
dt
dt
.01
1
5.700435524382011
.01
1
NIL
HORIZONTAL

BUTTON
1565
11
1656
47
NIL
profiler:start
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
1668
14
1760
47
NIL
profiler:stop
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
1563
58
1688
91
NIL
profiler:reset
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
1691
58
1868
91
NIL
output-print profiler:report
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
629
47
749
80
AdaptiveDT
AdaptiveDT
0
1
-1000

MONITOR
293
861
361
906
NIL
totmass
2
1
11

MONITOR
422
861
484
906
NIL
elapsed-time
2
1
11

MONITOR
488
861
553
906
NIL
iterations
2
1
11

PLOT
763
809
1169
929
DeltaT
iterations
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"dt" 1.0 0 -16777216 true "" "plot dt"

PLOT
1941
929
2365
1049
Angles
NIL
Agents
0.0
360.0
0.0
10.0
false
true
"set-histogram-num-bars 360" ""
PENS
"groups" 1.0 1 -16449023 true "" "histogram [angle] of groups"
"actors" 1.0 1 -2674135 true "" "histogram [angle] of actors"

PLOT
1564
233
1872
353
Annular
Magnitude
Agents
0.0
150.0
0.0
2.0
false
false
"set-histogram-num-bars 27" ""
PENS
"groups" 1.0 1 -16777216 true "" "histogram [mag] of groups"
"actors" 1.0 0 -2674135 true "" "histogram [mag] of actors"

BUTTON
2170
744
2263
777
Rotate
ask turtles [\n   pen-up\n   let xt xcor\n   let yt ycor\n   set xcor ( xt * cos rotateAxes ) + ( yt * sin rotateAxes )\n   set ycor ( -1 * (xt * sin rotateAxes ) ) + ( yt * cos rotateAxes )\n   pen-down\n   ]
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
2087
710
2259
743
rotateAxes
rotateAxes
0
360
108.0
1
1
NIL
HORIZONTAL

MONITOR
1647
181
1762
226
repulsive
repulsive
17
1
11

MONITOR
1765
181
1885
226
attractive
attractive
17
1
11

SLIDER
1682
95
1828
128
NumberInjected
NumberInjected
0
20
0.0
1
1
NIL
HORIZONTAL

SLIDER
1683
140
1828
173
InjectionMaxMass
InjectionMaxMass
0
20000
12000.0
1
1
NIL
HORIZONTAL

MONITOR
1572
183
1641
228
peakdist
peakdist
5
1
11

PLOT
1177
808
1546
928
elapsedtime
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"elapsedtime" 1.0 0 -16777216 true "" "plot elapsed-time"

CHOOSER
123
86
215
131
forceAdj
forceAdj
"Normal" "AttractOnly" "RepulseOnly" "NoRepulse" "NoAttract" "Invert"
0

SLIDER
1957
58
2129
91
Nsides
Nsides
1
100
33.0
1
1
NIL
HORIZONTAL

SLIDER
1956
101
2128
134
Pradius
Pradius
1
100
16.0
1
1
NIL
HORIZONTAL

SLIDER
1955
140
2127
173
Prange
Prange
1
100
48.0
1
1
NIL
HORIZONTAL

SLIDER
1957
240
2129
273
Pxcenter
Pxcenter
-100
100
0.0
1
1
NIL
HORIZONTAL

SLIDER
1957
288
2129
321
Pycenter
Pycenter
-100
100
0.0
1
1
NIL
HORIZONTAL

SWITCH
1963
15
2089
48
Geometry
Geometry
1
1
-1000

SLIDER
1954
191
2126
224
Pstep
Pstep
0
100
20.0
1
1
NIL
HORIZONTAL

PLOT
239
727
755
849
Median R
iterations
Radius
0.0
10.0
0.0
10.0
true
true
"if tribes [\n  let ttc tribenum - 1\n  create-temporary-plot-pen item ttc tribecolorname\n]\n" "if tribes [\n  let ttc 0\n  let tmass 0\n  repeat tribenum [\n    set-current-plot-pen item ttc tribecolorname\n    set-plot-pen-color item ttc tribecolors\n    set tmass sum [mass] of actives with [tribe = ttc]\n    plot (sum (map [ a -> vecmag a * [mass] of a] [self] of actives with [tribe = ttc])) / tmass\n    set ttc ttc + 1\n  ]\n]\n\n"
PENS
"R" 1.0 0 -16777216 true "" "plot (sum (map [ a -> vecmag a * [mass] of a] [self] of actives)) / totmass"

SWITCH
1956
340
2059
373
fill
fill
1
1
-1000

SLIDER
1951
389
2123
422
filldistance
filldistance
0
100
1.0
1
1
NIL
HORIZONTAL

MONITOR
557
860
637
905
NIL
stopreason
0
1
11

PLOT
2080
790
2480
922
Force Ratio
time
ratio
0.0
100.0
0.0
0.02
false
true
"" ""
PENS
"ratio" 1.0 0 -16777216 true "set attractive .01" "ifelse attractive + repulsive = 0 [ plot 0 ] [ plot (attractive / (attractive + repulsive) )]"

INPUTBOX
16
134
67
194
tickstop
0.0
1
0
Number

MONITOR
643
861
695
906
Final dt
finaldt
2
1
11

BUTTON
1940
477
2019
510
Rec View
set record-view True
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
2039
479
2108
512
Rec Intfc
set record-interface True
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
12
800
158
845
ProbDist
ProbDist
"None" "AttributeDiff" "DistanceProb"
0

SLIDER
13
848
185
881
Infprob
Infprob
0
100
100.0
1
1
NIL
HORIZONTAL

INPUTBOX
70
134
140
194
AgentStop
0.0
1
0
Number

INPUTBOX
1889
225
1939
285
xOrigin
0.0
1
0
Number

INPUTBOX
1889
295
1939
355
yOrigin
0.0
1
0
Number

SWITCH
1567
360
1710
393
ShowForces
ShowForces
1
1
-1000

CHOOSER
4
583
212
628
passive-mass-distribution
passive-mass-distribution
"Normal" "Exponential" "Random"
0

SLIDER
39
629
211
662
pmass-mean
pmass-mean
0
100
60.0
1
1
NIL
HORIZONTAL

SLIDER
40
664
212
697
pmass-stddev
pmass-stddev
0
40
15.0
.1
1
NIL
HORIZONTAL

SWITCH
4
548
152
581
PassiveMass
PassiveMass
1
1
-1000

PLOT
1576
806
2075
926
Moment of Inertia
time
MomInertia
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"MomentOfInertia" 1.0 0 -16777216 true "" "plot mominertia / 1000000"

CHOOSER
17
313
155
358
ySpatialDist
ySpatialDist
"Normal" "Exponential" "Random"
0

SLIDER
44
360
216
393
yMean
yMean
min-pycor
max-pycor
0.0
1
1
NIL
HORIZONTAL

SLIDER
44
395
216
428
yStd
yStd
0
100
15.0
1
1
NIL
HORIZONTAL

MONITOR
701
861
751
906
Oob #
oobcount
0
1
11

INPUTBOX
12
737
173
797
randomseed
100000.0
1
0
Number

INPUTBOX
144
134
219
194
DTStop
0.0
1
0
Number

MONITOR
233
860
283
905
MDCN
meandistclosestneighbor
2
1
11

BUTTON
16
887
108
920
NIL
setmore
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
2289
626
2461
659
gamp
gamp
0
1
0.01
.01
1
NIL
HORIZONTAL

SLIDER
2289
665
2461
698
gshift
gshift
-10
10
0.0
1
1
NIL
HORIZONTAL

SLIDER
2290
742
2462
775
glevel
glevel
-10
10
0.0
1
1
NIL
HORIZONTAL

SLIDER
2289
703
2461
736
gperiod
gperiod
0
100
7.0
1
1
NIL
HORIZONTAL

SWITCH
2288
588
2428
621
gravitycycle
gravitycycle
1
1
-1000

PLOT
2135
237
2475
357
g
NIL
g
0.0
1.0
-0.001
0.001
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot g"

SLIDER
2289
546
2461
579
phaseiteration
phaseiteration
0
20
0.0
1
1
NIL
HORIZONTAL

SWITCH
2290
504
2413
537
elapsedT
elapsedT
1
1
-1000

SLIDER
2137
72
2309
105
Kx
Kx
-.1
.1
0.001
.001
1
NIL
HORIZONTAL

SLIDER
2318
71
2490
104
Ky
Ky
-.1
.1
0.001
.001
1
NIL
HORIZONTAL

SWITCH
2137
25
2240
58
Temp
Temp
1
1
-1000

SLIDER
2140
111
2312
144
fxcenter
fxcenter
0
100
9.6
.1
1
NIL
HORIZONTAL

SLIDER
2137
156
2309
189
fxwidth
fxwidth
.1
400
84.1
1
1
NIL
HORIZONTAL

CHOOSER
2269
14
2407
59
gradtype
gradtype
"linear" "expo"
0

BUTTON
2421
21
2517
54
heatmap
heatmap
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
1548
399
1937
519
MassRatio
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"massratio" 1.0 0 -16777216 true "" "plot massratio"

SLIDER
2324
115
2496
148
fycenter
fycenter
-90
90
35.0
1
1
NIL
HORIZONTAL

SLIDER
2314
156
2486
189
fywidth
fywidth
1
100
50.0
1
1
NIL
HORIZONTAL

SLIDER
2315
200
2487
233
xwidth
xwidth
0
100
13.0
1
1
NIL
HORIZONTAL

SLIDER
2140
199
2312
232
ywidth
ywidth
0
100
35.0
1
1
NIL
HORIZONTAL

SWITCH
1732
360
1842
393
metrics
metrics
0
1
-1000

BUTTON
1763
13
1826
46
GrpStrt
show-group-start
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
1841
14
1944
47
tribes
tribes
0
1
-1000

INPUTBOX
1558
102
1608
162
agx
1.0
1
0
Number

INPUTBOX
1616
101
1666
161
agy
1.0
1
0
Number

INPUTBOX
112
908
357
968
expfile
ent6.txt
1
0
String

BUTTON
15
933
105
966
NIL
readexp
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
226
476
747
607
meany
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"set-plot-y-range -.2 .2\nif tribes [\n  let ttc tribenum - 1\n  create-temporary-plot-pen item ttc tribecolorname\n]\n" "if tribes [\n  let ttc 0\n  let txloc 0\n  let tyloc 0\n  repeat tribenum [\n    set-current-plot-pen item ttc tribecolorname\n    set-plot-pen-color item ttc tribecolors\n    set tyloc mean [yc] of actives with [tribe = ttc]\n    plot tyloc\n    set ttc ttc + 1\n  ]\n]\n"
PENS
"meany" 1.0 0 -16777216 true "" "plot meanyloc"

BUTTON
392
928
466
961
grpMv
show-group-start
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
489
916
734
976
tnum1
2.0
1
0
Number

INPUTBOX
751
932
996
992
tnum2
10.0
1
0
Number

INPUTBOX
1005
932
1250
992
tnum3
120.0
1
0
Number

INPUTBOX
1877
59
1943
119
injMassx
0.0
1
0
Number

INPUTBOX
1874
119
1946
179
injMassy
20.0
1
0
Number

@#$#@#$#@
## Abstract

The model is based on a vector representation of each agent.  The components of thevector are the key continuous “attributes” that determine the social behavior of theagent.  A simple mathematical force vector model is used to predict the effect of eachagent on all other agents.  The force law used is motivated by gravitational force lawsand electrical force laws for dipoles.  It assumes that the force between two agents isproportional to the ”similarity of attributes”, which is implemented mathematically asthe dot product of the vectors representing the attributes of the agents, and the forcegoes as the inverse square of the difference in attributes, which is expressed as theEuclidean distance in attribute space between the two vectors.  The force between theagents may be positive (attractive), zero, or negative (repulsive) depending on whetherthe angle between the corresponding vectors is less than, equal to, or greater than 90°.A positive force causes the attributes of the agents to become more similar and thecorresponding vectors to become more nearly parallel.  Interaction between all agents isallowed unless the difference between the attributes representing the agents exceeds aconfidence limit (the Attribute Influence Bound) set in the simulation.  Similar agentstend to form groups.  For small values of the Attribute Influence Bound, numerousgroups remains scattered throughout attribute space at the end of a simulation.  As theAttribute Influence Bound is increased, and agents with increasingly different attributescan communicate, fewer groups remain at the end, and the remaining groups haveincreasingly different characteristic attributes.  With a large Attribute Influence Boundall agents are connected and extreme bi- or tri-and occasional quadri-polarizationresults.  During the simulations, depending on the initial conditions, collective behaviorsof grouping, consensus, fragmentation and polarization are observed as well as certainsymmetries specific to the model, for example, the average of the attributes for allagents does not change significantly during a simulation.

## WHAT IS IT?

This model is exploring how simple rules can create complex behaviors. We start with several basic rules.

  * The universe is represented by a vector space, currently 2-D, in which the coordinates represent continuous measurable attributes characterizing the agents in the space.  It is assumed that the behavior of the agents (ie their movement in the vector space) depends on their attributes and their interactions with other agents.

  * Each agent is represented by a vector A(xa, ya) in the space.  All vectors start at the origin (0,0) and end at the point (xa, ya), where -100<xa<+100, 100<ya<+100.

  * Each agent A has a mass m<sub>A </sub> associated with it.  The influence of an agent A on another agent is proportional to its mass m<sub>A </sub> 

  * There are forces, attractive and repulsive, between any two agents A(xa,ya) and B(xb,yb) that depend on the relative orientation of the vectors representing the agents. The force of A on B is proportional to the dot product of the vectors AdotB times the mass of A, divided by the square of the distance between the agents (A-B)^2=R^2, times an overall scale factor g.  The force is a vector directed along the line connecting the heads of the vectors. The force is akin to a gravitational force except for presence of the dot product, which is reminiscent of the force between electrical dipoles.

  * The force of agent A on agent B causes an accelertion of agent B which equals the force of A on B divided by the mass m<sub>B </sub> of agent B.  The acceleration is exerted on an agent for a time interval DT, which causes a movement of the agent which equals the acceleration times (DT)<sup>2</sup> . In each iteration, all movements are calculated for each agent and the agents are moved accordingly.   

> F<sub>AonB</sub> = g  m<sub>A</sub> ( (A . B) / R<sup>2</sup>) (A-B)/R 
We assume
F<sub>AonB</sub> = m<sub>B</sub> a<sub>B</sub> 
solving for the acceleration a<sub>B</sub> on B due to force of A, we get
a<sub>B</sub> = g  (m<sub>A</sub> / m<sub>B</sub>) ( (A . B) / R<sup>2</sup>) (A-B)/R  
	where A, B, and F are vectors and R is the magnitude of the vector A-B
R = sqrt ((xa - xb) <sup>2</sup> + (ya - yb) <sup>2</sup>)
 g is the gravitational constant and a is the acceleration.

The force between two agents is attractive if their vectors tend to point in the same direction, which means the absolute value of the angle between the vectors representing the agents is 90 degrees or less 
(or the algebraic difference in the angles is between -90 and +90).

The force between two agents is repulsive if their vectors tend to point in opposite directions, which means the absolute value of the angle between the vectors representing the agents is greater than 90 degrees 
(or the algebraic difference in the angles is greater than 90 and less than 270).

Attribute Influence Bound (AIB)
If the distance R between two agents exceeds the value set in the Attribute Influence Bound (AIB) slider then we assume the agents do not communicate and the force between the agents is automatically set to zero. The maximum value of the AIB is about 280, which is the distance from one corner of the 200x200 space to the opposite corner.    

Setting the time interval DT
If AdaptiveDT is NOT selected:
Then the value on the DT slider is used.  In each iteration of the simulation, the movement of each agent is computed.

If AdaptiveDT IS selected:
If the forces are very small, then DT is increased until the largest movement of an agent is set equal to the Coalescence Radius.  On the other hand if the forces are very large, then DT is reduced so that the maximum distance traveled in one iteration for any agent is equal to the Coalescence Radius CR. Use of AdaptiveDT provides an efficient way to simulate situations in which forces are small and yet may act for long times.

Movement of Agents
The force of A on B causes an acceleration a<sub>B</sub> of the vector B that goes inversely with m<sub>B</sub>, the inertial mass of B resisting the change in the vector B.  The acceleration acts for a time step DT causing a displacement of the vector B equal to DeltaB.  The displacements of all agents due to the force on each other are computed in each time step.  

>  DeltaB = (F<sub>AonB</sub> / m<sub>B</sub>)(DT)<sup>2</sup>
   DeltaB = g (m<sub>A</sub> /m<sub>B</sub>) (DT)<sup>2</sup> ((A . B)/ R<sup>2</sup>)(A - B)/R

This particular model demonstrates attraction and repulsion in a vector space.  If the distance R between two agents becomes less than a preset value, the Coalescence Radius CR, the agents are merged into a group, with a mass equal to the sum of the masses of the component agents.  The force laws for groups are the same as for individual agents.  Once an agent is part of a group, the agent cannot leave the group.

The universe is infinite. Agents can move beyond the boundaries of the 200 x 200 universe. Their departure point from the visible universe is indicated by a ghost agent. The agents continue to operate in the invisible, infinite universe according to the same rules as in the visible universe. Agents in the invisible universe continuously manifest themselves at their point of departure from the visible universe. The center of the universe is indicated by a bullseye at coordinates (0,0).

#### Metrics Defined

* **Group Polarization Distance - GPD**
Consider the location of a group near the end of a simulation. The number of agents in the group is not changing as the group moves a distance R from its initial location as a group to its final location as the same group, a movement which often occurs as a result of repulsive forces. We can define the group polarization as GPD = R,
where R is the difference between the initial Center of Attributes and the final Center of Attributes for the group. This definition of group polarization indicates how much the attributes of the group have changed during the simulation, sort of an indication of the degree of radicalization of the agents in the group. Although, in general, it seems groups of low mass tend to move more than groups of high mass, the GPV has no explicit information about the mass of the group, just its movement in attributes space.

* **Group Polarization Moment - GPM**
It seems useful at times to be able to consider differences in significance between Group Polarization for small groups and for big groups. To accomplish this, we can define the Group Polarization Moment GPM as GPM = mR where m is the mass of the group. This could be considered analogous to momentum in physics, which is mass times velocity. The GPM could be considered as the product of the group mass and the average velocity which equals the distance it moves in the time it takes for one simulation, which is R.

* **Radicalization Metric**
The radicalization metric R(t) is a scalar defined at each time t of simulation that gives an overall indication of mean change in the attributes for all the agents or groups.  R(t) is given in terms of the difference between the location of each agent or group at time t,  (x_n(t), y_n(t)),  where  n indicates the n<sup>th</sup> agent or group, and the original position of the corresponding agent (x_n(0), y_n(0)) which occurs at time t=0 for N groups / agents.

>R(t) = Sqrt {( 1 / N ) Sum ( {[(x<sub>n</sub>(t)-x<sub>n</sub>(0))<sup>2</sup>}+(y<sub>n</sub>(t)-y<sub>n</sub>(0))<sup>2</sup>}].


## HOW TO USE IT

### Controls

#### Setup

* Before setup, select / setup the following parameters:
     * **Number** of agents- 0-200
     
     * **Spatial** distribution of agents: Random, Normal, or Exponential. The spatial distribution for each coordinate (x,y) is set independently. This is also true for the mean and standard deviations for each coordinates distribution. These are set by **xMean**, **xStd** and **yMean**, **yStd**
         * **Random** Distribution - mmean and stddev are ignored for random distribution
         * **Normal** Distribution - mmean gives the location (mmean,mmean) of the center of the distribution. The standard deviation ranges from 0 to 1. A standard deviation of 1 means a distance of 100 in our coordinate system. NOTE: In random or normal distributions, if the random number generator places a particular agent outside the boundaries 200 x 200,  then a new random number is chosen for the next agent, and the process is continued until the number of initial agents requested are all placed within the boundaries.  This process occurs regularly for a normal distribution with a standard deviation above about 0.5.  
         * **Exponential** Distribution - is equivalent to ( - mean ) * ln random-float 1.0. This is scaled to the size of the universe 200 x 200. The exponential distribution of x is given by the equation Probability distribution is  b exp[-bx] for x>0,  and is 0 for x<0.  On the other hand, if  x<0, then Probability distribution is b exp[bx] for x<0, and 0 for x>0. The distribution is normalized to 1 for the interval 0 to infinity.  The mean value is 1/b.  Thus setting the mean value determines the distribution since there are no additional parameters.  Setting the standard deviation does not affect this distribution. Note the distribution can give agents on only one side of the axis, but not on both sides.   If the mean for the x distribution is set to 0, then all agents are on the y-axis. If the mean for the y distribution is set to 0, then all agents are on the x-axis.
     
     * **Mass** distribution of the agents that are created.  Maxmass - this is the maximum mass that is allowed for any of the mass distributions.  The SD standard deviation in mass also needs to be specified for the normal distribution. The force of agent A on agent B is proportional to the mass of agent A.  The movement of agent B from the force due to A goes inversely as the mass of agent B.
        * **Random** - all agent masses are between 1 and maxmass
        * **Normal** - all masses are normally distributed around mass-mean with mass-stddev and between 1 and maxmass.
        * **Exponential** - is equivalent to ( - mass-mean ) * ln random-float 1.0. The mass is always between 1 and maxmass
  
     * **Attribute Influence Bound (AIB)** - This is the maximum distance between two agents at which they affect each other. There is no interaction between agents that are farther apart than the AIB. The largest possible distance in the 200 x 200 universe is the hypoteneuse 200 SqRt(2) = 283. 
     * **Distance exponent** - this is exponent for the distance (R - normally set to 2) in the force equation: F<sub>AonB</sub> = g  m<sub>A</sub> ( (A . B) / R<sup>DistanceExponent</sup>) (A-B)/R   

     * **Coalescence Radius  (CR)** - If two agents move closer together than the coalescence radius, then the agents are merged into one group agent and the mass of the group is the sum of the masses of the component agents. CR is typically set to 0.3. The location of an individual agent is shown by a circle.  After two individual agents coalesce, the resulting group is represented as a triangle.  The numbers beside the symbols indicate the corresponding mass.

     * **g** - This is the overall constant that scales the force between two agents, similar to the gravitational constant for gravitational forces. Typically set to 7 x 10^-5 .

     * **agx, agy** Set space metric for x and y axes for each agent.

     * **AdaptiveDT** - time granularity is important for running the simulation in reasonable time frames. The default value for the time step is DT=1.  If AdaptiveDT set to "on" , the simulation runs and the value of DT is varied to insure that the maximum movement of an agents equals the Coalescence Radius. The current value of DT is displayed in the slider for each iteration and plotted in the DT plot.  We have noticed more than an order of magnitude reduction in elapsed time to reach a final state with AdaptiveDT. We have compared the end results and verified that the end results are equivalent with a AdaptiveDT and a low value of DT for smooth dynamics.
	With AdaptiveDT off, you can set the value of DT that you want to use for the simulation with the slider.  The value is fixed for all iterations.  The default is 1.
 
     * **Randomseed** - if set to 0, each simulation run will generate a different random placements of agents and their masses. If Randomseed is set to a nonzero integer, that seed will allow you to start the simulation with the exactly the same random initial conditions of agent placements and masses and colors. This is useful for repeating scenarios where you want to test the same initial condition while varying some of the other parameters, e.g., Coalescence Radius.

     * **ProbDist** - is the probability distribution chosen for agent interactions.
         * None - all agents interact / influence each other depending on other parameters such as AIB.
         * AttributeDiff - agent interaction probability is based on the difference in attributes between agents. If the agent attributes are orthogonal to each other, then the probability of their interaction is zero. If the attributes are in total alignment, the probability of interaction is 100 %.
         * DistanceProb - the probability is (AIB - Dist) / AIB where Dist is the distance between the agents.

     * **infprob** - influence probability is the percentage of agents that are influenced by others. For example, if it is set to 10, 10% of the agents will be influenced by others. Which agents are not influenced is random.
     
     * **track** - agents / groups leave a track of their trajectory as they move.

#### Simulation Stopping Conditions

There are four conditions that cause the simulation to stop:

* No movement - force is zero for all agents. AIB exceeded or probability factors.
* ItStop - number of iterations requested reached
* AgentCount - stop at given number of agents left.
* DTStop - Stop if dt >= the given nonzer value.

####   Monitors    
	
##### _On the bottom of display_
* **totmass** - at bottom of display, this monitor give the total mass of all agents in the simulation
* **agents** - shows the total number of agents active at any given time in the simulation which equals individual agents plus group agents
* **elapsed-time** shows the sum of the DT values used in the simulation to this point..  One additional DT value is added to elapsed time for each iteration . 
* **iterations** - total number of iterations (time steps) that the model has run at any point in the simulation. **Note:** With dynamic dt, ticks (on the top) actually shows elapsed time. Iterations shows the actual number of iterations of the simulation.
* **stopreason** - why simulation stopped
* **Final DT** - final value of dt at end of simulation
* **Oob #** out of bounds count - how many agents fall outside the universe during initial setup. The out of bounders are deleted and not used in the simulation.


##### _On right side of display_
* peakdist - this give the maximum distance an agent would travel in the current iteration assuming DT = 1.   If AdaptiveDT is on, then DT will be modified so the actual maximum distance the agent travels is the coalescence distance CR. 
* repulsive - this is the sum of the repulsive forces for the current iteration
* attractive - this is the sum of the attractive forces for the current iteration


#### Optional Controls

##### _On the LEFT side of the display_
  * **AgentStop** - if non-zero, simulation stops when the agent count is less than or equal to this number.
  * **center** - calculates the center of mass of all the agents and moves them so that the center of mass is at the origin.
  * **Create** Agent - allows manual creation of an agent by using the mouse to place agents on the display grid.  Multiple agents may be created, one at a time.
    * **Initial**-mass sets the mass of the created agent and 
    * **Agent**-color sets the color of the created agent (a number between 0 and 140).
  * **debug** - shows debug messages at various simulation points.
  * **DTStop** - if nonzero, stop if dt is >= to this value. 
  * **expfile** specify experiment file. Should be in the same folder as the model file.The format of the expfile is described below. 
  * **forceAdj** - changes the application of the force law.
    * **Normal** - both positive and negative forces are in play. If A . B is positive, the force is attractive, If A . B is negative, the force is repulsive.
    * **AttractOnly** - all forces are attractive only. If A . B is negative it's absolute value is used
    * **RepulseOnly** - all force calculation are turned negative, minus the absolute value of F is used.
    * **NoAttract** - all attractive forces are set to zero so that only repulsive forces are in play.
    * **NoRepulse** - all repulsive forces are set to zero so that only attractive forces are in play.
    * **Invert** - forces are inverted. If attractive, they are made repulsive and vice versa.

   * **OutputData** - Clicking this button exports all the plots on the interface as a csv file called "counter.csv"
   * **readexp** read the experiment file specified.
   * **rotateAxes**-slider to set value to 0 to 360 degrees
   * **ROTATE** button, this causes the system to rotate the coordinate system clockwise the number of degrees specified in the rotateAxes slider.
   * **Record view** - if clicked before go, it will make a movie of the simulation with one frame for each iteration. Output file is view.mp4
   * **Record intfc** - if clicked before go, it will make a movie of the interface with one frame for each iteration. Output file is interface.mp4.
  * **setmore** allows additional changes after the initial setup. For example, you can set one distribution of agents, masses, etc. Then change those parameters and when you click setmore, it will add the additional agents with specified parameters.
  * **tickstop** - if nonzero, stop after that many iterations.
  * **World** - Clicking this button at any time will create a "world.csv" file that captures all the data on the interface, i.e., all the plots, values of global variables, all the positions and attributes of all the agents, and all the attibutes of each cell of the universe. 

##### _On the RIGHT side of the display_
 * **agx, agy** Set space metric for x and y axes for each agent.
 * **elapsedT** when on uses elapsed time for x in the sine function below. When off, it uses iteration count for x.
 * **gradtype** determines the type of gradient for the social pressure field:
     * **linear** adds a pressure field Kx * x coordinate, Ky * y coordinate for each agent.
     * **expo** adds a presure field for each coordinate c:
> K<sub>c</sub> pb * E<sup>pa - pb * c</sup> / ( 1 + (E<sup>pa - pb * c </sup>)<sup>2</sup>)
where pa = fcenter * 10  / fwidth, pb = 10 / fwidth

 * **fxcenter** is center / peak of the social force exponential effect for x
 * **fycenter** is center / peak of the social force exponential effect for y
 * **fxwidth** is the width of the social force effect for x
 * **fywidth** is the width of the social force effect for y
 * **gravitycycle** if off, use value of g for gravity. If on, use the parameters below to determine sine wave amplitude, frequency, level, shift from the following equation:  **g = glevel + gamp * Sine (x - gshift) / gfreq**. This allows cyclicality in the behavior of agents.
     * **gamp** amplitude of sine wave
     * **gfreq** frequence of sine wave
     * **glevel** y-axis level of sine wave
     * **gshift** x-axis shift of sine wave
     * **x** is elapsed time
 * **GrpStrt** - button shows the starting center of mass of all the groups
 * **InjectionMaxMass** this specifies the mass of the injected agents.   All injected agents have this same mass.
 * **Number injected** this allows the injection of a specified number of agents,  placed according the initial condition selected (random, normal distribution or exponential).
 * **parma** parameter a value for gradtype selection
 * **parmb** parameter b value for gradtype selection 
 * **Phaseiteration** how any iterations to do without mergers, collisions, or group formation.
 * **Radicalization** shows a metric of radicalization / extremism in the simulation. The idea here is that the difference between the starting point of an actor vs the end point (or current point) is a measure of how much an attribute measure has shifted. If an attribute value is lessened that lessens radicalization. If an attribute value is increased, that is assumed to indicate radicalization. If there are tribes, the radicalization for each tribe is shown in the tribe's color.
 * **ShowForces** shows the force vectors for each actor / group at each iteration step. **Note** that the size of sthe force vector is relative at each iteration. At each iteration, the maximum force is calculated. that force is scaled (by 40 units) to be the maximum force vector length. All other force vectors use that as the measure for that iteration.
 * **tribes** turns the tribes feature on. Each setup causes the formation of a new tribe. The typical way to use it would be to do an initial setup. Then change paraameters as desired, e.g., number of agents, distribution, etc. Then click on "setmore". This creates a second tribe with the given parameters. Multiple such tribes can be created. The tribes feature also gives all tribe members a unique color. Whenever actors / groups merge, the color of the merged group is the color of the actor / group with the larger mass.
 * **Temp** adds a "thermal field" of social pressure. This turn the feature on / off
 * **xOirigin** shifs the x origin left ( -# ) or right ( +# ). This is done at setup time before any iteration is done. **Note** that this is the same as shifting a normal distribution left or right by the specified amount.
* **yOrigin** shifs the y origin down ( -# ) or up ( +# ). This is done at setup time before any iteration is done.
 * **xwidth** is the width of the social force effect in universe units for x. **Note** that the center of the rectangle is fycenter for x and vice versa for y
 * **ywidth** is the width of the social force effect in universe units for y




##### Fixed Geometries of Agents

This allows the creation of initial conditions in which agents have regular polygonal geometries. It allsws the creation of grids of nested polygins. **Note** that the masses of the agents created are determined by the mass distribution selections for active and passive masses with the same controls as in the non-gemetry cases.

  * Geometry: on/off. Off is normal operation. No injections. On injects polygons.
  * Nsides - slider from 1 to 100. Indicates the number of sides for a regular polygon
  * Pradius - slider that indicates the distance to a vertex from the origin
  * Prange - slider to indicate the range of the polygons, maximum x value for a vertex.The calculations assume that the first vertex is on the x axis with y = 0
  * Pstep - slider to indicate the radius growth till the range limit is met
  * Pxcenter - is the center of the polygon. Initially set to 0. However, you can move the center of the polygons anywhere in the universe. Caveat,
  * Pycenter - is the y coordinate for the center of polygons.

Filling capability allows the creation of agents along the sides of the polygon created.

  * fill - on / off. Off is normal operation, Only agents at the vertices of polygons
  * filldistance - is the distance between a vertex and an agent on the side of a polygon. Agents are created filldistance apart till the next vertex is reached. No agents beyond the polygon sides or vertices are created.

##### Hidden Features

With randomseed set to 0, Netlogo generates its own seed. If one sees behavior on such a run that one would like to reproduce, one can get the seed used for that run by typing in the observer text / command box: _**rseedused**_. That seed can be put into the randomseed text box to rerun that simulation.

### Plots

**Agent Counter**

* **turtles** - the total count of all the active agents for each iteration which equals the sum of individual agents and group agents
* **groups** - the total number of all agent groups resulting from coalescence of individual agents for each iteration


**mean distances**

* **meandist**- mean distance between all the agents for each iteration, irrespective of whether a particular agent is within the ID
* **meanxsd** - standard deviation of the mean x distance between agents for each iteration
* **meanysd** - standard deviation of the mean y distance between agents for each iteration
* **meandistclosestneighbor** - the mean distance to the closesest neighbor for all agents for each iteration irrespective of the ID
* **dcnsd** - standard deviation of distance to closest neighbor for each iteration (if the screen display is made smaller, sometimes the legend for this does not display fully)

**maxdist meanxy** 

* **meanxloc** - active mass weighted mean x coordinate for all agents for each iteration. This equals  <b>&Sigma;</b> [active mass of agent<sub>i</sub> times its x coordinate x<sub>i</sub>]/total active mass of all agents. This is the center of mass of the system. To be able to visualize its change easily, we added a center of mass agent to the current location of the center of active mass at each iteration of the model. This movement is traced and you can see how the center of active mass changes during the simulation. The trajectory of the changing center of mass is indicated by changes in its color. It start red and progresses to higher colors.
* **meanyloc** - mass weighted mean y coordinate for all agents for each iteration calculated same way as meanxloc
* **maxdist** - the maximum distance that is traveled by an agent for each iteration.  If DynamicDT is on, then maxdist will equal the Coalescence Radius.

**Median R**

* **R** Mass weighted distance from the origin
* If there are tribes, **R** is plotted for each tribe with the tribe's color
 
**DeltaT**
 
* plot of the value of DT versus the iteration number.  DT would be constant equal to 1 or the set value of DT if AdaptiveDT is off.  If AdaptiveDT is on, the plot gives the values of DT employed in the simulation to secure maximum agent movements equal to the Coalescence Radius.

**Elapsed Time**

*Plot of the elapsed time (which is the sum of the values of DT) on the y-axis versus the iteration number on the x-axis.

**mass distribution**

* **histogram** of the mass distribution of the active agents (both individual and group agents)

**Forces**

  * Attract- shows the total of the attractive forces on all agents for each iteration
  * Repulse - the total of the repulsive forces for each iteration
  * Net force - the difference between attractive and repulsive forces for each iteration
  * mmass - the amount of mass that has merged on an iteration. Note the forces at merge time.
  * mcount - number of merges in an iteration

**peakdist**
  * peakdist - this is the maximum distance that an agent would move in an interation assuming that DT=1. (Note: If DynamicDT is on, then the actual value of DT used is modified to limit the actual maximum distance to CR, the coalescence radius. peakdist is computed to determine the value of DT required for this.) 

**dcn** (distance to closest neighbor)
* histogram of the distance to closest neightbor DCN,  scale is from 0 to about 300.

**Angles**
   * Histogram of the number of agents as a function of the angle at which the agents are located.  The angle 0-360 degrees is measured clockwise from the vertical axis. 

**Annular**
   * Histogram of the number of agents as a function of their distance 0 to 140 from the center (0,0). 

**Moment of Inertia**
   * Plot of the sum of the each mass * R<sup>2</sup> at each simulation step. This number is downscaled by dividing by 10<sup>6</sup> for plotting purposes.

**Force Ratio**

   * ratio - 100 * repulsive force / ( attractive + repulsive ). <b>Note</b> the 100 multiplier is for being able to visualize the graph values better.
   * repulsive - repulsive force at each step

### Formats

**expfile**


* The experiment file is read line by line. The basic structure is a set of initializations. This is followed by a set of parameter settings for a tribe. That is followed by a setmore command. Then the next set of parameters for the next tribe. The initialization sets up the number of tribes to be and the interaction matrix for the tribes. Here are the types of lines:
     * line beginning with **#** is a comment line and is not used to run the experment
     
     * a line beginng with **set** sets the value of the specified parameter. For example, set number 20 set the number of agents to 20
     * **setmore** creates a tribe with the parameter values at that point.
     * For the tribes feature, a matrix is created for the tribe interactions. For 2 tribes it is a 2 by 2 matrix and so on. For this
          * **set tribematrix matrix:make-identity 3**  - set the number of tribes and the size of the matrix.
          * **set tribematrix matrix:from-row-list [[1 -1 -1] [-1 1 -1] [-1 -1 1]]**  - sets the interaction matrix row by row for a 3 x 3 interaction matric.


## Copyright and License
Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International Public License.
License details at https://creativecommons.org/licenses/by-nc-sa/4.0/legalcode
Note: For full disclosure: This model base was the N-Bodies model in the Netlogo Sample Models library under Chemistry and Physics -> Mechanics -> unverified -> N-Bodies. That
was a starting point. The model has been modified almost completely from the starting point.
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

ghost
false
0
Polygon -7500403 true true 30 165 13 164 -2 149 0 135 -2 119 0 105 15 75 30 75 58 104 43 119 43 134 58 134 73 134 88 104 73 44 78 14 103 -1 193 -1 223 29 208 89 208 119 238 134 253 119 240 105 238 89 240 75 255 60 270 60 283 74 300 90 298 104 298 119 300 135 285 135 285 150 268 164 238 179 208 164 208 194 238 209 253 224 268 239 268 269 238 299 178 299 148 284 103 269 58 284 43 299 58 269 103 254 148 254 193 254 163 239 118 209 88 179 73 179 58 164
Line -16777216 false 189 253 215 253
Circle -16777216 true false 102 30 30
Polygon -16777216 true false 165 105 135 105 120 120 105 105 135 75 165 75 195 105 180 120
Circle -16777216 true false 160 30 30

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

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
setup-two-planet
repeat 125 [ go ]
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="expa1" repetitions="1" runMetricsEveryStep="false">
    <setup>let fn (word "data/" behaviorspace-experiment-name "/views" )
let fp (word "data/" behaviorspace-experiment-name "/plots" )
let rv (shell:exec "mkdir" "-p" fn fp)
set tribes true
set number 0
setup

set number tnum1
set mass-mean 5000
setmore

set number tnum2
set mass-mean 100
setmore

set number tnum3
set mass-mean 5
setmore

set tribematrix matrix:make-identity 2
set tribematrix matrix:from-row-list [[1 -1 -1] [1 1 -1] [-1 -1 1]]</setup>
    <go>go</go>
    <postRun>let fn (word "data/" behaviorspace-experiment-name "/views/view" behaviorspace-run-number ".png")
let fp (word "data/" behaviorspace-experiment-name "/plots/plot" behaviorspace-run-number ".csv")
let wd (word "data/" behaviorspace-experiment-name "/plots/world" behaviorspace-run-number ".csv")
export-view fn
export-world wd</postRun>
    <timeLimit steps="5000"/>
    <metric>active-mass-distribution</metric>
    <metric>attractive</metric>
    <metric>agentcount</metric>
    <metric>ctrmassv</metric>
    <metric>ctrmassvr</metric>
    <metric>dcnsd</metric>
    <metric>elapsed-time</metric>
    <metric>floc</metric>
    <metric>groupcount</metric>
    <metric>grppol</metric>
    <metric>grppolm</metric>
    <metric>grprad</metric>
    <metric>grpradm</metric>
    <metric>iterations</metric>
    <metric>masserror</metric>
    <metric>massratio</metric>
    <metric>meandist</metric>
    <metric>meandistclosestneighbor</metric>
    <metric>meandistorigin</metric>
    <metric>meanxloc</metric>
    <metric>meanyloc</metric>
    <metric>nspv</metric>
    <metric>nspvr</metric>
    <metric>oobcount</metric>
    <metric>peakdist</metric>
    <metric>radicalization</metric>
    <metric>repulsive</metric>
    <metric>rseedused</metric>
    <metric>stopreason</metric>
    <metric>timeend</metric>
    <metric>timestart</metric>
    <metric>totmass</metric>
    <metric>valchg</metric>
    <metric>valchgmax</metric>
    <metric>valchgmin</metric>
    <enumeratedValueSet variable="active-mass-distribution">
      <value value="&quot;Normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="AdaptiveDT">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-color">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="AgentStop">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="AIB">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CoalescenseRadius">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debug">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DistanceExponent">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dt">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DTStop">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="elapsedT">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="filldistance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fill">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forceAdj">
      <value value="&quot;Normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fxcenter">
      <value value="9.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fxwidth">
      <value value="84.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fycenter">
      <value value="35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fywidth">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="g">
      <value value="7.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gamp">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Geometry">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="glevel">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gperiod">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gradtype">
      <value value="&quot;linear&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gravitycycle">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gshift">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infprob">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-mass">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="InjectionMaxMass">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Kx">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ky">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mass-mean">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mass-stddev">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maxmass">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metrics">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nsides">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NumberInjected">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="passive-mass-distribution">
      <value value="&quot;Normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="PassiveMass">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="phaseiteration">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pmass-mean">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pmass-stddev">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pradius">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Prange">
      <value value="48"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ProbDist">
      <value value="&quot;None&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pstep">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pxcenter">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pycenter">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomseed">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rotateAxes">
      <value value="108"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ShowForces">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Temp">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tickstop">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tnum1">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tnum2" first="20" step="5" last="40"/>
    <enumeratedValueSet variable="tnum3">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="track">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="xMean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="xOrigin">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="xSpatialDist">
      <value value="&quot;Normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="xStd">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="xwidth">
      <value value="13"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yMean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yOrigin">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ySpatialDist">
      <value value="&quot;Normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yStd">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ywidth">
      <value value="35"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="expa2" repetitions="1" runMetricsEveryStep="false">
    <setup>let fn (word "data/" behaviorspace-experiment-name "/views" )
let fp (word "data/" behaviorspace-experiment-name "/plots" )
let rv (shell:exec "mkdir" "-p" fn fp)
set tribes true
set number 0
setup
set number tnum1
setmore

set number tnum2
set mass-mean mass-mean ^ 0.05
setmore

set number tnum3
set mass-mean mass-mean * 0.005
setmore

set tribematrix matrix:make-identity 2
set tribematrix matrix:from-row-list [[1 -1 -1] [1 1 -1] [-1 -1 1]]</setup>
    <go>go</go>
    <postRun>let fn (word "data/" behaviorspace-experiment-name "/views/view" behaviorspace-run-number ".png")
let fp (word "data/" behaviorspace-experiment-name "/plots/plot" behaviorspace-run-number ".csv")
let wd (word "data/" behaviorspace-experiment-name "/plots/world" behaviorspace-run-number ".csv")
export-view fn
export-world wd</postRun>
    <timeLimit steps="5000"/>
    <metric>attractive</metric>
    <metric>agentcount</metric>
    <metric>ctrmassv</metric>
    <metric>ctrmassvr</metric>
    <metric>dcnsd</metric>
    <metric>elapsed-time</metric>
    <metric>floc</metric>
    <metric>groupcount</metric>
    <metric>grppol</metric>
    <metric>grppolm</metric>
    <metric>grprad</metric>
    <metric>grpradm</metric>
    <metric>iterations</metric>
    <metric>masserror</metric>
    <metric>massratio</metric>
    <metric>meandist</metric>
    <metric>meandistclosestneighbor</metric>
    <metric>meandistorigin</metric>
    <metric>meanxloc</metric>
    <metric>meanyloc</metric>
    <metric>nspv</metric>
    <metric>nspvr</metric>
    <metric>oobcount</metric>
    <metric>peakdist</metric>
    <metric>radicalization</metric>
    <metric>repulsive</metric>
    <metric>rseedused</metric>
    <metric>stopreason</metric>
    <metric>timeend</metric>
    <metric>timestart</metric>
    <metric>totmass</metric>
    <metric>valchg</metric>
    <metric>valchgmax</metric>
    <metric>valchgmin</metric>
    <enumeratedValueSet variable="active-mass-distribution">
      <value value="&quot;Normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="AdaptiveDT">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-color">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="AgentStop">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="AIB">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CoalescenseRadius">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debug">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DistanceExponent">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dt">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DTStop">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="elapsedT">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="filldistance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fill">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forceAdj">
      <value value="&quot;Normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fxcenter">
      <value value="9.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fxwidth">
      <value value="84.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fycenter">
      <value value="35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fywidth">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="g">
      <value value="7.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gamp">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Geometry">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="glevel">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gperiod">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gradtype">
      <value value="&quot;linear&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gravitycycle">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gshift">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infprob">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-mass">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="InjectionMaxMass">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Kx">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ky">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mass-mean">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mass-stddev">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maxmass">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metrics">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nsides">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NumberInjected">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="passive-mass-distribution">
      <value value="&quot;Normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="PassiveMass">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="phaseiteration">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pmass-mean">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pmass-stddev">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pradius">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Prange">
      <value value="48"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ProbDist">
      <value value="&quot;None&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pstep">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pxcenter">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pycenter">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomseed">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rotateAxes">
      <value value="108"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ShowForces">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Temp">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tickstop">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tnum1" first="1" step="1" last="10"/>
    <steppedValueSet variable="tnum2" first="10" step="5" last="40"/>
    <enumeratedValueSet variable="tnum3">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="track">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="xMean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="xOrigin">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="xSpatialDist">
      <value value="&quot;Normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="xStd">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="xwidth">
      <value value="13"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yMean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yOrigin">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ySpatialDist">
      <value value="&quot;Normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yStd">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ywidth">
      <value value="35"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="expa3" repetitions="1" runMetricsEveryStep="false">
    <setup>let fn (word "data/" behaviorspace-experiment-name "/views" )
let fp (word "data/" behaviorspace-experiment-name "/plots" )
let rv (shell:exec "mkdir" "-p" fn fp)
set tribes true
set number 0
setup
set number tnum1
setmore

set number tnum2
set mass-mean mass-mean ^ 0.05
setmore

set number tnum3
set mass-mean mass-mean * 0.005
setmore

set tribematrix matrix:make-identity 2
set tribematrix matrix:from-row-list [[1 -1 -1] [1 1 -1] [1 1 1]]</setup>
    <go>go</go>
    <postRun>let fn (word "data/" behaviorspace-experiment-name "/views/view" behaviorspace-run-number ".png")
let fp (word "data/" behaviorspace-experiment-name "/plots/plot" behaviorspace-run-number ".csv")
let wd (word "data/" behaviorspace-experiment-name "/plots/world" behaviorspace-run-number ".csv")
export-view fn
export-world wd</postRun>
    <metric>attractive</metric>
    <metric>agentcount</metric>
    <metric>ctrmassv</metric>
    <metric>ctrmassvr</metric>
    <metric>dcnsd</metric>
    <metric>elapsed-time</metric>
    <metric>floc</metric>
    <metric>groupcount</metric>
    <metric>grppol</metric>
    <metric>grppolm</metric>
    <metric>grprad</metric>
    <metric>grpradm</metric>
    <metric>iterations</metric>
    <metric>masserror</metric>
    <metric>massratio</metric>
    <metric>meandist</metric>
    <metric>meandistclosestneighbor</metric>
    <metric>meandistorigin</metric>
    <metric>meanxloc</metric>
    <metric>meanyloc</metric>
    <metric>nspv</metric>
    <metric>nspvr</metric>
    <metric>oobcount</metric>
    <metric>peakdist</metric>
    <metric>radicalization</metric>
    <metric>repulsive</metric>
    <metric>rseedused</metric>
    <metric>stopreason</metric>
    <metric>timeend</metric>
    <metric>timestart</metric>
    <metric>totmass</metric>
    <metric>valchg</metric>
    <metric>valchgmax</metric>
    <metric>valchgmin</metric>
    <enumeratedValueSet variable="active-mass-distribution">
      <value value="&quot;Normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="AdaptiveDT">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-color">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="AgentStop">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="AIB">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CoalescenseRadius">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debug">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DistanceExponent">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dt">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DTStop">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="elapsedT">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="filldistance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fill">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forceAdj">
      <value value="&quot;Normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fxcenter">
      <value value="9.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fxwidth">
      <value value="84.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fycenter">
      <value value="35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fywidth">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="g">
      <value value="7.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gamp">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Geometry">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="glevel">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gperiod">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gradtype">
      <value value="&quot;linear&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gravitycycle">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gshift">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infprob">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-mass">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="InjectionMaxMass">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Kx">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ky">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mass-mean">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mass-stddev">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maxmass">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metrics">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nsides">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NumberInjected">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="passive-mass-distribution">
      <value value="&quot;Normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="PassiveMass">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="phaseiteration">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pmass-mean">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pmass-stddev">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pradius">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Prange">
      <value value="48"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ProbDist">
      <value value="&quot;None&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pstep">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pxcenter">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pycenter">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomseed">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rotateAxes">
      <value value="108"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ShowForces">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Temp">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tickstop">
      <value value="6000"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tnum1" first="1" step="1" last="10"/>
    <steppedValueSet variable="tnum2" first="10" step="5" last="40"/>
    <enumeratedValueSet variable="tnum3">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="track">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="xMean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="xOrigin">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="xSpatialDist">
      <value value="&quot;Normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="xStd">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="xwidth">
      <value value="13"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yMean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yOrigin">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ySpatialDist">
      <value value="&quot;Normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yStd">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ywidth">
      <value value="35"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="expa4" repetitions="1" runMetricsEveryStep="false">
    <setup>let fn (word "data/" behaviorspace-experiment-name "/views" )
let fp (word "data/" behaviorspace-experiment-name "/plots" )
let rv (shell:exec "mkdir" "-p" fn fp)
set tribes true
set number 0
setup
set number tnum1
setmore

set number tnum2
set mass-mean mass-mean ^ tribemassratio
setmore

set number tnum3
set mass-mean mass-mean * tribemassratio
setmore
set tickstop 5
set tribematrix matrix:make-identity 2
set tribematrix matrix:from-row-list [[1 -1 -1] [1 1 -1] [1 1 1]]</setup>
    <go>go</go>
    <postRun>let fn (word "data/" behaviorspace-experiment-name "/views/view" behaviorspace-run-number ".png")
let fp (word "data/" behaviorspace-experiment-name "/plots/plot" behaviorspace-run-number ".csv")
let wd (word "data/" behaviorspace-experiment-name "/plots/world" behaviorspace-run-number ".csv")
export-view fn
export-world wd</postRun>
    <timeLimit steps="10"/>
    <metric>attractive</metric>
    <metric>agentcount</metric>
    <metric>ctrmassv</metric>
    <metric>ctrmassvr</metric>
    <metric>dcnsd</metric>
    <metric>elapsed-time</metric>
    <metric>floc</metric>
    <metric>groupcount</metric>
    <metric>grppol</metric>
    <metric>grppolm</metric>
    <metric>grprad</metric>
    <metric>grpradm</metric>
    <metric>iterations</metric>
    <metric>masserror</metric>
    <metric>massratio</metric>
    <metric>meandist</metric>
    <metric>meandistclosestneighbor</metric>
    <metric>meandistorigin</metric>
    <metric>meanxloc</metric>
    <metric>meanyloc</metric>
    <metric>nspv</metric>
    <metric>nspvr</metric>
    <metric>oobcount</metric>
    <metric>peakdist</metric>
    <metric>radicalization</metric>
    <metric>repulsive</metric>
    <metric>rseedused</metric>
    <metric>stopreason</metric>
    <metric>timeend</metric>
    <metric>timestart</metric>
    <metric>totmass</metric>
    <metric>valchg</metric>
    <metric>valchgmax</metric>
    <metric>valchgmin</metric>
    <enumeratedValueSet variable="active-mass-distribution">
      <value value="&quot;Normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="AdaptiveDT">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-color">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="AgentStop">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="AIB">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CoalescenseRadius">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debug">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DistanceExponent">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dt">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DTStop">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="elapsedT">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="filldistance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fill">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forceAdj">
      <value value="&quot;Normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fxcenter">
      <value value="9.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fxwidth">
      <value value="84.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fycenter">
      <value value="35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fywidth">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="g">
      <value value="7.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gamp">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Geometry">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="glevel">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gperiod">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gradtype">
      <value value="&quot;linear&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gravitycycle">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gshift">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infprob">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-mass">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="InjectionMaxMass">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Kx">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ky">
      <value value="0.001"/>
    </enumeratedValueSet>
    <steppedValueSet variable="mass-mean" first="5000" step="500" last="10000"/>
    <enumeratedValueSet variable="mass-stddev">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maxmass">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metrics">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nsides">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NumberInjected">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="passive-mass-distribution">
      <value value="&quot;Normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="PassiveMass">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="phaseiteration">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pmass-mean">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pmass-stddev">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pradius">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Prange">
      <value value="48"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ProbDist">
      <value value="&quot;None&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pstep">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pxcenter">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pycenter">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomseed">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rotateAxes">
      <value value="108"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ShowForces">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Temp">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tickstop">
      <value value="6000"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tnum1" first="2" step="2" last="10"/>
    <steppedValueSet variable="tnum2" first="10" step="5" last="40"/>
    <enumeratedValueSet variable="tnum3">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="track">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tribemassratio" first="0.1" step="0.07" last="0.3"/>
    <enumeratedValueSet variable="xMean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="xOrigin">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="xSpatialDist">
      <value value="&quot;Normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="xStd">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="xwidth">
      <value value="13"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yMean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yOrigin">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ySpatialDist">
      <value value="&quot;Normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yStd">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ywidth">
      <value value="35"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="expa4d" repetitions="1" runMetricsEveryStep="false">
    <preExperiment>let fn (word "data/" behaviorspace-experiment-name "/views" )
let fp (word "data/" behaviorspace-experiment-name "/plots" )
let rv (shell:exec "mkdir" "-p" fn fp)
clear-all</preExperiment>
    <setup>set tribes true
set number 0
setup

set number tnum1
set mass-mean tribemassmean
setmore

set number tnum2
set mass-mean tribemassmean * tribemassratio
setmore

set number tnum3
set mass-mean mass-mean * tribemassratio
setmore
set tickstop 4000

set tribematrix matrix:make-identity 3
set tribematrix matrix:from-row-list [[1 -1 -1] [1 1 -1] [1 1 1]]</setup>
    <go>go</go>
    <postRun>let fn (word "data/" behaviorspace-experiment-name "/views/view" behaviorspace-run-number ".png")
let fp (word "data/" behaviorspace-experiment-name "/plots/plot" behaviorspace-run-number ".csv")
let wd (word "data/" behaviorspace-experiment-name "/plots/world" behaviorspace-run-number ".csv")
export-view fn
export-world wd</postRun>
    <metric>attractive</metric>
    <metric>agentcount</metric>
    <metric>ctrmassv</metric>
    <metric>ctrmassvr</metric>
    <metric>dcnsd</metric>
    <metric>elapsed-time</metric>
    <metric>floc</metric>
    <metric>groupcount</metric>
    <metric>grppol</metric>
    <metric>grppolm</metric>
    <metric>grprad</metric>
    <metric>grpradm</metric>
    <metric>iterations</metric>
    <metric>masserror</metric>
    <metric>massratio</metric>
    <metric>meandist</metric>
    <metric>meandistclosestneighbor</metric>
    <metric>meandistorigin</metric>
    <metric>meanxloc</metric>
    <metric>meanyloc</metric>
    <metric>nspv</metric>
    <metric>nspvr</metric>
    <metric>oobcount</metric>
    <metric>peakdist</metric>
    <metric>radicalization</metric>
    <metric>repulsive</metric>
    <metric>rseedused</metric>
    <metric>stopreason</metric>
    <metric>timeend</metric>
    <metric>timestart</metric>
    <metric>totmass</metric>
    <metric>valchg</metric>
    <metric>valchgmax</metric>
    <metric>valchgmin</metric>
    <enumeratedValueSet variable="tribemassratio">
      <value value="0.05"/>
      <value value="0.08"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tribemassmean">
      <value value="1000"/>
      <value value="2000"/>
      <value value="3000"/>
      <value value="4000"/>
      <value value="5000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="expa5" repetitions="1" runMetricsEveryStep="false">
    <preExperiment>clear-all</preExperiment>
    <setup>let fn (word "data/" behaviorspace-experiment-name "/views" )
let fp (word "data/" behaviorspace-experiment-name "/plots" )
let rv (shell:exec "mkdir" "-p" fn fp)

set tribes true
set number 0
setup

set number tnum1
set mass-mean tribemassmean
setmore

set number tnum2
set mass-mean tribemassmean * tribemassratio
setmore

set number tnum3
set mass-mean mass-mean * tribemassratio
setmore
set tickstop 4000

set tribematrix matrix:make-identity 3
set tribematrix matrix:from-row-list [[1 -1 -1] [1 1 -1] [1 1 1]]</setup>
    <go>go</go>
    <postRun>let fn (word "data/" behaviorspace-experiment-name "/views/view" behaviorspace-run-number ".png")
let fp (word "data/" behaviorspace-experiment-name "/plots/plot" behaviorspace-run-number ".csv")
let wd (word "data/" behaviorspace-experiment-name "/plots/world" behaviorspace-run-number ".csv")
export-view fn
export-world wd</postRun>
    <metric>activeForIter</metric>
    <metric>agentcount</metric>
    <metric>attractive</metric>
    <metric>breed-change</metric>
    <metric>center-of-mass-xc</metric>
    <metric>center-of-mass-yc</metric>
    <metric>ctrmassv</metric>
    <metric>ctrmassvr</metric>
    <metric>dcnsd</metric>
    <metric>dcnsum</metric>
    <metric>elapsed-time</metric>
    <metric>euclidflag</metric>
    <metric>finaldt</metric>
    <metric>floc</metric>
    <metric>forcescale</metric>
    <metric>groupcount</metric>
    <metric>grppol</metric>
    <metric>grppolm</metric>
    <metric>grprad</metric>
    <metric>grpradm</metric>
    <metric>itcounter</metric>
    <metric>iterations</metric>
    <metric>masscale</metric>
    <metric>masserror</metric>
    <metric>massratio</metric>
    <metric>maxdist</metric>
    <metric>maxforce</metric>
    <metric>meandist</metric>
    <metric>meandistclosestneighbor</metric>
    <metric>meandistorigin</metric>
    <metric>meanxloc</metric>
    <metric>meanxsd</metric>
    <metric>meanyloc</metric>
    <metric>meanysd</metric>
    <metric>mergeCount</metric>
    <metric>mergeTotMass</metric>
    <metric>mominertia</metric>
    <metric>nspv</metric>
    <metric>nspvr</metric>
    <metric>oobcount</metric>
    <metric>pax</metric>
    <metric>pay</metric>
    <metric>pbx</metric>
    <metric>pby</metric>
    <metric>peakdist</metric>
    <metric>phasecount</metric>
    <metric>phaseneg</metric>
    <metric>phasepos</metric>
    <metric>phasetrans</metric>
    <metric>radicalization</metric>
    <metric>radmax</metric>
    <metric>record-interface</metric>
    <metric>record-view</metric>
    <metric>repulsive</metric>
    <metric>rseedused</metric>
    <metric>scale</metric>
    <metric>stopflag</metric>
    <metric>stopreason</metric>
    <metric>timeend</metric>
    <metric>timestart</metric>
    <metric>tmat11</metric>
    <metric>tmat12</metric>
    <metric>tmat13</metric>
    <metric>tmat14</metric>
    <metric>tmat21</metric>
    <metric>tmat22</metric>
    <metric>tmat23</metric>
    <metric>tmat24</metric>
    <metric>tmat31</metric>
    <metric>tmat32</metric>
    <metric>tmat33</metric>
    <metric>tmat34</metric>
    <metric>tmat41</metric>
    <metric>tmat42</metric>
    <metric>tmat43</metric>
    <metric>tmat44</metric>
    <metric>totmass</metric>
    <metric>tribecolorname</metric>
    <metric>tribecolors</metric>
    <metric>tribecount</metric>
    <metric>tribefinalmass</metric>
    <metric>tribefinalmassP</metric>
    <metric>tribeinitmass</metric>
    <metric>tribeinitmassP</metric>
    <metric>tribemassmean</metric>
    <metric>tribemassratio</metric>
    <metric>tribematrix</metric>
    <metric>tribenum</metric>
    <metric>valchg</metric>
    <metric>valchgmax</metric>
    <metric>valchgmin</metric>
    <enumeratedValueSet variable="tribemassratio">
      <value value="0.05"/>
      <value value="0.08"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tribemassmean">
      <value value="1000"/>
      <value value="2000"/>
      <value value="3000"/>
      <value value="4000"/>
      <value value="5000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="expa6" repetitions="1" runMetricsEveryStep="false">
    <preExperiment>clear-all</preExperiment>
    <setup>setup</setup>
    <go>set tnum1 covariance
recenter
set tnum2 covariance
stop</go>
    <metric>number</metric>
    <metric>tnum1</metric>
    <metric>tnum2</metric>
    <enumeratedValueSet variable="number">
      <value value="2"/>
      <value value="10"/>
      <value value="50"/>
      <value value="200"/>
      <value value="500"/>
      <value value="1000"/>
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomseed">
      <value value="1"/>
      <value value="-200"/>
      <value value="400"/>
      <value value="-1010101"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="expa7" repetitions="1" runMetricsEveryStep="false">
    <preExperiment>clear-all</preExperiment>
    <setup>let fn (word "data/" behaviorspace-experiment-name "/views" )
let fp (word "data/" behaviorspace-experiment-name "/plots" )
let rv (shell:exec "mkdir" "-p" fn fp)
set tribes true
set number 0
setup

set number 100
setmore

set xMean xMean * -1
setmore


set tribematrix matrix:make-identity 2
set tribematrix matrix:from-row-list [[1 0] [0 1]]</setup>
    <go>go</go>
    <postRun>let fn (word "data/" behaviorspace-experiment-name "/views/view" behaviorspace-run-number ".png")
let fp (word "data/" behaviorspace-experiment-name "/plots/plot" behaviorspace-run-number ".csv")
let wd (word "data/" behaviorspace-experiment-name "/plots/world" behaviorspace-run-number ".csv")
export-view fn
export-world wd</postRun>
    <metric>attractive</metric>
    <metric>agentcount</metric>
    <metric>ctrmassv</metric>
    <metric>ctrmassvr</metric>
    <metric>dcnsd</metric>
    <metric>elapsed-time</metric>
    <metric>floc</metric>
    <metric>groupcount</metric>
    <metric>grppol</metric>
    <metric>grppolm</metric>
    <metric>grprad</metric>
    <metric>grpradm</metric>
    <metric>iterations</metric>
    <metric>masserror</metric>
    <metric>massratio</metric>
    <metric>meandist</metric>
    <metric>meandistclosestneighbor</metric>
    <metric>meandistorigin</metric>
    <metric>meanxloc</metric>
    <metric>meanyloc</metric>
    <metric>nspv</metric>
    <metric>nspvr</metric>
    <metric>oobcount</metric>
    <metric>peakdist</metric>
    <metric>radicalization</metric>
    <metric>repulsive</metric>
    <metric>rseedused</metric>
    <metric>stopreason</metric>
    <metric>timeend</metric>
    <metric>timestart</metric>
    <metric>totmass</metric>
    <metric>valchg</metric>
    <metric>valchgmax</metric>
    <metric>valchgmin</metric>
    <enumeratedValueSet variable="number">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomseed">
      <value value="1"/>
      <value value="-200"/>
      <value value="400"/>
      <value value="-1010101"/>
    </enumeratedValueSet>
    <steppedValueSet variable="xMean" first="-10" step="3" last="10"/>
  </experiment>
  <experiment name="expa8" repetitions="10" runMetricsEveryStep="false">
    <preExperiment>clear-all</preExperiment>
    <setup>let fn (word "data/" behaviorspace-experiment-name "/views" )
let fp (word "data/" behaviorspace-experiment-name "/plots" )
let rv (shell:exec "mkdir" "-p" fn fp)
set tribes true
set number 0
setup

set number 100
setmore

set xMean xMean * -1
setmore


set tribematrix matrix:make-identity 2
set tribematrix matrix:from-row-list [[1 0] [0 1]]</setup>
    <go>go</go>
    <postRun>let fn (word "data/" behaviorspace-experiment-name "/views/view" behaviorspace-run-number ".png")
let fp (word "data/" behaviorspace-experiment-name "/plots/plot" behaviorspace-run-number ".csv")
let wd (word "data/" behaviorspace-experiment-name "/plots/world" behaviorspace-run-number ".csv")
export-view fn
export-world wd</postRun>
    <metric>attractive</metric>
    <metric>agentcount</metric>
    <metric>ctrmassv</metric>
    <metric>ctrmassvr</metric>
    <metric>dcnsd</metric>
    <metric>elapsed-time</metric>
    <metric>floc</metric>
    <metric>groupcount</metric>
    <metric>grppol</metric>
    <metric>grppolm</metric>
    <metric>grprad</metric>
    <metric>grpradm</metric>
    <metric>iterations</metric>
    <metric>masserror</metric>
    <metric>massratio</metric>
    <metric>meandist</metric>
    <metric>meandistclosestneighbor</metric>
    <metric>meandistorigin</metric>
    <metric>meanxloc</metric>
    <metric>meanyloc</metric>
    <metric>nspv</metric>
    <metric>nspvr</metric>
    <metric>oobcount</metric>
    <metric>peakdist</metric>
    <metric>radicalization</metric>
    <metric>repulsive</metric>
    <metric>rseedused</metric>
    <metric>stopreason</metric>
    <metric>timeend</metric>
    <metric>timestart</metric>
    <metric>totmass</metric>
    <metric>valchg</metric>
    <metric>valchgmax</metric>
    <metric>valchgmin</metric>
    <enumeratedValueSet variable="number">
      <value value="50"/>
    </enumeratedValueSet>
    <steppedValueSet variable="xMean" first="-10" step="1" last="10"/>
  </experiment>
  <experiment name="expa9" repetitions="10" runMetricsEveryStep="false">
    <preExperiment>clear-all</preExperiment>
    <setup>let fn (word "data/" behaviorspace-experiment-name "/views" )
let fp (word "data/" behaviorspace-experiment-name "/plots" )
let rv (shell:exec "mkdir" "-p" fn fp)
set tribes true
set number 0
setup

set number 100
setmore

set xMean xMean * -1
setmore


set tribematrix matrix:make-identity 2
set tribematrix matrix:from-row-list [[1 0] [0 1]]</setup>
    <go>go</go>
    <postRun>let fn (word "data/" behaviorspace-experiment-name "/views/view" behaviorspace-run-number ".png")
let fp (word "data/" behaviorspace-experiment-name "/plots/plot" behaviorspace-run-number ".csv")
let wd (word "data/" behaviorspace-experiment-name "/plots/world" behaviorspace-run-number ".csv")
export-view fn
export-world wd</postRun>
    <metric>attractive</metric>
    <metric>agentcount</metric>
    <metric>ctrmassv</metric>
    <metric>ctrmassvr</metric>
    <metric>dcnsd</metric>
    <metric>elapsed-time</metric>
    <metric>floc</metric>
    <metric>groupcount</metric>
    <metric>grppol</metric>
    <metric>grppolm</metric>
    <metric>grprad</metric>
    <metric>grpradm</metric>
    <metric>iterations</metric>
    <metric>masserror</metric>
    <metric>massratio</metric>
    <metric>meandist</metric>
    <metric>meandistclosestneighbor</metric>
    <metric>meandistorigin</metric>
    <metric>meanxloc</metric>
    <metric>meanyloc</metric>
    <metric>nspv</metric>
    <metric>nspvr</metric>
    <metric>number</metric>
    <metric>oobcount</metric>
    <metric>peakdist</metric>
    <metric>radicalization</metric>
    <metric>repulsive</metric>
    <metric>rseedused</metric>
    <metric>stopreason</metric>
    <metric>timeend</metric>
    <metric>timestart</metric>
    <metric>totmass</metric>
    <metric>valchg</metric>
    <metric>valchgmax</metric>
    <metric>valchgmin</metric>
    <enumeratedValueSet variable="number">
      <value value="50"/>
    </enumeratedValueSet>
    <steppedValueSet variable="xMean" first="0" step="2" last="30"/>
  </experiment>
  <experiment name="expa10" repetitions="1" runMetricsEveryStep="false">
    <preExperiment>clear-all</preExperiment>
    <setup>let fn (word "data/" behaviorspace-experiment-name "/views" )
let fp (word "data/" behaviorspace-experiment-name "/plots" )
let rv (shell:exec "mkdir" "-p" fn fp)
set tribes true
set number 0
setup

set number 50
let ty yMean
set yMean 0
setmore

set xMean 0
set yMean ty
setmore

set tribematrix matrix:make-identity 2
set tribematrix matrix:from-row-list [[1 1] [1 1]]</setup>
    <go>go</go>
    <postRun>let fn (word "data/" behaviorspace-experiment-name "/views/view" behaviorspace-run-number ".png")
let fp (word "data/" behaviorspace-experiment-name "/plots/plot" behaviorspace-run-number ".csv")
let wd (word "data/" behaviorspace-experiment-name "/plots/world" behaviorspace-run-number ".csv")
export-view fn
export-world wd</postRun>
    <metric>attractive</metric>
    <metric>agentcount</metric>
    <metric>ctrmassv</metric>
    <metric>ctrmassvr</metric>
    <metric>dcnsd</metric>
    <metric>elapsed-time</metric>
    <metric>floc</metric>
    <metric>groupcount</metric>
    <metric>grppol</metric>
    <metric>grppolm</metric>
    <metric>grprad</metric>
    <metric>grpradm</metric>
    <metric>iterations</metric>
    <metric>masserror</metric>
    <metric>massratio</metric>
    <metric>meandist</metric>
    <metric>meandistclosestneighbor</metric>
    <metric>meandistorigin</metric>
    <metric>meanxloc</metric>
    <metric>meanyloc</metric>
    <metric>nspv</metric>
    <metric>nspvr</metric>
    <metric>number</metric>
    <metric>oobcount</metric>
    <metric>peakdist</metric>
    <metric>radicalization</metric>
    <metric>repulsive</metric>
    <metric>rseedused</metric>
    <metric>stopreason</metric>
    <metric>tglobal</metric>
    <metric>timeend</metric>
    <metric>timestart</metric>
    <metric>tribematrix</metric>
    <metric>tribenum</metric>
    <metric>totmass</metric>
    <metric>valchg</metric>
    <metric>valchgmax</metric>
    <metric>valchgmin</metric>
    <metric>xMean</metric>
    <metric>yMean</metric>
    <steppedValueSet variable="xMean" first="0" step="2" last="20"/>
    <steppedValueSet variable="yMean" first="0" step="2" last="20"/>
  </experiment>
  <experiment name="expa11" repetitions="10" runMetricsEveryStep="false">
    <preExperiment>clear-all</preExperiment>
    <setup>let fn (word "data/" behaviorspace-experiment-name "/views" )
let fp (word "data/" behaviorspace-experiment-name "/plots" )
let rv (shell:exec "mkdir" "-p" fn fp)
set tribes true
set number 0
setup

set number 100
let ty yMean
set yMean 0
setmore

set xMean 0
set yMean ty
setmore

set tribematrix matrix:make-identity 2
set tribematrix matrix:from-row-list [[1 1] [1 1]]</setup>
    <go>go</go>
    <postRun>let fn (word "data/" behaviorspace-experiment-name "/views/view" behaviorspace-run-number ".png")
let fp (word "data/" behaviorspace-experiment-name "/plots/plot" behaviorspace-run-number ".csv")
let wd (word "data/" behaviorspace-experiment-name "/plots/world" behaviorspace-run-number ".csv")
export-view fn
export-world wd</postRun>
    <metric>attractive</metric>
    <metric>agentcount</metric>
    <metric>ctrmassv</metric>
    <metric>ctrmassvr</metric>
    <metric>dcnsd</metric>
    <metric>elapsed-time</metric>
    <metric>floc</metric>
    <metric>groupcount</metric>
    <metric>grppol</metric>
    <metric>grppolm</metric>
    <metric>grprad</metric>
    <metric>grpradm</metric>
    <metric>iterations</metric>
    <metric>masserror</metric>
    <metric>massratio</metric>
    <metric>meandist</metric>
    <metric>meandistclosestneighbor</metric>
    <metric>meandistorigin</metric>
    <metric>meanxloc</metric>
    <metric>meanyloc</metric>
    <metric>nspv</metric>
    <metric>nspvr</metric>
    <metric>number</metric>
    <metric>oobcount</metric>
    <metric>peakdist</metric>
    <metric>radicalization</metric>
    <metric>repulsive</metric>
    <metric>rseedused</metric>
    <metric>stopreason</metric>
    <metric>tglobal</metric>
    <metric>timeend</metric>
    <metric>timestart</metric>
    <metric>tribematrix</metric>
    <metric>tribenum</metric>
    <metric>totmass</metric>
    <metric>valchg</metric>
    <metric>valchgmax</metric>
    <metric>valchgmin</metric>
    <metric>xMean</metric>
    <metric>yMean</metric>
    <steppedValueSet variable="xMean" first="0" step="2" last="20"/>
    <steppedValueSet variable="yMean" first="0" step="2" last="20"/>
  </experiment>
  <experiment name="expa12" repetitions="1" runMetricsEveryStep="false">
    <preExperiment>clear-all</preExperiment>
    <setup>let fn (word "data/" behaviorspace-experiment-name "/views" )
let fp (word "data/" behaviorspace-experiment-name "/plots" )
let rv (shell:exec "mkdir" "-p" fn fp)
set tribes true
set number 0
setup

set number 100
let ty yMean
set yMean 0
setmore

set xMean 0
set yMean ty
setmore

set tribematrix matrix:make-identity 2
set tribematrix matrix:from-row-list [[1 1] [1 1]]</setup>
    <go>go</go>
    <postRun>let fn (word "data/" behaviorspace-experiment-name "/views/view" behaviorspace-run-number ".png")
let fp (word "data/" behaviorspace-experiment-name "/plots/plot" behaviorspace-run-number ".csv")
let wd (word "data/" behaviorspace-experiment-name "/plots/world" behaviorspace-run-number ".csv")
export-view fn
export-world wd</postRun>
    <metric>attractive</metric>
    <metric>agentcount</metric>
    <metric>ctrmassv</metric>
    <metric>ctrmassvr</metric>
    <metric>dcnsd</metric>
    <metric>elapsed-time</metric>
    <metric>floc</metric>
    <metric>groupcount</metric>
    <metric>grppol</metric>
    <metric>grppolm</metric>
    <metric>grprad</metric>
    <metric>grpradm</metric>
    <metric>iterations</metric>
    <metric>masserror</metric>
    <metric>massratio</metric>
    <metric>meandist</metric>
    <metric>meandistclosestneighbor</metric>
    <metric>meandistorigin</metric>
    <metric>meanxloc</metric>
    <metric>meanyloc</metric>
    <metric>nspv</metric>
    <metric>nspvr</metric>
    <metric>number</metric>
    <metric>oobcount</metric>
    <metric>peakdist</metric>
    <metric>radicalization</metric>
    <metric>repulsive</metric>
    <metric>rseedused</metric>
    <metric>stopreason</metric>
    <metric>tglobal</metric>
    <metric>timeend</metric>
    <metric>timestart</metric>
    <metric>tribematrix</metric>
    <metric>tribenum</metric>
    <metric>totmass</metric>
    <metric>valchg</metric>
    <metric>valchgmax</metric>
    <metric>valchgmin</metric>
    <metric>xMean</metric>
    <metric>yMean</metric>
    <steppedValueSet variable="xMean" first="0" step="2" last="20"/>
    <steppedValueSet variable="yMean" first="0" step="2" last="20"/>
    <steppedValueSet variable="randomseed" first="-100000" step="8000" last="100000"/>
  </experiment>
  <experiment name="expa13" repetitions="1" runMetricsEveryStep="false">
    <preExperiment>clear-all</preExperiment>
    <setup>let fn (word "data/" behaviorspace-experiment-name "/views" )
let fp (word "data/" behaviorspace-experiment-name "/plots" )
let rv (shell:exec "mkdir" "-p" fn fp)
set tribes true
set number 0
setup

set number 100
setmore

setmore

set tribematrix matrix:make-identity 2
set tribematrix matrix:from-row-list [[1 1] [1 1]]

let fni (word "data/" behaviorspace-experiment-name "/views/viewinit" behaviorspace-run-number ".png")
export-view fni</setup>
    <go>go</go>
    <postRun>let fn (word "data/" behaviorspace-experiment-name "/views/view" behaviorspace-run-number ".png")
let fp (word "data/" behaviorspace-experiment-name "/plots/plot" behaviorspace-run-number ".csv")
let wd (word "data/" behaviorspace-experiment-name "/plots/world" behaviorspace-run-number ".csv")
export-view fn
export-world wd</postRun>
    <metric>attractive</metric>
    <metric>agentcount</metric>
    <metric>ctrmassv</metric>
    <metric>ctrmassvr</metric>
    <metric>dcnsd</metric>
    <metric>elapsed-time</metric>
    <metric>floc</metric>
    <metric>groupcount</metric>
    <metric>grppol</metric>
    <metric>grppolm</metric>
    <metric>grprad</metric>
    <metric>grpradm</metric>
    <metric>iterations</metric>
    <metric>masserror</metric>
    <metric>massratio</metric>
    <metric>meandist</metric>
    <metric>meandistclosestneighbor</metric>
    <metric>meandistorigin</metric>
    <metric>meanxloc</metric>
    <metric>meanyloc</metric>
    <metric>nspv</metric>
    <metric>nspvr</metric>
    <metric>number</metric>
    <metric>oobcount</metric>
    <metric>peakdist</metric>
    <metric>radicalization</metric>
    <metric>repulsive</metric>
    <metric>rseedused</metric>
    <metric>stopreason</metric>
    <metric>tglobal</metric>
    <metric>timeend</metric>
    <metric>timestart</metric>
    <metric>tribematrix</metric>
    <metric>tribenum</metric>
    <metric>totmass</metric>
    <metric>valchg</metric>
    <metric>valchgmax</metric>
    <metric>valchgmin</metric>
    <metric>xMean</metric>
    <metric>yMean</metric>
    <steppedValueSet variable="randomseed" first="-100000" step="8000" last="100000"/>
    <enumeratedValueSet variable="active-mass-distribution">
      <value value="&quot;Exponential&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="expa14" repetitions="1" runMetricsEveryStep="false">
    <preExperiment>clear-all</preExperiment>
    <setup>let fn (word "data/" behaviorspace-experiment-name "/views" )
let fp (word "data/" behaviorspace-experiment-name "/plots" )
let rv (shell:exec "mkdir" "-p" fn fp)
set tribes true
set number 0
setup

set number 100
setmore
setmore

set tribematrix matrix:make-identity 2
set tribematrix matrix:from-row-list [[1 1] [1 1]]

let fni (word "data/" behaviorspace-experiment-name "/views/viewinit" behaviorspace-run-number ".png")
export-view fni</setup>
    <go>go</go>
    <postRun>let fn (word "data/" behaviorspace-experiment-name "/views/view" behaviorspace-run-number ".png")
let fp (word "data/" behaviorspace-experiment-name "/plots/plot" behaviorspace-run-number ".csv")
let wd (word "data/" behaviorspace-experiment-name "/plots/world" behaviorspace-run-number ".csv")
export-view fn
export-world wd</postRun>
    <metric>attractive</metric>
    <metric>agentcount</metric>
    <metric>ctrmassv</metric>
    <metric>ctrmassvr</metric>
    <metric>dcnsd</metric>
    <metric>elapsed-time</metric>
    <metric>floc</metric>
    <metric>groupcount</metric>
    <metric>grppol</metric>
    <metric>grppolm</metric>
    <metric>grprad</metric>
    <metric>grpradm</metric>
    <metric>iterations</metric>
    <metric>masserror</metric>
    <metric>massratio</metric>
    <metric>meandist</metric>
    <metric>meandistclosestneighbor</metric>
    <metric>meandistorigin</metric>
    <metric>meanxloc</metric>
    <metric>meanyloc</metric>
    <metric>nspv</metric>
    <metric>nspvr</metric>
    <metric>number</metric>
    <metric>oobcount</metric>
    <metric>peakdist</metric>
    <metric>radicalization</metric>
    <metric>repulsive</metric>
    <metric>rseedused</metric>
    <metric>stopreason</metric>
    <metric>tglobal</metric>
    <metric>timeend</metric>
    <metric>timestart</metric>
    <metric>tribematrix</metric>
    <metric>tribenum</metric>
    <metric>totmass</metric>
    <metric>valchg</metric>
    <metric>valchgmax</metric>
    <metric>valchgmin</metric>
    <metric>xMean</metric>
    <metric>yMean</metric>
    <steppedValueSet variable="randomseed" first="-100000" step="4000" last="100000"/>
    <enumeratedValueSet variable="active-mass-distribution">
      <value value="&quot;Exponential&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="expa15" repetitions="1" runMetricsEveryStep="false">
    <preExperiment>clear-all</preExperiment>
    <setup>let fn (word "data/" behaviorspace-experiment-name "/views" )
let fp (word "data/" behaviorspace-experiment-name "/plots" )
let rv (shell:exec "mkdir" "-p" fn fp)
set tribes true
set number 0
setup
set number 100
let ty yMean
set yMean 0
setmore
set xMean 0
set yMean ty
setmore
let fni (word "data/" behaviorspace-experiment-name "/views/viewinit" behaviorspace-run-number ".png")
export-view fni</setup>
    <go>go</go>
    <postRun>let fn (word "data/" behaviorspace-experiment-name "/views/view" behaviorspace-run-number ".png")
let fp (word "data/" behaviorspace-experiment-name "/plots/plot" behaviorspace-run-number ".csv")
let wd (word "data/" behaviorspace-experiment-name "/plots/world" behaviorspace-run-number ".csv")
export-view fn
export-world wd</postRun>
    <timeLimit steps="6000"/>
    <metric>attractive</metric>
    <metric>agentcount</metric>
    <metric>ctrmassv</metric>
    <metric>ctrmassvr</metric>
    <metric>dcnsd</metric>
    <metric>elapsed-time</metric>
    <metric>floc</metric>
    <metric>groupcount</metric>
    <metric>grppol</metric>
    <metric>grppolm</metric>
    <metric>grprad</metric>
    <metric>grpradm</metric>
    <metric>iterations</metric>
    <metric>masserror</metric>
    <metric>massratio</metric>
    <metric>meandist</metric>
    <metric>meandistclosestneighbor</metric>
    <metric>meandistorigin</metric>
    <metric>meanxloc</metric>
    <metric>meanyloc</metric>
    <metric>nspv</metric>
    <metric>nspvr</metric>
    <metric>number</metric>
    <metric>oobcount</metric>
    <metric>peakdist</metric>
    <metric>radicalization</metric>
    <metric>repulsive</metric>
    <metric>rseedused</metric>
    <metric>stopreason</metric>
    <metric>tglobal</metric>
    <metric>timeend</metric>
    <metric>timestart</metric>
    <metric>tmat11</metric>
    <metric>tmat12</metric>
    <metric>tmat21</metric>
    <metric>tmat22</metric>
    <metric>tribematrix</metric>
    <metric>tribenum</metric>
    <metric>totmass</metric>
    <metric>valchg</metric>
    <metric>valchgmax</metric>
    <metric>valchgmin</metric>
    <metric>xMean</metric>
    <metric>yMean</metric>
    <enumeratedValueSet variable="active-mass-distribution">
      <value value="&quot;Exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomseed">
      <value value="-98765432"/>
      <value value="23987654"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tmat11">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tmat12">
      <value value="0.7"/>
      <value value="1"/>
      <value value="1.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tmat21">
      <value value="0.7"/>
      <value value="1"/>
      <value value="1.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tmat22">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="xMean" first="0" step="5" last="21"/>
    <steppedValueSet variable="yMean" first="0" step="5" last="21"/>
  </experiment>
  <experiment name="expa15a" repetitions="1" runMetricsEveryStep="false">
    <preExperiment>clear-all</preExperiment>
    <setup>let fn (word "data/" behaviorspace-experiment-name "/views" )
let fp (word "data/" behaviorspace-experiment-name "/plots" )
let rv (shell:exec "mkdir" "-p" fn fp)
set tribes true
set number 0
show (word "before setup tmat11 " tmat11 " " tmat12)
setup
show (word "after setup tmat11 " tmat11 " " tmat12)
set number 100
let ty yMean
set yMean 0
setmore
set xMean 0
set yMean ty
setmore

let fni (word "data/" behaviorspace-experiment-name "/views/viewinit" behaviorspace-run-number ".png")
export-view fni</setup>
    <go>go</go>
    <postRun>let fn (word "data/" behaviorspace-experiment-name "/views/view" behaviorspace-run-number ".png")
let fp (word "data/" behaviorspace-experiment-name "/plots/plot" behaviorspace-run-number ".csv")
let wd (word "data/" behaviorspace-experiment-name "/plots/world" behaviorspace-run-number ".csv")
export-view fn
export-world wd</postRun>
    <metric>attractive</metric>
    <metric>agentcount</metric>
    <metric>ctrmassv</metric>
    <metric>ctrmassvr</metric>
    <metric>dcnsd</metric>
    <metric>elapsed-time</metric>
    <metric>floc</metric>
    <metric>groupcount</metric>
    <metric>grppol</metric>
    <metric>grppolm</metric>
    <metric>grprad</metric>
    <metric>grpradm</metric>
    <metric>iterations</metric>
    <metric>masserror</metric>
    <metric>massratio</metric>
    <metric>meandist</metric>
    <metric>meandistclosestneighbor</metric>
    <metric>meandistorigin</metric>
    <metric>meanxloc</metric>
    <metric>meanyloc</metric>
    <metric>nspv</metric>
    <metric>nspvr</metric>
    <metric>number</metric>
    <metric>oobcount</metric>
    <metric>peakdist</metric>
    <metric>radicalization</metric>
    <metric>repulsive</metric>
    <metric>rseedused</metric>
    <metric>stopreason</metric>
    <metric>tglobal</metric>
    <metric>timeend</metric>
    <metric>timestart</metric>
    <metric>tmat11</metric>
    <metric>tmat12</metric>
    <metric>tmat21</metric>
    <metric>tmat22</metric>
    <metric>tribematrix</metric>
    <metric>tribenum</metric>
    <metric>totmass</metric>
    <metric>valchg</metric>
    <metric>valchgmax</metric>
    <metric>valchgmin</metric>
    <metric>xMean</metric>
    <metric>yMean</metric>
    <enumeratedValueSet variable="randomseed">
      <value value="-100000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="active-mass-distribution">
      <value value="&quot;Exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tmat11">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tmat12">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tmat21">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tmat22">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="xMean">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yMean">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="expa15b" repetitions="1" runMetricsEveryStep="false">
    <preExperiment>clear-all</preExperiment>
    <setup>let fn (word "data/" behaviorspace-experiment-name "/views" )
let fp (word "data/" behaviorspace-experiment-name "/plots" )
let rv (shell:exec "mkdir" "-p" fn fp)
set tribes true
set number 0
setup
set number 100
let ty yMean
set yMean 0
setmore
set xMean 0
set yMean ty
setmore
show (word "tmat12 tmat21" tmat12 " " tmat21)
let fni (word "data/" behaviorspace-experiment-name "/views/viewinit" behaviorspace-run-number ".png")
export-view fni</setup>
    <go>go</go>
    <postRun>let fn (word "data/" behaviorspace-experiment-name "/views/view" behaviorspace-run-number ".png")
let fp (word "data/" behaviorspace-experiment-name "/plots/plot" behaviorspace-run-number ".csv")
let wd (word "data/" behaviorspace-experiment-name "/plots/world" behaviorspace-run-number ".csv")
export-view fn
export-world wd</postRun>
    <timeLimit steps="6000"/>
    <metric>attractive</metric>
    <metric>agentcount</metric>
    <metric>ctrmassv</metric>
    <metric>ctrmassvr</metric>
    <metric>dcnsd</metric>
    <metric>elapsed-time</metric>
    <metric>floc</metric>
    <metric>groupcount</metric>
    <metric>grppol</metric>
    <metric>grppolm</metric>
    <metric>grprad</metric>
    <metric>grpradm</metric>
    <metric>iterations</metric>
    <metric>masserror</metric>
    <metric>massratio</metric>
    <metric>meandist</metric>
    <metric>meandistclosestneighbor</metric>
    <metric>meandistorigin</metric>
    <metric>meanxloc</metric>
    <metric>meanyloc</metric>
    <metric>nspv</metric>
    <metric>nspvr</metric>
    <metric>number</metric>
    <metric>oobcount</metric>
    <metric>peakdist</metric>
    <metric>radicalization</metric>
    <metric>repulsive</metric>
    <metric>rseedused</metric>
    <metric>stopreason</metric>
    <metric>tglobal</metric>
    <metric>timeend</metric>
    <metric>timestart</metric>
    <metric>tmat11</metric>
    <metric>tmat12</metric>
    <metric>tmat21</metric>
    <metric>tmat22</metric>
    <metric>tribematrix</metric>
    <metric>tribenum</metric>
    <metric>totmass</metric>
    <metric>valchg</metric>
    <metric>valchgmax</metric>
    <metric>valchgmin</metric>
    <metric>xMean</metric>
    <metric>yMean</metric>
    <enumeratedValueSet variable="active-mass-distribution">
      <value value="&quot;Exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomseed">
      <value value="-98765432"/>
      <value value="23987654"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tmat11">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tmat12">
      <value value="0.7"/>
      <value value="1.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tmat21">
      <value value="0.7"/>
      <value value="1.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tmat22">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="xMean">
      <value value="0"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yMean">
      <value value="0"/>
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="expa16" repetitions="1" runMetricsEveryStep="false">
    <preExperiment>clear-all</preExperiment>
    <setup>let fn (word "data/" behaviorspace-experiment-name "/views" )
let fp (word "data/" behaviorspace-experiment-name "/plots" )
let rv (shell:exec "mkdir" "-p" fn fp)
set tribes false
setup
let fni (word "data/" behaviorspace-experiment-name "/views/viewinit" behaviorspace-run-number ".png")
export-view fni</setup>
    <go>go</go>
    <postRun>let fn (word "data/" behaviorspace-experiment-name "/views/view" behaviorspace-run-number ".png")
let fp (word "data/" behaviorspace-experiment-name "/plots/plot" behaviorspace-run-number ".csv")
let wd (word "data/" behaviorspace-experiment-name "/plots/world" behaviorspace-run-number ".csv")
export-view fn
export-world wd</postRun>
    <timeLimit steps="10000"/>
    <metric>attractive</metric>
    <metric>agentcount</metric>
    <metric>ctrmassv</metric>
    <metric>ctrmassvr</metric>
    <metric>dcnsd</metric>
    <metric>elapsed-time</metric>
    <metric>floc</metric>
    <metric>groupcount</metric>
    <metric>grppol</metric>
    <metric>grppolm</metric>
    <metric>grprad</metric>
    <metric>grpradm</metric>
    <metric>injectionMaxMass</metric>
    <metric>injMassx</metric>
    <metric>injMassy</metric>
    <metric>iterations</metric>
    <metric>masserror</metric>
    <metric>massratio</metric>
    <metric>meandist</metric>
    <metric>meandistclosestneighbor</metric>
    <metric>meandistorigin</metric>
    <metric>meanxloc</metric>
    <metric>meanyloc</metric>
    <metric>nspv</metric>
    <metric>nspvr</metric>
    <metric>number</metric>
    <metric>oobcount</metric>
    <metric>peakdist</metric>
    <metric>radicalization</metric>
    <metric>repulsive</metric>
    <metric>rseedused</metric>
    <metric>stopreason</metric>
    <metric>tglobal</metric>
    <metric>timeend</metric>
    <metric>timestart</metric>
    <metric>tmat11</metric>
    <metric>tmat12</metric>
    <metric>tmat21</metric>
    <metric>tmat22</metric>
    <metric>tribematrix</metric>
    <metric>tribenum</metric>
    <metric>totmass</metric>
    <metric>valchg</metric>
    <metric>valchgmax</metric>
    <metric>valchgmin</metric>
    <metric>xMean</metric>
    <metric>yMean</metric>
    <steppedValueSet variable="randomseed" first="-100000" step="10000" last="100000"/>
    <enumeratedValueSet variable="InjectionMaxMass">
      <value value="60"/>
      <value value="6000"/>
      <value value="9000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="xMean">
      <value value="0"/>
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="xStd">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yMean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yStd">
      <value value="15"/>
    </enumeratedValueSet>
    <subExperiment>
      <enumeratedValueSet variable="injMassx">
        <value value="0"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="injMassy">
        <value value="-20"/>
        <value value="-10"/>
        <value value="0"/>
        <value value="10"/>
        <value value="20"/>
      </enumeratedValueSet>
    </subExperiment>
    <subExperiment>
      <enumeratedValueSet variable="injMassx">
        <value value="10"/>
        <value value="20"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="injMassy">
        <value value="0"/>
      </enumeratedValueSet>
    </subExperiment>
  </experiment>
  <experiment name="expa17" repetitions="1" runMetricsEveryStep="false">
    <preExperiment>clear-all</preExperiment>
    <setup>let fn (word "data/" behaviorspace-experiment-name "/views" )
let fp (word "data/" behaviorspace-experiment-name "/plots" )
let rv (shell:exec "mkdir" "-p" fn fp)
set tribes false
setup
let fni (word "data/" behaviorspace-experiment-name "/views/viewinit" behaviorspace-run-number ".png")
export-view fni</setup>
    <go>go</go>
    <postRun>let fn (word "data/" behaviorspace-experiment-name "/views/view" behaviorspace-run-number ".png")
let fp (word "data/" behaviorspace-experiment-name "/plots/plot" behaviorspace-run-number ".csv")
let wd (word "data/" behaviorspace-experiment-name "/plots/world" behaviorspace-run-number ".csv")
export-view fn
export-world wd</postRun>
    <timeLimit steps="10000"/>
    <metric>attractive</metric>
    <metric>agentcount</metric>
    <metric>ctrmassv</metric>
    <metric>ctrmassvr</metric>
    <metric>dcnsd</metric>
    <metric>elapsed-time</metric>
    <metric>floc</metric>
    <metric>groupcount</metric>
    <metric>grppol</metric>
    <metric>grppolm</metric>
    <metric>grprad</metric>
    <metric>grpradm</metric>
    <metric>injectionMaxMass</metric>
    <metric>injMassx</metric>
    <metric>injMassy</metric>
    <metric>iterations</metric>
    <metric>masserror</metric>
    <metric>massratio</metric>
    <metric>meandist</metric>
    <metric>meandistclosestneighbor</metric>
    <metric>meandistorigin</metric>
    <metric>meanxloc</metric>
    <metric>meanyloc</metric>
    <metric>nspv</metric>
    <metric>nspvr</metric>
    <metric>number</metric>
    <metric>oobcount</metric>
    <metric>peakdist</metric>
    <metric>radicalization</metric>
    <metric>repulsive</metric>
    <metric>rseedused</metric>
    <metric>stopreason</metric>
    <metric>tglobal</metric>
    <metric>timeend</metric>
    <metric>timestart</metric>
    <metric>tmat11</metric>
    <metric>tmat12</metric>
    <metric>tmat21</metric>
    <metric>tmat22</metric>
    <metric>tribematrix</metric>
    <metric>tribenum</metric>
    <metric>totmass</metric>
    <metric>valchg</metric>
    <metric>valchgmax</metric>
    <metric>valchgmin</metric>
    <metric>xMean</metric>
    <metric>yMean</metric>
    <steppedValueSet variable="randomseed" first="-100000" step="10000" last="100000"/>
    <enumeratedValueSet variable="InjectionMaxMass">
      <value value="60"/>
      <value value="6000"/>
      <value value="12000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="xMean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="xStd">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yMean">
      <value value="0"/>
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yStd">
      <value value="15"/>
    </enumeratedValueSet>
    <subExperiment>
      <enumeratedValueSet variable="injMassx">
        <value value="0"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="injMassy">
        <value value="-20"/>
        <value value="-10"/>
        <value value="1"/>
        <value value="10"/>
        <value value="20"/>
      </enumeratedValueSet>
    </subExperiment>
    <subExperiment>
      <enumeratedValueSet variable="injMassx">
        <value value="10"/>
        <value value="20"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="injMassy">
        <value value="0"/>
      </enumeratedValueSet>
    </subExperiment>
  </experiment>
</experiments>
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
0
@#$#@#$#@
