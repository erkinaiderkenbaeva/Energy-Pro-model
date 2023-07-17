extensions [																					
  gis																							                                                                                              ; GIS (importing map Amsterdam) , table (being able to create dictionaries of multiple variables)
  csv																							                                                                                              ; .csv (prebuild datasets from citizens Amsterdam)
  table																								  											                                                                  ; importing the table extension to ensure table output instead of lists (for exporting purposes)
  palette																						                                                                                            ; importing the pallette extension to include more detailed colors within the district
]
																																				
globals [																						
  start-time																					                                                                                          ; used for displaying the start time (e.g., 2021)
  current-time                                                                                                                                  ; used for displaying the start time + # of ticks
  end-time                                                                                                                                      ; used for adding a stop functionality in the model
																																				
                                                                                                                                                ; extern dataset (GIS / excel)
  buildings-dataset                                                                                                                             ; contains .shp files of the buildings in Amsterdam
  neighborhoods-dataset                                                                                                                         ; contains .shp files of the neighborhoods (wijknamen) of Amsterdam
  districts-dataset                                                                                                                             ; contains .shp files of the districts of Amsterdam
  display-district-labels1                                                                                                                      ; used for displaying the district borders in the interface. The addition of 1 at the end is used for storing the previous district label choice of the user
  display-district-labels                                                                                                                       ; used for displaying the district borders in the interface
  display-buildings-labels                                                                                                                      ; used for displaying the buildings (in red) in a particular district																																				
                                                                                                                                                ; store memory of button interface
  dis-neig                                                                                                                                      ; used for displaying the neighborhoods borders (in purple) of a particular district
  																																				
                                                                                                                                                ; current random-seed
  r-seed                                                                                                                                        ; random-seed to control stochasticity
																																				
                                                                                                                                                ; properties all households
  emissions                                                                                                                                     ; emissions of all households
  emissions-saved                                                                                                                               ; emissions saved by all households from adopting a technology. Used for storing the emissions saved compared to the previous tick of a household
  overview-investments                                                                                                                          ; overview of all the investments using a list format of 1 - 5 (e.g., 2: adopting double glazing)
  electricity-price                                                                                                                             ; global electricity price for the city of Amsterdam
  gas-price                                                                                                                                     ; global gas price for the city of Amsterdam
  uncertainty-energy-prices                                                                                                                     ; uncertainty effect that creates fluctuations in the global energy price of gas and electricity
  uncertainty-tol                                                                                                                               ; tolerance of households that affects their decision-making to adopt or not
  patches-wk-codes                                                                                                                              ; assign wijk codes to patches per district. Needed to exclude not used patches in the interface
																																				
                                                                                                                                                ; households per district
  cent-house                                                                                                                                    ; households in Centrum
  west-house                                                                                                                                    ; households in West
  wester-house                                                                                                                                  ; households in Westerpoort
  noord-house                                                                                                                                   ; households in Noord
  nwest-house                                                                                                                                   ; households in Nieuw-West
  zuid-house                                                                                                                                    ; households in Zuid
  zo-house                                                                                                                                      ; households in Zuid-Oost
  oost-house                                                                                                                                    ; households in Oost
																																				
                                                                                                                                                ; households not being able to perform an action are expected to do nothing
  idle                                                                                                                                          ; # of households not choosing and / or not being able to choose a decision
																																				
                                                                                                                                                ; overview investments
  overview-all-investment                                                                                                                       ; overview investments that updates every tick. Each investments per household is put in a long list
  overview-investments-owner                                                                                                                    ; overview investments from the households that are owners only
  overview-investments-tenant                                                                                                                   ; overview investments from the households that are tenants only
  strategies-owners                                                                                                                             ; strategies chosen from the owners only (cumulative)
  strategies-owners-tick                                                                                                                        ; overview strategies from the owners only per tick
  strategies-owners-old                                                                                                                         ; overview strategies from the owners only from the previous tick
  strategies-tenants                                                                                                                            ; overview strategies from the tenants only
  strategies-tenants-tick                                                                                                                       ; overview strategies from the tenants only per tick
  strategies-tenants-old                                                                                                                        ; overview strategies from the tenants only from the previous tick
  age-categories-global                                                                                                                         ; age categories from the tenants (starting from 17 till 100 with increments of 7 years)
  repeat-#                                                                                                                                      ; number of repeat strategies chosen (all households)
  imitate-#                                                                                                                                     ; number of imitate strategies chosen (all households)
  optimise-#                                                                                                                                    ; number of optimise strategies chosen (all households)
  inquire-#                                                                                                                                     ; number of inquire strategies chosen (all households)
																																				
  apartment-non-pv-list                                                                                                                         ; households that live in an apartment that haven't adopted solar panels
]
																																				
breed [ households household ]                                                                                                                  ; agent-breed that depicts households
breed [ measures measure ]                                                                                                                      ; agent-breed that depicts the 4 measurements in the top of the interface
breed [ district-labels district-label ]                                                                                                        ; agent-breed that depicts the district-labels in the interface
breed [ legends legend]                                                                                                                         ; agent-breed that depicts the legend in the interface
																																				
households-own [
  ownership                                                                                                                                     ; tenant or owner
  landlord                                                                                                                                      ; is the household a landlord?
  income                                                                                                                                        ; income category
  income-value                                                                                                                                  ; income actual value
  age                                                                                                                                           ; age actual value
  age-cat                                                                                                                                       ; age category
  education                                                                                                                                     ; education level
  wijk                                                                                                                                          ; wijk name
  d-type                                                                                                                                        ; type of house
  construction-year                                                                                                                             ; construction year of the household / building
  elect-consumption                                                                                                                             ; kWh / y
  gas-consumption                                                                                                                               ; M^3 / y
  elect-prod                                                                                                                                    ; kWh / y
  gas-prod                                                                                                                                      ; M^3 / y
  inv-list                                                                                                                                      ; overview investments
  score-inv                                                                                                                                     ; score investment performance
  h-composition                                                                                                                                 ; household composition (single, partner, family)
  contact-neighbors                                                                                                                             ; number inidicating if they interact with their neighbor often
  emissions-home                                                                                                                                ; emissions emitted from the building and the behavior of the household
  wealth                                                                                                                                        ; index value wealth resident (0 - 10)
  unc-tolerance                                                                                                                                 ; index value wealth resident (0 - 10)
  consumat-uncertainty                                                                                                                          ; Consumat uncertainty level of the household
  LNS                                                                                                                                           ; level of need satisfaction
  LNS-min                                                                                                                                       ; min level of need satisfaction
  sim-neigh                                                                                                                                     ; neighbors with same wijk, income and age category
  chosen-interact                                                                                                                               ; neighbor chosen for interaction based on sim-neighbor
  impact-interaction                                                                                                                            ; impact interaction of the household with the chosen neighbour
  BC                                                                                                                                            ; behavioral control formula
  mental-map                                                                                                                                    ; memory of agents
  adopted-previous?                                                                                                                             ; adopted previous tick?
  skip?                                                                                                                                         ; does the household skip the next tick?
  skip-old                                                                                                                                      ; skip value from a previous tick
  strategies-chosen                                                                                                                             ; strategy chosen for the current tick
  value-agent                                                                                                                                   ; additional belief of agents (prior knowledge or influences) --> randomly assigned (0 - 1)
  want-to-move                                                                                                                                  ; wants to move affects the uncertainty of a household to adopt a new tehnology
  cohesion                                                                                                                                      ; cohesion value that determines the likelihood of a household to interact with their neighbours
  const-year-cat                                                                                                                                ; construction year of a building (categorical)
  LNS-s                                                                                                                                         ; level of need satisfaction (social element)
  effect-investment                                                                                                                             ; year when the effect of the adopted investments are taken place
  same-building                                                                                                                                 ; determines which households are present in the same building
  building-nmr                                                                                                                                  ; unique identifier of each building
]
																																				
measures-own [
  m-type                                                                                                                                        ; type of measure (PV, heatpump, major insulation, double glazing)
  lifetime                                                                                                                                      ; lifetime of a measure (in years)
  depreciation                                                                                                                                  ; changes of an investment price per year
  energy-production                                                                                                                             ; kWh / y
  energy-prod-buildings                                                                                                                         ; kWh / y for buildings (multiple households)
  elect-for-heat                                                                                                                                ; kWh / y
  tech-price                                                                                                                                    ; wealth index value
  price-change                                                                                                                                  ; wealth index value
  tech-emissions                                                                                                                                ; index value emissions lifecycle
]
																																				
patches-own [
  wk_code                                                                                                                                       ; wijk ID
  wk_naam                                                                                                                                       ; wijk naam
  const_year                                                                                                                                    ; construction year of a building
  lst-neigh                                                                                                                                     ; list of agents on a patch
  mean-lst                                                                                                                                      ; mean of investments of households
  adopt-per-patch                                                                                                                               ; # of adoptions of a new technology of residents on a patch
]
																																				
;################################################################################################################################################
;########################################################### SETUP PROCEDURE  ###################################################################
;################################################################################################################################################
																																				
to setup
  clear-all-plots                                                                                                                               ; reset the plots in the interface
  default-kWh/M3                                                                                                                                ; assigns the default parameter values in the interface
  setup-measures                                                                                                                                ; creates the measure logos
  if new-random-seed? [set r-seed new-seed random-seed r-seed]                                                                                  ; assigns randomness to control stochasticity in the model
  setup-legend                                                                                                                                  ; creates the legend in the interface
  create-population_test                                                                                                                        ; creates the population
  reset-ticks                                                                                                                                   ; reset ticks
end
																																				
to setup-sim-neigh                                                                                                                              ; choose a similar neighbour of a resident within his vicinity in-radius of 2 patches (based on income, age, wijk)
  ask households[                                                                                                                               ; run it only once
    set sim-neigh [who] of households-here with [
      wijk = [wijk] of myself and
      income = [income] of myself and
      age-cat = [age-cat] of myself and
      who != [who] of myself]
  ]

  if collective-dm? [                                                                                                                           ; choose a head tenant, assign a building number and assign residents to all via 'same-building' procedure
    ifelse owners-only? and owners-building-only? [                                                                                             ; assign apartment based on wijk, construction year and d-type = "apartment"
      let x turtle-set households with [d-type = "apartment" and building-nmr = 0 and ownership = "owner"]                                      ; restrict building size based on input interface
      ask x [                                                                                                                                   ; if any resident in building has adopted PV, set PV to all residents in building
        let z random 10000000                                                                                                                   ; last section puts the head tenant in an overview list that counts the buildings
        let y turtle-set households-here with [                                                                                                 ;
          wijk = [wijk] of myself and                                                                                                           ;
          construction-year = [construction-year] of myself and                                                                                 ;
          d-type = "apartment" and                                                                                                              ;
          ownership = "owner"                                                                                                                   ;
        ]
        ask y [set same-building y set building-nmr z                                                                                           ;
          if count same-building < min-building-size [ set same-building 0 set building-nmr 0]]                                                 ;
																																				
        set same-building y                                                                                                                     ;
        set building-nmr z                                                                                                                      ;
        ifelse count same-building < min-building-size                                                                                          ;
        [ set same-building 0 set building-nmr 0]                                                                                               ;
        [
          let prod-pv-buildings reduce sentence [energy-prod-buildings] of measures with [m-type = "PV"]                                        ;
          if any? same-building with [member? 4 inv-list] or member? 4 inv-list [                                                               ;
            ask same-building [ set inv-list replace-item 3 inv-list 4 set elect-prod prod-pv-buildings]                                        ;
            set inv-list replace-item 3 inv-list 4                                                                                              ;																																	
          ]
        ]
      ]
																																				
      set apartment-non-pv-list [ ]                                                                                                             ;
      let q1 turtle-set x with [not member? 4 inv-list]                                                                                         ;
      let q remove-duplicates [building-nmr] of households with [building-nmr != 0 ]                                                            ;
      foreach q [ i -> let q2 one-of q1 with [building-nmr = i]  set apartment-non-pv-list lput q2 apartment-non-pv-list]                       ;
      let q3 remove-duplicates apartment-non-pv-list                                                                                            ;
      set apartment-non-pv-list turtle-set q3                                                                                                   ;
    ]
    [
      let x turtle-set households with [d-type = "apartment" and building-nmr = 0]                                                              ;
      ask x [                                                                                                                                   ;
        let z random 10000000                                                                                                                   ;
        let y turtle-set households-here with [                                                                                                 ;
          wijk = [wijk] of myself and                                                                                                           ;
          construction-year = [construction-year] of myself and                                                                                 ;
          d-type = "apartment"                                                                                                                  ;
																																				
        ]
        ask y [set same-building y set building-nmr z                                                                                           ;
          if count same-building < min-building-size [ set same-building 0 set building-nmr 0]]                                                 ;
																																				
        set same-building y                                                                                                                     ;
        set building-nmr z                                                                                                                      ;
        ifelse count same-building < min-building-size                                                                                          ;
        [ set same-building 0 set building-nmr 0]                                                                                               ;
        [
          let prod-pv-buildings reduce sentence [energy-prod-buildings] of measures with [m-type = "PV"]                                        ;
          if any? same-building with [member? 4 inv-list] or member? 4 inv-list [                                                               ;
            ask same-building [ set inv-list replace-item 3 inv-list 4 set elect-prod prod-pv-buildings]                                        ;
            set inv-list replace-item 3 inv-list 4                                                                                              ;
																																				
          ]
        ]
      ]
      set apartment-non-pv-list [ ]                                                                                                             ;
      let q1 turtle-set x with [not member? 4 inv-list]                                                                                         ;
      let q remove-duplicates [building-nmr] of households with [building-nmr != 0 ]                                                            ;
      foreach q [ i -> let q2 one-of q1 with [building-nmr = i]  set apartment-non-pv-list lput q2 apartment-non-pv-list]                       ;
      let q3 remove-duplicates apartment-non-pv-list                                                                                            ;
      set apartment-non-pv-list turtle-set q3                                                                                                   ;
    ]                                                                                                                                           																																			
  ]
end
																																				
to create-population_test                                                                                                                       ; creates the population from the given excel files in the directory
  set age-categories-global ["17-24" "25-34" "35-44" "45-54" "55-64" "65-74" "75-100"]                                                          ;
  set patches-wk-codes []                                                                                                                       ;
  let temp-wk-code sort-by > remove-duplicates [wk_code] of patches                                                                             ;
  foreach temp-wk-code [ i -> set patches-wk-codes lput patch-set patches with [wk_code = i] patches-wk-codes]                                  ;
  let households-list1 bf csv:from-file dataset-residents                                                                                       ;

  foreach households-list1 [ row ->                                                                                                             ;
    create-households 1 [                                                                                                                       ;
      set shape "circle"                                                                                                                        ;
      set size 0.5                                                                                                                              ;
      set heading 1                                                                                                                             ;
      set color white                                                                                                                           ;
      ;set district item 0 row                                                                                                                  ;
      set wijk item 1 row                                                                                                                       ;
      set ownership item 4 row                                                                                                                  ;
      set landlord item 5 row                                                                                                                   ;
      set age-cat item 6 row                                                                                                                    ;
      set age item 7 row                                                                                                                        ;
      set education item 8 row                                                                                                                  ;
      set h-composition item 10 row                                                                                                             ;
      set contact-neighbors item 11 row                                                                                                         ;
      set wealth item 13 row                                                                                                                    ;
      set d-type item 16 row                                                                                                                    ;
      set elect-consumption item 18 row                                                                                                         ;
      set gas-consumption item 19 row                                                                                                           ;
      set want-to-move item 20 row                                                                                                              ;
      set inv-list read-from-string item 21 row                                                                                                 ;
      set strategies-chosen [0 0 0 0]                                                                                                           ; repeat - imitate - optimise - inquire
																																				
      set LNS 0                                                                                                                                 ;
      set LNS-min item 22 row                                                                                                                   ;
      set cohesion item 23 row                                                                                                                  ;
      set income item 26 row                                                                                                                    ;
      set income-value item 12 row                                                                                                              ;
      set construction-year item 15 row                                                                                                         ;
      set const-year-cat item 27 row                                                                                                            ;
      set BC 0                                                                                                                                  ;
      set adopted-previous? false                                                                                                               ;
      set skip? false                                                                                                                           ;
      set skip-old 0                                                                                                                            ;
      set value-agent one-of [0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9]                                                                            ;
      set mental-map 0.1 + value-agent                                                                                                          ;
																																				
      let position-patch-set position wijk temp-wk-code                                                                                         ;
      let temp-patch-set item position-patch-set patches-wk-codes                                                                               ;
      ifelse is-patch? one-of temp-patch-set with [wk_code = [wijk] of myself and                                                               ;
      const_year = [construction-year] of myself] = true                                                                                        ;
      [move-to one-of temp-patch-set with [ wk_code = [wijk] of myself and const_year = [construction-year] of myself] ]                        ;
      [move-to one-of temp-patch-set with [ wk_code = [wijk] of myself] ]                                                                       ;
																																				
      evaluate-needs                                                                                                                            ;
      if LNS-adj? [                                                                                                                             ;
        set LNS-min precision random-normal LNS-slider 0.1 2                                                                                    ;
        if LNS-min > 1 [ set LNS-min 1 ]                                                                                                        ;
        if LNS-min < 0 [ set LNS-min 0 ]                                                                                                        ;
      ]
    ]
  ]
																																				
  setup-sim-neigh                                                                                                                               ; creates the similar neighbour attribute of each household
  investments-overview-hh                                                                                                                       ; creates the initial overview of the investments of households at tick 0
  production                                                                                                                                    ; existing production level based on investments of households (solar / heat pumps)
  update-score-investments                                                                                                                      ; updates the scores of the investments per household; how well are the households performing compared to the others?
  update-emissions                                                                                                                              ; updates the emission levels per household
  set emissions sum [emissions-home] of households                                                                                              ; updates the global emission level
  set repeat-# 0                                                                                                                                ;
  set imitate-# 0                                                                                                                               ;
  set optimise-# 0                                                                                                                              ;
  set inquire-# 0                                                                                                                               ;
end                                                                                                                                             ;
																																				
to setup-measures                                                                                                                               ; creates the measure legend (top right)
  if count measures < 1 [                                                                                                                       ;
  let data-file "2-EEM_RE/measures_updated.csv"                                                                                                 ;
  let measures-list bf csv:from-file data-file                                                                                                  ; importing the attributes from the excel sheet (for visual purposes)
																																				
  foreach measures-list [ row ->                                                                                                                ; assigning the values to measures
    create-measures 1 [                                                                                                                         ;
      set size 5                                                                                                                                ;
      set m-type item 0 row                                                                                                                     ;
      set energy-production item 1 row                                                                                                          ;
      set elect-for-heat item 2 row                                                                                                             ;
      set tech-price item 3 row                                                                                                                 ;
      set price-change item 4 row                                                                                                               ;
      set tech-emissions item 5 row                                                                                                             ;
      set lifetime item 6 row                                                                                                                   ;
      set shape item 7 row                                                                                                                      ;
      set xcor item 8 row                                                                                                                       ;
      set ycor item 9 row                                                                                                                       ;
      set color item 10 row                                                                                                                     ;
      set label item 11 row                                                                                                                     ;
      set energy-prod-buildings item 12 row                                                                                                     ;
    ]
  ]
  ]
end
																																				
to setup-legend                                                                                                                                 ; creates the legend within the interface (bottom left)
  if count legends < 1 [                                                                                                                        ;
    create-legends 1 [                                                                                                                          ;
      set size 1.1                                                                                                                              ;
      set color white                                                                                                                           ;
      set xcor 14                                                                                                                               ;
      set ycor 4                                                                                                                                ;
      set shape "circle"                                                                                                                        ;
      set label "Household   "                                                                                                                  ;
    ]
																																				
    create-legends 1 [                                                                                                                          ;
      set size 1.5                                                                                                                              ;
      set color white                                                                                                                           ;
      set xcor 14                                                                                                                               ;
      set ycor 8                                                                                                                                ;
      set shape "line"                                                                                                                          ;
      set label "Border        "                                                                                                                ;
      set heading 90                                                                                                                            ;
      set color violet + 1                                                                                                                      ;
    ]
																																				
    create-legends 1 [                                                                                                                          ;
      set size 1.5                                                                                                                              ;
      set color red                                                                                                                             ;
      set xcor 14                                                                                                                               ;
      set ycor 12                                                                                                                               ;
      set shape "flag"                                                                                                                          ;
      set heading 90                                                                                                                            ;
      set hidden? true  ]                                                                                                                       ;
																																				
    create-legends 1 [                                                                                                                          ;
      set size 1.1                                                                                                                              ;
      set color black                                                                                                                           ;
      set xcor 10                                                                                                                               ;
      set ycor 12                                                                                                                               ;
      set shape "circle"                                                                                                                        ;
      ;set label "Legend  "                                                                                                                     ;
    ]
  ]
end
																																				
;################################################################################################################################################
;########################################################### GO PROCEDURE  ######################################################################
;################################################################################################################################################
																																				
to go                                                                                                                                           ;
  if ticks != maximum-years [                                                                                                                   ; stops the model from running when the maximum number of years have been reached
    update-start                                                                                                                                ; update start values before the population starts the decision-making process
    ask households with [not skip?] [                                                                                                           ; excludes households that have determined to skip the current tick based on their level of need satisfaction (see report)
      interact                                                                                                                                  ; interaction phase among households
      evaluate-needs                                                                                                                            ; evaluate the needs per household
      check-behavioral-control                                                                                                                  ; updates values of the BC per household
      evaluate-satisfaction                                                                                                                     ; evaluates the satisfaction of the household which determines the decision to skip or participate the following tick in the decision-making process
    ]
    collective-decision-making                                                                                                                  ; collective decision making process which determines the choice to invest or not
    update-end                                                                                                                                  ; updates exit values
  tick                                                                                                                                          ; proceeds to the next tick
  ]
end

;################################################################################################################################################
;########################################################### MAIN PROCEDURES  ###################################################################
;################################################################################################################################################
																																				
to interact                                                                                                                                     ; interaction process between neighbours
  ifelse sim-neigh = []                                                                                                                         ; set sim-neighbor based on: location-wijk, age, income;
  [ ]                                                                                                                                           ; interact with this (one from the list) neighbor
  [                                                                                                                                             ; set impact of interaction = sim-neighbor * contact-with-neighbors
    if contact-neighbors >= 0.5 [                                                                                                               ;
      let x one-of sim-neigh                                                                                                                    ;
      set chosen-interact turtle x                                                                                                              ;
      let y [contact-neighbors] of chosen-interact                                                                                              ;
      set impact-interaction y * contact-neighbors                                                                                              ;
    ]
  ]                                                                                                                                             																																				
end
																																				
to evaluate-needs                                                                                                                               ; evalue needs procedure which assigns the LNS value of the household (level of need satisfaction)
  ifelse wealth < 0                                                                                                                             ; inclusion of LNS-s and LNS-e based on formula
  [set LNS 0]                                                                                                                                   ; it determines first if there is a chosen interact (e.g. actual interaction person)
  [                                                                                                                                             ; if there is, check if any investments the chosen to interact with and myself have made
    ifelse ticks != 2 [                                                                                                                         ; if there is, set LNS-s to cohesion * contact-neighbors * 1
      ifelse chosen-interact != 0 [                                                                                                             ; if there isn't, change 1 to 0.5
        let yearly-energy-cost (electricity-price * (elect-consumption - elect-prod)) +                                                         ; last section determines the income level and assigns a factor to it based on LNS formula
        (gas-price * (gas-consumption - gas-prod))                                                                                              ;
        let lns-e income-value - yearly-energy-cost                                                                                             ;
																																				
        let comparison-inv-list length filter [i -> i = true] (map = inv-list [inv-list] of chosen-interact)                                    ;
																																				
        ifelse chosen-interact != [] and comparison-inv-list >= 1                                                                               ;
        [ set LNS-s cohesion * 1]                                                                                                               ;
        [ set LNS-s cohesion * 0.5]                                                                                                             ;
																																																																				
        ifelse lns-e > 59500                                                                                                                    ;
        [set LNS (1 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                                    ;
        [ifelse lns-e >= 42600                                                                                                                  ;
          [set LNS (0.75 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                               ;
          [ifelse lns-e >= 30200                                                                                                                ;
            [set LNS (0.5 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                              ;
            [ifelse lns-e >= 21000                                                                                                              ;
              [set LNS (0.25 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                           ;
              [set LNS 0]                                                                                                                       ;
            ]
          ]
        ]
      ]
      [
        let yearly-energy-cost (electricity-price * (elect-consumption - elect-prod)) + (gas-price * (gas-consumption - gas-prod))              ;
        let left-amount income-value - yearly-energy-cost                                                                                       ;
        set LNS-s cohesion * 0.5                                                                                                                ;
																																																																								
        ifelse left-amount > 59500                                                                                                              ;
        [set LNS (1 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                                    ;
        [ifelse left-amount >= 42600                                                                                                            ;
          [set LNS (0.75 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                               ;
          [ifelse left-amount >= 30200                                                                                                          ;
            [set LNS (0.5 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                              ;
            [ifelse left-amount >= 21000                                                                                                        ;
              [set LNS (0.25 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                           ;
              [set LNS 0]                                                                                                                       ;
            ]
          ]
        ]
      ]
    ]
    [ evaluate-needs-price-cap ]                                                                                                                ;
  ]
end
																																				
																																				
																																				
to check-behavioral-control                                                                                                                     ; set BC based on most expensive investment not yet adopted
  let x one-of [2 3 4 5]                                                                                                                        ;
																																				
  ifelse x = 3 and not member? 3 inv-list                                                                                                       ;
  [set BC (wealth) - reduce sentence [tech-price] of measures with [m-type = "Insulation: walls, floor, roof"] ]                                ;
  ; we choose the most expensive insulation method for our BC1 --> if they can afford this one, then most likely the other ones to              ;
  [ifelse x = 5 and not member? 5 inv-list                                                                                                      ;
    [set BC (wealth) - reduce sentence [tech-price] of measures with [m-type = "heatpump"]]                                                     ;
    [ifelse x = 4 and not member? 4 inv-list                                                                                                    ;
      [set BC (wealth) - reduce sentence [tech-price] of measures with [m-type = "PV"]]                                                         ;
      [ifelse x = 2 and not member? 2 inv-list                                                                                                  ;
        [set BC (wealth) - reduce sentence [tech-price] of measures with [m-type = "Double glazing"]]                                           ;
          [ set BC wealth ]                                                                                                                     ;
        ]
    ]
  ]
  ifelse BC <= 0 [choose-decision-mode] [update-memory]
end
																																				
to update-memory                                                                                                                                ; updates the memory of the neighbours based on their past interactions and decisions
  ; if I adopted previous tick and or one of my chosen interact has adopted, update mental map                                                  ;
  ifelse chosen-interact != 0 [                                                                                                                 ;
    let x [adopted-previous?] of chosen-interact                                                                                                ;
    ifelse adopted-previous? or x = true and impact-interaction >= 0.5 [                                                                        ;
      remember-new-info                                                                                                                         ;
      set mental-map mental-map + 0.2                                                                                                           ;
      choose-decision-mode                                                                                                                      ;
    ]
    [
      choose-decision-mode]                                                                                                                     ;
  ]
  [                                                                                                                                             ;
    ifelse adopted-previous? ; if mental-map >= 1 --> reset to 0.1
    [
      remember-new-info                                                                                                                         ;
      set mental-map mental-map + 0.2                                                                                                           ;
      choose-decision-mode ]                                                                                                                    ;
    [
      choose-decision-mode                                                                                                                      ;
    ]
  ]
end
																																				
to remember-new-info                                                                                                                            ; updates the memory of the neighbours with their initial belief
  set mental-map 0.1 + value-agent                                                                                                              ;
end                                                                                                                                             ;
																																				
to choose-decision-mode                                                                                                                         ; determines which one of the four decision the household will make
  let energy-poor-c (electricity-price * (elect-consumption - elect-prod)) + (gas-price * (gas-consumption - gas-prod))                         ; inclusion of energy poor (more than 10% of income to energy) and consumat uncertainty based on uncertainty energy prices
  let energy-poor-? energy-poor-c / income-value                                                                                                ;
  set consumat-uncertainty precision random-normal want-to-move unc-stand.dev-consumat 2 + uncertainty-energy-prices                            ;
  if consumat-uncertainty > 1 [set consumat-uncertainty 1]                                                                                      ;
  if consumat-uncertainty < 0 [set consumat-uncertainty 0]                                                                                      ;
  set uncertainty-tol precision random-normal 0.5 0.1 2                                                                                         ;
																																				
  ifelse owners-only?                                                                                                                           ;
  [    if  BC <= 0 or LNS > LNS-min                                                                                                             ;
    [
      if-else consumat-uncertainty <= uncertainty-tol [ repeat-mode ] [ imitate-mode ]                                                          ;
    ]
																																				
    if BC > 0 and LNS < LNS-min and ownership = "owner" and energy-poor-? < 0.1                                                                 ;
    [
      if-else consumat-uncertainty <= uncertainty-tol [ optimize-mode ] [ inquire-mode ]                                                        ;
    ]                                                                                                                                           ;
    if BC < 0 and LNS <= LNS-min [set idle idle + 1 evaluate-satisfaction]                                                                      ;
    if BC > 0 and LNS >= LNS-min [set idle idle + 1 evaluate-satisfaction]                                                                      ;
  ]
																																				
  [
    if  BC <= 0 or LNS > LNS-min                                                                                                                ;
    [
      if-else consumat-uncertainty <= uncertainty-tol [ repeat-mode ] [ imitate-mode ]                                                          ;
    ]
																																				
    if BC > 0 and LNS < LNS-min and energy-poor-? < 0.1                                                                                         ;
    [
      if-else consumat-uncertainty <= uncertainty-tol [ optimize-mode ] [ inquire-mode ]                                                        ;
    ]
																																				
    if BC < 0 and LNS < LNS-min [set idle idle + 1]                                                                                             ;
    if BC > 0 and LNS > LNS-min [set idle idle + 1]                                                                                             ;
  ]
end
																																				
to collective-decision-making                                                                                                                   ; start of the collective decision-making process between neighbours in apartment buildings
  if collective-dm? and owners-only? [                                                                                                          ; at least 3 owners in each building
    ask apartment-non-pv-list with [ownership = "owner"][                                                                                       ; if collective decision-making is present, ask building that haven't adopted PV yet
      if count same-building with [ownership = "owner"] > 2 [                                                                                   ; to create a random list of length # of residents buildings
        let x count same-building with [ownership = "owner"]                                                                                    ; each resident votes randomly 0 or 1
        let y n-values x [i -> random 2]                                                                                                        ; if 75% voted 1, adopt PV
        let z length filter [ i -> i = 1 ] y                                                                                                    ;
        if (z / x)> 0.75 [                                                                                                                      ;
          set inv-list replace-item 3 inv-list 4                                                                                                ;
          ask same-building [ set inv-list replace-item 3 inv-list 4]                                                                           ;
          set apartment-non-pv-list other apartment-non-pv-list                                                                                 ;
        ]
      ]
    ]
  ]
																																				
   if collective-dm? and not owners-only? [                                                                                                     ;
    ask apartment-non-pv-list [                                                                                                                 ;
      let x count same-building                                                                                                                 ;
      let y n-values x [i -> random 2]                                                                                                          ;
      let z length filter [ i -> i = 1 ] y                                                                                                      ;
      if (z / x)> 0.75 [                                                                                                                        ;
        set inv-list replace-item 3 inv-list 4                                                                                                  ;
        ask same-building [ set inv-list replace-item 3 inv-list 4]                                                                             ;
        set apartment-non-pv-list other apartment-non-pv-list                                                                                   ;
      ]
    ]
  ]
end
																																				
to repeat-mode                                                                                                                                  ; repeat strategy
  set elect-consumption elect-consumption                                                                                                       ; keep the current behavior
  set gas-consumption gas-consumption                                                                                                           ;
  set adopted-previous? false                                                                                                                   ;
  let x (item 0 strategies-chosen) + 1                                                                                                          ;
  set strategies-chosen replace-item 0 strategies-chosen x                                                                                      ;
  set repeat-# repeat-# + 1                                                                                                                     ;
end                                                                                                                                             ;
																																				
to imitate-mode                                                                                                                                 ; imitate strategy
  ifelse chosen-interact != 0 [                                                                                                                 ; first determine investments of chosen interact
    let x random-float precision 1 2                                                                                                            ; two lists: one for double glazing and insulation, the other one for PV and heatpump
    if x <= impact-interaction                                                                                                                  ; based on # of investments made by chosen interact (max. of 4)
    [                                                                                                                                           ; use random-gamma distribution to change electricity and gas behavior of myself of maximum 25% (very rare cases)
      let invest-neigh filter [i -> i != 999] [inv-list] of chosen-interact                                                                     ; update investments and strategies
      let invest-neigh1 filter [i -> i != 1] invest-neigh                                                                                       ; if an agent does not have a chosen-interact, update idle + 1
      let invest-neigh-W-INS length filter [i -> i < 4] invest-neigh1                                                                           ; determines if we start behavior based on interaction
      let invest-neigh-PV-HP  length filter [i -> i > 3] invest-neigh1                                                                          ; comparing the enery savings from the measures our neighbour adopted
      if invest-neigh-W-INS > 0 or invest-neigh-PV-HP > 0 and x > 0 [                                                                           ; we take a random number between 0 and energy saving % that is also influenced by the impact of interaction
        let performance-neighbour invest-neigh-W-INS + invest-neigh-PV-HP                                                                       ;
        let k (4 / performance-neighbour) / impact-interaction                                                                                  ;
        let change-behavior precision ((random-gamma 25 k) / 100) 0                                                                             ;
        set elect-consumption precision (elect-consumption * (1 - change-behavior)) 0                                                           ;
        set gas-consumption precision (gas-consumption * (1 - change-behavior)) 0                                                               ;
      ]
    ]
    set adopted-previous? false                                                                                                                 ;
    let y (item 1 strategies-chosen) + 1                                                                                                        ;
    set strategies-chosen replace-item 1 strategies-chosen y                                                                                    ;
    set imitate-# imitate-# + 1                                                                                                                 ;
  ]
  [
    set idle idle + 1                                                                                                                           ;
  ]
end
																																				
to optimize-mode                                                                                                                                ; optimise strategy
  ifelse mental-map > 0.1 [                                                                                                                     ; mental-map > 0.1, first see if I adopted double glazing
    ifelse not member? 2 inv-list [                                                                                                             ; if not, choose randomly to invest first in PV or HP, HP or PV and ultimately in insulation
      set inv-list replace-item 1 inv-list 2                                                                                                    ; update values
      set wealth wealth - (reduce sentence [tech-price] of measures with [m-type = "Double glazing"])                                           ; effect-investments determines investments' effect taking place at tick + 1
      set effect-investment (list (ticks + 1) 2)                                                                                                ;
      set adopted-previous? true                                                                                                                ;
    ]
    [
      let x random 1                                                                                                                            ;
      ifelse x = 1 [                                                                                                                            ;
        ifelse not member? 4 inv-list and d-type = "non-apartment" and building-nmr = 0[                                                        ;
          set inv-list replace-item 3 inv-list 4                                                                                                ;
          set wealth wealth - (reduce sentence [tech-price] of measures with [m-type = "PV"])                                                   ;
          set effect-investment (list (ticks + 1) 4)                                                                                            ;
          set adopted-previous? true                                                                                                            ;
        ]
        [ifelse not member? 5 inv-list [                                                                                                        ;
          set inv-list replace-item 4 inv-list 5                                                                                                ;
          set wealth wealth - reduce sentence [tech-price] of measures with [m-type = "heatpump"]                                               ;
          set effect-investment (list (ticks + 1) 5)                                                                                            ;
          set adopted-previous? true                                                                                                            ;
          ]
          [ifelse not member? 3 inv-list [                                                                                                      ;
            set inv-list replace-item 2 inv-list 3                                                                                              ;
            set wealth wealth - (reduce sentence [tech-price] of measures with [m-type = "Insulation: walls, floor, roof"])                     ;
            set effect-investment (list (ticks + 1) 3)                                                                                          ;
            set adopted-previous? true                                                                                                          ;
            ]
            [
              set adopted-previous? false                                                                                                       ;
            ]
          ]
        ]
																																				
      ]
      [
        ifelse not member? 5 inv-list [                                                                                                         ;
          set inv-list replace-item 4 inv-list 5                                                                                                ;
          set wealth wealth - reduce sentence [tech-price] of measures with [m-type = "heatpump"]                                               ;
          set effect-investment (list (ticks + 1) 5)                                                                                            ;
          set adopted-previous? true                                                                                                            ;
        ]
        [ifelse not member? 4 inv-list and d-type = "non-apartment" and building-nmr = 0[                                                       ;
          set inv-list replace-item 3 inv-list 4                                                                                                ;
          set wealth wealth - (reduce sentence [tech-price] of measures with [m-type = "PV"])                                                   ;
          set effect-investment (list (ticks + 1) 4)                                                                                            ;
          set adopted-previous? true                                                                                                            ;
          ]
          [ifelse not member? 3 inv-list [                                                                                                      ;
            set inv-list replace-item 2 inv-list 3                                                                                              ;
            set wealth wealth - (reduce sentence [tech-price] of measures with [m-type = "Insulation: walls, floor, roof"])                     ;
            set effect-investment (list (ticks + 1) 3)                                                                                          ;
            set adopted-previous? true                                                                                                          ;
            ]
            [
              set adopted-previous? false                                                                                                       ;
            ]
          ]
        ]
      ]
    ]
    ask patch-here [set adopt-per-patch adopt-per-patch + 1]                                                                                    ;
    let y (item 2 strategies-chosen) + 1                                                                                                        ;
    set strategies-chosen replace-item 2 strategies-chosen y                                                                                    ;
    set optimise-# optimise-# + 1                                                                                                               ;
																																				
  ]
  [
    set adopted-previous? false                                                                                                                 ;
  ]
end
																																				
to inquire-mode                                                                                                                                 ; inquire strategy
  ifelse mental-map > 0.1 [                                                                                                                     ; mental-map > 0.1, first determine if any households-here did any investment

    if any? households-here with [mean inv-list < 800.2]                                                                                        ; if so, choose randomly investment based on investment of households-here
    [                                                                                                                                           ; update
      let x filter [ i -> i < 999] sort-by > reduce sentence [inv-list] of households-here                                                      ; calculation of mean list is used to determine if any investment has been made for any households for this particular patch
      let y random length x                                                                                                                     ;
																																				
      if item y x = 5 and not member? 5 inv-list[                                                                                               ;
        set inv-list replace-item 4 inv-list 5                                                                                                  ;
          set wealth wealth - reduce sentence [tech-price] of measures with [m-type = "heatpump"]                                               ;
          set effect-investment (list (ticks + 1) 5)                                                                                            ;
          set adopted-previous? true                                                                                                            ;
      ]                                                                                                                                         ;
																																				
      if item y x = 4 and not member? 4 inv-list and d-type = "non-apartment" and building-nmr = 0[                                             ;
        set inv-list replace-item 3 inv-list 4                                                                                                  ;
        set wealth wealth - (reduce sentence [tech-price] of measures with [m-type = "PV"])                                                     ;
        set effect-investment (list (ticks + 1) 4)                                                                                              ;
        set adopted-previous? true                                                                                                              ;
      ]                                                                                                                                         ;
																																				
      if item y x = 3 and not member? 3 inv-list[                                                                                               ;
        set inv-list replace-item 2 inv-list 3                                                                                                  ;
        set wealth wealth - (reduce sentence [tech-price] of measures with [m-type = "Insulation: walls, floor, roof"])                         ;
        set effect-investment (list (ticks + 1) 3)                                                                                              ;
        set adopted-previous? true                                                                                                              ;
      ]                                                                                                                                         ;
      ifelse item y x = 2 and not member? 2 inv-list[                                                                                           ;
        set inv-list replace-item 1 inv-list 2                                                                                                  ;
        set wealth wealth - (reduce sentence [tech-price] of measures with [m-type = "Double glazing"])                                         ;
        set effect-investment (list (ticks + 1) 2)                                                                                              ;
        set adopted-previous? true                                                                                                              ;
      ]                                                                                                                                         ;
      [set adopted-previous? false]                                                                                                             ;
    ]                                                                                                                                           ;
																																				
    ask patch-here [set adopt-per-patch adopt-per-patch + 1]                                                                                    ;
    let y (item 3 strategies-chosen) + 1                                                                                                        ;
    set strategies-chosen replace-item 3 strategies-chosen y                                                                                    ;
    set inquire-# inquire-# + 1                                                                                                                 ;
  ]                                                                                                                                             ;
  [                                                                                                                                             ;
    set adopted-previous? false                                                                                                                 ;
  ]                                                                                                                                             ;
end                                                                                                                                             ;
																																				
to evaluate-satisfaction                                                                                                                        ; evaluate level of satisfaction
  if-else LNS > LNS-min or mental-map < 0.5                                                                                                     ;
    [ set skip? true set skip-old ticks + 1]                                                                                                    ;
    [ set skip? false]                                                                                                                          ;
end                                                                                                                                             ;
																																				
																																				
;################################################################################################################################################
;########################################################### UPDATE PROCEDURES  #################################################################
;################################################################################################################################################

to update-start                                                                                                                                 ;	update procedure at the start of the GO procedure																														
  update-tech                                                                                                                                   ; update the tech measure attributes
  effect-of-investments                                                                                                                         ; update the tick in which the effect of the investment will take place per household
  production                                                                                                                                    ; update the production values of the household with solar panels and / or heat pumps
  set idle 0                                                                                                                                    ; reset idle number
  update-uncertainty-price                                                                                                                      ; update the uncertainty index value of the energy price
end
																																				
to update-end                                                                                                                                   ; update procedure after the GO procedure
  investments-overview-hh                                                                                                                       ; update the investment overview after new investments have been made
  update-score-investments                                                                                                                      ; update the scores of the household based on their investment performance
  update-skip-values                                                                                                                            ; updates the skip values
  update-emissions                                                                                                                              ; update the emission level
  update-households                                                                                                                             ; update the age of the household's representative
  set overview-all-investment reduce sentence [inv-list] of households                                                                          ; update the overview list of all the investments
end
																																				
to update-uncertainty-price                                                                                                                     ; updates the energy price uncertainty index value
  if ticks > 1 and unc-energy-prices-mean <= 0.4 [set uncertainty-energy-prices precision random-normal unc-energy-prices-mean 0.1 2]           ;
  if ticks > 1 and unc-energy-prices-mean > 0.4 and unc-energy-prices-mean <= 0.9                                                               ;
  [set uncertainty-energy-prices precision random-normal unc-energy-prices-mean 0.2 2]                                                          ;
  if ticks > 1 and unc-energy-prices-mean = 1                                                                                                   ;
  [set uncertainty-energy-prices precision random-normal unc-energy-prices-mean 0.4 2]                                                          ;
  if uncertainty-energy-prices < 0 [set uncertainty-energy-prices 0]                                                                            ;
end
																																				
to effect-of-investments                                                                                                                        ; based on effect-investment variable, determine the effect at that tick
    let agent-set households with [effect-investment != 0 and item 0 effect-investment = ticks]                                                 ; e.g. (5, 3) means at tick 5, effect insulation
    ask agent-set [                                                                                                                             ;
      let y item 1 effect-investment                                                                                                            ;
      if y = 2 [ set gas-consumption gas-consumption * 0.75                                                                                     ;
        set emissions-home emissions-home + (reduce sentence [tech-emissions] of measures with [m-type = "Double glazing"])]                    ;
      if y = 3 [ set gas-consumption gas-consumption * 0.75                                                                                     ;
        set emissions-home emissions-home +                                                                                                     ;
        (reduce sentence [tech-emissions] of measures with [m-type = "Insulation: walls, floor, roof"]) ]                                       ;
      if y = 4 [ set elect-consumption elect-consumption - reduce sentence [energy-production] of measures with [m-type = "PV"]                 ;
        set emissions-home emissions-home + (reduce sentence [tech-emissions] of measures with [m-type = "PV"])]                                ;
      if y = 5 [ set elect-consumption elect-consumption + reduce sentence [elect-for-heat] of measures with [m-type = "heatpump"]              ;
        set gas-consumption gas-consumption - 1200                                                                                              ;
        set emissions-home emissions-home + reduce sentence  [tech-emissions] of measures with [m-type = "heatpump"]]                           ;
      set effect-investment 0                                                                                                                   ;
    ]
end
																																				
to update-emissions                                                                                                                             ; based on tech emissions and consumptions, update emissions households
  set emissions-saved 0                                                                                                                         ; effect 1 M^3 - 1.8 KG/CO2, 1 kWh = 0.481 KG/CO2
  ask households [                                                                                                                              ;
      let emissions-old emissions-home                                                                                                          ;
      let gas (0.001785  * (gas-consumption - gas-prod))                                                                                        ;
      let elec (0.000315  * (elect-consumption - elect-prod))                                                                                   ;
      set emissions-home (0 + gas + elec)                                                                                                       ; https://www.rvo.nl/sites/default/files/2022-05/CE_Delft_210338_Emissiefactor_Elektriciteit_Fossiele_Bronnen_DEF.pdf
    if member? 2 inv-list [ set emissions-home emissions-home + 0.1 ]                                                                           ;
    if member? 3 inv-list [ set emissions-home emissions-home + 0.1 ]                                                                           ;
    if member? 4 inv-list [ set emissions-home emissions-home + 0.01 ]                                                                          ;
    if member? 5 inv-list [ set emissions-home emissions-home + 0.01 ]                                                                          ;
    set emissions-saved emissions-saved + (emissions-home - emissions-old)                                                                      ;
    ]
end
																																				
to check-r-seed                                                                                                                                 ; creates new random-seed if enabled within the interface
  if new-random-seed? [set r-seed new-seed]                                                                                                     ;
end
																																				
to update-households                                                                                                                            ; updates the age of household's representative
  update-age                                                                                                                                    ;
end
																																				
to production                                                                                                                                   ; update electricity and gas production of households with investments
  ifelse ticks > 0 [                                                                                                                            ;
      let x3 turtle-set households with [member? 4 inv-list and building-nmr = 0 and effect-investment = 0]                                     ;
      let x4 turtle-set households with [ member? 5 inv-list and effect-investment = 0]                                                         ;
      let x5 turtle-set households with [member? 4 inv-list and building-nmr > 0 and elect-prod = 0]                                            ;
      ask x3 [set elect-prod 0 set elect-prod elect-prod + reduce sentence [energy-production] of measures with [m-type = "PV"] ]               ;
      ask x4 [set gas-prod 0 set gas-prod gas-prod + reduce sentence [energy-production] of measures with [m-type = "heatpump"]]                ;
      ask x5 [set elect-prod 0 set elect-prod 650.5 ]                                                                                           ;
    ]
  [
    let x1 turtle-set households with [building-nmr = 0 and member? 4 inv-list]                                                                 ;
    let x2 turtle-set households with [member? 5 inv-list]                                                                                      ;
    ask x1 [set elect-prod 1301]                                                                                                                ;
    ask x2 [set gas-prod gas-prod + reduce sentence [energy-production] of measures with [m-type = "heatpump"]]                                 ;
																																				
  ]
end
																																				
to update-tech                                                                                                                                  ; updates the technology prices (measures)
    if ticks = 0 [ask measures [set tech-price tech-price * (1 + price-change)]]                                                                ; special case tick 1 and 2 (2021 and 2022) because we know the actual prices
    if ticks = 1 [                                                                                                                              ; after 2022, effect energy prices is based on slider interface with some unceratainty
      ask measures with [ m-type = "heatpump"] [set tech-price tech-price * (1.1)]                                                              ;
      set electricity-price precision (electricity-price * 1.515) 3                                                                             ;
      set gas-price precision (gas-price * 1.968) 3                                                                                             ; https://www.cbs.nl/nl-nl/nieuws/2022/07/prijs-van-energie-86-procent-hoger
    ]
    if ticks = 2 [                                                                                                                              ;
      ask measures with [ m-type = "heatpump"] [set tech-price tech-price * (1.2)]                                                              ;
      ask measures with [ m-type = "PV"] [set tech-price tech-price * (1.3)]                                                                    ;
      set electricity-price precision (0.4 + random-normal 0.4 0.1) 2 set gas-price precision (1.45 + random-normal 1.45 0.1) 2                 ;
    ]
    if ticks > 2 [ ask measures [set tech-price tech-price * (1 + price-change)]                                                                ;
    set electricity-price precision (random-normal electricity-price (uncertainty-energy-prices / electricity-price)) 3                         ;
    set gas-price precision (random-normal gas-price (uncertainty-energy-prices * 4 / gas-price)) 3                                             ;
    if electricity-price < 0 [set electricity-price 0.4]                                                                                        ;
    if electricity-price > 3 [set electricity-price 3]                                                                                          ;
    if gas-price < 0 [set gas-price 1.45]                                                                                                       ;
																																				
  ]
end
																																				
to update-age                                                                                                                                   ; updates the age of the household's representatives
  ask households [                                                                                                                              ; update age of households and see if they fall in a different category
    set age age + 1                                                                                                                             ; (depreciated) if they fall in a new age-category, find new similar neighbours
    if age = ((read-from-string substring age-cat 3 5) + 1)                                                                                     ;
    [                                                                                                                                           ;
      let x position age-cat age-categories-global                                                                                              ;
      set age-cat item (x + 1) age-categories-global                                                                                            ;
																																				
;      set sim-neigh [who] of households-here with [                                                                                            ;
;        wijk = [wijk] of myself and                                                                                                            ;
;        income = [income] of myself and                                                                                                        ;
;        age-cat = [age-cat] of myself and                                                                                                      ;
;        who != [who] of myself]
    ]
  ]
end
																																				
to investments-overview-hh                                                                                                                      ; update global lists for interface / plots
  set overview-investments reduce sentence [inv-list] of households                                                                             ;
  set overview-investments-owner reduce sentence [inv-list] of households with [ownership = "owner"]                                            ;
  set overview-investments-tenant reduce sentence [inv-list] of households with [ownership = "tenant"]                                          ;
  set strategies-owners-tick [0 0 0 0]                                                                                                          ;
  set strategies-tenants-tick [0 0 0 0]                                                                                                         ;
																																				
  if ticks = 0 [set strategies-owners-old reduce[ [ i a ] -> ( map + i a ) ]  [strategies-chosen] of households with [ownership = "owner"]      ;
    set strategies-tenants-old reduce[ [ i a ] -> ( map + i a ) ]  [strategies-chosen] of households with [ownership = "tenant"]]               ;
																																				
  if ticks > 0 [set strategies-owners-old strategies-owners                                                                                     ;
    set strategies-tenants-old strategies-tenants]                                                                                              ;
																																				
  set strategies-owners reduce[ [ i a ] -> ( map + i a ) ]  [strategies-chosen] of households with [ownership = "owner"]                        ;
  set strategies-tenants reduce[ [ i a ] -> ( map + i a ) ]  [strategies-chosen] of households with [ownership = "tenant"]                      ;
																																				
  ifelse ticks = 0 [set strategies-owners-tick strategies-owners]                                                                               ;
  [ set strategies-owners-tick (map - strategies-owners strategies-owners-old)]                                                                 ;
  ifelse ticks = 0 [set strategies-tenants-tick strategies-tenants]                                                                             ;
  [ set strategies-tenants-tick (map - strategies-tenants strategies-tenants-old)]                                                              ;
end
																																				
to update-district-colors                                                                                                                       ; updates the district labels
  ask patches [set pcolor black]                                                                                                                ;
  ask patches with [adopt-per-patch > 0] [                                                                                                      ;
    let y adopt-per-patch                                                                                                                       ;
    let x max [adopt-per-patch] of patches                                                                                                      ;
    set pcolor palette:scale-gradient [[255 0 0] [0 255 0]] y 0 x  ]                                                                            ;
end
																																				
to update-inv-colors                                                                                                                            ; updates the scores of the investments per household per 20%
  update-score-investments                                                                                                                      ;
  ask patches [set pcolor black]                                                                                                                ;
  let x turtle-set households with [score-inv > 0]                                                                                              ;
  let list-y (list (0) (precision (0.20 * 4)2) (precision (0.40 * 4)2) (precision (0.60 * 4)2) (precision (0.80 * 4)2) (4))                     ;
  ask x [                                                                                                                                       ;
    let x-count-list n-values 6 [mean [score-inv] of households-here]                                                                           ;
    let list-compare ( map [ [ a b ] -> a - b ] x-count-list list-y )                                                                           ;
    let list-compare1 map [i -> abs i] list-compare                                                                                             ;
    let test position min list-compare1 list-compare1                                                                                           ;
    if test = 0 [set pcolor red]                                                                                                                ;
    if test = 1 [set pcolor orange - 2 ]                                                                                                        ;
    if test = 2 [set pcolor orange ]                                                                                                            ;
    if test = 3 [set pcolor yellow ]                                                                                                            ;
    if test = 4 [set pcolor green + 2]                                                                                                          ;
    if test = 5 [set pcolor green]                                                                                                              ;
  ]
  if any? households with [not hidden?] [hide-households-button]                                                                                ;
end                                                                                                                                             																																			
																																				
to update-score-investments                                                                                                                     ; updates the scores of the investments
  ask households [                                                                                                                              ;
    set score-inv 0                                                                                                                             ;
    if member? 2 inv-list [ set score-inv score-inv + 1 ]                                                                                       ;
    if member? 3 inv-list [ set score-inv score-inv + 1 ]                                                                                       ;
    if member? 4 inv-list [ set score-inv score-inv + 1 ]                                                                                       ;
    if member? 5 inv-list [ set score-inv score-inv + 1 ]                                                                                       ;
  ]
end
																																				
to update-skip-values                                                                                                                           ; updates the skip values of the households (reset value)
  ask households with [skip-old = ticks] [set skip? false set color white set skip-old 0]                                                       ;
end

;################################################################################################################################################
;########################################################### REPORT PROCEDURES  #################################################################
;################################################################################################################################################
																																				
to-report %change-emissions-gas                                                                                                                 ; reports the gas consumption levels of the households
  report (sum [gas-consumption] of households - sum [gas-prod] of households) * 0.001785                                                        ;
end
																																				
to-report %change-emissions-electricity                                                                                                         ; reports the electricity consumption levels of the households
  report (sum [elect-consumption] of households - sum [elect-prod] of households) * 0.000315                                                    ;
end
																																				
to default-kWh/M3                                                                                                                               ; default values for each parameter within the interface
  set electricity-price elec-kwh-base                                                                                                           ;
  set gas-price gas-m3-base                                                                                                                     ;
  set maximum-years 10                                                                                                                          ;
  set min-building-size 3                                                                                                                       ;
  set strategies-owners [0 0 0 0]                                                                                                               ;
  set strategies-tenants [0 0 0 0]                                                                                                              ;
end
																																				
;################################################################################################################################################
;###################################################### INTERFACE PROCEDURE  ####################################################################
;################################################################################################################################################

to display-neighborhoods                                                                                                                        ; display the neighborhoods within the map
  ifelse dis-neig = 0 [                                                                                                                         ;
    set dis-neig 1                                                                                                                              ;
    gis:set-drawing-color violet                                                                                                                ;
																																				
    gis:draw neighborhoods-dataset 2                                                                                                            ;
  ]
  [set dis-neig 0                                                                                                                               ;
    gis:draw neighborhoods-dataset 0                                                                                                            ;
  ]                                                                                                                                             																																				
end
																																				
to display-districts                                                                                                                            ; display the district borders
  ifelse display-district-labels1 = 0                                                                                                           ;
  [set display-district-labels1 1                                                                                                               ;
    gis:set-drawing-color blue                                                                                                                  ;
    gis:draw districts-dataset 4                                                                                                                ;
  ]
  [
    set display-district-labels1 0                                                                                                              ;
    gis:draw districts-dataset 0                                                                                                                ;
  ]
end
																																				
to display/clear-district-labels                                                                                                                ; displays or removes the distric labels
  if-else display-district-labels = 1 [                                                                                                         ;
    set display-district-labels 0                                                                                                               ;
    ask district-labels [ die ]                                                                                                                 ;
  ]
  [
    set display-district-labels 1                                                                                                               ;
    foreach gis:feature-list-of districts-dataset [ this-district-vector-feature ->                                                             ;
      let centroid gis:location-of gis:centroid-of this-district-vector-feature                                                                 ;
      if not empty? centroid                                                                                                                    ;
      [ create-district-labels 1                                                                                                                ;
        [ set xcor item 0 centroid                                                                                                              ;
          set xcor xcor + 4                                                                                                                     ;
          set ycor item 1 centroid                                                                                                              ;
          set size 0                                                                                                                            ;
          set label gis:property-value this-district-vector-feature "district"                                                                  ;
          set label-color white                                                                                                                 ;
          if label = "Westpoort" [die]                                                                                                          ;
        ]
      ]
    ]
  ]
end
																																				
to display-buildings                                                                                                                            ; display the buildings
  if-else display-buildings-labels = 1 [                                                                                                        ;
    set display-buildings-labels 0                                                                                                              ;
    clear-drawing-button                                                                                                                        ;
    clear-drawing-button ]                                                                                                                      ;
    [
      set display-buildings-labels 1                                                                                                            ;
      gis:set-drawing-color red                                                                                                                 ;
      gis:draw buildings-dataset 1                                                                                                              ;
    ]
end
																																				
;################################################################################################################################################
;###################################################### GIS PROCEDURE  ##########################################################################
;################################################################################################################################################

to setup-gis-map-district                                                                                                                       ; setup GIS map coordinate system per chosen district
  if dataset-residents = "4-dataset-residents/centrum.csv" [                                                                                    ;
    gis:load-coordinate-system "3-bag-split/districts-split/Centrum/Centrum.prj"                                                                ;
    set buildings-dataset gis:load-dataset "3-bag-split/buildings-split/Centrum/Centrum.shp"                                                    ;
    set neighborhoods-dataset gis:load-dataset "3-bag-split/neighborhoods-split/Centrum/Centrum.shp"                                            ;
    set districts-dataset gis:load-dataset "3-bag-split/districts-split/Centrum/Centrum.shp"                                                    ;
    gis:set-world-envelope-ds (gis:envelope-union-of                                                                                            ;
      (gis:envelope-of buildings-dataset)                                                                                                       ;
      (gis:envelope-of neighborhoods-dataset)                                                                                                   ;
      (gis:envelope-of districts-dataset)                                                                                                       ;
      [219168.36498784827 223205.95238841785 5808737.690278248 5811752.98304086]                                                                ;
    )                                                                                                                                           ;
    ask patch 10 98 [set plabel "Centrum" ]                                                                                                     ;
  ]
																																				
  if dataset-residents = "4-dataset-residents/nieuw-west.csv" [                                                                                 ;
    gis:load-coordinate-system "3-bag-split/districts-split/Nieuw-West/Nieuw-West.prj"                                                          ;
    set buildings-dataset gis:load-dataset "3-bag-split/buildings-split/Nieuw-West/Nieuw-West.shp"                                              ;
    set neighborhoods-dataset gis:load-dataset "3-bag-split/neighborhoods-split/Nieuw-West/Nieuw-West.shp"                                      ;
    set districts-dataset gis:load-dataset "3-bag-split/districts-split/Nieuw-West/Nieuw-West.shp"                                              ;
    gis:set-world-envelope (gis:envelope-union-of                                                                                               ;
      (gis:envelope-of buildings-dataset)                                                                                                       ;
      (gis:envelope-of neighborhoods-dataset)                                                                                                   ;
      (gis:envelope-of districts-dataset)                                                                                                       ;
      [210936.20935921354 217111.4039406823 5805624.638530705 5812269.131413573] )                                                              ;
    ask patch 14 98 [set plabel "Nieuw-West" ]                                                                                                  ;
  ]
																																				
  if dataset-residents = "4-dataset-residents/noord.csv" [                                                                                      ;
    gis:load-coordinate-system "3-bag-split/districts-split/Noord/Noord.prj"                                                                    ;
    set buildings-dataset gis:load-dataset "3-bag-split/buildings-split/Noord/Noord.shp"                                                        ;
    set neighborhoods-dataset gis:load-dataset "3-bag-split/neighborhoods-split/Noord/Noord.shp"                                                ;
    set districts-dataset gis:load-dataset "3-bag-split/districts-split/Noord/Noord.shp"                                                        ;
    gis:set-world-envelope (gis:envelope-union-of                                                                                               ;
      (gis:envelope-of buildings-dataset)                                                                                                       ;
      (gis:envelope-of neighborhoods-dataset)                                                                                                   ;
      (gis:envelope-of districts-dataset)                                                                                                       ;
      [218202.12653161015 232656.54488092836 5809853.287294949 5816982.728837982] )                                                             ;
    ask patch 7 98 [set plabel "Noord" ]                                                                                                        ;
  ]
																																				
  if dataset-residents = "4-dataset-residents/oost.csv" [                                                                                       ;
    gis:load-coordinate-system "3-bag-split/districts-split/Oost/Oost.prj"                                                                      ;
    set buildings-dataset gis:load-dataset "3-bag-split/buildings-split/Oost/Oost.shp"                                                          ;
    set neighborhoods-dataset gis:load-dataset "3-bag-split/neighborhoods-split/Oost/Oost.shp"                                                  ;
    set districts-dataset gis:load-dataset "3-bag-split/districts-split/Oost/Oost.shp"                                                          ;
    gis:set-world-envelope-ds (gis:envelope-union-of                                                                                            ;
      (gis:envelope-of buildings-dataset)                                                                                                       ;
      (gis:envelope-of neighborhoods-dataset)                                                                                                   ;
      (gis:envelope-of districts-dataset)                                                                                                       ;
      [220304.74907714367 229080.0626632612 5804305.574424638 5811343.740244896] )                                                              ;
    ask patch 6 98 [set plabel "Oost" ]                                                                                                         ;
  ]
																																				
  if dataset-residents = "4-dataset-residents/west.csv" [                                                                                       ;
    gis:load-coordinate-system "3-bag-split/districts-split/West/West.prj"                                                                      ;
    set buildings-dataset gis:load-dataset "3-bag-split/buildings-split/West/west.shp"                                                          ;
    set neighborhoods-dataset gis:load-dataset "3-bag-split/neighborhoods-split/West/West.shp"                                                  ;
    set districts-dataset gis:load-dataset "3-bag-split/districts-split/West/West.shp"                                                          ;
    gis:set-world-envelope-ds (gis:envelope-union-of                                                                                            ;
      (gis:envelope-of buildings-dataset)                                                                                                       ;
      (gis:envelope-of neighborhoods-dataset)                                                                                                   ;
      (gis:envelope-of districts-dataset)                                                                                                       ;
      [216382.58573235135 219871.64456831536 5809331.6512702275 5811997.4022577815] )                                                           ;
    ask patch 6 98 [set plabel "West" ]                                                                                                         ;
																																				
  ]
  if dataset-residents = "4-dataset-residents/zuid.csv" [                                                                                       ;
    gis:load-coordinate-system "3-bag-split/districts-split/Zuid/Zuid.prj"                                                                      ;
    set buildings-dataset gis:load-dataset "3-bag-split/buildings-split/Zuid/Zuid.shp"                                                          ;
    set neighborhoods-dataset gis:load-dataset "3-bag-split/neighborhoods-split/Zuid/Zuid.shp"                                                  ;
    set districts-dataset gis:load-dataset "3-bag-split/districts-split/Zuid/Zuid.shp"                                                          ;
    gis:set-world-envelope-ds (gis:envelope-union-of                                                                                            ;
      (gis:envelope-of buildings-dataset)                                                                                                       ;
      (gis:envelope-of neighborhoods-dataset)                                                                                                   ;
      (gis:envelope-of districts-dataset)                                                                                                       ;
      [215003.69014464237 221764.215746817 5804831.391288961 5809822.918169942] )                                                               ;
    ask patch 5 98 [set plabel "Zuid" ]                                                                                                         ;
  ]
																																				
  if dataset-residents = "4-dataset-residents/zuid-oost.csv" [                                                                                  ;
    gis:load-coordinate-system "3-bag-split/districts-split/Zuidoost/Zuidoost.prj"                                                              ;
    set buildings-dataset gis:load-dataset "3-bag-split/buildings-split/Zuidoost/Zuidoost.shp"                                                  ;
    set neighborhoods-dataset gis:load-dataset "3-bag-split/neighborhoods-split/Zuidoost/Zuidoost.shp"                                          ;
    set districts-dataset gis:load-dataset "3-bag-split/districts-split/Zuidoost/Zuidoost.shp"                                                  ;
    gis:set-world-envelope-ds (gis:envelope-union-of                                                                                            ;
      (gis:envelope-of buildings-dataset)                                                                                                       ;
      (gis:envelope-of neighborhoods-dataset)                                                                                                   ;
      (gis:envelope-of districts-dataset)                                                                                                       ;
      [222459.67158689455 228776.2300195347 5799664.598483533 5805490.271049696] )                                                              ;
    ask patch 11 98 [set plabel "Zuid-Oost" ]                                                                                                   ;
																																				
  ]
																																				
  gis:set-coverage-minimum-threshold 0                                                                                                          ;
  gis:apply-coverage buildings-dataset "WK_CODE" wk_code                                                                                        ;
  gis:apply-coverage buildings-dataset "WK_NAAM" wk_naam                                                                                        ;
  gis:apply-coverage buildings-dataset "BOUWJAAR" const_year                                                                                    ;
  ask patches with [const_year > 0] [set const_year precision const_year 0]                                                                     ;
  ask patches with [not is-string? wk_naam] [  set wk_code " " set wk_naam " " set const_year " "]                                              ;
end


;;;;the following code block is not used as it is computationally demanding, but we include it here to demonstrate that is possible (at a smaller scale with less data) to load an envelope of all districts/areas
																																				
to setup-gis-map-all                                                                                                                            ; load the entire map (NOT USED)
  gis:load-coordinate-system "1-data-model/BAG_wijken/BAG3D_AMS_with-wijken.prj"                                                                ; automatic coordination system adjusted to the dataset

  set buildings-dataset gis:load-dataset "1-data-model/BAG_wijken/BAG3D_AMS_with-wijken.shp"                                                    ; load all datasets
  set neighborhoods-dataset gis:load-dataset "1-data-model/wijken-shape/amsterdam_wijken_without-weesp-polygon.shp"                             ;
  set districts-dataset gis:load-dataset "1-data-model/district-shape/amsterdam_stadsdeel_without-weesp-polygon.dbf"                            ;

  gis:set-world-envelope-ds (gis:envelope-union-of                                                                                              ; set the world envelope to the union of all dataset's envelopes
    (gis:envelope-of buildings-dataset)                                                                                                         ;
    (gis:envelope-of neighborhoods-dataset)                                                                                                     ;
    (gis:envelope-of districts-dataset)                                                                                                         ;
    [209471.0579208341 232656.54488092836 5799664.598483533 5817526.53944511] )                                                                 ;
																																			
  gis:set-coverage-minimum-threshold 0                                                                                                          ;
  gis:apply-coverage buildings-dataset "WK_CODE" wk_code                                                                                        ;
  gis:apply-coverage buildings-dataset "WK_NAAM" wk_naam                                                                                        ;

  display-districts                                                                                                                             ;
  display/clear-district-labels                                                                                                                 ;
end
;################################################################################################################################################
;###################################################### BUTTONS / GRAPHS PROCEDURE  #############################################################
;################################################################################################################################################
																														
to show-score-color-button                                                                                                                      ; show the scores of the investments
  ifelse any? patches with [pcolor = red or pcolor = green] [ask patches [ set pcolor black] ] [update-inv-colors]                              ;
end

to hide-households-button                                                                                                                       ; hide houdeholds
  ifelse any? households with [hidden? = true ] [ask households [set hidden? false] ] [ask households [set hidden? true]]                       ;
end                                                                                                                                             																																			                                                                                                                                          ;
																																				
to clear-drawing-button                                                                                                                         ; clear drawing
  clear-drawing                                                                                                                                 ;
  if display-district-labels = 1 [ display/clear-district-labels]                                                                               ;
  display-neighborhoods                                                                                                                         ;
  display-districts                                                                                                                             ;
  ask patches [set pcolor black]                                                                                                                ;
end
																																				
to reset-patches-button                                                                                                                         ; reset patches
  ask patches [set pcolor black]                                                                                                                ;
end
																																				
to distribution-households-button                                                                                                               ; show distribution of households
  let x patch-set patches with [any? households-here]                                                                                           ;
  let y mean [count households-here] of x                                                                                                       ;
  ask x [                                                                                                                                       ;
    set pcolor palette:scale-gradient [[230 230 250] [138 43 226] ] count households-here 0 y                                                   ;
  ]                                                                                                                                             ;
  if any? households with [not hidden?] [hide-households-button]                                                                                ;
end
																																				
to distribution-DG-button                                                                                                                       ; show distribution of double glazing
  ask patches [set pcolor black]                                                                                                                ;
  let x turtle-set households with [member? 2 inv-list]                                                                                         ;
  let y patch-set patches with [count households-here > 0]                                                                                      ;
  let y-one [count households-here with [member? 2 inv-list]] of patches                                                                        ;
  let y-two filter [i -> i > 0] y-one                                                                                                           ;
  let max-y mean y-two                                                                                                                          ;
  let list-y (list (0) (precision (0.20 * max-y)0)                                                                                              ;
  (precision (0.40 * max-y)0) (precision (0.60 * max-y)0) (precision (0.80 * max-y)0) (max-y))                                                  ;
  ask y [                                                                                                                                       ;
    let x-count count households-here with [member? 2 inv-list]                                                                                 ;
    let x-count-list n-values 6 [x-count]                                                                                                       ;
    let list-compare ( map [ [ a b ] -> a - b ] x-count-list list-y )                                                                           ;
    let list-compare1 map [i -> abs i] list-compare                                                                                             ;
    let test position min list-compare1 list-compare1                                                                                           ;
    if test = 0 [set pcolor red]                                                                                                                ;
    if test = 1 [set pcolor orange - 2 ]                                                                                                        ;
    if test = 2 [set pcolor orange ]                                                                                                            ;
    if test = 3 [set pcolor yellow ]                                                                                                            ;
    if test = 4 [set pcolor green + 2]                                                                                                          ;
    if test = 5 [set pcolor green]                                                                                                              ;
  ]
  if any? households with [not hidden?] [hide-households-button]                                                                                ;
end
																																				
to distribution-MI-button                                                                                                                       ; show distribution of major insulation
  ask patches [set pcolor black]                                                                                                                ;
  let x turtle-set households with [member? 3 inv-list]                                                                                         ;
  let y patch-set patches with [count households-here  > 0]                                                                                     ;
  let y-one [count households-here with [member? 3 inv-list]] of patches                                                                        ;
  let y-two filter [i -> i > 0] y-one                                                                                                           ;
  let max-y mean y-two                                                                                                                          ;
  let list-y (list (0) (precision (0.20 * max-y)0)                                                                                              ;
  (precision (0.40 * max-y)0) (precision (0.60 * max-y)0) (precision (0.80 * max-y)0) (max-y))                                                  ;
  ask y [                                                                                                                                       ;
    let x-count count households-here with [member? 3 inv-list]                                                                                 ;
    let x-count-list n-values 6 [x-count]                                                                                                       ;
    let list-compare ( map [ [ a b ] -> a - b ] x-count-list list-y )                                                                           ;
    let list-compare1 map [i -> abs i] list-compare                                                                                             ;
    let test position min list-compare1 list-compare1                                                                                           ;
    if test = 0 [set pcolor red]                                                                                                                ;
    if test = 1 [set pcolor orange - 2 ]                                                                                                        ;
    if test = 2 [set pcolor orange ]                                                                                                            ;
    if test = 3 [set pcolor yellow ]                                                                                                            ;
    if test = 4 [set pcolor green + 2]                                                                                                          ;
    if test = 5 [set pcolor green]                                                                                                              ;
  ]
  if any? households with [not hidden?] [hide-households-button]                                                                                ;
end
																																				
to distribution-HP-button                                                                                                                       ; show distribution of heat pumps
  ask patches [set pcolor black]                                                                                                                ;
  let x turtle-set households with [member? 5 inv-list]                                                                                         ;
  let y patch-set patches with [count households-here  > 0]                                                                                     ;
  let y-one [count households-here with [member? 5 inv-list]] of patches                                                                        ;
  let y-two filter [i -> i > 0] y-one                                                                                                           ;
  let max-y mean y-two                                                                                                                          ;
  let list-y (list (0) (precision (0.20 * max-y)0)                                                                                              ;
  (precision (0.40 * max-y)0) (precision (0.60 * max-y)0) (precision (0.80 * max-y)0) (max-y))                                                  ;
  ask y [                                                                                                                                       ;
    let x-count count households-here with [member? 5 inv-list]                                                                                 ;
    let x-count-list n-values 6 [x-count]                                                                                                       ;
    let list-compare ( map [ [ a b ] -> a - b ] x-count-list list-y )                                                                           ;
    let list-compare1 map [i -> abs i] list-compare                                                                                             ;
    let test position min list-compare1 list-compare1                                                                                           ;
    if test = 0 [set pcolor red]                                                                                                                ;
    if test = 1 [set pcolor orange - 2 ]                                                                                                        ;
    if test = 2 [set pcolor orange ]                                                                                                            ;
    if test = 3 [set pcolor yellow ]                                                                                                            ;
    if test = 4 [set pcolor green + 2]                                                                                                          ;
    if test = 5 [set pcolor green]                                                                                                              ;
  ]
  if any? households with [not hidden?] [hide-households-button]                                                                                ;
end
																																				
to distribution-PV-button                                                                                                                       ; show distribution of solar panels
  ask patches [set pcolor black]                                                                                                                ;
  let x turtle-set households with [member? 4 inv-list]                                                                                         ;
  let y patch-set patches with [count households-here  > 0]                                                                                     ;
  let y-one [count households-here with [member? 4 inv-list]] of patches                                                                        ;
  let y-two filter [i -> i > 0] y-one                                                                                                           ;
  let max-y mean y-two                                                                                                                          ;
  let list-y (list (0) (precision (0.20 * max-y)0)                                                                                              ;
  (precision (0.40 * max-y)0) (precision (0.60 * max-y)0) (precision (0.80 * max-y)0) (max-y))                                                  ;
  ask y [                                                                                                                                       ;
    let x-count count households-here with [member? 4 inv-list]                                                                                 ;
    let x-count-list n-values 6 [x-count]                                                                                                       ;
    let list-compare ( map [ [ a b ] -> a - b ] x-count-list list-y )                                                                           ;
    let list-compare1 map [i -> abs i] list-compare                                                                                             ;
    let test position min list-compare1 list-compare1                                                                                           ;
    if test = 0 [set pcolor red]                                                                                                                ;
    if test = 1 [set pcolor orange - 2 ]                                                                                                        ;
    if test = 2 [set pcolor orange ]                                                                                                            ;
    if test = 3 [set pcolor yellow ]                                                                                                            ;
    if test = 4 [set pcolor green + 2]                                                                                                          ;
    if test = 5 [set pcolor green]                                                                                                              ;
  ]
  if any? households with [not hidden?] [hide-households-button]                                                                                ;
end
																																				
to GIS-district-button                                                                                                                          ; GIS district procedure
  clear-all                                                                                                                                     ;
  setup-gis-map-district                                                                                                                        ;
  display-neighborhoods                                                                                                                         ;
  display-districts                                                                                                                             ;
  reset-ticks                                                                                                                                   ;
end

;################################################################################################################################################
;###################################################### MISCELLANEOUS PROCEDURES  ###############################################################
;################################################################################################################################################																																				
																																				
to evaluate-needs-price-cap                                                                                                                     ;
  let electricity-perc (elect-consumption - elect-prod) / 2900                                                                                  ;
  let gas-perc  (gas-consumption - gas-prod) / 1200                                                                                             ;
  if chosen-interact != 0 and electricity-perc > 1 and gas-perc > 1[                                                                            ;
    let yearly-energy-cost (0.4 * 2900 + (((electricity-perc - 1) * 2900 ) *                                                                    ;
    electricity-price)) + (1.45 * 1200 + (((gas-perc - 1) * 1200 ) *  gas-price))                                                               ;
    let lns-e income-value - yearly-energy-cost                                                                                                 ;
																																				
    let comparison-inv-list length filter [i -> i = true] (map = inv-list [inv-list] of chosen-interact)                                        ;
																																				
    ifelse chosen-interact != [] and comparison-inv-list >= 1                                                                                   ;
    [ set LNS-s cohesion * contact-neighbors + 1]                                                                                               ;
    [ set LNS-s cohesion * contact-neighbors + 0.5]                                                                                             ;
																																				
																																				
    ifelse lns-e > 59500                                                                                                                        ;
    [set LNS (1 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                                        ;
    [ifelse lns-e >= 42600                                                                                                                      ;
      [set LNS (0.75 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                                   ;
      [ifelse lns-e >= 30200                                                                                                                    ;
        [set LNS (0.5 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                                  ;
        [ifelse lns-e >= 21000                                                                                                                  ;
          [set LNS (0.25 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                               ;
          [set LNS 0]                                                                                                                           ;
        ]                                                                                                                                       ;
      ]                                                                                                                                         ;
    ]                                                                                                                                           ;
  ]                                                                                                                                             ;
  if chosen-interact != 0 and electricity-perc < 1 and gas-perc > 1[                                                                            ;
    let yearly-energy-cost (0.4 * (electricity-perc * 2900))  +                                                                                 ;
    (1.45 * 1200 + (((gas-perc - 1) * 1200 ) *  gas-price))                                                                                     ;
    let lns-e income-value - yearly-energy-cost                                                                                                 ;
																																				
    let comparison-inv-list length filter [i -> i = true] (map = inv-list [inv-list] of chosen-interact)                                        ;
																																				
    ifelse chosen-interact != [] and comparison-inv-list >= 1                                                                                   ;
    [ set LNS-s cohesion * contact-neighbors + 1]                                                                                               ;
    [ set LNS-s cohesion * contact-neighbors + 0.5]                                                                                             ;
																																				
																																				
    ifelse lns-e > 59500                                                                                                                        ;
    [set LNS (1 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                                        ;
    [ifelse lns-e >= 42600                                                                                                                      ;
      [set LNS (0.75 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                                   ;
      [ifelse lns-e >= 30200                                                                                                                    ;
        [set LNS (0.5 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                                  ;
        [ifelse lns-e >= 21000                                                                                                                  ;
          [set LNS (0.25 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                               ;
          [set LNS 0]                                                                                                                           ;
        ]                                                                                                                                       ;
      ]                                                                                                                                         ;
    ]                                                                                                                                           ;
  ]                                                                                                                                             ;
  if chosen-interact != 0 and electricity-perc < 1 and gas-perc < 1[                                                                            ;
    let yearly-energy-cost (0.4 * (electricity-perc * 2900)) + (1.45 * (gas-perc * 1200))                                                       ;
    let lns-e income-value - yearly-energy-cost                                                                                                 ;
																																				
    let comparison-inv-list length filter [i -> i = true] (map = inv-list [inv-list] of chosen-interact)                                        ;
																																				
    ifelse chosen-interact != [] and comparison-inv-list >= 1                                                                                   ;
    [ set LNS-s cohesion * contact-neighbors + 1]                                                                                               ;
    [ set LNS-s cohesion * contact-neighbors + 0.5]                                                                                             ;
																																				
																																				
    ifelse lns-e > 59500                                                                                                                        ;
    [set LNS (1 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                                        ;
    [ifelse lns-e >= 42600                                                                                                                      ;
      [set LNS (0.75 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                                   ;
      [ifelse lns-e >= 30200                                                                                                                    ;
        [set LNS (0.5 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                                  ;
        [ifelse lns-e >= 21000                                                                                                                  ;
          [set LNS (0.25 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                               ;
          [set LNS 0]                                                                                                                           ;
        ]                                                                                                                                       ;
      ]                                                                                                                                         ;
    ]                                                                                                                                           ;
  ]                                                                                                                                             ;
  if chosen-interact != 0 and electricity-perc > 1 and gas-perc < 1[                                                                            ;
    let yearly-energy-cost (0.4 * 2900 + (((electricity-perc - 1) * 2900 ) *                                                                    ;
    electricity-price)) + (1.45 * (gas-perc * 1200))                                                                                            ;
    let lns-e income-value - yearly-energy-cost                                                                                                 ;
																																				
    let comparison-inv-list length filter [i -> i = true] (map = inv-list [inv-list] of chosen-interact)                                        ;
																																				
    ifelse chosen-interact != [] and comparison-inv-list >= 1                                                                                   ;
    [ set LNS-s cohesion * contact-neighbors + 1]                                                                                               ;
    [ set LNS-s cohesion * contact-neighbors + 0.5]                                                                                             ;
																																				
																																				
    ifelse lns-e > 59500                                                                                                                        ;
    [set LNS (1 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                                        ;
    [ifelse lns-e >= 42600                                                                                                                      ;
      [set LNS (0.75 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                                   ;
      [ifelse lns-e >= 30200                                                                                                                    ;
        [set LNS (0.5 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                                  ;
        [ifelse lns-e >= 21000                                                                                                                  ;
          [set LNS (0.25 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                               ;
          [set LNS 0]                                                                                                                           ;
        ]                                                                                                                                       ;
      ]                                                                                                                                         ;
    ]                                                                                                                                           ;
  ]                                                                                                                                             ;
																																				
  if is-agent? chosen-interact = true and electricity-perc > 1 and gas-perc > 1  [                                                              ;
    let yearly-energy-cost (0.4 * 2900 + (((electricity-perc - 1) * 2900 ) *  electricity-price)) +                                             ;
    (1.45 * 1200 + (((gas-perc - 1) * 1200 ) *  gas-price))                                                                                     ;
    let left-amount income-value - yearly-energy-cost                                                                                           ;
    set LNS-s cohesion * contact-neighbors + 0.5                                                                                                ;
																																				
																																				
    ifelse left-amount > 59500                                                                                                                  ;
    [set LNS (1 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                                        ;
    [ifelse left-amount >= 42600                                                                                                                ;
      [set LNS (0.75 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                                   ;
      [ifelse left-amount >= 30200                                                                                                              ;
        [set LNS (0.5 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                                  ;
        [ifelse left-amount >= 21000                                                                                                            ;
          [set LNS (0.25 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                               ;
          [set LNS 0]                                                                                                                           ;
        ]                                                                                                                                       ;
      ]                                                                                                                                         ;
    ]                                                                                                                                           ;
  ]                                                                                                                                             ;
																																				
  if is-agent? chosen-interact = true and electricity-perc < 1 and gas-perc > 1  [                                                              ;
    let yearly-energy-cost (0.4 * (electricity-perc * 2900))  +                                                                                 ;
    (1.45 * 1200 + (((gas-perc - 1) * 1200 ) *  gas-price))                                                                                     ;
    let left-amount income-value - yearly-energy-cost                                                                                           ;
    set LNS-s cohesion * contact-neighbors + 0.5                                                                                                ;
																																				
																																				
    ifelse left-amount > 59500                                                                                                                  ;
    [set LNS (1 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                                        ;
    [ifelse left-amount >= 42600                                                                                                                ;
      [set LNS (0.75 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                                   ;
      [ifelse left-amount >= 30200                                                                                                              ;
        [set LNS (0.5 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                                  ;
        [ifelse left-amount >= 21000                                                                                                            ;
          [set LNS (0.25 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                               ;
          [set LNS 0]                                                                                                                           ;
        ]                                                                                                                                       ;
      ]                                                                                                                                         ;
    ]                                                                                                                                           ;
  ]                                                                                                                                             ;
																																				
  if is-agent? chosen-interact = true and electricity-perc < 1 and gas-perc > 1  [                                                              ;
    let yearly-energy-cost (0.4 * (electricity-perc * 2900)) + (1.45 * (gas-perc * 1200))                                                       ;
    let left-amount income-value - yearly-energy-cost                                                                                           ;
    set LNS-s cohesion * contact-neighbors + 0.5                                                                                                ;
																																				
																																				
    ifelse left-amount > 59500                                                                                                                  ;
    [set LNS (1 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                                        ;
    [ifelse left-amount >= 42600                                                                                                                ;
      [set LNS (0.75 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                                   ;
      [ifelse left-amount >= 30200                                                                                                              ;
        [set LNS (0.5 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                                  ;
        [ifelse left-amount >= 21000                                                                                                            ;
          [set LNS (0.25 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                               ;
          [set LNS 0]                                                                                                                           ;
        ]                                                                                                                                       ;
      ]                                                                                                                                         ;
    ]                                                                                                                                           ;
  ]                                                                                                                                             ;
																																				
  if is-agent? chosen-interact = true and electricity-perc > 1 and gas-perc < 1  [                                                              ;
    let yearly-energy-cost (0.4 * 2900 + (((electricity-perc - 1) * 2900 ) *                                                                    ;
    electricity-price)) + (1.45 * (gas-perc * 1200))                                                                                            ;
    let left-amount income-value - yearly-energy-cost                                                                                           ;
    set LNS-s cohesion * contact-neighbors + 0.5                                                                                                ;
																																				
																																				
    ifelse left-amount > 59500                                                                                                                  ;
    [set LNS (1 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                                        ;
    [ifelse left-amount >= 42600                                                                                                                ;
      [set LNS (0.75 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                                   ;
      [ifelse left-amount >= 30200                                                                                                              ;
        [set LNS (0.5 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                                  ;
        [ifelse left-amount >= 21000                                                                                                            ;
          [set LNS (0.25 ^ 0.5 ) * (LNS-s ^ 0.5)]                                                                                               ;
          [set LNS 0]                                                                                                                           ;
        ]                                                                                                                                       ;
      ]                                                                                                                                         ;
    ]                                                                                                                                           ;
  ]                                                                                                                                             ;
end

; Copyright ® Erkinai Derkenbaeva, Juriaan Wolfers
; 2023
@#$#@#$#@
GRAPHICS-WINDOW
210
10
824
625
-1
-1
6.0
1
15
1
1
1
0
0
1
1
0
100
0
100
1
1
1
year
30.0

MONITOR
826
56
944
101
# households
count households
17
1
11

BUTTON
24
721
119
754
clear-drawing
clear-drawing-button
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
826
625
945
658
(un)hide-households
hide-households-button
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

MONITOR
826
453
945
498
AVG kWh
mean [elect-consumption] of households
0
1
11

MONITOR
826
498
945
543
AVG gas M^3
mean [gas-consumption] of households
0
1
11

BUTTON
22
520
119
553
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
0

SWITCH
20
345
201
378
new-random-seed?
new-random-seed?
1
1
-1000

MONITOR
826
541
945
586
NIL
r-seed
17
1
11

BUTTON
210
625
330
658
#-adoptions-patch
if count households > 0 [update-district-colors]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
826
659
945
692
NIL
display-buildings
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
24
753
119
786
reset-patches
reset-patches-button
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
703
626
824
659
avg-adoption-rate
update-inv-colors
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

PLOT
954
34
1300
272
Player strategies owners
year
#
0.0
10.0
0.0
10.0
true
true
"set-plot-background-color grey + 3\n" ""
PENS
"repeat" 1.0 0 -16777216 true "" "plot item 0 strategies-owners-tick"
"imitate" 1.0 0 -7500403 true "" "plot item 1 strategies-owners-tick"
"optimize" 1.0 0 -10899396 true "" "plot item 2 strategies-owners-tick"
"inquire" 1.0 0 -1184463 true "" "plot item 3 strategies-owners-tick"

BUTTON
22
552
119
585
go-single
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

PLOT
1115
807
1388
1001
emissions gas / electricity
year
CO2 / ton
0.0
10.0
0.0
100.0
true
true
"set-plot-background-color grey + 3" ""
PENS
"CO2 gas" 1.0 0 -16777216 true "" "plot %change-emissions-gas"
"CO2 kWh" 1.0 0 -7500403 true "" "plot %change-emissions-electricity"
"Net zero" 1.0 0 -2674135 true "" "plot 0"

PLOT
567
807
842
1001
electricity consumption
year
kWh
0.0
10.0
0.0
10.0
true
true
"set-plot-background-color grey + 3" ""
PENS
"owner" 1.0 0 -13345367 true "" "plot sum [elect-consumption] of households with [ownership = \"owner\"]"
"tenant" 1.0 0 -2674135 true "" "plot sum [elect-consumption] of households with [ownership = \"tenant\"]"

PLOT
954
507
1300
744
investments change owners
year
#
0.0
10.0
0.0
10.0
false
true
"set-plot-background-color grey + 3\nset-plot-y-range 0 (length filter [ i -> i = 2 ] overview-investments-owner + 5000)" ""
PENS
"double glazing" 1.0 0 -13345367 true "" "plot length filter [ i -> i = 2 ] overview-investments-owner"
"major insulation" 1.0 0 -6459832 true "" "plot length filter [ i -> i = 3 ] overview-investments-owner"
"PV" 1.0 0 -1184463 true "" "plot length filter [ i -> i = 4 ] overview-investments-owner"
"heat pump" 1.0 0 -10899396 true "" "plot length filter [ i -> i = 5 ] overview-investments-owner"

MONITOR
826
11
944
56
year
2021 + ticks
17
1
11

INPUTBOX
20
95
108
155
maximum-years
10.0
1
0
Number

SWITCH
20
378
201
411
owners-only?
owners-only?
1
1
-1000

BUTTON
24
640
119
673
NIL
default-kWh/M3
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
22
464
117
497
setup
GIS-district-button\nsetup
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
24
688
119
721
NIL
clear-all
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
24
607
119
640
clear
clear-all-plots\nask households [die]\nask measures [die]\nreset-patches-button\ndefault-kWh/M3\nclear-globals
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
22
27
201
72
dataset-residents
dataset-residents
"4-dataset-residents/centrum.csv" "4-dataset-residents/nieuw-west.csv" "4-dataset-residents/noord.csv" "4-dataset-residents/oost.csv" "4-dataset-residents/west.csv" "4-dataset-residents/zuid.csv" "4-dataset-residents/zuid-oost.csv"
6

TEXTBOX
25
10
175
28
1: select district
12
0.0
1

TEXTBOX
24
75
174
93
2: adjust parameters
12
0.0
1

TEXTBOX
24
447
174
465
3: setup settings
12
0.0
1

TEXTBOX
25
503
175
521
4: run model
12
0.0
1

TEXTBOX
26
589
176
607
5: clear / reset\n
12
0.0
1

SLIDER
20
216
201
249
unc-energy-prices-mean
unc-energy-prices-mean
0
1
0.2
0.1
1
NIL
HORIZONTAL

SWITCH
20
312
201
345
collective-dm?
collective-dm?
0
1
-1000

INPUTBOX
107
95
197
155
min-building-size
3.0
1
0
Number

MONITOR
826
191
945
236
# ap-buildings all
length [building-nmr] of households with [building-nmr != 0]
17
1
11

MONITOR
826
279
945
324
buildings-PV-all
length remove-duplicates [building-nmr] of households with [building-nmr != 0 and member? 4 inv-list]
17
1
11

PLOT
841
807
1116
1001
gas consumption
year
M3
0.0
10.0
0.0
10.0
true
true
"set-plot-background-color grey + 3" ""
PENS
"owner" 1.0 0 -13345367 true "" "plot sum [gas-consumption] of households with [ownership = \"owner\"]"
"tenant" 1.0 0 -2674135 true "" "plot sum [gas-consumption] of households with [ownership = \"tenant\"]"

PLOT
954
271
1300
508
LNS owners
year
NIL
0.0
10.0
0.0
1.0
false
true
"set-plot-background-color grey + 3" ""
PENS
"Satisfied" 1.0 0 -10899396 true "" "plot (count households with [LNS > LNS-min and ownership = \"owner\"] / count households with [ownership = \"owner\"])"
"Unsatisfied" 1.0 0 -2674135 true "" "plot (count households with [LNS <= LNS-min and ownership = \"owner\"] / count households with [ownership = \"owner\"])"
"mean-LNS" 1.0 0 -16777216 true "" "plot mean [LNS] of households with [ownership = \"owner\"]"
"avg-min-LNS" 1.0 0 -7500403 true "" "plot mean [LNS-min] of households with [ownership = \"owner\"]"

PLOT
1299
270
1633
508
Uncertainty owners
year
(0 - 1)
0.0
10.0
0.0
1.0
true
true
"set-plot-background-color grey + 3" ""
PENS
"unc-consumat" 1.0 0 -16777216 true "" "plot mean [consumat-uncertainty] of households with [ownership = \"owner\"]"
"unc-prices" 1.0 0 -7500403 true "" "plot uncertainty-energy-prices"
"unc-tolerance" 1.0 0 -2674135 true "" "plot uncertainty-tol"
"avg-uncertainty" 1.0 0 -955883 true "" "plot mean [want-to-move] of households with [ownership = \"owner\"]"

PLOT
1299
34
1633
272
Player strategies tenant
year
#
0.0
10.0
0.0
10.0
true
true
"set-plot-background-color grey + 3" ""
PENS
"repeat" 1.0 0 -16777216 true "" "plot item 0 strategies-tenants-tick"
"imitate" 1.0 0 -7500403 true "" "plot item 1 strategies-tenants-tick"
"optimize" 1.0 0 -10899396 true "" "plot item 2 strategies-tenants-tick"
"inquire" 1.0 0 -1184463 true "" "plot item 3 strategies-tenants-tick"

PLOT
1299
507
1633
744
investments change tenants
year
#
0.0
10.0
0.0
10.0
true
true
"set-plot-background-color grey + 3\nset-plot-y-range 0 (length filter [ i -> i = 2 ] overview-investments-owner + 5000)" ""
PENS
"double glazing" 1.0 0 -13345367 true "" "plot length filter [ i -> i = 2 ] overview-investments-tenant"
"major insulation" 1.0 0 -6459832 true "" "plot length filter [ i -> i = 3 ] overview-investments-tenant"
"PV" 1.0 0 -1184463 true "" "plot length filter [ i -> i = 4 ] overview-investments-tenant"
"heat pump" 1.0 0 -10899396 true "" "plot length filter [ i -> i = 5 ] overview-investments-tenant"

TEXTBOX
477
779
806
817
Consumption & emissions
15
0.0
1

TEXTBOX
954
11
1104
29
Consumat
15
0.0
1

MONITOR
826
101
944
146
# owners
count households with [ownership = \"owner\"]
17
1
11

MONITOR
826
147
945
192
# tenants
count households with [ownership = \"tenant\"]
17
1
11

PLOT
1111
1427
1387
1619
types of landlords
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"set-plot-x-range 1 7\nset-plot-pen-interval 0\nset-histogram-num-bars 6\nset-plot-background-color grey + 3" ""
PENS
"pension / insur" 1.0 1 -16777216 true "" "let y length filter [i -> i = \"pension fund or insurance company\"] [landlord] of households\nlet x n-values y [1]\nhistogram x"
"h-corporation" 1.0 1 -1 true "" "let y length filter [i -> i = \"housing corporation\"] [landlord] of households\nlet x n-values y [2] histogram x"
"private" 1.0 1 -13345367 true "" "let y length filter [i -> i = \"private person\"] [landlord] of households\nlet x n-values y [3]\nhistogram x"
"family" 1.0 1 -2674135 true "" "let y length filter [i -> i = \"family\"] [landlord] of households\nlet x n-values y [4]\nhistogram x"
"munic / govern" 1.0 1 -10899396 true "" "let y length filter [i -> i = \"municipality or government\"] [landlord] of households\nlet x n-values y [5]\nhistogram x"
"none" 1.0 1 -1184463 true "" "let y length filter [i -> i = \"none from above\"] [landlord] of households\nlet x n-values y [6]\nhistogram x"

TEXTBOX
484
1206
667
1244
Overview district owners
15
0.0
1

PLOT
18
1233
292
1426
Age owners
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"set-plot-x-range 1 8\nset-plot-pen-interval 0\nset-histogram-num-bars 7\nset-plot-background-color grey + 3" ""
PENS
"17-24" 1.0 1 -16777216 true "" "let y length filter [i -> i = \"17-24\"] [age-cat] of households with [ownership = \"owner\"]\nlet x n-values y [1]\nhistogram x"
"25-34" 1.0 1 -1 true "" "let y length filter [i -> i = \"25-34\"] [age-cat] of households with [ownership = \"owner\"]\nlet x n-values y [2]\nhistogram x"
"35-44" 1.0 1 -13345367 true "" "let y length filter [i -> i = \"35-44\"] [age-cat] of households with [ownership = \"owner\"]\nlet x n-values y [3]\nhistogram x"
"45-54" 1.0 1 -2674135 true "" "let y length filter [i -> i = \"45-54\"] [age-cat] of households with [ownership = \"owner\"]\nlet x n-values y [4]\nhistogram x"
"55-64" 1.0 1 -10899396 true "" "let y length filter [i -> i = \"55-64\"] [age-cat] of households with [ownership = \"owner\"]\nlet x n-values y [5]\nhistogram x"
"65-74" 1.0 1 -1184463 true "" "let y length filter [i -> i = \"65-74\"] [age-cat] of households with [ownership = \"owner\"]\nlet x n-values y [6]\nhistogram x"
"75-100" 1.0 1 -2064490 true "" "let y length filter [i -> i = \"75-100\"] [age-cat] of households with [ownership = \"owner\"]\nlet x n-values y [7]\nhistogram x"

PLOT
291
1233
565
1427
Income owners
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"set-plot-x-range 1 6\nset-plot-pen-interval 0\nset-histogram-num-bars 5\nset-plot-background-color grey + 3" ""
PENS
"-21000" 1.0 1 -16777216 true "" "let y length filter [i -> i = -21000] [income] of households with [ownership = \"owner\"]\nlet x n-values y [1]\nhistogram x"
"<30.2" 1.0 1 -1 true "" "let y length filter [i -> i = \"21000-30200\"] [income] of households with [ownership = \"owner\"]\nlet x n-values y [2]\nhistogram x"
"<42.6" 1.0 1 -13345367 true "" "let y length filter [i -> i = \"30200-42600\"] [income] of households with [ownership = \"owner\"]\nlet x n-values y [3]\nhistogram x"
"<59.5" 1.0 1 -2674135 true "" "let y length filter [i -> i = \"42600-59500\"] [income] of households with [ownership = \"owner\"]\nlet x n-values y [4]\nhistogram x"
"59.5+" 1.0 1 -10899396 true "" "let y length filter [i -> i = \"59500+\"] [income] of households with [ownership = \"owner\"]\nlet x n-values y [5]\nhistogram x"

PLOT
565
1233
839
1428
Education owners
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"set-plot-x-range 1 4\nset-plot-pen-interval 0\nset-histogram-num-bars 3\nset-plot-background-color grey + 3" ""
PENS
"low" 1.0 1 -16777216 true "" "let y length filter [i -> i = \"low\"] [education] of households with [ownership = \"owner\"]\nlet x n-values y [1]\nhistogram x"
"middle" 1.0 1 -1 true "" "let y length filter [i -> i = \"middle\"] [education] of households with [ownership = \"owner\"]\nlet x n-values y [2]\nhistogram x"
"high" 1.0 1 -13345367 true "" "let y length filter [i -> i = \"high\"] [education] of households with [ownership = \"owner\"]\nlet x n-values y [3]\nhistogram x"

PLOT
838
1233
1113
1428
Household composition owners
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"set-plot-x-range 1 6\nset-plot-pen-interval 0\nset-histogram-num-bars 5\nset-plot-background-color grey + 3" ""
PENS
"couple" 1.0 1 -16777216 true "" "let y length filter [i -> i = \"couple\"] [h-composition] of households with [ownership = \"owner\"]\nlet x n-values y [1]\nhistogram x"
"single" 1.0 1 -1 true "" "let y length filter [i -> i = \"single-person\"] [h-composition] of households with [ownership = \"owner\"]\nlet x n-values y [2]\nhistogram x"
"couple + child." 1.0 1 -13345367 true "" "let y length filter [i -> i = \"couple+children\"] [h-composition] of households with [ownership = \"owner\"]\nlet x n-values y [3]\nhistogram x"
"one-parent" 1.0 1 -2674135 true "" "let y length filter [i -> i = \"one-parent\"] [h-composition] of households with [ownership = \"owner\"]\nlet x n-values y [4]\nhistogram x"
"other" 1.0 0 -10899396 true "" "let y length filter [i -> i = \"other\"] [h-composition] of households with [ownership = \"owner\"]\nlet x n-values y [5]\nhistogram x"

PLOT
1111
1233
1388
1428
Contact to neighbours owners
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"set-plot-x-range 1 6\nset-plot-pen-interval 0\nset-histogram-num-bars 5\nset-plot-background-color grey + 3" ""
PENS
"0" 1.0 1 -16777216 true "" "let y length filter [i -> i = 0] [contact-neighbors] of households with [ownership = \"owner\"]\nlet x n-values y [1]\nhistogram x"
"0.25" 1.0 1 -1 true "" "let y length filter [i -> i = 0.25] [contact-neighbors] of households with [ownership = \"owner\"]\nlet x n-values y [2]\nhistogram x"
"0.5" 1.0 1 -13345367 true "" "let y length filter [i -> i = 0.5] [contact-neighbors] of households with [ownership = \"owner\"]\nlet x n-values y [3]\nhistogram x"
"0.75" 1.0 1 -2674135 true "" "let y length filter [i -> i = 0.75] [contact-neighbors] of households with [ownership = \"owner\"]\nlet x n-values y [4]\nhistogram x"
"1" 1.0 1 -10899396 true "" "let y length filter [i -> i = 1] [contact-neighbors] of households with [ownership = \"owner\"]\nlet x n-values y [5]\nhistogram x"

PLOT
18
1425
292
1619
Dwelling type owners
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"set-plot-x-range 1 3\nset-plot-pen-interval 0\nset-histogram-num-bars 2\nset-plot-background-color grey + 3" ""
PENS
"apartment" 1.0 1 -16777216 true "" "let y length filter [i -> i = \"apartment\"] [d-type] of households with [ownership = \"owner\"]\nlet x n-values y [1]\nhistogram x"
"non-apartment" 1.0 1 -1 true "" "let y length filter [i -> i = \"non-apartment\"] [d-type] of households with [ownership = \"owner\"]\nlet x n-values y [2]\nhistogram x"

PLOT
838
1426
1112
1619
Construction year owners
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"set-plot-x-range 1 7\nset-plot-pen-interval 0\nset-histogram-num-bars 7\nset-plot-background-color grey + 3" ""
PENS
"-1946" 1.0 1 -16777216 true "" "let y length filter [i -> i = -1946] [const-year-cat] of households with [ownership = \"owner\"]\nlet x n-values y [1]\nhistogram x"
"<1980" 1.0 1 -1 true "" "let y length filter [i -> i = \"1946-1980\"] [const-year-cat] of households with [ownership = \"owner\"]\nlet x n-values y [2]\nhistogram x"
"<1990" 1.0 1 -13345367 true "" "let y length filter [i -> i = \"1981-1990\"] [const-year-cat] of households with [ownership = \"owner\"]\nlet x n-values y [3]\nhistogram x"
"<2000" 1.0 1 -2674135 true "" "let y length filter [i -> i = \"1991-2000\"] [const-year-cat] of households with [ownership = \"owner\"]\nlet x n-values y [4]\nhistogram x"
"<2010" 1.0 1 -10899396 true "" "let y length filter [i -> i = \"2001-2010\"] [const-year-cat] of households with [ownership = \"owner\"]\nlet x n-values y [5]\nhistogram x"
"2010+" 1.0 1 -1184463 true "" "let y length filter [i -> i = \"2010+\"] [const-year-cat] of households with [ownership = \"owner\"]\nlet x n-values y [6]\nhistogram x"

BUTTON
826
594
945
627
distribution-households
distribution-households-button
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
442
658
553
691
distribution-PV
distribution-PV-button
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
442
625
552
658
distribution-HP
distribution-HP-button
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
329
625
442
658
distribution-DG
distribution-DG-button
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
329
658
443
691
distribution-MI
distribution-MI-button
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
552
625
623
658
DG-run
distribution-DG-button
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
553
658
623
691
MI-run
distribution-MI-button
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
622
625
692
658
HP-run
distribution-HP-button
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
623
658
692
691
PV-run
distribution-PV-button
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
703
659
824
692
avg-adoption-run
update-inv-colors
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
210
658
330
691
#-adoptions-run
if count households > 0 [update-district-colors]
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

TEXTBOX
741
509
837
619
Color explanation:\n0%   Red\n20%  Red / orange\n40%  Orange\n60%  Yellow\n80%  Light green\n100% Green\n\n(% based on avg. number)
9
9.9
1

PLOT
18
807
293
1001
Electricity balance owners
year
NIL
0.0
10.0
0.0
2.0
true
true
"set-plot-background-color grey + 3" ""
PENS
"kWh" 1.0 0 -16777216 true "" "plot ( sum [elect-prod] of households with [ownership = \"owner\"] / sum [elect-consumption] of households with [ownership = \"owner\"])"
"balance" 1.0 0 -2674135 true "" "plot 1"

PLOT
18
1000
293
1194
Electricity balance all
year
NIL
0.0
10.0
0.0
2.0
true
true
"set-plot-background-color grey + 3" ""
PENS
"kWh" 1.0 0 -16777216 true "" "plot ( sum [elect-prod] of households / sum [elect-consumption] of households)"
"balance" 1.0 0 -2674135 true "" "plot 1"

MONITOR
826
235
945
280
# ap-buildings own
length [building-nmr] of households with [building-nmr != 0 and ownership = \"owner\" and count same-building with [ownership = \"tenant\"] < 1]
17
1
11

MONITOR
826
322
945
367
buildings-PV-owners
length remove-duplicates [building-nmr] of households with [building-nmr != 0 and member? 4 inv-list and ownership = \"owner\" and count same-building with [ownership = \"tenant\"] < 1]
17
1
11

PLOT
291
1426
565
1619
Dwelling type tenant
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"set-plot-x-range 1 3\nset-plot-pen-interval 0\nset-histogram-num-bars 2\nset-plot-background-color grey + 3" ""
PENS
"apartment" 1.0 1 -16777216 true "" "let y length filter [i -> i = \"apartment\"] [d-type] of households with [ownership = \"tenant\"]\nlet x n-values y [1]\nhistogram x"
"non-apartment" 1.0 1 -1 true "" "let y length filter [i -> i = \"non-apartment\"] [d-type] of households with [ownership = \"tenant\"]\nlet x n-values y [2]\nhistogram x"

PLOT
564
1426
839
1619
apt. build. owners / tenants
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"set-plot-x-range 1 6\nset-plot-pen-interval 0\nset-histogram-num-bars 5\nset-plot-background-color grey + 3" ""
PENS
"owners" 1.0 1 -16777216 true "" "let y length remove-duplicates [building-nmr] of households with [ownership = \"owner\" and building-nmr != 0 and count same-building with [ownership = \"tenant\"] < 1]\nlet x n-values y [1]\nhistogram x"
"tenants" 1.0 1 -1 true "" "let y length remove-duplicates [building-nmr] of households with [ownership = \"tenant\" and building-nmr != 0 and count same-building with [ownership = \"owner\"] < 1]\nlet x n-values y [2]\nhistogram x"
"mixed" 1.0 1 -13345367 true "" "let y length remove-duplicates [building-nmr] of households with [building-nmr != 0]\nlet x n-values y [3]\nhistogram x"
"PV-owners" 1.0 1 -2674135 true "" "let y length remove-duplicates [building-nmr] of households with [ownership = \"owner\" and building-nmr != 0 and count same-building with [ownership = \"tenant\"] < 1 and member? 4 inv-list]\nlet x n-values y [4]\nhistogram x"
"PV-tenants" 1.0 1 -10899396 true "" "let y length remove-duplicates [building-nmr] of households with [ownership = \"tenant\" and building-nmr != 0 and count same-building with [ownership = \"owner\"] < 1 and member? 4 inv-list]\nlet x n-values y [5]\nhistogram x"

SWITCH
20
411
201
444
owners-building-only?
owners-building-only?
1
1
-1000

PLOT
292
807
567
1001
Gas balance owners
year
NIL
0.0
10.0
0.0
2.0
true
true
"set-plot-background-color grey + 3" ""
PENS
"gas" 1.0 0 -16777216 true "" "plot ( sum [gas-prod] of households with [ownership = \"owner\"] / sum [gas-consumption] of households with [ownership = \"owner\"])"
"balance" 1.0 0 -2674135 true "" "plot 1"

PLOT
292
1000
567
1194
Gas balance all
year
NIL
0.0
10.0
0.0
2.0
true
true
"set-plot-background-color grey + 3" ""
PENS
"gas" 1.0 0 -16777216 true "" "plot ( sum [gas-prod] of households / sum [gas-consumption] of households)"
"balance" 1.0 0 -2674135 true "" "plot 1"

SLIDER
20
248
201
281
unc-stand.dev-consumat
unc-stand.dev-consumat
0
0.5
0.05
0.05
1
NIL
HORIZONTAL

PLOT
567
1001
842
1194
Average adoption rate
year
ratio
0.0
10.0
0.0
1.0
true
true
"set-plot-background-color grey + 3" ""
PENS
"avg-rate" 1.0 0 -16777216 true "" "plot mean [score-inv] of households with [score-inv > 0]"
"avg-all" 1.0 0 -2674135 true "" "plot mean [score-inv] of households"

INPUTBOX
20
155
108
215
elec-kwh-base
0.232
1
0
Number

INPUTBOX
107
155
197
215
gas-m3-base
0.829
1
0
Number

MONITOR
826
365
945
410
kWh-price (€)
electricity-price
2
1
11

MONITOR
826
409
945
454
M3-price (€)
gas-price
2
1
11

PLOT
842
1001
1115
1194
Energy prices (€/ kWh or M3)
year
NIL
0.0
10.0
0.0
1.0
true
true
"set-plot-background-color grey + 3" ""
PENS
"Electricity" 1.0 0 -1184463 true "" "plot electricity-price"
"Gas" 1.0 0 -10899396 true "" "plot gas-price"

SLIDER
20
280
112
313
LNS-slider
LNS-slider
0
1
0.8
0.1
1
NIL
HORIZONTAL

SWITCH
111
280
201
313
LNS-adj?
LNS-adj?
0
1
-1000

@#$#@#$#@
## WHAT IS IT?

The Energy Pro model is designed to explore how homeowners make adoption decisions on energy efficiency measures (EEM) and their contribution to the carbon emissions reduction goal of Amsterdam.

## HOW IT WORKS

The households' behavior and decision-making are designed based on the Consumat meta-model that incorporates four behavioral strategies: reperat, imitate, optimise, and inquire. 
The households choose one of these strategies each tick based on their level of need satisfaction and uncertainty levels, which in turn, depend on their socio-demographical and dwelling characteristics. Based on chosen strategies, households make decisions related to their energy consumption and EEM adoption. 

## HOW TO USE IT

Step 1) Choose a district from the list, for which we are running the model. 

Step 2) Adjust some model parameters depending on a scenario we are running. These 	include the level of uncertainty, minimum level of need satifaction, collective decision-making, tenants participating in the adoption decision-making, only owners buildings, and whether the model is run with a new random seed or not.
(!) Keep in mind while doing multiple runs, it is necessary to change the parameters after pressing the Setup button due to default values that are being reset. The structure is meant for a first run / load-in.  

Step 3) After dialing in the parameter functions, press the Setup-button.

Step 4) After a quick load, the model should be ready to go. Press the Go-button for a continuous run. Press the Go-single for a run on a "per tick" basis.

(Optional) Underneath the view, there are several buttons that allow us to see the distributions of the average adoption rate of the all EEM and separate EEM. 
These buttons will change the color of the patches for the most part and will change per tick. 
Buttons with a repeat circle can be used to monitor the changes in real-time and have to be pressed only once, followed by the Go-button if the model has been paused.

Step 5) After the model has reached its maximum ticks / years, press the
- clear: reset the parameter values for the existing district to run the same district with new and.or the same parameter values.
- clear-all: remove the entire districts and agents for a clean, fresh run.
- clear-drawing: remove the GIS data.
- reset-patches: remove all the patches related values.
- default kWh/M3: reset the energy related parameters to their default values.

## THINGS TO NOTICE

Using the default values, what are the existing patterns of the EEM decisions of households? Is it different per district? Can you find a link between uncertainty and the satisfaction levels of the resident?

What about the effect on the city's climate goals? Will the city reach its set goals? How does that relate to the emissions of the city? Can Amsterdam become fully self-sufficient in energy-needs?

To what extent are the distributions of the measures changing over time? Does it stagnate or accelerate over time (see step optional - how to use it)?

## THINGS TO TRY

There are several (demographic) differences across the 7 districts. Try to explore and identify the differences in EEM per district.

The default parameters do not include the inclusion of decisions by tenants. Disable the owners-only? to see the effect of all the residents have the ability to make a decision regarding EEM.

The level of need satisfaction is a big contributor how homeowners make decisions. Try to explore the effect of changing the level of need satisfaction threshold using the LNS-adj? and the corresponding threshold slider.

Normally, decisions in an apartment building regarding the purchase of EEM depends on a democratic process between the tenants living there. However, what happens if we remove this collective decision-making by disabling the collective-dm? button?

The uncertainty-sliders of the energy price and the uncertainty of Consumat impacts the outcome of the model. Try to explore using different values for these two sliders.

An apartment building might consists of both tenants and homeowners. Try to explore the effect if we include them in decision-making per building by mixing buildings with both tenants and homeowners by disabling the owners-building-only? button.

## EXTENDING THE MODEL

The model can be extended by adding socio-psychological variables that would also be important for designing behaviors and decision-making. We also suggest exploring a cultural aspect in decision-making across the city districts. 

In terms of the technical part of the energy system, it would be necessary to add storage batteries or e-cars serving as storage batteries for produced energy. In general, it would be interesting to see how urban mobility can accelerate the energy transition. 

## NETLOGO FEATURES

This model makes use of the GIS extension to load-in geographical data from the districts of the city of Amsterdam, the Netherlands.

The model makes use of the csv and table extensions to load-in .csv datasets with data for the residents and the measures as well as storing them in a table format.

The model makes use of the palette extensions to create better distribution colors and customizations options for the interface, meant for the color of the patches.

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

Copyright ® Erkinai Derkenbaeva, Juriaan Wolfers (2023)
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

crate
false
0
Rectangle -7500403 true true 45 45 255 255
Rectangle -16777216 false false 45 45 255 255
Rectangle -16777216 false false 60 60 240 240
Line -16777216 false 180 60 180 240
Line -16777216 false 150 60 150 240
Line -16777216 false 120 60 120 240
Line -16777216 false 210 60 210 240
Line -16777216 false 90 60 90 240
Polygon -7500403 true true 75 240 240 75 240 60 225 60 60 225 60 240
Polygon -16777216 false false 60 225 60 240 75 240 240 75 240 60 225 60

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

electric outlet
false
0
Rectangle -7500403 true true 45 0 255 297
Polygon -16777216 false false 120 270 90 240 90 195 120 165 180 165 210 195 210 240 180 270
Rectangle -16777216 true false 169 199 177 236
Rectangle -16777216 true false 169 64 177 101
Polygon -16777216 false false 120 30 90 60 90 105 120 135 180 135 210 105 210 60 180 30
Rectangle -16777216 true false 123 64 131 101
Rectangle -16777216 true false 123 199 131 236
Rectangle -16777216 false false 45 0 255 296

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

lightning
false
0
Polygon -7500403 true true 120 135 90 195 135 195 105 300 225 165 180 165 210 105 165 105 195 0 75 135

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

sun
false
0
Circle -7500403 true true 75 75 150
Polygon -7500403 true true 300 150 240 120 240 180
Polygon -7500403 true true 150 0 120 60 180 60
Polygon -7500403 true true 150 300 120 240 180 240
Polygon -7500403 true true 0 150 60 120 60 180
Polygon -7500403 true true 60 195 105 240 45 255
Polygon -7500403 true true 60 105 105 60 45 45
Polygon -7500403 true true 195 60 240 105 255 45
Polygon -7500403 true true 240 195 195 240 255 255
Rectangle -16777216 true false 0 210 300 315

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
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="owners-only?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dataset-residents">
      <value value="&quot;4-dataset-residents/zuid-oost.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gas-price">
      <value value="0.829"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-random-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-building-size">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="electricity-price">
      <value value="0.232"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="uncertainty-energy-prices">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="collective-dm?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maximum-years">
      <value value="10"/>
    </enumeratedValueSet>
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
